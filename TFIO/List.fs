namespace TFIO

[<RequireQualifiedAccess>]
module List =
    /// <summary>Inserts elements <c>ys</c> at the indices given in first tuple element</summary>
    let insert xs ys =
        match (xs, ys) with
        | ([], _) | (_, []) -> xs
        | _ ->
            let ys' = List.map (fun (i, y) -> (i % (List.length xs), y)) ys
            let rec insertRec xs ys n x2s =
                match (xs, ys) with
                | ([], _) | (_, []) -> x2s @ xs
                | (x'::xs', ys') ->
                    let (esNow, esLater) = List.partition (fun (i, _) -> i = n) ys'
                    insertRec xs' esLater (n + 1) (x2s @ List.map snd esNow @ [x'])
            insertRec xs ys' 0 []
    let insertV2 xs ys =
        match (xs, ys) with
        | ([], _) | (_, []) -> xs
        | _ ->
            let xsl = (List.length xs)
            let ys' = List.map (fun (i, y) -> (i % xsl, y)) ys
            let rec insertRec xs ys n x2s =
                match (xs, ys) with
                | ([], _) | (_, []) -> x2s @ xs
                | (x'::xs', ys') ->
                    let (esNow, esLater) = List.partition (fun (i, _) -> i = n) ys'
                    insertRec xs' esLater (n + 1) (x2s @ List.map snd esNow @ [x'])
            insertRec xs ys' 0 []