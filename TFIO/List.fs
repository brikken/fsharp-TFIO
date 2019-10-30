namespace TFIO.Collections

[<RequireQualifiedAccess>]
module List =
    /// <summary>Inserts elements <c>ys</c> at the indices given in first tuple element</summary>
    /// <param name="xs">List to insert elements into</param>
    /// <param name="ys">List of tuples. First tuple element indicates target position in <c>xs</c>. Position can be any integer, since the actual position <c>p</c> is <c>(abs p) % (List.length xs)</c></param>
    [<CompiledName("Insert")>]
    let insert xs ys =
        match (xs, ys) with
        | ([], _) -> List.map snd ys
        | (_, []) -> xs
        | _ ->
            let ys' = List.map (fun (i, y) -> ((abs i) % (List.length xs), y)) ys
            let rec insertRec xs ys n x2s =
                match (xs, ys) with
                | ([], _) | (_, []) -> x2s @ xs
                | (x'::xs', ys') ->
                    let (esNow, esLater) = List.partition (fun (i, _) -> i = n) ys'
                    insertRec xs' esLater (n + 1) (x2s @ List.map snd esNow @ [x'])
            insertRec xs ys' 0 []
