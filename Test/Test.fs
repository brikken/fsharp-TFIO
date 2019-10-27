// Learn more about F# at http://fsharp.org

open FsCheck
open TFIO

[<EntryPoint>]
let main _ =
    let leftIdentityProperty x y f = Trans.run (Trans.bind f (Trans.return' x)) y = Trans.run (f x) y
    let rightIdentityProperty m x = Trans.run (Trans.bind Trans.return' m) x = Trans.run m x
    let associativityProperty m f g x = Trans.run (Trans.bind g (Trans.bind f m)) x = Trans.run (Trans.bind (fun y -> Trans.bind g (f y)) m) x

    let actionCondition case (Action f) s = case (f s)
    let expectedResultProperty case res a s = actionCondition case a s ==> (Trans.run (M a) s |> res)
    
    let successfullSucceedProperty a s =
        expectedResultProperty
            (function | Done (_, _, Undo _, _) -> true | _ -> false)
            (function | Committed _ -> true | _ -> false)
            a s
    
    let undoneWillRollbackProperty a s =
        expectedResultProperty
            (function | Undone -> true | _ -> false)
            (function | RolledBack -> true | _ -> false)
            a s

    let justOneUndoneWillRollbackOrFailProperty acs s x i =
        let res =
            List.insert acs [(i, Action (fun _ -> Undone))]
            |> List.map (fun t -> (fun _ -> M t))
            |> List.fold (fun s' t' -> Trans.bind t' s') (Trans.return' x)
            |> fun m -> Trans.run m s
        match res with
        | RolledBack | Failed _ -> true
        | _ -> false

    let justOneFailedUndoGivesFailedProperty s1 t1 e1 w1 acs2 i2 s x =
        let res =
            (Action (fun _ -> Done (s1, t1, Undo (fun () -> Error e1), w1)))::(List.insert acs2 [(i2, Action (fun _ -> Undone))])
            |> List.map (fun t -> (fun _ -> M t))
            |> List.fold (fun s' t' -> Trans.bind t' s') (Trans.return' x)
            |> fun m -> Trans.run m s
        match res with
        | Failed _ -> true
        | _ -> false

    let config = { Config.Quick with EndSize = 100; MaxTest = 250; Config.QuietOnSuccess = true; }

    Check.One(config, leftIdentityProperty)
    Check.One(config, rightIdentityProperty)
    Check.One(config, associativityProperty)
    Check.One(config, successfullSucceedProperty)
    Check.One(config, undoneWillRollbackProperty)
    Check.One({ config with EndSize = 10; }, justOneUndoneWillRollbackOrFailProperty)
    Check.One({ config with EndSize = 10; }, justOneFailedUndoGivesFailedProperty)
    printfn "Done"
    0 // return an integer exit code
