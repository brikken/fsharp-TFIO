﻿// Learn more about F# at http://fsharp.org

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
        (not (List.isEmpty acs) && i >= 0) ==> (lazy
            (
                let actionUndo = Action (fun _ -> Undone)
                let l = List.length acs
                let i' = i % l
                let acs' = (List.take i' acs) @ [actionUndo] @ (List.skip i' acs |> List.take (l - i'))
                let res =
                    acs'
                    |> List.map (fun t -> (fun _ -> M t))
                    |> List.fold (fun s' t' -> Trans.bind t' s') (Trans.return' x)
                    |> fun m -> Trans.run m s
                match res with
                | RolledBack | Failed _ -> true
                | _ -> false
            )
        )

    let config = { Config.Quick with EndSize = 100; MaxTest = 250; Config.QuietOnSuccess = true; }

    Check.One(config, leftIdentityProperty)
    Check.One(config, rightIdentityProperty)
    Check.One(config, associativityProperty)
    Check.One(config, successfullSucceedProperty)
    Check.One(config, undoneWillRollbackProperty)
    Check.One({ config with EndSize = 10; }, justOneUndoneWillRollbackOrFailProperty)
    printfn "Done"
    0 // return an integer exit code
