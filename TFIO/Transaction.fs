namespace TFIO

type Undo<'S,'E> =
    | Undo of (unit -> Result<unit,'E>)
    | UndoF of (unit -> Result<unit,'S * 'E>)
type WrapUp = WrapUp of (unit -> unit)
type ActionResult<'S,'T,'E> = Done of 'S * 'T * Undo<'S,'E> * WrapUp | Undone
type TransState<'S,'T,'E> = ActDone of ActionResult<'S,'T,'E> | Failure of 'S * 'E
type TransResult<'S,'T,'E> = Committed of 'S * 'T | RolledBack | Failed of 'S * 'E
type Trans<'S,'T,'E> =
    | M of ('S -> ActionResult<'S,'T,'E>)
    | MF of ('S -> TransState<'S,'T,'E>)

module Trans =
    let private runM m s =
        match m with
        | (M f) -> ActDone (f s)
        | (MF f) -> f s
    let private fullInnerUndo u s =
        match u with
        | Undo f -> f >> Result.mapError (fun e -> (s, e))
        | UndoF f -> f
    let return' x =
        let returned s =
            Done (s, x, Undo (fun () -> Ok ()), WrapUp (fun () -> ()))
        M returned
    let bind f x =
        let bound s =
            match runM x s with
            | Failure (es, e) -> Failure (es, e)
            | ActDone (Done (s', t', undo_x, wrapup_x)) ->
                match runM (f t') s' with
                | Failure (es, e) -> Failure (es, e)
                | ActDone (Done (s'', t'', undo_f, wrapup_f)) ->
                    let undo = UndoF (fullInnerUndo undo_f s'' >> Result.bind (fullInnerUndo undo_x s'))
                    let wrapup = WrapUp (fun () -> let (WrapUp wx, WrapUp wf) = (wrapup_x, wrapup_f) in (wx (), wf ()) |> ignore)
                    ActDone (Done (s'', t'', undo, wrapup))
                | ActDone Undone ->
                    match (fullInnerUndo undo_x s') () with
                    | Ok () -> ActDone Undone
                    | Error (es, e) -> Failure (es, e)
            | ActDone Undone -> ActDone Undone
        MF bound
    let run m s =
        match runM m s with
        | ActDone (Done (s', t', _, w')) ->
            let (WrapUp wf') = w' in wf' ()
            Committed (s', t')
        | ActDone Undone -> RolledBack
        | Failure (s', e') -> Failed (s', e')
    type Builder() =
        member _.Bind(x, f) = bind f x
        member _.Return(x) = return' x
