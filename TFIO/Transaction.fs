namespace TFIO

type Undo<'S,'E> = Undo of (unit -> Result<unit,'S * 'E>)
type WrapUp = WrapUp of (unit -> unit)
type Outcome<'S,'T,'E> = Done of 'S * 'T * Undo<'S,'E> * WrapUp | Undone
type TransResult<'S,'T,'E> = ActDone of Outcome<'S,'T,'E> | Failure of 'E * 'S
type Trans<'S,'T,'E> =
    | M of ('S -> Outcome<'S,'T,'E>)
    | MF of ('S -> TransResult<'S,'T,'E>)

module Trans =
    let private runM m s =
        match m with
        | (M f) -> ActDone (f s)
        | (MF f) -> f s
    let return' x =
        let returned s =
            Done (s, x, Undo (fun () -> Ok ()), WrapUp (fun () -> ()))
        M returned
    let bind f x =
        let bound s =
            match runM x s with
            | Failure (e, es) -> Failure (e, es)
            | ActDone (Done (s', t', undo_x, wrapup_x)) ->
                match runM (f t') s' with
                | Failure (e, es) -> Failure (e, es)
                | ActDone (Done (s'', t'', undo_f, wrapup_f)) ->
                    let undo = Undo (fun () -> (let (Undo uf) = undo_f in uf ()) |> Result.bind (let (Undo ux) = undo_x in ux))
                    let wrapup = WrapUp (fun () -> let (WrapUp wx, WrapUp wf) = (wrapup_x, wrapup_f) in (wx (), wf ()) |> ignore)
                    ActDone (Done (s'', t'', undo, wrapup))
                | ActDone Undone ->
                    match (let (Undo ux) = undo_x in ux ()) with
                    | Ok () -> ActDone Undone
                    | Error (es, e) -> Failure (e, es)
            | ActDone Undone -> ActDone Undone
        MF bound
    type Builder() =
        member _.Bind(x, f) = bind f x
        member _.Return(x) = return' x
