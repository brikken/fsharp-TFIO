namespace TFIO.Transaction

type Undo<'S,'E> =
    | Undo of (unit -> Result<unit,'E>)
    | UndoF of (unit -> Result<unit,'S * 'E>)
type Wrap = WrapUp of (unit -> unit)
type ActionRes<'S,'T,'E> = Done of 'S * 'T * Undo<'S,'E> * Wrap | Undone
type TState<'S,'T,'E> = ActionDone of ActionRes<'S,'T,'E> | Failure of 'S * 'E
type TResult<'S,'T,'E> = Committed of 'S * 'T | RolledBack | Failed of 'S * 'E
type Action<'S,'T,'E> = Action of ('S -> ActionRes<'S,'T,'E>)
type TExec<'S,'T,'E> = TransExec of ('S -> TState<'S,'T,'E>)
type T<'S,'T,'E> =
    | M of Action<'S,'T,'E>
    | MF of TExec<'S,'T,'E>

[<RequireQualifiedAccess>]
module Trans =
    let private runM m s =
        match m with
        | (M (Action f)) -> ActionDone (f s)
        | (MF (TransExec f)) -> f s
    let private fullInnerUndo u s =
        match u with
        | Undo f -> f >> Result.mapError (fun e -> (s, e))
        | UndoF f -> f
    let return' x =
        let returned s =
            Done (s, x, Undo (fun () -> Ok ()), WrapUp (fun () -> ()))
        M (Action returned)
    let bind f x =
        let bound s =
            match runM x s with
            | Failure (es, e) -> Failure (es, e)
            | ActionDone (Done (s', t', undo_x, wrapup_x)) ->
                match runM (f t') s' with
                | Failure (es, e) -> Failure (es, e)
                | ActionDone (Done (s'', t'', undo_f, wrapup_f)) ->
                    let undo = UndoF (fullInnerUndo undo_f s'' >> Result.bind (fullInnerUndo undo_x s'))
                    let wrapup = WrapUp (fun () -> let (WrapUp wx, WrapUp wf) = (wrapup_x, wrapup_f) in (wx (), wf ()) |> ignore)
                    ActionDone (Done (s'', t'', undo, wrapup))
                | ActionDone Undone ->
                    match (fullInnerUndo undo_x s') () with
                    | Ok () -> ActionDone Undone
                    | Error (es, e) -> Failure (es, e)
            | ActionDone Undone -> ActionDone Undone
        MF (TransExec bound)
    let run m s =
        match runM m s with
        | ActionDone (Done (s', t', _, w')) ->
            let (WrapUp wf') = w' in wf' ()
            Committed (s', t')
        | ActionDone Undone -> RolledBack
        | Failure (s', e') -> Failed (s', e')
    type Builder() =
        member _.Bind(x, f) = bind f x
        member _.Return(x) = return' x

[<RequireQualifiedAccess>]
module Undo =
    [<CompiledName("Empty")>]
    let empty = Undo (fun () -> Ok ())

[<RequireQualifiedAccess>]
module Wrap =
    [<CompiledName("Empty")>]
    let empty = WrapUp (fun () -> ())
