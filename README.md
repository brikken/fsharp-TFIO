# Transaction
## Testing
Actions can succeed, fail and trigger rollback, succeed rollback or fail rollback
* Done (..)
* Undone
* Ok (..)
* Error (..)

### Properties
* Two transactions succeeding independently will also succeed together
* A failing transaction will also fail together with another transaction
* ```run (return' x) y = Committed (y, x)```
* ```run (M (fun s -> Done (s + z, x, Undo (fun () -> Ok ()), (fun () -> ())))) y = Committed (y + z, x)```