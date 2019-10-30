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

## How to implement
### Actions
A transaction consists of actions to be performed. For each action, consider the following:
* What task will the action perform?
* Does the action return a value and if yes, what?
* How does the action depend on the current transaction state?
* How does the action affect the state?
* How will the action be rolled back if required?
* Which final non-failable steps does the action required to wrap up?

# ToDo
* Files are always opened Read/Write - granular enough?

# IO
Fundamental assumption: No other processes or threads are doing file IO in the directory
## FileMode
What we need to know is, based on the current state:
* Is the requested FileMode possible? (Do we care? No)
* Should we take a backup? Always, if:
  * file exists
  * FileMode will overwrite
  * request is possible
* Which FileMode should actually be used? (The one requested. If it fails, it fails)

## Errors
Types of IO errors
* Breach of assumptions (causing undefined behaviour)
* Physical errors (halting operation)
