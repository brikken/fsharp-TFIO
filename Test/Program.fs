// Learn more about F# at http://fsharp.org

open FsCheck
open TFIO.Trans

[<EntryPoint>]
let main _ =
    let leftIdentityProperty x y f = run (bind f (return' x)) y = run (f x) y
    let rightIdentityProperty m x = run (bind return' m) x = run m x
    let associativityProperty m f g x = run (bind g (bind f m)) x = run (bind (fun y -> bind g (f y)) m) x

    let config = { Config.Quick with EndSize = 1000; MaxTest = 100; Config.QuietOnSuccess = true; }

    Check.One(config, leftIdentityProperty)
    Check.One(config, rightIdentityProperty)
    Check.One(config, associativityProperty)
    0 // return an integer exit code
