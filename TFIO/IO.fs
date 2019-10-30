namespace TFIO

open TFIO.Transaction
open System.IO

type OpenAction = { file: FileInfo; stream: FileStream; }
type ReadAction = { openAction: OpenAction; position: int64; read: byte []; }
type WriteAction = { openAction: OpenAction; position: int64; wrote: byte []; }
type CloseAction = { openAction: OpenAction; }
//type OverwriteAction = { original: FileInfo; backup: FileInfo; }
[<RequireQualifiedAccess>]
type OpenConsequence =
    | Nothing
    | NewCreated
    | ExistingOpened
    | ExistingOverwritten// of OverwriteAction
[<RequireQualifiedAccess>]
type Preserve =
    | Open
    | Overwrite
type OpenCondition =
    | OnlyIfNotExists
    | OnlyIfExists of Preserve
    | Indifferent of Preserve
[<RequireQualifiedAccess>]
type IOAction =
    | Open of OpenAction
    | Read of ReadAction
    | Write of WriteAction
    | Close of CloseAction
type IOLog =
    | IOLog of IOAction list

#nowarn "0104" // sensible pattern matching on enums
[<RequireQualifiedAccess>]
module IO =
    module private Open =
        let backup (file: FileInfo) (mode: FileMode) = (file.Exists) && mode <> FileMode.CreateNew
        (*let condition (mode: FileMode) =
            match mode with
            | FileMode.Append -> Indifferent Preserve.Open
            | FileMode.Create -> Indifferent Preserve.Overwrite
            | FileMode.CreateNew -> OnlyIfNotExists
            | FileMode.Open -> OnlyIfExists Preserve.Open
            | FileMode.OpenOrCreate -> Indifferent Preserve.Open
            | FileMode.Truncate -> OnlyIfExists Preserve.Overwrite
        let private preserveConsequence preserve =
            match preserve with
            | Preserve.Open -> ExistingOpened
            | Preserve.Overwrite -> ExistingOverwritten
        let consequence condition (file: FileInfo) =
            match (condition, file.Exists) with
            | (OnlyIfNotExists, true) -> Nothing
            | (OnlyIfNotExists, false) -> NewCreated
            | (OnlyIfExists preserve, true) -> preserveConsequence preserve
            | (OnlyIfExists _, false) -> Nothing
            | (Indifferent preserve, true) -> preserveConsequence preserve
            | (Indifferent _, false) -> NewCreated
        let mode consequence =
            match consequence with
            | Nothing -> None
            | NewCreated -> Some (FileMode.CreateNew)
            | ExistingOpened -> Some (FileMode.Open)
            failwith ""*)
    module private Backup =
        let toFile (file: FileInfo) (dir: DirectoryInfo) =
            let items = file.Name :: (Array.toList (dir.GetFiles()) |> List.map (fun f -> f.Name)) @ (Array.toList (dir.GetDirectories()) |> List.map (fun d -> d.Name))
            // TODO: finish. Make sure filename doesn't exist and isn't too long
            failwith ""
    let private tryIO io =
        try io () |> Ok
        with ex -> Error ex
    [<CompiledName("Open")>]
    let ``open`` (file: FileInfo) (mode: FileMode) =
        let action (IOLog ioLog) =
            let folder a s =
                match (s, a) with
                | (None, IOAction.Open openAction) when openAction.file.FullName = file.FullName -> Some openAction
                | (Some _, IOAction.Close closeAction) when closeAction.openAction.file.FullName = file.FullName -> None
                | _ -> s
            match List.foldBack folder ioLog None with
            | Some openAction -> Done ((IOLog ioLog), openAction.stream, Undo.empty, Wrap.empty)
            | None ->
                try
                    // TODO: find backup filename and make a backup, to be used in undo and wrapup functions
                    (*let backupFile =
                        if Open.backup file mode then
                            file.CopyTo()*)
                    let stream = file.Open(mode, FileAccess.ReadWrite, FileShare.None)
                    let ioLog' = (IOAction.Open { file = file; stream = stream; })::ioLog
                    let undo () =
                        match mode with
                        | FileMode.Append -> tryIO stream.Close
                        //| FileMode.Create
                    failwith ""
                with ex ->
                    failwith ""
        Action action