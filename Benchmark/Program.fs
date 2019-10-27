// Learn more about F# at http://fsharp.org

open System
open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes
open FsCheck
open TFIO

type ListBenchmark<'T>() =
    let mutable xs : 'T list = []
    let mutable ys : (int * 'T) list = []
    [<GlobalSetup>]
    member _.Setup() =
        xs <- Gen.sample 1000 10 Arb.generate<'T>
        let (ysN, ysSize) = (100, 10) in ys <- List.zip (Gen.sample ysN ysSize Arb.generate<int>) (Gen.sample ysN ysSize Arb.generate<'T>)
    [<Benchmark>]
    member _.Insert() =
        List.insert xs ys |> ignore
    [<Benchmark>]
    member _.InsertV2() =
        List.insertV2 xs ys |> ignore

[<EntryPoint>]
let main _ =
    BenchmarkRunner.Run typeof<ListBenchmark<string>> |> ignore
    0 // return an integer exit code
