module FilzbachTests
open NUnit.Framework
open FsUnit

open Angara.Filzbach

let uniform = Angara.Statistics.Uniform(0.,1.)

[<Test>]
let ParametersTests() =
    let p0 = Parameters.Empty
    p0.CountValues |> should equal 0
    p0.AllValues |> should equal [||]
    (fun () -> p0.GetValue 0 |> ignore) |> should throw typeof<System.IndexOutOfRangeException>
    (fun () -> p0.GetValue "unknown" |> ignore) |> should throw typeof<System.Collections.Generic.KeyNotFoundException>
    (fun () -> p0.GetValue("unknown",0) |> ignore) |> should throw typeof<System.Collections.Generic.KeyNotFoundException>
    (fun () -> p0.GetDefinition "unknown" |> ignore) |> should throw typeof<System.Collections.Generic.KeyNotFoundException>
    (p0:>IParameters).Count |> should equal 0

    // scalar
    (fun () -> p0.Add(null, [|0.5|],0.,1.) |> ignore) |> should throw typeof<System.ArgumentException> // empty name
    (fun () -> p0.Add("", [|0.5|],0.,1.) |> ignore) |> should throw typeof<System.ArgumentException> // empty name
    (fun () -> p0.Add("s", [||],0.,1.) |> ignore) |> should throw typeof<System.ArgumentException> // empty values
    (fun () -> p0.Add("s", [|0.5|],1.,0.) |> ignore) |> should throw typeof<System.ArgumentException> // lower>upper
    (fun () -> p0.Add("s", [|0.|],0.5,1.) |> ignore) |> should throw typeof<System.ArgumentException> // value out of range
    (fun () -> p0.Add("s", [|0.5|],0.,1.,isLog=true) |> ignore) |> should throw typeof<System.ArgumentException> // lower for log par
    let p1 = p0.Add("s", [|0.5|],0.,1.)
    p0.Add("s", [|0.5|],0.,1.) |> should equal p1
    (fun () -> p1.Add("s", [|0.5|],0.,1.) |> ignore) |> should throw typeof<System.ArgumentException> // duplicate name
    p1.CountValues |> should equal 1
    p1.AllValues |> List.ofSeq |> should equal [0.5]
    p1.GetValue 0 |> should equal 0.5
    (fun () -> p1.GetValue 1 |> ignore) |> should throw typeof<System.IndexOutOfRangeException>
    (fun () -> p1.GetValue -1 |> ignore) |> should throw typeof<System.IndexOutOfRangeException>
    p1.GetValue "s" |> should equal 0.5
    p1.GetValue("s", 0) |> should equal 0.5
    (fun () -> p1.GetValue("s", 1) |> ignore) |> should throw typeof<System.IndexOutOfRangeException>
    p1.GetDefinition "s" |> should equal {index=0; size=1; lower=0.; upper=1.; delay=0; prior=uniform; isLog=false; log_priordf=id}
    (p1:>IParameters).Count |> should equal 1
    (p1:>IParameters).ContainsKey "s" |> should be True
    (p1:>IParameters).ContainsKey "a" |> should be False
    (p1:>IParameters).["s"] |> should equal [|0.5|]
    let v = ref [||] 
    (p1:>IParameters).TryGetValue("s", v) |> should be True
    !v |> should equal [|0.5|]
    (p1:>IParameters).Keys |> Seq.toList |> should equal ["s"]
    (p1:>IParameters).Values |> Seq.toList |> should equal [[|0.5|]]

    // vector
    (fun () -> p0.Add("v", [|0.5; -0.5|],0.,1.) |> ignore) |> should throw typeof<System.ArgumentException> // value out of range
    let p2 = p0.Add("v", [|0.6;0.7|],0.,1.)
    p2.CountValues |> should equal 2
    p2.AllValues |> List.ofSeq |> should equal [0.6;0.7]
    p2.GetValue 0 |> should equal 0.6
    p2.GetValue 1 |> should equal 0.7
    (fun () -> p2.GetValue "v" |> ignore) |> should throw typeof<System.InvalidOperationException> // vector syntax
    p2.GetValue("v", 0) |> should equal 0.6
    p2.GetValue("v", 1) |> should equal 0.7
    p2.GetDefinition "v" |> should equal {index=0; size=2; lower=0.; upper=1.; delay=0; prior=uniform; isLog=false; log_priordf=id}
    (p2:>IParameters).Count |> should equal 1
    (p2:>IParameters).ContainsKey "v" |> should be True
    (p2:>IParameters).ContainsKey "a" |> should be False
    (p2:>IParameters).["v"] |> should equal [|0.6;0.7|]
    (p2:>IParameters).TryGetValue("v", v) |> should be True
    !v |> should equal [|0.6;0.7|]
    (p2:>IParameters).Keys |> Seq.toList |> should equal ["v"]
    (p2:>IParameters).Values |> Seq.toList |> should equal [[|0.6;0.7|]]

    // second scalar
    let p3 = p2.Add("s", [|0.5|],0.,1.)
    p2.Add("s", [|0.5|],0.,1.) |> should equal p3
    (fun () -> p1.Add("s", [|0.5|],0.,1.) |> ignore) |> should throw typeof<System.ArgumentException> // duplicate name
    p3.CountValues |> should equal 3
    p3.AllValues |> List.ofSeq |> should equal [0.6;0.7;0.5]
    p3.GetValue 0 |> should equal 0.6
    p3.GetValue 1 |> should equal 0.7
    p3.GetValue 2 |> should equal 0.5
    p3.GetValue("v", 0) |> should equal 0.6
    p3.GetValue("v", 1) |> should equal 0.7
    p3.GetValue "s" |> should equal 0.5
    p3.GetValue("s", 0) |> should equal 0.5
    p3.GetDefinition "s" |> should equal {index=2; size=1; lower=0.; upper=1.; delay=0; prior=uniform; isLog=false; log_priordf=id}
    p3.GetDefinition "v" |> should equal {index=0; size=2; lower=0.; upper=1.; delay=0; prior=uniform; isLog=false; log_priordf=id}
    (p3:>IParameters).Count |> should equal 2
    (p3:>IParameters).ContainsKey "s" |> should be True
    (p3:>IParameters).ContainsKey "v" |> should be True
    (p3:>IParameters).ContainsKey "a" |> should be False
    (p3:>IParameters).["s"] |> should equal [|0.5|]
    (p3:>IParameters).["v"] |> should equal [|0.6;0.7|]
    (p3:>IParameters).TryGetValue("s", v) |> should be True
    !v |> should equal [|0.5|]
    (p3:>IParameters).TryGetValue("v", v) |> should be True
    !v |> should equal [|0.6;0.7|]
    (p3:>IParameters).Keys |> Seq.toList |> should equal ["s";"v"]
    (p3:>IParameters).Values |> Seq.toList |> should equal [[|0.5|]; [|0.6;0.7|]]

    // second vector
    let p4 = p1.Add("v", [|0.6;0.7|],0.,1.)
    p1.Add("v", [|0.6;0.7|],0.,1.) |> should equal p4
    p4.CountValues |> should equal 3
    p4.AllValues |> List.ofSeq |> should equal [0.5;0.6;0.7]
    p4.GetValue 0 |> should equal 0.5
    p4.GetValue 1 |> should equal 0.6
    p4.GetValue 2 |> should equal 0.7
    p4.GetValue("v", 0) |> should equal 0.6
    p4.GetValue("v", 1) |> should equal 0.7
    p4.GetValue "s" |> should equal 0.5
    p4.GetValue("s", 0) |> should equal 0.5
    p4.GetDefinition "s" |> should equal {index=0; size=1; lower=0.; upper=1.; delay=0; prior=uniform; isLog=false; log_priordf=id}
    p4.GetDefinition "v" |> should equal {index=1; size=2; lower=0.; upper=1.; delay=0; prior=uniform; isLog=false; log_priordf=id}
    (p4:>IParameters).Count |> should equal 2
    (p4:>IParameters).ContainsKey "s" |> should be True
    (p4:>IParameters).ContainsKey "v" |> should be True
    (p4:>IParameters).ContainsKey "a" |> should be False
    (p4:>IParameters).["s"] |> should equal [|0.5|]
    (p4:>IParameters).["v"] |> should equal [|0.6;0.7|]
    (p4:>IParameters).TryGetValue("s", v) |> should be True
    !v |> should equal [|0.5|]
    (p4:>IParameters).TryGetValue("v", v) |> should be True
    !v |> should equal [|0.6;0.7|]
    (p4:>IParameters).Keys |> Seq.toList |> should equal ["s";"v"]
    (p4:>IParameters).Values |> Seq.toList |> should equal [[|0.5|]; [|0.6;0.7|]]


