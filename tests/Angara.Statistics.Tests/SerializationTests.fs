module SerializationTests

open NUnit.Framework
open Swensen.Unquote

open Angara.Serialization
open Angara.Statistics
open Angara.Filzbach
open Angara.Statistics.Serialization
open Angara.Filzbach.Serialization

[<Test>]
let DistributionSerialization() =
    let lib = SerializerLibrary.CreateDefault()
    lib.Register(DistributionSerializer())
    let check d =
        let is = serializeDistribution d
        d =! deserializeDistribution is
        let json = Json.FromObject(lib,d).ToString()
        d =! Json.ToObject(Newtonsoft.Json.Linq.JObject.Parse json,lib)
    Uniform(1., 2.) |> check
    LogUniform(1.,2.) |> check
    Normal(1., 2.) |> check
    LogNormal(1.,2.) |> check
    Gamma(1., 2.) |> check
    NegativeBinomial(1., 2.) |> check
    Binomial(2, 1.) |> check
    Bernoulli(0.7) |> check
    Exponential(2.) |> check
    Poisson(2.) |> check
    Mixture[0.4,LogNormal(1.,2.); 0.6,Normal(3.,4.)] |> check
    //
    raises<System.ArgumentException> <@ deserializeDistribution InfoSet.EmptyMap @>
    raises<System.ArgumentException> <@ deserializeDistribution (InfoSet.Seq[]) @>
    raises<System.ArgumentException> <@ deserializeDistribution (InfoSet.String "") @>
    raises<System.ArgumentException> <@ deserializeDistribution (InfoSet.Double 1.) @>

[<Test>]
let ParametersSerialization() =
    let lib = SerializerLibrary.CreateDefault()
    lib.Register(ParametersSerializer())
    let check p =
        let is = serializeParameters p
        p =! deserializeParameters is
        let json = Json.FromObject(lib,p).ToString()
        p =! Json.ToObject(Newtonsoft.Json.Linq.JObject.Parse json,lib)
    Parameters.Empty |> check
    Parameters.Empty.Add("a", 1.) |> check
    Parameters.Empty.Add("a", 1.).Add("b", [|2.; 3.|]) |> check
    Parameters.Empty.Add("b", [|2.; 3.|]).Add("a", 1.) |> check
    Parameters.Empty
        .Add("a", Uniform(1.,2.))
        .Add("z y w", LogUniform(3.,4.), 2)
        .Add("k", Normal(5.,6.), 3)
        .Add("l", LogNormal(7.,8.),2) |> check

    raises<System.ArgumentException> <@ deserializeParameters InfoSet.EmptyMap @>
    raises<System.ArgumentException> <@ deserializeParameters (InfoSet.Seq[]) @>
    raises<System.ArgumentException> <@ deserializeParameters (InfoSet.String "") @>
    raises<System.ArgumentException> <@ deserializeParameters (InfoSet.Double 1.) @>
