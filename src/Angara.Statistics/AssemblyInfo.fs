namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Angara.Statistics")>]
[<assembly: AssemblyProductAttribute("Angara.Statistics")>]
[<assembly: AssemblyDescriptionAttribute("A collection of statistics algorithms from Mersenne twister generator to MCMC sampling.")>]
[<assembly: AssemblyVersionAttribute("0.1.3")>]
[<assembly: AssemblyFileVersionAttribute("0.1.3")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.3"
    let [<Literal>] InformationalVersion = "0.1.3"
