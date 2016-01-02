module statistics.Tests
open NUnit.Framework
open FsUnit
type Complex = System.Numerics.Complex

open Angara.Statistics

[<Test>]
let Radix2TransformsRealSineCorrectly () =
    let n = 16 
    let step = pi2/float n
    let samples = Array.init 16 (fun i -> let x = step * float i in Complex(sin x, 0.))
    let spectrum = fft samples
    spectrum |> should haveLength 16
    spectrum |> Array.iteri (fun i c -> 
        c.Real |> should (equalWithin 1.e-12) 0.0)
    spectrum |> Array.iteri (fun i c -> 
        c.Imaginary |> should (equalWithin 1.e-12) (match i with 1 -> -8.0 | 15 -> 8.0 | _ -> 0.))
    let restore = ifft spectrum
    Seq.zip restore samples |> Seq.iteri (fun i (r,s) ->
        r.Real |> should (equalWithin 1.e-12) s.Real
        r.Imaginary |> should (equalWithin 1.e-12) s.Imaginary)

[<Test>]
let dct_1() =
    let result = dct [|1.0|]
    result |> should (equalWithin 1.e-12) [|2.0|]

[<Test>]
let dct_2a() =
    let result = dct [|1.0; 0.0|]
    result |> should (equalWithin 1.e-12) [|2.0; 2. * cos(pi*0.25)|]

[<Test>]
let dct_2b() =
    let result = dct [|0.0; 1.0|]
    result |> should (equalWithin 1.e-12) [|2.0; 2. * cos(pi*0.75)|]

[<Test>]
let dct_4() =
    let result = dct [|1.0; 0.0; 0.0; 0.0|]
    result |> should (equalWithin 1.e-12) [|2.0; 2.*cos(pi*1./8.); 2.*cos(pi*2./8.); 2.*cos(pi*3./8.)|]

[<Test>]
let within_tests() =
    let omicron = System.Double.Epsilon // smallest positive float
    let rec find_eps d = if 0.5*d + 1.0 = 1.0 then 1.0+d else find_eps (0.5*d)
    let epsilon = find_eps 1.0 // smallest float greater than one
    // minus 0 -- the second representation of zero
    let minus0 = -omicron / 2.0
    minus0 |> should equal 0.0
    let bits_0 = System.BitConverter.DoubleToInt64Bits 0.0
    let bits_minus0 = System.BitConverter.DoubleToInt64Bits minus0
    bits_minus0 |> should not' (equal bits_0)
    //
    // equality
    within 0u 1. 1. |> should be True
    within 0u -1. -1. |> should be True
    within 0u 0. 0. |> should be True
    within 0u minus0 minus0 |> should be True
    within 0u 0. minus0 |> should be True
    within 0u minus0 0. |> should be True
    // adjacent
    within 1u 0. omicron |> should be True
    within 0u 0. omicron |> should be False
    within 1u minus0 omicron |> should be True
    within 0u minus0 omicron |> should be False
    within 1u 0. -omicron |> should be True
    within 0u 0. -omicron |> should be False
    within 1u minus0 -omicron |> should be True
    within 0u minus0 -omicron |> should be False
    within 0u 1. epsilon |> should be False
    within 1u 1. epsilon |> should be True
    within 2u 1. epsilon |> should be True
    within System.UInt32.MaxValue 1. epsilon |> should be True
    within 0u epsilon 1. |> should be False
    within 1u epsilon 1. |> should be True
    within 2u epsilon 1. |> should be True
    within System.UInt32.MaxValue epsilon 1. |> should be True
    // two steps apart
    within 2u 0. (2.*omicron) |> should be True
    within 1u 0. (2.*omicron) |> should be False
    within 0u 0. (2.*omicron) |> should be False
    within 1u omicron -omicron |> should be False
    within 2u omicron -omicron |> should be True
    within 1u -omicron omicron |> should be False
    within 2u -omicron omicron |> should be True
    //
    for one in [1.; -1.] do
        let some = seq {1..100} |> Seq.scan (fun f i -> f*epsilon) (one*epsilon)
        some |> Seq.mapi (fun i f -> within (uint32 i) one f) |> should not' (contain true)
        some |> Seq.mapi (fun i f -> within (uint32 i + 1u) one f) |> should not' (contain false)

[<Test>]
let ridders_tests() =
    ridders 0. (-1.0, -2.0) (fun x -> x) |> should equal None
    ridders 0. (1.0, 2.0) (fun x -> x) |> should equal None
    ridders 0. (-2.0, -1.0) (fun x -> x) |> should equal None
    ridders 0. (2.0, 1.0) (fun x -> x) |> should equal None
    ridders 0. (1.0, 0.0) (fun x -> x) |> should equal (Some 0.0)
    ridders 0. (0.0, 1.0) (fun x -> x) |> should equal (Some 0.0)
    ridders 0. (-1.0, 1.0) (fun x -> x) |> should equal (Some 0.0)
    ridders 0. (1.0, -1.0) (fun x -> x) |> should equal (Some 0.0)
    // for linear function the solution is exact on the first iteration
    ridders 0. (-1.0, 2.0) (fun x -> x) |> should equal (Some 0.0)
    ridders 1e-5 (-1.0, 2.0) (fun x -> x) |> should equal (Some 0.0)
    ridders 0. (1.0, -2.0) (fun x -> x) |> should equal (Some 0.0)
    ridders 1e-5 (1.0, -2.0) (fun x -> x) |> should equal (Some 0.0)
    // quadratic function is well-behaved
    ridders 0. (0.5, 2.0) (fun x -> x * (x-1.)) |> should equal (Some 1.0)
    ridders 1e-5 (-1.0, 2.0) (fun x -> x) |> should equal (Some 0.0)
    match ridders 1e-3 (0.5, 2.0) (fun x -> x * (x-1.)) with
    | None -> Assert.Fail "should not be None"
    | Some v -> v |> should (equalWithin 1e-3) 1.0
    match ridders 1e-15 (0.5, 2.0) (fun x -> x * (x-1.)) with
    | None -> Assert.Fail "should not be None"
    | Some v -> v |> should (equalWithin 1e-15) 1.0
    // check exit by within 1u
    match ridders 0.0 (0.5, 2.0) (fun x -> if x < 1. then -1.0 else 1.0) with
    | None -> Assert.Fail "should not be None"
    | Some v -> 
        v |> should not' (equal 1.)
        within 1u v 1. |> should be True
    // check bisection branch when discriminant vanishes to 0
    match ridders 0.0 (0.5, 2.0) (fun x -> (sqrt System.Double.Epsilon) * (x-1.)) with
    | None -> Assert.Fail "should not be None"
    | Some v ->  within 1u v 1. |> should be True


[<Test>]
let KernelDensityEstimation_2() =
    let x,y = kde 2 [| 0.0; 1.0 |]
    x |> should (equalWithin 1.e-12) [| -0.1; 1.1 |]
    y |> should (equalWithin 1.e-12) [| 1.0/1.2; 1.0/1.2 |]

[<Test>]
let KernelDensityEstimation_Normal() =
    let data = [| 0.204644865259654; -0.144545587125715; -0.118956445994713; -0.0469338391365766; 0.03745006601189; 0.0474434648487626; 
        0.0489947779088937; -0.023541123128428; -0.141367168805941; 0.0209207976156952; 0.124715845091793; -0.0255870975000263; 0.0146939287935733; 
        -0.0125076454183801; -0.0451149615797648; 0.0844723611892559; 0.122250651498093; 0.0404912275709768; -0.237762060020886; -0.062608563178955; 
        0.0011964951706472; 0.251393099849191; 0.100797742833248; -0.00924513612403402; 0.00391431437480729; -0.00179251318410974; -0.0496055274851082;
        0.0139763147460233; -0.022857038101599; -0.0156334781559978; -0.117729953886438; -0.0145329856473145; 0.168665446368054; -0.125197371233141; 
        0.140664523630094; -0.151865856740158; -0.0512219960203086; -0.0782859725775293; 0.051599570148176; -0.150777387039718; -0.125869365367987; 
        -0.0701060122655738; -0.0362676987446099; -0.110384234156303; -0.0560945580954171; -0.0799446580772691; 0.0691093208986571; 0.0484885695433568; 
        0.0340971746323898; -0.06710993 |]

    let x,y = kde 16 data

    // Computed by Haskell's "kde":
    let xs = [| -0.28667757600789373; -0.24754516321828757; -0.2084127504286814; -0.16928033763907527; -0.1301479248494691; -9.101551205986294e-2; -5.1883099270256805e-2; -1.2750686480650641e-2; 2.6381726308955522e-2; 6.551413909856169e-2; 0.10464655188816785; 0.143778964677774; 0.18291137746738012; 0.22204379025698634; 0.26117620304659245; 0.30030861583619867 |]
    x |> should (equalWithin 1.e-12) xs

    let ys = [| 0.3379500767022503; 0.5684242002094704; 1.0274500185402244; 1.6464757237918854; 2.308063063786562; 2.9326163272608494; 3.42003145533083; 3.5678322580123796; 3.2496570321332054; 2.6063628556585825; 1.9158662533355955; 1.352070206249444; 0.938051703727766; 0.6397244704517325; 0.43125358539892195; 0.3160521681770156 |]
    y |> should (equalWithin 0.5) ys

    // from previous version of Angara.Math.KDE
    let ys' = [|
                0.35517311013031783
                0.64705323814149784
                1.1707958297235739
                1.7912613969845474
                2.3510114101830766
                2.7648724576259442
                3.0276889436729015
                3.1184456454369429
                2.9667295647726752
                2.5660398727253959
                2.0422444692266968
                1.5465126340059054
                1.1396742615948274
                0.81015114105654906
                0.55401464652080867
                0.40621277696505254
                |]
    y |> should (equalWithin 1.e-12) ys'
