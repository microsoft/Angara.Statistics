module Angara.Filzbach
open Angara.Statistics

type ParameterDefinition = 
    {
    index: int
    size: int
    lower: float
    upper: float
    isLog: bool
    /// If delay<1, the sampler initializes the parameter value with a random number.
    /// If delay=1, the sampler starts with the value from the definition record.
    /// If delay>1, the sampler doesn't change the parameter value for the first 'delay' iterations.
    delay: int
    prior: Distribution option
    } 
    member x.isFixed = x.delay = System.Int32.MaxValue

type IParameters = System.Collections.Generic.IReadOnlyDictionary<string, float[]>

/// A container for model parameters.
type Parameters private (pdefs: Map<string,ParameterDefinition>, pvalues: float[]) =
    let defaultLog isLog = defaultArg isLog false 
    let defaultDelay delay = defaultArg delay 0
    let nonInformative = Uniform(-infinity,infinity)
    let defaultPrior prior = defaultArg prior nonInformative
    let avalue pdef = Array.sub pvalues pdef.index pdef.size
    let all = pdefs |> Seq.map (fun kv -> 
        System.Collections.Generic.KeyValuePair<string, float[]>(kv.Key, avalue kv.Value))
    do
        assert(Array.length pvalues = Map.fold (fun sum _ def -> sum + def.size) 0 pdefs)
    new() = Parameters(Map.empty,[||])
    member internal x.definitions = pdefs
    member internal x.values = pvalues

    /// <summary>Adds a parameter to the container.</summary>
    /// <param name="name">Parameter name.</param>
    /// <param name="values">An array of one or more parameter values.</param>
    /// <param name="lower">Lower bound of parameter values.</param>
    /// <param name="upper">Upper bound of parameter values.</param>
    /// <param name="delay">Sampler behaviour: &lt;1 (default) -- initialize values with random numbers;
    /// =1 -- start with the values supplied in the call;
    /// &gt;1 -- release the parameter values after 'delay' iterations.</param>
    /// <param name="isLog">If true, the sampler will use logarithmic transform for the parameter.
    /// The default is false.</param>
    /// <param name="prior">Prior knowledge. The default is a non-informative prior.</param>
    member x.Add(name, values,  lower, upper, ?delay, ?isLog, ?prior) =
        if name = null || name = "" || Map.containsKey name pdefs then 
            invalidArg "name" "parameters must have unique non-empty names."
        if Array.length values < 1 then
            invalidArg "values" "empty values array, each parameter must have at least one value."
        if lower > upper then
            invalidArg "lower" "lower must not be greater than upper."
        values |> Array.iteri (fun i v ->
            if v < lower || v > upper then
                invalidArg "values" (sprintf "values[%d] is out of [lower..upper] range" i))
        if isLog.IsSome && isLog.Value && lower <= 0. then 
            invalidArg "lower" "lower must not be greater than upper."
        Parameters(
            pdefs |> Map.add name {
                index = Array.length pvalues 
                size = Array.length values
                lower = lower
                upper = upper
                isLog = defaultLog isLog
                delay = (if lower=upper then -1 else defaultDelay delay)
                prior = (let pr = defaultPrior prior in if pr = nonInformative then None else Some pr)
                }, 
            Array.append pvalues values)
    member x.Add(name:string, value:float, lower:float, upper:float, ?isFixed, ?isLog, ?prior) =
        // by default, the parameter is a scalar value
        x.Add(name, [|value|], lower, upper, (if defaultArg isFixed false then System.Int32.MaxValue else 0), defaultLog isLog, defaultPrior prior)
    member x.Add(name, value, lower, upper, delay, ?isLog, ?prior) =
        // by default, the parameter is a scalar value
        x.Add(name, [|value|], lower, upper, delay, defaultLog isLog, defaultPrior prior)
    member x.Add(name, size, value, lower, upper, ?isFixed, ?isLog, ?prior) = 
        if size<1 then
            invalidArg "size" "parameter size must be 1 or greater."
        x.Add(name, Array.create size value, lower, upper, (if defaultArg isFixed false then System.Int32.MaxValue else 0), defaultLog isLog, defaultPrior prior)
    member x.Add(name, size, value, lower, upper, delay, ?isLog, ?prior) = 
        if size<1 then
            invalidArg "size" "parameter size must be 1 or greater."
        x.Add(name, Array.create size value, lower, upper, delay, defaultLog isLog, defaultPrior prior)

    /// Replaces all parameter values
    member x.SetValues
            /// An array of all parameters values.
            /// For a parameter 'name' the parameter values start with index '.GetDefinition(name).index' in the array.
            values =
        if Array.length values <> Array.length pvalues then
            invalidArg "values" "invalid length of the array."
        Parameters(pdefs, values)

    /// Fast access to parameter values. See <see cref="SetValues"/> for explanation of indices.
    member x.GetValue idx = pvalues.[idx]

    /// Get a value of a scalar parameter.
    member x.GetValue name =
        let d = pdefs.[name]
        if d.size>1 then invalidOp ("use getValue(name,index) for the vector parameter "+name+".")
        pvalues.[d.index]

    /// Get a value of a vector parameter.
    member x.GetValue(name,idx) = 
        let d = pdefs.[name]
        if idx<0 || idx >= d.size then raise(System.IndexOutOfRangeException())
        pvalues.[d.index + idx]

    /// Get an array of all values of a parameter.
    member x.GetValues name = avalue pdefs.[name]

    /// Get all values of all parameters.
    /// For a parameter 'name' the parameter values start with index '.GetDefinition(name).index' in the sequence.
    member x.AllValues = Seq.ofArray pvalues

    /// Total number of all parameter values.
    member x.CountValues = Array.length pvalues

    /// Get a parameter definition.
    member x.GetDefinition name = pdefs.[name]

    /// Get a parameter name by value index.
    member x.GetName idx =
        if idx < 0 || idx >= Array.length pvalues then raise(System.IndexOutOfRangeException())
        Map.findKey (fun n d -> idx >= d.index && idx < d.index+d.size) pdefs

    interface IParameters with
        member x.Count = pdefs.Count
        member x.ContainsKey key = pdefs.ContainsKey key
        member x.Keys = pdefs |> Seq.map (fun kv -> kv.Key)
        member x.Values = all |> Seq.map (fun kv -> kv.Value)
        member x.get_Item key = avalue pdefs.[key]
        member x.TryGetValue(key, value: byref<float[]>) =
            if pdefs.ContainsKey key then
                value <- avalue pdefs.[key]
                true
            else false
    interface System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<string,float[]>> with
        member x.GetEnumerator() = all.GetEnumerator()
    interface System.Collections.IEnumerable with
        member x.GetEnumerator() = (all:>System.Collections.IEnumerable).GetEnumerator()
    override x.Equals other =
        match other with
        | :? Parameters as y -> y.definitions = x.definitions && y.values = x.values
        | _ -> false
    override x.GetHashCode() = pdefs.GetHashCode() ^^^ pvalues.GetHashCode()


/// An immutable state of Filzbach MCMC sampler.
type Sampler private (logl: Parameters -> float,
                      // utilities
                      pall: ParameterDefinition[],
                      // variables
                      metr_k: int,
                      rng: MT19937,
                      pp:Parameters,
                      deltas:float[],
                      ltotold: float,
                      ptotold:float,
                      accept: bool, // the probe has been accepted
                      runalt:int[], // number of alterations of individual parameters
                      runacc:int[] // number of accepted alterations of individual parameters
    ) =
    static let log_prior pall values = Array.fold2 (fun sum d v -> 
            match d.prior with None -> sum | Some p -> sum + log_pdf p v) 0. pall values
    static let make_pall (pp:Parameters) =
        let pall = Array.zeroCreate pp.CountValues
        for kv in pp.definitions do
            let pdef = kv.Value
            let index = pdef.index
            for offset in 0..pdef.size-1 do
                pall.[index+offset] <- pdef
        pall

    static member Create(pp: Parameters, rng: MT19937, logl: Parameters -> float) =
        // init_chains
        let pall = make_pall pp
        let paramcount = pall.Length
        // initRandomValues
        let values = pall |> Array.mapi (fun i def ->
            if def.delay<1 
            then Uniform(def.lower, def.upper) |> draw rng
            else pp.values.[def.index + i])
        // initStepSizes 
        let deltas = pall |> Array.map (fun def ->
            if def.isLog
            then 0.50*(log def.upper - log def.lower)
            else 0.50*(def.upper - def.lower))
        let runalt = Array.create paramcount 0
        let runacc = Array.create paramcount 0
        // init_counters
        let metr_number_ok = 0
        let pbacc = 0
        let pbcount = 0
        // init_bayestable
        let bayestable = []
        // init_likelihood
        let ltotold = pp.SetValues values |> logl
        let ptotold =  log_prior pall values
        // initialize iteration number
        let metr_k = 1
        Sampler(logl, pall, metr_k, rng, pp.SetValues values, deltas, ltotold, ptotold, false, runalt, runacc)

    member x.Probe(isBurnIn:bool) =
        let paramcount = pall.Length
        let rng = MT19937(rng)
        let values = Array.copy pp.values
        let deltas = Array.copy deltas
        let runalt = Array.copy runalt
        let runacc = Array.copy runacc

        // Select parameters to change. Most of the time change only one, two or three params.
        let alterable = [for i in 0..paramcount-1 do if pall.[i].delay < metr_k then yield i]
        let freeparamcount = alterable.Length
        if freeparamcount=0 then 
            Sampler(logl, pall, metr_k+1, rng, pp, deltas, ltotold, ptotold, accept, runalt, runacc)
        else
        let alt = // chain_params[i].alt=1 ~ alt |> List.any (fun item -> item=i)
            if freeparamcount=1 then alterable // one parameter always alters
            elif rng.uniform_float64() < 0.670 then
                // choose one param to alter
                let rnd = alterable.[rng.uniform_int(freeparamcount-1)]
                //alter parameters close by?
                [
                    if (rnd-1 >= 0) && (pall.[rnd-1].delay < metr_k) && (rng.uniform_float64() < 0.5)
                        then yield rnd-1
                    yield rnd
                    if (rnd+1 < paramcount) && (pall.[rnd+1].delay < metr_k) && (rng.uniform_float64() < 0.5)
                        then yield rnd+1
                ]
            else
                // change many parameters at once
                // draw prob change for this iteration
                let palt = min 0.99 (3.0/(float freeparamcount) * exp(4.0*(rng.uniform_float64() - 0.50)))
                // number of parameters to choose
                let nalt = max 1 (Binomial(freeparamcount, palt) |> draw rng |> int)
                // Knuth shuffle
                let shuffle = Array.create freeparamcount 0
                for i in 1..freeparamcount-1 do
                    let j = rng.uniform_int i
                    if j < i then shuffle.[i] <- shuffle.[j]
                    shuffle.[j] <- i
                [for i in 0..nalt-1 -> alterable.[shuffle.[i]]]

        // change parameter values, i.e. make the 'jump'
        for i in alt do
            let old = values.[i]
            values.[i] <- infinity
            while values.[i]<pall.[i].lower || values.[i]>pall.[i].upper do
                let add = rng.normal() * deltas.[i]
                if pall.[i].isLog then
                    values.[i] <- old * exp(add)
                else
                    values.[i] <- old + add

        // calc new lnlike
        let ltotnew = pp.SetValues values |> logl
        let ptotnew = log_prior pall values

        // compare new to old and accept or reject -- METROPOLIS CRITERION IS IN HERE
        let accept = 
            let dlik = (ltotnew+ptotnew) - (ltotold+ptotold)
            dlik >= 0. ||
            (let rndnum = max 0.00000010 (min 0.99999990 (rng.uniform_float64()))
            log rndnum < dlik)

        // act on acceptance
        for i in alt do
            runalt.[i] <- runalt.[i]+1
            if accept then runacc.[i] <- runacc.[i]+1

        if isBurnIn then
            // adjust jump steps
            for ii in 0..paramcount-1 do
                if runalt.[ii]=20 then 
                    let dmin, dmax = 
                        let def = pall.[ii]
                        if def.isLog then 0.010, 10.0 //TODO: ????
                        else let full = def.upper-def.lower in 0.0010*full, 0.50*full
                    if runacc.[ii] < 5 then
                        // decrease temperature by 20%
                        deltas.[ii] <- max dmin (min dmax (deltas.[ii] * 0.80))
                    elif runacc.[ii] > 5 then
                        // increase temperature by 20%
                        deltas.[ii] <- max dmin (min dmax (deltas.[ii] * 1.20))
                    runalt.[ii] <- 0
                    runacc.[ii] <- 0

        if (accept) then
            Sampler(logl, pall, metr_k+1, rng, pp.SetValues values, deltas, ltotnew, ptotnew, accept, runalt, runacc)
        else
            Sampler(logl, pall, metr_k+1, rng, pp, deltas, ltotold, ptotold, accept, runalt, runacc)

    member x.Parameters = pp
    member x.LogLikelihood = ltotold
    member x.LogPrior = ptotold
    member x.SamplingWidths = Array.copy deltas
    member x.IsAccepted = accept

    static member runmcmc(pp, logl, burnCount, sampleCount, ?thinning, ?rng) =
        let thinning = defaultArg thinning 100
        if thinning<1 then invalidArg "thinning" "must be > 0."
        let rng = defaultArg rng (MT19937())
        // initialize sampler
        let mutable sampler = Sampler.Create(pp, rng, logl)
        // do burn-in iterations
        for _ in 1..burnCount do sampler <- sampler.Probe(true)
        // collect sampleCount samples
        let mutable countAccepted = 0
        let samples = 
            [
            for _ in 1..sampleCount ->
                for _ in 1..thinning do
                    sampler <- sampler.Probe(false)
                    if sampler.IsAccepted then countAccepted <- countAccepted + 1
                Array.copy sampler.Parameters.values, sampler.LogLikelihood, sampler.LogPrior
            ]
        sampler, samples, ((float countAccepted) / (float sampleCount * float thinning))

    static member print(sampler:Sampler, samples:(float[]*float*float) seq, acceptanceRate) =
        printfn "Samples max log likelihood*prior = %g, acceptance rate at sampling = %5.3f" 
                    (samples |> Seq.map (fun (_,logl,logp) -> logl+logp) |> Seq.max)
                    acceptanceRate
        printfn "------------+------------+------------+------------+------------+------------+------------+------------+"
        printfn "       name |      lower |  lower 95%% |  lower 68%% |     median |  upper 68%% |  upper 95%% |      upper | prior"
        printfn "------------+------------+------------+------------+------------+------------+------------+------------+"
        for idx in 0..sampler.Parameters.CountValues-1 do
            let q = qsummary (samples |> Seq.map (fun (sample,_,_) -> sample.[idx]))
            let name = sampler.Parameters.GetName idx
            let pdef = sampler.Parameters.GetDefinition name
            let fullname = if pdef.size=1 then name else sprintf "%s[%d]" name (idx-pdef.index)
            printfn " %10s | %10g | %10g | %10g | %10g | %10g | %10g | %10g | %A"
                    (fullname.Substring(0,min 10 fullname.Length))
                    pdef.lower
                    q.lb95 q.lb68 q.median q.ub68 q.ub95
                    pdef.upper pdef.prior
            for off in 10..10..fullname.Length do
            printfn " %10s" (fullname.Substring(off,min 10 (fullname.Length-off)))
