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
    member x.Add(name, values, lower, upper, ?delay, ?isLog, ?prior) =
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
    member x.SetValues(values) =
        if Array.length values <> Array.length pvalues then
            invalidArg "values" "invalid length of the array."
        Parameters(pdefs, values)
    member x.GetValue idx = pvalues.[idx]
    member x.GetValue name =
        let d = pdefs.[name]
        if d.size>1 then invalidOp ("use getValue(name,index) for the vector parameter "+name+".")
        pvalues.[d.index]
    member x.GetValue(name,idx) = 
        let d = pdefs.[name]
        if idx<0 || idx >= d.size then raise(System.IndexOutOfRangeException())
        pvalues.[d.index + idx]
    member x.GetValues name = avalue pdefs.[name]
    member x.AllValues = Seq.ofArray pvalues
    member x.CountValues = Array.length pvalues
    member x.GetDefinition name = pdefs.[name]
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

//type internal psampler = {
//    value: float
//    prior: Distribution option
//    ival: float
//    decis: int
//    delta: float
//    pold: float
//    padd: float
//    altt: int
//    runalt: int
//    runacc: int
//    }
type Sampler private (pp:Parameters,
                      logl: Parameters -> float,
                      // utilities
                      pall: ParameterDefinition[],
                      // variables
                      metr_k: int,
                      rng: MT19937,
                      values: float[],
                      deltas:float[],
                      ltotold: float,
                      ptotold:float,
                      pbcount: int, // probe count
                      pbacc: int, // accepted probes
                      runalt:int[], // number of alterations of individual parameters
                      runacc:int[] // number of accepted alterations of individual parameters
    ) =
    let log_likelihood values = pp.SetValues values |> logl
    let log_prior values = Array.fold2 (fun sum d v -> 
            match d.prior with None -> sum | Some p -> sum + log_pdf p v) 0. pall values
    member x.Create(pp: Parameters, rng: MT19937, logl: Parameters -> float) =
        // init_chains
        let pall =
            pp.definitions 
            |> Seq.map (fun kv -> let d = kv.Value in Array.create d.size d)
            |> Array.concat
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
//        let pss = 
//            pp.definitions 
//            |> Seq.map (fun kv ->
//                let def = kv.Value
//                Array.init def.size (fun i ->
//                    // initRandomValues
//                    let v = (if def.delay<1 
//                            then Uniform(def.lower, def.upper) |> draw rng
//                            else pp.values.[def.index + i]);
//                    {ival=0.; decis=0; prior=def.prior;
//                    // initStepSizes 
//                    delta = (if def.isLog
//                            then 0.50*(log def.upper - log def.lower)
//                            else 0.50*(def.upper - def.lower));
//                    value = v;
//                    // init_paramerniformation
//                    pold = v; 
//                    padd=0.; altt=0; runalt=0; runacc=0}))
//            |> Array.concat
        // init_counters
        let metr_number_ok = 0
        let pbacc = 0
        let pbcount = 0
        // init_bayestable
        let bayestable = []
        // init_likelihood
        let ltotold = log_likelihood values
        let ptotold =  log_prior values
        // initialize iteration number
        let metr_k = 1
        Sampler(pp, logl, pall, metr_k, rng, values, deltas, ltotold, ptotold, 0, 0, runalt, runacc)
    member x.BurinIn() = 
        let paramcount = pall.Length
        let rng = MT19937(rng)
        let newvalues = Array.copy values
        let deltas = Array.copy deltas
        let runalt = Array.copy runalt
        let runacc = Array.copy runacc

        // Select parameters to change. Most of the time change only one, two or three params.
        let alterable = [for i in 0..paramcount-1 do if pall.[i].delay < metr_k then yield i]
        let freeparamcount = alterable.Length
        //TODO: if freeparamcount=0 then {this with metr_k = metr_k + 1} else
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
            let old = newvalues.[i]
            newvalues.[i] <- infinity
            while newvalues.[i]<pall.[i].lower || newvalues.[i]>pall.[i].upper do
                let add = rng.normal() * deltas.[i]
                if pall.[i].isLog then
                    newvalues.[i] <- old * exp(add)
                else
                    newvalues.[i] <- old + add

        // calc new lnlike
        let ltotnew = log_likelihood newvalues
        let ptotnew = log_prior newvalues

        // compare new to old and accept or reject -- METROPOLIS CRITERION IS IN HERE
        let accept = 
            let dlik = (ltotnew+ptotnew) - (ltotold+ptotold)
            dlik >= 0. ||
            (let rndnum = max 0.00000010 (min 0.99999990 (rng.uniform_float64()))
            log rndnum < dlik)

        // act on acceptance
        let bcount = pbcount + 1
        let pbacc = if accept then pbacc + 1 else pbacc
        for i in alt do
            runalt.[i] <- runalt.[i]+1
            if accept then runacc.[i] <- runacc.[i]+1

        // adjust jump steps? only if in burnin
        for ii in 0..paramcount-1 do
            if runalt.[ii]=20 then
                if runacc.[ii] < 5 then
                    // decrease temperature by 20%
                    deltas.[ii] <- deltas.[ii] * 0.80
                elif runacc.[ii] > 5 then
                    // increase temperature by 20%
                    deltas.[ii] <- deltas.[ii] * 1.20
                runalt.[ii] <- 0
                runacc.[ii] <- 0
                //if pall.[ii].isLog then

        if (accept) then
            Sampler(pp, logl, pall, metr_k, rng, newvalues, deltas, ltotnew, ptotnew, pbcount, pbacc, runalt, runacc)
        else
            Sampler(pp, logl, pall, metr_k, rng, values, deltas, ltotold, ptotold, pbcount, pbacc, runalt, runacc)
    (*

			/* adjust jump steps? only if in burnin */
			if (metr_k < burnin)
			{
				for (int ii = 0; ii < paramcount; ii++)
				{
					if (chain_params[ii].runalt == 20 && chain_params[ii].fixed < 1)
					{
						chain_params[ii].runalt = 0;

						if (chain_params[ii].runacc < 5)
						{
							/* decrease temperature */
							chain_params[ii].delta *= 0.80;
						}
						if (chain_params[ii].runacc > 5)
						{
							/* increase temperature */
							chain_params[ii].delta *= 1.20;
						}
						chain_params[ii].runacc = 0;

						if (chain_params[ii].type > 0) {
							if (chain_params[ii].delta < 0.010)
								chain_params[ii].delta = 0.010;
							if (chain_params[ii].delta > 10.0)
								chain_params[ii].delta = 10.0;
						}
						else {
							if (chain_params[ii].delta < (0.0010*(chain_params[ii].ub - chain_params[ii].lb)))
								chain_params[ii].delta = (0.0010*(chain_params[ii].ub - chain_params[ii].lb));
							if (chain_params[ii].delta > (0.50*(chain_params[ii].ub - chain_params[ii].lb)))
								chain_params[ii].delta = (0.50*(chain_params[ii].ub - chain_params[ii].lb));
						}
					} // if runalt == 20
				} // ii loop
			} // if still in first burnin

			/* keeping track of decisions thing */
			for (int ii = 0; ii < paramcount; ii++)
				if (chain_params[ii].alt < 1)
					chain_params[ii].decis = 0;

			if (metr_k % bayes_step == 0 && metr_k >= tburnin  && metr_k <= (tburnin + teststeps) && accept_flag)
			{
				// write sample into memory
				fill_bayestable(chain, metr_k);
				accept_flag = false;
			} // if outputting sample

			/* -------------- write to another file that logs everything (this file not used any more) ------------- */
			if (accept && out_chain)
			{
#pragma omp critical
			{
				mfile = workspace_fopen(out_chain_name, "a");
				// output chain, iteration
				fprintf(mfile, "%d\t%d\t%lf\t%lf", chain, metr_k, ltotnew[chain] + ptotnew[chain], ltotnew[chain]);
				// parameter values
				param* p = current_para();
				for (int i = 0; i < paramcount; i++)
				{
					fprintf(mfile, "\t%lf", p[i].value);
				}
				fprintf(mfile, "\n");
				fclose(mfile);
			}

			}

			/* unforce any forced accepts, rejects */
			accept_forced[currentchain] = 0;
			reject_forced[currentchain] = 0;
    *)
    member x.Sample() = ()
