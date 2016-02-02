module Angara.Filzbach

type ParameterDefinition = {
    index: int
    size: int
    lower: float
    upper: float
    isLog: bool
    isFixed: bool
    }

type IParameters = System.Collections.Generic.IReadOnlyDictionary<string, float[]>

type Parameters private (pdefs: Map<string,ParameterDefinition>, pvalues: float[]) =
    let defaultLog isLog = defaultArg isLog false 
    let defaultFixed isFixed = defaultArg isFixed false
    let avalue pdef = Array.sub pvalues pdef.index pdef.size
    let all = pdefs |> Seq.map (fun kv -> 
        System.Collections.Generic.KeyValuePair<string, float[]>(kv.Key, avalue kv.Value))
    do
        assert(Array.length pvalues = Map.fold (fun sum _ def -> sum + def.size) 0 pdefs)
    new() = Parameters(Map.empty,[||])
    member private x.definitions = pdefs
    member private x.values = pvalues
    member x.Add(name, values, lower, upper, ?isLog, ?isFixed) =
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
                isFixed = defaultFixed isFixed || lower=upper
                }, 
            Array.append pvalues values)
    member x.Add(name, value, lower, upper, ?isLog, ?isFixed) =
        // by default, the parameter is a scalar value
        x.Add(name, [|value|], lower, upper, defaultLog isLog, defaultFixed isFixed)
    member x.Add(name, size:int, value:float, lower:float, upper:float, ?isLog, ?isFixed) = 
        if size<1 then
            invalidArg "size" "parameter size must be 1 or greater."
        x.Add(name, Array.create size value, lower, upper, defaultLog isLog, defaultFixed isFixed)
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



