namespace tsf

open System
open Microsoft.FSharp.Reflection
open System.Text.RegularExpressions
open Utilities

module Entities =
    [<Literal>]
    let NaN = System.Double.MaxValue

    type FrequencyType = D|M|Q|S|A with 
        override this.ToString() = Utilities.DiscriminatedUnionToString<FrequencyType> this
        static member FromString s = Utilities.StringToDiscriminatedUnion<FrequencyType> s

    [<Struct>]
    type ConsolidateMethod = Sum|Average with
        override this.ToString() = Utilities.DiscriminatedUnionToString<ConsolidateMethod> this
        static member FromString s = Utilities.StringToDiscriminatedUnion<ConsolidateMethod> s

    [<Struct>]
    type FrequencyIndex = private FrequencyIndex of int with 
        static member max f = 
            match f with
            | FrequencyType.A -> 1
            | FrequencyType.S -> 2                    
            | FrequencyType.Q -> 4
            | FrequencyType.M -> 12
            | FrequencyType.D -> 366
        static member value (FrequencyIndex i) = i  

    [<Struct>]
    type Year = private Year of int with 
        static member public create yr =
            if yr < 1 || yr > 9999 then
                Error [InvalidYear]
            else
                Ok (Year yr)

        static member public value (Year y) = y

    [<Struct>]
    type ObservationValue = private ObservationValue of float
    module ObservationValue =
        let create value = (ObservationValue value)
        let value (ObservationValue value) = value

    [<Struct>]
    type ObservationIndex = private { _year:Year; _freq:FrequencyType; _idx: FrequencyIndex } with 
        member public this.Year = this._year
        member public this.Freq = this._freq
        member public this.Idx = this._idx

        static member create y f i = 
            let l = [1..(FrequencyIndex.max f)]
            let cFI i = 
                if List.contains i l then Ok (FrequencyIndex i)
                else Error [InvalidObservationIndex]

            result {
                let! y' = Year.create y
                and! i' = cFI i
                return! Ok { _year = y'; _freq = f; _idx = i' }
            }
        
        static member private create' y f i = 
            result {
                let y' = y |> int 
                let! f' = FrequencyType.FromString f 
                let i' = i |> int 
                return! (ObservationIndex.create y' f' i') 
            }
            
        static member convert input =                 
            let pattern = @"^([0-9]{4})([ASQMD])([0-9]+)"
            try
                let m = Regex.Match(input, pattern)
                if m.Success then 
                    // Ok (List.tail [ for g in m.Groups -> g.Value ])
                    let y = m.Groups[1].Value
                    let f = m.Groups[2].Value
                    let i = m.Groups[3].Value
                    ObservationIndex.create' y f i
                else 
                    Error [InvalidObservationIndex]
            with
                | ex -> Error [InvalidObservationIndex] 
            
    [<Struct>]
    type ObservationValues = private { _oidx:ObservationIndex; _values:float seq }with
        member this.OIdx = this._oidx
        member this.Values = this._values

        static member public create oidx values  = 
            Ok { _oidx = oidx; _values = values }

        static member public consolidate fCons (mCons:ConsolidateMethod) (ov:ObservationValues) = 
            let validateFrequency oldf newf = 
                if FrequencyIndex.max oldf >  FrequencyIndex.max newf then Ok newf
                else  Error [InvalidConsolidationOperation]

            let getIdx (i:FrequencyIndex) =
                Ok (FrequencyIndex 0)

            let calculate f axis s = 
                Seq.empty

            result {
                let y' = ov.OIdx.Year
                let! f' = validateFrequency ov.OIdx.Freq fCons
                let! i' = getIdx ov.OIdx.Idx
                let oi = { _year = y'; _freq = f'; _idx = i' }
                // return!
            }
