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
    //Named both union cases because of error FS3204 (nonsensical in this case IMHO) 
    type FrequencyIndex = CardinalType of c:int | DateType of d:DateOnly with
        static member max f = 
            match f with
            | FrequencyType.A -> 1
            | FrequencyType.S -> 2                    
            | FrequencyType.Q -> 4
            | FrequencyType.M -> 12
            | FrequencyType.D -> 366

    [<Struct>]
    type Year = private Year of int with 
        static member public create yr =
            if yr < 1 || yr > 9999 then
                Error [InvalidYear]
            else
                Ok (Year yr)

        static member public value (Year y) = y

    [<Struct>]
    type ObservationValue = private ObservationValue of float with 
        static member private value (ObservationValue v) = v 

    module ObservationValue =
        let create value = (ObservationValue value)
        let value (ObservationValue value) = value

    [<Struct>]
    type ObservationIndex = private { _year:Year; _freq:FrequencyType; _idx: FrequencyIndex } with 
        member public this.Year = this._year
        member public this.Freq = this._freq
        member public this.Idx = this._idx

        static member create y f i = 
            let cFI f i = 
                let l = [1..(FrequencyIndex.max f)]
                if List.contains i l then Ok (FrequencyIndex.CardinalType i)
                else Error [InvalidObservationIndex]

            result {
                let! y' = ToResult int y 
                and! f' = FrequencyType.FromString f 
                and! i' = ToResult int i 

                let! y'' = Year.create y'
                and! i'' = cFI f' i'
                return! Ok { _year = y''; _freq = f'; _idx = i'' }
            }
            
        static member FromString input =                 
            let pattern = @"^([0-9]{4})([ASQMD])([0-9]+)"
            try
                let m = Regex.Match(input, pattern)
                if m.Success then 
                    // Ok (List.tail [ for g in m.Groups -> g.Value ])
                    let y = m.Groups[1].Value
                    let f = m.Groups[2].Value
                    let i = m.Groups[3].Value
                    ObservationIndex.create y f i
                else 
                    Error [InvalidObservationIndex]
            with
                | ex -> Error [InvalidObservationIndex] 
            
    [<Struct>]
    type ObservationValues = { OIdx:ObservationIndex; Values:float seq } with
        static member public test (consFreq:FrequencyType) (ov:ObservationValues) = 
            let validateFrequency oldf newf = 
                if FrequencyIndex.max oldf >  FrequencyIndex.max newf then Ok newf
                else  Error [InvalidConsolidationOperation]

            let getConsOIdx consFreqMax ov = 
                let (CardinalType fIdx) = ov.OIdx.Idx
                fIdx / consFreqMax

            let getConsModulo consFreqMax oiFreqMax  = 
                oiFreqMax % consFreqMax

            let iterate oiFreqMax oi = 
                Ok (Seq.unfold (fun acc -> (Some (acc, (acc + 1) % oiFreqMax))) oi) 
                
            result {
                let! y' = Ok ov.OIdx.Year
                and! f' = validateFrequency ov.OIdx.Freq consFreq

                let oiFreqMax = FrequencyIndex.max ov.OIdx.Freq
                let consFreqMax = FrequencyIndex.max consFreq

                let (CardinalType i) = ov.OIdx.Idx
                let i' = getConsOIdx consFreqMax ov
                let d' = getConsModulo consFreqMax oiFreqMax
                let oi' = { _year = ov.OIdx.Year; _freq = f'; _idx = (CardinalType i') } 
                return! iterate oiFreqMax i 
            }
