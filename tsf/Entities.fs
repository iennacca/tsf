namespace tsf

open System
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
    // [NOTE]: Named both union cases because of error FS3204 (nonsensical in this case IMHO) 
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
    // [TODO]: Combine ObservationValue type and module
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
                let l = [0..((FrequencyIndex.max f)-1)]
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

        static member increment (oi:ObservationIndex) (inc:int) : ObservationIndex  = 
            match oi.Idx with 
            | CardinalType i ->
                let maxfreq = FrequencyIndex.max oi.Freq
                let mutable (Year y) = oi.Year
                let mutable idx = i
                while idx + inc > maxfreq do
                    y <- y + 1
                    idx <- idx - maxfreq
                idx <- idx + inc 
                { _year = (Year y); _freq = oi.Freq; _idx = (CardinalType idx)}

            | DateType d -> raise (NotImplementedException "Unimplemented increment")
            
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
 
        override this.ToString () = 
            sprintf "[%A %A %A]" this.Year this.Freq this.Idx

    [<Struct>]
    type ObservationValues = { OIdx:ObservationIndex; Values:float seq }

    module ObservationValueConsolidator =
        let private checkConsFreq oldf newf = 
            if FrequencyIndex.max oldf >  FrequencyIndex.max newf then Ok newf
            else Error [InvalidConsolidationOperation]

        // [NOTE]: Unusable for DateType
        let private getConsDivisor consFreq oiFreq  = 
            let oiFreqMax = FrequencyIndex.max oiFreq
            let consFreqMax = FrequencyIndex.max consFreq
            oiFreqMax / consFreqMax

        // [NOTE]: Unusable for DateType
        let private getConsFIdx d ovIdx = 
            let (CardinalType fIdx) = ovIdx
            fIdx / d

        let private getConsOIdx (consFreq:FrequencyType) (oi:ObservationIndex) (ov:seq<float>)=
            result {
                let! y' = Ok oi.Year
                and! f' = checkConsFreq oi.Freq consFreq

                let d' = getConsDivisor consFreq oi.Freq
                let i' = getConsFIdx d' oi.Idx
                return! Ok { _year = y'; _freq = f'; _idx = (CardinalType i') } 
            }

        // member this.getConsOIdxSeq oidx length = 
        //     Ok ((Seq.unfold (fun acc -> Some (acc, (ObservationIndex.increment acc 1))) oidx) |> Seq.take length) 

        // [TODO]: Remove type definition on consoidx?
        let private getConsOIdxSeq (consoidx:ObservationIndex) ov =
            let d = getConsDivisor consoidx.Freq ov.OIdx.Freq  
            let s = seq {
                let mutable j = 0
                
                let length = Seq.length ov.Values
                for i in 0 .. length - 1 do
                    let oi = ObservationIndex.increment ov.OIdx i
                    let (CardinalType c) = oi.Idx

                    yield oi
                    if (c + 1) % d = 0 then
                        yield ObservationIndex.increment consoidx j
                        j <- j + 1
            }
            Ok s

        let private getConsOValuesSeq (consoidx:ObservationIndex) cons ov = 
            let d = getConsDivisor consoidx.Freq ov.OIdx.Freq  
            let s = seq {
                let mutable i = 0
                let mutable j = 0
                let mutable l = []
                
                for v in ov.Values do
                    let oi = ObservationIndex.increment ov.OIdx i
                    i <- i + 1
                    let (CardinalType c) = oi.Idx

                    l <- List.append l [v]
                    yield (v, ov.OIdx)

                    if (c + 1) % d = 0 then
                        yield (cons l, ObservationIndex.increment consoidx j)
                        l <- []
                        j <- j + 1
            }
            Ok s

        let iterate consFreq oValues = 
            result {
                let! oi' = getConsOIdx consFreq oValues.OIdx oValues.Values
                return! (getConsOIdxSeq oi' oValues)
            }

        let consAdd l = 
            List.fold (+) 0.0 l

        let consolidate consFreq oValues = 
            result {
                let! oi' = getConsOIdx consFreq oValues.OIdx oValues.Values
                return! (getConsOValuesSeq oi' consAdd oValues)
            }
