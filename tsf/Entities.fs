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
    type ObservationValues = { OIdx:ObservationIndex; Values:float seq } with
        static member public iterate (consFreq:FrequencyType) (ov:ObservationValues) = 
            let checkConsFreq oldf newf = 
                if FrequencyIndex.max oldf >  FrequencyIndex.max newf then Ok newf
                else Error [InvalidConsolidationOperation]

            let getConsDivisor consFreqMax oiFreqMax  = 
                oiFreqMax / consFreqMax

            let getConsOIdx d ov = 
                let (CardinalType fIdx) = ov.OIdx.Idx
                fIdx / d

            // let getConsOIdxSeq oidx length = 
            //     Ok ((Seq.unfold (fun acc -> Some (acc, (ObservationIndex.increment acc 1))) oidx) |> Seq.take length) 

            let getConsOIdxSeq consoidx oidx length d = 
                let s = seq {
                    let mutable j = 0
                    for i in 0 .. length - 1 do
                        let oi = ObservationIndex.increment oidx i
                        let (CardinalType c) = oi.Idx
                        if (c + 1) % d = 0 then
                            yield oi
                            yield ObservationIndex.increment consoidx j
                            j <- j + 1
                        else yield oi
                }
                Ok s

            result {
                let! y' = Ok ov.OIdx.Year
                and! f' = checkConsFreq ov.OIdx.Freq consFreq

                let oiFreqMax = FrequencyIndex.max ov.OIdx.Freq
                let consFreqMax = FrequencyIndex.max consFreq
                let length = Seq.length ov.Values

                let m' = getConsDivisor consFreqMax oiFreqMax
                let i' = getConsOIdx m' ov
                let oi' = { _year = y'; _freq = f'; _idx = (CardinalType i') } 

                // return! (getConsOIdxSeq oi' length)
                return! (getConsOIdxSeq oi' ov.OIdx length m')
            }
