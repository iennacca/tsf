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
        static member fromString s = Utilities.StringToDiscriminatedUnion<FrequencyType> s

    [<Struct>]
    type FrequencyIndex = private FrequencyIndex of int
    module FrequencyIndex =
        let max f = 
            match f with
            | FrequencyType.A -> 1
            | FrequencyType.S -> 2                    
            | FrequencyType.Q -> 4
            | FrequencyType.M -> 12
            | FrequencyType.D -> 366
        
        let seqInfinite f i =
            let m = max f
            let mutable i' = i
            while i' > m do i' <- i' - m 
            Seq.initInfinite (fun idx -> 
                if idx + 1 >= m then 0 else idx + 1)

    [<Struct>]
    type Year = private Year of int
    module Year =
        let create yr =
            if yr < 1 || yr > 9999 then
                Error [InvalidYearError]
            else
                Ok (Year yr)

        let value (Year y) = y

    [<Struct>]
    type ObservationValue = private ObservationValue of float
    module ObservationValue =
        let create value = (ObservationValue value)
        let value (ObservationValue value) = value

    [<Struct>]
    type ObservationIndex = private { Year:Year; Freq:FrequencyType; Idx: FrequencyIndex }
    module ObservationIndex =
        let create y f i = 
            let l = [1..(FrequencyIndex.max f)]
            let cFI i = 
                if List.contains i l then Ok (FrequencyIndex i)
                else Error [InvalidObservationIndexError]

            result {
                let! y' = Year.create y
                and! i' = cFI i
                return! Ok { Year = y'; Freq = f; Idx = i' }
            }
        
        let private create' y f i = 
            result {
                let y' = y |> int 
                let! f' = FrequencyType.fromString f 
                let i' = i |> int 
                return! (create y' f' i') 
            }
            
        let convert input =                 
            let pattern = @"^([0-9]{4})([ASQMD])([0-9]+)"
            try
                let m = Regex.Match(input, pattern)
                if m.Success then 
                    // Ok (List.tail [ for g in m.Groups -> g.Value ])
                    let y = m.Groups[1].Value
                    let f = m.Groups[2].Value
                    let i = m.Groups[3].Value
                    Ok (create' y f i)
                else 
                    Error InvalidObservationIndexError
            with
                | ex -> Error InvalidObservationIndexError 

    [<Struct>]
    type ObservationIndexIterator = private { OIdx: ObservationIndex }

    [<Struct>]
    type ObservationValues = private { OIdx:ObservationIndex; Values:float seq }
    module ObservationValues = 
        let create oidx values  = 
            Ok { OIdx = oidx; Values = values }
        let values o =
            o.Values

