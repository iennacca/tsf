namespace tsf

open Utilities

module Entities =
    type Error = 
    | InvalidObsIndexError
    | TypeNotImplementedError
    | InvalidYearError
    | FreqError
    | IndexError

    type FreqType = 
        | D 
        | M 
        | Q
        | S
        | A

    [<Struct>]
    type FreqIndex = private FreqIndex of int
    module FreqIndex =
        let max f = 
            match f with
            | FreqType.A -> 1
            | FreqType.S -> 2                    
            | FreqType.Q -> 4
            | FreqType.M -> 12
            | FreqType.D -> 366
        
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
    type ObsValue = private ObsValue of float
    module ObsValue =
        let create value = (ObsValue value)
        let value (ObsValue value) = value

    [<Struct>]
    type ObsIndex = private { Year:Year; Freq:FreqType; Idx: FreqIndex }
    module ObsIndex =
        let create y f i = 
            let l = [1..(FreqIndex.max f)]
            let cFI i = 
                if List.contains i l then Ok (FreqIndex i)
                else Error [InvalidObsIndexError]

            result {
                let! y' = Year.create y
                and! i' = cFI i
                return! Ok { Year = y'; Freq = f; Idx = i' }
            }

    [<Struct>]
    type ObsValues = private { OIdx:ObsIndex; Values:float seq }
    module ObsValues = 
        let create values oidx  = 
            Ok { OIdx = oidx; Values = values }
        let values o = o.Values

