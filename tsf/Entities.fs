namespace tsf

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

    type FreqIndex = private FreqIndex of int

    type Year = private Year of int
    module Year =
        let create yr =
            if yr < 1 || yr > 9999 then
                Error InvalidYearError
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
            if i > 366 then Error InvalidObsIndexError
            else
            match f with
                | FreqType.A -> Ok { Year = (Year y); Freq = f; Idx = (FreqIndex 1) }
                | FreqType.S -> Ok { Year = (Year y); Freq = f; Idx = (FreqIndex i) }
                | FreqType.Q -> Error TypeNotImplementedError
                | FreqType.M -> Error TypeNotImplementedError
                | FreqType.D -> Error TypeNotImplementedError  

    [<Struct>]
    type ObsValues = private { OIdx:ObsIndex; Values:float seq }

    module ObsValues = 
        let create values oidx  = 
            Ok { OIdx = oidx; Values = values }
        let values o = o.Values

