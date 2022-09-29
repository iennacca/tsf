namespace tsf

[<Struct>]
type ObsValue = private ObsValue of float

module ObsValue =
    let create f = (ObsValue f)
    let value (ObsValue v) = v

type ObsValues = private { Values:float seq; }

module ObsValues = 
    let create count obsSeq = 
        Seq.init count obsSeq

type Year = private Year of int

module Year =
    let create yr =
        if yr < 1 || yr > 9999 then
            Error "Invalid year"
        else
            Ok (Year yr)

    let value (Year y) = y

type Month = private Month of int

module Month = 
    let create m = 
        if m < 1 || m > 12 then
            Error "Invalid month"
        else 
            Ok (Month m)

    let value (Month m) = m

type Quarter = Quarter of int

module Quarter = 
    let create q = 
        if q < 1 || q > 4 then 
            Error "Invalid quarter"
        else
            Ok (Quarter q)

    let value(Quarter q) = q 

