namespace tsf
            
module Program = 
    [<EntryPoint>]
    let main args =

#if !DEBUG
        let args' = [ "Hello"; "world" ] 
#else
        let args' = args
#endif   
        for arg in args' do
            printf "%s" arg

        0