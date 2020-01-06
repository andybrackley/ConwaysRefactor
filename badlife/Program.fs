// Implements Conway's Game Of Life badly
// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life on a torus

open System

let createWorldFromStringArray (lines: string[]) =
    let world : bool array array = Array.zeroCreate lines.Length 
    for x in [0 .. lines.Length-1] do
        world.[x] <- Array.zeroCreate lines.[x].Length
        let mutable c = 0
        while c < lines.[x].Length do
            if lines.[x].[c] = '_' then
                world.[x].[c] <- false
            elif lines.[x].[c] = '*' then
                world.[x].[c] <- true
            c <- c + 1

    world

let createWorldFromString (world: string) =
    let tokened = world.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
    createWorldFromStringArray tokened

let evolve (world : bool array array) : unit = 
    let mutable neighbours = 0
    let rows = world.Length
    let cols = world.[0].Length

    for g in [0 .. world.Length-1] do
        for k in [0 .. world.Length-1] do
           if (world.[if g - 1 < 0 then rows - 1 else g - 1].[if k - 1 < 0 then cols - 1 else k - 1]) then neighbours <- neighbours+1
           if (world.[if g - 1 < 0 then rows - 1 else g - 1].[k]) then neighbours <- neighbours+1
           if (world.[if g - 1 < 0 then rows - 1 else g - 1].[if k + 1 = cols then 0 else k + 1]) then neighbours <- neighbours+1

           if (world.[g].[if k - 1 < 0 then cols - 1 else k - 1]) then neighbours <- neighbours+1
           if (world.[g].[if k + 1 = cols then 0 else k + 1]) then neighbours <- neighbours+1
           if (world.[if g + 1 = rows then 0 else g + 1].[if k - 1 < 0 then cols - 1 else k - 1]) then neighbours <- neighbours+1
           if (world.[if g + 1 = rows then 0 else g + 1].[k]) then neighbours <- neighbours+1
           if (world.[if g + 1 = rows then 0 else g + 1].[if k + 1 = cols then 0 else k + 1]) then neighbours <- neighbours+1

           if (world.[g].[k]&& neighbours < 2) then world.[g].[k] <- false
           if (world.[g].[k] && neighbours = 2 || neighbours = 3) then world.[g].[k] <- true
           if (world.[g].[k] && neighbours > 3) then world.[g].[k] <- false
           if (not world.[g].[k] && neighbours = 3) then world.[g].[k] <- true 

[<EntryPoint>]
let main argv =
    let input = System.IO.StreamReader("sample_input.txt")
    let all_text = input.ReadToEnd()
    let world = createWorldFromString all_text

    evolve world

    for a in [0 .. world.Length - 1] do
        let mutable line = ""
        for b in [0 .. world.[0].Length-1] do
            if world.[a].[b] then line <- line + "*" else line <- line + " "
        printfn "%s" line
            
    42 // return an integer exit code
