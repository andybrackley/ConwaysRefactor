// Implements Conway's Game Of Life badly
// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life on a torus

open System

let createWorldFromStringArray (lines: string[]) =
    // Assume only valid inputs for the time being.
    let charAsCellType = function 
    | '*' -> true
    | _ -> false

    lines 
    |> Array.map(
        fun line -> 
            line.ToCharArray()
                |> Array.map charAsCellType)


let getWorldAsText(world: bool [][]) =
    let cellAsString = function
    | true -> "*"
    | false -> " "

    world |> Array.map(
        fun rows  -> rows |> Array.fold(fun str cell -> str + cellAsString cell) "")

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

    getWorldAsText world
        |> Array.iter(fun l -> printfn "%s" l)

            
    42 // return an integer exit code
