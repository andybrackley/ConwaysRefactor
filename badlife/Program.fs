// Implements Conway's Game Of Life badly
// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life on a torus

open System

type Coordinates = { x: int; y: int }

module World = 
    let createFromStringArray (lines: string[]) =
        // Assume only valid inputs for the time being.
        let charAsCellType = function 
        | '*' -> true
        | _ -> false

        lines 
        |> Array.map(
            fun line -> 
                line.ToCharArray()
                    |> Array.map charAsCellType)

    let getAsText(world: bool [][]) =
        let cellAsString = function
        | true -> "*"
        | false -> " "

        world |> Array.map(
            fun rows  -> rows |> Array.fold(fun str cell -> str + cellAsString cell) "")

    let createFromString (world: string) =
        let tokened = world.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        createFromStringArray tokened

module Generations = 
    let neighbourOffsets = [
        {x = -1; y = -1 }; { x = -1; y = 0 }; { x = -1; y = 1 }
        {x = 0; y = -1 }; { x = 0; y = 1 }
        {x = 1; y = -1 }; { x = 1; y = 0 }; { x = 1; y = 1 }    
    ]

    let sanitizeCoordinate coordinate height width =
        let bound v max = 
            match v with
            | v when v < 0 -> max - 1
            | v when v >= max -> 0 
            | v -> v

        { x =  bound coordinate.x height; y = bound coordinate.y width }

    let getNeighbourCoords (current: Coordinates) = 
        neighbourOffsets
        |> List.map (fun coord -> { x = current.x + coord.x; y = current.y + coord.y } )

    let getAliveNeighbourCount (world: bool[][]) height width currentCoord =
        getNeighbourCoords currentCoord
        |> List.map (fun c -> sanitizeCoordinate c height width)
        |> List.distinct
        |> List.map(fun coord -> world.[coord.x].[coord.y])
        |> List.filter(fun cell -> cell)
        |> List.sumBy(fun _ -> + 1)

    let newState state neighbours = 
        if (state && neighbours < 2) then false
        else if (state && neighbours = 2 || neighbours = 3) then true
        else if (state && neighbours > 3) then false
        else if (not state && neighbours = 3) then true 
        else state

    let evolve (world : bool array array) : unit = 
        let mutable neighbours = 0
        let rows = world.Length
        let cols = world.[0].Length

        for g in [0 .. world.Length-1] do
            for k in [0 .. world.Length-1] do
               let currentCoord = { x = g; y = k }
               let neighbourCount = getAliveNeighbourCount world rows cols currentCoord

               neighbours <- neighbours + neighbourCount

               let newState = newState world.[currentCoord.x].[currentCoord.y] neighbours 
               world.[g].[k] <- newState

[<EntryPoint>]
let main argv =
    use input = new System.IO.StreamReader("sample_input.txt")
    let all_text = input.ReadToEnd()
    let world = World.createFromString all_text

    Generations.evolve world

    World.getAsText world
        |> Array.iter(fun l -> printfn "%s" l)

            
    42 // return an integer exit code
