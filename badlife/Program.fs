// Implements Conway's Game Of Life badly
// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life on a torus

open System

module Cell =
    type State = Alive | Dead
    type Coordinates = { x: int; y: int }
    let isAlive = function | Alive -> true | Dead -> false

type World = {
    Grid: Cell.State[][]
}

module World = 
    open Cell

    let cellAt { Grid = grid } { x= x; y = y } = grid.[x].[y]
    let asWorld grid = { Grid = grid }

    let createFromStringArray (lines: string[]) =
        // Assume only valid inputs for the time being.
        let charAsCellType = function | '*' -> Alive | _ -> Dead
        lines 
            |> Array.map(fun line -> line.ToCharArray() |> Array.map charAsCellType)
            |> asWorld

    let createFromString (world: string) =
        let tokened = world.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        createFromStringArray tokened

    let getAsText { Grid = grid } =
        let cellAsString = function
        | Alive -> "*"
        | Dead -> " "

        grid |> Array.map(
            fun rows  -> rows |> Array.fold(fun str cell -> str + cellAsString cell) "")

    // Iterates the 2d array and allows a mapping function to be supplied for each cell
    // The mapper will receive a tuple of the Coordinate and the current state and is expected to return a new state
    let mapEachCell (mapper: Coordinates * Cell.State -> Cell.State) { Grid = grid } = 
        grid 
          |> Array.mapi(fun rowIndex row -> 
            row |> Array.mapi(fun colIndex col -> 
                ( { x = rowIndex; y = colIndex }, grid.[rowIndex].[colIndex] ))
                |> Array.map mapper
          )

module Generations = 
    open Cell

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

    let getAliveNeighbourCount world height width currentCoord =
        getNeighbourCoords currentCoord
        |> List.map (fun c -> sanitizeCoordinate c height width)
        |> List.distinct
        |> List.map(fun coord -> World.cellAt world coord)
        |> List.filter isAlive
        |> List.sumBy(fun _ -> + 1)

    let newState state neighbours = 
        match neighbours with
        | 3 -> Alive
        | 2 -> state
        | _ -> Dead

    let evolve originalWorld = 
        let rows = originalWorld.Grid.Length
        let cols = originalWorld.Grid.[0].Length

        let getCellNewState currentCoord currentState =
            let neighbourCount = getAliveNeighbourCount originalWorld rows cols currentCoord
            newState currentState neighbourCount

        originalWorld
        |> World.mapEachCell (fun (coord, cell) -> getCellNewState coord cell)
        |> World.asWorld

[<EntryPoint>]
let main argv =
    use input = new System.IO.StreamReader("sample_input.txt")
    let all_text = input.ReadToEnd()
    let world = World.createFromString all_text

    let newWorld = Generations.evolve world

    World.getAsText newWorld
        |> Array.iter(fun l -> printfn "%s" l)

            
    42 // return an integer exit code
