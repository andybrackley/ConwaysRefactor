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

    let copyAndUpdateWorld { Grid = grid } coord state = 
        let getCellState cellCoord cellValue = if cellCoord = coord then state else cellValue

        grid 
            |> Array.mapi(fun rowIndex row -> 
                row |> Array.mapi(fun colIndex cell -> getCellState {x = rowIndex; y = colIndex} cell ))
            |> asWorld

    let getAsText { Grid = grid } =
        let cellAsString = function
        | Alive -> "*"
        | Dead -> " "

        grid |> Array.map(
            fun rows  -> rows |> Array.fold(fun str cell -> str + cellAsString cell) "")

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

        // The current implementation keeps a total of ALL neighbours processed so far which
        // seems incorrect.  I think it should only process the immediate neighbour count to determine the new state
        // Also, the grid is updated on the fly as we are iterating through the rows and columns
        // again, I don't think this is correct and the grid should only update after the current generation has completed
        // I've maintained consistancy with the current implementation though which makes it slightly more complicated
        // as I have to use a fold and recreate the grid each time an update occurs

        let processCell liveWorld currentCoord neighbourTally =
            let currentState = World.cellAt liveWorld currentCoord
            let neighbourCount = getAliveNeighbourCount liveWorld rows cols currentCoord
            let totalNeighbours = neighbourCount + neighbourTally

            let newState = newState currentState totalNeighbours 
            
            let newWorld = 
                if(newState = currentState) then liveWorld
                else World.copyAndUpdateWorld liveWorld currentCoord newState

            (newWorld, totalNeighbours)

        let worldToCoords { Grid = grid } =
            grid
            |> Array.mapi(fun rowIndex rows -> rows |> Array.mapi(fun colIndex _ -> { x = rowIndex; y = colIndex }))
            |> Array.collect id

        worldToCoords originalWorld 
        |> Array.fold(fun (world, neighbourTally) coord -> processCell world coord neighbourTally) (originalWorld, 0)
        |> fst

[<EntryPoint>]
let main argv =
    use input = new System.IO.StreamReader("sample_input.txt")
    let all_text = input.ReadToEnd()
    let world = World.createFromString all_text

    let newWorld = Generations.evolve world

    World.getAsText newWorld
        |> Array.iter(fun l -> printfn "%s" l)

            
    42 // return an integer exit code
