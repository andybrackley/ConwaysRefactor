// Implements Conway's Game Of Life badly
// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life on a torus

open System

type CellState = Alive | Dead
type Coordinates = { x: int; y: int }

module World = 
    let createFromStringArray (lines: string[]) =
        // Assume only valid inputs for the time being.
        let charAsCellType = function | '*' -> Alive | _ -> Dead
        lines |> Array.map(fun line -> line.ToCharArray() |> Array.map charAsCellType)

    let getAsText(world: CellState [][]) =
        let cellAsString = function
        | Alive -> "*"
        | Dead -> " "

        world |> Array.map(
            fun rows  -> rows |> Array.fold(fun str cell -> str + cellAsString cell) "")

    let createFromString (world: string) =
        let tokened = world.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        createFromStringArray tokened

    let copyAndUpdateWorld (world : CellState[][]) coord state = 
        let getCellState cellCoord cellValue = if cellCoord = coord then state else cellValue

        world 
            |> Array.mapi(fun rowIndex row -> 
                row |> Array.mapi(fun colIndex cell -> getCellState {x = rowIndex; y = colIndex} cell ))

module Generations = 
    let isCellAlive = function | Alive -> true | Dead -> false

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

    let getAliveNeighbourCount (world: CellState[][]) height width currentCoord =
        getNeighbourCoords currentCoord
        |> List.map (fun c -> sanitizeCoordinate c height width)
        |> List.distinct
        |> List.map(fun coord -> world.[coord.x].[coord.y])
        |> List.filter isCellAlive
        |> List.sumBy(fun _ -> + 1)

    let newState state neighbours = 
        match neighbours with
        | 3 -> Alive
        | 2 -> state
        | _ -> Dead

    let evolve (world : CellState[][]) = 
        let rows = world.Length
        let cols = world.[0].Length

        // The current implementation keeps a total of ALL neighbours processed so far which
        // seems incorrect.  I think it should only process the immediate neighbour count to determine the new state
        // Also, the grid is updated on the fly as we are iterating through the rows and columns
        // again, I don't think this is correct and the grid should only update after the current generation has completed
        // I've maintained consistancy with the current implementation though which makes it slightly more complicated
        // as I have to use a fold and recreate the grid each time an update occurs

        let processCell (world: CellState[][]) currentCoord neighbourTally =
            let currentState = world.[currentCoord.x].[currentCoord.y]
            let neighbourCount = getAliveNeighbourCount world rows cols currentCoord
            let totalNeighbours = neighbourCount + neighbourTally

            let newState = newState currentState totalNeighbours 
            
            let newWorld = 
                if(newState = currentState) then world
                else World.copyAndUpdateWorld world currentCoord newState

            (newWorld, totalNeighbours)

        let worldToCoords world =
            world 
            |> Array.mapi(fun rowIndex rows -> rows |> Array.mapi(fun colIndex _ -> { x = rowIndex; y = colIndex }))
            |> Array.collect id

        worldToCoords world 
        |> Array.fold(fun (world, neighbourTally) coord -> processCell world coord neighbourTally) (world, 0)
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
