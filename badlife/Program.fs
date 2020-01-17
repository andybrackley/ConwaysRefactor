// Implements Conway's Game Of Life badly
// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life on a torus

open System

module Cell =
    type Row = Row of int
    type Col = Col of int

    let rowAsInt(Row r) = r
    let colAsInt(Col c) = c

    type State = Alive | Dead
    type Coordinates = { x: Row; y: Col }
    let isAlive = function | Alive -> true | Dead -> false

type World = {
    LastRow : Cell.Row
    LastCol : Cell.Col
    LiveCells : Set<Cell.Coordinates>
}

module World = 
    open Cell

    let cellAt world coordinate = 
        if world.LiveCells |> Set.contains coordinate then Alive else Dead
    
    let createFromStringArray (lines: string[]) =
        let rowCount = lines.Length - 1
        let colCount = lines |> Seq.tryHead |> Option.map(fun line -> (line |> Seq.length) - 1) |> Option.defaultValue 0

        // Assume only valid inputs for the time being.
        let charAsCellType = function | '*' -> Alive | _ -> Dead
        let liveCells = 
            lines |>
                Seq.mapi(fun row line -> 
                    line |> Seq.mapi(fun col ch -> { x = Row(row); y = Col(col) }, charAsCellType ch ))
            |> Seq.concat
            |> Seq.filter(fun (_, state) -> Cell.isAlive state)
            |> Seq.map fst
            |> Set.ofSeq

        { 
            LastRow = Row(rowCount)
            LastCol = Col(colCount)
            LiveCells = liveCells 
        }
            

    let createFromString (world: string) =
        let tokened = world.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        createFromStringArray tokened

    let getCells world row = 
        let (Col lastCol) = world.LastCol
        seq {
            for col in 0 .. lastCol do 
                let coord = { x = row; y = Col(col) }
                yield coord, cellAt world coord 
        }

    let getRows world =
        let (Row lastRow) = world.LastRow
        [ 0 .. lastRow ] |> Seq.map(fun row -> Row(row))


    let getAsText world =
        let cellAsString = function
        | Alive -> '*'
        | Dead -> ' '

        let lines = 
            getRows world
            |> Seq.map(fun row -> 
                let cells = getCells world row
                cells |> Seq.map(fun (_, cell) -> cellAsString cell)
                      |> Seq.fold(fun lineStr ch -> lineStr + string(ch)) "")
        lines

    let getAs2dArray world =
        getRows world
        |> Seq.map(fun row -> 
            let cells = getCells world row
            cells |> Seq.map snd)


module Generations = 
    open Cell

    let getNeighbourCoords world (coord : Coordinates) = 
        let getBounds currentPos maxPos = 
            let lower = if currentPos = 0 then maxPos else currentPos - 1 
            let upper = if currentPos = maxPos then 0 else currentPos + 1 
            (lower, upper)

        let getRowBounds(Row currentRow) (Row lastRow) = getBounds currentRow lastRow
        let getColBounds(Col currentCol) (Col lastCol) = getBounds currentCol lastCol

        let (topRow, bottomRow) = getRowBounds coord.x world.LastRow
        let (leftCol, rightCol) = getColBounds coord.y world.LastCol

        [
            { x = Row(topRow); y = Col(leftCol) } 
            { x = Row(topRow); y = coord.y }
            { x = Row(topRow); y = Col(rightCol) } 

            { x = coord.x; y = Col(leftCol) } 
            { x = coord.x; y = Col(rightCol) } 

            { x = Row(bottomRow); y = Col(leftCol) } 
            { x = Row(bottomRow); y = coord.y }
            { x = Row(bottomRow); y = Col(rightCol) } 
        ] |> Seq.distinct


    let newState state neighbours = 
        match neighbours with
        | 3 -> Alive
        | 2 -> state
        | _ -> Dead

    let cellsToProcess world =
        world.LiveCells 
            |> Seq.collect (getNeighbourCoords world)
            |> Set.ofSeq

    let getAliveNeighbourCount world coord = 
        getNeighbourCoords world coord
        |> Seq.filter(fun c -> World.cellAt world c |> isAlive)
        |> Seq.length

    let evolveWorld world =
        // Get all cells to process
        let cellsToProcess = cellsToProcess world |> Seq.toArray

        // foreach neighbouring cell calculate state
        let evolved = 
            cellsToProcess 
                |> Seq.map(fun coord -> 
                    let aliveNeighbours = getAliveNeighbourCount world coord
                    let currentState = World.cellAt world coord
                    (coord, newState currentState aliveNeighbours))
                |> Seq.filter(fun (_, state) -> Cell.isAlive state)
                |> Seq.map fst
                |> Set.ofSeq

        { world with LiveCells = evolved }

[<EntryPoint>]
let main argv =
    use input = new System.IO.StreamReader("sample_input.txt")
    let all_text = input.ReadToEnd()
    let world = World.createFromString all_text

    let newWorld = Generations.evolveWorld world

    World.getAsText newWorld
        |> Seq.iter(fun l -> printfn "%s" l)

    0 // return an integer exit code
