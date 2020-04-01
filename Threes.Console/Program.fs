open System

[<AutoOpen>]
module Rnd =
    let nextRandom (random: Random) upperBound = random.Next upperBound

[<AutoOpen>]
module GameTypes =
    [<RequireQualifiedAccess; Struct>]
    type Cell =
        | Empty
        | One
        | Two
        | Three of int

    type Field = Cell array array

    type Game = Game of Field

    let width = 8
    let height = 8

    type Move = Up | Down | Left | Right

    let (|Vertical|Horizontal|) move =
        match move with
        | Up | Down -> Vertical
        | Left | Right -> Horizontal

[<AutoOpen>]
module Game =
    let private createField value = [| for _ in 0 .. width-1 -> Array.create height value |]

    let private emptyField() = createField Cell.Empty

    let private copyField field = [| for col in field -> Array.copy col |]

    let newGame random numOnes numTwos numThrees =
        let field = emptyField()

        let rec putCell c =
            let x = nextRandom random width
            let y = nextRandom random height
            if field.[x].[y] = Cell.Empty then
                field.[x].[y] <- c
            else
                putCell c
        for _ in 1..numOnes do putCell Cell.One
        for _ in 1..numTwos do putCell Cell.Two
        for _ in 1..numThrees do putCell <| Cell.Three 0

        Game field

    let private moveSeq move =
        match move with
        | Up -> seq { for x in 0 .. width-1 do for y in 0 .. height-2 -> (x, y + 1), (x, y) }
        | Down -> seq { for x in 0 .. width-1 do for y in height-1 .. -1 .. 1 -> (x, y - 1), (x, y) }
        | Left -> seq { for x in 0 .. width-2 do for y in 0 .. height-1 -> (x + 1, y), (x, y) }
        | Right -> seq { for x in width-1 .. -1 .. 1 do for y in 0 .. height-1 -> (x - 1, y), (x, y) }

    let makeMove move (Game field) random =
        let newField = copyField field
        let hasMoved = Array.create (match move with | Vertical -> width | Horizontal -> height) false

        for (fromX, fromY), (toX, toY) in moveSeq move do
            let source, target = newField.[fromX].[fromY], newField.[toX].[toY]
            let newSource, newTarget =
                match source, target with
                | s, Cell.Empty -> Cell.Empty, s
                | Cell.One, Cell.Two -> Cell.Empty, Cell.Three 0
                | Cell.Two, Cell.One -> Cell.Empty, Cell.Three 0
                | Cell.Three n1, Cell.Three n2 when n1 = n2 -> Cell.Empty, Cell.Three <| n1 + 1
                | s, t -> s, t
            if newSource <> source || newTarget <> target then
                match move with
                | Vertical -> hasMoved.[fromX] <- true
                | Horizontal -> hasMoved.[fromY] <- true
            newField.[fromX].[fromY] <- newSource
            newField.[toX].[toY] <- newTarget

        if Array.contains true hasMoved then
            let rec findTrue() =
                let n = nextRandom random hasMoved.Length
                if hasMoved.[n] then n else findTrue()
            let n = findTrue()
            match move with
            | Up -> newField.[n].[height - 1] <- Cell.Three 0
            | Down  -> newField.[n].[0] <- Cell.Three 0
            | Left -> newField.[width - 1].[n] <- Cell.Three 0
            | Right -> newField.[0].[n] <- Cell.Three 0

        Game newField

[<AutoOpen>]
module Print =

    let private cellToStr cell =
        match cell with
        | Cell.Empty -> "     "
        | Cell.One -> "  1  "
        | Cell.Two -> "  2  "
        | Cell.Three 0 -> "  3  "
        | Cell.Three 1 -> "  6  "
        | Cell.Three 2 -> " 12  "
        | Cell.Three 3 -> " 24  "
        | Cell.Three 4 -> " 48  "
        | Cell.Three 5 -> " 96  "
        | Cell.Three 6 -> " 192 "
        | Cell.Three 7 -> " 384 "
        | Cell.Three 8 -> " 768 "
        | Cell.Three 9 -> "1536 "
        | Cell.Three 10 -> "3072 "
        | Cell.Three 11 -> "6144 "
        | Cell.Three _ -> " INF "

    let private printHorizontalDelimiter() =
        for _ in 0 .. width-1 do
            printf "+-----"
        printfn "+"

    let private printRow (field: Field) n =
        for i in 0 .. width-1 do
            printf "|%s" (cellToStr field.[i].[n])
        printfn "|"

    let printGame (Game field) =
        printHorizontalDelimiter()
        for i in 0 .. height-1 do
            printRow field i
            printHorizontalDelimiter()


[<EntryPoint>]
let main argv =

    let random = Random()

    let rec nextMove game =

        let processMove move = nextMove <| makeMove move game random

        Console.Clear()
        printGame game

        let keyInfo = Console.ReadKey false
        match keyInfo.Key with
        | ConsoleKey.UpArrow -> processMove Up
        | ConsoleKey.DownArrow -> processMove Down
        | ConsoleKey.LeftArrow -> processMove Left
        | ConsoleKey.RightArrow -> processMove Right
        | ConsoleKey.Escape -> ()
        | _ -> nextMove game

    let initital = newGame random 8 8 6
    nextMove initital
    0
