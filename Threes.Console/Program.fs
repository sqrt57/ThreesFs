open System

[<AutoOpen>]
module GameTypes =
    [<RequireQualifiedAccess>]
    type Cell =
        | Empty
        | Three

    type Field = Cell array array

    type Game = Game of Field

    let width = 6
    let height = 4

    type Move = Up | Down | Left | Right

[<AutoOpen>]
module Game =
    let private createField value = [| for col in 0 .. width-1 -> Array.create height value |]

    let private emptyField() = createField Cell.Empty

    let newGame (random: Random) numThrees =
        let field = emptyField()
        let rec addThree num =
            if num > 0 then
                let x = random.Next width
                let y = random.Next height
                if field.[x].[y] = Cell.Empty then
                    field.[x].[y] <- Cell.Three
                    addThree (num - 1)
                else
                    addThree num
            else
                ()
        addThree numThrees
        Game field

    let private isBlockeByWallGetter move =
        match move with
        | Up -> fun (x, y) -> y = 0
        | Down -> fun (x, y) -> y = height-1
        | Left -> fun (x, y) -> x = 0
        | Right -> fun (x, y) -> x = width-1

    let private moveSourceGetter move =
        match move with
        | Up -> fun (x, y) -> if y < height-1 then Some (x, y+1) else None
        | Down -> fun (x, y) -> if y > 0 then Some (x, y-1) else None
        | Left -> fun (x, y) -> if x < width-1 then Some (x+1, y) else None
        | Right -> fun (x, y) -> if x > 0 then Some (x-1, y) else None

    let makeMove move (Game field) =
        let newField = emptyField()
        let isBlockeByWall = isBlockeByWallGetter move
        let moveSource = moveSourceGetter move
        for x in 0 .. width-1 do
            for y in 0 .. height-1 do
                if isBlockeByWall (x, y) && field.[x].[y] = Cell.Three then
                    newField.[x].[y] <- field.[x].[y]
                else
                    match moveSource (x, y) with
                    | Some (sx, sy) -> newField.[x].[y] <- field.[sx].[sy]
                    | None -> newField.[x].[y] <- Cell.Empty
        Game newField

[<AutoOpen>]
module Print =

    let private cellToStr cell =
        match cell with
        | Cell.Empty -> " "
        | Cell.Three -> "3"

    let private printHorizontalDelimiter() =
        for i in 0 .. width-1 do
            printf "+---"
        printfn "+"

    let private printRow (field: Field) n =
        for i in 0 .. width-1 do
            printf "| %s " (cellToStr field.[i].[n])
        printfn "|"

    let printGame (Game field) =
        printHorizontalDelimiter()
        for i in 0 .. height-1 do
            printRow field i
            printHorizontalDelimiter()


[<EntryPoint>]
let main argv =

    let rec nextMove game =

        let processMove move = nextMove <| makeMove move game

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

    let random = Random()
    let initital = newGame random 3
    nextMove initital
    0
