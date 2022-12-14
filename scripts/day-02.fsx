type GameInput = Rock | Paper | Scissors

let mapOpponentInput abc =
    match abc with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith <| sprintf "Unknown game input %s" abc

let mapMyInput xyz =
    match xyz with
    | "Y" -> Paper
    | "X" -> Rock
    | "Z" -> Scissors
    | _ -> failwith <| sprintf "Unknown game input %s" xyz

type GameRound = {
    Opponent : GameInput
    Player : GameInput
}

let parseGameRoundFromInput (input : string) =
    let values = input.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
    { Opponent = (mapOpponentInput values[0]); Player = (mapMyInput values[1]) }

type RoundOutcome = PlayerWins | OpponentWins | Tie

let calculateRoundOutcome gameRound =
    match gameRound with
    | { Opponent = Paper; Player = Scissors } -> PlayerWins
    | { Opponent = Paper; Player = Rock } -> OpponentWins
    | { Opponent = Rock; Player = Scissors } -> OpponentWins
    | { Opponent = Rock; Player = Paper } -> PlayerWins
    | { Opponent = Scissors; Player = Rock } -> PlayerWins
    | { Opponent = Scissors; Player = Paper } -> OpponentWins
    | _ -> Tie

let calculatePlayerScore gameRound =
    match gameRound with
    | { Player = Rock } -> 1
    | { Player = Paper } -> 2
    | { Player = Scissors } -> 3

let calculateRoundScore gameRound =
    let outcome = calculateRoundOutcome gameRound
    let playerScore = calculatePlayerScore gameRound 

    match outcome with
    | PlayerWins -> 6 + playerScore
    | Tie -> 3 + playerScore
    | OpponentWins -> 0 + playerScore

let allInput = System.IO.File.ReadAllLines "./day02-input"

let part1 = allInput
            |> Array.map parseGameRoundFromInput
            |> Array.map calculateRoundScore
            |> Array.sum

printfn "Part 1 %d" part1

let mapMyInputPart2 opponent xyz =
    match xyz with
    | "X" -> match opponent with        // Player must lose
             | Rock -> Scissors
             | Paper -> Rock
             | Scissors -> Paper
    | "Y" -> opponent                   // Player must tie
    | "Z" -> match opponent with        // Player must win
             | Rock -> Paper
             | Paper -> Scissors
             | Scissors -> Rock
    | _ -> failwith <| sprintf "Unknown game input %s" xyz

let parseGameRoundFromInputPart2 (input : string) =
    let values = input.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)

    let opponent = mapOpponentInput values[0]
    let myInput = mapMyInputPart2 opponent values[1]
    { Opponent = opponent; Player = myInput }

let part2 = allInput
            |> Array.map parseGameRoundFromInputPart2
            |> Array.map calculateRoundScore
            |> Array.sum

printfn "Part 2 %d" part2
