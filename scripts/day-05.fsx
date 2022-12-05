open System.Collections.Generic

//
// Create stacks manually
//
let stack1 = new Stack<char>()
stack1.Push 'N'
stack1.Push 'R'
stack1.Push 'G'
stack1.Push 'P'

let stack2 = new Stack<char>()
stack2.Push 'J'
stack2.Push 'T'
stack2.Push 'B'
stack2.Push 'L'
stack2.Push 'F'
stack2.Push 'G'
stack2.Push 'D'
stack2.Push 'C'

let stack3 = new Stack<char>()
stack3.Push 'M'
stack3.Push 'S'
stack3.Push 'V'

let stack4 = new Stack<char>()
stack4.Push 'L'
stack4.Push 'S'
stack4.Push 'R'
stack4.Push 'C'
stack4.Push 'Z'
stack4.Push 'P'

let stack5 = new Stack<char>()
stack5.Push 'P'
stack5.Push 'S'
stack5.Push 'L'
stack5.Push 'V'
stack5.Push 'C'
stack5.Push 'W'
stack5.Push 'D'
stack5.Push 'Q'

let stack6 = new Stack<char>()
stack6.Push 'C'
stack6.Push 'T'
stack6.Push 'N'
stack6.Push 'W'
stack6.Push 'D'
stack6.Push 'M'
stack6.Push 'S'

let stack7 = new Stack<char>()
stack7.Push 'H'
stack7.Push 'D'
stack7.Push 'G'
stack7.Push 'W'
stack7.Push 'P'

let stack8 = new Stack<char>()
stack8.Push 'Z'
stack8.Push 'L'
stack8.Push 'P'
stack8.Push 'H'
stack8.Push 'S'
stack8.Push 'C'
stack8.Push 'M'
stack8.Push 'V'

let stack9 = new Stack<char>()
stack9.Push 'R'
stack9.Push 'P'
stack9.Push 'F'
stack9.Push 'L'
stack9.Push 'W'
stack9.Push 'G'
stack9.Push 'Z'

type Instruction = {
    CrateCount : int
    SourceStack : int
    DestinationStack : int
}

let parseInstructionInput (input : string) =
    let r = System.Text.RegularExpressions.Regex.Match(input, "move (\d+) from (\d) to (\d)")
    { CrateCount = System.Convert.ToInt32(r.Groups[1].Value)
      SourceStack = System.Convert.ToInt32(r.Groups[2].Value)
      DestinationStack = System.Convert.ToInt32(r.Groups[3].Value) }


let folder (stacks : Stack<char> array) instruction =
    for x in 1..instruction.CrateCount do
        let crate = stacks[instruction.SourceStack - 1].Pop()
        stacks[instruction.DestinationStack - 1].Push crate
    
    stacks

let popFinalMessage (stacks : Stack<char> array) =
    let finalMessage = [| stacks[0].Pop(); stacks[1].Pop(); stacks[2].Pop(); stacks[3].Pop(); stacks[4].Pop(); stacks[5].Pop(); stacks[6].Pop(); stacks[7].Pop(); stacks[8].Pop() |]
    new string(finalMessage)

let allInput = System.IO.File.ReadAllLines "./day05-input"
               |> Array.skip 10

(*
let finalCrates = allInput
                  |> Array.map parseInstructionInput
                  |> Array.fold folder [|stack1; stack2; stack3; stack4; stack5; stack6; stack7; stack8; stack9 |]

let part1 = popFinalMessage finalCrates

printfn "Part 1 %s" part1
*)

//
// To preserve order, use a temp stack & then pop and push to destination
//
let part2Folder (stacks : Stack<char> array) instruction =
    let temp = new Stack<char>()
    for x in 1..instruction.CrateCount do
        stacks[instruction.SourceStack - 1].Pop() |> temp.Push

    for x in 1..temp.Count do
        temp.Pop() |> stacks[instruction.DestinationStack - 1].Push

    stacks

let finalCratesPart2 = allInput
                        |> Array.map parseInstructionInput
                        |> Array.fold part2Folder [|stack1; stack2; stack3; stack4; stack5; stack6; stack7; stack8; stack9 |]

let part2 = popFinalMessage finalCratesPart2

printfn "Part 2 %s" part2
