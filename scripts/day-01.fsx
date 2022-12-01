let input = System.IO.File.ReadAllLines("./day01-input")

let groupByElf (array : string[]) =
    let (elves, finalElf) = array
                            |> Array.fold (fun (groups, currentGroup) item -> if item.Length = 0 then (currentGroup :: groups, [||]) else (groups, (Array.append currentGroup [|item|]))) ([], [||])

    //
    // Prepend the last accumulated elf to the list of elves
    // Reverse the list in case order matters
    //
    // Narrator: Order did not matter
    finalElf :: elves
    |> List.rev

let calculateTotalCalories (elf : string[]) =
    elf
    |> Array.map System.Convert.ToInt32
    |> Array.sum

let elves = groupByElf input

let fattestElf = List.maxBy calculateTotalCalories elves

printfn "Part 1 : %d" (calculateTotalCalories fattestElf)

let partTwo = elves
              |> List.map calculateTotalCalories
              |> List.sortDescending
              |> List.take 3
              |> List.sum

printfn "Part 2 : %d" partTwo
