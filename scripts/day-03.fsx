let divideRucksackItems (input : string) =
    input.ToCharArray()
    |> Array.splitInto 2

let findCommonItems (input : char[][]) =
    let compartment1 = Array.fold (fun set item -> Set.add item set) Set.empty input[0]
    let compartment2 = Array.fold (fun set item -> Set.add item set) Set.empty input[1]

    Set.intersect compartment1 compartment2

let calculatePriority commonItems =
    let calculatePriority (item : char) =
        match item with
        | _ as lowercase when lowercase >= 'a' && lowercase <= 'z' -> System.Convert.ToInt32 lowercase - System.Convert.ToInt32 'a' + 1
        | _ as uppercase  -> System.Convert.ToInt32 uppercase - System.Convert.ToInt32 'A' + 27

    commonItems
    |> Set.map calculatePriority
    |> Set.toList
    |> List.sum

let allInput = System.IO.File.ReadAllLines "./day03-input"

let part1 = allInput
            |> Array.map divideRucksackItems
            |> Array.map findCommonItems
            |> Array.map calculatePriority
            |> Array.sum

printfn "Part 1 %d" part1
