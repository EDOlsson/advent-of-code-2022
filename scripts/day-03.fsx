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

let parseInputIntoGroups allInput =
    allInput
    |> Array.chunkBySize 3

let locateCommonItem (elfGroup : string[]) =
    let convertItemsToSet (items : string) =
        items.ToCharArray()
        |> Array.fold (fun set item -> Set.add item set)  Set.empty

    let member1 = convertItemsToSet elfGroup[0]
    let member2 = convertItemsToSet elfGroup[1]
    let member3 = convertItemsToSet elfGroup[2]

    Set.intersect member1 (Set.intersect member2 member3)

let part2 = parseInputIntoGroups allInput
                 |> Array.map locateCommonItem
                 |> Array.map calculatePriority
                 |> Array.sum

printfn "Part 2 %d" part2
