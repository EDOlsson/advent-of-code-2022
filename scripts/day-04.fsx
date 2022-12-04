type SectionList = {
    StartId : int
    EndId : int
}

let parseAssignment (input : string) =
    let parseSectionList (parsedRange : string) =
        let sectionIds = parsedRange.Split("-", System.StringSplitOptions.RemoveEmptyEntries)
        { StartId = System.Convert.ToInt32 sectionIds[0]; EndId = System.Convert.ToInt32 sectionIds[1] }

    input.Split(",", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map parseSectionList

let isSectionDuplicate (assignments : SectionList array) =
    assignments[0].StartId <= assignments[1].StartId && assignments[0].EndId >= assignments[1].EndId
    ||
    assignments[1].StartId <= assignments[0].StartId && assignments[1].EndId >= assignments[0].EndId

let allInput = System.IO.File.ReadAllLines "./day04-input"

let part1 = allInput
            |> Array.map parseAssignment
            |> Array.filter isSectionDuplicate
            |> Array.length

printfn "Part 1 %d" part1
