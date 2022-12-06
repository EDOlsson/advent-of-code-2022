let allInput = System.IO.File.ReadAllText "./day06-input"

let isDistinct window =
    let distinctWindow = Array.distinct window
    distinctWindow = window

let part1 = allInput.ToCharArray()
            |> Array.windowed 4
            |> Array.map isDistinct
            |> Array.takeWhile (fun x -> not x)
            |> Array.length
            |> (+) 4

printfn "Part 1 %d" part1

let part2 = allInput.ToCharArray()
            |> Array.windowed 14
            |> Array.map isDistinct
            |> Array.takeWhile (fun x -> not x)
            |> Array.length
            |> (+) 14

printfn "Part 2 %d" part2
