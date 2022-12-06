let allInput = System.IO.File.ReadAllText "./day06-input"

let isDistinct window =
    let distinctWindow = Array.distinct window
    window.Length = distinctWindow.Length &&
    window[0] = distinctWindow[0] &&
    window[1] = distinctWindow[1] &&
    window[2] = distinctWindow[2] &&
    window[3] = distinctWindow[3]

let part1 = allInput.ToCharArray()
            |> Array.windowed 4
            |> Array.map isDistinct
            |> Array.takeWhile (fun x -> not x)
            |> Array.length
            |> (+) 4

printfn "Part 1 %d" part1
