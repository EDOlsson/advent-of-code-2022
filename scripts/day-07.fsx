open System.Collections.Generic
open System.Linq

type Command =
    | ChangeDirectory of string
    | List

(*
type FileSystemItem =
    | File of FileInfo
    | Directory of DirectoryInfo
and FileInfo = { name : string; fileSize : int }
and DirectoryInfo = { name : string; subItems : FileSystemItem list }
*)

type FileInfo2 = { name : string; fileSize : int64 }
type DirectoryInfo2 = { name : string }

type FileSystemItem2 =
    | AFile of FileInfo2
    | ADirectory of DirectoryInfo2

type Input =
    | UserCommand of Command
    | DirectoryListing of DirectoryInfo2
    | FileListing of FileInfo2

let parseInputLine (line : string) =
    let parseCommand (inputCommand : string) =
        if inputCommand.StartsWith("$ cd")
            then UserCommand (ChangeDirectory (inputCommand.Substring(5)))
            else UserCommand List

    let parseDirectoryEntry (inputDirectory : string) =
        let r = inputDirectory.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        DirectoryListing { name = r[1] }

    let parseFileEntry (inputFile : string) =
        let r = inputFile.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        FileListing { name = r[1]; fileSize = System.Convert.ToInt64 r[0] }

    match line with
    | _ when line.StartsWith("$") -> parseCommand line
    | _ when line.StartsWith("dir") -> parseDirectoryEntry line
    | _ when System.Text.RegularExpressions.Regex.IsMatch(line, "^\d+.*") -> parseFileEntry line
    | _ -> failwith <| sprintf "Unknown entry : %s" line

type State = {
    currentDirectory : string
    fileSystem : Map<string, FileSystemItem2 list>
}

let folder state input =
    let findParent state' =
        let directoryExistsInList targetDir (fileSystemItems : FileSystemItem2 list) =
            fileSystemItems
            |> List.choose (fun i -> match i with
                                        | ADirectory d -> Some d
                                        | _ -> None)
            |> List.exists (fun i -> i.name = targetDir)

        let matchingEntry = state'.fileSystem
                            |> Map.filter (fun _ dirs -> directoryExistsInList state'.currentDirectory dirs)
                            |> Map.keys
        
        if Seq.isEmpty matchingEntry then @"/" else Seq.head matchingEntry

    match input with
    | DirectoryListing d -> 
            printfn "Adding directory listing %s" d.name |> ignore
            let entries = Map.tryFind state.currentDirectory state.fileSystem
            match entries with
            | Some e -> { state with fileSystem = Map.add state.currentDirectory (ADirectory d :: e) state.fileSystem }
            | None -> { state with fileSystem = Map.add state.currentDirectory [ADirectory d] state.fileSystem }
    | FileListing f ->
            printfn "Adding file listing %s" f.name |> ignore
            let entries = Map.tryFind state.currentDirectory state.fileSystem
            match entries with
            | Some e -> { state with fileSystem = Map.add state.currentDirectory (AFile f :: e) state.fileSystem }
            | None -> { state with fileSystem = Map.add state.currentDirectory [AFile f] state.fileSystem }
    | UserCommand cmd ->
            printfn "Processing command %A" cmd |> ignore
            match cmd with
            | List -> state
            | ChangeDirectory dir -> match dir with
                                        | @"/" -> { state with currentDirectory = dir }
                                        | ".." -> let parentDir = findParent state
                                                  { state with currentDirectory = parentDir }
                                        | _ as d -> { state with currentDirectory = d }
    
let allInput = System.IO.File.ReadAllLines "./day07-input"

let allDirs = allInput
            |> Array.map parseInputLine
            |> Array.fold folder { currentDirectory = ""; fileSystem = Map.empty }

let calculateDirectorySizes (fileSystem : Map<string, FileSystemItem2 list>) =
    let rec calcDirSize dir (fs : Map<string, FileSystemItem2 list>) =
        (Map.find dir fs)
        (*
        |> List.fold (fun runningSize fileItem -> match fileItem with
                                                            | AFile f -> runningSize + f.fileSize
                                                            | ADirectory d -> runningSize + calcDirSize d.name fs) 0L
        *)
        |> List.rev

    fileSystem
    |> Map.map (fun dir _ -> calcDirSize dir fileSystem)

let part1 = calculateDirectorySizes allDirs.fileSystem
            |> Map.filter (fun _ size -> size > 100000L)
            |> Map.values
            |> Seq.sum

printfn "Part 1 : %d" part1

let testInput = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""
                    .Split("\n", System.StringSplitOptions.RemoveEmptyEntries)

let x' = testInput
                |> Array.map parseInputLine
                |> Array.fold folder { currentDirectory = ""; fileSystem = Map.empty }

printfn "%A" x'

let y' = calculateDirectorySizes x'.fileSystem

printfn "%A" y'
