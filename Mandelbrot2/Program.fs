open System.Drawing
open System.IO
open System.Numerics
open System
open Palettes

[<Literal>]
let paletteLoops = 2
//wiki
let palette = Palette.wiki2

let f (z : Complex) (c : Complex) = (z * z) + c

// Returns None if in the set, else Some (it, z) where it is the escape iteration and z the value at that point
let rec iterate currentIteration maxIterations (z : Complex) (c : Complex) =
    if z.Magnitude > 256.0 then Some (currentIteration, z)
    else if currentIteration >= maxIterations then None
    else iterate (currentIteration+1) maxIterations (f z c) c

// Iterates a whole 2d Array of points
let mandelbrot iterations points =
    let testC c = async{
        return iterate 0 iterations Complex.Zero c
    }
    points |> Array.map (fun row ->
        Async.Parallel [ for c in row -> testC c ] |> Async.RunSynchronously
    )

// Returns a matrix of x*y Complex numbers representing points in the complex plane
let pointsMatrix (xPixels : int) (yPixels : int) (center : Complex) (r : float) =
    let pointIncrement = (r / (float)(xPixels/2))
    let startX = center.Real - (pointIncrement*(float)(xPixels/2))
    let startY = center.Imaginary + (pointIncrement*(float)(yPixels/2))
    [|
        for i in 0..(yPixels-1) -> [|
            for j in 0..(xPixels-1) -> Complex(startX+(pointIncrement*float j), startY-(pointIncrement*float i))
        |]
    |]

// returns interpolated color between c1 and c2 with m between 0 and 1
let interpolate (c1:Color) (c2:Color) (m:float) =
    let createColor (r,g,b) = Color.FromArgb(255, min r 255, min g 255, min b 255)
    createColor(
        (1. - m) * float c1.R + float c2.R * m |> int,
        (1. - m) * float c1.G + float c2.G * m |> int,
        (1. - m) * float c1.B + float c2.B * m |> int
    )

// returns the color of a pixel given its parameters
let color (iterColors : array<int>) (it : int) (nu : float) ((s, n, d) : (int * int * int)) =
    let c1 = palette.[iterColors.[it-1]]
    let c2 = palette.[(iterColors.[it-1] + 1)%palette.Length]
    let m = (((nu % 1.0)*(float(n-s)/float d))+(float s/float d))
    interpolate c1 c2 m

// Generates the iteration counts and histogram
// count -> counts how many points escaped at each iteration
// hist -> (accumulated count)/total points
let histogram iterations (points : array<array<option<int * Complex>>>) =
    let count = Array.zeroCreate iterations
    let width = points.[0].Length
    let height = points.Length
    for i in 0..(width-1) do
        for j in 0..(height-1) do
            match points.[j].[i] with
            | Some (it, _) -> count.[it-1] <- count.[it-1] + 1
            | None -> ()
    let (accum, total) = Array.mapFold (fun acc i -> (acc + i, acc + i)) 0 count
    let hist = Array.map (fun n -> (float)n/(float)total) accum
    (count, hist)

// Calculates the percentage of a certain iteration inside the group of iterations with the same color (for color smoothing)
// Returns an array of (start, end, denominator)
let positions (iterColors : array<int>) (iterCounts : array<int>) =
    let rec separate (l : List<int>) =
        match l with
        | x :: _ -> List.takeWhile (fun n -> n = x) l :: separate (List.skipWhile (fun n -> n = x) l)
        | _ -> []
    let lists = separate (List.ofArray iterColors)
    let genCount n =
        [| for i in 1..n -> (i, n) |]
    let groups = List.map (fun (l : List<int>) -> genCount l.Length) lists
    let rec processGroup i (all : list<array<(int * int)>>) =
        match all with
        | x :: xs ->
            let size = snd x.[0]
            let subIters = iterCounts.[i..(i+size-1)]
            let total = Array.sum subIters
            List.ofArray (Array.mapi (fun i _ -> ((Array.sum subIters.[0..(i-1)]), (Array.sum subIters.[0..i]), total)) subIters) :: (processGroup (i+size) xs)
        | _ -> []
    processGroup 0 groups |> List.concat |> Array.ofList

// Assigns a color to every iteration. Makes sure all contiguous iterations have contiguous or equal colors.
// Returns an array of indexes to the palette
let assignColors (hist : array<float>) =
    let iterColors : array<int> = Array.zeroCreate hist.Length
    let colors = palette.Length
    let rec assign i lastC lastN =
        if i < iterColors.Length then
            match (int ((float paletteLoops)*(float colors)*hist.[i-1])) with
            | n when n > lastN ->
                iterColors.[i] <- (lastC+1)%colors
                assign (i+1) (lastC+1) n
            | _ ->
                iterColors.[i] <- lastC%colors
                assign (i+1) lastC lastN
    assign 1 -1 -1
    iterColors

// generates the bitmap from number of iterations and a result array from the mandelbrot function
let draw iterations (points : array<array<option<int * Complex>>>) =
    let width = points.[0].Length
    let height = points.Length
    let (iterCounts, hist) = histogram iterations points
    let iterColors = assignColors hist
    let p = positions iterColors iterCounts
    let bitmap = new Bitmap(width, height)
    for i in 0..(width-1) do
        for j in 0..(height-1) do
            match points.[j].[i] with
            | Some (it, c) ->
                let logZn = (log ( c.Real*c.Real + c.Imaginary*c.Imaginary )) / 2.0
                let nu = float it + 1.0 - (log( logZn / log(2.0) )) / log(2.0)
                bitmap.SetPixel(i, j, (color iterColors it nu p.[it-1]))
            | None -> bitmap.SetPixel(i, j, Color.Black)
    bitmap

let render xPixels yPixels iterations x y r =
    let bm = pointsMatrix xPixels yPixels (Complex(x, y)) r |> mandelbrot iterations |> draw iterations
    bm.Save(Path.Combine(__SOURCE_DIRECTORY__, "plot.png"))

[<EntryPoint>]
let main argv =
    let xPixels = int argv.[0]
    let yPixels = int argv.[1]
    let mutable iterations = int argv.[2]
    let mutable x = float argv.[3]
    let mutable y = float argv.[4]
    let mutable r = float argv.[5]
    render xPixels yPixels iterations x y r
    // Interactive
    (*
    printf "->"
    let mutable key = System.Console.ReadKey()
    while (not (key.Key.Equals(ConsoleKey.Q))) do
        match key with
        | k when k.Key.Equals(ConsoleKey.W) -> y <- y + (r/4.0)
        | k when k.Key.Equals(ConsoleKey.S) -> y <- y - (r/4.0)
        | k when k.Key.Equals(ConsoleKey.A) -> x <- x - (r/4.0)
        | k when k.Key.Equals(ConsoleKey.D) -> x <- x + (r/4.0)
        | k when k.Key.Equals(ConsoleKey.I) -> iterations <- iterations + 50
        | k when k.Key.Equals(ConsoleKey.O) -> iterations <- iterations - 50
        | k when k.Key.Equals(ConsoleKey.Add) -> r <- (r/1.5)
        | k when k.Key.Equals(ConsoleKey.Subtract) -> r <- (r*1.5)
        printfn "x: %A, y: %A, r: %A" x y r
        render xPixels yPixels iterations x y r
        printf "\n->"
        key <- System.Console.ReadKey()
    printfn "\nBye!"*)
    0
