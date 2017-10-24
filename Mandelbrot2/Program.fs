open System.Drawing
open System.IO
open System.Numerics
open System

let f (z : Complex) (c : Complex) = (z * z) + c

let rec iterate currentIteration maxIterations (z : Complex) (c : Complex) =
    if z.Magnitude > 256.0 then Some (currentIteration, z)
    else if currentIteration >= maxIterations then None
    else iterate (currentIteration+1) maxIterations (f z c) c

let mandelbrot iterations points =
    let testC c = iterate 0 iterations Complex.Zero c
    points |> Array.map (fun row -> Array.map testC row)

let pointsMatrix (xPixels : int) (yPixels : int) (center : Complex) (r : float) =
    let pointIncrement = (r / (float)(xPixels/2))
    let startX = center.Real - (pointIncrement*(float)(xPixels/2))
    let startY = center.Imaginary + (pointIncrement*(float)(yPixels/2))
    [|
        for i in 0..(yPixels-1) -> [|
            for j in 0..(xPixels-1) -> Complex(startX+(pointIncrement*float j), startY-(pointIncrement*float i))
        |]
    |]

let createColor (r,g,b) = Color.FromArgb(255, min r 255, min g 255, min b 255)

let interpolate (c1:Color) (c2:Color) (m:float) =
    createColor(
        (1. - m) * float c1.R + float c2.R * m |> int,
        (1. - m) * float c1.G + float c2.G * m |> int,
        (1. - m) * float c1.B + float c2.B * m |> int
    )

let color (adjPalette : array<Color>) (palette : array<Color>) (it : int) (nu : float) (p : (int * int * int)) =
    let c1 = adjPalette.[it-1]
    let iNext = ((Array.findIndex (fun c -> c = c1) palette)+1)%palette.Length
    let c2 = palette.[iNext]
    let (s, n, d) = p
    let m = (((nu % 1.0)*(float(n-s)/float d))+(float s/float d))
    interpolate c1 c2 m

let histogram iterations (points : array<array<option<int * Complex>>>) =
    let h = Array.zeroCreate iterations
    let width = points.[0].Length
    let height = points.Length
    for i in 0..(width-1) do
        for j in 0..(height-1) do
            match points.[j].[i] with
            | Some (it, _) -> h.[it-1] <- h.[it-1] + 1
            | None -> ()
    let (accum, total) = Array.mapFold (fun acc i -> (acc + i, acc + i)) 0 h
    Array.map (fun n -> (float)n/(float)total) accum

let iterCount iterations (points : array<array<option<int * Complex>>>) =
    let h = Array.zeroCreate iterations
    let width = points.[0].Length
    let height = points.Length
    for i in 0..(width-1) do
        for j in 0..(height-1) do
            match points.[j].[i] with
            | Some (it, _) -> h.[it-1] <- h.[it-1] + 1
            | None -> ()
    h

let positions (adjPalette : array<Color>) (iterCounts : array<int>) =
    let rec separate (l : List<Color>) =
        match l with
        | x :: _ -> List.takeWhile (fun n -> n = x) l :: separate (List.skipWhile (fun n -> n = x) l)
        | _ -> []
    let lists = separate (List.ofArray adjPalette)
    let genCount n =
        [| for i in 1..n -> (i, n) |]
    let groups = List.map (fun (l : List<Color>) -> genCount l.Length) lists
    let rec processGroup i (all : list<array<(int * int)>>) =
        match all with
        | x :: xs ->
            let size = snd x.[0]
            let subIters = iterCounts.[i..(i+size-1)]
            let total = Array.sum subIters
            List.ofArray (Array.mapi (fun i _ -> ((Array.sum subIters.[0..(i-1)]), (Array.sum subIters.[0..i]), total)) subIters) :: (processGroup (i+size) xs)
        | _ -> []
    processGroup 0 groups |> List.concat |> Array.ofList

let makePalette (palette : array<Color>) (hist : array<float>) =
    let colorPalette : array<Color> = Array.zeroCreate hist.Length
    let colors = palette.Length
    let rec assign i lastC lastN =
        if i < colorPalette.Length then
            match (int (2.0*(float colors)*hist.[i-1])) with
            | n when n > lastN ->
                colorPalette.[i] <- palette.[(lastC+1)%colors]
                assign (i+1) (lastC+1) n
            | _ ->
                colorPalette.[i] <- palette.[lastC%colors]
                assign (i+1) lastC lastN
    assign 1 -1 -1
    colorPalette

let draw iterations (points : array<array<option<int * Complex>>>) =
    let palette = [|
        Color.FromArgb(255, 66, 30, 15);
        Color.FromArgb(255, 25, 7, 26);
        Color.FromArgb(255, 9, 1, 47);
        Color.FromArgb(255, 4, 4, 73);
        Color.FromArgb(255, 0, 7, 100);
        Color.FromArgb(255, 12, 44, 138);
        Color.FromArgb(255, 24, 82, 177);
        Color.FromArgb(255, 57, 125, 209);
        Color.FromArgb(255, 134, 181, 229);
        Color.FromArgb(255, 211, 236, 248);
        Color.FromArgb(255, 241, 233, 191);
        Color.FromArgb(255, 248, 201, 95);
        Color.FromArgb(255, 255, 170, 0);
        Color.FromArgb(255, 204, 128, 0);
        Color.FromArgb(255, 153, 87, 0);
        Color.FromArgb(255, 106, 52, 3);
    |]
    let width = points.[0].Length
    let height = points.Length
    let hist = histogram iterations points
    let iterCounts = iterCount iterations points
    let bitmap = new Bitmap(width, height)
    let adjPalette = makePalette palette hist
    let p = positions adjPalette iterCounts
    for i in 0..(width-1) do
        for j in 0..(height-1) do
            match points.[j].[i] with
            | Some (it, c) ->
                let logZn = (log ( c.Real*c.Real + c.Imaginary*c.Imaginary )) / 2.0
                let nu = float it + 1.0 - (log( logZn / log(2.0) )) / log(2.0)
                bitmap.SetPixel(i, j, (color adjPalette palette it nu p.[it-1]))
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
    //(positions ([|1./8.;2./8.;3./8.;8./8.|]) 1 ([|1;2;1;4|])) |> printfn "%A"
    // Interactive
    (*printf "->"
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
        render xPixels yPixels iterations x y r
        printf "\n->"
        key <- System.Console.ReadKey()
    printfn "\nBye!"*)
    0
