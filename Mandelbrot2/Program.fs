open System.Drawing
open System.IO
open System.Numerics

let f (z : Complex) (c : Complex) = (z * z) + c

let rec iterate currentIteration maxIterations (z : Complex) (c : Complex) =
    if z.Magnitude > 2.0 then Some currentIteration
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

let draw iterations (points : array<array<option<int>>>) =
    let palette = [| for i in 0..iterations -> Color.FromArgb(255, 0, 0, (255*i/iterations)) |]
    let width = points.[0].Length
    let height = points.Length
    let bitmap = new Bitmap(width, height)
    for i in 0..(width-1) do
        for j in 0..(height-1) do
            match points.[j].[i] with
            | Some it -> bitmap.SetPixel(i, j, palette.[it])
            | None -> bitmap.SetPixel(i, j, Color.Black)
    bitmap

[<EntryPoint>]
let main argv =
    let xPixels = int argv.[0]
    let yPixels = int argv.[1]
    let iterations = int argv.[2]
    let x = float argv.[3]
    let y = float argv.[4]
    let r = float argv.[5]
    let bm = pointsMatrix xPixels yPixels (Complex(x, y)) r |> mandelbrot iterations |> draw iterations
    bm.Save(Path.Combine(__SOURCE_DIRECTORY__, "plot.png"))
    0
