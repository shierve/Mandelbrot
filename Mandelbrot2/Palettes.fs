module Mandelbrot.Palettes

open System.Drawing

type Palette =
    struct
        static member wiki = [|
            Color.FromArgb(255, 66, 30, 15)
            Color.FromArgb(255, 25, 7, 26)
            Color.FromArgb(255, 9, 1, 47)
            Color.FromArgb(255, 4, 4, 73)
            Color.FromArgb(255, 0, 7, 100)
            Color.FromArgb(255, 12, 44, 138)
            Color.FromArgb(255, 24, 82, 177)
            Color.FromArgb(255, 57, 125, 209)
            Color.FromArgb(255, 134, 181, 229)
            Color.FromArgb(255, 211, 236, 248)
            Color.FromArgb(255, 241, 233, 191)
            Color.FromArgb(255, 248, 201, 95)
            Color.FromArgb(255, 255, 170, 0)
            Color.FromArgb(255, 204, 128, 0)
            Color.FromArgb(255, 153, 87, 0)
            Color.FromArgb(255, 106, 52, 3)
        |]
        //autumn
        static member autumn = [|
            Color.FromArgb(255, 16, 16, 26)
            Color.FromArgb(255, 80, 101, 132)
            Color.FromArgb(255, 251, 227, 176)
            Color.FromArgb(255, 253, 220, 76)
            Color.FromArgb(255, 254, 95, 43)
            Color.FromArgb(255, 135, 55, 34)
        |]
        static member sepia = [|
            Color.FromArgb(255, 31, 32, 44)
            Color.FromArgb(255, 59, 72, 105)
            Color.FromArgb(255, 173, 175, 198)
            Color.FromArgb(255, 231, 221, 221)
            Color.FromArgb(255, 215, 164, 152)
            Color.FromArgb(255, 231, 221, 221)
            Color.FromArgb(255, 173, 175, 198)
            Color.FromArgb(255, 59, 72, 105)
            Color.FromArgb(255, 31, 32, 44)
        |]
        static member wiki2 = [|
            Color.FromArgb(255,7,0,93)
            Color.FromArgb(255,17,25,135)
            Color.FromArgb(255,30,74,172)
            Color.FromArgb(255,67,118,205)
            Color.FromArgb(255,134,175,225)
            Color.FromArgb(255,208,232,247)
            Color.FromArgb(255,237,231,190)
            Color.FromArgb(255,245,201,90)
            Color.FromArgb(255,253,168,1)
            Color.FromArgb(255,200,129,1)
            Color.FromArgb(255,148,84,0)
            Color.FromArgb(255,100,49,1)
            Color.FromArgb(255,66,18,6)
            Color.FromArgb(255,14,3,14)
            Color.FromArgb(255,5,0,38)
            Color.FromArgb(255,5,0,71)
        |]
        static member wiki2reverse = [|
            Color.FromArgb(255,208,232,247)
            Color.FromArgb(255,134,175,225)
            Color.FromArgb(255,67,118,205)
            Color.FromArgb(255,30,74,172)
            Color.FromArgb(255,17,25,135)
            Color.FromArgb(255,7,0,93)
            Color.FromArgb(255,5,0,71)
            Color.FromArgb(255,5,0,38)
            Color.FromArgb(255,14,3,14)
            Color.FromArgb(255,66,18,6)
            Color.FromArgb(255,100,49,1)
            Color.FromArgb(255,148,84,0)
            Color.FromArgb(255,200,129,1)
            Color.FromArgb(255,253,168,1)
            Color.FromArgb(255,245,201,90)
            Color.FromArgb(255,237,231,190)
        |]
    end