namespace Mandelbrot.Numerics

open System
open System.Numerics
open System.Globalization


// invariants: (p,q) in lowest form, q >= 0
type BigRational (p : BigInteger, q : BigInteger) =
    member __.IsNegative =
        sign p < 0

    member __.IsPositive =
        sign p > 0

    member __.Numerator = p

    member __.Denominator = q

    member __.Sign =
        sign p

    override __.GetHashCode () =
        // This hash code must be identical to the hash for BigInteger when the numbers coincide.
        if q.IsOne then p.GetHashCode ()
        else (p.GetHashCode () <<< 3) + q.GetHashCode ()

    override __.ToString () =
        if q.IsOne then
            p.ToString ()
        else
            p.ToString () + "/" + q.ToString ()

    static member Equals (x : BigRational, y : BigRational) =
        // normal form, so structural equality
        x.Numerator = y.Numerator && x.Denominator = y.Denominator

    static member Compare (x : BigRational, y : BigRational) =
        compare (x.Numerator * y.Denominator) (y.Numerator * x.Denominator)

    static member ToDouble (num : BigRational) =
        float num.Numerator / float num.Denominator

    static member Normalize (p : BigInteger, q : BigInteger) =
        if q.IsZero then
            (* throw for any x/0 *)
            raise <| System.DivideByZeroException ()
        elif q.IsOne then
            BigRational (p, q)
        else
            let k = BigInteger.GreatestCommonDivisor (p, q)
            let p = p / k
            let q = q / k
            if sign q < 0 then
                BigRational (-p, -q)
            else
                BigRational (p, q)
    
    static member Normalize (R : BigRational) =
        BigRational.Normalize (R.Numerator, R.Denominator)

    static member Create (p : int, q : int) =
        BigRational.Normalize (bigint p, bigint q)

    static member Create (p, q) =
        BigRational.Normalize (p, q)

    /// Return the given rational number
    static member (~+) (n1 : BigRational) = n1

    /// Return the negation of a rational number
    static member (~-) (num : BigRational) =
        // still coprime, bq >= 0
        BigRational (-num.Numerator, num.Denominator)

    /// Return the sum of two rational numbers
    static member (+) (x : BigRational, y : BigRational) =
        BigRational.Normalize ((x.Numerator * y.Denominator) + (y.Numerator * x.Denominator), x.Denominator * y.Denominator)

    /// Return the difference of two rational numbers
    static member (-) (x : BigRational, y : BigRational) =
        BigRational.Normalize ((x.Numerator * y.Denominator) - (y.Numerator * x.Denominator), x.Denominator * y.Denominator)

    /// Return the product of two rational numbers
    static member (*) (x : BigRational, y : BigRational) =
        BigRational.Normalize (x.Numerator * y.Numerator, x.Denominator * y.Denominator)

    /// Return the ratio of two rational numbers
    static member (/) (x : BigRational, y : BigRational) =
        BigRational.Normalize (x.Numerator * y.Denominator, x.Denominator * y.Numerator)
    
    static member Log (x : BigRational) =
        (BigInteger.Log x.Numerator) - (BigInteger.Log x.Denominator)

    static member Reciprocal (num : BigRational) =
        BigRational.Normalize (num.Denominator, num.Numerator)

    static member Pow (num : BigRational, n : int) =
        // p,q powers still coprime
        if n < 0 then BigRational.Normalize (BigInteger.Pow (num.Denominator, -n), BigInteger.Pow (num.Numerator, -n))
        else BigRational (BigInteger.Pow (num.Numerator, n), BigInteger.Pow (num.Denominator, n))

    static member FromBigInteger z =
        BigRational.Create (z, BigInteger.One)

    static member FromInt32 n =
        BigRational.Create (n, 1)

    /// Returns the integer part of a rational number.
    static member ToBigInteger (num : BigRational) =
        // have p = d.q + r, |r| < |q|
        let d, r = BigInteger.DivRem (num.Numerator, num.Denominator)

        if r < BigInteger.Zero then
            // p = (d-1).q + (r+q)
            d - BigInteger.One
        else
            // p = d.q + r
            d

    static member Parse (str : string) =
        let len = str.Length
        if len = 0 then
            invalidArg "str" "empty string"

        let j = str.IndexOf '/'
        if j >= 0 then
            let p = BigInteger.Parse (str.Substring (0, j))
            let q = BigInteger.Parse (str.Substring (j + 1, len - j - 1))
            BigRational.Create (p, q)
        else
            //TODO: check for decimal point, then e-X
            let p = BigInteger.Parse str
            BigRational.Create (p, BigInteger.One)

    override this.Equals (that : obj) =
        match that with
        | :? BigRational as that ->
            BigRational.Equals (this, that)
        | _ -> false

    interface System.IComparable with
        member this.CompareTo (obj : obj) =
            match obj with
            | :? BigRational as other ->
                BigRational.Compare (this, other)
            | _ ->
                invalidArg "obj" "the object does not have the correct type"

    interface System.IComparable<BigRational> with
        member this.CompareTo other =
            BigRational.Compare (this, other)


type BigComplex (a : BigRational, b : BigRational) =
    member __.Real = a

    member __.Imaginary = b

    override __.ToString () =
        a.ToString () + " + " + b.ToString () + "i"

    static member Create (a, b) =
        BigComplex (a, b)

    // returns true if the module is bigger than a given integer (n is the square of the integer to compare)
    static member biggerThanSquared (c : BigComplex) (n : int) =
        ((c.Real*c.Real) + (c.Imaginary*c.Imaginary)) > BigRational.Create ((n), 1)

    static member (*) (x : BigComplex, y : BigComplex) =
        BigComplex (((x.Real*y.Real) - (x.Imaginary*x.Imaginary)), ((x.Real*y.Imaginary) + (x.Imaginary*y.Real)))
    
    static member (+) (x : BigComplex, y : BigComplex) =
        BigComplex ((x.Real+y.Real), (x.Imaginary*y.Imaginary))
    
    static member zero =
        let a = BigRational.FromInt32 0
        let b = BigRational.FromInt32 0
        BigComplex(a, b)