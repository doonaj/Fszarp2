
open System

//zad 2.1

let wybor () =
    printfn "Podaj wartosc: "
    let x = int(Console.ReadLine())
    match x with
    | 1 -> printfn "wprowadziles 1"
    | 2 -> printfn "wprowadziles 2"
    | 3 -> printfn "wprowadziles 3"
    | 4 -> printfn "wprowadziles 4"
    | _ -> printfn "wprowadziles inna wartosc"

//zad 2.2

let para () =
    printfn "Podaj dwie wartosci: "
    let x = float(Console.ReadLine())
    let y = float(Console.ReadLine())
    (x, y) 
    
    

//zad 2.3

let trojkat a b c =
    let p = (a + b + c) / 2.0
    (sqrt(p * (p - a) * (p - b) * (p - c)), p * 2.0)

//zad 2.4



//zad 2.5



//zad 2.6



//zad 2.7



//zad 2.8



//zad 2.9



//zad 2.10



//zad 2.11



//zad 2.12



//zad 2.13



//zad 2.14



//zad 2.15

[<EntryPoint>]
let main argv =
    
    printfn "Zad 2.1"
    wybor()

    printfn "Zad 2.2"
    let para = para()
    match para with 
    | (a,b) when a > b -> printfn "Pierwsza liczba jest większa jak druga"
    | (a,b) when b > a -> printfn "Druga liczba jest większa jak pierwsza"
    | (_,_) -> printfn "Obie liczby są równe"

    printfn "Zad 2.3"

    printfn "Zad 2.4"
    printfn "Zad 2.5"
    printfn "Zad 2.6"
    printfn "Zad 2.7"
    printfn "Zad 2.8"
    printfn "Zad 2.9"
    printfn "Zad 2.10"
    printfn "Zad 2.11"
    printfn "Zad 2.12"
    printfn "Zad 2.13"
    printfn "Zad 2.14"
    printfn "Zad 2.15"

    0 
