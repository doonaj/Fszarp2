
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

let slowo email =
    let login = string((email:string).Split("@").[0])
    let domena = string((email:string).Split('@').[1])

    printfn "Login: %A" login
    printfn "Domena: %A" domena

//zad 2.5

let slowo2 email=
    let login = string((email:string).Split("@").[0])
    let domena = string((email:string).Split('@').[1])
    let alias1 = string(domena.Split('.').[0])
    let alias2 = string(domena.Split('.').[1])

    printf "Email użytkownika: %A" login

    match alias1 with
    |"pcz" -> printf " Należy do domeny PCZ"
    |_ -> printf " Nie należy do domeny PCZ" 

//zad 2.6



//zad 2.7



//zad 2.8



//zad 2.9

type Ulamki2 = {
    X:float;
    Y:float;
}

let dodwanie Ulamki2 =
    Ulamki2.X + Ulamki2.Y

let odejmowanie Ulamki2 =
    Ulamki2.X - Ulamki2.Y

let mnozenie Ulamki2 =
    Ulamki2.X * Ulamki2.Y

let dzielenie Ulamki2 =
    Ulamki2.X / Ulamki2.Y

//zad 2.10

let licz (x, y) = 
    let fromdate = DateTime.Parse(x)
    let fromdate2 = DateTime.Parse(y)
    (fromdate - fromdate2).Days

let dzien nr =
    match nr with
    |1 -> printf "Wtorek"
    |2 -> printf "Środa"
    |3 -> printf "Czwartek"
    |4 -> printf "Piatek"
    |5 -> printf "Sobota"
    |6 -> printf "Niedziela"

//zad 2.11

let dzielenie_zad11 a b =
    if b <> 0.0 then
        (a/b, true)
    else
        (0.0, false)

let wynik_dzielenia wynik =
    match wynik with
    | (x, true) when x>0.0 -> printfn "Wynik to: %A jest dodatni" x
    | (x, true) when x=0.0 -> printfn "Wynik to: %A jest zerem" x
    | (x, true) -> printfn "Wynik to: %A jest ujemny" x
    | (_,false) -> printfn "Nie można dzielić przez 0"

//zad 2.12



//zad 2.13

let trojkat_obwod (a, b, c) = 
    if a > b && a > c then
        let suma = b + c
        if a >= suma then
            (0.0, false)
        else
        let obwod = a + b + c  
        (obwod, true)
    elif b > a && b > c then
        let suma = a + c
        if b >= suma then
            (0.0, false)
        else
        let obwod = a + b + c  
        (obwod, true)
    elif c > a && c > b then
        let suma = a + b
        if c >= suma then
            (0.0, false)
        else
        let obwod = a + b + c  
        (obwod, true)
    else
        (0.0, false)

let trojkat_pole (a, b, c) = 
    let p = (a+b+c)/2.0
    if a > b && a > c then
        let suma = b + c
        if a >= suma then
            (0.0, false)
        else
        let pole = Math.Sqrt(p*(p-a)*(p-b)*(p-c))
        let obwod = a + b + c  
        (pole, true)
    elif b > a && b > c then
        let suma = a + c
        if b >= suma then
            (0.0, false)
        else
        let pole = Math.Sqrt(p*(p-a)*(p-b)*(p-c))
        (pole, true)
    elif c > a && c > b then
        let suma = a + b
        if c >= suma then
            (0.0, false)
        else
        let pole = Math.Sqrt(p*(p-a)*(p-b)*(p-c))
        (pole, true)
    else
        (0.0, false)

let wynik_trojkata wynik =
    match wynik with
    | (x, true) -> printf "Pole wynosi: %A"x
    | (_,false) -> printf "Nie można policzyć obwodu"

let wynik_trojkata2 wynik =
    match wynik with
    | (x, true) -> printf " a obwód wynosi: %A"x
    | (_,false) -> printf " Nie można policzyć pola"

//zad 2.14



//zad 2.15

type Osoba = {
    imie:string;
    nazwisko:string;
    wiek:int;
}

let printMenu () =
    Console.Clear();
    printfn "1 - Dodaj rekord"
    printfn "2 - Edytuj rekord"
    printfn "3 - Pokaz rekord"
    printfn "4 - koniec"

let wczytaj komunikat = 
    printfn "%s" komunikat
    Console.ReadLine()

let pobierzRekord () = 
    let imie = wczytaj "Podaj imię: "
    let nazwisko = wczytaj "Podaj nazwisko: "
    let wiek = int (wczytaj "Podaj wiek: ")
    {imie = imie; nazwisko = nazwisko; wiek = wiek}

let edytujRekord osoba =

    let nowaOsoba = pobierzRekord ()

    { 
      imie = if String.IsNullOrWhiteSpace nowaOsoba.imie then osoba.imie else nowaOsoba.imie;
      nazwisko = if String.IsNullOrWhiteSpace nowaOsoba.nazwisko then osoba.nazwisko else nowaOsoba.nazwisko;
      wiek = if nowaOsoba.wiek = 0 then osoba.wiek else nowaOsoba.wiek;
    }

let pokazRekord osoba =
    printfn "%s" osoba.imie
    printfn "%s" osoba.nazwisko
    printfn "%d" osoba.wiek
    Console.ReadKey() |> ignore
  


let rec menu osoba kont =
    if kont then
        printMenu ()
        let klawisz = Console.ReadKey ()
        let osoba,kontynuuj = 
            match klawisz.Key with
            | ConsoleKey.D1 -> pobierzRekord (), true
            | ConsoleKey.D2 -> edytujRekord osoba, true
            | ConsoleKey.D3 -> pokazRekord osoba; osoba, true
            | ConsoleKey.D4 -> osoba, false
            | _ -> osoba, true
        menu osoba kontynuuj

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
    let wynik = trojkat 3.0 3.0 3.0
    printfn "Pole trojkata to %f, a jego obwod to %f" (fst wynik) (snd wynik) 

    printfn "Zad 2.4"
    let email = string (Console.ReadLine())
    (slowo email)

    printfn "Zad 2.5"
    let adress = string (Console.ReadLine())
    (slowo2 adress)

    printfn "Zad 2.6"
    printfn "Zad 2.7"
    printfn "Zad 2.8"
    printfn "Zad 2.9"
    let wynik = dodwanie {X = (2.0 / 3.0) ; Y = (3.0 / 5.0)}
    let wynik2 = odejmowanie {X = (2.0 / 3.0) ; Y = (3.0 / 5.0)}
    let wynik3 = mnozenie {X = (2.0 / 3.0) ; Y = (3.0 / 5.0)}
    let wynik4 = dzielenie {X = (2.0 / 3.0) ; Y = (3.0 / 5.0)}
    printfn "Wyniki operacji na ułamkach 1: %A" wynik
    printfn "Wyniki operacji na ułamkach %A" wynik2
    printfn "Wyniki operacji na ułamkach %A" wynik3
    printfn "Wyniki operacji na ułamkach %A" wynik4

    printfn "Zad 2.10"
    let wynik = licz ("17/04/2021", "01/01/1990")
    if (wynik % 7 = 0) then 
        printfn "Poniedziałek"
    else 
        let sum = wynik/7
        let sum1 = sum * 7
        let sum2 = wynik - sum1
        printfn "%A" (dzien sum2)

    printfn "Zad 2.11"
    printfn "Podaj wartosc A i B: "
    let a = float (Console.ReadLine ())
    let b = float (Console.ReadLine ())
    let wynik = dzielenie_zad11 a b
    (wynik_dzielenia wynik)

    printfn "Zad 2.12"
    printfn "Zad 2.13"
    printfn "Podaj wartosc A i B: "
    let a = float (Console.ReadLine ())
    let b = float (Console.ReadLine ())
    let c = float (Console.ReadLine ())
    let wynik1 = trojkat_pole (a, b, c)
    let wynik2 = trojkat_obwod (a, b, c)
    (wynik_trojkata wynik1)
    (wynik_trojkata2 wynik2)

    printfn "Zad 2.14"
    printfn "Zad 2.15"
    menu {imie="";nazwisko="";wiek=0} true

    0 
