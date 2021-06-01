
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

let email (email:string) =
    let login = (email:string).Split("@").[0]
    let domena = (email:string).Split('@').[1]

    (login, domena)

//zad 2.5

let sufix (login, domena:string) =
    let alias1 = domena.Split('.').[0]
    let alias2 = domena.Split('.').[1]

    printf "Email użytkownika: %A" login

    match alias1 with
    |"pcz" | "PCZ" -> printfn " należy do domeny PCZ"
    |_ -> printfn " nie należy do domeny PCZ" 

//zad 2.6

let Euklides (a1, b1, c1) (a2, b2, c2) = 
   sqrt((a1 - a2)**2.0 + (b1 - b2)**2.0 + (c1 - c2)**2.0)

//zad 2.7

let wnetrze (srodekX, srodekY) (R:float) (punktX, punktY) =
    let d = sqrt((srodekX - punktX)**2.0 + (srodekY - punktY)**2.0)
    match d with
    | k when k  <= R -> printfn "Punkt miesci sie w okregu"
    | _ -> printfn "Punkt miesci sie poza okregiem"

//zad 2.8

// do pelnej realizacji brakuje upraszczania wyników (podział licznika i mianownika przez NWD)
// i zabezpieczenia przed dzieleniem przez zero

let dodaj a b = 
    let licznik = fst a * snd b + fst b * snd a  
    let mianownik = snd a * snd b
    licznik, mianownik

let odejmij a b = 
    let licznik = fst a * snd b - fst b * snd a  
    let mianownik = snd a * snd b
    licznik, mianownik

let pomnoz a b = 
    let licznik = fst a * fst b 
    let mianownik = snd a * snd b
    licznik, mianownik

let podziel a b = 
    let licznik = fst a * snd b 
    let mianownik = snd a * fst b
    licznik, mianownik

//zad 2.9

// do pelnej realizacji brakuje upraszczania wyników (podział licznika i mianownika przez NWD)
// i zabezpieczenia przed dzieleniem przez zero

type Ulamki = {
    licznik:int;
    mianownik:int;
}

let dodwanie ulamek1 ulamek2 =
    let licznik = ulamek1.licznik * ulamek2.mianownik + ulamek2.licznik * ulamek1.mianownik
    let mianownik = ulamek1.mianownik * ulamek2.mianownik
    {licznik=licznik; mianownik=mianownik}

let odejmowanie ulamek1 ulamek2 =
    let licznik = ulamek1.licznik * ulamek2.mianownik - ulamek2.licznik * ulamek1.mianownik
    let mianownik = ulamek1.mianownik * ulamek2.mianownik
    {licznik=licznik; mianownik=mianownik}

let mnozenie ulamek1 ulamek2 =
    let licznik = ulamek1.licznik * ulamek2.licznik
    let mianownik = ulamek1.mianownik * ulamek2.mianownik
    {licznik=licznik; mianownik=mianownik}

let dzielenie ulamek1 ulamek2 =
    let licznik = ulamek1.licznik * ulamek2.mianownik
    let mianownik = ulamek1.mianownik * ulamek2.licznik
    {licznik=licznik; mianownik=mianownik}

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
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.2"
    let para = para()
    match para with 
    | (a,b) when a > b -> printfn "Pierwsza liczba jest większa jak druga"
    | (a,b) when b > a -> printfn "Druga liczba jest większa jak pierwsza"
    | (_,_) -> printfn "Obie liczby są równe"
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()
    
    printfn "Zad 2.3"
    let a = 3.0
    let b = 3.0
    let c = 3.0
    let wynik = trojkat a b c
    printfn "Pole trojkata o bokach %f, %f i %f to %f, a jego obwod to %f" a b c (fst wynik) (snd wynik) 
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.4"
    printfn "Podaj adres email: "
    let adres = Console.ReadLine()
    let para = (email adres)
    printfn "Identyfikator żytkownika to %s" (fst para)
    printfn "Adres domeny to %s" (snd para)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.5"
    printfn "Podaj adres email: "
    let adress = Console.ReadLine()
    sufix (email adress)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.6"
    let A = (1.0, 1.0, 1.0)
    let B = (2.0, 2.0, 2.0)
    printfn "Odleglosc od siebie punktow %s i %s wynosi %f" (string(A)) (string(B)) (Euklides A B)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.7"
    let srodek = (2.0, 1.0)
    let promien = 1.0
    let punkt = (1.0, 1.0)
    wnetrze srodek promien punkt
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.8"

    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.9"
    let ulamki = { X = (2.0 / 3.0) ; Y = (3.0 / 5.0)}
    let wynik = dodwanie ulamki
    let wynik2 = odejmowanie ulamki
    let wynik3 = mnozenie ulamki
    let wynik4 = dzielenie ulamki
    printfn "Wyniki dodawania ułamków %A" wynik
    printfn "Wyniki dodawania ułamków %A" wynik2
    printfn "Wyniki mnożenia ułamków %A" wynik3
    printfn "Wyniki dzielenia ułamków %A" wynik4
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.10"
    let wynik = licz ("17/04/2021", "01/01/1990")
    if (wynik % 7 = 0) then 
        printfn "Poniedziałek"
    else 
        let sum = wynik/7
        let sum1 = sum * 7
        let sum2 = wynik - sum1
        printfn "%A" (dzien sum2)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.11"
    printfn "Dzielenie liczb.\nPodaj dzielną: "
    let a = float (Console.ReadLine ())
    printfn "Podaj dzielnik: "
    let b = float (Console.ReadLine ())
    let wynik = dzielenie_zad11 a b
    (wynik_dzielenia wynik)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.12"
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.13"
    printfn "Podaj długości boków trójkąta: "
    let a = float (Console.ReadLine ())
    let b = float (Console.ReadLine ())
    let c = float (Console.ReadLine ())
    let wynik1 = trojkat_pole (a, b, c)
    let wynik2 = trojkat_obwod (a, b, c)
    (wynik_trojkata wynik1)
    (wynik_trojkata2 wynik2)
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.14"
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    printfn "Zad 2.15"
    menu {imie="";nazwisko="";wiek=0} true
    printfn "\nNaciśnij przycisk by kontynuować"
    Console.ReadKey() |> ignore
    Console.Clear()

    0 
