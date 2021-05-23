
open System

// zad 1.1
let pole r = Math.PI*r**2.0

// zad 1.2
let pierwiastki a b c =
    printf "Funkcja %.2fx^2 + %.2fx + %.2f " a b c
    let delta = b**2.0 - 4.0*a*c
    if a = 0.0 then
        printf "nie jest funkcja kwadratowa"
    if delta < 0.0 then
        printfn "nie ma pierwiastkow."
    elif delta = 0.0 then
        let x = -b/(2.0*a)
        printfn " ma jeden pierwiastek x = %.2f." x
    else
        let x1 = (-b + delta)/(2.0*a)
        let x2 = (-b - delta)/(2.0*a)
        printfn " ma dwa pierwiastek x1 = %.2f i x2 = %.2f." x1 x2

// zad 1.3
let trojkat a b c =
    if a <> 0.0 && b <> 0.0 && c <> 0.0 && a < b + c && b < a + c && c < a + b then
        true
    else 
        false

// zad 1.4
let poleTrojkata a b c = 
    if trojkat a b c then 
        let p = (a + b + c) / 2.0
        let P = sqrt(p * (p - a) * (p - b) * (p - c))
        printfn "Pole tego trojkata wynosi %.2f" P
    else 
        printfn "Z odcinkow o dlugosci %.2f, %.2f i %.2f nie można stworzyć trojkata." a b c

// zad 1.5
let rec suma n =
    if n <= 0 then
        failwith "Nieprawidlowe dane wejsciowe"
    else
        let rec sum n a =
            if n = 1 then
                a
            else sum (n-1) (n+a)
        sum n 1

// zad 1.6
let rec potega x n =
    if n <= 0 then
        failwith "Nieprawidlowe dane wejsciowe."
    else
        let rec pot x n a =
            if n = 1 then
                a
            else pot x (n-1) (x*a)
        pot x n x

// zad 1.7
let rec fib n =
    if n < 0 then
        failwith "Nieprawidlowe dane wejsciowe."
    else
        let rec nfib n a b =
            if n = 0 then
                a
            elif n = 1 then
                b
            else 
                nfib  (n - 1) b (a + b)
        nfib n 0 1       

// zad 1.8
let rec dwumianNewtona n k =
    if n < 0 || k < 0 || k > n then 
        failwith "Nieprawidlowe dane wejsciowe."
    elif k = 0 || k = n then
        1
    else
        dwumianNewtona (n - 1) (k - 1) + dwumianNewtona (n - 1) k

// zad 1.9
let rec czyPierwsza n = 
    if n < 1 then
        failwith "Nieprawidlowe dane wejsciowe."
    else   
        let rec czyPierwszaTR a test=
            if (not test) then 
                false
            elif a <= 1 then 
                true
            else czyPierwszaTR (a - 1) (n % a <> 0 && test)
        czyPierwszaTR (n/2) true

// zad 1.10
let rec szansaNa6 () = 
    let gll = new Random()
    let rec szansa i suma = 
        if i = 1000 then
            (float)suma/1000.0
        else
            let rzut = gll.Next(6) + 1
            if rzut = 6 then 
                szansa (i + 1) (suma + 1)
            else
                szansa (i + 1) suma
    szansa 0 0

// zad 1.11
let rec szansaNa66 () = 
    let gll = new Random()
    let rec szansa i suma = 
        if i = 1000 then
            (float)suma/1000.0
        else
            let rzut1 = gll.Next(6) + 1
            let rzut2 = gll.Next(6) + 1
            if rzut1 = 6 && rzut2 = 6 then 
                szansa (i + 1) (suma + 1)
            else
                szansa (i + 1) suma
    szansa 0 0

// zad 1.12
let rec NWD a b =
    if b<>0 then
        NWD b (a%b)
    else a

// zad 1.13
let rec wartosciSzeregow () =
    let rec wartosc1 () =    
        let e = 10.0**(-7.0)
        let rec szereg1 i suma = 
            let element = 1.0/(((float)i)**2.0)
            if element < e then
                suma
            else
                szereg1 (i + 1) (suma + element)
        szereg1 1 0.0
    let rec wartosc2 () =        
        let e = 10.0**(-7.0)
        let rec szereg2 i suma = 
            let element = ((-1.0)**(float)i)/(float)(Funkcje.silnia i)
            if element < e && element > -e then
                suma
            else
                szereg2 (i + 1I) ((float)suma + (float)element)
        szereg2 1I 0.0
    let rec wartosc3 () =
        let e = 10.0**(-7.0)
        let rec szereg3 i suma =
            let element = 1.0/(((float)i)*((float)i+1.0))
            if element < e then
                suma
            else
                szereg3 (i + 1) (suma + element)
        szereg3 1 0.0
    let rec wartosc4 () =    
        let e = 10.0**(-7.0)
        let rec szereg4 i suma =
            let element = ((-2.0)**(float)i) / (float)(Funkcje.silnia i)
            if element < e && element > -e then
                suma
            else
                szereg4 (i + 1I) (suma + element)
        szereg4 1I 0.0

    printfn "Wartosc szeregu 1 / i^2 wynosi: %A" (wartosc1 ())
    printfn "Wartosc szeregu (-1)^i / i! wynosi: %A" (wartosc2 ())
    printfn "Wartosc szeregu 1 / (i * (i + 1)) wynosi: %A" (wartosc3 ())
    printfn "Wartosc szeregu (-2.0)^i / i! wynosi: %A" (wartosc4 ())

// zad 1.15
[<Measure>] type C 
[<Measure>] type F
let zmianaCF:float<F*C^-1> = 1.8<F/C>
let konwersjaCF(C:float<C>) = (32.0<F>) + zmianaCF * C
    
// zad 1.16
let zmianaFC:float<C*F^-1> = 5.0/9.0<F/C>
let konwersjaFC(F:float<F>) = (F - 32.0<F>) * zmianaFC 

// zad 1.17
let rec palindrom str = 
    let rec palindromLicznik (str:string) i k = 
        if i = k then 
            if str.[i] = str.[k] then true
            else false
        elif i > k then true
        elif str.[i] = str.[k] then palindromLicznik str (i+1) (k-1)
        else false
    palindromLicznik str 0 (str.Length-1)

// zad 1.18
let rec licznikZnakow tekst znak = 
    let rec licznik (tekst:string) (znak:char) suma i = 
        if i = tekst.Length then suma
        elif tekst.[i] = znak then licznik tekst znak (suma + 1) (i + 1)
        else licznik tekst znak suma (i + 1)
    licznik tekst znak 0 0

// zad 1.19 
let licznikWyrazow (tekst:string) = 
    let slowa = tekst.Split([|' '; '\n'|])
    let ile = slowa.Length
    ile

// zad 1.20
let licznikLiczbWCiagachLiczbowych tekst = 
    let rec licznik (tekst:string) (suma:int) dlugosc i =
        if i = tekst.Length && dlugosc > 1 then suma + dlugosc
        elif i = tekst.Length && dlugosc <= 1 then suma
        else
            if tekst.[i] = '0' || tekst.[i] = '1' || tekst.[i] = '2' || tekst.[i] = '3' 
                || tekst.[i] = '4' || tekst.[i] = '5'|| tekst.[i] = '6' 
                || tekst.[i] = '7' || tekst.[i] = '8' || tekst.[i] = '9' then 
                    licznik tekst suma (dlugosc + 1) (i + 1)
            elif dlugosc > 1 then licznik tekst (suma + dlugosc) 0 (i+1)
            else licznik tekst suma 0 (i+1)
    licznik tekst 0 0 0

let najdluzszyCiagLiczbowy tekst = 
    let rec licznik (tekst:string) (najdluzszy:int) dlugosc i =
        if i = tekst.Length && dlugosc > najdluzszy then dlugosc
        elif i = tekst.Length && dlugosc <= najdluzszy then najdluzszy
        else
            if tekst.[i] = '0' || tekst.[i] = '1' || tekst.[i] = '2' || tekst.[i] = '3' 
                || tekst.[i] = '4' || tekst.[i] = '5'|| tekst.[i] = '6'
                || tekst.[i] = '7' || tekst.[i] = '8' || tekst.[i] = '9' then 
                    if dlugosc < najdluzszy then licznik tekst najdluzszy (dlugosc + 1) (i + 1) 
                    else licznik tekst (dlugosc + 1) (dlugosc + 1) (i + 1)
            else licznik tekst najdluzszy 0 (i + 1)
    licznik tekst 0 0 0

// zad 1.21


// zad 1.22


// zad 1.23
//trojkat z zad 1.3

let prostokatny a b c =
    if a**2.0 + b**2.0 = c**2.0 || b**2.0 + c**2.0 = a**2.0 || c**2.0 + a**2.0 = b**2.0 
    then true else false

let rownoboczny a b c = 
    if  a = b && a = c then true else false

let rownoramienny a b c = 
    if  a = b || b = c || c = a then true else false

// zad 1.24
let dataPesel (pesel:string) = 
    let rok = int(pesel.[0..1])
    let miesiac = int(pesel.[2..3])
    let dzien = int(pesel.[4..5])
    if  dzien = 29 && miesiac % 20 = 2 && rok % 4 = 0 && rok <> 0 || int(pesel.[0..5]) = 229 then true 
    elif dzien >= 1 && dzien <= 28 && miesiac % 20 = 2 then true
    elif dzien >= 1 && dzien <= 30 && (miesiac % 20 = 4 || miesiac % 20 = 6 || miesiac % 20 = 9 
        || miesiac % 20 = 11) then true
    elif dzien >= 1 && dzien <= 31 && (miesiac % 20 = 1 || miesiac % 20 = 3 || miesiac % 20 = 5 
        || miesiac % 20 = 7 || miesiac % 20 = 8 || miesiac % 20 = 10 || miesiac % 20 = 12) then true
    else false 

let sumaKontrolna (pesel:string) =
    let sumaWazona = 
        int(pesel.[0]) * 1 + int(pesel.[1]) * 3 + int(pesel.[2]) * 7 
        + int(pesel.[3]) * 9 + int(pesel.[4]) * 1 + int(pesel.[5]) * 3 
        + int(pesel.[6]) * 7 + int(pesel.[7]) * 9 + int(pesel.[8]) * 1 + int(pesel.[9]) * 3
    if sumaWazona = int(pesel.[10]) then true else false

let dlugoscPesel (pesel:string) = 
    if pesel.Length = 11 then true else false

let czyPesel (pesel:string) =
    if pesel.Length = 11 then
        if (dataPesel pesel) && (sumaKontrolna pesel) then true else false
    else false
        
// zad 1.25
let szyfruj (tekst:string) kod =
    let zaszyfrowane = String.init tekst.Length ( fun i -> sprintf"%c" ( if int(tekst.[i]) - kod > 127 then char(int(tekst.[i]) + kod - 128) else char(int(tekst.[i]) + kod) ) )
    zaszyfrowane

// zad 1.26
let deszyfruj (tekst:string) kod =
    let odszyfrowane = String.init tekst.Length ( fun i -> sprintf"%c" ( if int(tekst.[i]) - kod < 0 then char(int(tekst.[i]) - kod + 128) else char(int(tekst.[i]) - kod) ) )
    odszyfrowane

// zad 1.27




[<EntryPoint>]
let main argv =
   
    printfn "Zad 1.1"
    let r = 2.0
    let x = pole r
    printfn "Dla promienia %.2f pole wynosi %.2f" r x
    
    printfn "\nZad 1.2"
    pierwiastki 1.0 2.0 3.5

    printfn "\nZad 1.3"
    let a = 1.2
    let b = 3.6
    let c = 4.0
    printf "Z odcinkow o dlugosci %.2f, %.2f i %.2f " a b c
    if trojkat a b c then 
        printfn "można stworzyć trojkat."
    else
        printfn "nie można stworzyć trojkata."

    printfn "\nZad 1.4"
    poleTrojkata a b c
   
    printfn "\nZad 1.5"
    let n = 7
    printfn "Suma %d pierwszych liczb naturalnych to %d" n (suma n)
    
    printfn "\nZad 1.6"
    let x = 2
    let n = 10
    printfn "%d do potegi %d wynosi %d" x n (potega x n)

    printfn "\nZad 1.7"
    let n = 10
    printfn "%d element ciagu Fibonacciego to %d" n (fib n)

    printfn "\nZad 1.8"
    let n = 4
    let k = 2
    printfn "Dwumian Newtona dla n = %d i k = %d wynosi %d" n k (dwumianNewtona n k)

    printfn "\nZad 1.9"
    let n = 73
    if (czyPierwsza n) then
        printfn "Liczba %d jest liczbą pierwsza" n
    else 
        printfn "Liczba %d nie jest liczbą pierwsza" n

    printfn "\nZad 1.10"
    printfn "Prwadopodobenstwo wyrzucenia 6 przy rzucie kostka wynosi: %.3f" (szansaNa6 ())

    printfn "\nZad 1.11"
    printfn "Prwadopodobenstwo wyrzucenia dwoch 6 przy rzucie dwiema kostkami wynosi: %.3f" (szansaNa66 ())

    printfn "\nZad 1.12"
    let a = 641424
    let b = 784931
    printfn "Największy wspolny dzielnik liczb %d i %d to %d" a b (NWD a b)
    
    printfn "\nZad 1.13"
    wartosciSzeregow ()
    
    printfn "\nZad 1.15"
    let temperaturaC = 41.0<C>
    let temperaturaF = konwersjaCF(temperaturaC)
    printfn "Temperatura %.2fC w stopniach Farenhaita to %.2f" temperaturaC temperaturaF

    printfn "\nZad 1.16"
    let TempF = 120.0<F>
    let TempC = konwersjaFC(TempF)
    printfn "Temperatura %.2fF w stopniach Celcjusza to %.2f" TempF TempC
    
    printfn "\nZad 1.17"
    let wyraz = "anna"
    printf "Wyraz %s " wyraz
    if not (palindrom wyraz) then printf "nie "
    printfn "jest palindromem"

    printfn "\nZad 1.18, 1.19, 1.20,"
    let tekst = "1. Proba zliczenia 5423 z 26436. Zadzwon jak sie skonczy 123456789"
    printfn "%s" tekst
    printfn "Tekst ma %d znakow '%c'" (licznikZnakow tekst 'a') 'a'
    printfn "Tekst ma %d wyrazow" (licznikWyrazow tekst)
    printfn "Tekst ma %d cyfr w ciągach" (licznikLiczbWCiagachLiczbowych tekst)
    printfn "Najdluzszy ciag ma %d cyfr" (najdluzszyCiagLiczbowy tekst)
    printfn "\nZad 1.21"
    printf "Podaj imie: "
    let imie = Console.ReadLine()
    printf "Podaj nazwisko: "
    let nazwisko = Console.ReadLine()
    printfn "Witaj %s %s" imie nazwisko

    printfn "\nZad 1.22"
    printfn "Sprwadzenie Przestępności roku\nPodaj rok: "
    let rok = int(Console.ReadLine())
    if rok % 4 = 0 && rok % 100 <> 0 || rok % 400 = 0 then
        printfn "rok %d jest rokiem przystepnym" rok
    else printfn "rok %d nie jest rokiem przystepnym" rok

    printfn "\nZad 1.23"
    printfn "Sprwadzenie rodzaju trojkata\nPodaj trzy dlugosci bokow: "
    let a = float(Console.ReadLine())
    let b = float(Console.ReadLine())
    let c = float(Console.ReadLine())
    if trojkat a b c then
        if rownoboczny a b c then 
            printfn "Trojkat o bokach %.2f %.2f %.2f jest rownoboczny" a b c
        elif rownoramienny a b c then 
            printf "Trojkat o bokach %.2f %.2f %.2f jest rownoramienny" a b c
            if prostokatny a b c then 
                printfn "m trojkatem prostokatnym"
            else printfn ""
        elif prostokatny a b c then 
            printfn "Trojkat o bokach %.2f %.2f %.2f jest prostokatny" a b c
        else 
            printfn "Trojkat o bokach %.2f %.2f %.2f jest trojkatem dowolnym" a b c
    else 
        printfn "Z dlugosci %.2f %.2f %.2f odcinkow nie na sie utworzyc trojkata" a b c
    
    printfn "\nZad 1.24"
    printfn "Podaj liczbe aby sprawdzic czy jest to PESEL"
    let pesel = Console.ReadLine()
    if (czyPesel pesel) then printfn "Liczba %s to PESEL" pesel else printfn "Liczba %s to nie PESEL" pesel

    printfn "\nZad 1.25"
    printfn "Wpisz tekst do zakodowania"
    let tekst = Console.ReadLine()
    printfn "Podaj kod szyfru (0 - 26)"
    let kod = Console.ReadLine()
    if int(kod) < int("0") && int(kod) > int("26") then printfn "Nieprawidlowy kod" else
        let KOD = int(kod)
        printfn "Zaszyfrowany tekst to"
        let zaszyfrowanyTekst = szyfruj tekst KOD
        printfn "%s" (zaszyfrowanyTekst)   

    printfn "\nZad 1.26"
    printfn "Wpisz zakodowany tekst"
    let tekst = Console.ReadLine()
    printfn "Podaj kod szyfru (0 - 26)"
    let kod = Console.ReadLine()
    if int(kod) < int("0") && int(kod) > int("26") then printfn "Nieprawidlowy kod" else
        let KOD = int(kod)
        printfn "Odszyfrowany tekst to"
        printfn "%s" (deszyfruj tekst KOD)   


    printfn "\nZad 1.27"
    printfn "Prosze podac minuty do zmienienia w godzinie"
    let czas = int(Console.ReadLine())    
    if czas > 1440 then
        printfn "Te minuty odpowiadaja następnej dobie"    
    else 
        let godziny = czas / 60
        let minuty = czas - godziny * 60
        printfn "%d minut odpowiada godzinie %02d:%02d" czas godziny minuty
   
    printfn "\nZad 1.28"
    printfn "Podaj ile minut do starty rakiety"
    let czas = int(Console.ReadLine())
    Funkcje.odliczanie czas 

    printfn "\nZad 1.29"
    printfn "Podaj pierwszy element ciagu Collatza"
    let wartosc = int(Console.ReadLine())
    Funkcje.Collatz wartosc 

    printfn "\nZad 1.30"
    printfn "Podaj wartosci do obliczenia sredniej:"
    let srednia = Funkcje.srednia () 
    printfn "Srednia z tych wartosci wynosi %.3f:" srednia

    0 