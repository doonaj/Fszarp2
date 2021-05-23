module Funkcje

open System

// zad 1.28
let rec odliczanie (czas:int) =
    if czas < 0 then 
        printfn "Nieprawidlawa wartosc"
    elif czas = 0 then 
        printfn "START" 
    else 
        printfn "Do startu pozostalo %d minut" czas
        odliczanie (czas - 1)

// zad 1.29
let rec Collatz liczba =
    let rec collatz liczba i =
        if liczba < 1 then printfn "Nieprawidlawa wartosc"
        elif liczba > (2147483647 - 1) / 3 then printfn "Obliczenia niemozliwe z powodu ograniczen typu int"
        elif liczba = 1 then printfn "%d. %d\n" i liczba
        elif liczba % 2 = 0 then 
            printf "%d. %d\n" i liczba
            collatz (int(liczba/2)) (i + 1)
        else 
            printf "%d. %d\n" i liczba
            collatz (3 * liczba + 1) (i + 1)
    collatz liczba 0

// zad 1.30
let rec srednia () =   
    let rec Srednia n suma srednia =
        let liczba = int(Console.ReadLine())
        if liczba < 0 then srednia
        else
            Srednia (n + 1) (suma + liczba) (float(suma)/float(n))
    Srednia 1 0 0.0

 //Funkcja pomocnicza
let rec silnia n =
    let rec sil n acc =
        if n = 0I then
            acc
        else
            sil (n-1I) (n*acc)
    sil n 1I