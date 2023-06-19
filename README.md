# sudoku_solver

## Kasutamine:

1. dune build
2. dune exec sudoku_solver
3. programm küsib faili nime, kus on kirjas sudoku mängulaud
4. programm väljastab lahendatud mängulaua
5. kui mängulauda ei saa lahendada, siis väljastab programm veateate

## Faili formaat:
Mängulaud on failis kirjas 9x9 maatriksina, kus tühjad kohad on tähistatud märgiga "_" ning kõik ruudud on eraldatud tühikuga.

Näide failis example.txt:

```
_ 2 9 3 _ 8 4 5 6
5 7 8 2 _ 6 1 _ 9
_ _ _ 1 _ 5 _ 7 _
3 _ 5 _ 2 _ 6 _ _
_ _ _ _ _ 9 _ 4 _
_ 9 1 _ 6 7 _ _ _
_ 3 _ _ 5 _ _ _ _
_ _ _ _ _ 2 9 _ 3
9 _ 7 _ _ _ _ 2 4
``` 