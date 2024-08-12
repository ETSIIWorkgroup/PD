# TRABAJO PROGRAMACIÓN DECLARATIVA - 2023/2024

Alumnos:

- ALEJANDRO FERNÁNDEZ TRIGO
- MARIO GARCÍA GONZALEZ

### Ejecución

1. main (llamada por el usuario mediante -> :l App.Main)
2. Seleccion de modo de juego:

MODO DE JUEGO 
         1 LIBRE 
         2 GUIADO 

3. Selección de dificultad:

SELECCIÓN DE DIFICULTAD
    1. FÁCIL
    2. MEDIO
    3. DIFÍCIL

x <- input

3. Una vez elegido se presenta el tablero:

X X X |8 X 7 |9 1 X |
9 X X |3 4 X |2 X X |
X X 5 |X X X |X 7 X |
------|------|------|
5 9 3 |7 X 2 |X 6 4 |
X X 1 |X X X |X 3 8 |
8 7 X |6 3 1 |X 9 2 |
------|------|------|
7 4 X |X X X |X 2 X |
X X X |X X 4 |3 X X |
X 5 2 |1 7 X |X X X |
------|------|------|

, junto al tablero siempre se presenta inmediatamente la petición de fila y columna.

4. Selección de fila y columna.

FILA: <- input 
COLUMNA: <- input

5. Opciones: se presentan 3 opciones para operar sobre la posición elegida.

    1. ESCRIBIR NÚMERO
    2. VER POSIBILIDADES
    3. OBTENER PISTA

- Si se pide escribir número:
    
    - Se pide el valor:
    VALOR: <- input

    - Se vuelve a presentar el tablero (punto nº 3) y se vuelve a iterar si el tablero no está lleno.

- Si se pide ver posibilidades:

    - Se presentan las posibilidades como lista:
    POSIBILIDADES: '8','9'
    
    - Se vuelve a presentar el tablero (punto nº 3) y se vuelve a iterar si el tablero no está lleno.

- Si se pide obtener pista:

    - Se vuelve a presentar el tablero (punto nº 3) pero la posición seleccionada ha sido automáticamente rellenada, y se vuelve a iterar si el tablero no está lleno.

6. Tras cada iteración, si el tablero no está completo se vuelve al punto nº 3, pero en 
caso de haber terminado se comprueba la solución: 

    - Si está bien resuelto, se muestra: "¡SUDOKU COMPLETADO!" y el juego termina.

    - Si no está bien resuelto, se muestra: "¡VUELVE A INTENTARLO!" y el juego arranca desde el punto nº 2.

Adicional: Si se elige el modo GUIADO se validan los valores introducidos en cada iteración. 