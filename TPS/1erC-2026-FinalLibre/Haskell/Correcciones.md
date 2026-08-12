5 - ¿Por qué usaron la notación infija para el operador de concatenación?
7 - ¿Por qué usaron recCueva en lugar de foldCueva? En ningún caso están usando la subestructura (en el caso del depósito nombran "y" a la variable pero nunca la leen así que podría llamarse "_" como en el resto de los casos).
- El primer argumento de recCueva podría ser directamente "Salida". Si hubieran usado foldCueva en lugar de recCueva, los argumentos 2 y 4 podrían ser directamente "Depósito" y "Bifurcación", respectivamente.
- Los nombres de las variables son confusos.
- Dado que ya la habían implementado en el ejercicio anterior, podrían haber reutilizado "esCompuerta" en lugar de usar un case.
8 - Tener dos case anidados dificulta mucho la lectura.
9 - No queda claro cuál es el papel del parámetro "k". Siempre lo pasan tal como viene (por lo que siempre vale la función que toma 2 parámetros y devuelve Nothing) y, en el único lugar que la usan (el primer caso) la usan para devolver Nothing.