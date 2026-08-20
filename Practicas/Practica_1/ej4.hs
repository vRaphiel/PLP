paresDeNat :: [(Int, Int)]
paresDeNat = [(x, d - x) | d <- [0..], x <- [0..d]]

{--
Resuelto por diagonalizacion:
1. Definir una métrica global acotada (El generador infinito principal)
Creamos una variable (d) que se genera desde el infinito hacia adelante. 
Esta variable representa "el costo total" o "la cota superior" de la combinación actual:
d <- [0..]

2. Acotar los generadores intermedios por esa métrica
Todos los demás generadores ya **no** son infinitos. 
Van a estar obligados a correr dentro de un rango estrictamente acotado por la métrica global y 
las variables que ya elegiste ante.
x <- [0..d]

3. Calcular la última variable por diferencia (La resta)
En lugar de generar la última variable con un `<-`, la calculamos usando una ligadura `let` 
restando del total. Así garantizamos que la suma o la métrica se cumpla exactamente y que no 
haya espacio para búsquedas infinitas fallidas:
let y = d - x
--}