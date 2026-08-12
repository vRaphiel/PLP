Examen Final Paradigmas de Programacién 27 de febrero de 2026

Ejercicio 1 (Programacién funcional). El tipo algebraico ABBComp a describe a los arboles binarios de busqueda (ABB) parcialmente comprimidos con elementos de tipo a y sin repetidos. Los ABBComp son arboles que pueden tener algun subarbol donde, en vez de seguir con una estructura arbórea, sus elementos se incluyen directamente en una lista ordenada (se asume que a pertenece a la clase Ord). 

data ABBComp a = Nil | Comp [a] | Nodo a (ABBComp a) (ABBComp a) 

Por ejemplo, las siguientes expresiones denotan drboles de tipo ABBComp Int que denotan los mismos elementos:
Comp [0,2,14,22]
Nodo 2 (Nodo 0 Nil Nil) (Comp [14,22])
Nodo 14 (Nodo 2 (Nodo 0 Nil Nil) Nil) (Nodo 22 Nil Nil) 

a) Dar el tipo y definir una funcién foldABBComp, que abstraiga el esquema de recursión estructural sobre los ABB parcialmente comprimidos. 

b) Sin usar recursión explicita, dar el tipo y definir una funcién mapABBComp que aplique una funcién dada a todos los valores de un arbol del tipo indicado anteriormente. 

c) Dar el tipo y definir recABBComp que abstraiga el esquema de recursión primitiva sobre estos arboles. 

d) Sin usar recursión explicita, dar el tipo y definir la funcién ordenado, que permitirá verificar el invariante de la estructura (i.e., que los elementos de un ABBComp estén ordenados). 

e) Dar el tipo y definir la funcién iguales(ai,a2) , que recibe dos ABBComp y devuelve True si son iguales al recorrerlos inorder. Se deberá usar alguno de los esquemas de recursión anteriores y justificar por qué se lo usé. 

Ejercicio 2 (Calculo-A). Se extiende el calculo-\ para permitir una maratón de términos del mismo tipo, a partir del agregado de la extensión ganador [M1, ..., Mn] (con n > 1). Así, el resultado de su evaluacién será la del subtérmino cuya evaluacidn finaliza en primer lugar, yendo en rondas donde se recorren los subtérminos de izquierda a derecha (i.e., en cada ronda se evaltia un paso de cada subtérmino, si ya no es un valor, hasta recorrer todos ellos). 
    Por ejemplo, ganador [if (Az : bool.x) false then true else false, true, (Az : bool.x) false] —» true. 

a) Extender formalmente la gramdtica de los términos y el sistema de tipos. 

b) Extender la gramatica de los valores, en caso de ser necesario, y las reglas de semantica operacional small-step. Tomar una decisión sobre lo que sucede cuando hay varios subtérminos que finalizan en la misma ronda. De ser necesario, se pueden considerar definiciones adicionales para poder tener control de en qué paso de la evaluacion se encuentra cada subtérmino. Justificar adecuadamente. 

c) Explicar detalladamente qué se modificarfa en los puntos anteriores para la extensión perdedor [M1,..., Mn], que es igual a la anterior, pero donde el resultado de la evaluacién es la del ultimo subtérmino que finaliza su evaluacidn. 

Ejercicio 3 (Programación Lógica). A las listas comunes de nimeros enteros (e.g., [8,1,1,1, 10,10,2)) se le quiere sumar un nuevo tipo de elemento que son los comprimidos (anotados como c(N,K), donde N es el mimero y K es la cantidad de repeticiones, con K > 1). Asi, podremos generar listas comprimidas, donde se puedan agrupar los elementos contiguos repetidos, indicando la cantidad de valores iguales por medio un par de valores (obs.: si se comprime ciertos elementos contiguos, se deben incluir todos ellos, por lo que, por ejemplo, [8,c(1,2) ,1,10,10,2] no sera una compresion valida para el caso anterior). No es obligatorio comprimir toda la lista para que esta sea comprimida. Sd6lo por nombrar algunas, la lista anterior podrfa tener distintas versiones comprimidas: [c(8,1),c(1,3),10,10,2), [8,c(1,3),c(10,2),2], [c(8,1) ,c(1,3) ,c(10,2) ,c(2,1)], y 18.01,1,30/10.9)1: 

a) Definir un predicado comprimido(+L1, -L2) que dada una lista L1, L2 es una versién comprimida de L1. No deben generarse soluciones L2 idénticas más de una vez.

b) ¿El predicado comprimido puede ser reversible en uno o ambos argumentos? Justificar detalladamente. 

c) Definir un predicado iguales(+L1, +L2), que indique si dos listas son observacionalmente iguales, considerando que ambas denotan la misma lista de nimeros enteros. Se pueden agregar predicados auxiliares.


Examen Final Paradigmas de Programacién 6 de marzo de 2026

Ejercicio 1 (Cálculo-\). Sean M,N dos programas (es decir, términos cerrados y tipables) que ademas tienenel mismo tipo, Decimos que M es distingible de N si existe una lista infinita de valores V1...Vn con n >= 0 tales que MBVV1...Vn -> *Va y NV1...Vn -> *Vb con Va != Vb

a) Demostrar que el programa Af : Bool > Bool. f (f true) es distinguible del programa \f : Bool —+ Bool. 

b) Decimos que M. Exhibir que M doses indistinguible programasMde N y N distintos,si no vale talesque Mquees distinguibleM seaindistinguible de N. Es facilde N.de ver queM es indistinguiblef true. 

c) distinguibleDecidir si ladesiguienteM. afirmacines verdadera o falsa yAejustificar: si M es distinguible"(one de N, entonces N es | , 

d) siDecidirM es si distinguiblela siguientedeafirmaciénN, con M,Nes verdadera: T,entonceso falsa Af y :T justificar:+Bool. f M es distinguibleae J rou de Af : T - Bool. fN, ia 

Ejercicio 2 (Programacion Logica y Resolucion).
Se cuenta con la siguiente base de conocimientos sobre relaciones familiares:
progenitor (diego,dalma).       progenitor (chitoro, ana).  pareja(chitoro,tota).
progenitor (diego, gianinna).   progenitor(ana,daniel).     pareja(diego,claudia).
progenitor(tota,diego).         pareja(gianinna,osvaldo).   pareja(ana,pedro).

pareja(X,Y) :- pareja(Y,X).
ancestro(A, X) :- progenitor(A,X). 
ancestro(Y,X) :- progenitor(A, Y), ancestro(Y, X).
Donde progenitor(P, H) indica que P es progenitor de H, pareja (X, Y) indica que X e Y estan en pareja, y ancestro(A, X) indica que A es un ancestro (directo o indirecto) de X.


a) Definir un predicado descendientes(+P, -L), que instancie en L la lista de todos los descendientes de P, sin repetidos.Obs.: No se exige ningún orden particular en la lista resultante.

b) Analizar y justificar detalladamente si uno o ambos argumentos del predicado anterior pueden ser reversibles.

c) Definir un predicado ancestroComunMasCercano(+P1, +P2, -A), que se satisfaga cuando A es el ancestro común más cercano entre P1 y P2. Si hay más de uno, debería dar todos ellos.Por ejemplo: Si dos personas son hermanas, sus ancestros comunes más cercanos serían sus progenitores.

d) Comparar qué pasa en Prolog y en Resolución General ante una fórmula como la de abajo, y explicar en detalle por qué podrían haber diferencias:
∃A.(ancestro(A,daniel)∧ancestro(A,gianinna))


Ejercicio 3: Programación funcional

El tipo algebraico AdjAB a describe a las adjunciones de árboles binarios con elementos de tipo a. Las AdjAB representan listas de árboles binarios, donde cada uno de ellos está al mismo nivel que el otro, que pueden o no tener una raíz en común de acuerdo al constructor usado.

data AdjAB a = Raiz a [AB a] | Adj [AB a]
data AB a = Nil | Hoja a | Arb a (AB a) (AB a)

Por ejemplo, las siguientes expresiones denotan distintos árboles de tipo AdjAB Int:
- Adj [Nil, Nil, Arb 3 Nil Nil, Hoja 6]
- Raiz 5 [Arb 3 (Arb 1 Nil Nil) (Hoja 4), Arb 3 Nil Nil, Nil]
- Raiz 10 []

a) Recursión Estructural (foldAdjAB)

Dar el tipo y definir una función foldAdjAB, que abstraiga el esquema de recursión estructural sobre las adjunciones de árboles binarios, recorriendo todos los árboles por completo.

b) Sin usar recursión explícita, dar el tipo y definir una función mapAdjAB que aplique una función dada a todos los valores de una adjunción.

c) Dar el tipo y definir recAdjAB que abstraiga el esquema de recursión primitiva sobre las adjunciones, recorriendo todos los árboles por completo.

d) Sin usar recursión explícita, dar el tipo y definir la función ordenado, que permita saber si toda la adjunción está ordenada. Así, cada árbol debe estar ordenado como un ABB y, adicionalmente, si hubiera raíz común, esta debe ser mayor que la raíz de todos los árboles. En el caso de Nil, este árbol está trivialmente ordenado y, además, se lo puede ignorar para comparar con la raíz común.

Examen Final Paradigmas de Programacién 29 de mayo de 2026

Ejercicio 1: Programación Funcional

El tipo algebraico Trie a describe los árboles de tipo trie (del inglés reTRIEval), data Trie a = TrieNodo (Maybe a) [(Char, Trie a)].
El camino desde la raíz hasta un nodo determina una cadena. Si el nodo contiene un valor de la forma Just v, entonces esa cadena representa una palabra del conjunto o una clave definida del diccionario. Si contiene Nothing, entonces la cadena no está definida como palabra o clave, aunque puede ser prefijo de otras cadenas.

Decimos que la representación es naïve, porque la lista de hijos puede contener más de una aparición del mismo carácter. Por eso, una misma cadena puede estar representada por más de un camino dentro del trie.

Por ejemplo:
Haskell

t :: Trie Bool
t =
  TrieNodo Nothing
    [ ('a', TrieNodo (Just True) [])
    , ('b', TrieNodo Nothing
        [ ('a', TrieNodo (Just True)
            [ ('d', TrieNodo Nothing []) ])
        ])
    , ('c', TrieNodo (Just True) [])
    ]

a) Dar el tipo y definir una función foldTrie, que abstraiga el esquema de recursión estructural sobre los tries.

b) Sin usar recursión explícita, definir la función palabras :: Trie a -> [[Char]] que, dado un trie, devuelva todas las palabras incluidas en él. Una palabra está incluida si y solo si el nodo alcanzado por ese camino desde la raíz contiene un valor de la forma Just _, y esto incluye a la raíz, lo que implica que se incluye a la palabra vacía. No se deben incluir palabras repetidas.

Por ejemplo, dado el trie anterior:
palabras t ~> ["a", "ba", "c"]

c) Sin usar recursión explícita, dar el tipo y definir una función mapTrie que aplique una función dada a todos los valores de un trie.

d) Se quiere definir una función conLongitud :: Trie a -> Trie (a, Int) que asocie a cada valor almacenado la longitud de la palabra correspondiente. ¿Puede definirse usando únicamente mapTrie? Justificar. En caso negativo, definir una función más general que permita implementarla.

Ejercicio 2: Programación Lógica y Resolución

Una estructura arbórea es o bien el símbolo o o bien una lista de estructuras arbóreas. Por ejemplo, las siguientes son estructuras arbóreas válidas:
- o
- []
- [o, o]
- [[o, o], o, [o, o]]
- [o, [o, o, [o, []], o], [o, []]]


a) Definir en Prolog un predicado estructuraArborea(-E) que genere todas las posibles estructuras arbóreas.

b) Pasar la base de conocimientos a forma clausal.

c) Dar un ejemplo de consulta que, bajo la estrategia usual de Prolog, no finalice en alguna situación.

d) Mostrar que, usando Resolución General, puede responderse la consulta anterior. Explicar la diferencia con Prolog.

Ejercicio 3: Cálculo-$\lambda$ Se extiende el cálculo-$\lambda$ con un constructor de términos $M ::= \dots \mid M \bowtie N$. No se extienden los conjuntos de tipos ni el conjunto de valores. Se agregan las siguientes reglas de tipado y reducción:

$$\frac{\Gamma \vdash M : \tau \quad \Gamma \vdash N : \tau}{\Gamma \vdash M \bowtie N : \tau} \text{T-}\bowtie \qquad \frac{M \rightarrow M'}{M \bowtie N \rightarrow M' \bowtie N} \text{E-}\bowtie_1 \qquad \frac{M \rightarrow M'}{V \bowtie M \rightarrow V \bowtie M'} \text{E-}\bowtie_2$$$$\frac{}{V_1 \bowtie V_2 \rightarrow V_1} \text{E-SELECT}_1 \qquad \frac{}{V_1 \bowtie V_2 \rightarrow V_2} \text{E-SELECT}_2$$

Observar que se pierde el determinismo, ya que por ejemplo $\text{true} \bowtie \text{false} \rightarrow \text{true}$ y $\text{true} \bowtie \text{false} \rightarrow \text{false}$.

a)Decidir si es verdadera o falsa y justificar: existe un valor $V$ tal que $V~\text{true} \rightarrow^* \text{true}$ y también $V~\text{true} \rightarrow^* \text{false}$.

b) Si $M$ es un término cerrado y tipable, definimos $\mathcal{E}(M)$ como el conjunto de los valores $V$ tales que $M \rightarrow^* V$. Por ejemplo, $\mathcal{E}(\text{true} \bowtie (\text{true} \bowtie \text{false})) = \{\text{true}, \text{false}\}$. Suponiendo que $\mathcal{E}(M)$ tiene un elemento y $M : \text{bool}$, definir $M'$ tal que $\mathcal{E}(M')$ tenga dos elementos.

c) Decidir si es verdadera o falsa y justificar: existe un término cerrado y tipable $M$ tal que $\mathcal{E}(M~(M~(\text{true} \bowtie \text{false})))$ es un conjunto de 8 elementos.