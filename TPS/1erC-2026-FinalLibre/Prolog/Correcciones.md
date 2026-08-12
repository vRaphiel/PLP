En general:
- Faltan los patrones de instanciación de (casi) todos los predicados auxiliares.
1 - No se entiende el rol que juega el parámetro "Salida".
3 - ¿Cuál sería la diferencia entre los predicados 'valuación' y 'listaDeValuaciones'? El primero se define a partir del segundo, pasándole los mismos parámetros en el mismo orden así que sería equivalente invocar a cualquiera de los dos.
4 - Misma corrección que en el ejercicio anterior para 'esVálida' y 'esFormulaValida'.
- El comentario que agregaron muestra que no están cumpliendo el enunciado. No están asegurando que todas las variables de la fórmula tengan un valor de verdad asignado en la valuación. La consulta "esVálida(pitufo & ~vuela, [(pitufo,t)])" debería dar falso pero da verdadero.
7 - subvaluación no funciona si las variables aparecen en otro orden. La consulta "subvaluación([(alfajor,t),(pitufo,f)],[(pitufo,f),(alfajor,t)])" debería dar verdadero pero da falso.
- El nombre del predicado avanzarHastaPar es demasiado imperativo.
8 - ¿Qué significa "libre"? ¿Que no está instanciada?
- Sólo había que analizar reversibilidad en el primer argumento. No era necesario analizar qué sucedía si el segundo era una variable no instanciada. La primera oración responde correctamente pero no justifica.
9 - No queda claro cuál es el propósito del predicado valuaciónConjuntas. Pareciera que lo que hace es simplemente extraer el código repetido de 'consecuencia' y 'contradictorias' pero sin ninguna cohesión.
- Anidar nots dificulta mucho entender el código. Como regla general, si usan un not, que lo de adentro sea un único predicado auxiliar.
- El código de 'modeloÚnico' funciona por como implementaron 'variables' y 'valuación' pero técnicamente no es correcto porque comparan las valuaciones con el operador '\='. Como las valuaciones son listas, si las variables estuvieran en otro en orden serían consideradas distintas a pesar de estar representando al mismo conjunto de pares.