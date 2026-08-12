# Ejercicio 8

## a)

```smalltalk
[:x | x + 1] value: 2.
```
Se puede ejecutar, el valor resultado de ejecutar el clousure es 3.

## b)

```smalltalk
[|x| x := 10. x + 12] value.
```
Se puede ejecutar, el valor resultado de ejecutar el clousure es 22.
El ```|x|``` es una variable dentro del clousure, no el colaborador.

## c)

```smalltalk
[:x :y | |z| z := x + y] value: 1 value: 2.
```
Se puede ejecutar, el valor resultado de ejecutar el clousure es 3.

## d)

```smalltalk
[:x :y | x + 1] value: 1.
```
No se puede ejecutar porque requiere el y. El clousure acepta dos argumentos, y solo se le esta pasando un mensaje value en vez de 2.

## e)

```smalltalk
[:x | [:y | x + 1]] value: 2.
```
No se va a ejecutar porque nunca se manda el mensaje value al clousure interno, a pesar de que el x si lo reciba.
Resultado:  [closure] in [] in UndefinedObject>>DoIt .
Usando algo como: 
[:x | [:y | x + 1] value: _] value: 2. donde _ es cualquier valor, por ejemplo el mismo x, este va a ejecutarse.

## f)

```smalltalk
[[:x | x + 1]] value
```
Se busca evaluar el clousure, el value lo ejecuta, pero el clousure dentro espera un argumento que no se le esta pasando.
Una solucion es:
[[:x | x + 1] value: _] value. Donde _ es cualquier Integer para X

## g)

```smalltalk
[:x :y :z | x + y + z] valueWithArguments: #(1 2 3).
```
Retorna 6, el valueWithArguments asigna cada valor en orden. x = 1, y = 2, z = 3.

## h)
```smalltalk
[ |z| z := 10. [:x | x + z]] value value: 10.
```
Resultado: 20.
El primer value ejecuta el clousure y devuelve como resultado el clousure interno con z definido como 10, que tiene el valor ```x``` como argumento. Este clousure, recibe ```value: 10``` y se ejecuta finalmente retornando el valor de 20.


