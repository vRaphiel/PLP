%! completar(+S)
% Indica que se debe completar el predicado. Siempre falla.
completar(S) :- write("COMPLETAR: "), write(S), nl, fail.

%--------------- Vocabulario ---------------------------------------------------

sustantivoMasc(alfajor).
sustantivoMasc(iglú).
sustantivoMasc(pitufo).

sustantivoFem(persona).
sustantivoFem(gaviota).
sustantivoFem(mermelada).

determinanteFem(una).
determinanteMasc(un).

pronombrePersonal(alonso).
pronombrePersonal(matilde).
pronombrePersonal(charlyGarcía).
pronombrePersonal(doctorDoom).

verboTrans(hechiza).
verboTrans(molesta).
verboTrans(admira).

preposición(a).

verboIntrans(vuela).
verboIntrans(estudia).
verboIntrans(come).

negación(no).
negación(niAhí).

condicional(si).
condicional(cuando).

consecuente(entonces).
consecuente(luego).

%----------- Operadores (para hacer mas legible una formula proposicional) -----
:- op(900, yfx, >).         % implicación
:- op(800, yfx, &).         % conjunción
:- op(750,  fy, ~).         % negación

%------------ Ejercicio 1 ------------------------------------------------------

% parsea(-Frase,-Semántica).
parsea(Frase, Semántica) :- frase(Semántica, Frase, []).

% Frase → Condicional Oración Consecuente Oración
frase(Semantica1 > Semantica2, [Condicion | XS], Salida) :-
    condicional(Condicion),
    oracion(Semantica1, XS, [Consecuente | XS2]),
    consecuente(Consecuente),
    oracion(Semantica2, XS2, Salida).

% Frase → Oración
frase(Sem, Entrada, Salida) :- oracion(Sem, Entrada, Salida).

% Oración → Sujeto Predicado
oracion(SemanticaSujeto & SemanticaPredicado, Entrada, Salida) :-
    sujeto(SemanticaSujeto, Entrada, Resto),
    predicado(SemanticaPredicado, Resto, Salida).

% Sujeto → DeterminanteFem SustantivoFem
sujeto(SustantivoFem, [DeterminanteFem, SustantivoFem | Salida], Salida) :-
    determinanteFem(DeterminanteFem),
    sustantivoFem(SustantivoFem).

% Sujeto → DeterminanteMasc SustantivoMasc
sujeto(SustantivoMasc, [DeterminanteMasc, SustantivoMasc | Salida], Salida) :-
    determinanteMasc(DeterminanteMasc),
    sustantivoMasc(SustantivoMasc).

% Sujeto → PronombrePersonal
sujeto(PronombrePersonal, [PronombrePersonal | Salida], Salida) :-
    pronombrePersonal(PronombrePersonal).

% Predicado → VerboTrans Preposición Sujeto
predicado(VerboTrans & SemanticaSujeto, [VerboTrans, Preposición | Resto1], Salida) :-
    verboTrans(VerboTrans),
    preposición(Preposición),
    sujeto(SemanticaSujeto, Resto1, Salida).

% Predicado → Negación VerboTrans Preposición Sujeto
predicado(~(VerboTrans & SemanticaSujeto), [Neg, VerboTrans, Preposición | Resto1], Salida) :-
    negación(Neg),
    verboTrans(VerboTrans),
    preposición(Preposición),
    sujeto(SemanticaSujeto, Resto1, Salida).

% Predicado → VerboIntrans
predicado(VerboIntrans, [VerboIntrans | Salida], Salida) :-
    verboIntrans(VerboIntrans).

% Predicado → Negación VerboIntrans
predicado(~VerboIntrans, [Negación, VerboIntrans | Salida], Salida) :-
    negación(Negación),
    verboIntrans(VerboIntrans).

%------------ Ejercicio 2 ------------------------------------------------------

% variables(+Fórmula,-Variables)
% variables(Fórmula, Variables) :- esListaDeVariables(Fórmula, VariablesRepetidos), sort(VariablesRepetidos, Variables).
% Podemos usar sort para ordenar la lista y quitar repetidos. Desconocia si se puede 

variables(Fórmula, Variables) :- esListaDeVariables(Fórmula, VariablesRepetidos), sinRepetidos(VariablesRepetidos, Variables).

esListaDeVariables(X, [X]) :- atom(X).
esListaDeVariables(~F, Vars) :- esListaDeVariables(F, Vars).
esListaDeVariables(Sem1 & Sem2, Vars) :- esListaDeVariables(Sem1, Vars1), esListaDeVariables(Sem2, Vars2), append(Vars1, Vars2, Vars).
esListaDeVariables(Sem1 > Sem2, Vars) :- esListaDeVariables(Sem1, Vars1), esListaDeVariables(Sem2, Vars2), append(Vars1, Vars2, Vars).

sinRepetidos([], []).
sinRepetidos([H | XS], L) :- member(H, XS), sinRepetidos(XS, L).
sinRepetidos([H | XS], [H | RL]) :- not(member(H, XS)), sinRepetidos(XS, RL).

%------------ Ejercicio 3 ------------------------------------------------------

% valuación(+Variables, -Valuación)
valuación(Variables, Valuación) :- listaDeValuaciones(Variables, Valuación).

listaDeValuaciones([], []).
listaDeValuaciones([X | XS], [(X, t) | XT]) :- listaDeValuaciones(XS, XT).
listaDeValuaciones([X | XS], [(X, f) | XT]) :- listaDeValuaciones(XS, XT).

%------------ Ejercicio 4 ------------------------------------------------------

% Si una variable de la fórmula no aparece en Valuación, member/2 falla y la tratamos como falsa.
% esVálida(+Fórmula, +Valuación)
esVálida(Fórmula, Valuación) :- esFormulaValida(Fórmula, Valuación).

esFormulaValida(Variable, Valuación) :- 
    atom(Variable),
    member((Variable, t), Valuación).

esFormulaValida(~F, Valuación) :- not(esFormulaValida(F, Valuación)).

esFormulaValida(F1 & F2, Valuación) :- 
    esFormulaValida(F1, Valuación),
    esFormulaValida(F2, Valuación).

esFormulaValida(F1 > _, Valuación) :- 
    not(esFormulaValida(F1, Valuación)).

esFormulaValida(F1 > F2, Valuación) :- 
    esFormulaValida(F1, Valuación),
    esFormulaValida(F2, Valuación).

%------------ Ejercicio 5 ------------------------------------------------------

% modelo(+Fórmula, -Valuación)
modelo(Fórmula, Valuación) :- 
    variables(Fórmula, Variables),
    valuación(Variables, Valuación),
    esVálida(Fórmula, Valuación).

%------------ Ejercicio 6 ------------------------------------------------------

% mejorModelo(+Fórmula, -Valuación)
mejorModelo(Fórmula, Valuación) :- 
    modelo(Fórmula, Valuación),
    cantidadDeVariablesVerdaderas(Valuación, Cantidad),
    not((modelo(Fórmula, Valuación2),
        cantidadDeVariablesVerdaderas(Valuación2, Cantidad2),
        Cantidad2 > Cantidad)).

% cantidadDeVariablesVerdaderas(+Modelo, -Cantidad)
cantidadDeVariablesVerdaderas([], 0).
cantidadDeVariablesVerdaderas([(_, t) | XS], C) :- 
    cantidadDeVariablesVerdaderas(XS, SubC),
    C is SubC + 1.
cantidadDeVariablesVerdaderas([(_, f) | XS], C) :- 
    cantidadDeVariablesVerdaderas(XS, C).

%------------ Ejercicio 7 ------------------------------------------------------

% subvaluación(+Valuación1, +Valuación2)
subvaluación([], _).
subvaluación([Par | Sub], Valuación) :-
    avanzarHastaPar(Par, Valuación, Resto),
    subvaluación(Sub, Resto).

% Avanza en Valuación hasta encontrar Par, que lo usamos para tener un orden.
avanzarHastaPar(Par, [Par | Resto], Resto).
avanzarHastaPar(Par, [_ | RestoVal], Resto) :-
    avanzarHastaPar(Par, RestoVal, Resto).

% Iniciamos con un modelo de la fórmula y luego verificamos la subvaluación, de esta forma,
% no generamos valuaciones aleatorias cuando Valuación2 es libre.
% extiende(+Valuación1, +Fórmula, -Valuación2)
extiende(Valuación1, Fórmula, Valuación2) :- modelo(Fórmula, Valuación2), subvaluación(Valuación1, Valuación2).

%------------ Ejercicio 8 ------------------------------------------------------
% subvaluación/2 es reversible respecto del primer argumento. Cuando Valuación2 está instanciada: 
%    subvaluacion(-Valuación1, +Valuación2) devuelve todas sus subvaluaciones (subsecuencias de pares, sin repetidos).
% En cambio, si Valuación2 es libre no lo planteamos reversible, ya que habría demasiadas (o infinitas) valuaciones posibles. 
% En este caso podemos acotar el dominio (Ej.: solo variables de una fórmula) o separar un predicado generador de otro que
% solo verifica, como hicimos en extiende/3.
% Ej.: ?- subvaluación(V, [(a, t), (b, f)]).
%      V = [] ; V = [(a, t)] ; V = [(b, f)] ; V = [(a, t), (b, f)].

%------------ Ejercicio 9 ------------------------------------------------------

% Auxiliar: Validamos sobre la unión de variables de ambas fórmulas, con Fórmula1 válida. 
% Nos dimos cuenta que consecuencia y contradictorias al momento de resolverlas, se repetia la logica. 
% Por lo tanto, creamos este auxiliar.
% validaciónConjuntas(+Fórmula1, +Fórmula2, -Valuación)
valuaciónConjuntas(Fórmula1, Fórmula2, Valuación) :-
    variables(Fórmula1, Vars1),
    variables(Fórmula2, Vars2),
    union(Vars1, Vars2, Vars),
    valuación(Vars, Valuación),
    esVálida(Fórmula1, Valuación).

% consecuencia(+Fórmula1, +Fórmula2)
consecuencia(Fórmula1, Fórmula2) :- not((valuaciónConjuntas(Fórmula1, Fórmula2, V), not(esVálida(Fórmula2, V)))).

% equivalentes(+Formula1, +Fórmula2)
equivalentes(Fórmula1, Fórmula2) :-
    consecuencia(Fórmula1, Fórmula2),
    consecuencia(Fórmula2, Fórmula1).

% contradictorias(+Fórmula1, +Fórmula2)
contradictorias(Fórmula1, Fórmula2) :- not((valuaciónConjuntas(Fórmula1, Fórmula2, V), esVálida(Fórmula2, V))).

% modeloÚnico(+Fórmula, -Valuación)
modeloÚnico(Fórmula, Valuación) :-
    modelo(Fórmula, Valuación),
    not((modelo(Fórmula, V2), V2 \= Valuación)).

%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

% Funciones auxiliares para armar y validar resultados de findall
listasIguales(L1, L2) :- msort(L1, M), msort(L2, M).
estaEnLosResultados(Resultados, Esperado) :- member(R, Resultados), listasIguales(R, Esperado).
tieneEstosResultados(Resultados, Esperados) :- maplist(estaEnLosResultados(Resultados), Esperados).

sinDuplicados(L) :- length(L, N), list_to_set(L, S), length(S, N).

% Se espera que completen con las subsecciones de tests que crean necesarias, más allá de las puestas en estos ejemplos

cantidadTestsParsea(19).
testParsea(1) :- parsea([si, una, persona, hechiza, a, una, gaviota, entonces, un, alfajor, hechiza, a, una, mermelada], persona & (hechiza & gaviota) > alfajor & (hechiza & mermelada)).
testParsea(2) :- parsea([si, una, persona, hechiza, a, una, gaviota, entonces, un, alfajor, hechiza, a, un, iglú], persona & (hechiza & gaviota) > alfajor & (hechiza & iglú)).
testParsea(3) :- parsea([si, una, persona, hechiza, a, una, gaviota, entonces, un, alfajor, hechiza, a, un, pitufo], persona & (hechiza & gaviota) > alfajor & (hechiza & pitufo)).
testParsea(4) :- parsea([matilde, no, hechiza, a, una, gaviota], matilde & ~(hechiza & gaviota)).
testParsea(5) :- parsea([un, pitufo, come], pitufo & come).
testParsea(6) :- parsea([una, persona, niAhí, admira, a, alonso], persona & ~(admira & alonso)).
testParsea(7) :- parsea([cuando, doctorDoom, no, estudia, luego, matilde, vuela], (doctorDoom & ~estudia) > (matilde & vuela)).
testParsea(8) :- parsea([si, un, iglú, niAhí, vuela, entonces, una, mermelada, molesta, a, charlyGarcía], (iglú & ~vuela) > (mermelada & (molesta & charlyGarcía))).
testParsea(9) :- not(parsea([el, pitufo, come], _)). % "el" no esta en la gramatica
testParsea(10) :- not(parsea([un, pitufo, a, come], _)). % mal uso preposicion
testParsea(11) :- parsea([charlyGarcía, come], charlyGarcía & come).
testParsea(12) :- parsea([charlyGarcía, estudia], charlyGarcía & estudia). % tabla del enunciado
testParsea(13) :- parsea([un, pitufo, no, vuela], pitufo & ~vuela).
testParsea(14) :- parsea([alonso, no, hechiza, a, una, gaviota], alonso & ~(hechiza & gaviota)).
testParsea(15) :- parsea([si, una, mermelada, vuela, entonces, matilde, no, come], (mermelada & vuela) > (matilde & ~come)).
testParsea(16) :- parsea([cuando, doctorDoom, no, vuela, entonces, un, pitufo, molesta, a, charlyGarcía], (doctorDoom & ~vuela) > (pitufo & (molesta & charlyGarcía))).
testParsea(17) :- parsea([un, iglú, no, molesta, a, matilde], iglú & ~(molesta & matilde)).
testParsea(18) :- not(parsea([], _)). % frase vacia
testParsea(19) :- not(parsea([un, pitufo, come, extra], _)). % sobra una palabra al final

cantidadTestsVariables(9).
testVariables(1) :- findall(V, variables(persona & (hechiza & gaviota) > alfajor & (hechiza & mermelada), V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[persona, hechiza, gaviota, alfajor, mermelada]]).
testVariables(2) :- findall(V, variables(~a & (b > (~a & c)), V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[a, b, c]]).
testVariables(3) :- findall(V, variables(pitufo, V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[pitufo]]).
testVariables(4) :- findall(V, variables(a > a, V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[a]]).
testVariables(5) :- findall(V, variables(a & (b & (c & (d & e))), V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[a, b, c, d, e]]).
testVariables(6) :- findall(V, variables(~(~a), V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[a]]).
testVariables(7) :- findall(V, variables(a & a, V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[a]]). % a repetida
testVariables(8) :- findall(V, variables((a & b) & a, V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[a, b]]).
testVariables(9) :- findall(V, variables(~(a & b) > (b & a), V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[a, b]]).

cantidadTestsValuación(6).
testValuación(1) :- findall(V, valuación([mermelada, gaviota], V), Vs), length(Vs, 4), tieneEstosResultados(Vs, [[(mermelada, t), (gaviota, t)], [(mermelada, t), (gaviota, f)], [(mermelada, f), (gaviota, t)], [(mermelada, f), (gaviota, f)]]).
testValuación(2) :- findall(V, valuación([a], V), Vs), length(Vs, 2), tieneEstosResultados(Vs, [[(a, t)], [(a, f)]]).
testValuación(3) :- findall(V, valuación([], V), Vs), length(Vs, 1), tieneEstosResultados(Vs, [[]]).
testValuación(4) :- findall(V, valuación([a, b, c], V), Vs), length(Vs, 8), tieneEstosResultados(Vs, [[(a, t), (b, t), (c, t)], [(a, t), (b, t), (c, f)], [(a, t), (b, f), (c, t)], [(a, t), (b, f), (c, f)], [(a, f), (b, t), (c, t)], [(a, f), (b, t), (c, f)], [(a, f), (b, f), (c, t)], [(a, f), (b, f), (c, f)]]).
testValuación(5) :- findall(V, valuación([a, b], V), Vs), sinDuplicados(Vs), length(Vs, 4). % no queremos repetidos al pedir ;
testValuación(6) :- findall(V, valuación([x], V), Vs), length(Vs, 2), tieneEstosResultados(Vs, [[(x, t)], [(x, f)]]).

cantidadTestsEsVálida(10).
testEsVálida(1) :- not(esVálida(persona & (hechiza & gaviota), [(persona, f), (hechiza, t), (gaviota, f)])).
testEsVálida(2) :- esVálida(gaviota, [(gaviota, t)]).
testEsVálida(3) :- esVálida(a > b, [(a, f), (b, f)]).
testEsVálida(4) :- esVálida(a > b, [(a, t), (b, t)]).
testEsVálida(5) :- not(esVálida(a > b, [(a, t), (b, f)])).
testEsVálida(6) :- esVálida(~(~a), [(a, t)]).
testEsVálida(7) :- esVálida(a > a, [(a, f)]).
testEsVálida(8) :- not(esVálida(a & b, [(a, t)])). % falta b en la valuacion
testEsVálida(9) :- not(esVálida(a & b, [(a, t), (b, f), (c, t)])). % c de mas
testEsVálida(10) :- esVálida(a, [(a, t), (b, f)]). % b de mas pero no molesta

cantidadTestsModelo(6).
testModelo(1) :- findall(M, modelo(gaviota > mermelada, M), Ms), length(Ms, 3), tieneEstosResultados(Ms, [[(gaviota, t), (mermelada, t)], [(gaviota, f), (mermelada, t)], [(gaviota, f), (mermelada, f)]]).
testModelo(2) :- findall(M, modelo(~a & a, M), Ms), length(Ms, 0).
testModelo(3) :- findall(M, modelo(a > a, M), Ms), length(Ms, 2), tieneEstosResultados(Ms, [[(a, t)], [(a, f)]]).
testModelo(4) :- findall(M, modelo(~(a & b), M), Ms), length(Ms, 3), tieneEstosResultados(Ms, [[(a, t), (b, f)], [(a, f), (b, t)], [(a, f), (b, f)]]).
testModelo(5) :- findall(M, modelo(gaviota > mermelada, M), Ms), sinDuplicados(Ms), length(Ms, 3).
testModelo(6) :- findall(M, modelo(a & b, M), Ms), length(Ms, 1), tieneEstosResultados(Ms, [[(a, t), (b, t)]]). % unico modelo

cantidadTestsMejorModelo(6).
testMejorModelo(1) :- findall(M, mejorModelo(~a > a, M), Ms), length(Ms, 1), tieneEstosResultados(Ms, [[(a, t)]]).
testMejorModelo(2) :- findall(M, mejorModelo(~(~a & b) > (a & ~b), M), Ms), length(Ms, 2), tieneEstosResultados(Ms, [[(a, t), (b, f)], [(a, f), (b, t)]]).
testMejorModelo(3) :- findall(M, mejorModelo(a & b, M), Ms), length(Ms, 1), tieneEstosResultados(Ms, [[(a, t), (b, t)]]).
testMejorModelo(4) :- findall(M, mejorModelo(~(a & b), M), Ms), length(Ms, 2), tieneEstosResultados(Ms, [[(a, t), (b, f)], [(a, f), (b, t)]]).
testMejorModelo(5) :- findall(M, mejorModelo(~a & ~b, M), Ms), length(Ms, 1), tieneEstosResultados(Ms, [[(a, f), (b, f)]]).
testMejorModelo(6) :- findall(M, mejorModelo(~a > a, M), Ms), sinDuplicados(Ms), length(Ms, 1).

cantidadTestsSubvaluación(9).
testSubvaluación(1) :- subvaluación([(gaviota, t)], [(gaviota, t), (mermelada, f)]).
testSubvaluación(2) :- not(subvaluación([(gaviota, t)], [(gaviota, f), (mermelada, f)])).
testSubvaluación(3) :- subvaluación([], [(a, t)]).
testSubvaluación(4) :- not(subvaluación([(a, t)], [(a, f)])).
testSubvaluación(5) :- subvaluación([(a, t)], [(a, t)]).
testSubvaluación(6) :- findall(S, subvaluación(S, [(a, t), (b, f)]), L), length(L, 4), sinDuplicados(L), tieneEstosResultados(L, [[], [(a, t)], [(b, f)], [(a, t), (b, f)]]). % al revertir el 1er arg
testSubvaluación(7) :- findall(S, subvaluación(S, [(a, t)]), L), length(L, 2), sinDuplicados(L), tieneEstosResultados(L, [[], [(a, t)]]).
testSubvaluación(8) :- not(subvaluación([(a, f)], [(a, t)])).
testSubvaluación(9) :- not(subvaluación([(a, t), (b, f)], [(a, t)])). % la subvaluacion no puede ser mas grande

cantidadTestsExtiende(8).
testExtiende(1) :- extiende([(a, t)], ~a > b, [(a, t), (b, t)]).
testExtiende(2) :- extiende([(a, f)], ~a > b, [(a, f), (b, t)]).
testExtiende(3) :- not(extiende([(a, t)], a & ~a, [(a, t)])). % formula insatisfacible
testExtiende(4) :- findall(V, extiende([], a > b, V), Vs), length(Vs, 3), tieneEstosResultados(Vs, [[(a, f), (b, t)], [(a, t), (b, t)], [(a, f), (b, f)]]).
testExtiende(5) :- findall(V, extiende([], a > b, V), Vs), sinDuplicados(Vs).
testExtiende(6) :- extiende([(a, t)], a & b, [(a, t), (b, t)]).
testExtiende(7) :- not(extiende([(a, t)], a > b, [(a, t), (b, f)])). % no es modelo
testExtiende(8) :- extiende([(a, t)], ~a > b, [(a, t), (b, f)]).

cantidadTestsConsecuencia(7).
testConsecuencia(1) :- consecuencia(a, a).
testConsecuencia(2) :- consecuencia(a & b, a).
testConsecuencia(3) :- not(consecuencia(a, a & b)).
testConsecuencia(4) :- not(consecuencia(a, a > b)).
testConsecuencia(5) :- consecuencia(a, b > a).
testConsecuencia(6) :- consecuencia(~a & a, b). % premisa insatisfacible, vale igual
testConsecuencia(7) :- not(consecuencia(a & b, a & ~b)).

cantidadTestsEquivalentes(5).
testEquivalentes(1) :- equivalentes(a, a).
testEquivalentes(2) :- equivalentes(~(~a), a).
testEquivalentes(3) :- equivalentes(a > b, ~(a & ~b)).
testEquivalentes(4) :- not(equivalentes(a & b, a)).
testEquivalentes(5) :- not(equivalentes(a, a > b)).

cantidadTestsContradictorias(6).
testContradictorias(1) :- contradictorias(a, ~a).
testContradictorias(2) :- contradictorias(a & b, ~(a & b)).
testContradictorias(3) :- not(contradictorias(a, b)). % pueden ser verdaderas a la vez
testContradictorias(4) :- contradictorias(a > b, a & ~b).
testContradictorias(5) :- not(contradictorias(a, a)).
testContradictorias(6) :- not(contradictorias(a > b, a > b)).

cantidadTestsModeloÚnico(6).
testModeloÚnico(1) :- modeloÚnico(a, [(a, t)]).
testModeloÚnico(2) :- modeloÚnico(a & b, [(a, t), (b, t)]).
testModeloÚnico(3) :- not(modeloÚnico(a > b, _)).
testModeloÚnico(4) :- not(modeloÚnico(a & ~a, _)).
testModeloÚnico(5) :- not(modeloÚnico(a & b, [(a, t), (b, f)])). % modelo valido pero no unico
testModeloÚnico(6) :- findall(M, modeloÚnico(a, M), Ms), length(Ms, 1), tieneEstosResultados(Ms, [[(a, t)]]).

tests(parsea) :- cantidadTestsParsea(M), forall(between(1,M,N), testParsea(N)).
tests(variables) :- cantidadTestsVariables(M), forall(between(1,M,N), testVariables(N)).
tests(valuación) :- cantidadTestsValuación(M), forall(between(1,M,N), testValuación(N)).
tests(esVálida) :- cantidadTestsEsVálida(M), forall(between(1,M,N), testEsVálida(N)).
tests(modelo) :- cantidadTestsModelo(M), forall(between(1,M,N), testModelo(N)).
tests(mejorModelo) :- cantidadTestsMejorModelo(M), forall(between(1,M,N), testMejorModelo(N)).
tests(subvaluación) :- cantidadTestsSubvaluación(M), forall(between(1,M,N), testSubvaluación(N)).
tests(extiende) :- cantidadTestsExtiende(M), forall(between(1,M,N), testExtiende(N)).
tests(consecuencia) :- cantidadTestsConsecuencia(M), forall(between(1, M, N), testConsecuencia(N)).
tests(equivalentes) :- cantidadTestsEquivalentes(M), forall(between(1, M, N), testEquivalentes(N)).
tests(contradictorias) :- cantidadTestsContradictorias(M), forall(between(1, M, N), testContradictorias(N)).
tests(modeloÚnico) :- cantidadTestsModeloÚnico(M), forall(between(1, M, N), testModeloÚnico(N)).

tests(todos) :-
  tests(parsea),
  tests(variables),
  tests(valuación),
  tests(esVálida),
  tests(modelo),
  tests(mejorModelo),
  tests(subvaluación),
  tests(extiende),
  tests(consecuencia),
  tests(equivalentes),
  tests(contradictorias),
  tests(modeloÚnico).

tests :- tests(todos).
