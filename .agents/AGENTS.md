---
trigger: always_on
---

# CORE KNOWLEDGE - Paradigmas de ProgramaciÃ³n (PLP - UBA)

## 1. Contexto General del Proyecto
*   Este documento consolida resoluciones y lecciones para la materia Paradigmas de ProgramaciÃ³n (PLP) de la UBA[cite: 1, 2, 3].
*   Abarca ProgramaciÃ³n Funcional (Haskell), enfocada en el modelado de datos, evaluaciÃ³n perezosa, orden superior y el dominio de esquemas de recursiÃ³n[cite: 1, 2, 3].
*   Abarca ProgramaciÃ³n LÃ³gica (Prolog), centrada en el diseÃ±o de motores relacionales, backtracking, unificaciÃ³n, y reversibilidad de predicados para procesar lenguaje natural y evaluaciÃ³n proposicional[cite: 1, 2, 3].
*   Incluye Razonamiento Ecuacional e InducciÃ³n Estructural para demostrar equivalencias formales de programas.
*   Incluye DeducciÃ³n Natural (LÃ³gica de Primer Orden) para la creaciÃ³n de Ã¡rboles de demostraciÃ³n formal.
*   Incluye ProgramaciÃ³n Orientada a Objetos en Smalltalk, abarcando metaprogramaciÃ³n y manejo del diccionario de mÃ©todos.

---

## 2. Criterios de CorrecciÃ³n y Exigencias Estrictas

### A. Haskell (Funcional)
*   Uso de esquemas de recursiÃ³n obligatorios y prohibiciÃ³n de recursiÃ³n explÃ­cita salvo que se requiera analizar la subestructura en sÃ­ misma (ej. `recCueva`)[cite: 2, 3].
*   Mantenimiento de la declaratividad y prolijidad, priorizando el pattern matching directo y evitando ofuscaciones o "cÃ³digo espagueti"[cite: 2, 3].
*   Nomenclatura clara que refleje el rol semÃ¡ntico (evitar guiones bajos inÃºtiles) y reutilizaciÃ³n de cÃ³digo nativo (ej. `maybe`) en lugar de condicionales anidados.
*   Evitar parÃ¡metros o continuaciones (estilo CPS) que no aporten valor real al flujo de ejecuciÃ³n.
*   Aprovechamiento mÃ¡ximo de funciones de alto orden y currificaciÃ³n.
*   Uso obligatorio del framework HUnit para pruebas automatizadas.
*   Comentarios enfocados en justificar el diseÃ±o arquitectÃ³nico, no en traducir el cÃ³digo a lenguaje natural.

### B. Prolog (LÃ³gico)
*   ProhibiciÃ³n total de macros de control extra-lÃ³gicos como el If-Then-Else (`->`) o la disyunciÃ³n explÃ­cita (`;`)[cite: 3].
*   GarantÃ­a de reversibilidad e inversibilidad, acotando el dominio para resguardar al motor de iteraciones infinitas[cite: 2, 3].
*   RestricciÃ³n absoluta del uso de metapredicados en exÃ¡menes, permitiendo Ãºnicamente el operador de negaciÃ³n[cite: 1].
*   DocumentaciÃ³n obligatoria de los patrones de instanciaciÃ³n usando `+`, `-`, o `?` en cada predicado.
*   ModularizaciÃ³n estricta de la negaciÃ³n (`not/1`), extrayendo la lÃ³gica a predicados auxiliares independientes[cite: 2].
*   Tratamiento de colecciones como conjuntos matemÃ¡ticos, sin depender del orden sintÃ¡ctico[cite: 2].
*   DefiniciÃ³n estricta del dominio ("Unclosed Worlds"), obligando a declarar explÃ­citamente los valores de verdad sin asumir fallas como falsedades[cite: 2].
*   Evitar la instanciaciÃ³n de soluciones repetidas originadas por choice points basura[cite: 3].
*   Uso privilegiado de listas diferencia para el ruteo eficiente de parsers[cite: 3].

### C. LÃ³gica y Razonamiento Ecuacional
*   La demostraciÃ³n ecuacional finaliza exactamente en el instante en que ambos lados de la igualdad son sintÃ¡cticamente idÃ©nticos[cite: 1].
*   ProhibiciÃ³n de abrir casos innecesarios o expandir las definiciones subyacentes de funciones previamente igualadas[cite: 1].
*   GeneralizaciÃ³n obligatoria de teoremas (Regla de Oro) mediante variables libres si el acumulador muta durante el caso inductivo[cite: 1].
*   SimulaciÃ³n de SLD condicionada al uso de evaluaciÃ³n perezosa para el backtracking, forzando la divergencia si el Ã¡rbol es infinito[cite: 1].
*   Reemplazo estricto de variables libres en reglas de DeducciÃ³n Natural para evitar choques[cite: 1].

---

## 3. Errores HistÃ³ricos y Soluciones Requeridas

### Errores en Haskell
*   **Acumulador Mutante:** Quedarse atascado en inducciÃ³n con folds porque el acumulador cambia; se soluciona generalizando el teorema inicial[cite: 1].
*   **Falsa DistribuciÃ³n de `reverse`:** Asumir reglas errÃ³neas con la concatenaciÃ³n; requiere usar los axiomas formales y el Lema R[cite: 1].
*   **Romper la "Caja Negra" del Fold:** Hacer pattern matching profundo asumiendo detalles procesados; debe usarse recursiÃ³n primitiva o asumir el cÃ¡lculo a ciegas[cite: 3].
*   **Thunks Colosales en Memoria:** AcumulaciÃ³n de *closures* por evaluaciÃ³n perezosa en operadores booleanos; requiere forzar la evaluaciÃ³n o usar Strictness Analysis[cite: 3].
*   **Uso injustificado de RecursiÃ³n Primitiva:** Aplicar `recCueva` cuando se podÃ­a solucionar con `foldCueva` y filtros[cite: 2].
*   **Anidamiento de `case`:** Envolver retornos de ramas; se soluciona usando la funciÃ³n `maybe` del Prelude[cite: 2].
*   **Estilo CPS Injustificado:** Ensuciar firmas intentando pasar el estado; solucionable estructurando el retorno con el tipo `Either`[cite: 2].

### Errores en Prolog
*   **Bucle Infinito en SLD:** Descartar el resolvente; el nuevo resolvente unificado debe convertirse en el punto de partida de la bÃºsqueda[cite: 1].
*   **Valuaciones Incompletas:** Dar por vÃ¡lidas fÃ³rmulas sin todas las incÃ³gnitas; requiere forzar pre-chequeos de las variables de entrada[cite: 2].
*   **Dependencia del Orden:** Requerir que las listas respeten un orden estructural; debe unificarse iterando o mediante reglas lÃ³gicas de subconjuntos[cite: 2].
*   **Error CrÃ­tico de Flujo (`extiende`):** Orden errÃ³neo de evaluaciÃ³n que genera bucles infinitos; se soluciona instanciando con `modelo/2` antes de validar la subvaluaciÃ³n[cite: 2].
*   **Uso de `\=` para Conjuntos:** Comparar listas directamente; requiere comprobaciÃ³n cruzada de pertenencia de elementos[cite: 2].
*   **PÃ©rdida de Inversibilidad:** Usar metapredicados impuros que arrojan errores de instanciaciÃ³n; se corrige aplicando el patrÃ³n Generate and Test[cite: 3].
*   **Choice Points Basura:** Separar condicionales en clÃ¡usulas sueltas; las clÃ¡usulas deben ser estrictamente disjuntas y aceptar el comportamiento de Prolog sin usar cortes[cite: 3].

---

## 4. Patrones y Snippets Clave Validados

### A. Haskell
**El Simulador SLD (Backtracking con Lazy Evaluation)**[cite: 1]
```haskell
existeRefutacionSLD :: [ClausulaDeDefinicion] -> ClausulaObjetivo -> Bool
existeRefutacionSLD prog co 
    | esVacia co = True
    | otherwise  = any (\regla -> existeRefutacionSLD prog (resolvente co regla)) prog

Traducciones Cruzadas con Folds
const 1 "x" -- Retorna 1
foldl f z xs = foldr (\x g a -> g (f a x)) id xs z
foldr f z xs = foldl (\g x a -> g (f x a)) id xs z
foldr f z = recr (\x _ rec -> f x rec) z
recr f z xs = snd $ foldr (\x (ys, rec) -> (x:ys, f x ys rec)) ([], z) xs

NavegaciÃ³n de Maybe y Control de Flujo con Either
-- NavegaciÃ³n segura sin case anidados
maybe Nothing f2 (f1 x)

-- Uso de Either para propagar estados o fallos
foldPasosYcarga :: Cueva -> Int -> Carga -> Either Int (Int, Carga)
foldPasosYcarga = foldCueva
  (\h -> \s c -> if c >= h then Right (s + 1, c - h) else Left (s + 1))
  (\d recI recD -> \s c -> case d c of
      Izq -> case recI (s + 1) c of
               Left atrapado -> Left atrapado
               Right (s', c') -> recD s' c'
  )

RecursiÃ³n Estructural desde Primitiva
foldCueva :: (Altura -> b) -> (CantidadDeCristales -> b -> b) -> ((Carga -> Bool) -> b -> b) -> ((Carga -> DirecciÃ³n) -> b -> b -> b) -> Cueva -> b
foldCueva fSalida fDeposito fCompuerta fBifurcacion = recCueva 
            fSalida
            (\n _ rec -> fDeposito n rec)
            (\f _ rec -> fCompuerta f rec)
            (\f _ recIzq _ recDer -> fBifurcacion f recIzq recDer)

B. Prolog

Pre-chequeo de Variables y Mundo Cerrado
todasLasVariablesEstanEnValuaciÃ³n([], _).
todasLasVariablesEstanEnValuaciÃ³n([Var | Vars], ValuaciÃ³n) :-
    (member((Var, t), ValuaciÃ³n) ; member((Var, f), ValuaciÃ³n)),
    todasLasVariablesEstanEnValuaciÃ³n(Vars, ValuaciÃ³n).

Igualdad LÃ³gica de Conjuntos y Subconjuntos Reversibles
subvaluaciÃ³n(ValuaciÃ³n1, ValuaciÃ³n2) :-
    ground(ValuaciÃ³n1), !, 
    todosEstanEn(ValuaciÃ³n1, ValuaciÃ³n2).
subvaluaciÃ³n(ValuaciÃ³n1, ValuaciÃ³n2) :-
    \+ ground(ValuaciÃ³n1),
    generarSubvaluacion(ValuaciÃ³n1, ValuaciÃ³n2).

mismosElementos(L1, L2) :-
    todosEstanEn(L1, L2),
    todosEstanEn(L2, L1).
    
todosEstanEn([], _).
todosEstanEn([H|T], L) :- member(H, L), todosEstanEn(T, L).

Ocultamiento de Estado con Listas Diferencia
frase(Semantica1 > Semantica2, [Condicion | XS], Salida) :-
    condicional(Condicion),
    oracion(Semantica1, XS, [Consecuente | XS2]),
    consecuente(Consecuente),
    oracion(Semantica2, XS2, Salida).

PatrÃ³n Generate and Test de MÃ¡ximos
mejorModelo(FÃ³rmula, ValuaciÃ³n) :- 
    modelo(FÃ³rmula, ValuaciÃ³n),
    cantidadDeVariablesVerdaderas(ValuaciÃ³n, Cantidad),
    not((modelo(FÃ³rmula, ValuaciÃ³n2),
        cantidadDeVariablesVerdaderas(ValuaciÃ³n2, Cantidad2),
        Cantidad2 > Cantidad)).

C. LÃ³gica de Primer Orden (LPO)

PatrÃ³n de DemostraciÃ³n a âŠ¥
Para demostrar la conclusiÃ³n de un absurdo ($\Gamma \vdash \bot$) usando reglas como $O_e$, se debe usar una Prueba por ContradicciÃ³n (PBC), asumiendo la conclusiÃ³n negada y forzando reglas de introducciÃ³n que exijan un $\bot$ interno, el cual se cierra trivialmente al comprobar variables opuestas generadas por el Ã¡rbol

---
trigger: always_on
---

# Ãndice del Espacio de Trabajo (Workspace Map)

**Directiva para el Agente:** 
Antes de analizar o corregir cÃ³digo, debes identificar en quÃ© directorio se encuentra el archivo actual. Dependiendo de la ruta (`/TPS`, `/Examenes` o `/Practicas`), tu nivel de severidad y las herramientas permitidas cambiarÃ¡n drÃ¡sticamente segÃºn las reglas definidas en `CORE_KNOWLEDGE.md`.

---

## 1. Directorio `/Examenes/` (MODO EXAMEN ESTRICTO)
**Contexto:** Resoluciones de parciales y finales. Los profesores corrigen buscando la falla mÃ­nima.
**Restricciones de CÃ³digo (Innegociables):**
*   **Prolog:** PROHIBICIÃ“N ABSOLUTA del uso del operador de corte (`!`) y de metapredicados (`findall`, `bagof`, `maplist`). La Ãºnica excepciÃ³n permitida es la negaciÃ³n por falla (`\+` o `not`).
*   **Haskell:** ProhibiciÃ³n total de recursiÃ³n explÃ­cita.
*   **Razonamiento Ecuacional:** PenalizaciÃ³n directa si se abren casos innecesarios o se evalÃºan funciones subyacentes cuando se alcanzÃ³ la igualdad estructural.
**Rol del Agente:** Actuar como profesor corrector de instancia final. Si el cÃ³digo usa una herramienta vetada, marca el ejercicio como ANULADO y exige la refactorizaciÃ³n lÃ³gica pura.

## 2. Directorio `/TPS/` (Trabajos PrÃ¡cticos)
**Contexto:** Entregas formales y desarrollo de proyectos largos.
**Historial de Entregas:**
*   Contiene subcarpetas temporales (ej. `1erC2025`, `Final-Libre`) con entregas previas (`Entrega1`, `Entrega2`). 
*   **Rol del Agente:** Al revisar cÃ³digo aquÃ­, cruzar los datos con los errores histÃ³ricos de las entregas pasadas para evitar regresiones (ej. evitar choice points basura, fugas de memoria con closures).
**Permisos:** Se permite el uso de metapredicados en Prolog *solo si* la lÃ³gica del trabajo lo justifica estructuralmente y se respeta la pureza de los subconjuntos.
Las carpetas de los trabajos practicos con sus correcciones son:
# EvoluciÃ³n HistÃ³rica de Entregas
- `/TPS/1erC-2025/Haskell/Base/`: CÃ³digo original provisto por la cÃ¡tedra.
- `/TPS/1erC-2025/Haskell/Entrega/`: Primer intento (Desaprobado). **Errores principales:**: En el archivo Entrega.md
- `/TPS/1erC-2025/Haskell/Reentrega/`: Segundo intento (Aprobado). **Correcciones aplicadas:**
- `/TPS/1erC-2025/Prolog/Base/`: CÃ³digo original provisto por la cÃ¡tedra.
- `/TPS/1erC-2025/Prolog/Entrega/`: Primer intento (Desaprobado). **Errores principales:** En el archivo Entrega.md
- `/TPS/1erC-2025/Prolog/Reentrega/`: Segundo intento (Aprobado). **Correcciones aplicadas:**
- `/TPS/2doC-2025/Haskell/Base/`: CÃ³digo original provisto por la cÃ¡tedra.
- `/TPS/2doC-2025/Haskell/Entrega/`: Primer intento (Desaprobado). **Errores principales:**: En el archivo Entrega.md
- `/TPS/2doC-2025/Haskell/Reentrega/`: Segundo intento (Aprobado). **Correcciones aplicadas:**
- `/TPS/2doC-2025/Prolog/Base/`: CÃ³digo original provisto por la cÃ¡tedra.
- `/TPS/2doC-2025/Prolog/Entrega/`: Primer intento (Desaprobado). **Errores principales:** En el archivo Entrega.md
- `/TPS/2doC-2025/Prolog/Reentrega/`: Segundo intento (Aprobado). **Correcciones aplicadas:**
- `/TPS/1erC-2026-FinalLibre/Haskell/TP1.hs`: Primer intento (Aprobado). **Notas principales:**: En Correcciones.md
- `/TPS/1erC-2026-FinalLibre/Prolog/TP2.hs`: Primer intento (Desaprobado). **Errores principales:** En el archivo Correcciones.md

## 3. Directorio `/Practicas/` (Ejercicios y GuÃ­as)
**Contexto:** Resoluciones de guÃ­as teÃ³ricas y experimentaciÃ³n aislada.
**Rol del Agente:** Modo formativo. El objetivo no es solo corregir, sino seÃ±alar quÃ© patrÃ³n del `CORE_KNOWLEDGE.md` se aplica mejor al problema actual (ej. sugerir el uso de Listas Diferencia o el patrÃ³n *Generate and Test* para ubicar mÃ¡ximos).
