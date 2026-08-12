---
name: profiler
description: Ejecuta la habilidad profiler
---

DescripciÃ³n: Analizador de Big-O, consumo de memoria y DRY (Don't Repeat Yourself).
Prompt: "Analiza el siguiente cÃ³digo exclusivamente desde la perspectiva de rendimiento y uso de recursos.

    Calcula la complejidad temporal y espacial asintÃ³tica.

    Haskell: Identifica explÃ­citamente si hay riesgo de acumulaciÃ³n de Thunks (Space Leaks) o si una funciÃ³n de alto orden estÃ¡ recorriendo la misma estructura mÃºltiples veces innecesariamente.

    Prolog: Rastrea el Ã¡rbol de ejecuciÃ³n (SLD) e identifica Choice Points que queden abiertos. Exige que las clÃ¡usulas sean mutuamente excluyentes.

    PropÃ³n una refactorizaciÃ³n que elimine cÃ³digo duplicado y mejore la eficiencia, sin romper la declaratividad."
