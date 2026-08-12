---
name: best_practices
description: Ejecuta la habilidad best_practices
---

DescripciÃ³n: Validador de estilo basado en estÃ¡ndares canÃ³nicos de la comunidad.
Prompt: "Revisa la calidad, nomenclatura y estructuraciÃ³n de este cÃ³digo basÃ¡ndote estrictamente en los siguientes estÃ¡ndares internacionales:

    Para Haskell: Aplica las reglas del linter oficial HLint y la 'Haskell Style Guide' de Johan Tibell. Penaliza el uso de parÃ©ntesis innecesarios (sugiriendo $), lambdas redundantes (sugiriendo point-free style donde sea legible) y anidamiento.

    Para Prolog: Aplica las 'Coding Guidelines for Prolog' de Michael A. Covington. Exige espaciado correcto en los operadores, nombres de variables descriptivos (PascalCase) y predicados en snake_case.
    Rechaza cualquier cÃ³digo que, aunque funcione, no respete estos estÃ¡ndares de la industria."
