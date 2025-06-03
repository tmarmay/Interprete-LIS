## Laboratorio Lenguajes y Compiladores 

#### Tomas Marmay ~ Famaf 2025
---

### Consigna
Implementar el lenguaje imperativo simple + fallas + input-output (LIS+Fallas+IO).

Extender la sintaxis abstracta con los constructores correspondientes y 
re-definir la función semántica

```haskell
sem :: Expr dom → Σ → dom
```

que implementa el significado de estas expresiones; implementar las instancias
de la clase DomSem para MInt, MBool y Ω.

1. Agregar los constructores sintácticos correspondientes al lenguaje imperativo simple: newvar, asignación, while, sentencia condicional y composición.
2. Implementar su función semántica. Al no tener fallas e input-output nuestro dominio alcanza con Σ⊥, sin embargo no podemos representar (razonablemente)
el error producido por una división por cero. Por ejemplo, si consideramos el programa "x := 3 / 0", entonces tendríamos dos opciones a priori; la primera es que la implementación propague Nothing (pensar si esto es posible dada nuestra definición de Ω), otra posibilidad es devolver "Abort σ" para algún σ (opción sugerida). Para esta última es útil definirse una función que transforme un posible error representado con Nothing a un error representado con Abort; meditar sobre la función ya definida (>>==). En este caso notar que estaríamos utilizando Σ'⊥.
3. Agregar los constructores sintácticos para extender el lenguaje con fallas.
4. Implementar su función semántica teniendo en cuenta que el error de división por cero queremos representarlo con abort.

5.   Agregar los constructores sintácticos para extender el lenguaje con
     input-output
6. Implementar su función semántica.

---

### Breve explicaicon

El módulo [Interprete.hs](Interprete.hs) implementa un intérprete para un lenguaje imperativo llamado LIS visto en el teorico de la materia. El lenguaje incluye expresiones aritméticas, booleanas, comandos de control de flujo, manejo de excepciones e I/O.

#### Semantica denotacional
- Variables (`Var`) se representan como String.
- Entornos (`Σ`) son funciones de variables a enteros: ```Var -> Int```

#### Tipos de dominio

- MInt : Maybe Int
- MBool : Maybe Bool
- `Ω` : Formado por la union de:
    - `Normal Σ` : ejecución sin errores
    - `Abort Σ` : ejecución con falla
    - `Out (Int, Ω)` : salida de un entero
    - `In (Int -> Ω)` : entrada de un entero

#### AST del lenguaje
```haskell
data Expr a where
  -- Expresiones aritméticas
  CInt | V | Plus | Dif | Times | Div

  -- Expresiones booleanas
  Eq | Lt | Neq | And | Or | Not

  -- Comandos
  Skip | Assign | Seq | If | Newvar | While
  Fail | Catch
  Output | Input
```

#### Evaluación Semántica

El tipo `DomSem` define una clase para interpretar expresiones en diferentes dominios:

```haskell
class DomSem dom where
  sem :: Expr dom -> Σ -> dom
```

Instancias implementadas:
- `DomSem MInt` : evaluación de expresiones aritméticas
- `DomSem MBool` : evaluación de expresiones booleanas
- `DomSem Ω` : evaluación de comandos

---

### Como Probar el Interprete

```bash
ghci Test.hs # carga el interprete y los test
```
```haskell
eval_ej<int> -- ejecuta el test numero <int>
```

---

Nota: primero desarrolle el interprete de una version mas simplificada del lenguaje - ver [.interprete.hs](.interprete.hs) - lo que me ayudo mucho a poder desarrollar el segundo interprete.