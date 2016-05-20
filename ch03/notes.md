# 03 Defining Types, Streamlining Functions

Defining a New Data Type:
```hs
-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)
```
- _BookInfo_: type constructor
- _Book_: value constructor
- _Int, String, [String]_: components
Usually the value constructor and the type constructor have the same name.

Compare the discriminated union in C and in Haskell
```c
enum shape_type {
    shape_circle,
    shape_poly,
};

struct circle {
    struct vector centre;
    float radius;
};

struct poly {
    size_t num_vertices;
    struct vector *vertices;
};

struct shape 
{
    enum shape_type type;
    union {
  struct circle circle;
  struct poly poly;
    } shape;
};
```
```hs
type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]
```

Recursive Types
```bash
*Main> :l ListADT.hs 
[1 of 1] Compiling Main             ( ListADT.hs, interpreted )
Ok, modules loaded: Main.
*Main> Cons 0 Nil
Cons 0 Nil
*Main> Cons 1 it
Cons 1 (Cons 0 Nil)
*Main> Cons 2 it
Cons 2 (Cons 1 (Cons 0 Nil))
*Main> Cons 3 it
Cons 3 (Cons 2 (Cons 1 (Cons 0 Nil)))
```