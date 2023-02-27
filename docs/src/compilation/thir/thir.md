# Typed high-level intermediate representation (THIR)

The THIR is used as a transition phase between the HIR and the MIR. It progressively rewrites the IR into lower-level
constructs until it can be readily lowered into MIR. Compared to HIR, the THIR is:

- Combined (there is only one `Model` which contains all of the items from all model files)
- Fully typed (expressions always have full, correct types - we abort compilation if there were errors)
- Destructured - declaration items can only declare a single identifier
- Fully resolved (identifiers do not have their own names, they simply point to the item they are for)
- Desugared (since type information is now available, we can perform more desugarings)
- All items have their own data, so declarations/constraint items in let expressions now have their own arenas for their
  expressions

The language constructs made available in THIR are an attempt to provide rewriting stages with a convenient
representation which is not too high-level and complex to process, but not so low-level as to be cumbersome to use.

## Structure

The root THIR node is the `Model` which contains arenas for each kind of item. Since there is only one `Model`, the
item indices now globally identify the items in the program. Each item contains storage for the expressions it owns in
its `ExpressionAllocator`. Expressions contain their types, which are computed during construction. A builder API is
provided (`ExpressionBuilder`) for creating expressions.

## Lowering from HIR

Lowering involves adding the items from the HIR into the THIR model. This is done in topologically sorted order, so
we only have to visit each item once. One exception is that functions with bodies are lowered in two stages: first,
the function signature is added, then the body is added to it after all items have been processed. Otherwise identifiers
in the function bodies could refer to items not yet added to the THIR.

### Desugarings

The THIR involves several semantic desugarings from the HIR.

Destructuring variable declarations are rewritten using multiple declarations:

<table style="width:100%">

<tr><th>HIR syntax</th><th>Desugaring</th></tr>

<tr>
<td>

```mzn
any: (x, (y, z)) =
  (1, (2, 3));
```

</td>
<td>

```mzn
tuple(int, tuple(int, int)): A =
  (1, (2, 3));
int: x = A.1;
tuple(int, int): B = A.2;
int: y = B.1;
int: z = B.2;
```

</td>
</tr>

</table>

Type aliases are removed as they are resolved:

<table style="width:100%">

<tr><th>HIR syntax</th><th>Desugaring</th></tr>

<tr>
<td>

```mzn
type Foo = tuple('..'(1, 3), '..'(2, 4));
var Foo: x;
```

</td>
<td>

```mzn
set of int: A = '..'(2, 4);
set of int: B = '..'(1, 3);
tuple(var A, var B): x;
```

</td>
</tr>

</table>

2D array literals are rewritten using `array2d`:

<table style="width:100%">

<tr><th>MiniZinc syntax</th><th>Desugaring</th></tr>

<tr>
<td>

```mzn
[| 1, 2
 | 3, 4 |]
```

</td>
<td>

```mzn
array2d(1..2, 1..2, [1, 2, 3, 4])
```

</td>
</tr>

<tr>
<td>

```mzn
[|    c: d:
 | a: 1, 2
 | b: 3, 4 |]
```

</td>
<td>

```mzn
array2d([a, b], [c, d], [1, 2, 3, 4])
```

</td>
</tr>

</table>

Indexed array literals are rewritten using `arrayNd`:

<table style="width:100%">

<tr><th>MiniZinc syntax</th><th>Desugaring</th></tr>

<tr>
<td>

```mzn
[3: a, b, c]
```

</td>
<td>

```mzn
arrayNd(3, [a, b, c])
```

</td>
</tr>

<tr>
<td>

```mzn
[3: a, 4: b, 5: c]
```

</td>
<td>

```mzn
arrayNd([3, 4, 5], [a, b, c])
```

</td>
</tr>

</table>

Slicing is rewritten using `slice_xd` function calls:

<table style="width:100%">

<tr><th>HIR syntax</th><th>Desugaring</th></tr>

<tr>
<td>

```mzn
any: x = [1, 2, 3];
any: y = x['..'(1, 2)];
```

</td>
<td>

```mzn
array [int] of int: x = [1, 2, 3];
array [int] of int: y = let {
  set of int: A = '..'(1, 2);
} in slice_1d(x, [A], A);
```

</td>
</tr>

<tr>
<td>

```mzn
any: x = [1, 2, 3];
any: y = x[..<];
```

</td>
<td>

```mzn
array [int] of int: x = [1, 2, 3];
array [int] of int: y = let {
  set of int: A = '..<'(index_set(x));
} in slice_1d(x, [A], A);
```

</td>
</tr>

</table>

Case expressions are rewritten such that the destructuring is moved into the branch RHS, and pattern identifiers which
create new variables are replaced with the wildcard `_` pattern:

<table style="width:100%">

<tr><th>HIR syntax</th><th>Desugaring</th></tr>

<tr>
<td>

```mzn
enum Foo = A(Bar);
enum Bar = {B};
var Foo: x;
any: y = case x of
  A(v) => v
endcase;
```

</td>
<td>

```mzn
enum Bar = { B };
enum Foo = A(Bar);
var Foo: x;
var Bar: y = case x of
  A(_) => let {
    var Bar: v = A⁻¹(x);
  } in v
endcase;
```

</td>
</tr>

</table>

Pattern matching in comprehension generators is rewritten using a case expression in the generator's `where` clause.
Destructuring is rewritten as assignment generators:

<table style="width:100%">

<tr><th>HIR syntax</th><th>Desugaring</th></tr>

<tr>
<td>

```mzn
enum Foo = A(Bar);
enum Bar = {B};
any: x = [v | A(v) in Foo];
```

</td>
<td>

```mzn
enum Bar = { B };
enum Foo = A(Bar);
array [int] of Bar: x = [
  v | i in Foo
      where case i of
              A(_) => true,
              _ => false
            endcase,
        v = A⁻¹(i)
];
```

</td>
</tr>

</table>

## Model transformations

- Insertion of coercion functions
- Decomposition of `var` if-then-else conditionals
- Decomposition of `var` where-clause comprehensions
- Decomposition of lambda functions

## Type specialisation

See [Type specialisation](./type-specialise.md) for more detail about the process.

## Type erasure

See [Type erasure](./type-erasure.md) for more detail about the process.