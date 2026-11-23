<p align="center">
  <img
    src="https://raw.githubusercontent.com/shackle-rs/shackle/develop/assets/logo.svg"
    alt="shackle logo"
    height="250px">
</p>

Shackle is a constraint modelling and rewriting library and compiler framework written in rust.
Shackle is still in the early stages of development.
Its components are not yet stable and might drastically change.

## Development

The compiler tests and the `shackle compile` command require the upstream
[MiniZinc standard library](https://github.com/MiniZinc/libminizinc/tree/develop/share/minizinc) from develop branch.
Set `MZN_STDLIB_DIR` to MiniZinc's `share/minizinc` (not the `std` subdirectory).

The compiler test suite can then be run with:

```sh
MZN_STDLIB_DIR=/path/to/share/minizinc cargo test --all-features
```

To compile and solve, use

```sh
MZN_STDLIB_DIR=/path/to/share/minizinc cargo run -p shackle-cli -- solve path/to/model.mzn
```

Or compile a MiniZinc model to MicroZinc (suffixed with `.shackle.mzn`):

```sh
MZN_STDLIB_DIR=/path/to/share/minizinc cargo run -p shackle-cli -- compile path/to/model.mzn
```

To execute the Shackle runtime and write NanoZinc to `model.shackle.nzn`,
select the NanoZinc target. It accepts DZN or JSON data files:

```sh
MZN_STDLIB_DIR=/path/to/share/minizinc cargo run -p shackle-cli -- compile --target nzn path/to/model.mzn path/to/data.dzn
```

NanoZinc compilation is experimental. Some enum constructors, runtime operations,
and data expressions are not yet supported. Solving a NanoZinc target is not yet
implemented. MiniZinc and MicroZinc compilation are data independent and reject
data files.

## Acknowledgements

This research was partially funded by the Australian Government through the Australian Research Council Industrial Transformation Training Centre in Optimisation Technologies, Integrated Methodologies, and Applications ([OPTIMA](https://optima.org.au)), Project ID IC200100009.
