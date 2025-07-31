# NAND programming language


A programming language to play around and create boolean functions while being strictly limited to operations involving NAND gates.

The NAND gate is functionally complete, which means that any boolean function ($\set{0, 1}^n \to \set{0, 1}^m$) can be implemented using only combinations of NAND gates.

Still, it is **not** a Turing-complete language: while loops are not allowed.

![a screenshot of the NAND programming language](https://rreemmii-dev.github.io/nand-language/main.png)


## Installation and Use

### Build the interpreter

To build the interpreter, you need to have `dune` installed.

```bash
cd src
dune build
cd ..
mv src/interpreter.exe .
```

### Run the interpreter

After building the interpreter, you can execute it on your `.nand` source file.

```bash
./interpreter.exe file.nand
```


## Syntax and Features

### Variables

As a convention, lowercase names are used for booleans, and uppercase names for arrays.

```
x = 1;  /* Defines x as the boolean value 1 (true) */
x = 0;  /* Changes the value of x to 0 (false) */

X{8} = 0;  /* Defines X as an array of lenght 8 filled with 0 (false) */
X[2] = 1;  /* Changes the value of X[2] to 1 (true) */
```

### Functions

Using the same convention as for variables, lowercase names are used for functions returning booleans, and uppercase names for functions returning arrays.

```
fun not(x) {
    return nand(x, x);  /* nand is the only arleady-defined function */
}
```

### For loops

For loops can only iterate over a range of indices (inclusive).

```
for i = 0 to 3 {
    X[i] = not(X[i]);
}
```

### Debugging

There are two debugging tools: `debug print` and `debug [int64 | int32 | int16 | int8]`

```
debug print X;  /* Prints X as a human-readable value */
debug int8 X;  /* Prints the 8-bit integer representation of X */
```

### Example code

#### Simple example

Here is a simple example:

```
fun not(x) {
    return nand(x, x);
}

fun main() {
    X{8} = 0;
    X[2] = 1;

    debug print X;  /* Output: Array {  Bool false  Bool false  Bool true  Bool false  Bool false  Bool false  Bool false  Bool false    } */

    for i = 0 to 3 {
        X[i] = not(X[i]);
    }

    debug print X;  /* Output: Array {  Bool true  Bool true  Bool false  Bool true  Bool false  Bool false  Bool false  Bool false    } */
    debug int8 X;  /* Output: Int 11 */
}
```


## Larger example code

A larger example code containing useful predefined functions can be found in [file.nand](file.nand)


## License

Distributed under the MIT License. See [LICENSE.md](LICENSE.md)
