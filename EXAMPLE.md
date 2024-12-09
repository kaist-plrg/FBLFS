# Wrapper

Wrapper takes single binary executable and wraps it with a symbol information.
It has ".elf_ext" extension and can be executed as a normal binary.
*Note: Wrapper only works on Linux because it uses readelf executable.*

## Usage

```bash
$ ./_build/default/ocaml-core/src/tool/wrapper.exe <binary> 
```


# Lifter
Lifter takes a single wrapped binary and lifts it to several formats.

## Usage

```bash
$ ./_build/default/ocaml-core/src/tool/lifter.exe <wrapped_binary> 
```

# Interpreter
Interpreter takes a single lifted binary and interprets it.

## Usage

```bash
$ ./_build/default/ocaml-core/src/tool/interpreter.exe <lifted_binary> 
```

applicable lifted binary formats are:
- .fgir_dump
- .asir_dump
- .ioir_dump

# DafnyPrinter
DafnyPrinter takes a single IOIR lifted binary and prints it in Dafny format.

## Usage

```bash
$ ./_build/default/ocaml-core/src/tool/dafny_printer.exe <ioir_dump> 
```


# Running Example

Consider the following C code, which is saved in EXAMPLE.c:

```c
int add(int a, int b) {
    return a + b;
}
int main() {
    int a = 0;
    int b = 1;
    int c = add(a, b);
    return c;
}
```

To compile the C code, run the following command:

```bash
$ gcc EXAMPLE.c -fno-pie -no-pie -O0 -o EXAMPLE
```

To lift the compiled binary, run the following command:

```bash
$ ./_build/default/ocaml-core/src/tool/wrapper.exe EXAMPLE
$ ./_build/default/ocaml-core/src/tool/decompiler.exe EXAMPLE.elf_ext
```

To interpret the lifted binary, run the following command:

```bash
$ ./_build/default/ocaml-core/src/tool/interpreter.exe EXAMPLE.fgir_dump
(or EXAMPLE.asir_dump or EXAMPLE.ioir_dump)
```

To print the lifted binary in Dafny format, run the following command:

```bash
$ ./_build/default/ocaml-core/src/tool/dafnyprinter.exe EXAMPLE.ioir_dump
```
