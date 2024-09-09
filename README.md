# bb

**WARNING: EXPERIMENTAL, DO NOT USE**

bb is a build system which allows you to define build rules using [OCaml][], then
to execute them using [ninja][].

bb intends to be a hackable build system.

bb stands for "Blitzbau", which is German for "lightning construction".

## Example run

Have `ninja` installed, then:

```
dune exec -- ./lib/bbtop.exe _b.ml
bb build
```

[ninja]: https://ninja-build.org/
[OCaml]: https://ocaml.org/
