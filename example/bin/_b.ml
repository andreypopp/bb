executable "bin"
  ~deps:[ `lib "lib"; `ocamlfind_lib "re" ]
  ~srcs:[ P.v "a.ml"; P.v "b.ml" ]
