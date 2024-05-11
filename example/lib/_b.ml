library "lib"
  ~deps:[ `ocamlfind_lib "tyxml" ]
  ~srcs:[ P.v "lib_a.ml"; P.v "lib_b.ml" ]
