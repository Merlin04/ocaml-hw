# Semantics

Write inference rules for an language of propositional logic defined by the following AST type:

```
type prop =
    | And of prop * prop
    | Or of prop * prop
    | Not of prop
    | True
    | False
```

```
e1 ==> v1      e2 ==> v2
------------------------
And (e1, e2) ==> v1 & v2

e1 ==> v1      e2 ==> v2
------------------------
Or (e1, e2) ==> v1 | v2

e1 ==> v1
----------------
Not (e1) ==> !v1

-------------
True ==> true

---------------
False ==> false
```