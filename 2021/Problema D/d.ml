type ntree = Node of int * int * (ntree*int) option * (ntree*int) list
  
(* se u v w são três colónias previamente definidas e ligadas à Terra 
que sabemos ter um custo de 28,  então podemos definir a Terra como:
Terra: planeta numero 1, com peso 28, não tem pai, ligado aos planetas 
u, v e w  com buracos de minhoca de peso respectivo 12, 10 e 7 *)

let terra = Node (1,28,none,[(u,12),(v,10),(w,7)])