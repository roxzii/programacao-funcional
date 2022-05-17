open Printf
(*Nós utilizamos ref lista pois é uma nameira fácil de adicionar elementos a lista*)
let lista = ref []

[@@@ warning "-8"]
let rec get_nth = function
  | h :: _, 0 -> h
  | h :: t, n -> get_nth (t, n-1)
  [@@@ warning "+8"]

  (*Algoritmo dimânico:*)
  let rec minCoins m v=
  let table = Array.init ( v+1) (fun _ -> (9999) ) in
  begin
  Array.set table 0 0;
  for i = 1 to v do 
    begin
        for j = 0 to (m-1) do
            begin
                if get_nth ( !lista, j) <= i then
                    begin
                        let sub_res = table.(i-get_nth ( !lista, j))in
                            if (sub_res != 9999 && (sub_res +1) < table.(i))                            
                              then 
                              begin
                              Array.set table i (sub_res + 1)
                              end
                    end
            end
          done
    end
  done;
  if table.(v) = 9999 then (- 1) else table.(v)
  end

(*Função responsável pela leitura dos valores*)
let read =
  let quantidades = read_int () in
  let t0 = ref 0 in (*--> t0 é utilizada pelo Algoritmo Guloso para guarda o número de moedas*)
  let algdimanico = ref 0 in
  let erro = ref 0 in
  let troco = ref 0 in
  let fox = ref 0 in (*--> Fox é a variavél onde fica guardada o valor do troco*)
  let g = ref 0 in
  let paragem = ref 0 in 
  (*A varavél paragem é utilizada para para o algoritmo Guloso para não entrar em ciclo infimito*)

  (*Parte responsável por adicionar os valores a lista de valores
  -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*)
  if quantidades <= 100 then
    for n = 1 to quantidades do
      let valor = read_int () in
      lista := valor :: !lista;
    done;
    (*Ordenamos a lista por ordena descrestente para poder ser usada*)
    lista := List.rev (List.sort compare !lista);
  (*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*)
  for n = 1 to 550 do
    troco := n;
    fox := !fox + 1;
      (*Este if é utilizado para saber se é possivel dá troco para o valor com as moedas da lista
        se não for possivel o Algoritmo dimâmico devolve -1*)
      algdimanico := (minCoins quantidades !fox);
      if !algdimanico <> -1 then
      (
        (*Algoritmo Guloso:
        XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
        while !troco <> 0 && !paragem < 590 do
          for n = 0 to quantidades - 1 do
            if get_nth (!lista, n) <= !troco && !g = 0  then
              (troco := !troco - get_nth (!lista, n);
              t0 := !t0 + 1;
              g := 10)
          done;
          paragem := !paragem + 1;
          g := 0
          (*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
        done;
        if algdimanico < t0 && !erro <> 45 || !paragem = 590 && !erro <> 45 then
          (printf "%d\n" !fox;
          erro := 45);
        t0 := 0;
        paragem := 0;
      )
  done;
  if !erro = 0 then
    printf "YES\n"