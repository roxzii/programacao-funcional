(*
Ruben José Dias Alves Nº:46244    Ano:1º Engenharia Informática
José Miguel ALves Melo dos Santos Nº:45968   Ano:1º Engenharia Informática
*)
open Printf
(*A referência max é utilizada para se poder fazer comparação com o count e determinar o menor número de regras*)
let max = ref 99999

(*Dezenas e unidades:
Esta função determina as dezenas e as unidades de um algarismo e depois calcula o produto entre as dezenas e as unidades*)
let rec unidades_dezenas valor =
  let aux = ref 0 and aux1 = ref 0 in
  aux1 := valor;
  while valor >= !aux + 100 do
    aux := !aux + 100
  done;
  aux1 := abs (!aux1 - !aux);
  let dezenas = ref 0 in
  while !aux1 >= !dezenas + 10 do
    dezenas := !dezenas + 10
  done;
  let unidades = ref 0 in
  while !aux1 - !dezenas > !unidades do
    unidades := !unidades + 1;
  done;
  dezenas := !dezenas/10;
  !unidades * !dezenas
  (*++++++++++++++++++++++++++++++++++++++++++++++*)


(*Esta função é responsável pela aplicação das regras sobre o valor introduzido pelo utilizador:*)
let rec recursiva num count =
  if num = 42 then
  begin
    (*Compara o número de regras obtido anteriormente com o número de regras obtido agora. Se o obtido agora for menor
    então ele faz a substituição*)
    if count < !max then
      max := count
    end
  else
    begin
    (*Verifica se o valor é menor que 42, se for não há regras possíveis que se possa aplicar para chegar ao 42.
    Ele também verifica se já foi obtido o menor número de regras e se sim não continua.*)
    if num > 42 && count < !max then
      begin
      if num mod 2 = 0 then
        recursiva (num/2) (count + 1);
      if num mod 3 = 0 || num mod 4 = 0 then
        if unidades_dezenas num > 0 then
            recursiva (num - unidades_dezenas num) (count + 1);
      if num mod 5 = 0 then
        recursiva (num - 42) (count + 1)
      end
    end

let () = printf "Por favor, introduza um valor:\n"
let () = recursiva (read_int ()) 0
(*Faz a impressão do BAD LUCK ou do número de regras mínimos para obter o valor 42.*)
let () = 
  if !max = 99999 then 
    printf "BAD LUCK\n" 
  else 
    printf "Número de regras utilizadas: %d\n" !max