open Scanf;;
open Printf;;
open List;;


type trans = (int * char * int)
type maquina = ( trans list * int list * int list)
type ass = (int * int list)

let x = ref 0

let leitura_finais n =
    let rec leitor_finais lista n =
      if n=0 then lista else let state = Scanf.scanf "%d " (fun x -> x) in
                            let lista = lista@[state] in
                            leitor_finais lista (n-1)
                            in 
                            leitor_finais [] n

(* função recursiva para a leitura das transições *)
let rec leitura_transicoes n lista =
if n = 0 then
lista
else
let a = Scanf.scanf "%d " (fun x -> x) in
let b = Scanf.scanf "%c " (fun x -> x) in
let c = Scanf.scanf "%d\n" (fun x -> x) in
leitura_transicoes (n-1) ((a,b,c)::lista)

let rec printar_transicoes lista =
    match lista with
    | [] -> printf ""
    | (h1,h2,h3)::t -> printar_transicoes t ; printf "%d %c %d\n" h1 h2 h3 
  
let printar_cardinalidade cardinalidade =
  if cardinalidade = 0 then printf "1\n"
  else printf "%d\n" cardinalidade  

let rec printar_lista lista lista_finais=
match lista_finais with
| [] -> printf "1"
| _ ->
      if (List.length lista = 1) 
      then printf "%d\n" (List.hd lista) 
      else 
      let() = printf "%d " (List.hd lista) in
      printar_lista (List.tl lista) lista_finais

let rec construir_alfabeto transicoes aux =
    match transicoes with
    | [] -> aux
    | (h1,h2,h3)::t -> 
                    if not (List.mem h2 aux)
                    then construir_alfabeto t (h2::aux)
                    else construir_alfabeto t aux

let permutacoes letra palavra aux=
  if List.length aux = 0 then aux else
    let operador = ref aux in
    for i=0 to ((List.length palavra)-1) do
        let aux = ((String.make 1 (List.nth palavra i))^(String.make 1 letra)) in
        operador:=(aux::!operador)
    done;
    (!operador)

let explode s =
  let rec exp i l =
  match i with
   _ when i < 0 -> l
  | _ -> exp (i-1) (s.[i]::l)
        in exp (String.length s - 1) []

let criar_lista tamanho =
let lista_aux = ref [] in
    for i=tamanho downto 1 do
        lista_aux:=(i::!lista_aux)
    done;
    (!lista_aux)

let select letra estado transicao =
    map(function(a,b,c) -> c)
    (filter (function (x,y,w) -> x=estado && y=letra) transicao)

let normalize c = 
  fold_left 
    (fun res (x,y) -> 
      if (mem (x,y) res || mem (y,x) res) 
      then res 
      else ((x,y)::res) ) 
    [] c

let normalizar c = 
  fold_left 
    (fun res x -> 
      if (mem x res) 
      then res 
      else (x::res)) 
    [] c

let lista_igual estado1 estado2 lista_iniciais lista_finais lista_restantes =
      if ((List.mem estado1 (lista_iniciais@lista_restantes)) && (List.mem estado2 (lista_iniciais@lista_restantes))) ||
         ((List.mem estado1 lista_finais) && (List.mem estado2 lista_finais))
      then true else false

let rec aux_lista_palavras lista estado transicao =
    match lista with
    | [] -> estado
    | h::t -> 
              let var1 = select h estado transicao in
              if (List.length var1) = 0 then
              estado
              else
              let var2 = List.hd var1 in
              aux_lista_palavras t var2 transicao

let rec lista_palavras estado1 estado2 transicao alfabeto lista_iniciais lista_finais lista_restantes=
    match alfabeto with
    | [] -> true
    | h::t ->
                  let lista_palavra = explode h in
                  let st1 = aux_lista_palavras lista_palavra estado1 transicao in
                  let st2 = aux_lista_palavras lista_palavra estado2 transicao in
                  if lista_igual st1 st2 lista_iniciais lista_finais lista_restantes then
                  lista_palavras estado1 estado2 transicao t lista_iniciais lista_finais lista_restantes
                  else false


let rec teste_aux el tot transicao aux alfabeto lista_iniciais lista_finais lista_restantes=
  match tot with
  | [] -> aux
  | h::tail -> 
                if el = 1 && h <> 1 && (List.mem 1 lista_finais) && (List.mem h lista_finais) then
                if 
                lista_palavras el h transicao alfabeto lista_iniciais lista_finais lista_restantes then
                teste_aux el tail transicao ((el,h)::aux) alfabeto lista_iniciais lista_finais lista_restantes
                else
                teste_aux el tail transicao aux alfabeto lista_iniciais lista_finais lista_restantes
                else
                if h = el || ((List.mem h (lista_iniciais@lista_restantes) && (List.mem el lista_finais))) || ((List.mem h lista_finais) && (List.mem el (lista_restantes@lista_iniciais))) then teste_aux el tail transicao aux alfabeto lista_iniciais lista_finais lista_restantes else
                if lista_palavras el h transicao alfabeto lista_iniciais lista_finais lista_restantes then
                teste_aux el tail transicao ((el,h)::aux) alfabeto lista_iniciais lista_finais lista_restantes
                else  teste_aux el tail transicao aux alfabeto lista_iniciais lista_finais lista_restantes


let rec teste tot tot2 transicao alfabeto lista_iniciais lista_finais lista_restantes aux=
  match tot with
  | [] -> aux
  | h::t -> 
          let l = teste_aux h tot2 transicao aux alfabeto lista_iniciais lista_finais lista_restantes in
          teste t tot2 transicao alfabeto lista_iniciais lista_finais lista_restantes (l@aux)

let verificador x y z =
  if x = z then 1
  else 
  if y = z then 2
  else
  3 

let rec transitividade_aux elemento lista aux =
    match lista with
    | [] -> (List.rev aux)
    | (x,y)::t -> 
                if verificador x y elemento = 1 then transitividade_aux elemento t (y::aux)
                else  
                if verificador x y elemento = 2 then transitividade_aux elemento t (x::aux) 
                else
                transitividade_aux elemento t aux   

let rec get_cardinalidade x lista =
    match x with
    | [] -> 0
    | h::t -> 
            if (List.mem h lista) then 1
            else get_cardinalidade t lista

let rec eliminar_repeticoes_aux head aux =              
    match aux with
    [] -> true
    | h::t -> if not (List.mem head h) then
              eliminar_repeticoes_aux head t
              else false

let rec primeiro_iniciais lista elemento=
    match lista with
    | [] -> []
    | h::t -> if (List.mem elemento h) then h
              else primeiro_iniciais t elemento

let verificar_final lista_finais lista=
    let booleano = ref 0 in
    for i=0 to ((List.length lista)-1) do
      if (List.mem (List.nth lista i) lista_finais) then booleano:=1
    done;
    0

let rec primeiro_finais lista lista_finais aux=
    match lista with
    | [] -> aux
    | h::t -> if verificar_final lista_finais h = 1 then 
              primeiro_finais t lista_finais ([h]@aux)
              else
              primeiro_finais t lista_finais aux

let verificar lista1 lista2 =
  let booleano = ref 1 in
  for i=0 to ((List.length lista1)-1) do
      if (List.mem (List.nth lista1 i) lista2) then booleano:=0
  done;
  (!booleano)

let rec configurar_maquina maquina lista contador lista_iniciais lista_finais =
    match lista with
    | [] -> maquina
    | h::t -> if (verificar h lista_finais)=0 && (verificar h lista_iniciais)=1 then
              configurar_maquina ([contador,[h]]@maquina) t (contador+1) lista_iniciais lista_finais
              else
              if (verificar h lista_iniciais)=1 && (verificar h lista_finais)=1 then
              configurar_maquina ([contador,[h]]@maquina) t (contador+1) lista_iniciais lista_finais
              else
              configurar_maquina maquina t contador lista_iniciais lista_finais

let rec par_final el maquina =
  match maquina with
  | [] -> -1
  | (h1,[h2])::t -> if (List.mem el h2) then h1 else
                  par_final el t
  | _ -> -1

let rec encontrar_par_final lista el aux maquina =
    match lista with
    | [] -> aux
    | (h,w)::t -> let final = par_final w maquina in
                  encontrar_par_final t el ([el,h,final]@aux) maquina

let rec novas_transicoes maquina1 maquina2 alfabeto aux transicao =
    match maquina1 with
    | [] -> aux
    | (h,[w])::t -> let head = List.hd w in
                  let atingidos = ref [] in
                  for i=0 to ((List.length alfabeto)-1) do
                    let h = (List.nth alfabeto i) in
                    let var = select h head transicao in
                    let var_aux = (List.hd var) in
                    let new_two = (!atingidos) in
                    if (List.length var) <> 0 then atingidos:=((h,var_aux)::new_two)
                done;

                  let new_acc = encontrar_par_final (!atingidos) h aux maquina2 in
                  novas_transicoes t maquina2 alfabeto (new_acc@aux) transicao
    | _ -> aux

let rec nova_lista_finais maquina aux lista_finais=
    match maquina with
    | [] -> (List.rev aux)
    | (h1,[h2])::t -> if (List.mem (List.hd h2) lista_finais) then 
                    nova_lista_finais t (h1::aux) lista_finais
                    else
                    nova_lista_finais t aux lista_finais
    | _ -> aux

let ler_input() = 
  let nr_elementos_S = scanf "%d\n" (fun x->x) in
  let _nr_estados_iniciais = scanf "%d\n" (fun x->x) in
  let lista_iniciais = [1] in
  let nr_estados_finais = scanf "%d\n" (fun x->x) in
  let lista_finais = leitura_finais nr_estados_finais in
  let nr_transicoes = scanf "%d\n" (fun x->x) in
  let transicoes = ref [] in 
  transicoes := (leitura_transicoes nr_transicoes []);
  let conjunto = List.rev (criar_lista nr_elementos_S) in
  let lista_restantes = ref [] in
    for i=0 to ((List.length conjunto)-1) do
        if not (List.mem (List.nth conjunto i) lista_iniciais) && not (List.mem (List.nth conjunto i) lista_finais) 
        then lista_restantes:=((List.nth conjunto i)::!lista_restantes)
    done;
  (!transicoes,lista_iniciais,lista_finais,!lista_restantes,conjunto)

(* --------------------------------------------------------- *)
  
let get_inicial transicoes lista_iniciais lista_finais lista_restantes conjunto =
let alfabeto = construir_alfabeto transicoes [] in
let novo_ref = ref [] in
    for i=0 to ((List.length alfabeto)-1) do
        let aux = (String.make 1 (List.nth alfabeto i)) in
        novo_ref:=(aux::!novo_ref)
    done;
let novo = (!novo_ref) in
let rev_alfabeto = List.rev alfabeto in 
let lista_teste_ref = ref [] in
  if List.length rev_alfabeto = 0
    then
    lista_teste_ref:=(novo)
    else
    for i=0 to ((List.length rev_alfabeto)-1) do
        lista_teste_ref := permutacoes (List.nth rev_alfabeto i) rev_alfabeto novo
    done;
let lista_teste = (!lista_teste_ref) in
let acc = teste conjunto conjunto transicoes lista_teste lista_iniciais lista_finais lista_restantes [] in
let acc_normalizado = normalize acc in
let recebido_aux = ref [] in
    for i=0 to ((List.length conjunto)-1) do
        if not (List.mem (transitividade_aux (List.nth conjunto i) acc_normalizado [(List.nth conjunto i)]) (!recebido_aux)) 
        then recebido_aux:=([transitividade_aux (List.nth conjunto i) acc_normalizado [(List.nth conjunto i)]]@(!recebido_aux))
    done;
let recebido = (!recebido_aux) in
let recebido_sem_repeticoes_ref= ref [] in
    for i=0 to ((List.length recebido)-1) do
        let h = List.nth recebido i in
        let aux = (!recebido_sem_repeticoes_ref) in
        let head = List.hd h in
        if (eliminar_repeticoes_aux head aux) then
                recebido_sem_repeticoes_ref:=([h]@aux)
    done;
let recebido_sem_repeticoes = (!recebido_sem_repeticoes_ref) in
(recebido_sem_repeticoes,alfabeto)

(* ------------------------------------------------------------- *)

let get_novo recebido_sem_repeticoes lista_iniciais lista_finais =
  let conjunto_novo_ref = ref [] in 
  for i=(List.length recebido_sem_repeticoes) downto 1 do
        conjunto_novo_ref:=(i::!conjunto_novo_ref)
    done;
let conjunto_novo = (!conjunto_novo_ref) in
let recebido_sem_repeticoes_rev = (List.rev recebido_sem_repeticoes) in
let cardinalidade_inicial_ref = ref 0 in
    for i=0 to ((List.length recebido_sem_repeticoes_rev)-1) do
        if (get_cardinalidade (List.nth recebido_sem_repeticoes_rev i) lista_iniciais)=1 then
          cardinalidade_inicial_ref:=(1+(!cardinalidade_inicial_ref))
      done;
let cardinalidade_inicial = (!cardinalidade_inicial_ref) in
let nova_lista_inicial_ref = ref [] in 
    for i=(cardinalidade_inicial) downto 1 do
          nova_lista_inicial_ref:=(i::!nova_lista_inicial_ref)
      done;
let nova_lista_inicial = (!nova_lista_inicial_ref) in
let cardinalidade_finais_ref = ref 0 in
    for i=0 to ((List.length recebido_sem_repeticoes_rev)-1) do
        if (get_cardinalidade (List.nth recebido_sem_repeticoes_rev i) lista_finais)=1 then
          cardinalidade_finais_ref:=(1+(!cardinalidade_finais_ref))
      done;
let cardinalidade_finais = (!cardinalidade_finais_ref) in
let lista_final_ref = ref [] in
    for i=0 to ((List.length (conjunto_novo))-1) do
          let h=List.nth (conjunto_novo) i in
          if not (List.mem (List.nth (conjunto_novo) i) (nova_lista_inicial)) then
          lista_final_ref:=(h::!lista_final_ref);
          x:=(1+(!x));
      done;
let lista_final = (!lista_final_ref) in
let head_inicial = List.hd lista_iniciais in
let nova_lista_iniciais = primeiro_iniciais recebido_sem_repeticoes_rev head_inicial in
let nova_lista_finais = primeiro_finais recebido_sem_repeticoes_rev lista_finais [] in
(cardinalidade_finais,lista_final,nova_lista_iniciais,nova_lista_finais)

(* ------------------------------------------------------------ *)

let get_final nova_lista_iniciais recebido_sem_repeticoes lista_iniciais lista_finais alfabeto transicoes =
let prefix_machine = [1,[nova_lista_iniciais]] in
let maquina_final = configurar_maquina prefix_machine (List.rev recebido_sem_repeticoes) 2 lista_iniciais lista_finais in
let lista_transicoes = (novas_transicoes maquina_final maquina_final alfabeto [] transicoes) in
(maquina_final,lista_transicoes)

(* ----------------------------------------------------------- *)

let print_output recebido_sem_repeticoes cardinalidade_finais maquina_final lista_finais lista_final lista_transicoes =
let() = printf "%d\n1\n" (List.length recebido_sem_repeticoes) in
let () = printar_cardinalidade cardinalidade_finais in
let () = printar_lista (List.rev (nova_lista_finais maquina_final [] lista_finais)) lista_final in
let() = printf "%d\n" (List.length (normalizar lista_transicoes)) in
let() = printar_transicoes (normalizar lista_transicoes) in
0

(* ------------------------------------------------------------ *)

let main() =
let transicoes,lista_iniciais,lista_finais,lista_restantes,conjunto = ler_input () in
 
let recebido_sem_repeticoes,alfabeto = get_inicial transicoes lista_iniciais lista_finais lista_restantes conjunto in

let cardinalidade_finais, lista_final, nova_lista_iniciais, nova_lista_finais = get_novo recebido_sem_repeticoes lista_iniciais lista_finais in

let maquina_final, lista_transicoes = get_final nova_lista_iniciais recebido_sem_repeticoes lista_iniciais lista_finais alfabeto transicoes in

let _a = print_output recebido_sem_repeticoes cardinalidade_finais maquina_final lista_finais lista_final lista_transicoes in
0

(* -------------------------------------------------------------- *)

let _b = main ()
