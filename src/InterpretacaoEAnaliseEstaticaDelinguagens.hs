module InterpretacaoEAnaliseEstaticaDelinguagens where

-- Interpretadores recebem programas como entrada. Strings como
--
-- "def inc = (lambda x . + x 1); def v = + 3 2; def resultado = inc v"
--
-- são dadas como entrada, e a saída é o resultado da execução.
--
-- Para interpretar programas, precisamos representr os programas de forma
-- abstrata, como uma árvore resultante do processo de parsing. Para
-- representar os programas de uma linguagem funcional simples,
-- temos o seguinte (identificador, número, lambda exp, aplicação, definição
-- e programa).

type Id = String
type Numero = Double
data TermoLinFun = Identifier Id
                 | Literal Numero
                 | Lambda Id TermoLinFun
                 | Aplicacao TermoLinFun TermoLinFun

data Membro = Campo Id
            | MetodoDecl Id [Id] Termo
            | Construtor [Id] Termo

data Declaracao = Def Id Termo
                | ClasseDecl Id [Membro]

type Programa = [Declaracao]


-- Aplicacao String TermoLinFun TermoLinFun
-- seria específico para aplicações binárias.
--
-- Melhor como acima do que como acima
-- type Definicao = (String,TermoLinFun)


-- Por exemplo, o programa abaixo
--
-- def inc = (lambda x . + x 1); def v = + 3 2; def resultado = inc v
--
-- seria representado como

-- def1 = Def "inc" (Lambda "x" (Aplicacao (Aplicacao (Identifier "+") (Identifier "x")) (Literal 1)))
-- def2 = Def "v" (Aplicacao (Aplicacao (Identifier "+") (Literal 3)) (Literal 2))
-- def3 = Def "resultado" (Aplicacao (Identifier "inc") (Identifier "v"))
-- prog1 = [def1,def2,def3]


-- O resultado da interpretação seria um dos seguintes, já que a
-- linguagem manipula apenas números e funções.

data ValorFun = Numero Double
              | Funcao (ValorFun -> ValorFun)
              | Excecao

instance Show ValorFun where
    show (Numero n) = show n
    show (Funcao f) = "Function definition cannot be printed!"
    show Excecao = "Excecao durante a execucao do interpretador"


-- A função que implementa o interpretador dos termos precisa receber como parâmetro um
-- ambiente, contendo as funções pré-definidas, e as definidas pelo programador.

type Ambiente = [(Id,Valor)]

-- No nosso caso, o ambiente teria apenas a definição de "+".

ambientesimples = [("+",Funcao (\x -> (Funcao (\y -> somaValorFun x y))))]

somaValorFun (Numero x) (Numero y) = Numero (x+y)
somaValorFun _ _ = Excecao


-- Temos agora duas funções de interpretação, uma para termos e uma
-- para programas. A de termos simplesmente lê o ambiente. A de programa
-- propaga alterações no ambiente, para acumular as funções definidas.

-- intTermo a (Identifier i) = getValor i a
-- intTermo a (Literal l) = Numero l
-- intTermo a (Lambda i t) = Funcao (\v -> intTermo ((i,v):a) t)
-- intTermo a (Aplicacao t1 t2) = aplica v1 v2
--                                 where v1 = intTermo a t1
--                                       v2 = intTermo a t2

-- intPrograma a [] = Excecao
-- intPrograma a [(Def i t)] = intTermo a t
-- intPrograma a ((Def i t):ds) = intPrograma ((i,v):a) ds
--                                where v = intTermo a t

-- getValor i [] = Excecao
-- getValor i ((j,v):l) = if i == j then v else getValor i l

-- aplica (Funcao f) v = f v
-- aplica _ _ = Excecao


-- Exemplo de reescrita
--
-- intPrograma as [Def "x" (Aplicacao (Aplicacao (Identifier "+")
--                                               (Identifier "x"))
--                                    (Literal 1.0))] =
-- intTermo as (Aplicacao (Aplicacao (Identifier "+")
--                                   (Identifier "x"))
--                        (Literal 1.0)) =
-- aplica v1 v2
-- aplica (Funcao (\y -> somaValorFun Excecao y)) (Numero 1.0) =
-- (\y -> somaValorFun Excecao y) (Numero 1.0) =
-- somaValorFun Excecao (Numero 1.0) =
-- Excecao
--
-- v1 = intTermo as (Aplicacao (Identifier "+") (Identifier "x"))
--    = aplica v11 v12
--    = aplica (Funcao (\x -> (Funcao (\y -> somaValorFun x y)))) Excecao
--    = Funcao (\y -> somaValorFun Excecao y)
--
-- v11 = intTermo as Identifier "+" =
-- Funcao (\x -> (Funcao (\y -> somaValorFun x y)))
--
-- v12 = intTermo as Identifier "x" =
-- getValor "x" as =
-- Excecao
--
-- v2 = intTermo as (Literal 1.0)
-- = Numero 1.0

-- Nas linguagens com atribuição (o valor de uma variável pode mudar ao
-- longo da execução), precisamos lidar com a noção de estado. Além do
-- ambiente contendo definições imutáveis, precisamos de uma noção de estado, ou
-- memória, para armazenar os valores das variáveis em um determinado ponto da
-- execução. A função de interpretação não só recebe o estado como parâmetro.
-- Ela também retorna como resultado o valor da interpretação e o novo estado,
-- contendo as alterações nos valores das variáveis.

-- Para simplifcar, temos apenas termos na linguagem, incluindo atribuições
-- (que podem representar tanto definições no sentido do interpretador anterior
-- quanto mudanças nos valores de variáveis) e composição sequencial (como o ";"
-- em Java, que entre outras coisas faz o papel da lista de definições da linguagem
-- anterior). Representamos, por simplicidade, soma como um termo específico da
-- linguagem.

data Termo = Var Id
           | Lit Numero
           | LitBool Bool   -- ADICIONADO
           | Som Termo Termo
           | Mul Termo Termo      -- ADICIONADO
           | Lam Id Termo
           | Apl Termo Termo
           | Atr Termo Termo -- Mod
           | FieldAccess Termo Id
           | Seq Termo Termo
           | While Termo Termo    -- ADICIONADO
           | New Id               -- ADICIONADO
           | InstanceOf Termo Id  -- ADICIONADO
           | If Termo Termo Termo  -- ADICIONADO
           | This
           | Call Termo Id [Termo]
           | For Termo Termo Termo Termo -- Adicionado: Feature - <Chgs3> For
           | Menor Termo Termo    -- Adicionado: Feature - <Chgs3> Menor (Auxiliar para o For)
           | MenorIgual Termo Termo -- Adicionado: Feature - <Chgs3> MenorIgual (Auxiliar para o For)
           | Igual Termo Termo    -- Adicionado: Feature - <Chgs3> Igual (Auxiliar para o For)

-- -- A aplicação "(lambda x . + x 2) 3" seria
-- termo1 = (Apl (Lam "x" (Som (Var "x") (Lit 2))) (Lit 3))

-- -- A aplicação "(lambda x . + x y) 3" seria
-- termo2 = (Apl (Lam "x" (Som (Var "x") (Var "y"))) (Lit 3))

-- -- A composição sequencial "y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
-- termo3 = (Seq (Atr "y" termo2) termo2)

-- -- A composição sequencial "y := 3 ; (lambda x . + x y) 3" seria
-- sq1 = (Seq (Atr "y" (Lit 3)) termo2)

-- -- A composição sequencial "y := 3 ; y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
-- sq2 = (Seq (Atr "y" (Lit 3)) termo3)

-- -- A composição sequencial "y := (z := 5) + z ; y := (lambda x . + x y) 3 ; (lambda x . + x y) 3" seria
-- sq3 = (Seq (Atr "y" (Som (Atr "z" (Lit 5)) (Var "z"))) termo3)



-- O resultado da interpretação seria um dos seguintes, já que a
-- linguagem manipula apenas números e funções. Como as funções
-- podem acessar e modificar variáveis que mudam de valor ao longo
-- da execução, é necessário receber não só o argumento da função,
-- e retornar seu resultado. É preciso receber também o estado atual,
-- e retornar o novo estado modificado pela execução da função.

data Valor = Num Double
           | Fun (Valor -> Estado -> Heap -> (Valor,Estado, Heap))
           | Bool Bool            -- ADICIONADO
           | Null                 -- ADICIONADO
           | Erro
           | Classe Id [Membro]  -- Adicionado
           | Ref Endereco -- Adicionado

type Estado = [(Id,Valor)]

-- Objeto: nome da classe e conjunto de estados/atributos
type Objeto = (Id, Estado)

-- Endereço: endereco de memoria na heap
type Endereco = Int

-- Heap: proximo endereço livre e conjunto de referencias e objetos
type Heap = (Endereco, [(Endereco, Objeto)])

-- Programa + main
rodarPrograma :: Programa -> Termo -> (Valor, Estado, Heap)
rodarPrograma prog main =
      let ambiente = criarAmbiente prog
      in int ambiente main [] (0, [])

criarAmbiente :: [Declaracao] -> Ambiente
criarAmbiente decls = classes ++ definicoes
  where
    classes =
      [ (nome, Classe nome membros)
      | ClasseDecl nome membros <- decls
      ]

    definicoes = 
      [ (nome, criarDef nome termo)
      | Def nome termo <- decls
      ]

    criarDef nome (Lam param body) = 
      Fun (\v e h -> int ((param,v):a0) body e h)
    criarDef nome termo = 
      Fun (\_ e h -> int a0 termo e h)
    
    a0 = classes ++ definicoes

-- int :: [(Id, Valor)] -> Termo -> [(Id, Valor)] -> (Valor, [(Id, Valor)])
int :: Ambiente -> Termo -> Estado -> Heap -> (Valor, Estado, Heap)

int a (Var x) e h = (search x (a ++ e), e, h)

int a (Lit n) e h = (Num n, e, h)

int a (LitBool b) e h = (Bool b, e, h)

int a (Som t u) e h = (somaVal v1 v2, e2, h2)
                    where (v1,e1,h1) = int a t e h
                          (v2,e2,h2) = int a u e1 h1

int a (Lam x t) e h = (Fun (\v e' h' -> int ((x,v):a) t e' h'), e, h)

int a (Mul t u) e h = (multVal v1 v2, e2, h2)
                    where (v1,e1,h1) = int a t e h
                          (v2,e2,h2) = int a u e1 h1

int a (Apl t u) e h = app v1 v2 e2 h2
                    where (v1,e1,h1) = int a t e h
                          (v2,e2,h2) = int a u e1 h1

int a (Atr left right) e h =
      -- avaliando o lado esquerdo
      case left of
            -- variavel normal
            Var x -> 
                  let 
                        (v, e1, h1) = int a right e h
                        e2 = wr (x, v) e1
                  in 
                        (v, e2, h1)
            -- atributo de objeto
            FieldAccess objeto campo ->
                  -- obtem referencia
                  let 
                        (vObj, e1, h1@(prox, objs)) = int a objeto e h
                  in 
                        case vObj of
                              -- alterando referencia 
                              Ref endereco ->
                                    -- obtem valor atributido (v)
                                    let 
                                          (v, e2, h2@(prox1, objs1)) = int a right e1 h1

                                          -- atualiza valor do campo
                                          objs2 = map (\(end, obj) -> if end==endereco
                                                                  then (end, atualizaCampo campo v obj)
                                                                  else (end, obj)
                                                      ) objs1
                                          
                                          -- atualiza heap
                                          h3 = (prox1, objs2)
                                    in 
                                          (v, e2, h3)

                              -- erro (nao eh referencia)
                              _ -> (Erro, e1, h1)
            _ -> (Erro, e, h)

int a (Seq t u) e h = int a u e1 h1
                    where (_,e1,h1) = int a t e h

int a (While cond corpo) e h =                      -- ADICIONADO
       case int a cond e h of
            (Bool True, e1, h1)  -> int a (Seq corpo (While cond corpo)) e1 h1
            (Bool False, e1, h1) -> (Null, e1, h1)
            (_, e1, h1)          -> (Erro, e1, h1)

int a (New nomeClasse) e (proximoEndereco, objetos) =
      -- procura def classe no ambiente
      case lookup nomeClasse a of
            -- encontra
            Just (Classe _ membros) ->
                  let
                        -- inicializa campos
                        camposIniciais = [(nome, Null) | Campo nome <- membros]
                        -- cria objeto
                        objeto = (nomeClasse, camposIniciais)
                        -- incorpora objeto na heap
                        objs = (proximoEndereco, objeto) : objetos
                        -- atualiza heap
                        heap = (proximoEndereco+1, objs)
                  in 
                        (Ref proximoEndereco, e, heap)
            -- erro
            Nothing -> (Erro, e, (proximoEndereco, objetos))

int a (InstanceOf objeto nomeClasse) e h =               -- ADICIONADO
    let 
      -- busca referencia a objeto
      (resultado, e1, h1@(proximoEndereco, objetos)) = int a objeto e h
    in case resultado of
            Ref endereco -> 
                  case lookup endereco objetos of
                        -- compara nome da classe
                        Just (nomeClasseEncontrada, _) -> (Bool (nomeClasse == nomeClasseEncontrada), e1, h1)
                        Nothing -> (Erro, e1, h1)
            _ -> (Erro, e1, h1)

int a (If cond t1 t2) e h =
    case int a cond e h of
        (Bool True, e1, h)  -> int a t1 e1 h
        (Bool False, e1, h) -> int a t2 e1 h
        (_, e1, h)          -> (Erro, e1, h)

int a (FieldAccess objeto campo) e h =
      let (vObj, e1, h1@(prox, objs)) = int a objeto e h
      in case vObj of
            Ref endereco ->
                  case lookup endereco objs of
                        Just (_, campos) ->
                              case lookup campo campos of
                                    Just valorCampo -> (valorCampo, e1, h1)
                                    Nothing -> (Erro, e1, h1)
                        Nothing -> (Erro, e1, h1)
            _ -> (Erro, e1, h1)

-- Implementação do For, que é uma estrutura de repetição com inicialização,
-- condição e incremento.

int a (For init cond incr corpo) e h = 
    let (_, e1, h1) = int a init e h        -- inicialização  
        loop e' h' = case int a cond e' h' of  -- função do loop
                    (Bool True, e'', h'') ->
                        let
                            (_, e''', h''') = int a corpo e'' h''  -- executa o corpo
                            (_, e'''', h'''') = int a incr e''' h''' -- executa o incremento
                        in loop e'''' h''''
                    (Bool False, e'', h'') -> (Null, e'', h'') -- finaliza o loop
                    (_, e'', h'') -> (Erro, e'', h'') -- condição inválida
    in loop e1 h1

-- Implementação dos operadores de comparação
-- Menor, MenorIgual e Igual, que são usados no For
int a (Menor t1 t2) e h =
    case (v1, v2) of
        (Num n1, Num n2) -> (Bool (n1 < n2), e2, h2)
        _ -> (Erro, e2, h2)
    where (v1, e1, h1) = int a t1 e h
          (v2, e2, h2) = int a t2 e1 h1

int a (MenorIgual t1 t2) e h =
    case (v1, v2) of
        (Num n1, Num n2) -> (Bool (n1 <= n2), e2, h2)
        _ -> (Erro, e2, h2)
    where (v1, e1, h1) = int a t1 e h
          (v2, e2, h2) = int a t2 e1 h1

int a (Igual t1 t2) e h =
    case (v1, v2) of
        (Num n1, Num n2) -> (Bool (n1 == n2), e2, h2)
        (Bool b1, Bool b2) -> (Bool (b1 == b2), e2, h2)
        _ -> (Erro, e2, h2)
    where (v1, e1, h1) = int a t1 e h
          (v2, e2, h2) = int a t2 e1 h1

-- search :: Eq a => a -> [(a, Valor)] -> Valor

search i [] = Erro
search i ((j,v):l) = if i == j then v else search i l

-- somaVal :: Valor -> Valor -> Valor

somaVal (Num x) (Num y) = Num (x+y)
somaVal _ _ = Erro

-- multVal :: Valor -> Valor -> Valor
multVal (Num x) (Num y) = Num (x * y)
multVal _ _ = Erro


-- app :: Valor -> Valor -> Estado -> Heap -> (Valor,Estado,Heap)

app (Fun f) v e h = f v e h
app _ _ e h = (Erro, e, h)

-- wr :: Eq a => (a, t) -> [(a, t)] -> [(a, t)]

wr (i,v) [] = [(i,v)]
wr (i,v) ((j,u):l) = if (i == j) then (j,v):l else (j,u):(wr (i,v) l)

-- atualizaCampo :: Id -> Valor -> Objeto -> Objeto

atualizaCampo campoAlvo novoValor (nomeObj, campos) = 
      -- mapeia os pares de nome e valor dos campos
      (nomeObj, map (\(nomeCampo,valorCampo) -> 
            -- campo desejado
            if nomeCampo==campoAlvo 
                  -- troca valor
                  then (nomeCampo,novoValor) 
                  -- mantem
                  else (nomeCampo,valorCampo)) campos)


-- Chamando o interpretador com o ambiente e a memória vazios.

at t = int [] t [] (0, [])

-- Se soma não fosse um termo específico da linguagem:
-- at t = int [("+",Fun (\x -> \e -> (Fun (\y -> \e2 -> (somaVal x y,e2),e)))] t []

instance Show Valor where
   show (Num x) = show x
   show (Bool b) = show b            -- ADICIONADO
   show Null = "null"                -- ADICIONADO
   show (Ref e) = show e  -- Mod
   show Erro = "Erro"
   show (Fun _) = "Função"
   show (Classe nome _) = "<classe " ++ nome ++ ">"


-- Termos usados nos testes de if
termoIf1 = If (LitBool True) (Lit 10) (Lit 20)   -- Esperado: 10
termoIf5 = If (InstanceOf (New "Pessoa") "Pessoa") (Lit 1) (Lit 0)  -- Esperado: 1
termoIf7 = If (Lit 3) (Lit 1) (Lit 2) -- Esperado: Erro (condição não booleana)

programaComClasse :: [Declaracao]
programaComClasse = [ClasseDecl "Pessoa" [Campo "nome", Campo "idade"]]

testarIf :: IO ()
testarIf = do
    putStrLn "Testes do termo If: "

    let (v1, _, _) = at termoIf1  -- Este funciona sem ambiente
    putStrLn ("If True then 10 else 20 = " ++ show v1)

    -- Este precisa do ambiente com classe definida
    let (v5, _, _) = rodarPrograma programaComClasse termoIf5
    putStrLn ("If InstanceOf Pessoa Pessoa then 1 else 0 = " ++ show v5)

    let (v7, _, _) = at termoIf7  -- Teste de erro funciona sem ambiente
    putStrLn ("If 3 then 1 else 2 = " ++ show v7)

-- Testes para o For
exemploFor :: Termo
exemploFor = For (Atr (Var "i") (Lit 1))           -- init: i = 1
                       (MenorIgual (Var "i") (Lit 10))  -- cond: i <= 10
                       (Atr (Var "i") (Som (Var "i") (Lit 1)))  -- incr: i = i + 1
                       (Atr (Var "soma") (Som (Var "soma") (Var "i")))  -- corpo: soma += i

testarFor :: IO ()
testarFor = do
    putStrLn "Testando loop For:"   

    let estadoInicial = [("soma", Num 0)] -- Estado inicial com a soma = 0
    let heapInicial = (0, [])             -- Heap inicial vazio

    let (resultado, estadoFinal, heapFinal) = int [] exemploFor estadoInicial heapInicial -- Executa o For
    
    putStrLn $ "Resultado do For: " ++ show resultado
    
    case search "soma" estadoFinal of
        Num n -> putStrLn $ "Soma final: " ++ show n
        _ -> putStrLn "Erro ao obter o valor da soma"

