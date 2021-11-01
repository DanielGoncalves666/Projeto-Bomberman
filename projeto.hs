{-
Nomes: Daniel Gonçalves
       Marcus Vinícius Torres
       Lorenzo Machado Burgos
-}

{-
Funcionalidades:
construirTabuleiro :: [Celula] -> Int -> Int -> Tabuleiro
imprimirTabuleiro :: Tabuleiro -> [[Char]]

adicionarListaJogador :: ListaJogadores -> Int -> (Int,Int) -> ListaJogadores
alterarListaJogador :: ListaJogadores -> Jogador -> ListaJogadores
retirarListaJogador :: ListaJogadores -> Cell -> ListaJogadores
moverJogador :: Tabuleiro -> Jogador -> Char -> (Tabuleiro,Jogador)
    N, O, S, L
adicionarBombaNaLista :: ListaBombas -> Bomba -> ListaBombas
removerBombaDaLista :: ListaBombas -> (Int,Int) -> ListaBombas
obterTemporizadorBomba :: Bomba -> Int
colocarBomba :: Tabuleiro -> Jogador -> ListaBombas -> (Tabuleiro, Jogador, ListaBombas)
arremesso :: Tabuleiro -> Jogador -> ListaBombas -> (Tabuleiro,ListaBombas)
explodirBombas :: Tabuleiro -> ListaJogadores -> ListaBombas -> (Tabuleiro,ListaJogadores, ListaBombas)
explosao :: Tabuleiro -> ListaJogadores -> Bomba -> (Tabuleiro,ListaJogadores)
detectarFim :: ListaJogadores -> Bool 
-}

-------------------------------------------------------------- Código Funcional Impuro ---------------------------------------------------------

exemplo = [ [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra],
            [Pedra], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Pedra],
            [Pedra], [Jogador 1, Grama], [Pedra], [Grama], [Pedra], [Grama], [Pedra], [Grama], [Grama], [Pedra],
            [Pedra], [Grama], [Parede,Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Pedra],
            [Pedra], [Grama], [Grama], [Grama], [Presente "Bomba",Grama], [Grama], [Grama], [Grama], [Grama], [Pedra],
            [Pedra], [Parede, Presente "Arremesso",Grama], [Pedra], [Parede,Grama], [Pedra], [Grama], [Pedra], [Grama], [Grama], [Pedra],
            [Pedra], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Pedra],
            [Pedra], [Bomba,Grama], [Pedra], [Grama], [Pedra], [Pedra], [Pedra], [Grama], [Grama], [Pedra],
            [Pedra], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Grama], [Pedra],
            [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra], [Pedra]]
-- Nas funções deste projeto estamos usando coordenadas do plano cartesiano. Ou seja, x indica a coluna e y indica a linha.
-- Muita atenção com este detalhe. Por exemplo, a posição no canto inferior esquerdo é x = 0 e y = 9


listaJogadores :: ListaJogadores
listaJogadores = [(1,(1,2),'S', capacidadeBasica)]

capacidadeBasica :: Capacidades
capacidadeBasica = [("Patins",1), ("Alcance",1),("Bomba",1),("Arremesso",1)]

iniciarTabuleiro :: (Tabuleiro, ListaJogadores, ListaBombas)
iniciarTabuleiro = (construirTabuleiro exemplo 10 10, listaJogadores, [])

-- tempoDeExplosão definido lá embaixo, junto com as bombas
main = do
    menu tab lj lb
    where (tab, lj, lb) = iniciarTabuleiro

menu :: Tabuleiro -> ListaJogadores -> ListaBombas -> IO ()
menu tabuleiro listaJ listaB = do
    let nlB = diminuirTemporizadorBombas listaB
        (tabN, listJN, listaBN) = explodirBombas tabuleiro listaJ nlB   -- realiza o processo de explosão de todas as bombas 
        op = detectarFim listJN -- true se o fim de jogo foi alcançada
    
    imprimirTabuleiroIO tabN 
    
    putStrLn "Escolha o que fazer: n = Norte, s = Sul, l = Leste, o = Oeste, b = ColocarBomba, a = Arremessar, p = Sair"
    opcao <- getChar

    if op then putStrLn "Fim de Jogo."
    else if opcao == 'p' then return ()
                    else if opcao == 'b' then 
                         let (tab', lj, lb) = colocarBomba tabN (head listJN) listaBN
                         in menu tab' [lj] lb
                    else if opcao == 'a' then
                         let (tab', lb) = arremesso tabN (head listJN) listaBN
                         in menu tab' listJN lb
                    else let (tab', lj) = case opcao of
                                                     'n' -> moverJogador tabN (head listJN) 'N'
                                                     's' -> moverJogador tabN (head listJN) 'S'
                                                     'l' -> moverJogador tabN (head listJN) 'L'
                                                     'o' -> moverJogador tabN (head listJN) 'O'
                                                     _ -> (tabN, head listJN)
                        in menu tab' [lj] listaBN
    return ()


-------------------------------------------------------- Código Funcional Puro ----------------------------------------------------------------

data Cell = Grama | Pedra | Presente String | Parede | Bomba | Jogador Int deriving(Show,Eq)
-- tanto grama como pedra, precisam estar no fundo da pilha, obrigatoriamente
-- Ambos presentes tem mesma prioridade na pilha
-- O jogador só pode estar acima da grama, assim como a bomba
-- parede so pode estar no fundo da pilha sobre grama ou sobre os presentes
    -- Minha análise inicial diz que a ordenação na estrutura não vai dar certo, precisaremos do auxilia de funções
    -- a estrutura não pode ter Ord pois não há uma única maneira de se comparar prioridade, existem elementos que tem mesma prioridade.
    -- Adição de presente bomba.
-- A tupla definida junto com Bomba é usada para armazenar informaações importantes
    -- (ID do jogador que plantou, Alcance da explosão, Tempo até explodir)

type Celula = [Cell]
type Linha = [Celula]
type Tabuleiro = [Linha]

--verifica se a numeração do jogador é válida
-- a última linha é pra considerar todos os casos, incluindo todos os valores para Cell
properJogador :: Cell -> Bool
properJogador (Jogador n) = n > 0 && n < 5
properJogador _ = False

-- não há verificação de validade das strings do presente pois não sabemos quais presentes podem existir
ehPresente :: Cell -> Bool
ehPresente (Presente n) = True
ehPresente _ = False

existeJogador :: Celula -> Bool 
existeJogador [] = False
existeJogador (l:ls)
    | properJogador l = True 
    | otherwise = existeJogador ls

existePresente :: Celula -> Bool 
existePresente [] = False 
existePresente (l:ls)
    | ehPresente l = True
    | otherwise = existePresente ls

existeNaCelula :: Celula -> Cell -> Bool
existeNaCelula [] _ = False
existeNaCelula (l:ls) c
    | l == c = True
    | otherwise = existeNaCelula ls c

-------------------------------------------------------------------------------------------------------------------------------------

{-

a = construirTabuleiro exemplo 10 10
a1 = imprimirTabuleiro a

(b,j1) = moverJogador a jogador1 'N'
(b,j1,lb) = colocarBomba a jogador1 lb
(b,lbn) = arremesso a jogador3 lb

b1 = imprimirTabuleiro b

obterCelula b 1 2

(c,listaJogadores) = explosao a listaJogadores (2,(7,7),2,0)

-}

-------------------------------------------------------------------------------------------------------------------------------------

--cada lista dentro da lista se refere a uma célula
construirTabuleiro :: [Celula] -> Int -> Int -> Tabuleiro
construirTabuleiro t lin col
    | length t /= lin * col = []
    | verificarTabuleiro t = construirLinha t lin col
    | otherwise = []

verificarTabuleiro :: [Celula] -> Bool
verificarTabuleiro [] = True
verificarTabuleiro l
    | ehValido (head l) = verificarTabuleiro (tail l)
    | otherwise = False

construirLinha :: [Celula] -> Int -> Int -> Tabuleiro
construirLinha _ 0 _ = []
construirLinha l lin col = take col l : construirLinha (drop col l) (lin - 1) col

-- consideramos que os indices começam em 0.
obterCelula :: Tabuleiro -> Int -> Int -> Celula
obterCelula t x y =  (t !! y) !! x
    --primeiro ocorre a seleção da linha, que é indicada pelo y e so entao ocorre a seleção da coluna, indicada pelo x
    --tomar cuidado para nao tentar pegar celulas invalidas

editarTabuleiro :: Tabuleiro -> Celula -> Int -> Int -> Tabuleiro
editarTabuleiro [] _ _ _ = []
editarTabuleiro (th:ts) c x 0 = editarLinha th c x : ts
editarTabuleiro (th:ts) c x y = th : editarTabuleiro ts c x (y - 1)

editarLinha :: Linha -> Celula -> Int -> Linha
editarLinha [] _ _ = []
editarLinha (l:ls) c 0 = c : ls
editarLinha (l:ls) c x = l : editarLinha ls c (x - 1)

-- grama -> presentes e bomba e jogador 
-- parede na base, sobre grama ou sobre presente
-- pedra na base
ehValido :: Celula -> Bool
ehValido [] = True    -- Célula vazia
ehValido [x] = x == Grama || x == Parede || x == Pedra
ehValido p@(x:y:xs)
    | tamanho == 2 = (y:xs) == [Grama] && x /= Pedra
    | tamanho == 3 = (x == Parede && ehPresente y && compararFim) || (pJx && pJy && compararFim)
    | otherwise = pJx && pJy && ehValido (y:xs)
    where tamanho = length p
          compararFim = xs == [Grama]
          pJx = properJogador x
          pJy = properJogador y


remover :: Celula -> Cell -> Celula
remover [] (Jogador n) = []
remover [] item = []
remover c@(x:xs) (Jogador n)    -- Recursiva, pois não há local certo para o Jogador estar em jogadores
    | x == Jogador n = xs
    | otherwise = x : remover xs (Jogador n)
remover c@(x:xs) item           
    | x == item = xs
    | otherwise = x : remover xs item


adicionar :: Celula -> Cell -> Celula
adicionar [] _ = []
adicionar l@(x:xs) item
    | x == Pedra || x == Parede || x == Bomba = l
    | properJogador x && item == Bomba = l
    | properJogador item || item == Bomba = item:l
    | otherwise = l

imprimirTabuleiroIO :: Tabuleiro -> IO ()
imprimirTabuleiroIO [] = do
    return ()
imprimirTabuleiroIO (t:ts) = do
    putStrLn $ imprimirLinha t
    imprimirTabuleiroIO ts
    return ()

imprimirTabuleiro :: Tabuleiro -> [[Char]]
imprimirTabuleiro [] = []
imprimirTabuleiro (t:ts) = imprimirLinha t : imprimirTabuleiro ts


imprimirLinha :: Linha -> String
imprimirLinha [] = []
imprimirLinha l
    | null (head l) = ' ' : imprimir
    | topo == Grama = 'G' : imprimir
    | topo == Pedra = '-' : imprimir
    | topo == Parede = 'P' : imprimir
    | topo == Bomba = 'B' : imprimir
    | ehPresente topo = 'o' : imprimir
    | properJogador topo = 'J' : imprimir
    | otherwise = 'H' : imprimir
    where topo = head (head l)
          imprimir = imprimirLinha (tail l)



-------------------------------------------------------------------------------------------------------------------------------------

-- Inteiro para indicar o número do jogador
-- Tupla de inteiros que indica a posição na matriz
-- Caractere que indica a direção que o jogador está olhando
-- Tupla de três elementos, cada uma contendo uma string e um inteiro.
    --Patins, Bomba, alcance e arremesso

type Capacidades = [(String,Int)] --alteramos de tupla para lista pois isso permite a adição de novos presentes sem extensiva alteração do codigo
type Jogador = (Int, (Int, Int), Char, Capacidades)

-- No programa principal nossos jogadores estarão armazenados em uma lista.
type ListaJogadores = [Jogador]

-- as coordenadas não foram verificas se são válidas
adicionarListaJogador :: ListaJogadores -> Int -> (Int,Int) -> ListaJogadores
adicionarListaJogador [] id xy = [(id,xy,'N',[("Patins",1),("Alcance",1),("Bomba",1)])]
adicionarListaJogador li@(l:ls) id xy
    | idAtual == id = li                -- já existe jogador com esse ID na lista
    | otherwise = adicionarListaJogador ls id xy
    where idAtual = obterNumeroJogador l

-- vai ser usada quando o jogador morre por explosão ou por ter caido em um buraco
-- essa função, por questões da disponibilidade da lista de jogadores, deverá verificar se um jogador específico tem
    --coordenadas (-1,-1) e alterá-lo, caso contrário, apenas uma substituição é necesária
alterarListaJogador :: ListaJogadores -> Jogador -> ListaJogadores
alterarListaJogador [] _ = []                 -- o jogador precisa já existir para podermos alterá-lo
alterarListaJogador (l:ls) j
    | id == idCabeca = if (x,y) == (-1,-1) then ls     -- remove o jogador
                                           else j : ls
    | otherwise = l : alterarListaJogador ls j
    where id = obterNumeroJogador j
          idCabeca = obterNumeroJogador j
          (x,y) = obterCoordenadas j

retirarListaJogador :: ListaJogadores -> Cell -> ListaJogadores
retirarListaJogador [] _ = []
retirarListaJogador (l:ls) (Jogador id)
    | idCabeca == id = ls
    | otherwise = l : retirarListaJogador ls (Jogador id)
    where idCabeca = obterNumeroJogador l
retirarListaJogador l _ = l
 
obterNumeroJogador :: Jogador -> Int
obterNumeroJogador (n,_,_,_) = n

obterCoordenadas :: Jogador -> (Int,Int)
obterCoordenadas (_,xy,_,_) = xy

alterarCoordenadas :: Jogador -> (Int,Int) -> Jogador
alterarCoordenadas (a,_,c,d) xy = (a,xy,c,d)

obterDirecao :: Jogador -> Char
obterDirecao (_,_,c,_) = c

valoresDirecao :: Char -> (Int,Int)
valoresDirecao c
    | c == 'N' = (0,-1)
    | c == 'L' = (1,0)
    | c == 'O' = (-1,0)
    | c == 'S' = (0,1)
    | otherwise = (0,0)


alterarDirecao :: Jogador -> Char -> Jogador
alterarDirecao (a,b,_,d) c = (a,b,c,d)

-- (Patins,1) --> velocidade
-- (Bomba,1) --> Quantidade de bombas
-- (Alcance,1) --> Alcance da explosão
obterCapacidades :: Jogador -> Capacidades
obterCapacidades (_,_,_,t) = t

alterarCapacidades :: Jogador -> Capacidades -> Jogador
alterarCapacidades (a,b,c,_) t = (a,b,c,t)

obterPresente :: Capacidades -> Cell -> Int
obterPresente [] (Presente _) = 0  -- Indica que aquele presente não existe
obterPresente p (Presente s)
    | fst cabeca == s = snd cabeca
    | otherwise = obterPresente (tail p) (Presente s)
    where cabeca = head p
obterPresente _ _ = -1 -- quando não for um presente

-- Adiciona 1 ao presente especificado
incrementarPresente :: Capacidades -> Cell -> Capacidades
incrementarPresente [] (Presente s) = [(s,1)]                            --adiciona o novo presente à lista
incrementarPresente p (Presente s)
    | fst cabeca == s = (s, snd cabeca + 1) : cauda
    | otherwise = cabeca : incrementarPresente cauda (Presente s)
    where cabeca = head p
          cauda = tail p
incrementarPresente p _ = p -- se não for um presente, não acontece nada

-- Diminui 1 ao presente especificado
decrementarPresente :: Capacidades -> Cell -> Capacidades
decrementarPresente [] _ = []
decrementarPresente p (Presente s)
    | fst cabeca == s && snd cabeca == 0 = p                    -- Se encontrarmos o presente e ele já for zero, não acontece nada
    | fst cabeca == s = (s, snd cabeca - 1) : cauda
    | otherwise = cabeca : decrementarPresente cauda (Presente s)
    where cabeca = head p
          cauda = tail p
decrementarPresente p _ = p

podeSeMover :: Jogador -> Bool
podeSeMover j = xy /= (-1,-1)
    where xy = obterCoordenadas j

moverJogador :: Tabuleiro -> Jogador -> Char -> (Tabuleiro,Jogador)
moverJogador t j c
    | c == 'N' && movimentoValido t (x, y - 1) = moverAux t j 0 (-1) 'N'
    | c == 'L' && movimentoValido t (x + 1, y) = moverAux t j 1 0 'L'
    | c == 'S' && movimentoValido t (x, y + 1) = moverAux t j 0 1 'S'
    | c == 'O' && movimentoValido t (x - 1, y) = moverAux t j (-1) 0 'O'
    | otherwise = (t,novaDirecao)
    where (x,y) = obterCoordenadas j
          novaDirecao = alterarDirecao j c

-- xarg e yarg são as variáveis de alteração. Elas variam entre -1, 0 e 1.
moverAux :: Tabuleiro -> Jogador -> Int -> Int -> Char -> (Tabuleiro,Jogador)
moverAux t j xarg yarg c
    | null cDe = ( editarTabuleiro t remOrigem x y, jogadorNoBuraco)  -- Morte
    | ehPresente d = (tabuleiroFinalPresente, jogadorPresenteFinal)  -- Pegar Presente
    | otherwise = (tabuleiroFinal, jogadorFinal)                     -- Movimentação normal
    where n = obterNumeroJogador j              -- número do jogador
          remOrigem = remover cO (Jogador n)    -- remove o jogador da sua célula de origem
          adiDestino = adicionar cD (Jogador n) -- adiciona o jogador na célula de destino
          destino = adicionar (remover cD d) (Jogador n) -- remove o presente da célula de destino e adiciona o jogador
          tabuleiroFinal = editarTabuleiro (editarTabuleiro t remOrigem x y) adiDestino (x + xarg) (y + yarg) -- calcula o tabuleiro final para movimentação normal
          tabuleiroFinalPresente = editarTabuleiro (editarTabuleiro t destino (x + xarg) (y + yarg)) remOrigem x y -- calcula o tabuleiro final para movimentação normal com captura de presente
          cDe = obterCelula t (x + xarg) (y + yarg) --celula destino -> usado apenas para verificar se a celula é um buraco 
          cD@(d:ds) = obterCelula t (x + xarg) (y + yarg)  -- celula destino
          cO@(o:os) = obterCelula t x y        -- celula origem
          (x,y) = obterCoordenadas j           -- coordenadas iniciais do jogador
          capacAlt = incrementarPresente (obterCapacidades j) d  --adiciona o presente pego
          jogadorPresenteFinal = alterarCoordenadas (alterarDirecao (alterarCapacidades j capacAlt) c) (x + xarg, y + yarg) --jogador se movimenta e pega presente
          jogadorNoBuraco = alterarCoordenadas (alterarDirecao j c) (-1,-1)   --jogador cai no buraco
          jogadorFinal = alterarCoordenadas (alterarDirecao j c) (x + xarg, y + yarg)   --jogador se moviementa normal

-- tabuleiro e a celula para a qual o jogador se movimentará
movimentoValido :: Tabuleiro -> (Int, Int) -> Bool
movimentoValido t (x,y)
    | null c = True
    | cabeca == Grama || ehPresente cabeca || properJogador cabeca = True
    | otherwise = False
    where c =  obterCelula t x y
          cabeca = head c
-- Ele so se movimenta para celulas com grama, presente, jogador ou buraco

{- 
    Jogador só se move para células cujo topo da pilha é
        Grama
        Algum presente
        Outro Jogador
        Buraco
            Neste caso o jogador cairá no buraco e morrerá
    Caso o jogador não possa se mover para aquela posição do tabuleiro, a variável que indica para que lado ele tá olhando precisa ser alterada
-}



--------------------------------------------------------------------------------------------------------------------------------------

type ListaBombas = [Bomba]

type Bomba = (Int, (Int, Int), Int, Int)

adicionarBombaNaLista :: ListaBombas -> Bomba -> ListaBombas
adicionarBombaNaLista l b = b : l

-- remove a bomba presente nas coordenadas dadas
removerBombaDaLista :: ListaBombas -> (Int,Int) -> ListaBombas
removerBombaDaLista [] _ = []
removerBombaDaLista (l:ls) xy
    | lxy == xy = ls
    | otherwise = l : removerBombaDaLista ls xy
    where lxy = obterCoordenadasBomba l

obterBombaNaCoordenada :: ListaBombas -> (Int,Int) -> Bomba
obterBombaNaCoordenada [] _ = (-1,(-1,-1),-1,-1)
obterBombaNaCoordenada (l:ls) xy
    | xy == lxy = l
    | otherwise = obterBombaNaCoordenada ls xy
    where lxy = obterCoordenadasBomba l

-- Inteiro que indica o ID do jogador que plantou a bomba (ver o caso que o jogador já morreu)
-- Tupla de dois inteiros, que indica a posição da bomba
-- Inteiro que indica a quantidade de casas que a explosão chega, por padrão sendo 1
-- Inteiro que indica o tempo até a explosão, explode quando chegar em zero. Podemos também usar como um indicador de quando a bomba vai explodir
    -- podemos ligar cada bomba ao jogador que a colocou, retirando 1 do marcador de bombas. Quando o marcador for 0, não poderá colocar novas bombas.
        -- Quando uma bomba explodir, aumentamos novamente o marcador de bombas do jogador.

tempoDeExplosão = 10 --tempo até a bomba explodir, alterável

obterIDBomba :: Bomba -> Int
obterIDBomba (a,_,_,_) = a

obterCoordenadasBomba :: Bomba -> (Int,Int)
obterCoordenadasBomba (_,b,_,_) = b

obterAlcanceBomba :: Bomba -> Int
obterAlcanceBomba (_,_,c,_) = c

obterTemporizadorBomba :: Bomba -> Int
obterTemporizadorBomba (_,_,_,d) = d

diminuirTemporizadorBombas :: ListaBombas -> ListaBombas
diminuirTemporizadorBombas [] = []
diminuirTemporizadorBombas (bomb:bs) = (a,b,c, d - 1) : diminuirTemporizadorBombas bs
    where (a,b,c,d) = bomb

colocarBomba :: Tabuleiro -> Jogador -> ListaBombas -> (Tabuleiro, Jogador, ListaBombas)
colocarBomba t j lb
    | qtdBombas == 0 = (t,j,lb) -- o jogador não pode colocar mais nenhuma bomba no momento
    | existeNaCelula c Bomba = (t,j,lb)     -- o jogador já colocou uma bomba na célula
    | otherwise = (novoTabuleiro, novoJogador,novaListaBombas)
    where n = obterNumeroJogador j      -- ID do jogador
          (x,y) = obterCoordenadas j    -- coordenadas do jogador
          capac = obterCapacidades j    -- Lista de capacidades (presentes) do jogador
          alc = obterPresente capac (Presente "Alcance") --alcande das bombas do jogador
          qtdBombas = obterPresente capac (Presente "Bomba") --quantidade de bombas que o jogador ainda pode colocar
          c = obterCelula t x y         -- celula onde o jogador esta
          colocar = adicionarBomba c -- coloca a bomba na célula que o jogador está.
          novoTabuleiro = editarTabuleiro t colocar x y -- altera o tabuleiro para incluir a celula com a bomba
          novaCapacidade = decrementarPresente capac (Presente "Bomba") -- diminui a quantidade de bombas disponíveis
          novoJogador = alterarCapacidades j novaCapacidade -- altera o jogador
          novaListaBombas = adicionarBombaNaLista lb (n,(x,y),alc,tempoDeExplosão)
--OBS: o jogador pode estar acima da bomba, mas não pode ser mover para uma célula que possua bomba.


-- Celula onde a bomba será adicionada
adicionarBomba :: Celula -> Celula
adicionarBomba [] = []
adicionarBomba [Grama] = [Bomba, Grama] -- colocação padrão
adicionarBomba c@(x:xs)
    | properJogador x = x : adicionarBomba xs -- colocará a bomba abaixo de todos os jogadores na célula, incluindo o que botou a bomba
    | otherwise = c                               -- caso o topo não seja um jogador, significa que não dá pra colocar bomba ali


-- precisaremos de uma função para decrementar ou alterar o relógio inteiro das bombas
    -- para alterar isso ou verificar se uma bomba está pra explodir

--------------------------------------------------------------------------------------------------------------------------------------------


-- tendo o tabuleiro e os dados do jogador, recalcula a posição da bomba, se possível.
arremesso :: Tabuleiro -> Jogador -> ListaBombas -> (Tabuleiro,ListaBombas)
arremesso t j lb
    | existeNaCelula c Bomba && valorArremesso > 0 = (tf,lba)
    | otherwise = (t,lb)
    where (x,y) = obterCoordenadas j
          (xarg, yarg) = valoresDirecao $ obterDirecao j  -- obtém os modificadores de posição de acordo com a direção que o jogador olha
          valorArremesso = obterPresente (obterCapacidades j) (Presente "Arremesso") -- obtém o valor do presente Arremesso
          c = obterCelula t (x + xarg) (y + yarg)         -- obtém a célula adjascente à direção que o jogador estiver olhando
          ba = obterBombaNaCoordenada lb (x + xarg, y + yarg)                   -- obtém a bomba na posição que o jogador está olhando
          nb = arremessar tsb ba (xarg,yarg) valorArremesso                     -- bomba antiga atualizada
          (xn, yn) = obterCoordenadasBomba nb                                   -- obtém as coordenada de onde a bomba deve ser inserida
          cn = obterCelula t xn yn                                              -- obtém a célula onde a bomba será inserida
          tsb = editarTabuleiro t (remover c Bomba) (x + xarg) (y + yarg)        -- tabuleiro sem bomba
          tf = editarTabuleiro tsb (adicionar cn Bomba) xn yn                   -- tabuleiro final
          lbr = removerBombaDaLista lb (x + xarg, y + yarg)                     -- lista com a bomba removida
          lba = adicionarBombaNaLista lbr nb                                                            -- lista com a nova bomba adicionada

arremessar :: Tabuleiro -> Bomba -> (Int, Int) -> Int -> Bomba
arremessar t (a,(x,y),c,d) (xarg, yarg) qtd
    | qtd == 0 = (a,(x,y),c,d)
    | valida = (a,(x,y),c,d)    -- caso a célula atual esteja ocupada a bomba é colocada na anterior
    | otherwise = arremessar t (a, (x + xarg, y + yarg),c,d) (xarg,yarg) (qtd - 1)
    where cel = obterCelula t (x + xarg) (y + yarg)         -- obtém a célula adjascente à direção que o jogador estiver olhando
          valida = existeNaCelula cel Pedra || existeNaCelula cel Parede || existeNaCelula cel Bomba || existeJogador cel || existePresente cel


--------------------------------------------------------------------------------------------------------------------------------------------
-- temos certeza que não é a melhor forma de fazer essa função
-- realiza a explosão das bombas em uma lista
explodirBombas :: Tabuleiro -> ListaJogadores -> ListaBombas -> (Tabuleiro,ListaJogadores, ListaBombas)
explodirBombas t lj [] = (t,lj,[])
explodirBombas t lj (bomb:bs)
    | temporizador == 0 = (tn2, ljn2, lbn2)
    | otherwise = (tn1, ljn1, bomb:lbn1)
    where temporizador = obterTemporizadorBomba bomb
          (tn1, ljn1, lbn1) = explodirBombas t lj bs           -- quando a bomba verificada não explode
          (te, lje) = explosao t lj bomb                    -- realiza a explosao da bomba
          (tn2, ljn2, lbn2) = explodirBombas te lje bs           -- quando a bomba verificada EXPLODE


-- tendo o tabuleiro e os dados do jogador que plantou a bomba, recalcula o tabuleiro
-- talvez tenhamos que criar um tipo de dado bomba

explosao :: Tabuleiro -> ListaJogadores -> Bomba -> (Tabuleiro,ListaJogadores)
explosao t j b = (tabf,lisjf)
    where (x,y) = obterCoordenadasBomba b
          alc = obterAlcanceBomba b
          tab0 = editarTabuleiro t (remover (obterCelula t x y) Bomba) x y -- remove a Bomba da célula de origem
          (tab1, lisj1) = explosãoDeslocar tab0 j (x,y) 0 0 1 -- própria bomba, caso tenha algum jogador
          (tab2, lisj2) = explosãoDeslocar tab1 lisj1 (x,y) 0 (-1) alc -- Norte
          (tab3, lisj3) = explosãoDeslocar tab2 lisj2 (x,y) 1 0 alc -- Leste
          (tab4, lisj4) = explosãoDeslocar tab3 lisj3 (x,y) 0 1 alc -- Sul
          (tabf, lisj5) = explosãoDeslocar tab4 lisj4 (x,y) (-1) 0 alc -- Oeste
          id = obterIDBomba b 
          lisjf = retornarCapacidadeBomba lisj5 id


    -- A bomba tem que ter chegado no tempo 0 para que possa explodir
    -- Uma verificação de explosão é setada para cada uma das quatro direções, de acordo com o alcance da explosão
        -- Não acontece nada à
            -- Células vazias e com grama
            -- Células com Pedra ou outra bomba permanecem inalteradas
        -- Células com parede no topo tem seu topo retirado
        -- Células com presente no topo tem o presente destruído
        -- Células com jogador, a explosão matará os jogadores

-- quando a bomba explodir retorna a capacidade de colocar bomba ao jogador que a colocou
retornarCapacidadeBomba :: ListaJogadores -> Int -> ListaJogadores
retornarCapacidadeBomba [] _ = []
retornarCapacidadeBomba (j:js) id
    | obterNumeroJogador j == id = novoJogador:js
    | otherwise = j : retornarCapacidadeBomba js id
    where capac = obterCapacidades j
          capacN = incrementarPresente capac (Presente "Bomba")
          novoJogador = alterarCapacidades j capacN

-- Função auxiliar deslocar, que será responsável pelo deslocamento da explosão nas quatro direções
-- (x,y) coordenadas anteriores e xarg e yarg alteradores
explosãoDeslocar :: Tabuleiro -> ListaJogadores -> (Int,Int) -> Int -> Int -> Int-> (Tabuleiro,ListaJogadores)
explosãoDeslocar t lj (x,y) xarg yarg alcance
    | alcance == 0 = (t,lj)                     --fora de alcance da explosão
    | c == Pedra || c == Bomba = (t,lj)
    | null vazio = explosãoDeslocar t lj (x + xarg, y + yarg) xarg yarg (alcance - 1)   -- célula vazia fica inalterada
    | c == Grama = explosãoDeslocar t lj (x + xarg, y + yarg) xarg yarg (alcance - 1)   -- célula com grama fica inalterada
    | c == Parede || ehPresente c = explosãoDeslocar novoTabuleiro lj (x + xarg, y + yarg) xarg yarg (alcance - 1)
    | existeJogador ce = explosãoDeslocar novoTabuleiroSemJ novaListaJogadores (x + xarg, y + yarg) xarg yarg (alcance -1)  -- mata jogadores e desloca
    | otherwise = (t,lj) --gambiarra
    where (x1,y1) = (x + xarg, y + yarg)        -- coordenadas da célula explodida
          ce@(c:cs) = obterCelula t x1 y1          -- conteúdo da célula explodida
          vazio = obterCelula t x1 y1           -- conteúdo da célula explodida pro caso vazio (evita erro)
          novoTabuleiro = editarTabuleiro t cs x1 y1
          (cel, novaListaJogadores) = matarJogadores ce lj
          novoTabuleiroSemJ = editarTabuleiro t cel x1 y1


matarJogadores :: Celula -> ListaJogadores -> (Celula,ListaJogadores)
matarJogadores [] lj = ([],lj)         
matarJogadores ce@(c:cs) lj
    | properJogador c =  matarJogadores cs novaLista
    | otherwise = (ce,lj)
    where novaLista = retirarListaJogador lj c


-----------------------------------------------------------------------------------------------------

{-
    Considerando que precisamos detectar o fim de jogo seria útil, em um jogo com o intuito de matar os outros jogadores
    que usássemos uma lista para armazenar os jogadores e o fim de jogo seria alcance quando um ou nenhum jogador sobrasse vivo nessa lista
        Lembrando que no estado atual o jogador morto é indicado pelas suas coordenadas sendo (-1,-1)
-}

detectarFim :: ListaJogadores -> Bool 
detectarFim [] = True 
detectarFim _ = False 
