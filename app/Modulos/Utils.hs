module Utils where

import qualified Tipos
import qualified Parser.Tipos as Parser

instrucoesParaMaquinaTuring :: [Parser.Instrucao] -> Tipos.MaquinaTuring
instrucoesParaMaquinaTuring instrucoes = Tipos.MaquinaTuring {
  Tipos.estados = encontrarEstados instrucoes [],
  Tipos.alfabeto = definirAlfabeto instrucoes,
  Tipos.transicoes = definirTransicoes instrucoes,
  Tipos.estadoInicial = definirEstadoInicial
}
  where
    encontrarEstados :: [Parser.Instrucao] -> [String] -> [Tipos.Estado]
    encontrarEstados [] listaFinal = map Tipos.Estado listaFinal
    encontrarEstados (Parser.Instrucao estadoAtual _ _ _ _ : xs) listaEstados
        | estadoAtual `notElem` listaEstados = encontrarEstados xs (listaEstados ++ [estadoAtual])
        | otherwise = encontrarEstados xs listaEstados

    definirAlfabeto :: [Parser.Instrucao] -> [Tipos.Simbolo]
    definirAlfabeto instrucoes' = map (Tipos.Simbolo . (:[])) (alfabeto instrucoes' [])
      where
        alfabeto :: [Parser.Instrucao] -> [Char] -> [Char]
        alfabeto [] listaFinal = listaFinal
        alfabeto (Parser.Instrucao _ simboloAtual _ _ _ : xs) listaSimbolos
          | simboloAtual `notElem` listaSimbolos = alfabeto xs (listaSimbolos ++ [simboloAtual])
          | simboloAtual == '_' || simboloAtual == '*' = alfabeto xs listaSimbolos
          | otherwise = alfabeto xs listaSimbolos

    definirTransicoes :: [Parser.Instrucao] -> [Tipos.Transicao]
    definirTransicoes = map instrucaoParaTransicao
        where
            instrucaoParaTransicao :: Parser.Instrucao -> Tipos.Transicao
            instrucaoParaTransicao (Parser.Instrucao estadoAtual simboloAtual novoSimbolo direcao novoEstado) = Tipos.Transicao {
                Tipos.estadoAtual = Tipos.Estado estadoAtual,
                Tipos.simboloAtual = simbolo simboloAtual,
                Tipos.simboloSerEscrito = simbolo novoSimbolo,
                Tipos.direcao = direcaoParaDirecao direcao,
                Tipos.proximoEstado = Tipos.Estado novoEstado
            }
                where
                    direcaoParaDirecao :: Char -> Tipos.Direcao
                    direcaoParaDirecao 'l' = Tipos.Esquerda
                    direcaoParaDirecao 'r' = Tipos.Direita
                    direcaoParaDirecao '*' = Tipos.Parado
                    direcaoParaDirecao _ = error "Instrução de direção inválida"

                    simbolo :: Char -> Tipos.Simbolo
                    simbolo '_' = Tipos.Vazio
                    simbolo '*' = Tipos.Manter
                    simbolo simbolo' = Tipos.Simbolo [simbolo']

    definirEstadoInicial :: Tipos.Estado
    definirEstadoInicial = Tipos.Estado "0"
