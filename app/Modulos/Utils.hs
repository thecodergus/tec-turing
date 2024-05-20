module Utils where

import qualified Tipos
import qualified Parser.Tipos as Parser
import Tipos (Tipo)

instrucoesParaMaquinaTuring :: (Tipo, [Parser.Instrucao]) -> Tipos.MaquinaTuring
instrucoesParaMaquinaTuring (tipo, instrucoes) = Tipos.MaquinaTuring {
  Tipos.estados = encontrarEstados instrucoes [],
  Tipos.alfabeto = definirAlfabeto instrucoes,
  Tipos.transicoes = definirTransicoes instrucoes,
  Tipos.estadoInicial = definirEstadoInicial,
  Tipos.tipo = tipo
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
          | simboloAtual == '_' || simboloAtual == '*' = alfabeto xs listaSimbolos
          | simboloAtual `notElem` listaSimbolos = alfabeto xs (listaSimbolos ++ [simboloAtual])
          | otherwise = alfabeto xs listaSimbolos

    definirTransicoes :: [Parser.Instrucao] -> [Tipos.Transicao]
    definirTransicoes = map instrucaoParaTransicao
        where
            instrucaoParaTransicao :: Parser.Instrucao -> Tipos.Transicao
            instrucaoParaTransicao (Parser.Instrucao estadoAtual simboloAtual' novoSimbolo direcao novoEstado) = Tipos.Transicao {
                Tipos.estadoAtual = Tipos.Estado estadoAtual,
                Tipos.simboloAtual = simboloAtual simboloAtual',
                Tipos.simboloSerEscrito = simboloNovo novoSimbolo,
                Tipos.direcao = direcaoParaDirecao direcao,
                Tipos.proximoEstado = Tipos.Estado novoEstado
            }
                where
                    direcaoParaDirecao :: Char -> Tipos.Direcao
                    direcaoParaDirecao 'l' = Tipos.Esquerda
                    direcaoParaDirecao 'r' = Tipos.Direita
                    direcaoParaDirecao '*' = Tipos.Parado
                    direcaoParaDirecao _ = error "Instrução de direção inválida"

                    simboloAtual :: Char -> Tipos.SimboloAtual
                    simboloAtual '_' = Tipos.SimboloAtual Tipos.Vazio
                    simboloAtual '*' = Tipos.Todos
                    simboloAtual simbolo' = Tipos.SimboloAtual $ Tipos.Simbolo [simbolo']

                    simboloNovo :: Char -> Tipos.SimboloSerEscrito
                    simboloNovo '_' = Tipos.SimboloSerEscrito Tipos.Vazio
                    simboloNovo '*' = Tipos.Manter
                    simboloNovo simbolo' = Tipos.SimboloSerEscrito $ Tipos.Simbolo [simbolo']

    definirEstadoInicial :: Tipos.Estado
    definirEstadoInicial = Tipos.Estado "0"
