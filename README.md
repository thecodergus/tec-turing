# Trabalho de Teoria da computação

## Titulo

Tradutor de Modelos de Máquina de Turing

## Resumo

Este trabalho tem como objetivo desenvolver um tradutor de modelos de máquina de Turing. O tradutor deve ser capaz de ler um arquivo texto com extensão .in contendo um programa de máquina de Turing escrito na sintaxe do simulador online disponível em <http://morphett.info/turing/turing.html>, e produzir um arquivo texto com extensão .out com um programa equivalente para o modelo "oposto" ao de entrada, ou seja, se a entrada for uma máquina de Turing do modelo de Sipser, a saída deve ser uma máquina de Turing de fita duplamente infinita, e vice-versa.

O programa deve ser capaz de ser executado em um computador com sistema operacional Ubuntu 22.04.4 LTS de 64 bits e deve ser escrito em uma linguagem de programação de escolha livre. Os arquivos de entrada devem conter apenas máquinas de Turing determinísticas e codificação válida para o simulador, e o alfabeto da fita deve ser constituído exclusivamente de letras (maiúsculas e minúsculas) e dígitos.

A entrega do trabalho deve conter o programa final, instruções claras de execução e o endereço de um repositório público no GitHub com todo o código-fonte. Quaisquer dúvidas a respeito do trabalho devem ser postadas no fórum de dúvidas da disciplina.


## Regras


### Sintaxe

- Cada linha deve conter uma tupla na forma '(símbolo atual) (novo símbolo) (direção) (novo estado)'.
- Você pode usar qualquer número ou palavra para (estado atual) e (novo estado), por exemplo, 10, a, estado1. Os rótulos de estado diferenciam maiúsculas de minúsculas.
- Você pode usar quase qualquer caractere para (símbolo atual) e (novo símbolo), ou '_' para representar em branco (espaço). Os símbolos diferenciam maiúsculas de minúsculas.
- Não é possível usar ';', '*', '_' ou espaço em branco como símbolos.
- (direção) deve ser 'l', 'r' ou '*', denotando 'mover para a esquerda', 'mover para a direita' ou 'não se mover', respectivamente.
- Qualquer coisa após um ';' é um comentário e é ignorada.
- A máquina para quando alcança qualquer estado começando com 'halt', por exemplo, halt, halt-accept.

### Além disso

- '*' pode ser usado como curinga em (símbolo atual) ou (estado atual) para corresponder a qualquer caractere ou estado.
- '*' pode ser usado em (novo símbolo) ou (novo estado) para significar 'sem alteração'.
- '!' pode ser usado no final de uma linha para definir um ponto de interrupção, por exemplo, '1 a b r 2 !'. A máquina pausará automaticamente após executar esta linha.
- Você pode especificar a posição inicial para a cabeça usando '*' na entrada inicial.


## Estrategias

### Para qualquer entrada

- A configuração programa será encapsulada por hashtags (#). Programa exemplo:
  
```
    0 0 # r q0
    0 1 # r q1
    q0 0 0 r q0
    q0 1 0 r q1
    q0 _ 0 r qf
    q1 0 1 r q0
    q1 1 1 r q1 
    q1 _ 1 r qf
    qf _ # * halt
```

### Quando a entrada é uma maquina de Turing de fita duplamente infinita e a saída é uma maquina de Turing de Sipser

Quando a entrada é uma maquina de Turing de fita duplamente infinita e a saída é uma maquina de Turing de Sipser, a estratégia é a seguinte:
 
- O primeiro espaço após a hashtag vira um espaço em branco.
- Vai movendo todos os simbolos da fita para a direita até encontrar um espaço em branco.
- Quando encontrar um espaço em branco, ele vai para a direita até encontrar o simbolo que estava no primeiro espaço em branco.
- Quando encontrar o simbolo que estava no primeiro espaço em branco, ele vai executar o programa normalmente a partir do estado mais a esquerda que precisava do espaço em branco mais a esquerda.