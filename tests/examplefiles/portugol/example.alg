Algoritmo "semnome"
// Descrição   : Aqui você descreve o que o programa faz!
// Autor(a)    : Lorhan Sohaky
// Data atual  : 04/12/2022
Var
// Seção de Declarações das variáveis 

meu_caractere: caractere
meu_inteiro: inteiro
meu_real: real
meu_vetor: vetor[1..10] de inteiro

Inicio
// Seção de Comandos, procedimento, funções, operadores, etc... 

    escreva("Digite N1:")
    leia(n1)
    n1 <- 1

    escreva("Digite N2:")
    leia(n2)

    escreval("Valor 1:", n1)
    escreva("Valor 2:", n2)

    resultado <- n1 + n2
    escreval(n1,"+", n2, "=", resultado)

    resultado <- n1 - n2
    escreval(n1,"-", n2, "=", resultado)

    resultado <- n1 * n2
    escreval(n1,"*", n2, "=", resultado)

    resultado <- n1 / n2
    escreval(n1,"/", n2, "=", resultado)

    resultado <- n1 DIV n2
    escreval(n1,"DIV", n2, "=", resultado)

    resultado <- n1 MOD n2
    escreval(n1,"MOD", n2, "=", resultado)

    me <- (n1 + n2 + n3) / 3
    escreva("Média:", me)

    vq <- sm / 5
    vp <- sm - (qq * vq)
    vpd <- sm - ((vp * 15) / 100)

    se ( n1 > n2 ) entao
        escreva("N1 é maior que N2")
    fimse

    se ( n2 > n1 ) entao
        escreva("N2 é maior que N1")
    fimse

    se ( n2 = n1 ) entao
        escreva("N2 = N1")
    fimse 

    se ( n  mod 2 <> 0 ) entao
        escreva("Impar")
    senao
        escreva("Par")
    fimse

    se (n < 10) entao
        escreva("N < 10")
    senao
        se ( n = 10 ) entao
            escreva("N = 10")
        senao
            se (n > 10) e (n < 50) entao
                escreva("10 < N < 50")
            senao
                se (n = 50) entao
                    escreva("N = 50")
                senao
                    escreva("N > 50")
                fimse
            fimse
        fimse
    fimse

    se ( x = "M" ) ou ( x = "masculino" ) entao
        escreva("Sexo: masculino")
    senao 
        se ( x = "F" ) ou ( x = "feminino" ) entao
            escreva("Sexo: feminino")
        senao
            escreva("Digite novamente!")
        fimse
    fimse

    se (a >= b) e (b >= c) entao
        maior <- a
        menor <- c
    senao
        se (a >= c) e (c >= b) entao
            maior <- a
            menor <- b
        senao
            se (b >= a) e (a >= c) entao
                maior <- b
                menor <- c
            senao
                se (b >= c) e (c >= a) entao
                    maior <- b
                    menor <- a
                senao
                    se (c >= a) e (a >= b) entao
                        maior <- c
                        menor <- b
                    senao
                        maior <- c
                        menor <- a
                    fimse
                fimse
            fimse
        fimse
    fimse

    escolha (n)
    caso 1
        escreva("Janeiro")
    caso 2
        escreva("Fevereiro")
    caso 3
        escreva("Marco")
    caso 4
        escreva("Abril")
    caso 5
        escreva("Maio")
    caso 6
        escreva("Junho")
    caso 7
        escreva("Julho")
    caso 8
        escreva("Agosto")
    caso 9
        escreva("Setembro")
    caso 10
        escreva("Outubro")
    caso 11
        escreva("Novembro")
    caso 12
        escreva("Dezembro")
    outrocaso
        escreva("'Numero invalido'")
    fimescolha

    enquanto (n <11) faca
        escreva(n)
        n <- n + 1
    fimenquanto

    para n de 1 ate 10 faca
        escreva(n)
    fimpara

    funcao soma: inteiro
        var aux: inteiro
        inicio
        // n, m e res são variáveis globais
        aux <- n + m
        retorne aux
    fimfuncao

    res <- soma
    escreva(res)

     funcao soma (x,y: inteiro): inteiro
        inicio
        retorne x + y
    fimfuncao

    n <- 4
    m <- -9
    res <- soma(n,m)
    Pi * 10
    escreva(res * Pi * exp(10))
Fimalgoritmo