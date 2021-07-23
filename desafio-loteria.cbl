      $set sourceformat"free"
       identification division.                                                                 *>divisão de identificação do programa
       program-id. "desafio-loteria".                                                         *>--- program-id é uma informação obrigatória ---
       author. "Camila da Rosa Hostin".
       installation. "PC".
       date-written. 22/07/2020.
       date-compiled. 22/07/2020.

       environment division.                                                                    *>divisão para configurações de ambiente
       configuration section.
           special-names. decimal-point is comma.

       input-output section.                                                                    *>declaração de recursos externos
       file-control.
       i-o-control.

       data division.                                                                           *>declaração de variáveis
                                                                                                *>- data division, tem 4 sessões possíveis -
       file section.                                                                            *>--- variáveis de arquivo ---

       working-storage section.                                                                 *>--- variáveis de trabalho ---

       01  ws-uso-comum.
           05  ws-ind-lot                          pic  9(02).
           05  ws-ind                              pic  9(02).
           05  ws-qtd-num-jog                      pic  9(02).
           05  ws-tentativa                        pic  9(02).
           05  ws-contador                         pic  9(09).

       01  ws-sorteio.                                                                          *>  variáveis para número randômico
           05  ws-semente                          pic  9(08).
           05  ws-semente1                         pic  9(08).
           05  ws-num_random                       pic  9(01)v9(08).

       01  ws-relogio.                                                                          *>  variáveis de semente e relógio, usados para sorteio
           05  ws-hora                             pic  9(02).
           05  ws-minuto                           pic  9(02).
           05  ws-segundo                          pic  9(02).
           05  ws-cent_segundo                     pic  9(02).

       01  ws-numeros-sorteados.                                                                *>  variáveis para guardar números sorteados
           05  ws-sort-num1                        pic  9(02).
           05  ws-sort-num2                        pic  9(02).
           05  ws-sort-num3                        pic  9(02).
           05  ws-sort-num4                        pic  9(02).
           05  ws-sort-num5                        pic  9(02).
           05  ws-sort-num6                        pic  9(02).

       01  ws-numeros-usuario.                                                                  *>  números apostados pelo usuário
           05  ws-num1                             pic  9(02).
           05  ws-num2                             pic  9(02).
           05  ws-num3                             pic  9(02).
           05  ws-num4                             pic  9(02).
           05  ws-num5                             pic  9(02).
           05  ws-num6                             pic  9(02).
           05  ws-num7                             pic  9(02).
           05  ws-num8                             pic  9(02).
           05  ws-num9                             pic  9(02).
           05  ws-num10                            pic  9(02).

       77  ws-diferenca-hr                         pic  9(02).
       77  ws-diferenca-min                        pic  9(02).                                  *>  variáveis para cálculo de tempo
       77  ws-diferenca-seg                        pic  9(02).

       01  ws-hora-inicio.                                                                      *>  variáveis de para saber o tempo do sorteio
           05  ws-hor                              pic 9(002).
           05  ws-min                              pic 9(002).
           05  ws-seg                              pic 9(002).

       01  ws-hora-final.                                                                       *>  variáveis de para saber o tempo do sorteio
           05  ws-hor-fim                          pic  9(02).
           05  ws-min-fim                          pic  9(02).
           05  ws-seg-fim                          pic  9(02).

       linkage section.                                                                         *>--- variáveis para comunicação entre programas ---

       screen section.                                                                          *>declaração de tela
      *>------------------ apresentação do problema -------------------------<*

      *> Um pesquisador da área da matemática quer testar na prática a
      *>probabilidade de acerto da Mega-Sena. Para isso solicita a
      *>construção de um programa de computador na linguagem Cobol
      *>conforme as especificações abaixo.
      *>Especificações:
      *>Crie um programa que receba uma aposta e sorteie 6 números
      *>aleatórios compreendidos no subconjunto dos números inteiros no
      *>intervalo de 1 a 60.
      *>Uma oposta pode conter entre 6 e 10 números compreendidos no
      *>subconjunto dos números inteiros no intervalo de 1 a 60.
      *>O programa deve seguir sorteando até que os números sorteados
      *>sejam idênticos aos números da aposta.
      *> Imprimir os resultados da rodada:
      *> - Cada sorteio deve ser exibido na tela em uma linha;
      *> - Exibir a quantidade de tentativas até o acerto;
      *> - Exibir o tempo gasto até acertar a aposta.
      *> Informações adicionais:
      *>De acordo com a revista Exame, 30/12/2014 a probabilidade de
      *>acerto na Mega-Sena são:
      *>Qdt num          Valor da        Probabilidade
      *>jogados          aposta          de acerto (1 em ..)
      *>   6              R$2,50             50 milhões
      *>   7              R$17,50            7,1 milhões
      *>   8              R$70,00            1,78 milhões
      *>   9              R$210,00           595,9 mil
      *>   10             R$525,00           238,3 mil

      *>--------------------------------------------------------------------<*
       procedure division.                                                                      *>declaração do corpo do programa

           perform 1000-inicializa.
           perform 2000-processamento.
           perform 3000-finaliza.

      *>--------------------------------------------------------------------<*
      *> procedimentos de inicialização
      *>--------------------------------------------------------------------<*
       1000-inicializa section.

           move 0 to ws-contador
           move 0 to ws-ind-lot
           .
       1000-inicializa-exit.
           exit.
      *>--------------------------------------------------------------------<*
      *> processamento principal
      *>--------------------------------------------------------------------<*
       2000-processamento section.

           move function current-date(9:6)         to  ws-hora-inicio                           *>  função para pegar quanto tempo o programa ficou sorteando - início

           display '********************* Faca seu jogo *********************'
           display '**                                                     **'
           display '**   01   02   03   04   05   06   07   08   09   10   **'
           display '**                                                     **'
           display '**   11   12   13   14   15   16   17   18   19   20   **'
           display '**                                                     **'
           display '**   21   22   23   24   25   26   27   28   29   30   **'
           display '**                                                     **'                  *>  display dos números que podem ser jogados
           display '**   31   32   33   34   35   36   37   38   39   40   **'
           display '**                                                     **'
           display '**   41   42   43   44   45   46   47   48   49   50   **'
           display '**                                                     **'
           display '**   51   52   53   54   55   56   57   58   59   60   **'
           display '**                                                     **'
           display '*********************************************************'

      *>   fazer tratamento para não repetir números
           display 'Informe o Primeiro Numero?'
           accept ws-num1
           display 'Informe o Segundo Numero?'
           accept ws-num2
           display 'Informe o Terceiro Numero?'
           accept ws-num3
           display 'Informe o Quarto Numero?'
           accept ws-num4
           display 'Informe o Quinto Numero?'
           accept ws-num5
           display 'Informe o Sexto Numero?'
           accept ws-num6
           display 'Informe o Setimo Numero? Nao Que Apostar Mais Numeros? Digite 00'
           accept ws-num7
           if   ws-num7 = 00 then                                                               *>  opção, se não quiser jogar mais de 6 números, colocar 00 no 7ºn
                move 00                            to  ws-num7                                  *>  move 00 para os outros números
                move 00                            to  ws-num8
                move 00                            to  ws-num9
                move 00                            to  ws-num10
                perform 2100-sorteia-loteria
           end-if
           display 'Informe o Oitavo Numero? Nao Que Apostar Mais Numeros? Digite 00'
           accept ws-num8
           if   ws-num8 = 00 then                                                               *>  opção, se não quiser jogar mais de 7 números, colocar 00 no nº8
                move 00                            to  ws-num8                                  *>  move 00 para os outros números
                move 00                            to  ws-num9
                move 00                            to  ws-num10
                perform 2100-sorteia-loteria
           end-if
           display 'Informe o Nono Numero? Nao Que Apostar Mais Numeros? Digite 00'
           accept ws-num9                                                                       *>  opção, se não quiser jogar mais de 8 números, colocar 00 no nº9
           if   ws-num9 = 00 then                                                               *>  move 00 para os outros números
                move 00                            to  ws-num9
                move 00                            to  ws-num10
                perform 2100-sorteia-loteria
           end-if
           display 'Informe o Decimo Numero? Nao Que Apostar Mais Numeros? Digite 00'
           accept ws-num10                                                                      *>  opção, se não quiser jogar mais de 9 números, colocar 00 no nº10

           if   ws-num10 = 00 then                                                              *>  move 00 para o último número
                move 00                            to  ws-num10
                perform 2100-sorteia-loteria
           end-if

           perform 2100-sorteia-loteria
           .
       2000-processamento-exit.
           exit.
      *>--------------------------------------------------------------------<*
      *> procedimento de sorteio da loteria
      *>--------------------------------------------------------------------<*
       2100-sorteia-loteria section.
           move zero to ws-ind-lot                                                              *>  inicializando variável índice

           perform until ws-ind-lot <> 0

               move ws-semente                     to  ws-relogio

               accept ws-semente from time

               perform 2200-semente-delay                                                       *>  gerar primeiro número randômico
               compute ws-sort-num1  =  function random(ws-semente) * 60

               perform 2200-semente-delay                                                       *>  gerar segundo número randômico
               compute ws-sort-num2  =  function random(ws-semente + ws-sort-num1) * 60

               perform 2200-semente-delay                                                       *>  gerar terceiro número randômico
               compute ws-sort-num3  =  function random(ws-semente + ws-sort-num2) * 60

               perform 2200-semente-delay                                                       *>  gerar quarto número randômico
               compute ws-sort-num4  =  function random(ws-semente + ws-sort-num3) * 60

               perform 2200-semente-delay                                                       *>  gerar quinto núemro randômico
               compute ws-sort-num5  =  function random(ws-semente + ws-sort-num4) * 60

               perform 2200-semente-delay                                                       *>  gerar sexto núemro randômico
               compute ws-sort-num6  =  function random(ws-semente + ws-sort-num5) * 60

               perform 2300-conferir-nm-sorteados

           end-perform
           .
       2100-sorteia-loteria-exit.
           exit.
      *>--------------------------------------------------------------------<*
      *> delay nos números de sorteio da loteria
      *>--------------------------------------------------------------------<*
       2200-semente-delay section.

           perform 10 times
               accept ws-semente1 from time
               move ws-semente1 to ws-semente
               perform until ws-semente > ws-semente1
                   accept ws-semente from time
               end-perform
           end-perform
           .
       2200-semente-delay-exit.
           exit.
      *>--------------------------------------------------------------------<*
      *> conferindo se os números sorteados são diferentes entre eles
      *> e diferentes de 00
      *>--------------------------------------------------------------------<*
       2300-conferir-nm-sorteados section.

           if   ws-sort-num1 <> ws-sort-num2
           and  ws-sort-num1 <> ws-sort-num3
           and  ws-sort-num1 <> ws-sort-num4
           and  ws-sort-num1 <> ws-sort-num5
           and  ws-sort-num1 <> ws-sort-num6
           and  ws-sort-num1 <> '00' then
                if   ws-sort-num2 <> ws-sort-num3
                and  ws-sort-num2 <> ws-sort-num4
                and  ws-sort-num2 <> ws-sort-num5
                and  ws-sort-num2 <> ws-sort-num6
                and  ws-sort-num2 <> '00' then
                     if   ws-sort-num3 <> ws-sort-num4
                     and  ws-sort-num3 <> ws-sort-num5
                     and  ws-sort-num3 <> ws-sort-num6
                     and  ws-sort-num3 <> '00' then
                          if   ws-sort-num4 <> ws-sort-num5
                          and  ws-sort-num4 <> ws-sort-num6
                          and  ws-sort-num4 <> '00' then
                               if   ws-sort-num5 <> ws-sort-num6
                               and  ws-sort-num5 <> '00' then
                                    if   ws-sort-num6 <> '00' then

                                         perform 2400-conferir-aposta
                                         display ws-sort-num1 ' | ' ws-sort-num2
                                         ' | ' ws-sort-num3 ' | ' ws-sort-num4 ' | '            *>  apresentação dos números sorteados
                                         ws-sort-num5 ' | ' ws-sort-num6 ' | '
                                         ' - ' ws-contador

                                    else
                                         perform 2100-sorteia-loteria
                                    end-if
                               end-if
                          end-if
                     end-if
                end-if
           end-if
           .
       2300-conferir-nm-sorteados-exit.
           exit.
      *>--------------------------------------------------------------------<*
      *> conferindo se os números sorteados são iguais os da aposta
      *>--------------------------------------------------------------------<*
       2400-conferir-aposta section.

           add 1 to ws-contador

           if   ws-sort-num1 = ws-num1 or ws-sort-num1 = ws-num2
           or   ws-sort-num1 = ws-num3 or ws-sort-num1 = ws-num4
           or   ws-sort-num1 = ws-num5 or ws-sort-num1 = ws-num6
           or   ws-sort-num1 = ws-num7 or ws-sort-num1 = ws-num8
           or   ws-sort-num1 = ws-num9 or ws-sort-num1 = ws-num10 then
                if   ws-sort-num2 = ws-num1 or ws-sort-num2 = ws-num2
                or   ws-sort-num2 = ws-num3 or ws-sort-num2 = ws-num4
                or   ws-sort-num2 = ws-num5 or ws-sort-num2 = ws-num6
                or   ws-sort-num2 = ws-num7 or ws-sort-num2 = ws-num8
                or   ws-sort-num2 = ws-num9 or ws-sort-num2 = ws-num10 then
                     if   ws-sort-num3 = ws-num1 or ws-sort-num3 = ws-num2
                     or   ws-sort-num3 = ws-num3 or ws-sort-num3 = ws-num4
                     or   ws-sort-num3 = ws-num5 or ws-sort-num3 = ws-num6
                     or   ws-sort-num3 = ws-num7 or ws-sort-num3 = ws-num8
                     or   ws-sort-num3 = ws-num9 or ws-sort-num3 = ws-num10 then
                          if   ws-sort-num4 = ws-num1 or ws-sort-num4 = ws-num2
                          or   ws-sort-num4 = ws-num3 or ws-sort-num4 = ws-num4
                          or   ws-sort-num4 = ws-num5 or ws-sort-num4 = ws-num6
                          or   ws-sort-num4 = ws-num7 or ws-sort-num4 = ws-num8
                          or   ws-sort-num4 = ws-num9 or ws-sort-num4 = ws-num10 then
                               if   ws-sort-num5 = ws-num1 or ws-sort-num5 = ws-num2
                               or   ws-sort-num5 = ws-num3 or ws-sort-num5 = ws-num4
                               or   ws-sort-num5 = ws-num5 or ws-sort-num5 = ws-num6
                               or   ws-sort-num5 = ws-num7 or ws-sort-num5 = ws-num8
                               or   ws-sort-num5 = ws-num9 or ws-sort-num5 = ws-num10 then
                                    if   ws-sort-num6 = ws-num1 or ws-sort-num6 = ws-num2
                                    or   ws-sort-num6 = ws-num3 or ws-sort-num6 = ws-num4
                                    or   ws-sort-num6 = ws-num5 or ws-sort-num6 = ws-num6
                                    or   ws-sort-num6 = ws-num7 or ws-sort-num6 = ws-num8
                                    or   ws-sort-num6 = ws-num9 or ws-sort-num6 = ws-num10 then
                                         move function current-date(9:6)  to  ws-hora-final     *>  função para pegar quanto tempo o programa ficou sorteando - início
                                         display 'Voce Acertou!'                                *>  se todos os números são iguais, o programa exibe 'você acertou',

                                         display ws-num1 ' | ' ws-num2 ' | ' ws-num3 ' | '
                                          ws-num4 ' | ' ws-num5 ' | ' ws-num6 ' | ' ws-num7     *>  apresenta os números apostados e os
                                          ' | ' ws-num8 ' | ' ws-num9 ' | ' ws-num10 ' | '

                                         display ws-sort-num1 ' | ' ws-sort-num2 ' | '
                                         ws-sort-num3 ' | ' ws-sort-num4 ' | '                  *>  apresenta os números sorteados, também
                                         ws-sort-num5 ' | ' ws-sort-num6 ' | '

                                         perform 2500-tempo-sorteando                           *>  chamar section para calcular o tempo de sorteio
                                         display 'Tempo que levou para acertar' ws-diferenca-hr ' : '
                                         ws-diferenca-min ' : ' ws-diferenca-seg

                                         display 'Quantidade de Sorteios - ' ws-contador        *>  apresenta o número de vezes que o programa sorteou

                                         perform 3000-finaliza
                                    else
                                         perform 2100-sorteia-loteria
                                    end-if
                               end-if
                          end-if
                     end-if
                end-if
           end-if
           .
       2400-conferir-aposta-exit.
           exit.
      *>--------------------------------------------------------------------<*
      *> tempo total que os números ficaram sorteando
      *>--------------------------------------------------------------------<*
       2500-tempo-sorteando section.

           compute ws-diferenca-hr  = (ws-hor - ws-hor-fim)                                     *>  calculo da diferença de horas
           compute ws-diferenca-min = (ws-min - ws-min-fim)                                     *>  calculo da diferença de minutos
           compute ws-diferenca-seg = (ws-seg - ws-seg-fim)                                     *>  calculo da diferença de segundos
           .
       2500-tempo-sorteando-exit.
           exit.
      *>--------------------------------------------------------------------<*
      *> procedimentos de finalização
      *>--------------------------------------------------------------------<*
       3000-finaliza section.

           stop run
           .
       3000-finaliza-exit.
           exit.


