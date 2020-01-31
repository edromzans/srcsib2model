# Formato dos dados ambientais de entrada: arquivo data2

São sete colunas de dados, sendo, da esquerda para direita:


1<sup>a</sup> -- datetime: ano, mês, dia e hora YYMMDDHH 

2<sup>a</sup> -- Ki(Wm\^{$-2$}): irradiância de onda curta incidente observada [Wm$^{-2}$]

3<sup>a</sup> -- em(hPa): pressão de vapor d'água observada [hPa]

4<sup>a</sup> -- tm(K): temperatura do ar observada [K]

5<sup>a</sup> -- um(ms\^{$-1$}): velocidade horizontal do vento observada [ms$^{-1}$]

6<sup>a</sup> -- prec(mm): precipitação observada [mm]

7<sup>a</sup> -- Rn(Wm\^{$-2$}): saldo de radiação observado [Wm$^{-2}$]

O formato explícito em fortran é "(A8,6F11.4)", exemplo:

```fortran
write(2,'(A8,6F11.4)') nymd, swdown, em, tm, um, prec, rnetm
```

Os arquivos data2 são gerados pelo programa em Fortran 95 denominado como "data2\_format.f95" localizado no
diretório "utils" do repositório. Inicialmente Rn ficava na 3<sup>a</sup> coluna, mas em agosto de 2019, a ordem das colunas foi alterada para permitir a inserção de uma condição if no código "sib2c" que seleciona o flag ilw e decide sobre o uso da coluna de Rn. 

# Equação rsoil no Sib2xb

O LCB vem utilizando 2 equações para rsoil:

1.  rsoil =  amax1 (0.1, 694. - fac*1500.) + 23.6        
2.  rsoil =  amax1 (0.1, 1001. - exp(fac*6.686))         

A tabela abaixo indica qual a equação rsoil tem sido utilizada para cada local de medidas.


##### Tabela 1 - Equação rsoil utilizada em cada sítio de medidas #####
| Locais      | equação |
| ----------- | ----------- |
| Cana |  2 |
| Cerrado |  2 |
| Eucalipto | 1 |
| Fazenda K77 | 2 | 
| Floresta Atlântica | 1 |
| Floresta Rondônia | 2 |
| Pastagem Rondônia | 2|
| Pastagem SP | 2 |


# Tradução do SiB2 Fortran 77 para Fortran 95

Identificou-se que o modelo SiB2 que vinha sendo utilizado no LCB, escrito em Fortran 77, apresentava erros de compilação e de execução quando compilado a partir do gfortran no linux debian buster e versões anteriores mesmo usando o g77 que integrava o gcc até a sua versão 3.4.6. O SiB2 no LCB geralmente era utilizado a partir de compiladores de Fortran 77 em windows antigos que já não estão mais disponíveis no LCB, portanto, foi estudada as possibilidades de continuar fazendo altereções no SiB2 usando compiladores de Fortran mais portáveis e nas versões mais atuais e neste sentido o gfortran que integra o gcc 8.3.0 foi considerado o mais indicado.

Fazendo modificações no código do SiB2 em Fortran 77, combinadas com flags de compilação disponíveis no gfortran, os erros de compilação e execução do SiB2 do LCB no gfortran do gcc 8.3.0 foram solucionados e os resultados das saídas do modelo compilado no gfortran e nos compiladores de windows antigos ficaram iguais, apresentando pequenas diferenças associadas com arredondamento numérico das variáveis do tipo real, o que era de se esperar.

Considerando os avanços da linguagem Fortran 77 para 95 e as necessiades de constantes adaptações no SiB2 do LCB, o código do SiB2 do LCB foi traduzido integralmente para Fortran 95.

# SiB2 em Fortran 95: comsibc.h e pardif.h como um módulo de variaveis

Com a tradução do SiB2 de Fortran 77 para 95, foi desenvovida uma versão do código que elimina as declarações de variáveis do tipo "commom" criando módulos de variáveis de Fortran. Nesta versão deve-se compilar o código conforme os seguintes comandos:

```shell
gfortran comsibc.f95 pardif.f95 sib2x.f95 Sib2xa.f95 Sib2xb.f95 -o sib2run
```
ou com os flags de compilação:
```shell
gfortran -fno-automatic -finit-local-zero comsibc.f95 pardif.f95 sib2x.f95 Sib2xa.f95 Sib2xb.f95 -o sib2run
```

# SiB2pymod

O SiB2pymod é o modelo SiB2 em Fortran 95, compilado como um módulo de python 
3 com o uso do f2py do integra o numpy. Para isso, uma pequena mudança
no código do SiB2 é feita. O programa sib2 é descrito como
uma subrotina. Desta forma o sib2 é usado como uma função python.
```shell
f2py3.7 --fcompiler=gnu95 --f90flags='-fno-automatic -finit-local-zero' -c *.f95 -m sib2pymod
```

# Tradução do SiB2 para linguagem C com f2c 

Pode ser conveniente traduzir o SiB2 do Fortan 77 para linguagem C e para isto pode-se utilizar o f2c (http://www.netlib.org/f2c/). Este processo tem se mostrado eficiente, evitando incompatibilidades de compiladores mais antigos de fortran para os mais atuais. Desta forma obtém-se o código em C e compila-se com o um compilador C, gerando um binário executável independente de um compilador de Fortan.

Inicialmente os arquivos .for devem ser renomeados para .F. Após execute.


```shell
f2c -Nn802 *.F
```

Desta forma obtém os códigos fontes em C: sib2x.c, Sib2xa.c e Sib2xb.c. Para obter o executável, aqui nomeado com SiB2runF2C, execute.  

```shell
gcc -c -o sib2x.o sib2x.c
gcc -c -o Sib2xa.o Sib2xa.c
gcc -c -o Sib2xb.o Sib2xb.c
gcc -o SiB2runF2C sib2x.o Sib2xa.o Sib2xb.o -lf2c -lm
```

Este mesmo resultado se obtém utilizando o script fort77 em perl que pode ser obtido pelo pacote "fort77 - Invoke f2c like a real compiler" do debian.
