avaliacao\_1
================
Mateus Reida da Silva

**Questão 01**</br> O datasset abaixo é referente a os dados dos peixes
do rio madeira, que irá auxiliar nas resoluções das demais questões a
seguir.

``` r
dados_prm <- read_csv("dados/brutos/peixes_rio_madeira.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_character(),
    ##   id = col_double(),
    ##   data = col_datetime(format = ""),
    ##   mes = col_double(),
    ##   ano = col_double(),
    ##   local = col_double(),
    ##   cp_cm = col_double(),
    ##   peso_g = col_double()
    ## )
    ## i Use `spec()` for the full column specifications.

``` r
dados_prm %>% view()
```

**(a)** </br> Inicialmente é pedido na questão para que seja realizado a
frequência da variavel *ordem*, dessa forma o procedimento está disposto
abaixo, foi preciso realizar outro procedimrnto que criou-se também uma
variavel *n* que representa a frequência.

``` r
dados_prm %>% 
  group_by(ordem) %>% 
  summarise(n = n()) %>% 
  arrange(n)
```

    ## # A tibble: 12 x 2
    ##    ordem                  n
    ##    <chr>              <int>
    ##  1 Lepidosireniformes     2
    ##  2 Pleuronectiformes      2
    ##  3 Beloniformes           5
    ##  4 Não identificado      17
    ##  5 Myliobatiformes       41
    ##  6 Osteoglossiformes    433
    ##  7 Gymnotiformes        693
    ##  8 Acanthuriformes     1602
    ##  9 Cichliformes        1947
    ## 10 Clupeiformes        2821
    ## 11 Siluriformes       27451
    ## 12 Characiformes      64356

Depois de realizar os procedimentos acima, os dados foram organizados de
maneira crescente em ordem numerica as observações.

**(B)**</b> A ordem de peixes que foi mais indentificada foi a
Characiformes, a mesma também apresenta o valor de 12 observações.

**(c)**</br> Os não indentificados na variavel ordem são 17, como
demonstrado no procedimento realizado abaixo.

Seleção dos tipos de peixe.

``` r
dados_prm %>% 
  distinct(ordem)
```

    ## # A tibble: 12 x 1
    ##    ordem             
    ##    <chr>             
    ##  1 Siluriformes      
    ##  2 Characiformes     
    ##  3 Cichliformes      
    ##  4 Clupeiformes      
    ##  5 Acanthuriformes   
    ##  6 Não identificado  
    ##  7 Gymnotiformes     
    ##  8 Osteoglossiformes 
    ##  9 Myliobatiformes   
    ## 10 Beloniformes      
    ## 11 Pleuronectiformes 
    ## 12 Lepidosireniformes

``` r
dados_prm %>% 
  filter(ordem == "Não indentificado")
```

    ## # A tibble: 0 x 21
    ## # ... with 21 variables: id <dbl>, tipo_campanha <chr>, campanha <chr>,
    ## #   data <dttm>, mes <dbl>, ano <dbl>, ciclo_hidrologico <chr>, bacia <chr>,
    ## #   ambiente <chr>, local <dbl>, ponto <chr>, especie <chr>, genero <chr>,
    ## #   familia <chr>, ordem <chr>, nome_comum <chr>, cp_cm <dbl>, peso_g <dbl>,
    ## #   sexo <chr>, estadio_de_maturacao <chr>, habito_alimentar <chr>

Organização de forma crescente.

``` r
dados_prm %>% 
  group_by(ordem) %>% 
  summarise(n = n()) %>% 
  arrange(n)
```

    ## # A tibble: 12 x 2
    ##    ordem                  n
    ##    <chr>              <int>
    ##  1 Lepidosireniformes     2
    ##  2 Pleuronectiformes      2
    ##  3 Beloniformes           5
    ##  4 Não identificado      17
    ##  5 Myliobatiformes       41
    ##  6 Osteoglossiformes    433
    ##  7 Gymnotiformes        693
    ##  8 Acanthuriformes     1602
    ##  9 Cichliformes        1947
    ## 10 Clupeiformes        2821
    ## 11 Siluriformes       27451
    ## 12 Characiformes      64356

**Questão 02**

``` r
dados_prm %>% 
  distinct(bacia)
```

    ## # A tibble: 3 x 1
    ##   bacia      
    ##   <chr>      
    ## 1 Rio Madeira
    ## 2 Rio Mamoré 
    ## 3 Rio Guaporé

``` r
dados_prm %>% 
  select(ordem, peso_g)
```

    ## # A tibble: 99,370 x 2
    ##    ordem         peso_g
    ##    <chr>          <dbl>
    ##  1 Siluriformes      35
    ##  2 Siluriformes      35
    ##  3 Siluriformes      25
    ##  4 Characiformes    265
    ##  5 Siluriformes     160
    ##  6 Characiformes    130
    ##  7 Siluriformes      25
    ##  8 Characiformes     65
    ##  9 Cichliformes      15
    ## 10 Characiformes    250
    ## # ... with 99,360 more rows

``` r
dados_prm %>%
  drop_na() %>% 
  summarise(
    media_peso = mean(peso_g)
  )
```

    ## # A tibble: 1 x 1
    ##   media_peso
    ##        <dbl>
    ## 1       207.

calculo da média

``` r
dados_prm %>% 
  group_by(ordem) %>% 
  drop_na() %>% 
  summarise(
    media_peso = mean(peso_g)
  )
```

    ## # A tibble: 11 x 2
    ##    ordem             media_peso
    ##  * <chr>                  <dbl>
    ##  1 Acanthuriformes        556. 
    ##  2 Beloniformes            13.6
    ##  3 Characiformes          165. 
    ##  4 Cichliformes           210. 
    ##  5 Clupeiformes           264. 
    ##  6 Gymnotiformes          150. 
    ##  7 Myliobatiformes       1878. 
    ##  8 Não identificado       494. 
    ##  9 Osteoglossiformes      595. 
    ## 10 Pleuronectiformes      156  
    ## 11 Siluriformes           273.

Calculo do desvio padrão

``` r
dados_prm %>% 
  group_by(ordem) %>% 
  drop_na() %>% 
  summarise(
    dsp_peso = sd(peso_g)
  )
```

    ## # A tibble: 11 x 2
    ##    ordem             dsp_peso
    ##  * <chr>                <dbl>
    ##  1 Acanthuriformes      459. 
    ##  2 Beloniformes          15.5
    ##  3 Characiformes        236. 
    ##  4 Cichliformes         283. 
    ##  5 Clupeiformes         272. 
    ##  6 Gymnotiformes        206. 
    ##  7 Myliobatiformes     1871. 
    ##  8 Não identificado    1078. 
    ##  9 Osteoglossiformes   1821. 
    ## 10 Pleuronectiformes    187. 
    ## 11 Siluriformes         933.

**a** </br> A medida de variabilidade mais adequada é a de ceoficiente
de variação, depois de realizar os procedimentos dispostos acima

**b** </br> Ordem de peixe Acanthuriformes.

``` r
dados_prm %>% 
  group_by(ordem) %>% 
  drop_na() %>% 
  summarise(
    cv = (sd(peso_g)/mean(peso_g)) * 100
  )
```

    ## # A tibble: 11 x 2
    ##    ordem                cv
    ##  * <chr>             <dbl>
    ##  1 Acanthuriformes    82.5
    ##  2 Beloniformes      114. 
    ##  3 Characiformes     143. 
    ##  4 Cichliformes      135. 
    ##  5 Clupeiformes      103. 
    ##  6 Gymnotiformes     138. 
    ##  7 Myliobatiformes    99.6
    ##  8 Não identificado  218. 
    ##  9 Osteoglossiformes 306. 
    ## 10 Pleuronectiformes 120. 
    ## 11 Siluriformes      342.

**Questão 03**

``` r
dados_prm %>% 
  distinct(sexo)
```

    ## # A tibble: 4 x 1
    ##   sexo        
    ##   <chr>       
    ## 1 Fêmea       
    ## 2 Macho       
    ## 3 Não coletado
    ## 4 fêmea

``` r
dados_prm %>% 
  mutate(
    sexo_recode = recode(
      sexo,
      "Fêmea" = "Fêmea",
      "Macho" = "Macho",
      "fêmea" = "Fêmea",
      "Não coletado" = "Não coletado"
    )
  ) %>% 
  group_by(sexo_recode) %>% 
  summarise(
    n = n())
```

    ## # A tibble: 3 x 2
    ##   sexo_recode      n
    ## * <chr>        <int>
    ## 1 Fêmea        28331
    ## 2 Macho        21469
    ## 3 Não coletado 49570

**a**

``` r
(28331 - 21469)
```

    ## [1] 6862

``` r
(6862/28331) * 100
```

    ## [1] 24.22082

**b** </br> Nesse procedimento foi possivel calcular o sexo dos peixes
que tem maior peso, e verificou-se que foi a Fêmea.

``` r
dados_prm %>% 
  mutate(
    sexo_recode = recode(
      sexo,
  "Fêmea" =  "Fêmea",
  "Macho"   =   "Macho",
  "fêmea" = "Fêmea",
  "Não coletado" = "Não coletado"
    )
  ) %>% 
  select(peso_g, sexo_recode) %>% 
  group_by(sexo_recode) %>% 
  drop_na() %>% 
  summarise(
    n = sum(peso_g)
  )
```

    ## # A tibble: 3 x 2
    ##   sexo_recode         n
    ## * <chr>           <dbl>
    ## 1 Fêmea        7643750.
    ## 2 Macho        4498112.
    ## 3 Não coletado 8329769.

**Questão 04** </br> De acordo com os procedimentos realizados, com base
nos calculos foi perceptivel notar que teve 8552 peixes machos.

``` r
dados_prm %>% 
  distinct(habito_alimentar)
```

    ## # A tibble: 5 x 1
    ##   habito_alimentar
    ##   <chr>           
    ## 1 Onívoro         
    ## 2 Carnívoro       
    ## 3 Detritívoro     
    ## 4 Herbívoro       
    ## 5 Indeterminado

``` r
dados_prm %>% 
  mutate(
    sexo_recode = recode(
      sexo,
  "Fêmea" =  "Fêmea",
  "Macho"   =   "Macho",
  "Não coletado" = "Não coletado",
  "fêmea" = "Fêmea"
    ) 
  ) %>% 
  filter(habito_alimentar == "Carnívoro") %>% 
  group_by(sexo_recode) %>% 
  summarise(
    n = n()
  )
```

    ## # A tibble: 3 x 2
    ##   sexo_recode      n
    ## * <chr>        <int>
    ## 1 Fêmea        13903
    ## 2 Macho         8552
    ## 3 Não coletado 11476

**Questão 05** </br> De acordo com os calculos realizados a seguir
analisou-se que o maior rendimento líquido é de 7267672

``` r
dados_ch <- read_csv("dados/brutos/contracheque(1).csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_character()
    ## )
    ## i Use `spec()` for the full column specifications.

    ## Warning: 6638 parsing failures.
    ## row          col           expected actual                               file
    ## 412 cpf          delimiter or quote      ; 'dados/brutos/contracheque(1).csv'
    ## 412 lotacao      delimiter or quote      J 'dados/brutos/contracheque(1).csv'
    ## 412 lotacao      delimiter or quote      ; 'dados/brutos/contracheque(1).csv'
    ## 412 indenizacoes delimiter or quote      4 'dados/brutos/contracheque(1).csv'
    ## 412 indenizacoes delimiter or quote      ; 'dados/brutos/contracheque(1).csv'
    ## ... ............ .................. ...... ..................................
    ## See problems(...) for more details.

``` r
dados_ch %>% view()
```

``` r
dados_ch %>% 
  group_by(rendimento_liquido) %>% 
  summarise(n = n()) %>% 
  arrange(desc(rendimento_liquido))
```

    ## # A tibble: 1,205 x 2
    ##    rendimento_liquido                     n
    ##    <chr>                              <int>
    ##  1 Juiz do Trabalho Substituto            4
    ##  2 JUIZ DE DIREITO DE ENTRANCIA FINAL     1
    ##  3 9921.44                                1
    ##  4 97975.97                               1
    ##  5 9790.84                                1
    ##  6 9523.34                                1
    ##  7 93632.84                               1
    ##  8 93092.56                               1
    ##  9 926.32                                 3
    ## 10 9243.80                                1
    ## # ... with 1,195 more rows

**Questão 06**</br> De acordo com os procedimentos realizados a seguir
37334 magstrados recebem o valor acima de 39293.32.

``` r
dados_ch %>% 
  distinct(cargo) %>% view()
```

``` r
dados_ch %>% 
  filter(rendimento_liquido > 39293.32) %>% 
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   646

**a** </br> De acordo com os procedimentos realizados abaixo a
quantidade de 1136 magistrados receberam acima do valor de 100000

``` r
dados_ch %>% 
  filter(rendimento_liquido > 100000) %>% 
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  1790

**b**</br> Diates dos procedimentos realizados abaixo o tribunal
regional do trabalho de setima região (CE) é o que possui um maior valor
de variabilidade.

``` r
dados_ch %>% 
  distinct(tribunal)
```

    ## # A tibble: 24 x 1
    ##    tribunal                                                                     
    ##    <chr>                                                                        
    ##  1 "Superior Tribunal de Justiça"                                               
    ##  2 "Superior Tribunal Militar"                                                  
    ##  3 "Conselho Nacional de Justiça"                                               
    ##  4 "Tribunal Superior do Trabalho / Conselho Superior da Justiça do Trabalho"   
    ##  5 "Tribunal Superior Eleitoral"                                                
    ##  6 "Conselho da Justiça Federal"                                                
    ##  7 "Tribunal Regional Federal da 1a Região"                                     
    ##  8 "JUIZADO ESPECIAL CIVEL, CRIMINAL E DA FAZENDA PUBLICA DA COMARCA DE LARANJA~
    ##  9 "Tribunal Regional Eleitoral do Rio de Janeiro"                              
    ## 10  <NA>                                                                        
    ## # ... with 14 more rows

``` r
dados_ch %>%
  drop_na() %>%
  summarise(
    media_rendimento = mean(rendimento_liquido)
  )
```

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## # A tibble: 1 x 1
    ##   media_rendimento
    ##              <dbl>
    ## 1               NA

``` r
dados_ch %>% 
  group_by(tribunal) %>% 
  select(rendimento_liquido) %>% 
  drop_na() %>% 
  summarise(
    media_rendimento = mean(rendimento_liquido)
  )
```

    ## Adding missing grouping variables: `tribunal`

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## # A tibble: 23 x 2
    ##    tribunal                                                     media_rendimento
    ##  * <chr>                                                                   <dbl>
    ##  1 "Auxiliar Fixo - 14a.Vara do Trab.de Belo Horizonte,31a.Var~               NA
    ##  2 "Auxiliar Fixo - 1a.Vara do Trab.de Juiz de Fora,2a.Vara do~               NA
    ##  3 "Auxiliar Fixo - 20a.Vara do Trab.de Belo Horizonte,25a.Var~               NA
    ##  4 "Auxiliar Fixo - 20a.Vara do Trab.de Belo Horizonte,25a.Var~               NA
    ##  5 "Conselho da Justiça Federal"                                              NA
    ##  6 "Conselho Nacional de Justiça"                                             NA
    ##  7 "JUIZADO ESPECIAL CIVEL, CRIMINAL E DA FAZENDA PUBLICA DA C~               NA
    ##  8 "Superior Tribunal de Justiça"                                             NA
    ##  9 "Superior Tribunal Militar"                                                NA
    ## 10 "Tribunal Regional do Trabalho da 1ª Região"                               NA
    ## # ... with 13 more rows

``` r
dados_ch %>% 
  group_by(tribunal) %>% 
  select(rendimento_liquido) %>% 
  drop_na() %>% 
  summarise(
    dp_rendimento = sd(rendimento_liquido), dp_rendimento = n()) %>% arrange(desc(dp_rendimento))
```

    ## Adding missing grouping variables: `tribunal`

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## # A tibble: 23 x 2
    ##    tribunal                                         dp_rendimento
    ##    <chr>                                                    <int>
    ##  1 Tribunal Regional do Trabalho da 2ª Região                 808
    ##  2 Tribunal Regional do Trabalho da 1ª Região                 443
    ##  3 Tribunal Regional Eleitoral do Rio Grande do Sul           205
    ##  4 Tribunal Regional Federal da 1a Região                     184
    ##  5 Tribunal Regional Eleitoral do Rio de Janeiro              182
    ##  6 Tribunal Regional Eleitoral de Santa Catarina              146
    ##  7 Superior Tribunal de Justiça                                86
    ##  8 Tribunal Regional Eleitoral de Tocantins                    56
    ##  9 Tribunal Regional Eleitoral de Sergipe                      55
    ## 10 Superior Tribunal Militar                                   54
    ## # ... with 13 more rows

variabilidade do tribunal

``` r
dados_ch %>% 
  group_by(tribunal) %>% 
  select(rendimento_liquido) %>% 
  drop_na() %>% 
  summarise(
    cv = (sd(rendimento_liquido)/mean(rendimento_liquido)) * 100) 
```

    ## Adding missing grouping variables: `tribunal`

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## # A tibble: 23 x 2
    ##    tribunal                                                                   cv
    ##  * <chr>                                                                   <dbl>
    ##  1 "Auxiliar Fixo - 14a.Vara do Trab.de Belo Horizonte,31a.Vara do Trab.d~    NA
    ##  2 "Auxiliar Fixo - 1a.Vara do Trab.de Juiz de Fora,2a.Vara do Trab.de Ju~    NA
    ##  3 "Auxiliar Fixo - 20a.Vara do Trab.de Belo Horizonte,25a.Vara do Trab.d~    NA
    ##  4 "Auxiliar Fixo - 20a.Vara do Trab.de Belo Horizonte,25a.Vara do Trab.d~    NA
    ##  5 "Conselho da Justiça Federal"                                              NA
    ##  6 "Conselho Nacional de Justiça"                                             NA
    ##  7 "JUIZADO ESPECIAL CIVEL, CRIMINAL E DA FAZENDA PUBLICA DA COMARCA DE L~    NA
    ##  8 "Superior Tribunal de Justiça"                                             NA
    ##  9 "Superior Tribunal Militar"                                                NA
    ## 10 "Tribunal Regional do Trabalho da 1ª Região"                               NA
    ## # ... with 13 more rows

arranjo em ordem decrescente

``` r
dados_ch %>% 
  group_by(tribunal) %>% 
  select(rendimento_liquido) %>% 
  drop_na() %>% 
  summarise(
    cv_rendimento = (sd(rendimento_liquido)/mean(rendimento_liquido)) * 100) %>% 
arrange(desc(cv_rendimento))
```

    ## Adding missing grouping variables: `tribunal`

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in var(if (is.vector(x) || is.factor(x)) x else as.double(x), na.rm =
    ## na.rm): NAs introduzidos por coerção

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(rendimento_liquido): argument is not numeric or logical:
    ## returning NA

    ## # A tibble: 23 x 2
    ##    tribunal                                                        cv_rendimento
    ##    <chr>                                                                   <dbl>
    ##  1 "Auxiliar Fixo - 14a.Vara do Trab.de Belo Horizonte,31a.Vara d~            NA
    ##  2 "Auxiliar Fixo - 1a.Vara do Trab.de Juiz de Fora,2a.Vara do Tr~            NA
    ##  3 "Auxiliar Fixo - 20a.Vara do Trab.de Belo Horizonte,25a.Vara d~            NA
    ##  4 "Auxiliar Fixo - 20a.Vara do Trab.de Belo Horizonte,25a.Vara d~            NA
    ##  5 "Conselho da Justiça Federal"                                              NA
    ##  6 "Conselho Nacional de Justiça"                                             NA
    ##  7 "JUIZADO ESPECIAL CIVEL, CRIMINAL E DA FAZENDA PUBLICA DA COMA~            NA
    ##  8 "Superior Tribunal de Justiça"                                             NA
    ##  9 "Superior Tribunal Militar"                                                NA
    ## 10 "Tribunal Regional do Trabalho da 1ª Região"                               NA
    ## # ... with 13 more rows

**Questão 07**

``` r
dados_cp <- read_csv("dados/brutos/cursos-prouni.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   grau = col_character(),
    ##   turno = col_character(),
    ##   curso_busca = col_character(),
    ##   cidade_busca = col_character(),
    ##   uf_busca = col_character(),
    ##   cidade_filtro = col_character(),
    ##   universidade_nome = col_character(),
    ##   campus_nome = col_character(),
    ##   nome = col_character()
    ## )
    ## i Use `spec()` for the full column specifications.

    ## Warning: 380 parsing failures.
    ## row col   expected    actual                             file
    ##  17  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ##  39  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 445  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 549  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 550  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## ... ... .......... ......... ................................
    ## See problems(...) for more details.

``` r
dados_cp %>% view()
```

**a**</b> Curso integral.

**b** </br> Calculo da media e mediana.

``` r
dados_cp %>% 
  group_by(turno) %>% 
  select(nota_integral_ampla) %>%
  drop_na() %>% 
  summarise(
    media_intgegral = mean(nota_integral_ampla)
  )
```

    ## Adding missing grouping variables: `turno`

    ## # A tibble: 5 x 2
    ##   turno             media_intgegral
    ## * <chr>                       <dbl>
    ## 1 Curso a Distância            545.
    ## 2 Integral                     663.
    ## 3 Matutino                     609.
    ## 4 Noturno                      602.
    ## 5 Vespertino                   622.

``` r
dados_cp %>% 
  group_by(turno) %>% 
  select(nota_integral_ampla) %>%
  drop_na() %>% 
  summarise(
    mediana_intgegral = median(nota_integral_ampla))
```

    ## Adding missing grouping variables: `turno`

    ## # A tibble: 5 x 2
    ##   turno             mediana_intgegral
    ## * <chr>                         <dbl>
    ## 1 Curso a Distância              552 
    ## 2 Integral                       657.
    ## 3 Matutino                       610.
    ## 4 Noturno                        602.
    ## 5 Vespertino                     621.

**c** </br> Calculo do turno que possui a menor homogenidade.

``` r
dados_cp %>% 
  group_by(turno) %>% 
  select(nota_integral_ampla) %>% 
  drop_na() %>% 
  summarise(
    dsp_integral = sd(nota_integral_ampla))
```

    ## Adding missing grouping variables: `turno`

    ## # A tibble: 5 x 2
    ##   turno             dsp_integral
    ## * <chr>                    <dbl>
    ## 1 Curso a Distância         53.2
    ## 2 Integral                  57.9
    ## 3 Matutino                  43.5
    ## 4 Noturno                   41.3
    ## 5 Vespertino                41.0

``` r
dados_cp %>% 
  group_by(turno) %>% 
  select(nota_integral_ampla) %>% 
  drop_na() %>% 
  summarise(
    cv = (sd(nota_integral_ampla)/mean(nota_integral_ampla)) * 100
  )
```

    ## Adding missing grouping variables: `turno`

    ## # A tibble: 5 x 2
    ##   turno                cv
    ## * <chr>             <dbl>
    ## 1 Curso a Distância  9.77
    ## 2 Integral           8.73
    ## 3 Matutino           7.15
    ## 4 Noturno            6.86
    ## 5 Vespertino         6.58

Depois de realzar os procedimentos dispostos acima, foi possível chegar
a conclusão de que o turno com menor homogênidade é o vespertino.

**Questão 08**

De acordo com analíse dos dados, foi possível indentificar que o estado
da Bahia está ocupando a quinta posicão.

``` r
dados_cp <- read_csv("dados/brutos/cursos-prouni.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   grau = col_character(),
    ##   turno = col_character(),
    ##   curso_busca = col_character(),
    ##   cidade_busca = col_character(),
    ##   uf_busca = col_character(),
    ##   cidade_filtro = col_character(),
    ##   universidade_nome = col_character(),
    ##   campus_nome = col_character(),
    ##   nome = col_character()
    ## )
    ## i Use `spec()` for the full column specifications.

    ## Warning: 380 parsing failures.
    ## row col   expected    actual                             file
    ##  17  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ##  39  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 445  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 549  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## 550  -- 20 columns 1 columns 'dados/brutos/cursos-prouni.csv'
    ## ... ... .......... ......... ................................
    ## See problems(...) for more details.

``` r
dados_cp %>% 
  group_by(uf_busca) %>%
  summarise(
    y = n()
  ) %>% 
  arrange(desc(y))
```

    ## # A tibble: 28 x 2
    ##    uf_busca     y
    ##    <chr>    <int>
    ##  1 SP       11501
    ##  2 MG        4128
    ##  3 PR        3821
    ##  4 RS        3060
    ##  5 BA        2496
    ##  6 SC        2160
    ##  7 RJ        1425
    ##  8 GO        1238
    ##  9 PA        1192
    ## 10 PE        1148
    ## # ... with 18 more rows

``` r
dados_cp %>% 
  distinct(uf_busca)
```

    ## # A tibble: 28 x 1
    ##    uf_busca
    ##    <chr>   
    ##  1 MS      
    ##  2 CE      
    ##  3 SP      
    ##  4 AC      
    ##  5 MG      
    ##  6 MT      
    ##  7 BA      
    ##  8 RJ      
    ##  9 PA      
    ## 10 PR      
    ## # ... with 18 more rows

**Questão 09**</br> Nessa questão utilizou-se os dados do curso proune e
foi realizado o procedimento abaixo para poder selecionar os cursos que
não tem repetições, dessa forma intificou-se a quantidade de 296 cursos
distintos.

``` r
dados_cp %>%
  distinct(nome) %>% view()
```

**Questão 10** </br> De acordo com a analise dos grafícos que
representam os cursos de Direito e Medicina, foi perceptivel a conclusão
de que ambos grafícos são uma curtose, o grafíco correspondente ao curso
de Direito é chamado de palsticúrtica, isso quer dizer que os dados
numericos desse grafíco estão destribuidos de forma dispersas, o que
pode-se levar a entender também que o desvio padrão é consideravelmente
alto. No grafíco que representa o curso de medicina podemos observar que
a representação dele tem um pico elevado, essa representação demonstra a
simetria entre as partes graficas, se tracejarmos uma linha dividindo ao
meio o grafíco as partes será simetricas, o que induz afirmar que os
valores da média, moda e mediana são parecidos.
