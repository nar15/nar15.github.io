``` r
library(ggplot2)
library(reshape)
```

#Semana 1

El objetivo sera tener una idea de **como es la salida de personas de la
malla triangular.**

Al comienzo de la simulacion tenemos un numero de gente distribuida por
las calles —habitaciones— de la malla, que se comportaran segun los
parametros del modelo, o segun su perfil particular (podemos distinguir
distintos tipos de agente segun altura, velocidad, etc.).

Ademas, a lo largo del tiempo aparecen nuevos agentes a un ritmo
constante de 0.1744 (se puede cambiar) personas por segundo en los
rectangulos designados para tal fin (dichas areas se encuentran en los
limites del casco historico -nuestra zona de estudio- y el resto de la
ciudad).

De la misma manera, los agentes pueden salir de la malla triangular tras
haber dado un paseo por la zona. En este momento, cada agente tiene una
probabilidad del 10% de salir por cada una de las puertas de salida.

------------------------------------------------------------------------

Para comenzar, realizamos una simulacion muy simple con las siguientes
**condiciones de partida**:

-   **NO** hay ningún agente en el interior de la malla al inicio

-   Un unico punto de entrada de personas en *Porta Faxeira* (creo que
    era esa)

-   Todas las salidas abiertas (10 en total, también la de entrada)

-   Duracion: 10 hora (36000 seg.)

Pathfinder nos devuelve un documento CSV de dimensión 36000 × 973 donde:

-   Cada **fila** es un instante de tiempo *t* = 0, 1, 2, ...360000
-   Cada **columna** mide un dato de interes a lo largo del tiempo. Hay
    muchas columnas sobrantes entre las 973. Las de mayor interés para
    nosotros serán:
    -   time(s) ; El instante t de tiempo
    -   “Remaining (Total)” ; El numero de agentes dentro de la malla en
        el instante t
    -   “Exited (Total)” ; El numero de agentes que han salido hasta el
        instante t (acumulativo)
    -   “SALIDA.NOMBRE” ; El numero de salidas por la salida *NOMBRE*
        hasta el instante t

Leo todo el CSV y selecciono las columnas que me interesan (manualmente,
haciendo referencia al numero de columna). Esto tengo que cambiarlo
porque si se añaden mas columnas por el medio (se añaden más puertas)
entonces ya no sería el mismo número de columna. Esto es, a ver si
mejoro la forma en el que lee el csv..

Ya lo he cambiado, se dividen las columnas segun el nombre de columna,
usando la funcion `grepl()`. Primero eliminamos las columnas referentes
a las puertas del interior de la malla triangular, que no nos interesan
ahora mismo (`todo[,!grepl("^Door",names(todo))]]` elimina las columnas
que contienen la palabra *Doors* al inicio del nombre de columna).
Seguimos teniendo algunas columnas

Despues, se crean dos dataframes distintos, `datos1` y `datos2`; el
primero contiene el numero de personas dentro de la malla y las salidas
totales; el segundo tiene el numero de salidas por cada puerta.
Seguramente existe una forma mucho mas eficiente de hacerlo…

``` r
todo<-read.csv("C:/Users/arnic/OneDrive/Escritorio/PRACTICAS_CITMAGA/MODELO_1_CASCO_CALLES_doors.csv",header=T)
todo<-todo[,!grepl("^Door",names(todo))] #quitamos las columnas que no nos interesan (las que tienen "Door")
attach(todo)


datos1<-todo[,!grepl("*SALIDA",names(todo))]
datos2<-todo[,grepl("*time",names(todo))|grepl("*SALIDA",names(todo))&!grepl("*boundary",names(todo))&!grepl("*width",names(todo))]
datos2[,-1] <- lapply(datos2[-1], cumsum)
rm(todo)

colnames(datos1)
```

    ## [1] "time.s."           "Remaining..Total." "Exited..Total."

``` r
colnames(datos2)
```

    ##  [1] "time.s."              "SALIDA.CAMINO"        "SALIDA.CARRETAS"     
    ##  [4] "SALIDA.ENSINANZA"     "SALIDA.FRANCISCO"     "SALIDA.MAZARELOS"    
    ##  [7] "SALIDA.PENA"          "SALIDA.PORTA.FAXEIRA" "SALIDA.PRAZA.GALICIA"
    ## [10] "SALIDA.ROQUE"         "SALIDA.TRINIDADE"

Siempre esta bien representar los datos con una grafica

``` r
ggdatos<- melt(datos1, id.vars = "time.s.")
ggplot(ggdatos, aes(x = time.s., y = value, color= variable)) +
  geom_line() 
```

![](test_md_files/figure-markdown_github/unnamed-chunk-3-1.png)

Vemos que el número de personas dentro de la malla (ROJO) se estabiliza
rápidamente —los que entran por los que salen—. Observamos el final de
la simulacion, vemos que el numero de personas se mantiene en torno a
los 70. Y que en 10 horas han pasado por la malla triangular 5768
agentes. Los atractores nos permiten que los agentes se queden
“atrapados” en lugares de interes durante mas tiempo, aportando, quizas,
mas realismo a las simulaciones. Pero eso es otro tema…

``` r
k<-length(datos1$time.s.)
datos1[seq(k-30,k,1),]
```

    ##        time.s. Remaining..Total. Exited..Total.
    ## 35972 35971.00                70           5764
    ## 35973 35972.00                71           5764
    ## 35974 35973.00                71           5764
    ## 35975 35974.00                71           5764
    ## 35976 35975.00                70           5765
    ## 35977 35976.00                70           5765
    ## 35978 35977.00                70           5765
    ## 35979 35978.00                71           5765
    ## 35980 35979.00                71           5765
    ## 35981 35980.00                71           5765
    ## 35982 35981.00                71           5765
    ## 35983 35982.00                71           5765
    ## 35984 35983.00                71           5765
    ## 35985 35984.00                72           5765
    ## 35986 35985.00                72           5765
    ## 35987 35986.00                72           5765
    ## 35988 35987.00                72           5765
    ## 35989 35988.00                71           5766
    ## 35990 35989.00                71           5766
    ## 35991 35990.00                71           5767
    ## 35992 35991.00                71           5767
    ## 35993 35992.00                71           5767
    ## 35994 35993.00                71           5767
    ## 35995 35994.00                71           5767
    ## 35996 35995.00                71           5767
    ## 35997 35996.00                71           5768
    ## 35998 35997.00                71           5768
    ## 35999 35998.00                71           5768
    ## 36000 35999.00                71           5768
    ## 36001 36000.00                71           5768
    ## 36002 36000.03                71           5768

------------------------------------------------------------------------

Otras columnas del documento CSV nos dan información más precisa de
cuantas personas salieron por cada una de nuestras 10 salidas en cada
instante t. Esto nos interesara mas, si queremos estudiar el
comportamiento de la salida de los agentes. Estas columnas son las que
hemos guardado en `datos2`: “SALIDA CARRETAS”, “SALIDA ENSINANZA”,…
Utilizamos `cumsum()` y `lapply()` para ir acumulando el número de
salidas a lo largo del tiempo.

``` r
ggdatos<- melt(datos2, id.vars = "time.s.")
ggplot(ggdatos, aes(x = time.s., y = value, color = variable)) +
  geom_line()
```

![](test_md_files/figure-markdown_github/unnamed-chunk-5-1.png)

------------------------------------------------------------------------

Al final de la simulacion, el numero de personas que salieron por cada
puerta es:

``` r
fin.exits<-datos2[k,-1]
fin.exits
```

    ##       SALIDA.CAMINO SALIDA.CARRETAS SALIDA.ENSINANZA SALIDA.FRANCISCO
    ## 36002           526             598              580              597
    ##       SALIDA.MAZARELOS SALIDA.PENA SALIDA.PORTA.FAXEIRA SALIDA.PRAZA.GALICIA
    ## 36002              581         562                  565                  579
    ##       SALIDA.ROQUE SALIDA.TRINIDADE
    ## 36002          602              578

``` r
sum(fin.exits)
```

    ## [1] 5768

En efecto si los sumamos todos obtenemos 5768, como a aparece en la
columna \`Exited..Total.´. Supongo que las variaciones en el numero de
salidas vendra dada por la forma de la malla, la situacion de los
atractores, y el propio comportamiento de Pathfinder. Podemos
presentarlo con un grafico de barras

``` r
barplot(as.numeric(fin.exits),names.arg=colnames(fin.exits),horiz=1,space=1,cex.names=0.5,las=2)
```

![](test_md_files/figure-markdown_github/unnamed-chunk-7-1.png)

# Cuestiones

-   ¿Podemos extraer alguna conclusion de este caso sencillo?
-   Considerar simulaciones menos degeneradas
-   Podemos utilizar la muestra que tenemos para acercar las
    simulaciones a la realidad (entiendo que no, puesto que solo
    contamos con una)
