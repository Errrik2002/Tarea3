expresion <-read.csv("rawdata/expression_matrix (1).tsv",dec=",",sep="\t")
View(expresion)


colnames(expresion) <- namesbase

namesbase <- c("ID_REF", "WT1", 'WT2', "WT3", "KO1", "KO2", "KO3")


expresion1 <- t(expresion)

View(expresion1) ############esto ya esta bien, toda la pinche tarde para esto alv 

head(expresion)




WT <- subset(expresion, select= c("ID_REF", "WT1", "WT2", "WT3"))

WTT <- t(WT)



KO <- subset(expresion, select= c("ID_REF","KO1", "KO2", "KO3"))

KOT <- t(KO)


WTT[1,3]
mean(as.numeric(WTT[-1,1]))

WTT[1,1]

length(colnames(WTT))


#####Calcula la media de expresión para cada gen en ambas condiciones.

####Usar la base transpuesta de cada condicion del KO y el WT
promexpre <- function(a){
x <- 1
for (i in 1:length(colnames(a))) {
  
  
  
  print(paste0(a[1,x] , " tiene un promedio de expresion de " , mean(as.numeric(a[-1,x]))))
  
  x <- x+1
  
}
}

promexpre(KOT)


####################
#Calcula el log2FC para cada gen como la diferencia en los logaritmos de la media de expresión de ambas
#condiciones.Utiliza un faunción definida por ti para realizar este cálculo.


# a = base de referencia o sea WT
# b = base de cambio o sea KO
# psicion del gen que quieres ver el cambio 


Log2FC <- function(a, b, c ){
  c <- as.numeric(c)
  
  
 wt <- mean(as.numeric(a[-1,c]))  
 print(paste0(a[1,c] , " tiene un promedio de expresion de " , wt , " es la referencia"))
    
ko <- mean(as.numeric(b[-1,c]))
 print(paste0(b[1,c] , " tiene un promedio de expresion de " , ko , " es el cambio"))  
  

cambio <- ko/wt

FC <- log2(cambio)

print(paste0("El Log Fold Change del gen ", a[1,c], " es de ", FC))


if(FC<1){
  print("La expresion disminuyo :c")
}

if(FC>1){
  print("La expresion aumento c:")
}

}

Log2FC(WTT, KOT, 47)


#################
#Identifica los genes que están diferencialmente expresados mediante una prueba estadística adecuada.


expresdif <- function(a,b){
  x <- 1
  for (i in 1:length(colnames(a))) {
    
    pruebat <- t.test(as.numeric(a[-1,x]), as.numeric(b[-1,x]))
    valorP <- as.numeric(pruebat[3]) #me da el valor de P

    if(valorP<0.05){
      print(paste0("el gen ", a[1,x] ," esta diferencialmente expreado respecto al control, con valor de P de ", valorP))
    }
    
    x <- x+1
  }
  
}

expresdif(WTT, KOT)  ###########DUDAAAAAA















