#Identifica un conjunto de datos de la base de datos GEO que contanga datos de expresión de genes para
#una enfermedad o condición que te interese, Usa la búsqueda en la caj de diállogo con palabras clave
#y operadores booleanos para ello. Pon un screenshot para la condicón seleccionada.

###SE USO UNA CONDICION "AND", SE OBSERVA EN LA IMAGEN "AND" EN LA CARPETA DE "image"

#ESTE FUE EL SCRIPT QUE SE GENERO

              ##################################
# Version info: R 4.2.2, Biobase 2.58.0, GEOquery 2.66.0, limma 3.54.0, DESeq2 1.38.3
################################################################
#   Differential expression analysis with limma
BiocManager::install("GEOquery")
BiocManager::install("limma")
BiocManager::install("umap")
BiocManager::install("maptools")
library(GEOquery)

library(limma)

library(umap)

# load series and platform data from GEO

gset <- getGEO("GSE35332", GSEMatrix =TRUE, AnnotGPL=TRUE)
if (length(gset) > 1) idx <- grep("GPL1261", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

# make proper column names to match toptable 
fvarLabels(gset) <- make.names(fvarLabels(gset))

# group membership for all samples
gsms <- "000XXX111XXX"
sml <- strsplit(gsms, split="")[[1]]

# filter out excluded samples (marked as "X")
sel <- which(sml != "X")
sml <- sml[sel]
gset <- gset[ ,sel]

# log2 transformation
ex <- exprs(gset)
qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
LogC <- (qx[5] > 100) ||
  (qx[6]-qx[1] > 50 && qx[2] > 0)
if (LogC) { ex[which(ex <= 0)] <- NaN
exprs(gset) <- log2(ex) }

# assign samples to groups and set up design matrix
gs <- factor(sml)
groups <- make.names(c("IL3","SCF AND IL3"))
levels(gs) <- groups
gset$group <- gs
design <- model.matrix(~group + 0, gset)
colnames(design) <- levels(gs)

gset <- gset[complete.cases(exprs(gset)), ] # skip missing values

fit <- lmFit(gset, design)  # fit linear model

# set up contrasts of interest and recalculate model coefficients
cts <- c(paste(groups[1],"-",groups[2],sep=""))
cont.matrix <- makeContrasts(contrasts=cts, levels=design)
fit2 <- contrasts.fit(fit, cont.matrix)


################################ESTADISTICAA##########
# compute statistics and table of top significant genes
fit2 <- eBayes(fit2, 0.01)
tT <- topTable(fit2, adjust="fdr", sort.by="B", number=250)

tT <- subset(tT, select=c("ID","adj.P.Val","P.Value","t","B","logFC","GB_ACC","SPOT_ID","Gene.Symbol","Gene.symbol","Gene.title"))
write.table(tT, file=stdout(), row.names=F, sep="\t")


################################################
# Visualize and quality control test results.
# Build histogram of P-values for all genes. Normal test
# assumption is that most genes are not differentially expressed.
tT2 <- topTable(fit2, adjust="fdr", sort.by="B", number=Inf)
hist(tT2$adj.P.Val, col = "grey", border = "white", xlab = "P-adj",
     ylab = "Number of genes", main = "P-adj value distribution")

# summarize test results as "up", "down" or "not expressed"
dT <- decideTests(fit2, adjust.method="fdr", p.value=0.05, lfc=0)

# Venn diagram of results
vennDiagram(dT, circle.col=palette())

# create Q-Q plot for t-statistic
t.good <- which(!is.na(fit2$F)) # filter out bad probes
qqt(fit2$t[t.good], fit2$df.total[t.good], main="Moderated t statistic")

# volcano plot (log P-value vs log fold change)
colnames(fit2) # list contrast names
ct <- 1        # choose contrast of interest
volcanoplot(fit2, coef=ct, main=colnames(fit2)[ct], pch=20,
            highlight=length(which(dT[,ct]!=0)), names=rep('+', nrow(fit2)))

# MD plot (log fold change vs mean log expression)
# highlight statistically significant (p-adj < 0.05) probes
plotMD(fit2, column=ct, status=dT[,ct], legend=F, pch=20, cex=1)
abline(h=0)

################################################################
# General expression data analysis
ex <- exprs(gset)

# box-and-whisker plot
ord <- order(gs)  # order samples by group
palette(c("#1B9E77", "#7570B3", "#E7298A", "#E6AB02", "#D95F02",
          "#66A61E", "#A6761D", "#B32424", "#B324B3", "#666666"))
par(mar=c(7,4,2,1))
title <- paste ("GSE35332", "/", annotation(gset), sep ="")
boxplot(ex[,ord], boxwex=0.6, notch=T, main=title, outline=FALSE, las=2, col=gs[ord])
legend("topleft", groups, fill=palette(), bty="n")

# expression value distribution
par(mar=c(4,4,2,1))
title <- paste ("GSE35332", "/", annotation(gset), " value distribution", sep ="")
plotDensities(ex, group=gs, main=title, legend ="topright")

# UMAP plot (dimensionality reduction)
ex <- na.omit(ex) # eliminate rows with NAs
ex <- ex[!duplicated(ex), ]  # remove duplicates
ump <- umap(t(ex), n_neighbors = 3, random_state = 123)
par(mar=c(3,3,2,6), xpd=TRUE)
plot(ump$layout, main="UMAP plot, nbrs=3", xlab="", ylab="", col=gs, pch=20, cex=1.5)
legend("topright", inset=c(-0.15,0), legend=levels(gs), pch=20,
       col=1:nlevels(gs), title="Group", pt.cex=1.5)

library("maptools")  # point labels without overlaps

pointLabel(ump$layout, labels = rownames(ump$layout), method="SANN", cex=0.6)

# mean-variance trend, helps to see if precision weights are needed
plotSA(fit2, main="Mean variance trend, GSE35332")



###########################
#ESTE FUE EL SCRIPT QUE RESULTO DE LA PAGINA GEO2R


#Preprocesa los datos de microarreglos con los métodos adecuados tales como normalización. #########      ##########

#EN LA IMAGEN "script1" se oberva la normalizacion 



#Usa el paquete GEO2R para hacer un análsis de expresión diferencial . 
#Cambia la prueba estadística, utiliza al menos tres distintas, y 
#el p-value umbral para comparar el número y cuáles genes diferencialmente  ########          #####################
#expresados existen por cada cambio. Para esta parte escribe código en R para
#comparar tales conjuntos.

#LAS IMAGENES "condiciones" CORRESPONDEN A LOS VALORES DE P y pruebas estadisticas que se realizaron



#En la imagen del volcano plot condicion1img se observa un volcano normal, pues las condiciones no fueron cambiadas

# En la del condiciones2img que tuvo un valor de P de 0.01, fue a lo mejor un valor muy especifico, que a lo mejor varios 
#genes no lograron alcanzar, debido a eso no se ven objetos de colores en el volcanoplot

#EN la imagen condiciones3img, se uso un valor de P mas alto, por lo que se ve que mas genes son capaces de 
#llegar a una diferencia significativa, pero no tanto como en las condiciones normales. 









#Para esta parte escribe código en R para comparar tales conjuntos.
#Para cada cambio selecciona los 10 genes cuya cambio es más significativo y lee cuál es su función.       ############      ############
#Compara tus resultados con los obtenidos en el artículo en donde se encuentran los datos.




#################### CARGAR LA BASE DE DATOS ############

#############3
script1 <-read.csv("rawdata/script1.tsv",dec=",",sep="\t")
script11 <- t(script1)

pvalores11 <- script1[,3]




genesmasexpresados <- function(x){
  
  pvalores11 <- x[,3]
  
masexpre111 <- sort(as.numeric(pvalores11), decreasing = F)[1:10] #sacar los genes con valor de P mas bajo
#implica un cambio mas significativo
library(dplyr)
y <- 1
mylistgene <- c()
for (i in 1:10) {
  
elgen <- x %>% filter_all(any_vars(. %in% c(masexpre111[y]))) #de los 10  valores que saco de Pvalue los busca 1 por uno en la base que cargamos
#por lo que nos da el rengon donde esta el nombre el ID
print(elgen)
y <- y+1
mylistgene[[i]] <- elgen #lo agregamos a una lista
}
mylistgene 

print(paste0("la lista de genes se guardo en el objeto mylistgene, imprimelo para consultarlo las veces que quieras"))

}

genesmasexpresados(script1) #tenemos la lista e imprimoimos y nos da el renglon del objeto con Pvalue mas bajos
edif1111 <- mylistgene
edif1111
# Rora RAR-related orphan receptor alpha
# Rora RAR-related orphan receptor alpha
# Rora RAR-related orphan receptor alpha
# Eya4 EYA transcriptional coactivator and phosphatase 4
# Plac8 placenta-specific 8
# Cntn4 contactin 4
# Tcrg-V4 T cell receptor gamma, variable 4
# Tmeff2 transmembrane protein with EGF-like and two follistatin-like domains 2
#  Rora RAR-related orphan receptor alpha
# Fblim1 filamin binding LIM protein 1


############
script2 <-read.csv("rawdata/script2.tsv",dec=",",sep="\t")
genesmasexpresados(script2)
edif2222 <- mylistgene
edif2222


#
#Rora RAR-related orphan receptor alpha
#Eya4 EYA transcriptional coactivator and phosphatase 4
#  Plac8 placenta-specific 8
# Cntn4 contactin 4
# Tcrg-V4 T cell receptor gamma, variable 4
# Tmeff2 transmembrane protein with EGF-like and two follistatin-like domains 2
# Fblim1 filamin binding LIM protein 1

################

script3 <-read.csv("rawdata/script3.tsv",dec=",",sep="\t")
genesmasexpresados(script3)


#
#
#
#  Rora RAR-related orphan receptor alpha
#  Tcrg-V4 T cell receptor gamma, variable 4
#  Tcrg-V4 T cell receptor gamma, variable 4
# Tmem176b transmembrane protein 176B
# Tcrg-V4///Trgv2 T cell receptor gamma, variable 4///T cell receptor gamma variable 2
#Calcrl calcitonin receptor-like
#  Tmem176a transmembrane protein 176A



#############Se Extraen los 10 genes con cambios mas significativos 
#de cada base de datos generada por cada condicion


#Rora RAR-related orphan recepto alpha. Es un miembro de los recpetores nucleares, receptor de hormonas
#que s epuede unir como monomero o homodimero a la hormona. Una ve activado por la hormona eleva la expresion de genes

#Eya4 EYA transcriptional coactivator and phosphatase 4
#Puede funcionar como activador transcripcional atra vez de su actividad fosfatasa y de importancia en el desarrollo
#del ojo

#Plac8 placenta-specific 8'
#Se puede unir a la cromatina, se ve envueta en la regulacion positiva de termogenesis y regulacion de la 
#transcripcion por la POL II. Ayuda  al diferenciacion de la Brown fat Cell

#Cntn4 contactin 4
#codifica para una proteina de la familia contractinas de inmunoglobulinas.
#Son moleculas de adhesion con funcion en la formacion de redes neuronales y su plasticidad

#Tcrg-V4 T cell receptor gamma, variable 4
#V region of the variable domain of T cell receptor (TR) gamma chain that participates in the antigen recognition (PubMed:24600447).


#Tmeff2 transmembrane protein with EGF-like and two follistatin-like domains 2
#codifica para una proteina de la familia tomeregulina, que son proteinas transmembranales,
#puede actuar como oncogen o tumor suppressor, depende del contexto celular.

#Fblim1 filamin binding LIM protein 1
#Codifica para una proteina de la region terminal de union de la flamina, 
#la proteina se liga con la adhesion celular a estructuras como citoesqueleto de actina. Union y estabilizacion de flamentos de actina

#Tmem176b transmembrane protein 176B
#localizado en la membrna nuclear y regula negativamente la diferenciacion de las celulas dendriticas 

#T cell receptor gamma variable 2
#Predicted to be active in external side of plasma membrane.
#V region of the variable domain of T cell receptor (TR) gamma chain that participates in the antigen recognition (PubMed:24600447).

#Calcrl calcitonin receptor-like
#Es el receptor de CGRP. La actividad de este esta regulada por las proteinas G. Respuesta a succrosa e internaliacion del receptor

#Tmem176a transmembrane protein 176A
#Similar  la Tmem176B, regula de manera negativa  ala diferenciacion de celulas dendriticas.





###AL COMPARAR LOS GENES DEL PAPER con los que obtuvimos, se dieorn diferentes genes, bueno en el paper solo menciona unos
#relacionado a una funcion importante, como Gnaq, que es un gen que si salio expresado diferencialmente, pero no d elos 10 primeros
#y es una subunidad de la proteina G acoplada a receptores. Pues lo que observaron ellos fue que hubo menor efecto de la desgranulacion de los 
#mastocitos cuando se exponian a IgE, que son los efecores a la desgranulacion que puede llevar a una reaccion alergica o anafilaxis.
#Pero este efecto de la baja desgranulacion no puede ser por la baja cantidad de receptores de IgE, pues la cantidad de receptores
#para IgE, son iguales en las que fueron control con IL3 y las tratadas con IL3 y SCF (Stem Cell factor). Por lo que se puede decir que no elimino al receptor
# si no que genero cambios o de subunidades de la proteina G, por lo que hay una menor expresion de una subunidad importante que
#funcionaba para la correcta se;alizacion de desgranulacion al mastocisto, al afectar esto, el mensaje de que el mastocisto
#sse debe de desgranular, es poca, por lo tanto no hay desgranulacion o al menos no una tan distinguible.










#A partir de una condicón específica, selecciona los genes sub expresados y 
#elabora una red de co-expresión con geneMANIA, ya sea en el sitio web o como
#plugin de Cytoscape. Consrtuye una red análoga con los sobre-expresados.


#LAS IMAGENES DE LAS REDES DE GENEMANIA ESTAN EN LA CARPETA images como
#sobreexpresadosGENEMANIA y analogo a los subexpresados


#SOBREEXPRESADOS 
#La red que formo es una red de baja densidad y de tipo scalefree, pues s eobservan
#nodos que tienen pocas conexiones mientras que otros tienen una alta cantidad de
#conexiones, se ve que Tmem176A, que es una proteina de membrana nuclear que afecta
#ela diferenciacion de las celulas dendriticas, seguramnete durante el proceso 
#de evitar la diferenciacion, afecta a otros varios genes de importancia para su desarrollo
#por eso es una de los genes con varias conexiones, un hub

#SUBEXPRESADOS
#Se observa cluster mas definidos de genes, al menos 4. Uno con varios genes que puede
#ener una alta densidad, mientras que los otros son de a lo mucho 6 genes, pero con alta densidad
#la mayoria conectan entre si. Gnaq es un gen que confroma el clustrs de varios genes
#la funcion de este gen es para una subunidad del receptor acoplado a proteina G
#se pofria intuir que todo el cluster esta relacionado al funcionamiento de la proteina G
#por que varios estan conectados entre si y con alta densidad. Podr[ia pensarse como subunidades
#de la proteina G y todo ese cluster es el receptor acoplado a proteina G






