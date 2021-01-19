# To select Samples
#install.packages("caret")
#install.packages("devtools")
#install.packages("Hmisc")
#install.packages("ggcorrplot")
#install.packages("caTools")
#install.packages("ROCR")
#install.packages("rpart.plot")
#install.packages("rpart.plot.version1")
#install.packages("e1071")
#install.packages("randomForest")
#install.packages("grDevices")

#install.packages("PerformanceAnalytics")

#repos <- getOption('repos')
#repos["CRAN"] <- "http://cran.rstudio.org"
#option(repos = repos)
#install.packages('UsingR')

# LOAD LIBS
library(caret)
library(devtools)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(caTools)
library(rpart)
library(cluster)
library (e1071) # NAIVE-BAYES
#library(rpart.plot)
library(randomForest)
library(ROCR)
library(grDevices)
library(neuralnet)

setRepositories()
#library("PerformanceAnalytics")
#ap <- available.packages()
#ap['rpart']
# LOAD FROM GIT - TESTE TESTE
devtools::install_github("rstudio/rstudioapi")

# SET Colors before of Work.. #?rainbow
color<-rainbow(22,s = 0.5)#rep(palette(),5)

### PERSONAL FUNCTION IN END OF THIS DOCUMENT

# Show 2 decimals
options("scipen" = 2)
#Set Work Space to Local DIR
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

"
PLANO DE VIABILIDADE (INICIAL)
1) Carregar os arquivos 'winequality-white.csv' e 'winequality-red.csv'.
2) Conhecer as dimensões iniciando a analise exploratória já se preparando para as proximas abordagens mais profundas.
3) Fazer conclusões e um novo plano
"
# ReadFiles
Wine_white <- read.csv("winequality-white.csv", sep=";", row.names=NULL)
Wine_red <- read.csv("winequality-red.csv", sep=";", row.names=NULL)

## KNOW DATA
# WINE WHITE
# Get dimensions
dim(Wine_white)   #4898 rows and 12 variables
head(Wine_white,10)
#Verify if has NA
sapply(Wine_white, function(x) sum(is.na(x)))
# WINE RED
# Get dimensions
dim(Wine_red)     #1599 rows and 12 variables
# Top 10
head(Wine_white,10)
#Verify if has NA
sapply(Wine_red, function(x) sum(is.na(x)))

# amostra of data
str(Wine_white)
str(Wine_red)

#SUMMARY
summary(Wine_white)
summary(Wine_red)
"
Não valores NA em nenhuma váriavel para ambos os grupos.
Possuem as mesmas variaveis, porém, com quantidade de registros diferentes

Observeri que citric.acid, density, pH e alcohol observei possuem mínimos, médias, médianas e máximos semelhantes entre os grupos (Whine e Red). 
Logo não realizarei analise de Outlier separadamente para este conjunto de variaveis  

Todas as outras variaveis (exceto estas citadas acima) possuem são bastante distintas entre esses dois grupos.
Então iremos normalizar e selecionar volume de registros iguais para produção do modelo

PLANO TÁTICO: 
1) Analisar Boxplot e Histograma de cada variavel separadamente por grupo.
2) Excluir outliers. 
3) Selecionar amostra de quantidades identicas tanto para White quanto para Red para treinamento do modelo.
4) Normalizar as variaveis e aplicar os modelos.
5) Merge da base para treino
"
# EXPLORATION FUNCTION
SHOW_PLOTS <- function(DBASE, XBASE='', ...){ 
  # Set Temporary Dataset
  xDBASE<-DBASE
  # Defaults
  dots <- list(...); xxcmd<-''; xxprt<-''; a<-0 ; dname<-substitute(DBASE); xname<-''; xFilter<-''; xDrop<-''; xKeep<-''; xChart<-'boxplot'
  ## FILTER DATASE
  if( any('xFilter'==names(dots)) ){
    if( nchar(dots$xFilter)>0 ){
      xcmd <- paste( "xDBASE<-xDBASE[xDBASE$", dots['xFilter'], ",]", sep="")
      eval( parse( text = xcmd ) )
      xFilter <- paste("[", dname, "$", dots['xFilter'], ",]", sep="")
    }
  }
  ## DATA FRAME TO COMPARE
  if( is.data.frame(XBASE)) xname<-substitute(XBASE)
  ## DROP VARIABLES
  if( any('xDrop'==names(dots)) ) xDrop<-dots$xDrop
  ## KEEP VARIABLES
  if( any('xKeep'==names(dots)) ) xKeep<-dots$xKeep
  ## CHART TYPE
  if( any('xChart'==names(dots)) ) xChart<-dots$xChart
  ## ACTION
  for ( i in names(xDBASE)){
    if( any(i==xDrop) ) next
    if( length(unlist(strsplit(xKeep, "")))>0 ) if( !(any(i==xKeep)) ) next
    a <- a + 1
    xcmd <- paste(xChart,"( ", "xDBASE", "$", i, ",col=color[", a, "]", ",main=\"", i, "\"", ",xlab=NULL", ',ylab=NULL',',cex.main=0.9', ")", ";\n", sep="")
    xxcmd <- paste(xxcmd, xcmd, sep = "")
    xprt <- paste(xChart,"( ", dname, xFilter, "$", i, ",col=color[", a, "]", ",main=\"", i, "\"", ")", ";\n", sep="")
    xxprt <- paste(xxprt, xprt, sep = "")
    # Grid Plot
    xprow <- switch(a, c(1,1), c(1,2), c(3,1), c(2,2), c(3,2), c(3,2), c(3,3), c(3,3), c(3,3), c(3,4), c(3,4), c(3,4), c(4,4), c(4,4), c(4,4), c(4,4))
    if(is.data.frame(XBASE)){
      a <- a + 1
      xcmd <- paste(xChart,"( ", xname, "$", i, ",col=color[", a-1, "]", ",main=\"", i, "\"", ",xlab=NULL", ',ylab=NULL',',cex.main=0.9', ")", ";\n", sep="")
      xxcmd <- paste(xxcmd, xcmd, sep = "")
      # Grid Plot
      xprow <- switch(a, c(1,1), c(1,2), c(3,1), c(2,2), c(3,2), c(3,2), c(2,4), c(2,4), c(3,4), c(3,4), c(3,4), c(3,4), c(4,4), c(4,4), c(4,4), c(4,4))
    }
  }
  # Show Grid Graphs
  # print(a); print(xprow)
  # Margin Plot
  par(mar=c(0,2,2,1)+ 0.2,oma=c(.2,.2,.4,.2))
  if(xChart=='hist'){par(mar=c(1.5,2,2,1) + 0.3)}
  par (mfrow=xprow)
  # Execute Command
  eval(parse(text = xxcmd))
  # Set Title
  if('xTitle' %in% names(dots)) if(nchar(dots$xTitle)>0) title(dots$xTitle, line = -0.5, outer = TRUE)
  # Print Commands
  cat(xxprt)
  # Remove Temporary DataSet
  rm("xDBASE")
}
# WINE WHITE
SHOW_PLOTS(Wine_white, xTitle="WINE WHITE - Histogram", xDrop=c("Wine","ID"), xChart='hist')
SHOW_PLOTS(Wine_white, xTitle="WINE WHITE - BoxPlot", xDrop=c("Wine","ID"), xChart='boxplot')
# WINE RED
SHOW_PLOTS(Wine_red, xTitle="WINE RED - Histogram", xDrop=c("Wine","ID"), xChart='hist')
SHOW_PLOTS(Wine_red, xTitle="WINE RED - BoxPlot", xDrop=c("Wine","ID"), xChart='boxplot')

"
Resultado:
Há outilers na maioria das variaveis nos dois grupos, estão vamos remove-los no próximo passo.
"
"
OUTLIERS ANALISYS
"
# OUTILERS FUNCITON (REMOVE)
SELECT_OUTLIERS <- function(x, na.rm = TRUE, ...) {
  # Defaults
  dots <- list(...);
  xMin<-0.01; if( any('xMin'==names(dots)) ) xMin<-dots$xMin
  xMax<-0.99; if( any('xMax'==names(dots)) ) xMax<-dots$xMax
  # Action
  qnt <- quantile(x, probs=c(xMin, xMax), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  #y
  #PRINT RESULT
  par(mar=c(5,5,3,5))
  par (mfrow=c(2,2))
  hist(x,main = 'before');boxplot(x)
  hist(y,main = 'after');boxplot(y)
  title(substitute(x), line = -1, outer = TRUE)
  # HOW USE: #set.seed(1); x <- rnorm(100); x #y <- SELECT_OUTLIERS(x); y
  y
}
# New DataSets to ajust
outliers.white <- Wine_white
outliers.red <- Wine_red
## SELECT OUTLIERS
#  WINE WHITE
outliers.white$fixed.acidity<-SELECT_OUTLIERS(Wine_white$fixed.acidity,xMin=.1,xMax=.90)
outliers.white$volatile.acidity<-SELECT_OUTLIERS(Wine_white$volatile.acidity,xMin=.0,xMax=.85)
outliers.white$residual.sugar<-SELECT_OUTLIERS(Wine_white$residual.sugar,xMin=.0,xMax=.80)
outliers.white$chlorides<-SELECT_OUTLIERS(Wine_white$chlorides,xMin=.01,xMax=.90)
outliers.white$free.sulfur.dioxide<-SELECT_OUTLIERS(Wine_white$free.sulfur.dioxide,xMin=.0,xMax=.9)
outliers.white$total.sulfur.dioxide<-SELECT_OUTLIERS(Wine_white$total.sulfur.dioxide,xMin=.2,xMax=.88)
outliers.white$sulphates<-SELECT_OUTLIERS(Wine_white$sulphates,xMin=.1,xMax=.90)
#  WINE RED
outliers.red$fixed.acidity<-SELECT_OUTLIERS(Wine_red$fixed.acidity,xMin=.0,xMax=.90)
outliers.red$volatile.acidity<-SELECT_OUTLIERS(Wine_red$volatile.acidity,xMin=.0,xMax=.85)
outliers.red$residual.sugar<-SELECT_OUTLIERS(Wine_red$residual.sugar,xMin=.0,xMax=.88)
outliers.red$chlorides<-SELECT_OUTLIERS(Wine_red$chlorides,xMin=.01,xMax=.85)
outliers.red$free.sulfur.dioxide<-SELECT_OUTLIERS(Wine_red$free.sulfur.dioxide,xMin=.0,xMax=.85)
outliers.red$total.sulfur.dioxide<-SELECT_OUTLIERS(Wine_red$total.sulfur.dioxide,xMin=.0,xMax=.88)
outliers.red$sulphates<-SELECT_OUTLIERS(Wine_red$sulphates,xMin=.0,xMax=.95)
## DISPLAY RESULT: Before x After without Outliers
#  WINE WHITE
SHOW_PLOTS(Wine_white,outliers.white, xTitle="COMPARE: WINE WHITE x without Outilers, Histogram", xDrop=c("Wine","ID"), xKeep=c("fixed.acidity","residual.sugar","volatile.acidity","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","sulphates"), xChart='hist')
SHOW_PLOTS(Wine_white,outliers.white, xTitle="COMPARE: WINE WHITE x without Outilers, Boxplot", xDrop=c("Wine","ID"), xKeep=c("fixed.acidity","residual.sugar","volatile.acidity","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","sulphates"), xChart='boxplot')
#  WINE RED
SHOW_PLOTS(Wine_red,outliers.red, xTitle="COMPARE: WINE RED x without Outilers, Histogram", xDrop=c("Wine","ID"), xKeep=c("fixed.acidity","residual.sugar","volatile.acidity","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","sulphates"), xChart='hist')
SHOW_PLOTS(Wine_red,outliers.red, xTitle="COMPARE: WINE RED x without Outilers, Boxplot", xDrop=c("Wine","ID"), xKeep=c("fixed.acidity","residual.sugar","volatile.acidity","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","sulphates"), xChart='boxplot')
## REMOVE OUTLIERS of SMILARS EXCEPTIONS : Médias, medianas, mínimos e máximo bastante similares, faremos uma analise mais cuidadosa
## Display exceptions var outliers 
SHOW_PLOTS(Wine_white,Wine_red, xTitle="COMPARE: WHITE x RED, Boxplot", xDrop=c("Wine","ID"), xKeep=c('citric.acid', 'density', 'pH', 'alcohol'), xChart='boxplot')
SHOW_PLOTS(Wine_white,Wine_red, xTitle="COMPARE: WHITE x RED, Boxplot", xDrop=c("Wine","ID"), xKeep=c('citric.acid', 'density', 'pH', 'alcohol'), xChart='hist')
#  REMOVE OUTLIERS of citric.acid
outliers.white$citric.acid<-SELECT_OUTLIERS(Wine_white$citric.acid,xMin=.25,xMax=.96)
outliers.red$citric.acid<-SELECT_OUTLIERS(Wine_red$citric.acid,xMin=.0,xMax=.85)
#  REMOVE OUTLIERS of density
outliers.white$density<-SELECT_OUTLIERS(Wine_white$density,xMin=.0,xMax=.75)
outliers.red$density<-SELECT_OUTLIERS(Wine_red$density,xMin=.0,xMax=.85)
#  REMOVE OUTLIERS of pH
outliers.white$pH<-SELECT_OUTLIERS(Wine_white$pH,xMin=.2,xMax=.95)
outliers.red$pH<-SELECT_OUTLIERS(Wine_red$pH,xMin=.03,xMax=.85)
#  REMOVE OUTLIERS of alcohol
outliers.white$alcohol<-SELECT_OUTLIERS(Wine_white$alcohol,xMin=.0,xMax=.65)
outliers.red$alcohol<-SELECT_OUTLIERS(Wine_red$alcohol,xMin=.0,xMax=.9)
## DISPLAY RESULT
SHOW_PLOTS(outliers.white,outliers.red, xTitle="COMPARE: WINES WHITE x RED, Histogram", xDrop=c("Wine","ID"), xKeep=c('citric.acid', 'density', 'pH', 'alcohol'), xChart='hist')
SHOW_PLOTS(outliers.white,outliers.red, xTitle="COMPARE: WINES WHITE x RED, Boxplot", xDrop=c("Wine","ID"), xKeep=c('citric.acid', 'density', 'pH', 'alcohol'), xChart='boxplot')

# COUNT ROW WITH NA
row_na.white<-apply(outliers.white, 1, function(x){any(is.na(x))})
row_na.red<-apply(outliers.red, 1, function(x){any(is.na(x))})

# FILTER NA
filtred.white<-outliers.white[!row_na.white,]
filtred.red<-outliers.red[!row_na.red,]

# RESULT
cat('WHITE, original:',nrow(Wine_white),', NAs:',sum(row_na.white),', final:', nrow(filtred.white))
cat('RED, original:',nrow(Wine_red),', NAs:',sum(row_na.red),', final:', nrow(filtred.red))

# Comparando as bases sem os outliers
SHOW_PLOTS(outliers.white,outliers.red, xTitle="COMPARE without Outliers: WINES WHITE x RED, Histogram", xDrop=c("Wine","ID"), xKeep=c('fixed.acidity','volatile.acidity','citric.acid','residual.sugar','chlorides','free.sulfur.dioxide','total.sulfur.dioxide','density'), xChart='hist')
SHOW_PLOTS(outliers.white,outliers.red, xTitle="COMPARE without Outliers: WINES WHITE x RED, Histogram", xDrop=c("Wine","ID"), xKeep=c('pH','sulphates','alcohol'), xChart='hist')

"
Resultado:
Variaveis ficaram mais equilibradas após a remoção dos outliers.
Base WINE WHITE havia 4898, removendo os outilers ficaam 4428 (90%)
Base WINE RED havia 1599, removendo os outilers ficaam 1374 (86%)
"
"
BASE OF TRAIN
A seleção para nossa base de treino será aproximadamente de 70% sobre o grupo com menos registros.
Logo WINE RED possui a menor quantidade de registros, então 70% de 1374 é igual á 962.
"
# SELECTION TRAIN BASE FUNCTION
SELECT_RANDOW <- function(dbase, xreg, ...){
  dbase$IDxx <- seq.int(nrow(dbase))
  xxdb_train <- createDataPartition( dbase$IDxx, p=( xreg /nrow( dbase )), list=FALSE, times=1)
  xxdb <- dbase[xxdb_train,]
  xxdb <- xxdb[1:xreg,]
  # drop column
  xxdb$IDxx = NULL
  rm("xxdb_train")
  xxdb
}
# SELECIONANDO 962 REGISTROS
train.white<-SELECT_RANDOW(filtred.white,962)
train.red<-SELECT_RANDOW(filtred.white,962)
# VERIFICANDO O VOLUME SELECIONADO
dim(train.white)
dim(train.red)

#train.white$quality
#train.red$quality

# SET TYPE WINE IN DATASET, 0 = whine, 1 = red
train.white$wine <- 0 
train.red$wine <- 1

# MERGE DATASET
train.wine <- merge(train.white,train.red,all=TRUE)

# Histograma de qualidade 
hist(train.wine$quality, main = "Histograma de qualidade")

# DISPLAY VOLUME
dim(train.wine)

# SUMMARY OF TRAIN
summary(train.wine)

#Padronizar ou normalizar, # normalizado: média = 0, padronizar: valores escalares de 0 á 1
train.scale.wine<-train.wine
train.scale.wine[1:11] <- sapply(train.wine[1:11], function(x) scale(x))
train.stand.wine<-train.wine
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
train.stand.wine[1:11] <- sapply(train.wine[1:11], function(x) range01(x))

# Display Summary of Data Normalization
summary(train.scale.wine)

# Display Summary of Data Standartization
summary(train.stand.wine)

"
Resultado
Temos agora noss base de treino pronta para o aplicação dos modelos
"
"
CORRELAÇÃO
"
# VIEW CORRELATION
rcorr(as.matrix(train.wine))
# PLOT CORREELATION
corr <- round(cor(train.wine), 2)
p.mat <- cor_pmat(train.wine)
ggcorrplot(corr, hc.order = TRUE, type = "upper", lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"), p.mat = p.mat)


# COORELATION AND CLUSTER
# corrplot(corr, order = "hclust", addrect = 3)
"
Resultado
A variavel 'wine' que identifica o tipo de vinho (Whine ou Red) é insignificante para todas as outras variaveis, logo, ele também deverá ser desconsiderado no modelo.
Confirmando que o tipo de vinho não é uma variavel significativa para o modelo.
Como a correlção entre 'density' e residual sugar esta muito alta vamos padronizar a variavel 'density' para que ela não atrapalhe no modelo
"
# Padronizando 'Density'
train.wine.v2<-train.wine
train.wine.v2$density<- scale(train.wine$density)
train.wine.v2$density<-as.numeric(train.wine.v2$density)
"
BASE DE TESTE
Setando a base de teste
"
teste.wine <- merge(Wine_white,Wine_red,all=TRUE) 
dim(teste.wine) # 6495 * 80% = 5196
teste.wine<-SELECT_RANDOW(teste.wine,5196)
dim(teste.wine) # 6495 * 80% = 5196
"
PCA
"
v<-train.wine[c(0:11)]
prcomp(v, scale = T)
dev.off()
plot(prcomp(v, scale = T),main="PCA")
summary(prcomp(v, scale = T))
#biplot(prcomp(v, scale = TRUE))
"
RESULTADO
A variação proporcional encontrada pelo PCA em seu melhor componete é baixa. PC1 explica cerca de 31% da varianância total.
Visto isso não temos variaveis com grande vantagens para o modelo.
"
"
MODELOS ESCALARES
"
"
REGRESSAO LINEAR
"
train.wine.v2$density<-NULL
regressao_mult <- lm(quality~. , train.wine.v2)
summary(regressao_mult)
# Multiple R-squared:  0.2681,	Adjusted R-squared:  0.2635 
# SELECIONANDO VARIAVEIS COM STEPWISE
stepwise <- step(lm(quality~., train.wine.v2), direction="both")
summary(stepwise)
# Multiple R-squared:  0.2681,	Adjusted R-squared:  0.2646 
# VARIAVEIS SELECIONADAS
names(stepwise$coefficients)[-1]
cc <- stepwise$coefficients
eqn <- paste("Y =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "), " + e") 
# Equação regressão linear
print(eqn)
attach(teste.wine)
# Por da saia precisar ser em número inteiro incluímos a função CEILING na saída da equação da regressão
Y = ceiling(0.51 + 0.16 * fixed.acidity + -2.12 * volatile.acidity + 0.11 * residual.sugar + -3.3 * chlorides + 0.01 * free.sulfur.dioxide + 0.96 * pH + 0.76 * sulphates + 0.07 * alcohol + -0.67 * density)
Y <- ifelse(Y>9, 9, Y)
erro <- NULL
erro$x <- teste.wine$quality
erro$y <- Y
erro$z <- teste.wine$quality - Y
par(mfrow=c(1,1))
dev.off()
hist(erro$x,col="yellow",freq=T, xlab="Nota Qualidade", ylab="Frequencia", main="Histograma de acuracidade", las=1)
hist(erro$y,col=scales::alpha('skyblue',.3),freq=T,add=T )
# Matriz de confusão
table(teste.wine$quality,Y)
# ACURACIA
acuracia<- round( sum( ifelse(erro$x==erro$y, 1, 0))/length(erro$x) * 100 , 2)
print( acuracia )
# DEVIO PADRÃO DO ERRO
sd(erro$z,na.rm=T)
train.wine$wine <- NULL #OCULTAR ISSO DAQUI
"
Resultado
R-quadrado apresenta ~27% de explicação do modelo e R-ajustado ~26%, logo há baixo índice de assertividade.
Mesmo o desvio padrão do erro sendo baixo ~1, a acuracia de 38% ainda não é relevante. 
"

"
NAIVE-BAYES 0
"
#Normalizando a amostra de teste (a de treino já tem versão normalizada)
teste.scale.wine <- teste.wine
teste.scale.wine[1:11] <- sapply(teste.wine[1:11], function(x) scale(x))

# aplicação do algoritmo de Naive-bayes 
train.wine$quality <- as.factor(train.wine$quality)
naive <- naiveBayes(train.wine$quality~., data = train.wine)

#prevendo na base de teste - Não funcionou
nb.pred <- predict(naive, teste.wine)

#calculando acerto
nb.results = data.frame(vinhoBom = teste.wine$quality, pred = nb.pred)
nb.results$pred <- as.numeric(nb.results$pred)

# Matriz confusão
resumo = table(teste.scale.wine$quality,nb.pred)
resumo

train.wine$quality <- as.numeric(train.wine$quality)
"
RANDOM FOREST
"
#reduzindo a fórmula
model_rf <- randomForest(train.scale.wine$quality ~., data = train.scale.wine, method = "rf", mtry = 6 )
# Calculando o predito
pred_rf <- predict(model_rf, train.scale.wine, type= "class")
#Arredondar resultados eliminando as casas decimais para avaliação da acuracidade
pred_rf <- floor(pred_rf)

# Matriz de Confusão 
resumo<-table(pred_rf, train.scale.wine$quality)
resumo

"
ARVORE DE PREDIÇÃO
"
set.seed(123)
arvore <- rpart(quality ~ fixed.acidity +volatile.acidity +citric.acid +residual.sugar +chlorides +free.sulfur.dioxide +total.sulfur.dioxide +pH +sulphates +alcohol, data = train.wine.v2)
summary(arvore)
# PLOT
plot(arvore, uniform=TRUE, main="Classification Tree for Wine Quality")
text(arvore, use.n=TRUE, all=TRUE, cex=0.7, pretty = 1)
"
RESULTADO
De acordo com a árvore de decisão, para escolher o melhor vinho devemos seuiPara escolher o melhor vinho deve seguir 'alcohol'<10.62, 'volatile.acidity'>=0.234, alcohol>=9.05 e 'chlorides'>=0.055   
"

"
OBJETIVO 2
"
# Data set disponiveis
# train.wine, train.wine.v2, train.stand.wine, train.scale.wine, teste.wine

# CONSIDERANDO QUE QUALIDADE >= 6 É BOM e o restante NÃO É BOM
# Logo teremo que incluir essa variavel como dummy e nosso dataset de treino
train.wine$quality <- as.numeric(train.wine$quality)
WINE_BOM = ifelse(train.wine$quality>=6, 1, 0)
clas.train.wine<-train.wine;              clas.train.wine$quality<-WINE_BOM
clas.train.wine.v2<-train.wine.v2;        clas.train.wine.v2$quality<-WINE_BOM 
clas.train.stand.wine<-train.stand.wine;  clas.train.stand.wine$quality<-WINE_BOM
clas.train.scale.wine<-train.scale.wine;  clas.train.scale.wine$quality<-WINE_BOM
WINE_BOM = ifelse(teste.wine$quality>=6, 1, 0)
clas.teste.scale.wine<-teste.scale.wine;  clas.teste.scale.wine$quality<-WINE_BOM
clas.teste.wine<- teste.wine;             clas.teste.wine$quality<-WINE_BOM
"
REGRESSÃO LOGISTICA
"
str(clas.train.wine$quality)
# Para adiantar a analise já iremos fazer uso do stepwise para selecionar as variaveis mais significantes automaticamente
modelo_reglog<-step(glm(quality~.,data=clas.train.wine,family = binomial), direction="both")
summary(modelo_reglog)
# Variaveis selecionadas pelo modelo
names(modelo_reglog$coefficients[-1])
# Set dataset de teste
# clas.teste.wine<- teste.wine[c('volatile.acidity','residual.sugar','free.sulfur.dioxide','pH','sulphates','alcohol','density','quality')]

# Calculo da probabilidade de acerto
probabilidade <-predict(modelo_reglog, clas.teste.wine, type = "response")
predito <- ifelse(probabilidade >= 0.6,1,0)
# Apresentando resultos
resumo <- table(predito,clas.teste.wine$quality)
resumo
# matriz de confusão
resumo
tx_acerto <-(resumo[1]+resumo[4])/sum(resumo)
tx_acerto 
#TAXA DE ACERTO ~73%
"
RESULTADO
Com 7 variaveis selecionadas pela função stepwise conseguimos 73% de acerto.
E um bom modelo para ser colado em produção.
"
"
OBJETIVO 3
"
"
ANALISE PCA
"

# Set Class
wineClasses <- factor(clas.train.scale.wine$wine)
winePCA <- prcomp(scale(clas.train.scale.wine))
plot(winePCA$x[,1:2], col = wineClasses, main= "PCA")
pairs(clas.train.scale.wine, col = wineClasses, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("Cv1","Cv2","Cv3"), pch = 16, col = c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5)


"
ARVORE DE REGRESSAO
"

# grow tree 
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
             method="anova", data=cu.summary)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results   

# plot tree 
plot(fit, uniform=TRUE, 
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree2.ps", 
     title = "Pruned Regression Tree for Mileage")


"
KMEANS
"
#Apresetando variaveis
str(train.wine)
summary(train.wine)
# Escolhendo variaveis para montar o cluter
# pH,residual.sugar,alcohol,total.sulfur.dioxide
# Porque estas apresentam maiores distinções entre média, mínimo e máximo
# DataSet para o kmeans
data.kameans <- train.wine[c('pH','residual.sugar','alcohol','total.sulfur.dioxide')]
# Normalizando
data.kameans_str <- sapply(data.kameans, function(x) scale(x))
summary(data.kameans_str)
# Determinando o melhor valor de k
mydata <- data.kameans_str
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
# Plot K values
plot(1:15, wss, type="b", xlab="Numero de Clusters", ylab="Qtd de Grupos",col="skyblue",pch=12)
"
Logo o melhor resultado de K é entre 4 e 5, por estarem num ponto otimo para separação de grupos. A partir deste número o grupos ficam mais homegeneos e custa mais (processamento) realizar a clusterização.
Porém nos escolhemos cluster = 4, acreditamos que já seja suficente para analisar o modelo.
"
# Usando CLuster = 4
fit <- kmeans(mydata,4 ) 
data.kameans$cluster<-fit$cluster

ggplot(data.kameans,aes(pH,total.sulfur.dioxide,color=as.factor(cluster)))+geom_point()
"
Com estar duas variveis os grupos ficaram misturados
"
ggplot(data.kameans,aes(pH,alcohol,color=as.factor(cluster)))+geom_point()
"
Com a comparação entre pH e alcohol, podemos visualizar facilmente os 4 grupos
"
ggplot(data.kameans,aes(total.sulfur.dioxide,residual.sugar,color=as.factor(cluster)))+geom_point()
"
Com a comparação entre total.sulfur.dioxide e residual.sugar, somente o grupo 2 se desctacou
"

#--- INICIANDO PARTE DE REDES NEURAIS ---

#Transformando Nota em Categoria
train.scale.wine$vinhoBom = ifelse(train.wine$quality>=6, 1, 0)
teste.scale.wine$vinhoBom = ifelse(teste.wine$quality>=6, 1, 0)

#Gerando fórmula para utilização do Modelo com as mesmas já calculadas para a logistica
formula <- quality ~ volatile.acidity + residual.sugar + free.sulfur.dioxide + pH + sulphates + alcohol + density

# ################################## #
# #   Support Vector Machine (SVM) # #
# ################################## #  

#reutlizando as amostras de Treino já anteriormente separadas e normalizadas
clas.train.scale.wine$quality = as.numeric(clas.train.scale.wine$quality)
svm.forest <- svm ( formula, data =clas.train.scale.wine)  

#gerando predição
svm.pred <- predict (svm.forest,clas.train.scale.wine)

#calculando resultados
svm.results = NULL
svm.results <- data.frame(vinhoBom = clas.train.scale.wine$quality, pred = svm.pred)

#exibindo resultados para a base de treino
dev.off()
plot(svm.results$pred, svm.results$vinhoBom, col=c("yellow","blue"),pch = c(15,12), main="Resultado base de treino")

#verificando resultados com a base de Teste
svm.teste.pred = predict(svm.forest, clas.teste.scale.wine)
svm.teste.results = data.frame(vinhoBom = clas.teste.scale.wine$quality, pred = svm.teste.pred)

#plotando
plot(svm.teste.results$vinhoBom, type='p', col='red', main="Resultado base de treino")
points(svm.teste.results$pred, type='p', col='green')

# Calculando Erro quadratico médio
sqrt( mean( (svm.teste.results$vinhoBom - svm.teste.results$pred)^2 ) )
# Plotando matriz de confusão
svm_res <- ifelse(svm.teste.results$pred>0.6,1,0)
# Matriz de confusão
resumo<-table(clas.teste.scale.wine$quality, svm_res)
resumo
# Acuracia
tx_acerto <-(resumo[1]+resumo[4])/sum(resumo)
tx_acerto 


# ################################################### #
# #  REDES NEURAIS ARTIFICIAIS - MULTI LAYER PERCEPTRON   #  #
# ################################################### # rede neural

train.formula = vinhoBom ~ volatile.acidity + residual.sugar + free.sulfur.dioxide

# aplicação do algoritmo de redes neurais (MLP uma vez que a função de ativação é logística ) 
rn.florest <- neuralnet(train.formula, data = train.scale.wine , hidden=5, err.fct='sse' , act.fct='logistic', 
                       linear.output=TRUE)

plot(rn.florest)

#gerando amostra de teste com dados selecionados
teste.scale_sel.wine <- subset(teste.scale.wine, select = c('volatile.acidity','residual.sugar','free.sulfur.dioxide','pH','sulphates','alcohol','density','quality'))

#calculando resposta
rn.florest.results <- compute(rn.florest, teste.scale_sel.wine)
rn.results  = data.frame(vinhoBom = teste.scale.wine$vinhoBom, pred = rn.florest.results$net.result)

# comparando o  real com o predito
plot(rn.results$vinhoBom, type='p', col='red')
points(rn.results$pred,col='green')

#Calculando Erro quadratico médio
sqrt( mean( (rn.results$vinhoBom - rn.results$pred)^2 ) )

# Matriz de confusão
rn.results$pred_int = ifelse(rn.results$pred > 0.5, 1, 0)
mat_conf = table(rn.results$vinhoBom, rn.results$pred_int)

tx_acerto = (mat_conf[1] + mat_conf[4]) / sum(mat_conf) 
print(tx_acerto*100)

"
RANDOM FOREST
"
#Criando o modelo
model_rf <- randomForest(clas.train.scale.wine$quality ~., data = clas.train.scale.wine, mtry = 100 )
# Calculando o predito
pred_rf <- predict(model_rf, clas.train.scale.wine, type= "class")
#Arredondar resultados eliminando as casas decimais para avaliação da acuracidade
pred_rf <- floor(pred_rf)

# Matriz de Confusão 
resumo<-table(pred_rf, clas.train.scale.wine$quality)
resumo

# Acuracia
tx_acerto <-(resumo[1]+resumo[4])/sum(resumo)
tx_acerto 

"
Resultado: Com 37% de acuracia este modelo não é relevante.
"

# ################################################### #
# #                  NAIVE-BAYES                    # #
# ################################################### #

# Aplicação do algoritmo de Naive-bayes 
clas.train.scale.wine$quality <- as.factor(clas.train.scale.wine$quality)
naive <- naiveBayes(clas.train.scale.wine$quality~., data = clas.train.scale.wine)

#prevendo na base de teste - Não funcionou
nb.pred <- predict(naive, clas.teste.scale.wine)

#calculando acerto
nb.results = data.frame(vinhoBom = clas.teste.wine$quality, pred = nb.pred)

#Calculando Erro quadratico médio
sqrt( mean( (nb.results$vinhoBom - nb.results$pred)^2 ) )

# Matriz confusão
resumo = table(clas.teste.scale.wine$quality,nb.pred)
tx_acerto = (resumo[1] + resumo[4]) / sum(resumo)
tx_acerto