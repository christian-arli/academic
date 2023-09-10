setwd("C:/Users/Riccardo/Downloads/HeartAttack")
df=read.csv("heart.csv")
library(tidyverse)
library(caret)
library(GGally)
library(corrplot)
library(clValid)
library(EMCluster)
library(dbscan)
library(caTools)
library(ROSE)
library(tidymodels)
library(pROC)
library(corrplot)
library(cluster)
library(highcharter)
library(e1071)
#Il set di dati ? formato da 303 osservazioni e 14 variabili. La variabile di risposta ? binaria: 1 rappresenta un rischio elevato di avere un infarto, 0 rappresenta un basso rischio di avere infarto.
#I predittori sono un mix di variabili continue e categoriche.
#Un attacco di cuore si verifica quando una o pi? arterie coronarie vengono bloccate. Nel corso del tempo un'arteria coronaria pu? restringersi a causa dell'accumulo di varie sostanze, tra cui il colesterolo (aterosclerosi). 

######DESCRIZIONE VARIABILI

#age => et? del paziente
#sex => sesso del paziente: donna -> 0
#                           uomo -> 1
#cp => Tipo di dolore al petto: angina tipica -> 0
#                               angina atipica ->1
#                               dolore non anginoso ->2
#                               asintomatico ->3
#
#trtbps => pressione sanguigna a riposo (in mmHg)
#chol => colesterolo in mg/dl recuperato tramite sensore BMI
#fbs => zucchero nel sangue a digiuno > 120 mg/dl: Vero -> 1
#                                                  Falso -> 0
#restecg => risultati elettrocardiografici a riposo: Normali -> 0
#                                                    Anomalo -> 1 (con anomalia dell'onda ST-T (inversione dell'onda T e/o elevazione o depressione ST > 0,05 mV)
#                                                    con ipertrofia ventricolare sinistra probabile o definita secondo i criteri di Estes -> 2
#thalachh => frequenza cardiaca massima raggiunta
#exng => angina indotta dall'esercizio
#oldpeak =>  depressione della ST indotta dall'esercizio ( rispetto alla misurazione a riposo )
#slp => la slope del segmento ST dell'esercizio di picco: upsloping -> 0
#                                                         piatta -> 1
#                                                         downsloping  -> 2
#caa =>  number of major vessels (0-3)
#thall => talassemia: nulla -> 0
#                     difetto fisso -> 1
#                     normale -> 2
#output => diagnosi di malattie cardiache: restringimento del diametro < 50%. Bassa probabilit? di malattie cardiache -> 0
#                                          restringimento del diametro > 50%. Pi? probabilit? di malattie cardiache -> 1
#0,5 ? scelto come valore soglia in termini di probabilit?. Non si tratta di sapere se qualcuno avr? o meno un infarto, ma di sapere quanto ? probabile.

####DEFINIZIONI MEDICHE UTILI 
#ANGINA: L'angina ? un dolore transitorio al torace o sensazione di pressione che si manifesta quando il muscolo cardiaco non riceve una sufficiente quantit? di ossigeno. 
#Un paziente con angina lamenta di solito fastidio o una pressione sotto lo sterno. L'angina insorge in risposta a uno sforzo e si attenua a riposo

#COLESTEROLO: Il colesterolo ? un grasso presente nel sangue che viene in gran parte prodotto dall'organismo, mentre in minima parte viene introdotto con la dieta.
#Mentre, in quantit? fisiologiche, il colesterolo ? coinvolto in diversi processi fondamentali per il funzionamento dell'organismo, quando ? presente in quantit? eccessiva costituisce uno dei fattori di rischio maggiori per le malattie cardiache.

#ECG: L'elettrocardiogramma ? la riproduzione grafica dell'attivit? elettrica del cuore registrata a livello della superficie del corpo.

#DEPRESSIONE DEL TRATTO ST: fa riferimento ad un'alterazione dell'elettrocardiogramma di superficie.

#TALASSEMIA: una malattia ereditaria del sangue dovuta alla sintesi ridotta o assente di una delle catene dell'emoglobina, proteina responsabile del trasporto di ossigeno attraverso tutto l'organismo.
attach(df)
#detach(df)


str(df)
df$sex=as.factor(sex)
levels(df$sex)=c("Donna","Uomo")
df$cp=as.factor(cp)
levels(df$cp)=c("Angina tipica","Angina atipica","Dolore non anginoso","Asintomatico")
df$fbs=as.factor(fbs)
levels(df$fbs)=c("Falso","Vero")
df$restecg=as.factor(restecg)
levels(df$restecg)=c("Normale","STT","Ipertrofia")
df$exng=as.factor(exng)
levels(df$exng)=c("No","Si")
df$slp=as.factor(slp)
levels(df$slp)=c("Upsloping","Flat","Downsloping")
df$caa=as.factor(caa)
df$thall=as.factor(thall)
df$output=as.factor(output)
levels(df$output)=c("BP","AP")

head(df)
anyNA(df) #no NA
summary(df)
str(df)
prop.table(table(output)) #le classi sembrano essere  bilanciate. Bene, proseguiamo
table(output)
data <- data.frame(
  output=c("Bassa Probabilit?","Alta probabilit?"),
  value=c(138,165)
)
ggplot(data,aes(x="",y=value,fill=output))+
  geom_bar(stat = "identity",width = 1,col="white")+
  coord_polar("y",start = 0)+theme_void()
ggplot(df,aes(output))+
  geom_bar(fill=c("turquoise4"),col="black")+
  geom_text(stat = "count",aes(label=..count..),vjust=1.3)+
  theme_classic()
ggplot(df)+
  geom_count(mapping = aes(output,sex),col=c("violet"))+
  theme_classic()
#poche donne con bassa probabilit? di malattie cardiache. Uomini rispetto a Y son ben bilanciati.
ggplot(df,aes(output,sex))+
  geom_boxplot(fill=output)+
  theme_classic()
ggpairs(df,columns = c(1,4,5,8,10,14),mapping = aes(col=(output)))
#le variabili numeriche non sembrano tra loro correlate. Buona cosa, evitiamo collinearit?.
correlazione=cor(df[,c(1,4,5,8,10)])
round(correlazione,2)
col <-colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                         "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                         "#4393C3", "#2166AC", "#053061"))
corrplot(correlazione, method="color", col=col(200),addCoef.col = "black" , 
         type="upper", order="hclust",cl.length = 6,number.cex = 0.6,number.digits = 1,
         number.font = 1,
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=F)


###AGE
summary(age)
ggplot(df,aes(age))+
  geom_histogram(col="black",fill="turquoise4",binwidth = 2.5)+
  geom_density(aes(y=2.5 * ..count..),col="red")+
  geom_vline(aes(xintercept = mean(age), color=c("mean")))+
  geom_vline(aes(xintercept = median(age), color=c("median")))+
  scale_x_continuous(breaks = c(seq(29,77,by=2)))+
  scale_color_manual(name="",values = c(mean="darkorange",median="turquoise3"))+
  theme_classic()
mean(age)
median(age)
ggplot(df,aes(age))+
  geom_boxplot(col="blue")+
  theme_classic()
which.max(table(age))
#L'et? media e mediana sono molto simili. La moda ? 58 anni. 

####SEX
ggplot(df,aes(sex))+
  geom_bar(col="black",fill=c("lightcoral","tomato4"))+
  geom_text(stat = "count",aes(label=..count..),vjust=+2)+
  theme_classic()
#La variabile sesso,  a differenza della variabile target, ? meno bilanciata. Il 68.3% degli individui ? di sesso maschile

###cp
ggplot(df,aes(cp))+
  geom_bar(col="black",fill=c("#F8766D","#B79F00", "#00BA38" ,"#00BFC4"))+
  geom_text(stat = "count",aes(label=..count..),vjust=+2)+
  theme_classic()

###trtbps
summary(trtbps)
ggplot(df,aes(trtbps))+
  geom_histogram(col="black",fill="turquoise4",binwidth = 2.5)+
  geom_density(aes(y=2.5 * ..count..),col="red")+
  geom_vline(aes(xintercept = mean(trtbps), color=c("mean")))+
  geom_vline(aes(xintercept = median(trtbps), color=c("median")))+
  scale_x_continuous(breaks = c(seq(94,200,by=10)))+
  scale_color_manual(name="",values = c(mean="darkorange",median="turquoise3"))+
  theme_classic()
mean(trtbps)
median(trtbps)




###chol
# valori del colesterolo totale, non dovrebbero mai superare i 200 mg/dl;tra 200 e 239 si parla di colesterolo moderatamente alto.. Al di sopra dei 240 mg/dl si parla di colesterolo alto.
summary(chol)
ggplot(df,aes(chol))+
  geom_histogram(col="black",fill="turquoise4",binwidth = 2.5)+
  geom_density(aes(y=2.5 * ..count..),col="red")+
  geom_vline(aes(xintercept = mean(chol), color=c("mean")))+
  geom_vline(aes(xintercept = median(chol), color=c("median")))+
  scale_x_continuous(breaks = c(seq(126,564,by=30)))+
  scale_color_manual(name="",values = c(mean="darkorange",median="turquoise3"))+
  theme_classic()
mean(chol)
median(chol)



###fbs
#Il valore normale della glicemia a digiuno viene mantenuto tra 70 e 100mg/dl. 
#Si parla di iperglicemia quando vi ? un innalzamento del glucosio nel sangue che pu? portare a conseguenze negative sul nostro stato di salute.
ggplot(df,aes(fbs))+
  geom_bar(col="black",fill=c("#F8766D","#B79F00"))+
  geom_text(stat = "count",aes(label=..count..),vjust=+2)+
  theme_classic()
#l'85% degli individui in esame hanno livelli di zucchero nel sangue a digiuno con valori inferiori a 120mg/dl

###restecg
table(restecg)
ggplot(df,aes(restecg))+
  geom_bar(col="black",fill=c("#F8766D","#B79F00","#00BA38"))+
  geom_text(stat = "count",aes(label=..count..),vjust=+2)+
  theme_classic()


###thalachh
summary(thalachh)
ggplot(df,aes(thalachh))+
  geom_histogram(col="black",fill="turquoise4",binwidth = 2.5)+
  geom_density(aes(y=2.5 * ..count..),col="red")+
  geom_vline(aes(xintercept = mean(thalachh), color=c("mean")))+
  geom_vline(aes(xintercept = median(thalachh), color=c("median")))+
  scale_x_continuous(breaks = c(seq(71,202,by=10)))+
  scale_color_manual(name="",values = c(mean="darkorange",median="turquoise3"))+
  theme_classic()
mean(thalachh)
median(thalachh)



###exng
table(exng)
ggplot(df,aes(exng))+
  geom_bar(col="black",fill=c("#F8766D","#B79F00"))+
  geom_text(stat = "count",aes(label=..count..),vjust=-0.2)+
  theme_classic()

###oldpeak
summary(oldpeak)
ggplot(df,aes(oldpeak))+
  geom_histogram(col="black",fill="turquoise4")+
  geom_vline(aes(xintercept = mean(oldpeak), color=c("mean")))+
  geom_vline(aes(xintercept = median(oldpeak), color=c("median")))+
  scale_x_continuous(breaks = c(seq(0,6.2,by=0.2)))+
  scale_color_manual(name="",values = c(mean="darkorange",median="turquoise3"))+
  theme_classic()
mean(oldpeak)
median(oldpeak)

###slp
table(slp)
ggplot(df,aes(slp))+
  geom_bar(col="black",fill=c("#F8766D","#B79F00","#00BA38"))+
  geom_text(stat = "count",aes(label=..count..),vjust=+2)+
  theme_classic()


###caa
table(caa)
ggplot(df,aes(caa))+
  geom_bar(col="black",fill=c("#F8766D","#B79F00","#00BA38","#00BFC4", "#619CFF"))+
  geom_text(stat = "count",aes(label=..count..),vjust=+2)+
  theme_classic()
#i valori dovrebbero andare da 0 a 3, c'? qualcje 4. Possibile errore di scrittura

###thall
  table(thall)
ggplot(df,aes(thall))+
  geom_bar(col="black",fill=c("#F8766D","#B79F00","#00BA38","#00BFC4"))+
  geom_text(stat = "count",aes(label=..count..),vjust=+2)+
  theme_classic()
#anche qui lo 0 credo sia un errore


###
###Altri grafici interessanti
ggplot(df,aes(sex,fill=output))+
  geom_bar(position = "dodge", stat = "count")+
  theme_classic()

ggplot(df,aes(cp,fill=output))+
  geom_bar(position = "dodge", stat = "count")+
  theme_classic()
#possiamo dire in linea di massima che avere un'angina ? un chiaro sintomo di possibile attacco di cuore. 

ggplot(df,aes(output,fill=fbs))+
  geom_bar(position = "dodge", stat = "count")+
  theme_classic()
#sembra che lo zucchero nel sangue non sia discriminante tra Alta e Bassa probabilit?

ggplot(df, aes(x = age, y = chol, color = output)) +
  geom_point(size = 2)+
  geom_smooth(method='lm', formula= y~x)+
  theme_classic()
ggplot(df, aes(x = age, y = thalachh, color = output)) +
  geom_point(size = 2)+
  geom_smooth(method='lm', formula= y~x)+
  theme_classic()
ggplot(df, aes(x = thalachh, y = chol, color = output)) +
  geom_point(size = 2)+
  geom_smooth(method='lm', formula= y~x)+
  theme_classic()



ggplot(df,aes(output,age))+
  geom_boxplot(col="black",fill="#F8766D")+theme_classic()
ggplot(df,aes(output,age))+
  geom_dotplot(binaxis="y",stackdir="center",col="black",fill="#F8766D")+theme_classic()
ggplot(df,aes(output,age))+
  geom_violin(scale = "area",col="black",fill="#F8766D")+theme_classic()
#? intuitivo che le persone anziane potrebbero avere maggiori probabilit? di infarto, ma secondo la distribuzione dell'et?, ? evidente che questo non ? il caso.

ggplot(df,aes(output,trtbps))+
  geom_boxplot(col="black",fill="#F8766D")+theme_classic()
ggplot(df,aes(output,trtbps))+
  geom_dotplot(binaxis="y",stackdir="center",col="black",fill="#F8766D")+theme_classic()

ggplot(df,aes(output,chol))+
  geom_boxplot(col="black",fill="#F8766D")+theme_classic()
ggplot(df,aes(output,chol))+
  geom_dotplot(binaxis="y",stackdir="center",col="black",fill="#F8766D")+theme_classic()

ggplot(df,aes(output,thalachh))+
  geom_boxplot(col="black",fill="#F8766D")+theme_classic()
ggplot(df,aes(output,thalachh))+
  geom_dotplot(binaxis="y",stackdir="center",col="black",fill="#F8766D")+theme_classic()
# ? chiaro che persone con una frequenza cardiaca elevata hanno una maggiore probabilit? di infarto


ggplot(df,aes(output,oldpeak))+
  geom_boxplot(col="black",fill="#F8766D")+theme_classic()
ggplot(df,aes(output,oldpeak))+
  geom_dotplot(binaxis="y",stackdir="center",col="black",fill="#F8766D")+theme_classic()
#Ci sono alcuni valori anomali in tutte le caratteristiche continue.
boxplot(df[,c("age","trtbps","chol","oldpeak","thalachh")],col="violet")

#La maggior parte delle persone hanno et? compresa tra 50-60 anni, lieve dolore al petto, pressione sanguigna tra 120 a 140, colesterolo tra 200-300,
#zucchero nel sangue inferiore ai 120  mentre la frequenza cardiaca tra 150-175
#Le persone con et? 40-60 hanno maggiori probabilit? di avere malattie cardiache
#Le persone con frequenza cardiaca pi? elevata sono pi? soggette a infarto.
#Circa i due terzi dei dati provengono da pazienti di sesso maschile; tuttavia, una percentuale maggiore di pazienti di sesso femminile inclusi nel set di dati sono ad alto rischio di infarto.


#trtbps (pressione sanguigna a riposo) ha una relazione debole o leggermente negativa con attacco di cuore
#
#thalach (frequenza cardiaca massima raggiunta) ha una relazione positiva con l'infarto

#oldpeak  ha una relazione negativa con l'infarto

#Le donne hanno pi? probabilit? di avere problemi di cuore rispetto agli uomini (in base al rapporto)
#persone di mezza et? (45-60 anni) hanno maggiori probabilit? di infarto
#Le persone con alto Cholestoral (pi? di 200) hanno una probabilit? molto maggiore di infarto
#Le persone con frequenza cardiaca massima raggiunta > 150 ha una maggiore probabilit? di infarto
#L'et? ha una relazione negativa con il thalachh (frequenza cardiaca massima raggiunta)
#Dolore toracico ha pi? alta probabilit? di infarto
#l'angina tipica ha probabilit? pi? bassa dell'attacco di cuore che altri dolori di cassa
#Le persone con vasi principali inferiori (caa) hanno probabilit? molto maggiori di infarto
#L'et? ha una relazione positiva con n navi principali (caa) (le persone anziane hanno maggiori probabilit? di avere navi)
#le persone con Thall == 2 hanno maggiori probabilit? di infarto
#Le donne sono suscettibili di avere livelli pi? elevati di colesterolo rispetto agli uomini
#Cholestoral ha una relazione positiva con l'et?
#Slope ha una relazione positiva con l'infarto e People with Slope == 2 ha una probabilit? molto pi? alta di infarto
#trtbps (pressione sanguigna a riposo) ha una relazione positiva con l'et?
#oldpeak (depressione ST indotta dall'esercizio fisico rispetto al riposo) ha una relazione altamente negativa con l'infarto
#Le persone con presenza di exng (Angina indotta dall'esercizio) hanno meno probabilit? di avere un infarto ?


#########CLUSTERING 
#KMEDIE- Iterative based clustering

new.df=scale(df[,c("age","trtbps","chol","oldpeak","thalachh")])
#standardizzo i dati essendo algoritmi basati sul concetto di distanza.
km.res=kmeans(new.df,2,iter.max = 100)
print(km.res)
km.res=kmeans(new.df,2,iter.max = 50)
?kmeans
res=as.factor(km.res$cluster)
levels(res)=c("Bassa Probabilit?","Alta probabilit?")
#con meno iterazioni i risultati non sembrano molto diversi.
confusionMatrix(df[,"output"],res)

#proviamo con k pi? grandi
km.res=kmeans(new.df,3,iter.max = 50)
km.res$betweenss
km.res$tot.withinss
km.res=kmeans(new.df,4,iter.max = 50)
km.res$betweenss
km.res$tot.withinss
km.res=kmeans(new.df,5,iter.max = 50)
km.res$betweenss
km.res$tot.withinss
km.res=kmeans(new.df,6,iter.max = 50)
km.res$betweenss
km.res$tot.withinss
km.res=kmeans(new.df,7,iter.max = 50)
km.res$betweenss
km.res$tot.withinss
km.res=kmeans(new.df,8,iter.max = 50)
km.res$betweenss
km.res$tot.withinss
#proviamo con validation interna la ssq
SSQs <- numeric()
kappas <- 2:13
for( k in kappas ) {
  km.res <- kmeans( new.df, centers=k )
  SSQs <- c(SSQs,km.res$tot.withinss)
}

plot( kappas, SSQs, type="o", lwd=3, col="blue" )


###Proviamo la silhouettesils <- numeric()
sils <- numeric()
for( k in kappas ) {
  res <- kmeans( new.df, centers=k )
  sil <- silhouette( res$cluster, dist(new.df) )
  sils <- c(sils, (summary(sil))$avg.width )
}

plot( kappas, sils, type="o", lwd=3, col="red" )


####Hierarchical Clustering
# Euclidean distances between scaled data
d <- dist(new.df)

# hierarchical agglomerative clustering
h.res <- hclust( d, method="complete" ) # try also with other values of "method", such as "single", "average", "centroid"...
print(h.res)

# dendogram
plot(h.res)

# obtaining clusters from the dendogram
cluster <- cutree(h.res,k=2)
cluster


##DBSCAN
# dbscan.res <- dbscan( ds, eps=0.7, minPts=5 )
dbscan.res <- dbscan( new.df, eps=0.7, minPts=5 )
print( dbscan.res )
# Altre prove...
dbscan.res <- dbscan( new.df, eps=0.5, minPts=5 )
print( dbscan.res )
#diminuendo di poco il raggio, non riesce a trovare nessun cluster
dbscan.res <- dbscan( new.df, eps=0.5, minPts=3 )
print( dbscan.res )
dbscan.res <- dbscan( new.df, eps=1, minPts=5 )
print( dbscan.res )
dbscan.res <- dbscan( new.df, eps=1, minPts=5 )
print( dbscan.res )


####
#####CLASSIFICATION
set.seed(123)
model.svm <- svm(output~., data=df, scale=F, type="C-classification", kernel="linear")
model.svm$fitted
confusionMatrix(df[,"output"], model.svm$fitted)
###I DATI NON SONO LINEARMENTE SEPARABILI
#parte <- createDataPartition(output, p = 0.8, list=FALSE)
#training <- df[parte,]
#test <- df[-parte,]

#CV sul modello (addestrato): in due righe addestramento e cross validazione

#trctrl=trainControl(method = "LOOCV", number=10, savePredictions = T)

#Method pu? includere anche la leave one out cross validation con "LOOCV" e
#la "repeatedcv" aggiungendo il parametro repeats
#savePrediction=T se ci servono anche le previsioni

#mdl=train(output~., data=df, method="svmLinear2", trControl=trctrl, tuneGrid=data.frame(cost=c(0.1,0.3,0.7,1,1.5)))

#method potrebbe richiedere di installare nuovi pacchetti per stimare il modello (chiesti prima)
#tuneLength specifica il numero di configurazioni da stimare per il modello cambiando
#gli iperparametri (con =0 si usa la configurazione di base)
modLog <- train(output~., data=df, method = 'glmnet', trControl=trctrl, family="binomial")
modLog
modSvm <- train(output~., data=df, method = 'svmLinear', trControl=trctrl)
modSvm
modDt <- train(output~., data=df, method = 'rpart', trControl=trctrl)
modDt
res <- resamples(list(Logit=modLog, SVM=modSvm, kNN=modKnn, D_Tree = modDt))
summary(res)



###KNN 
trctrl <- trainControl(method="LOOCV",savePredictions = T)
trctrl2 <- trainControl(method="repeatedcv", number=10, repeats=4,savePredictions = T)
modKnn <- train(output~., data=df, method = 'knn', preProcess = c("center","scale"), trControl=trctrl,tuneGrid=data.frame(k=c(1:30)))
modKnn
#modKnn2 <- train(output~., data=df, method = 'knn', preProcess = c("center","scale"), trControl=trctrl2)
#modKnn2

#Il primo algoritmo testato è il KNN poichè, nonostante risulti spesso inefficiente, disponendo di un dataset
#di dimensione relativamente ridotta poteva essere utile valutarne i risultati anche per avere una prima
#idea della complessità dei dati a disposizione.
#K-NN è un algoritmo distance-based, infatti le k osservazioni più vicine sono riconosciute secondo
# una qualche misura di distanza, quindi, esso incorre nelle classiche problematiche relative agli
#algoritmi basati su misure di distanza.
#In fase di preprocessing dei dati è stato necessario normalizzarli al fine di riportare tutte le variabili sullo
#stesso range di variazione; inoltre si è scelto di adottare la distanza euclidea come misura di distanza
#per l'individuazione delle k osservazioni più vicine.
#Si è optato per una leave one out cross validation al fine di eliminare l'aleatorietà del processo di cross-validation.
#L'obbiettivo è quello di massimizzare l'accuracy per classificare al meglio il rischio di infarto per ciascun individuo
#Il metodo in questione ha un solo iperparametro k
#Si è deciso di testare una griglia di valore per l'iperparametro k al fine di valutare quale fosse il migliore
#e i valore di k che massimizzano l'accuracy risultano essere 9 e 10, come dimostrato dal grafico sottostante.
#Si è scelto di optare per k=9 per non incorrere in problemi di incertezza dovuti all'osservazione del
#medesimo numero di osservazioni appartente alle due classe target.
plot(modKnn)
#La confusion matrix ci offre valori interessanti come la sensitivity e specificity
confusionMatrix(modKnn$pred[which(modKnn$pred[,4]==9),1], df[,"output"], positive = "AP")
round(145/(145+20), 4)
#Nota che quello che noi intendiamo con sensitivity qua è la specificity
#Provo con un KNN ponderato in modo da dare più peso alle osservazioni vicine e meno a quelle lontane
modKnnW <- train(output~., data=df, method = 'ownn', preProcess = c("center","scale"), trControl=trctrl, tuneGrid=data.frame(K=c(1:30)))
modKnnW2 <- train(output~., data=df, method = 'ownn', preProcess = c("center","scale"), trControl=trctrl, tuneGrid=data.frame(K=c(31:50)))
modKnnW$pred[which(modKnnW$pred[,4]==30),]
#k=30
confusionMatrix(modKnnW$pred[which(modKnnW$pred[,4]==30),1],df[,"output"], positive = "AP")
round(144/(144+21), 4)   #bisogna sempre guardare specificity come valore di sensitivity
#k=31
confusionMatrix(modKnnW2$pred[which(modKnnW2$pred[,4]==31),1],df[,"output"])
#k=32
confusionMatrix(modKnnW2$pred[which(modKnnW2$pred[,4]==32),1],df[,"output"])
#k=33
confusionMatrix(modKnnW2$pred[which(modKnnW2$pred[,4]==33),1],df[,"output"])
#k=34
confusionMatrix(modKnnW2$pred[which(modKnnW2$pred[,4]==34),1],df[,"output"]) #male
#k=35
confusionMatrix(modKnnW2$pred[which(modKnnW2$pred[,4]==35),1],df[,"output"])




###SVM
#Per prima cosa si è provato a testare un SVM con kernel lineare  sul datset completo nella sua configurazione 
#di base per verificare se i dati a nostra disposizione fossero linearmente separabili ma, avendo ottenuto
#un rischio empirico non nullo, abbiamo concluso che essi non lo fossero. In altri termini non esiste un
#iperpiano in grado di separare i dati nelle due classi previste dal dataset senza commettere errore.
#Successivamente si è provato ad applicare i kernel in modo da verificare se, superando il
#limite della linearità, fosse possibile individuare una trasformazione implicita dell'input
#space affinchè i dati siano linearmente separabili nel feature space.
#In questo modo, sarebbe possibile applicare SVM nel feature space mantenendone la struttura base.
#Anche in questo caso non si è ottenuto un rischio empirico nullo
library(e1071)
model.svm <- svm(output~., data=df, scale=F, type="C-classification", kernel="linear", cost=1,epsilon=0.1)
confusionMatrix(df$output, model.svm$fitted)
summary(model.svm)

model.svm.poly3 <- svm(output~., data=df, scale=F, type="C-classification", kernel="polynomial")
confusionMatrix(df$output, model.svm.poly3$fitted)
remove(model.svm.poly3)

model.svm.radial <- svm(output~., data=df, scale=F, type="C-classification", kernel="radial")
confusionMatrix(df$output, model.svm.radial$fitted)
#Linearmente separabili

#Si è provato ad applicare un SVM con kernel lineare rispetto a una griglia di valori dell'iperparametro C.
#Anche in questo caso si è optato per una leave one out cross validation.
#Il parametro di regolarizzazione C permette di controllare la penalizzazione dell'errore nel contesto di
#SVM soft-margin, più esso aumenta e più peso avranno le classificazioni errate: otterrò quindi un iperpiano
#di separazione con margine inferiore. Al contrario se C diminuisce il peso agli errori di classificazione
#diminuisce e si ottiene un grado di gneralizzazione maggiore in caso di dati noisy.
mdlsvmlin=train(output~., data=df, method="svmLinear2", trControl=trctrl, tuneGrid=data.frame(cost=c(0.1,0.3,0.7,1,10,100)))
plot(mdlsvmlin)
confusionMatrix(mdlsvmlin$pred[which(mdlsvmlin$pred[,4]==0.1),1],df[,"output"], positive = "AP")
confusionMatrix(mdlsvmlin$pred[which(mdlsvmlin$pred[,4]==0.3),1],df[,"output"], positive = "AP")
confusionMatrix(mdlsvmlin$pred[which(mdlsvmlin$pred[,4]==0.7),1],df[,"output"], positive = "AP")
confusionMatrix(mdlsvmlin$pred[which(mdlsvmlin$pred[,4]==10),1],df[,"output"], positive = "AP")
confusionMatrix(mdlsvmlin$pred[which(mdlsvmlin$pred[,4]==10),1],df[,"output"], positive = "AP")
round(147/(147+33), 4)
#Dal grafico sopra si può notare che il valore di C che massimizza l'accuracy del modello è C=0.1

#Si è provato ad applicare kernel radiale e polinomiale tuttavia risultano meno performanti in termini di
#accuracy:
mdlsvmrad=train(output~., data=df, method="svmRadial", trControl=trctrl, tuneGrid=data.frame(sigma=rep(0.03154875, 6),C=c(0.1,0.7,1,2,5,10)))
mdlsvmrad
plot(mdlsvmrad)
confusionMatrix(mdlsvmrad$pred[which(mdlsvmrad$pred[,5]==2),1],df[,"output"],positive = "AP")


mdlsvmpol=train(output~., data=df, method="svmPoly", trControl=trctrl, tuneLength=4)
plot(mdlsvmpol)




#relazione tra C e numero di SV, cercare di capire




###BINOMIAL REGRESSION
#Data la natura dicotomica della variabile target si è deciso di testare una binomial regression con funzione
#link logit. Per questo algoritmo non sono previsti iperparametri per cui è stato necessario effettuare solo
#addestrare il modello secondo una leave one out cross validation. 
mdlglm=train(output~., data=df, method="glm", trControl=trctrl)
mdlglm
confusionMatrix(mdlglm$pred[,1],df[,"output"],positive = "AP") 
round(146/(146+30), 4)

#Analisi del modello
#Significatività dei vari coeff
summary(mdlglm$finalModel)
exp(2.417107)
Anova(mdlglm$finalModel)
anova(mdlglm$finalModel)
#Test di bontà del modello (gdl??? 2 sono sbagliati, teoricamente k2-k1)
1-pchisq(deviance(mdlglm$finalModel),23)
#Verifichiamo gli outlier
outlierTest(mdlglm$finalModel) 
#osservazione 140 risulta outlier
influenceIndexPlot(mdlglm$finalModel)
#INFLUENCE PLOTS
#a "bubble" plot of Studentized residuals by hat values, with the areas of the
#circles representing the observations proportional to Cook's distances. 
#Vertical reference lines are drawn at twice and three times the average hat value,
#horizontal reference lines at -2, 0, and 2 on the Studentized-residual scale
influencePlot(mdlglm$finalModel)
#GRAFICO 1: RESIDUALS VS FITTED. Riporta eta.hat vs residui di devianza (eD)
#grafico valido per valutare la linearità del predittore
#GRAFICO 2: NORMAL Q-Q (un leggero allontanamento dalla bisettrice nel QQ-plot e' frequente)
#grafico per la normalità dei residui
#GRAFICO 3: SCALE-LOCATION. Riporta eta.hat vs sqrt(eDS)
#grafico valido per valutare la funzione di varianza
#GRAFICO 4: RESIDUALS VS LEVERAGE. Riporta h.ii vs ePS
#grafico valido per la diagnostica delle singole osservazioni
par(mfrow=c(1,1))
plot(mdlglm$finalModel)


#UTILE PER INTERPRETAZIONE DEI COEFF
#interpretazione: esempio beta_location2. beta.hat= -0.4168=log odds ratio
exp(-0.4168)  #circa 0.66 stima dell'odds ratio
#passando da location 1 a location 2, e fermo restando tutto il resto,
#l'odds relativo alla prob. di sopravvivenza
#diminuisce al 66% (cioe' del 34%) dell'odds relativo a location 1 
#(ASSOCIAZIONE NEGATIVA TRA SOPRAVVIVENZA E LOCATION 2)




###RANDOM FOREST
#Il quarto metodo testato è stato quello della random forest. In questo caso l'iperparametro da impostare
#è mtry, ovvero il numero di esplicative che vengono utilizzate per ogni albero all'interno della random
#forest. Si è deciso di optare per una repeated k-folds cross-validation con k pari a 10 perchè la leave one 
#out cross validation risultava troppo onerosa dal punto di vista computazionale.
mdlrf=train(output~., data=df, method="rf", trControl=trctrl, tuneLength=5)
mdlrf
mdlrf$pred
confusionMatrix(mdlrf$pred[which(mdlrf$pred[,4]==2),1],df[,"output"],positive = "AP")



###CLASSIFICATORE DI BAYES

trctrl3 <- trainControl(method="LOOCV",savePredictions = "all", classProbs = T,summaryFunction = twoClassSummary)
mdlnb=train(output~., data=df, method="naive_bayes", trControl=trctrl3)
mdlnb
mdlnb$pred[which(mdlnb$pred[,6]==TRUE),1]
confusionMatrix(mdlnb$pred[which(mdlnb$pred[,6]==TRUE),1],df[,"output"],positive = "AP")
round(156/(156+9), 4)

mdlnb$pred[which(mdlnb$pred[,6]==TRUE),]
#Confusion matrix per soglia 0.05
pred.index=which(mdlnb$pred[which(mdlnb$pred[,6]==T),4]>0.5)
pred=vector(length = 303)
pred[pred.index]=1
pred[-pred.index]=0
pred=as.factor(pred)
levels(pred)=c("BP", "AP")
confusionMatrix(pred,df[,"output"],positive = "AP")

my.roc(M=cbind(df[,"output"],mdlnb$pred[which(mdlnb$pred[,6]==TRUE),3],mdlnb$pred[which(mdlnb$pred[,6]==TRUE),4]))


###RETE NEURALE
set.seed(123)
NNModel <- train(output~., data=df,
                 method = "nnet",
                 preProcess = c("scale"),
                 trControl= trctrl,
                 tuneLenght=10)
NNModel$pred[which(NNModel$pred[,4]==1 & NNModel$pred[,5]==0.1),1]
confusionMatrix(NNModel$pred[which(NNModel$pred[,4]==1 & NNModel$pred[,5]==0.1),1], df[,"output"],positive = "AP")

NNModel$finalModel

NNModel2 <- train(output~., data=df,
                 method = "mxnet",
                 preProcess = c("scale"),
                 trControl= trctrl,
                 tuneLenght=10)

??mxnet




confusionMatrix(df$output,f.class(mdlnb$pred[order(mdlnb$pred[,5]),4], 0.95, levels(df$output)))
#0.8659;0.7525






