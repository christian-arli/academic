#carico le library
library(mclust)
library(tidyverse)
library(mixtools)
library(flexmix)
library(GGally)
library(Rmixmod)
library(hexbin)
library(corrplot)
library("caret")


####################
#ANALISI ESPLORATIVA
str(breast_cancer)

breast_cancer$Diagnosis=as.factor(breast_cancer$Diagnosis)
breast_cancer=breast_cancer[,-1]   #tolgo ID che non serve
ggpairs(breast_cancer[,2:11])
ggpairs(breast_cancer[,12:21])
ggpairs(breast_cancer[,22:31])

#riduzione dimensionalità con PCA
pca=princomp(breast_cancer[,-1])
summary(pca)    #le prime due componenti spiegano il 99,8% della variabilità
str(pca$loadings)   

bcnew=as.matrix(breast_cancer[,-1])%*%pca$loadings[,1:2]   

bcnew=cbind(bcnew, breast_cancer[,1])
summary(bcnew)
?ggpairs
ggpairs(bcnew[,-3]) 
#correlazione nulla come da definizione di PCA
#spiccata asimmetria per la componente 1
#noto anche elevata dispersione al crescere di comp1

(p<-ggplot(data=bcnew, mapping = aes(x=Comp.1, y=Comp.2, color=Diagnosis)) + 
    geom_point(alpha= .5,  size=3, aes(color = Diagnosis))+ theme_classic())
#benigni e maligni sembrano ben separati

?geom_smooth
p+geom_smooth(method="lm",  se=F, size=1.5, fullrange=T)
#sembrano avere andamenti diversi i due cluster, in particolar modo se si considerano solo i benigni
#le retta sembra interpolare bene i punti, meno soddisfacente se consideriamo i maligni


ggplot(data=bcnew, mapping = aes(x=Comp.1, y=Comp.2, color=Diagnosis))+
facet_wrap(~Diagnosis, scales="free")+geom_point()+
geom_smooth(method="lm",  se=T, size=1.5, fullrange=T, color=NA, alpha=0.3)+
  geom_line(stat="smooth", method=lm, size=2, alpha=0.3, color="black")

?geom_smooth

ggplot(bcnew, mapping=aes(x=Comp.1))+geom_density() #grafici ottenibili anche con ggpairs
ggplot(bcnew, mapping=aes(x=Comp.2))+geom_density() 

ggplot(data=bcnew, mapping = aes(x=Comp.1, y=Comp.2))+geom_hex() #utile per capire alta concetrazione di punti

ggplot(data=bcnew, mapping = aes(x=Comp.1, y=Comp.2))+
  geom_boxplot(mapping = aes(group = cut_width(Comp.1, 700))) #utile?


###########################
#CLUSTERING 
mclustICL(bcnew[,-3]) #VVV, 2
plot(mclustICL(bcnew[,-3]))

clustnonimposti=Mclust(bcnew[,-3])
summary(clustnonimposti)

ggplot(bcnew, mapping=aes(Comp.1, Comp.2, color=clustnonimposti$classification))+
  geom_point()+labs(color="Gruppi")+theme_classic()

clust=Mclust(bcnew[,-3], G=2)   #ho provato a imporre 2 cluster che son quelli che conosco 
summary(clust)
str(clust)

ggplot(bcnew, mapping=aes(Comp.1, Comp.2, color=clust$classification))+
  geom_point()+labs(color="Gruppi")+theme_classic()


clustdensity=densityMclust(bcnew[,-3], G=2)
plot(clustdensity, what="density", type="image", xlim=c(0,1500), ylim=c(-100,400))
#probabilmente i gruppi sono mal distinti 
summary(clust, parameters=T)
#BIC e ICL simili è una situazione ottima perchè significa che l'entropia è bassa
#adattamento del modello ai dati e clusterizzazione covergono su modelli simili

plot(clust, what="uncertainty")

classError(clust$classification, bcnew$Diagnosis)

par(mfrow=c(1,2))
coordProj (bcnew, dimens=c(1,2), what="classification",
           classification=bcnew$Diagnosis,
           col=c("red2","green3"),
           sub="True Classification")


# Plot mclust classification
#--------------------------
coordProj (bcnew, dimens=c(1,2), what="classification",
           classification=clust$classification,
           col=c("green3","red2"), 
           sub="Model-Based Clustering")

miss_class<-classError(clust$classification, class=bcnew$Diagnosis)$misclassified #37 missclassificate
points(bcnew[miss_class,c(1,2)],pch=19)

par(mfrow = c(1, 1))

coordProj (data=bcnew, dimens=c(1,2), what="uncertainty",
           parameters=clust$parameters , z=clust$z,
           truth=bcnew$Diagnosis, xlim=c(100,3850), sub="Uncertainty")

#model based clustering mostra risultati soddisfacenti in quanto i cluster sembrano corrispondere alla
#classificazione medica supervised, ciò è molto importante per l'obiettivo futuro della classificazione
#dire che è stato fatto clustering principalmente per questo motivo


#la media risulta molto simile per entrambi i gruppi (lo vedo dal summary)
#provo a fare clustering con la sola comp.1
clustcomp1=Mclust(bcnew[,-c(2,3)], G=2)
summary(clustcomp1, parameters=T)
plot(clustcomp1, what="uncertainty")
classError(clustcomp1$classification, bcnew$Diagnosis) #error rate è CER
mean(clustcomp1$uncertainty)

#prova clustering con tutto il ds#
#clustcompleto=Mclust(breast_cancer[,-1], G=2)
#classError(clustcompleto$classification, breast_cancer$Diagnosis)
#mean(clustcompleto$uncertainty)
#risultato:viene peggio ma con incertezza migliore


mean(clust$uncertainty)   #potrebbe essere utile per confrontarla con clust su tutto il dataset


levels(bcnew$Diagnosis)

plot(clust, what="uncertainty")

classError(clust$classification, bcnew$Diagnosis)

#ho provato a vedere error rate con sola area, potrebbe definire bene maligno e benigno
#sum(Mclust(breast_cancer$Area_extreme, G=2)$classification!=as.numeric(breast_cancer$Diagnosis))/n


###################################################
####CLUSTERING con flexmix, clustering con covariate (non mettere nel report)
#(fit<-stepFlexmix(Comp.2~Comp.1,data=bcnew, k=1:4, concomitant = FLXPmultinom(~Comp.1),
#                     nrep = 10,verbose = TRUE, drop = F, unique = FALSE))
(fit<-flexmix(Comp.2~Comp.1,data=bcnew, k=2, concomitant = FLXPmultinom(~Comp.1)))
                 
str(fit)

(p<-ggplot(data=bcnew, mapping = aes(x=Comp.1, y=Comp.2, color=fit@cluster)) + 
    geom_point(alpha= .5,  size=3))

#expert network
parameters(fit)   
ICL(fit)
KLdiv(fit)

#Inferenza su GLM expert model
asymp.inf<-refit(fit)  #questo oggetto contiene la parte di inferenza che ci interessa
summary(asymp.inf)

#gathing network
summary(fit)
#dal ratio sembro più sicuro delle allocazioni nel secondo gruppo
round(posterior(fit),3) #probabilità a posteriori
plot(fit) #non c'è una netta separazione tra i gruppi come si vedeva dallo scatterplot (rettangoli alti per valori intorno a 0.5)

ggplot(data=bcnew, mapping = aes(x=Comp.1, y=Comp.2, color=fit@cluster))+
  geom_point()+ 
  geom_smooth(method="lm", se=F, size=1)

(final.fit<-stepFlexmix(Comp.2~Comp.1,data=bcnew, k=2, concomitant = FLXPmultinom(~Comp.1),
                      nrep = 10,verbose = TRUE, drop = F, unique = FALSE))





#########################
#CLASSIFICAZIONE con EDDA
table(bcnew$Diagnosis)
#non ho classi particolarmente sbilanciate in numerosità per cui non sono in
#ambito di campionamento retrospettivo
#creo il training e test set
n=569
test.set.labels<-sample(1:n,50)

#creo più classificatori e scelgo il migliore tra i best results
prova=list(mod=c(rep(NA, 15)), cv=c(rep(NA, 15)))
cl=list()
for(i in 1:15){
  cl[[i]]=mixmodLearn(bcnew[-test.set.labels,-3], bcnew$Diagnosis[-test.set.labels], 
                 models=mixmodGaussianModel(family="all",equal.proportions=FALSE), criterion=c('CV'))
  prova$mod[i]=cl[[i]]@bestResult@model
  prova$cv[i]=cl[[i]]@bestResult@criterionValue
}

prova$mod[which.min(prova$cv)] #modello migliore (volume e orientation liberi ma stessa forma, VEV)
CLASSIF=cl[[1]]         #migliore tra i migliori



CV = rep(NA ,length(CLASSIF@models@listModels) )
for (i in 1: length(CLASSIF@models@listModels)){
  ind = which(CLASSIF@results [[i]] @model == CLASSIF@models@listModels)
  CV[ind] = CLASSIF@results [[i]] @criterionValue [1]
}

par(mfrow=c(1,1))

plot(CV ,type='b',xlab='',xaxt='n',col =3); axis(1,at=1: length(
  CLASSIF@results),labels=substr(CLASSIF@models@listModels,10 ,30),cex.axis =0.8
  ,las =2)
abline(v=which.min(CV), col=1, lty =2)



PREDICTION<- mixmodPredict(data=bcnew[test.set.labels,-3], classificationRule=CLASSIF["bestResult"])

sum(PREDICTION@partition!=as.numeric(bcnew$Diagnosis[test.set.labels]))
mean(as.integer(bcnew$Diagnosis[test.set.labels]) != PREDICTION["partition"])

#grafico di confronto tra LDA, nostro modello e QDA 

par(mfrow=c(1,3))

#pkLC (LDA)
#----------
pkLC <-mixmodLearn(bcnew[,-3], bcnew$Diagnosis ,models=
                     mixmodGaussianModel(listModels="Gaussian_pk_L_C"))
prec <- 150; Z <- bcnew[ ,-3]
x1<-seq(-200 ,4000 , length=prec) 
x2<-seq(-800 ,1000 , length=prec)   
s<-expand.grid(x1 ,x2); s <- as.data.frame(s)
P<-mixmodPredict(s,pkLC@bestResult)@proba     
plot(bcnew[,-3], type='n',xlim=c(-100 ,3000) ,ylim=c(-500,800))
pastel <- .7
points(s,type='p',pch=16,col=c(rgb(1,pastel ,pastel),
                               rgb(pastel ,1,pastel),
                               rgb(pastel ,pastel ,1))[max.col(P)])

points(bcnew[,-3],col=as.numeric(bcnew$Diagnosis)+1,pch=19,
       xlim=c(-100 ,3000) ,ylim=c(-600 ,900))
title(main="LDA"); box()
#linee di confine lineari, non sembra discriminare bene i gruppi


#pk_Lk_Dk_A_Dk (mio modello, VEV)
#-----
pkLkDkADk <-mixmodLearn(bcnew[,-3], bcnew$Diagnosis ,models=
                      mixmodGaussianModel(listModels="Gaussian_pk_Lk_Dk_A_Dk"))
prec <- 150; Z <- bcnew[ ,-3]
x1<-seq(-200 ,4000, length=prec)
x2<-seq(-600 ,900 , length=prec)
s<-expand.grid(x1 ,x2); s <- as.data.frame(s)
P<-mixmodPredict(s,pkLkDkADk@bestResult)@proba   
plot(bcnew[,-3], type='n',xlim=c(-100 ,3000) ,ylim=c(-500,800))
pastel <- .7
points(s,type='p',pch=16,col=c(rgb(1,pastel ,pastel),rgb(pastel ,1,
                                                         pastel),rgb(pastel ,pastel ,1))[max.col(P)])

points(bcnew[,-3],col=as.numeric(bcnew$Diagnosis)+1,pch=19,
       xlim=c(-100 ,3000) ,ylim=c(-600 ,900))
title(main="EDDA with model VEV"); box()


#pkLkCk (QDA)
#------------
pkLkCk <-mixmodLearn(bcnew[,-3], bcnew$Diagnosis ,models=
                       mixmodGaussianModel(listModels="Gaussian_pk_Lk_Ck"))
prec <- 150; Z <- bcnew[ ,-3]
x1<-seq(-200 ,4000 , length=prec)
x2<-seq(-600 ,900 , length=prec)
s<-expand.grid(x1 ,x2); s <- as.data.frame(s)
P<-mixmodPredict(s,pkLkCk@bestResult)@proba   
plot(bcnew[,-3], type='n',xlim=c(-100 ,3000) ,ylim=c(-500,800))
pastel <- .4
points(s,type='p',pch=16,col=c(rgb(.5,pastel ,pastel),rgb(pastel ,.5,
                                                         pastel),rgb(pastel ,pastel ,.5))[max.col(P)])
points(bcnew[,-3],col=as.numeric(bcnew$Diagnosis)+1,pch=19,
       xlim=c(-100 ,3000) ,ylim=c(-600 ,900))
title(main="QDA"); box()



########################
#CLASSIFICAZIONE con MDA
CLASSIFmda=MclustDA(bcnew[-test.set.labels,-3], bcnew$Diagnosis[-test.set.labels])

summary(CLASSIFmda)

predict(CLASSIFmda, bcnew[test.set.labels,-3])$class
sum(predict(CLASSIFmda, bcnew[test.set.labels,-3])$class!=bcnew$Diagnosis[test.set.labels])/50
#3 classificate erroneamente come con EDDA


