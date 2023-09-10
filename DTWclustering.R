library(tidyverse)
library(caret)
library(GGally)
library(dplyr)
library(MASS)
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
library(Rmixmod)
library(mclust)
library(GGally)
library(factoextra)
library(car)
library(klaR)
library(reshape)
library(zoo)
library(mice)
library(VIM)
library(imputeTS)
library(dplyr)
library(lubridate)
library(viridis)

#carica manualmente dataset come data e igora riga
A2A <- read.csv("C:/Users/ricca/OneDrive/Desktop/UNIVERSITA/Magistrale/Data Challenge/A2A_curva_carico_giornaliera/A2A_curva_carico_giornaliera.csv")
summary(data)
anyNA(data)
sum(is.na(A2A)) 
#9338 dati mancanti
#togliamo le var che hanno commentato il tolto perche non sono di utilità nell'analisi
#alcune non avevano variabilità, altre erano perfettamente al pod
summary(A2A$Pod)
table(A2A$Pod)
table(A2A$Anno)  #tolgo
#tutte 2017, inutile
table(A2A$Mese)
table(A2A$giornoAttiva)
table(A2A$giornoReattiva)
table(A2A$PuntoDispacciamento) #tolto
table(A2A$Tensione) #tolgo
#tutte 220
table(A2A$Trattamento)
table(A2A$PotContrImp)
table(A2A$PotDisp)
#seguono var da togliere perche costanti
table(A2A$CifreAtt) #tolgo
table(A2A$CifreRea) #tolgo
table(A2A$Raccolta) #tolgo
table(A2A$TipoDato) #tolgo
table(A2A$Validato) #tolgo


#A2A=A2A[,-c(2,7,11,12,13,14,15)]


#grafico del prof
A2A%>%
  mutate(Data = make_date(Anno, Mese, giornoAttiva)) %>%
  dplyr::select(Pod, Data, Attiva1:Attiva96) ->
  wdt
wdt %>%
  group_by(Pod) %>%
  summarise(n_giorni = n()) %>%
  group_by(n_giorni) %>%
  summarise(frequenza = n()) %>%
  ggplot(aes(x=n_giorni, y=frequenza)) +
  geom_col() +
  ggtitle("Distribuzione numero di Pod per numero di giorni di lettura")



wdt %>%
  mutate(Settimana = week(Data)) %>%
  group_by(Pod, Settimana) %>%
  summarise(n_giorni = n()) %>%
  group_by(Pod) %>%
  summarise(n_settimane_complete = sum(n_giorni == 7)) %>%
  group_by(n_settimane_complete) %>%
  summarise(frequenza=n()) %>%
  ggplot(aes(x=n_settimane_complete, y=frequenza)) +
  geom_col() +
  ggtitle("Distribuzione numero di Pod per numero di settimane complete")



#a=wdt %>%
#  mutate(Giorno_Settimana = day(Data)) %>%
#  group_by(Pod, Giorno_Settimana) %>%
#  summarise(n_osservazioni = n())
#a2a_settimana_wide <- a %>%
#  spread(Giorno_Settimana, n_osservazioni, fill = 0)
#rm(a2a_settimana_wide)

#creo variabile data al fine di separare le settimane e successivamente per i weekend
A2A=A2A%>%
  mutate(Data = make_date(Anno, Mese, giornoAttiva))


# Crea una nuova variabile settimana completa
A2A_new<- A2A %>%
  mutate(week_complete = isoweek(Data)) %>%
  group_by(Pod, week_complete) %>%
  filter(n() == 7) # Filtra solo le settimane complete

# Conta il numero di pod differenti per ogni settimana completa
week_counts <- A2A_new %>%
  distinct(Pod, week_complete) %>%
  group_by(week_complete) %>%
  summarise(num_pods = n()) %>%
  arrange(desc(num_pods))

# Visualizza la settimana completa con il maggior numero di pod differenti
week_counts[1,]
#settimana 41

#creo dataset con solo pod della settimna 41
pod_settimana_41 <- A2A %>%
  mutate(week_complete = isoweek(Data)) %>%
  filter(week_complete == 41 )  %>%
  group_by(Pod, week_complete) %>%
  filter(n() == 7)


#creo var weekend tremite funz indicatrice per separare giorni settimana e weekend
pod_settimana_41 <- pod_settimana_41 %>%
  mutate(weekend = ifelse(wday(Data, week_start = 1) %in% c("6","7"), "si", "no"))%>%
  group_by(Pod)

#prima tolgo tutti i pod spenti, cioe con misurazioni nulle
var_names <- paste0("Attiva", 1:96)
pod_settimana_41_zero= pod_settimana_41%>%filter(across(all_of(var_names), ~ . == 0))
pod_settimana_41_def <- anti_join(pod_settimana_41, pod_settimana_41_zero, by = NULL)
pod_settimana_41_def = pod_settimana_41_def[,-c(2:15,113)]
sum(is.na(pod_settimana_41_def))#375
colSums(is.na(pod_settimana_41_def))
rowSums(is.na(pod_settimana_41_def))
missing_vars_41 <- var_names[sapply(pod_settimana_41_def[var_names], function(x) any(is.na(x)))]
pod_settimana_41_zero%>%
  group_by(Pod)%>%
  filter(n() == 1)
#tolgo questi pod per avere settimane complete, sono i pod che sono stati eliminati prima
#(i pod spenti)
pod_da_eliminare=c("5b71ccfdcffb98411387f95abeb36e71105094a4b5552c26537b961a",
                   "918e1bc42d92352bc7c4a8d2494918b5fee7d6cc9a511875e1bd8f5a",
                   "13b643cdb8857bec691f6430e843c2bd71c909aad27524b1c5b8b52f",
                   "5f30d831b2e75b05f604b9146ed306316d6395fc1dbbf94db7d663a5",
                   "7102c8581acaa6483a837be07a0d733ce651c9baa4d8b57f7707c7b4",
                   "807aa65c31f3a793e6f60b575cff9e98c1e1087d6667bd3cee5c08f7",
                   "2630249852cc3ac8d6a6a46cfaa1a547355e9d1712aa6ee0d2c45d56",
                   "f55e759725ba33caa48ebfde18ae3ac561a7e395cd2081028eeb6cc2",
                   "c425b3b7a64a68e226d5bc5bee241f6fc2c1e1d5b094439e56905c7a",
                   "caa40853b32931329c2c6087b424467c1c299bc4b5df7feb0d5bd2e8",
                   "f97f294d90f9848a839b4b56ad93f9244c95f738aaa51a432526512a",
                   "da02eb16363dc852304859ed797b15e5273b4e54f3b3c04a6e47d323")
pod_settimana_41_def=pod_settimana_41_def%>%
  filter(!Pod %in% pod_da_eliminare)

  

sum(is.na(pod_settimana_41_def$Attiva96))/375 #test
md.pattern(pod_settimana_41_def)
aggr_plot <- aggr(pod_settimana_41_def, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(pod_settimana_41_def), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
#grafico destra illeggibile, sx istrogramma dei missing


anyNA(pod_settimana_41_def)

missing_vars <- var_names[sapply(pod_settimana_41_def[var_names], function(x) any(is.na(x)))]

#imputazione dei valori mancanti nella settimana attraverso medie mobili:
pod_settimana_41_defvar <- pod_settimana_41_def %>%
  # per ogni riga, calcola la media mobile delle due colonne precedenti per ogni colonna dal 2 alla 96
  mutate_at(missing_vars, ~ifelse(is.na(.),
                              ifelse(sum(!is.na(lag(.)))>=2, 
                                     rollmeanr(., k=2, fill=NA), 
                                     lag(.)),
                              .)) %>%
  # se ancora ci sono NA, sostituisci con l'ultimo valore non mancante della riga
  mutate_at(missing_vars, ~ifelse(is.na(.),
                              na_locf(.),
                              .))

anyNA(pod_settimana_41_defvar)
colSums(is.na(pod_settimana_41_defvar))
settimana41_noweek=pod_settimana_41_defvar%>%
  filter(weekend=="no")
settimana41_siweek=pod_settimana_41_defvar%>%
  filter(weekend=="si")

###### HCLUST PER GIORNI SETTIMANA
# estraggo solo le colonne del tempo
tempo <- settimana41_noweek[, 2:97]

# normalizzazione z-score
tempo_norm <- scale(tempo)
# clustering gerarchica con metodo Ward
set.seed(123)
hc <- hclust(dist(tempo_norm), method = "ward.D2")
# plot del dendrogramma
plot(hc)

# taglio il dendrogramma in 20 cluster
gruppi <- cutree(hc, k = 20)

# creo un dataframe con i cluster e le aziende
cluster_settimanale_noweek <- data.frame(Pod = settimana41_noweek$Pod, cluster = gruppi)

# stampo il numero di aziende in ogni cluster
table(cluster_settimanale_noweek$cluster)

#interpretazione difficile perchè non abbiamo una sorta di rappresentante del cluster come in kmedie
#provo a stampare tutte le serie storiche sovrapponendole secondo i cluster
par(mfrow=c(3,3))
for (t in 1:dim(table(cluster_settimanale_noweek$cluster))) {
  plot(ts(t(settimana41_noweek[which(cluster_settimanale_noweek$cluster==t)[1],2:97])))
  for (i in 2:length(which(cluster_settimanale_noweek$cluster==t))) {
    cols <- rainbow(length(which(cluster_settimanale_noweek$cluster==t))-1)
    lines(ts(t(settimana41_noweek[which(cluster_settimanale_noweek$cluster==t)[i],2:97])), col=cols[i])
  }
}
par(mfrow=c(1,1))


###### HCLUST PER GIORNI WEEKEND
tempo <- settimana41_siweek[, 2:97]

# normalizzazione z-score
tempo_norm <- scale(tempo)
# clustering gerarchica con metodo Ward
set.seed(123)
hc <- hclust(dist(tempo_norm), method = "ward.D2")
# plot del dendrogramma
plot(hc)

# taglio il dendrogramma in 20 cluster
gruppi <- cutree(hc, k = 20)

# creo un dataframe con i cluster e le aziende
cluster_settimanale_siweek <- data.frame(Pod = settimana41_siweek$Pod, cluster = gruppi)

# stampo il numero di aziende in ogni cluster
table(cluster_settimanale_siweek$cluster)


#####KMEANS GIORNI SETTIMANALI
set.seed(123)  # Imposta il seed per la riproducibilità
tempo <- settimana41_noweek[, 2:97]

# normalizzazione z-score
tempo_norm <- scale(tempo)
kmeans_results_weekday <- kmeans(tempo_norm, centers = 20, nstart = 40)

table(kmeans_results_weekday$cluster)
par(mfrow=c(3,3))
for (t in 1:dim(table(kmeans_results_weekday$cluster))) {
  plot(ts(t(settimana41_noweek[which(kmeans_results_weekday$cluster==t)[1],2:97])), main=paste0("Cluster ", t), ylab="")
  for (i in 2:length(which(kmeans_results_weekday$cluster==t))) {
    cols <- rainbow(length(which(kmeans_results_weekday$cluster==t))-1)
    lines(ts(t(settimana41_noweek[which(kmeans_results_weekday$cluster==t)[i],2:97])), col=cols[i])
  }
}
par(mfrow=c(1,1))


####KMEANS WEEKEND
set.seed(123)  # Imposta il seed per la riproducibilità
tempo <- settimana41_siweek[, 2:97]

# normalizzazione z-score
tempo_norm <- scale(tempo)
kmeans_results_weekend <- kmeans(tempo_norm, centers = 20, nstart = 40)

par(mfrow=c(3,3))
for (t in 1:dim(table(kmeans_results_weekend$cluster))) {
  plot(ts(t(settimana41_siweek[which(kmeans_results_weekend$cluster==t)[1],2:97])), main=paste0("Cluster ", t), ylab="")
  for (i in 2:length(which(kmeans_results_weekend$cluster==t))) {
    cols <- rainbow(length(which(kmeans_results_weekend$cluster==t))-1)
    lines(ts(t(settimana41_siweek[which(kmeans_results_weekend$cluster==t)[i],2:97])), col=cols[i])
  }
}
par(mfrow=c(1,1))


# Grafico a barre per i giorni settimanali
ggplot(data = as.data.frame(kmeans_results_weekday$cluster)) +
  geom_bar(mapping = aes(x = kmeans_results_weekday$cluster)) +
  scale_x_continuous(breaks = 1:30) +
  labs(x = "Cluster", y = "Numero di osservazioni", title = "Distribuzione delle osservazioni nei cluster (giorni settimanali)")

# Grafico a barre per il weekend
ggplot(data = as.data.frame(kmeans_results_weekend$cluster)) +
  geom_bar(mapping = aes(x = kmeans_results_weekend$cluster)) +
  scale_x_continuous(breaks = 1:30) +
  labs(x = "Cluster", y = "Numero di osservazioni", title = "Distribuzione delle osservazioni nei cluster (weekend)")

# Visualizzazione dei cluster trovati con k-medie

#visualizziamo collocazione dei cluster in uno spazio bidimensionale formato dalle prime due 
#principali derivanti dalle 96 variabili attiva
cols <- viridis_pal()(20)
fviz_cluster(kmeans_results_weekday, data = scale(settimana41_noweek[, 2:97]), stand = FALSE,
             geom = "point", palette = cols, main = "Cluster dei giorni settimanali")

fviz_cluster(kmeans_results_weekend, data = scale(settimana41_siweek[, 2:97]), stand = FALSE,
             geom = "point", palette = cols, main = "Cluster dei Weekend")
par(mfrow=c(1,1))



(rowSums(settimana41_siweek[,2:97])/96)[1]
rowSums(settimana41_noweek[,2:97])/96
sett=c()
wknd=c()
for (i in 1:1276) {
  sett[i]=mean((rowSums(settimana41_siweek[,2:97])/96)[i])
}


#Visualizziamo andamento diverso tra week e settimana
sett= settimana41_noweek %>% 
  group_by(Pod) %>%
  summarise(across(everything(), ~mean(.)))
sett=rowSums(sett[,2:97])/96
wknd= settimana41_siweek %>% 
  group_by(Pod) %>%
  summarise(across(everything(), ~mean(.)))
wknd=rowSums(wknd[,2:97])/96
round((sort(abs(sett-wknd)))[3],7)
which(round(abs(sett-wknd),7)==1e-06)
#2540, 6350 rispettivamente domenica e venerdi del pod con maggiori differenze tra week e noweek
plot(ts(t(settimana41_siweek[2540,2:97])))
lines(ts(t(settimana41_siweek[2539,2:97])))
plot(ts(t(settimana41_noweek[6350,2:97])))
lines(ts(t(settimana41_noweek[6349,2:97])))
lines(ts(t(settimana41_noweek[6348,2:97])))
lines(ts(t(settimana41_noweek[6347,2:97])))
lines(ts(t(settimana41_noweek[6346,2:97])))
#sotto c'è il secondo più distinto
plot(ts(t(settimana41_siweek[2530,2:97])))
lines(ts(t(settimana41_siweek[2529,2:97])))
plot(ts(t(settimana41_noweek[5575,2:97])))
lines(ts(t(settimana41_noweek[5574,2:97])))
lines(ts(t(settimana41_noweek[5573,2:97])))
lines(ts(t(settimana41_noweek[5572,2:97])))
lines(ts(t(settimana41_noweek[5571,2:97])))
#questo esempio è del pod che si differenzia meno in media tra comportamento in settimana e 
#durante il weekend
plot(ts(t(settimana41_siweek[2356,2:97])))
lines(ts(t(settimana41_siweek[2355,2:97])))
plot(ts(t(settimana41_noweek[5890,2:97])))
lines(ts(t(settimana41_noweek[5889,2:97])))
lines(ts(t(settimana41_noweek[5888,2:97])))
lines(ts(t(settimana41_noweek[5887,2:97])))
lines(ts(t(settimana41_noweek[5886,2:97])))
#immaginiamo consumo automatizzato
#questo esempio è del secondo pod che si differenzia meno in media tra comportamento in settimana 
#e durante il weekend
plot(ts(t(settimana41_siweek[30,2:97])))
lines(ts(t(settimana41_siweek[29,2:97])))
plot(ts(t(settimana41_noweek[75,2:97])))
lines(ts(t(settimana41_noweek[74,2:97])))
lines(ts(t(settimana41_noweek[73,2:97])))
lines(ts(t(settimana41_noweek[72,2:97])))
lines(ts(t(settimana41_noweek[71,2:97])))


############################

A2A=as_tibble(A2A)
summary(A2A)
var_names <- paste0("Attiva", 1:96)

# selezionare le osservazioni in cui le variabili consecutive assumono valore 0 contemporaneamente
a2a_zero <- A2A %>% filter(across(all_of(var_names), ~ . == 0))

a2a_filtered <- anti_join(A2A, a2a_zero, by = NULL)
a2a_filtered %>% filter(across(all_of(var_names), ~ . == 0))
a2a_filtered=a2a_filtered[,-c(2,4,5,6,7,8,9,10,11,12,13,14,15)]
missing_vars <- var_names[sapply(a2a_filtered[var_names], function(x) any(is.na(x)))]
sum(is.na(a2a_filtered))
for (var in missing_vars) {
  a2a_filtered <- a2a_filtered %>% 
    mutate(!!var := ifelse(is.na(!!sym(var)), rollmean(!!sym(var), k = 2, fill = "extend"), !!sym(var)))
}
anyNA(a2a_filtered)

###aggregazione in base ai quarti d'ora generali raggruppando secondo pod e mese###
#uso il dataset filtrato
data_grouped= a2a_filtered %>% 
  group_by(Pod, Mese) %>%
  summarise(across(everything(), ~mean(.)))

#numero di NA in data raggruppato
sum(is.na(data_grouped)) #0 come ci aspettavamo

#provo senza normalizzazione
kmeans_fit <- kmeans(data_grouped[, 3:98], centers = 60, nstart = 70)
kmeans_fit$centers
kmeans_fit$size
par(mfrow=c(3,3))
for (i in 1:ncol(kmeans_fit$centers)) {
  plot(ts(kmeans_fit$centers[i,]), main=paste0("Cluster ", i), ylab="")
}
par(mfrow=c(1,1))
gruppi=kmeans_fit$cluster
#(kmeans_fit$betweenss/(k-1))/(kmeans_fit$tot.withinss/(7527-k))


# Versione con dati normalizzati
data_clustering_norm <- scale(data_grouped[, 3:98])
dim(data_clustering_norm)
summary(data_clustering_norm)
hist(data_clustering_norm[,7], freq=F, breaks=150, xlim=c(-3,5))
curve(dnorm(x), add=T)
?hist
# Eseguiamo il clustering con 30 cluster
set.seed(125)
CH=c()
for (k in 10:40) {
  kmeans_fit <- kmeans(data_clustering_norm, centers = k, nstart = 3)
  CH[k-9]=(kmeans_fit$betweenss/(k-1))/(kmeans_fit$tot.withinss/(7527-k))
}
which.max(CH)
plot(CH)
#quasi linearmente decrescente da 1 a 100, quindi il numero di k è arbitrario


kmeans_fit <- kmeans(data_clustering_norm, centers = 60, nstart = 70, iter.max = 50)
(kmeans_fit$betweenss/(k-1))/(kmeans_fit$tot.withinss/(7527-k))
par(mfrow=c(3,3))
for (i in 1:60) {
  plot(ts(kmeans_fit$centers[i,]), main=paste0("Cluster ", i), ylab="")
}
par(mfrow=c(1,1))
plot(silhouette(kmeans_fit$cluster, dist = dist(data_clustering_norm)))

which(kmeans_fit$size<30)
plot(kmeans_fit$size)


#dtw sui risultati
library("dtw")
d <- dist(ts(kmeans_fit$centers), method="DTW")
#str(d)
tree=hclust(d, method = "average")
plot(tree, xlab="")
#prove
plot(ts(kmeans_fit$centers[21,]))
rect.hclust(tree, k=21)
#identify(tree, k=22)
gruppi=cutree(tree, k=21)
sil=silhouette(gruppi, dist=d)
plot(sil)
#12- 0.36 21/20/19-0.3
#vediamo come sono i cluster
length(which(gruppi==1))
which(gruppi==21)
plot(ts(kmeans_fit$centers[56,]), ylab="", ylim=c(0,5))
lines(ts(kmeans_fit$centers[54,]), col="red")
lines(ts(kmeans_fit$centers[60,]), col="green")
lines(ts(kmeans_fit$centers[34,]), col="grey")
lines(ts(kmeans_fit$centers[41,]), col="yellow")
lines(ts(kmeans_fit$centers[46,]), col="blue")
lines(ts(kmeans_fit$centers[48,]), col="orange")
lines(ts(kmeans_fit$centers[49,]), col="lightblue")
lines(ts(kmeans_fit$centers[59,]), col="purple")
#gruppo 1 fa cagare
#gruppo 2 (solo un centroide) si attesta su valori alti
#gruppo 3 (solo un centroide ) con due picchi (tenere)



for (i in 1:21) {
  for (t in 1:length(which(gruppi==i))) {
    plot(ts(kmeans_fit$centers[(which(gruppi==i)),]))
  }
}







# Visualizziamo il risultato
table(kmeans_fit$cluster)

df <- data.frame(kmeans_fit$cluster, data_grouped$Pod)


# Tabella di riepilogo
summary_table <- df %>%
  count(kmeans_fit$cluster, data_grouped$Pod)


# Visualizzazione della tabella
print(summary_table)


small_clusters <- names(table(kmeans_fit$cluster))[table(kmeans_fit$cluster) < 30]
small_clusters


fviz_cluster(kmeans_fit, data = data_clustering_norm, stand = FALSE,
             geom = "point", palette = cols,ggtheme = theme_classic())



