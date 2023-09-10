library(tictoc)
library(caret)
library(tidymodels)
library(pROC)
library(randomForest)
library(doParallel)
library(glmnet)
library(earth)
library(tidyverse)
library(dplyr)
library(kernlab)
library(Boruta)
library(Cubist)
library(elasticnet)
library(frbs)
library(quantregForest)
library(neuralnet)
library(furrr)
library(stacks)
library(randomGLM)
library(caretEnsemble)
library(tinytex)
# IMPORT-----
#tic()
PATH <- "https://raw.githubusercontent.com/aldosolari/DM/master/docs/DATA/" 
train1 <- as_tibble(read.csv(paste0(PATH, "train.csv"), sep = ""))
test1 <- as_tibble(read.csv(paste0(PATH, "test.csv"), sep = ""))


#####PREPROCESSING#####
summary(train1)
#trasformo in factor le variabili fattoriali
train1=train1%>% as_tibble() %>%
  mutate_at(.vars = c("vas1", "vas2", "payment.method",
                      "gender", "status", "tariff.plan", "activ.area", "activ.chan"), factor)
test1=test1%>% as_tibble() %>%
  mutate_at(.vars = c("vas1", "vas2", "payment.method",
                      "gender", "status", "tariff.plan", "activ.area", "activ.chan"), factor)

#sistemo la variabile activ.area
train1$activ.area[which(train1$activ.area==0)] <- 1
train1$activ.area <-  factor(train1$activ.area, levels = levels(train1$activ.area)[-1])

#suppongo che in valori imputati negativi abbiamo un errore di segno quindi li cambio in positivi
change_neg <- function(data) {
  # Scorro tutte le colonne del dataset
  for (col in names(data)) {
    # Se la colonna è di tipo numerico
    if (is.numeric(data[[col]])) {
      # Cambio di segno i valori negativi
      data[[col]][data[[col]] < 0] <- -data[[col]][data[[col]] < 0]
    }
  }
  
  return(data)
}
#la applico a training e test
train1=change_neg(train1)
test1=change_neg(test1)

summary(train1)
summary(test1)
#tutto a posto sia su training che su test



#SELEZIONE DI VARIABILI#
#BORUTA#
boruta <- Boruta(cbind(train1[,1:9], log1p(train1[,10:99])), log1p(train1$y))
#X e y devono essere convertiti a dataframe e vettore
# Esegui l'algoritmo Boruta
boruta_result <- Boruta::getSelectedAttributes(boruta)
# Visualizza i risultati
print(boruta_result)



#####REGRESSIONE#####
rf <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)
  
  #decido di togliere ch.sms e ch.cc 
  #sia da train che da test
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  # Imposta il numero di core da utilizzare per la parallelizzazione
  no_cores <- availableCores() - 1
  plan(multicore, workers = no_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )
  rf_model <- train(
    y ~ .,                        # Formula della variabile risposta e predittori
    data = train.regr,             # Dataset di addestramento
    method = "rf",                 # Metodo di addestramento (random forest)
    trControl = ctrl,              # Controllo per la cross-validation
    tuneLength = 5,               # prova 5 possibili combinazioni di iperparametri
    importance = TRUE              # Calcola l'importanza delle variabili
  )

  return(rf_model)
}

risultati_rf=rf(train1)
risultati_rf
#la migliore configurazione è stata trovata per mtry=16
1.982086^2*15310
#errore stimato è di 60527.6 (in linea con le aspettative)
#Provo a calcolare a mano:
sum((log1p(train1$y)-predict(risultati_rf))^2)



xgb <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)
  
  #decido di togliere ch.sms e ch.cc 
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  #addestro rf, xgboost
  # Imposta il numero di core da utilizzare per la parallelizzazione
  no_cores <- availableCores() - 1
  plan(multicore, workers = no_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )

  xgb_model <- train(
    y ~ .,                        # Formula della variabile risposta e predittori
    data = train.regr,             # Dataset di addestramento
    method = "xgbTree",            # Metodo di addestramento (XGBoost)
    trControl = ctrl,              # Controllo per la cross-validation
    tuneLength = 5               # prova 5 possibili combinazioni di iperparametri
  )
  
  return(xgb_model)
}

risultati_xgb=xgb(train1)
risultati_xgb



marsmodel <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)
  
  #decido di togliere ch.sms e ch.cc 
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  # Imposta il numero di core da utilizzare per la parallelizzazione
  no_cores <- availableCores() - 1
  plan(multicore, workers = no_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )

  mars_model <- train(
    y ~ .,                        # Formula della variabile risposta e predittori
    data = train.regr,             # Dataset di addestramento
    method = "earth",              # Metodo di addestramento (MARS)
    trControl = ctrl,              # Controllo per la cross-validation
    tuneLength = 10
  )

  return(mars_model)
}

risultati_mars=marsmodel(train1)
risultati_mars
2.014457^2*15309



lasso <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)

  #decido di togliere ch.sms e ch.cc 
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  #addestro rf, xgboost
  # Imposta il numero di core da utilizzare per la parallelizzazione
  n_cores <- detectCores()
  registerDoParallel(cores = n_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )

  lasso_model <- train(y ~ ., data = train.regr, method = "glmnet", 
                       trControl = ctrl, tuneLength = 5)

  return(lasso_model)
}

risultati_lasso=lasso(train1)
risultati_lasso
2.063283^2*15309
#l'errore stimato è 65112.19



svm_linear <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)

  #decido di togliere ch.sms e ch.cc 
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  # Imposta il numero di core da utilizzare per la parallelizzazione
  n_cores <- detectCores()
  registerDoParallel(cores = n_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )

  svm_model <- train(y ~ ., data = train.regr, method = "svmLinear", 
                     trControl = ctrl, tuneLength = 3)
  
  return(svm_model)
}


risultati_svmlinear=svm_linear(train1)
risultati_svmlinear
2.104935^2*15309
#errore stimato 67830.37



svm_Poly <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)
  
  #decido di togliere ch.sms e ch.cc 
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  # Imposta il numero di core da utilizzare per la parallelizzazione
  n_cores <- detectCores()
  registerDoParallel(cores = n_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )
  
  svm_model <- train(y ~ ., data = train.regr, method = "svmPoly", 
                     trControl = ctrl, tuneLength = 3)
  
  return(svm_model)
}

risultati_svmpoly=svm_Poly(train1)
risultati_svmpoly
2.056449^2*15309



svm_Radial <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)

  #decido di togliere ch.sms e ch.cc 
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  # Imposta il numero di core da utilizzare per la parallelizzazione
  n_cores <- detectCores()
  registerDoParallel(cores = n_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )
  
  svm_model <- train(y ~ ., data = train.regr, method = "svmRadial", 
                     trControl = ctrl, tuneLength = 3)
  
  return(svm_model)
}

risultati_svmradial=svm_Radial(train1)
risultati_svmradial
2.055624^2*15309




cubista <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)

  #decido di togliere ch.sms e ch.cc 
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  #addestro rf, xgboost
  # Imposta il numero di core da utilizzare per la parallelizzazione
  n_cores <- detectCores()
  registerDoParallel(cores = n_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )
  
  svm_model <- train(y ~ ., data = train.regr, method = "cubist", 
                     trControl = ctrl, tuneLength = 5)
  
  return(svm_model)
}

risultati_cubist=cubista(train1)
risultati_cubist
2.043859^2*15309




ridge <- function(training) {
  train.regr <- training %>% 
    mutate(across(starts_with("q"), .fns = log1p)) %>%
    mutate(y = log1p(y))
  
  for (i in 1:9) {
    train.regr[, paste0("q0", i, ".out.dur.tot")] <- train.regr[, paste0("q0", i, ".out.dur.peak")] + train.regr[, paste0("q0", i, ".out.dur.offpeak")]
    train.regr[, c(paste0("q0", i, ".out.dur.peak"), paste0("q0", i, ".out.dur.offpeak"))] <- NULL
  }
  y <- train.regr$y
  train.regr <- train.regr %>% 
    select(-y) %>% 
    mutate(y = y)
  
  #decido di togliere ch.sms e ch.cc 
  train.regr <- train.regr[, -grep("ch.sms$|ch.cc$", names(train.regr))]
  
  #tolgo var fattoriali inutili
  train.regr <- train.regr %>% 
    select(-c("vas1", "vas2", "status", "activ.area"))
  
  #rimuovo i primi 3 mesi
  vars <- c("q01.out.ch.peak", "q01.out.val.peak", "q01.out.ch.offpeak", "q01.out.val.offpeak", "q01.in.ch.tot", "q01.in.dur.tot",
            "q02.out.ch.peak", "q02.out.val.peak", "q02.out.ch.offpeak", "q02.out.val.offpeak", "q02.in.ch.tot", "q02.in.dur.tot",
            "q03.out.ch.peak", "q03.out.val.peak", "q03.out.ch.offpeak", "q03.out.val.offpeak", "q03.in.ch.tot", "q03.in.dur.tot")
  train.regr <- train.regr %>%
    select(-vars)
  
  # Imposta il numero di core da utilizzare per la parallelizzazione
  n_cores <- detectCores()
  registerDoParallel(cores = n_cores)
  ctrl <- trainControl(
    method = "cv",                 # Utilizza la cross-validation
    number = 5,                    # Numero di folds della cross-validation
    allowParallel = TRUE,         # Abilita la parallelizzazione
    savePredictions = T          #salva le previsioni ottenute dal modello
  )
  
  svm_model <- train(y ~ ., data = train.regr, method = "ridge", 
                     trControl = ctrl, tuneLength = 5)
  
  return(svm_model)
}

risultati_ridge=ridge(train1)
risultati_ridge
2.062855^2*15309


#risultati_WM
#provare a calcolare manualmente l'errore e vedere se coincide con quello stimato


#####ENSEMBLE#####
#Ora raccolgo tutte le prevsioni che faranno da input per il model stacking
#RANDOM FOREST#
previsioni_rf=risultati_rf$pred[which(risultati_rf$pred$mtry==16),]
previsioni_rf=previsioni_rf[order(previsioni_rf$rowIndex),"pred"]
sum((log1p(train1$y)-previsioni_rf)^2) #torna perfettamente
previsioni_rf[which(previsioni_rf<0)]=0 #le osservazioni minori di 0 le rendo pari a 0
sum((log1p(train1$y)-previsioni_rf)^2)

#XGB#
previsioni_xgb=risultati_xgb$pred[which(risultati_xgb$pred$nrounds==50 & risultati_xgb$pred$max_depth==3 & 
                                          risultati_xgb$pred$eta==0.3 & risultati_xgb$pred$gamma==0 & 
                                          risultati_xgb$pred$colsample_bytree==0.8 & 
                                          risultati_xgb$pred$min_child_weight==1 & risultati_xgb$pred$subsample==1),]
previsioni_xgb=previsioni_xgb[order(previsioni_xgb$rowIndex), "pred"]
sum((log1p(train1$y)-previsioni_xgb)^2)
previsioni_xgb[which(previsioni_xgb<0)]=0 #le osservazioni minori di 0 le rendo pari a 0
sum((log1p(train1$y)-previsioni_xgb)^2)

#MARS#
previsioni_mars=risultati_mars$pred[which(risultati_mars$pred$nprune==14),]
previsioni_mars=previsioni_mars[order(previsioni_mars$rowIndex),"pred"]
sum((log1p(train1$y)-previsioni_mars)^2)
previsioni_mars[which(previsioni_mars<0)]=0 #le osservazioni minori di 0 le rendo pari a 0
sum((log1p(train1$y)-previsioni_mars)^2)

#LASSO#
previsioni_lasso=risultati_lasso$pred[which(risultati_lasso$pred$alpha==0.55 & round(risultati_lasso$pred$lambda,4)==round(0.002213746,4)),]
previsioni_lasso=previsioni_lasso[order(previsioni_lasso$rowIndex),"pred"]
length(previsioni_lasso)
sum((log1p(train1$y)-previsioni_lasso)^2)
previsioni_lasso[which(previsioni_lasso<0)]=0
sum((log1p(train1$y)-previsioni_lasso)^2)

#SVM LINEAR#
previsioni_svmlinear=risultati_svmlinear$pred[order(risultati_svmlinear$pred$rowIndex),"pred"]
length(previsioni_svmlinear)
sum((log1p(train1$y)-previsioni_svmlinear)^2)
previsioni_svmlinear[which(previsioni_svmlinear<0)]=0
sum((log1p(train1$y)-previsioni_svmlinear)^2)

#SVM POLY#
previsioni_svmpoly=risultati_svmpoly$pred[which(risultati_svmpoly$pred$degree==3 & 
                                                risultati_svmpoly$pred$scale==0.01 & risultati_svmpoly$pred$C==0.25),]
previsioni_svmpoly=previsioni_svmpoly[order(previsioni_svmpoly$rowIndex),"pred"]
length(previsioni_svmpoly)
sum((log1p(train1$y)-previsioni_svmpoly)^2)
previsioni_svmpoly[which(previsioni_svmpoly<0)]=0
sum((log1p(train1$y)-previsioni_svmpoly)^2)

#SVM RADIAL#
previsioni_svmradial=risultati_svmradial$pred[which(risultati_svmradial$pred$C==1),]
previsioni_svmradial=previsioni_svmradial[order(previsioni_svmradial$rowIndex),"pred"]
length(previsioni_svmradial)
sum((log1p(train1$y)-previsioni_svmradial)^2)
previsioni_svmradial[which(previsioni_svmradial<0)]=0
sum((log1p(train1$y)-previsioni_svmradial)^2)

#SVM CUBIST#
previsioni_cubist=risultati_cubist$pred[which(risultati_cubist$pred$committees==20 & risultati_cubist$pred$neighbors==0),]
previsioni_cubist=previsioni_cubist[order(previsioni_cubist$rowIndex),"pred"]
length(previsioni_cubist)
sum((log1p(train1$y)-previsioni_cubist)^2)

#RIDGE#
previsioni_ridge=risultati_ridge$pred[which(risultati_ridge$pred$lambda==0.001),]
previsioni_ridge=previsioni_ridge[order(previsioni_ridge$rowIndex),"pred"]
length(previsioni_ridge)
sum((log1p(train1$y)-previsioni_ridge)^2)
previsioni_ridge[which(previsioni_ridge<0)]=0
sum((log1p(train1$y)-previsioni_ridge)^2)


###MODEL STACKING###
# Creazione del training set per il model stacking
stacking_data <- data.frame(
  previsioni_cubist,
  previsioni_lasso,
  previsioni_mars,
  previsioni_rf,
  previsioni_ridge,
  previsioni_svmlinear,
  previsioni_svmpoly,
  previsioni_svmradial,
  previsioni_xgb,
  y = log1p(train1$y)
)
stacking_data
#stacking_data[which(stacking_data$previsioni_xgb<0.5),"y"]
stacking_data$y==0
stacking_data[which(stacking_data$y==0),]


# Addestramento del modello di stacking
stacking_data_rf=stacking_data[,c("previsioni_cubist", "previsioni_mars", "previsioni_rf", "previsioni_ridge", "previsioni_svmlinear", "y")]
stacking_data_rf$interazione=(stacking_data_rf$previsioni_cubist*stacking_data_rf$previsioni_mars*stacking_data_rf$previsioni_rf*stacking_data_rf$previsioni_ridge*stacking_data_rf$previsioni_svmlinear)^(1/5)
stack_model <- train(
  y ~ .,
  data = stacking_data_rf,
  method = "rf",
  trControl=trainControl(method = "cv", number = 5,savePredictions = T),
  tuneLength= 5)
stack_model
#risultato con stacking_data:
1.984340^2*15310
#miglioramento ridicolo rispetto alla rf aggregando con rf
#risultato con stacking_data_rf:
#ancora peggio, lascia stare

#ENSEMBLE CON MEDIA#
str(stacking_data[,-10])
rowMeans(stacking_data[,-10])
sum((log1p(train1$y)-rowMeans(stacking_data[,-10]))^2)
#peggioramento rispetto a rf singola

#MODEL STACKING GLMNET#
set.seed(123)
stack_model_glmnet <- train(
  y ~ ., 
  data = stacking_data, 
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 50, savePredictions = T),
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, 0.1)),
  na.action = na.pass
)
stack_model_glmnet
1.966845^2*15309


#MODEL STACKING LM#
set.seed(123)
stack_model_lm <- train(
  y ~ previsioni_mars+previsioni_rf+previsioni_cubist+previsioni_svmradial,#+previsioni_xgb+previsioni_cubist, 
  data = stacking_data, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 50, savePredictions = T),
  tuneGrid = expand.grid(intercept=F)
)
stack_model_lm
1.967408^2*15309
stack_model_lm$finalModel
summary(stack_model_lm)
0.088793+0.171837+0.581407+0.002432+0.0099410+0.055186


#prova sul testset
load("vector3.RData")
vetsenzana<-na.omit(vet)
test<-na.omit(totale[vetsenzana,])
predict(stack_model_lm, test)
sum((as.vector(predict(risultati_rf, test))-log1p(test$y))^2)
str(test$y)
str(as.vector(predict(stack_model_lm, test)))







#####CARET ENSEMBLE#####
models=list(risultati_cubist, risultati_lasso, risultati_mars, risultati_rf, risultati_ridge,
            risultati_svmradial,risultati_xgb)
models=caretList(models)
stack=caretStack(models)





#MODEL STACKING #
# Crea una lista dei modelli base
base_models <- list(risultati_cubist, risultati_lasso, risultati_mars, risultati_rf, 
                    risultati_ridge, risultati_svmlinear, risultati_svmpoly, risultati_svmradial
                    , risultati_xgb)

# Crea il modello di stacking
stack_model <- stackModels(
  base_models,
  method = "glm",
  data = stacking_data,
  y = "y",
  stack_control = stackControl(method = "cv", number = 5)
)



write.table(file="843370_previsione.txt", yhat, row.names = FALSE, col.names = FALSE)

hist(train1$y, freq=F, col="red", main = "", xlab = "log(1+y)", breaks = 50)
hist(expm1(train1$y), freq=F, col="red", main = "", xlab = "y", breaks = 50)

boxplot(y~tariff.plan, data=train1, col="red", ylab = "log(1+y)")
boxplot(log1p(y)~activ.area, data=test, col="red", ylab = "log(1+y)")





