source("src/utils/functions.R")
source("src/utils/preprocess.R")
source("src/utils/preprocess_test.R")

library(AUC)
options(java.home="C:\\Program Files (x86)\\Java\\jre1.8.0_291")
library(RWeka)
library(mice)
library(caret)
library(FSelectorRcpp)

library(tidyverse)
library(Amelia)
library(naniar)



#Imputamos los datos faltantes
imp=mice(df, maxit=3, meth='pmm', seed=1)

df=complete(imp)

#Preparamos el dataset para que tenga una sola clase a predecir, a la cual llamaremos target
df_copy=df
df_copy$h1n1_vaccine=as.numeric(levels(df$h1n1_vaccine))[df$h1n1_vaccine]
df_copy$seasonal_vaccine=as.numeric(levels(df$seasonal_vaccine))[df$seasonal_vaccine]
df_labels=collapse_labels(df_copy, 36, 37)
df_labels=df_labels %>% mutate_if(is.character, as.factor)
df_labels=df_labels %>% mutate_if(is.numeric, as.factor)

set.seed(9)

# Aplico el algoritmo Ripper
model.Ripper = JRip(target~., df_labels, control = Weka_control(N=5, O=5, F=5))
# ctrl <- trainControl(method="LGOCV", p=0.8, seeds=NA)
# grid <- expand.grid(MinWeights=c(1, 2, 5, 10, 15), NumOpt=5, NumFolds=1:5)
# model.Ripper= train(target~. , data=df_labels , method='JRip', na.action = na.pass, trControl=ctrl,tuneGrid=grid)
#saveRDS(model.Ripper, "modelRipper.rds")



####CALCULAMOS EL VALOR AUCROC EN TRAINING######################################################################################

#Realizamos la predicción
model.Ripper.pred = predict(model.Ripper, newdata = df_labels, type = 'probability')

prediccion=as.data.frame(model.Ripper.pred)

prediccion=prediccion %>% mutate(id=as.numeric(rownames(prediccion))) %>% relocate(id)

prediccion_split=as.data.frame(split_target(prediccion))

#Calculamos el AUCROC
ROC_h1n1=roc(prediccion_split[,2],df[,36])
acierto_h1n1=auc(ROC_h1n1)
acierto_h1n1

ROC_seasonal=roc(prediccion_split[,3], df[,37])
acierto_seasonal=auc(ROC_seasonal)
acierto_seasonal

training_multi=mean(c(acierto_h1n1, acierto_seasonal))

####PARA ENVIAR A DRIVENDATA PARA CALCULAR EL TEST######################################################################################

#Imputamos los datos faltantes en test
imp=mice(features_test, maxit=3, meth='pmm', seed=1)
features_test=complete(imp)

#Realizamos la predicción para test

model.Ripper.pred.test = predict(model.Ripper, newdata = features_test, type = 'probability')

#Preparamos los datos para subirlos a DrivenData
prediccion_test=as.data.frame(model.Ripper.pred.test)
prediccion_test=prediccion_test %>% mutate(id=1) %>% relocate(id)

prediccion_split_test=split_target(prediccion_test)
prediccion_split_test=as.data.frame(prediccion_split_test)

submission_dataframe=get_submission_dataframe(prediccion_split_test$h1n1_vaccine, prediccion_split_test$seasonal_vaccine)

write_csv(submission_dataframe, "submission_JRIP_collapsed.csv")

#####################################################################################################################################################
####Con multiclasificador (es decir, con dos clasificadores)#########################################################################################
#####################################################################################################################################################


#Preparamos el dataset para con multiclasificador
df_copy=df_copy %>% mutate_if(is.character, as.factor)
df_copy=df_copy %>% mutate_if(is.numeric, as.factor)

df_copy.h1n1=df_copy %>% select(-c("seasonal_vaccine"))
df_copy.seasonal=df_copy %>% select(-c("h1n1_vaccine"))

# Aplico el algoritmo Ripper
model.Ripper.h1n1 = JRip(h1n1_vaccine~., df_copy.h1n1,control = Weka_control(N=5, O=5, F=5))
model.Ripper.seasonal = JRip(seasonal_vaccine~., df_copy.seasonal,control = Weka_control(N=5, O=5, F=5))


####CALCULAMOS EL VALOR AUCROC EN TRAINING######################################################################################

#Realizamos la predicción
model.Ripper.pred.h1n1 = predict(model.Ripper.h1n1, newdata = df_copy, type = 'probability')
model.Ripper.pred.seasonal = predict(model.Ripper.seasonal, newdata = df_copy, type = 'probability')

prediccion.h1n1=as.data.frame(model.Ripper.pred.h1n1)
prediccion.seasonal=as.data.frame(model.Ripper.pred.seasonal)

prediccion.h1n1=prediccion.h1n1 %>% mutate(id=as.numeric(rownames(prediccion.h1n1))) %>% relocate(id)
prediccion.seasonal=prediccion.seasonal %>% mutate(id=as.numeric(rownames(prediccion.seasonal))) %>% relocate(id)


#Calculamos el AUCROC
ROC_h1n1=roc(prediccion.h1n1[,3],df[,36])
acierto_h1n1=auc(ROC_h1n1)
acierto_h1n1

ROC_seasonal=roc(prediccion.seasonal[,3], df[,37])
acierto_seasonal=auc(ROC_seasonal)
acierto_seasonal

training_separado=mean(c(acierto_h1n1, acierto_seasonal))




####PARA ENVIAR A DRIVENDATA PARA CALCULAR EL TEST######################################################################################

#Realizamos las predicciones con ambos modelos
model.Ripper.pred.h1n1.test = predict(model.Ripper.h1n1, newdata = features_test, type = 'probability')
model.Ripper.pred.seasonal.test = predict(model.Ripper.seasonal, newdata = features_test, type = 'probability')

#Preparamos los datos par subirlos a DrivenData
prediccion.h1n1.test=as.data.frame(model.Ripper.pred.h1n1.test)
prediccion.seasonal.test=as.data.frame(model.Ripper.pred.seasonal.test)

prediccion.h1n1.test=prediccion.h1n1.test %>% mutate(id=as.numeric(rownames(prediccion.h1n1.test))) %>% relocate(id)
prediccion.seasonal.test=prediccion.seasonal.test %>% mutate(id=as.numeric(rownames(prediccion.seasonal.test))) %>% relocate(id)

submission_dataframe=get_submission_dataframe(prediccion.h1n1.test[,3], prediccion.seasonal.test[,3])

write_csv(submission_dataframe, "submission_JRIP_multi.csv")


###############################################################################################################################
#### Con Bagging###############################################################################################################
###############################################################################################################################


# Entrenamiento de los modelos con Bagging con multiclasificador, uno para cada variable
train_bagging_JRip_h1n1 <- function (number_classifiers, data) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(data)[1], replace = T, size = dim(data)[1])
      JRip(h1n1_vaccine~., data, subset=samples,control = Weka_control(N=5, O=5, F=5))
    }
  )
}

train_bagging_JRip_seasonal <- function (number_classifiers, data) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(data)[1], replace = T, size = dim(data)[1])
      JRip(seasonal_vaccine~., data, subset=samples,control = Weka_control(N=5, O=5, F=5))
    }
  )
}

#Entrenamiento de los modelos con Bagging sin multiclasificador, uno para cada variable

train_bagging_JRip_collapsed <- function (number_classifiers, data) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(data)[1], replace = T, size = dim(data)[1])
      JRip(target~., data, subset=samples,control = Weka_control(N=5, O=5, F=5))
    }
  )
}

#Predicción de Bagging con Multiclasificador
predict_bagging <- function(models, test) {
  apply(
    array(
      unlist(
        lapply(
          1:length(models),
          function(i) {
            predict(models[[i]], newdata = test, type="probability")
          }
        )
      ),
      dim = c(dim(test)[1], 2, length(models))
    ),
   c(1, 2),
   mean
 )
}

#Predicción de Bagging sin Multiclasificador
predict_bagging_collapsed <- function(models, test) {
  apply(
    array(
      unlist(
        lapply(
          1:length(models),
          function(i) {
            predict(models[[i]], newdata = test, type="probability")
          }
        )
      ),
      dim = c(dim(test)[1], 4, length(models))
    ),
    c(1, 2),
    mean
  )
}

#Construcción de los modelos con Bagging con multiclasificador
models_h1n1 <- train_bagging_JRip_h1n1(
  5,
  df_copy.h1n1
)



models_seasonal <- train_bagging_JRip_seasonal(
  5,
  df_copy.seasonal
)



###Training con multiclasificador#############################################################################################################
preds_h1n1 <- predict_bagging(
  models_h1n1,
  df_copy
)

preds_seasonal <- predict_bagging(
  models_seasonal,
  df_copy
)

ROC_h1n1=roc(preds_h1n1[,2],df[,36])
acierto_h1n1=auc(ROC_h1n1)
acierto_h1n1

ROC_seasonal=roc(preds_seasonal[,2], df[,37])
acierto_seasonal=auc(ROC_seasonal)
acierto_seasonal

training_bagging=mean(c(acierto_h1n1, acierto_seasonal))

###Test con multiclasificador, para enviar a DrivenData con Bagging#############################################################################################################


preds_h1n1_test <- predict_bagging(
  models_h1n1,
  features_test
)

preds_seasonal_test <- predict_bagging(
  models_seasonal,
  features_test
)

submission <- get_submission_dataframe(
  preds_h1n1_test[, 2],
  preds_seasonal_test[, 2]
)


write_csv(submission, "submission_JRIP_bagging.csv")   

###Training sin multiclasificador#############################################################################################################

models_collapsed <- train_bagging_JRip_collapsed(
  5,
  df_labels
)

preds_collapsed <-predict_bagging_collapsed(
  models_collapsed,
  df_labels
)


preds_collapsed=as.data.frame(preds_collapsed)

preds_collapsed=preds_collapsed %>% mutate(id=as.numeric(rownames(preds_collapsed))) %>% relocate(id)

preds_collapsed=as.data.frame(split_target(preds_collapsed))


ROC_h1n1=roc(preds_collapsed[,2],df[,36])
acierto_h1n1=auc(ROC_h1n1)
acierto_h1n1

ROC_seasonal=roc(preds_collapsed[,3], df[,37])
acierto_seasonal=auc(ROC_seasonal)
acierto_seasonal

training_collapsed_bagging=mean(c(acierto_h1n1, acierto_seasonal))


###Test sin multiclasificador, para enviar a DrivenData con Bagging#############################################################################################################

preds_collapsed_test <-predict_bagging_collapsed(
  models_collapsed,
  features_test
)

preds_collapsed_test=as.data.frame(preds_collapsed_test)
preds_collapsed_test=preds_collapsed_test %>% mutate(id=1) %>% relocate(id)

prediccion_split_test_bagging=split_target(preds_collapsed_test)
prediccion_split_test_bagging=as.data.frame(prediccion_split_test_bagging)

submission_dataframe=get_submission_dataframe(prediccion_split_test_bagging$h1n1_vaccine, prediccion_split_test_bagging$seasonal_vaccine)

write_csv(submission_dataframe, "submission_JRIP_collapsed_bagging.csv")