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



#df <- read_dataset("src/data/drivendata/train.csv")


imp=mice(df, maxit=3, meth='pmm', seed=1)

df=complete(imp)


df_copy=df
df_copy$h1n1_vaccine=as.numeric(levels(df$h1n1_vaccine))[df$h1n1_vaccine]
df_copy$seasonal_vaccine=as.numeric(levels(df$seasonal_vaccine))[df$seasonal_vaccine]
df_labels=collapse_labels(df_copy, 36, 37)
df_labels=df_labels %>% mutate_if(is.character, as.factor)
df_labels=df_labels %>% mutate_if(is.numeric, as.factor)

# feature_search, probar con esa
#pesosG <- FSelectorRcpp::information_gain(target~., df_labels)
#mejores <- FSelectorRcpp::cut_attrs(pesosG, k=0.5)
#mejores[[length(mejores) + 1]]="target"
#df_filter=df_labels %>% select(mejores)
df_filter=df_labels

set.seed(9)
#Hacer bagging



# Aplico el algoritmo Ripper
model.Ripper = JRip(target~., df_filter, control = Weka_control())
# ctrl <- trainControl(method="LGOCV", p=0.8, seeds=NA)
# grid <- expand.grid(MinWeights=c(1, 2, 5, 10, 15), NumOpt=5, NumFolds=1:5)
# model.Ripper= train(target~. , data=df_labels , method='JRip', na.action = na.pass, trControl=ctrl,tuneGrid=grid)
#saveRDS(model.Ripper, "modelRipper.rds")


####TEST DENTRO DEL TRAIN######################################################################################


model.Ripper.pred = predict(model.Ripper, newdata = df_filter, type = 'probability')

prediccion=as.data.frame(model.Ripper.pred)

prediccion=prediccion %>% mutate(id=as.numeric(rownames(prediccion))) %>% relocate(id)

prediccion_split=as.data.frame(split_target(prediccion))


ROC_h1n1=roc(prediccion_split[,2],df[,36])
acierto_h1n1=auc(ROC_h1n1)
acierto_h1n1

ROC_seasonal=roc(prediccion_split[,3], df[,37])
acierto_seasonal=auc(ROC_seasonal)
acierto_seasonal

training_multi=mean(c(acierto_h1n1, acierto_seasonal))

####PARA ENVIAR A DRIVENDATA######################################################################################


imp=mice(features_test, maxit=3, meth='pmm', seed=1)
features_test=complete(imp)



# model.Ripper.pred.test = predict(model.Ripper, newdata = features_test, type = 'probability')
# 
# prediccion_test=as.data.frame(model.Ripper.pred.test)
# prediccion_test=prediccion_test %>% mutate(id=1) %>% relocate(id)
# 
# prediccion_split_test=split_target(prediccion_test)
# prediccion_split_test=as.data.frame(prediccion_split_test)
# 
# submission_dataframe=get_submission_dataframe(prediccion_split_test$h1n1_vaccine, prediccion_split_test$seasonal_vaccine)
# 
# write_csv(submission_dataframe, "submission_JRIP.csv")               

#####################################################################################################################################################
####Con multiclasificador (es decir, con dos clasificadores)#########################################################################################
#####################################################################################################################################################


#Preparamos el dataset para sin multiclasificador
df_copy=df_copy %>% mutate_if(is.character, as.factor)
df_copy=df_copy %>% mutate_if(is.numeric, as.factor)

df_copy.h1n1=df_copy %>% select(-c("seasonal_vaccine"))
df_copy.seasonal=df_copy %>% select(-c("h1n1_vaccine"))

# Aplico el algoritmo Ripper
model.Ripper.h1n1 = JRip(h1n1_vaccine~., df_copy.h1n1,control = Weka_control(N=5, O=5, F=5))
model.Ripper.seasonal = JRip(seasonal_vaccine~., df_copy.seasonal,control = Weka_control(N=5, O=5, F=5))


####TEST DENTRO DEL TRAIN######################################################################################


model.Ripper.pred.h1n1 = predict(model.Ripper.h1n1, newdata = df_copy, type = 'probability')
model.Ripper.pred.seasonal = predict(model.Ripper.seasonal, newdata = df_copy, type = 'probability')

prediccion.h1n1=as.data.frame(model.Ripper.pred.h1n1)
prediccion.seasonal=as.data.frame(model.Ripper.pred.seasonal)

prediccion.h1n1=prediccion.h1n1 %>% mutate(id=as.numeric(rownames(prediccion.h1n1))) %>% relocate(id)
prediccion.seasonal=prediccion.seasonal %>% mutate(id=as.numeric(rownames(prediccion.seasonal))) %>% relocate(id)



ROC_h1n1=roc(prediccion.h1n1[,3],df[,36])
acierto_h1n1=auc(ROC_h1n1)
acierto_h1n1

ROC_seasonal=roc(prediccion.seasonal[,3], df[,37])
acierto_seasonal=auc(ROC_seasonal)
acierto_seasonal

training_separado=mean(c(acierto_h1n1, acierto_seasonal))




####PARA ENVIAR A DRIVENDATA######################################################################################

model.Ripper.pred.h1n1.test = predict(model.Ripper.h1n1, newdata = features_test, type = 'probability')
model.Ripper.pred.seasonal.test = predict(model.Ripper.seasonal, newdata = features_test, type = 'probability')

prediccion.h1n1.test=as.data.frame(model.Ripper.pred.h1n1.test)
prediccion.seasonal.test=as.data.frame(model.Ripper.pred.seasonal.test)

prediccion.h1n1.test=prediccion.h1n1.test %>% mutate(id=as.numeric(rownames(prediccion.h1n1.test))) %>% relocate(id)
prediccion.seasonal.test=prediccion.seasonal.test %>% mutate(id=as.numeric(rownames(prediccion.seasonal.test))) %>% relocate(id)

submission_dataframe=get_submission_dataframe(prediccion.h1n1.test[,3], prediccion.seasonal.test[,3])

write_csv(submission_dataframe, "submission_JRIP_multi.csv")      


training_multi
training_separado


###############################################################################################################################
#### Con Bagging###############################################################################################################
###############################################################################################################################



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

train_bagging_JRip_collapsed <- function (number_classifiers, data) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(data)[1], replace = T, size = dim(data)[1])
      JRip(target~., data, subset=samples,control = Weka_control(N=5, O=5, F=5))
    }
  )
}


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
  )
}

models_h1n1 <- train_bagging_JRip_h1n1(
  3,
  df_copy.h1n1
)



models_seasonal <- train_bagging_JRip_seasonal(
  3,
  df_copy.seasonal
)



###Training#############################################################################################################
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

###Test#############################################################################################################


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


models_h1n1 <- train_bagging_JRip_h1n1(
  3,
  df_copy.h1n1
)
