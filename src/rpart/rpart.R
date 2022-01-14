#RPART#

#Librerias

library(tidyverse)
library(AUC)
library(rpart)
library(NoiseFiltersR)
library(FSelectorRcpp)

#Función para separar probabilidades

split_target <- function(probs_dataframe) {
  # Se asume que probs_dataframe tiene una fila por instancia y es de la forma (id, prob(0,0), prob(0,1), prob(1,0), prob(1,1)).
  
  new_dataframe <- apply(probs_dataframe, 1, function(row) c(
    row[1], # id
    row[4]+row[5], # p(10)+p(11)
    row[3]+row[5]  # p(01)+(11)
  )) %>% t()
  
  colnames(new_dataframe) = c("respondent_id", "h1n1_vaccine", "seasonal_vaccine")
  return(new_dataframe)
}


#Carga de datos originales

features = as.data.frame(read_csv('training_set_features.csv'))
labels = as.data.frame(read_csv('training_set_labels.csv'))

df = merge(features,labels,by = "respondent_id")

df = df %>% select(-respondent_id)

df_test = as.data.frame(read_csv('test_set_features.csv'))

#Convertir a factor

df = df %>% mutate_if(is.character, as.factor)
df_test = df_test %>% mutate_if(is.character, as.factor)

df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)

#Creación de conjuntos de train y test

train = sample(1:nrow(df),2*nrow(df)/3)
test = df[-train,]

#Modelo con dos etiquetas (multiclasificador)

model.rPart.h1n1 = rpart(h1n1_vaccine~., data=df[,-37], subset=train)
summary(model.rPart.h1n1)

model.rPart.seasonal = rpart(seasonal_vaccine~., data=df[,-36], subset=train)
summary(model.rPart.seasonal)

#Prediccion con multiclasificador

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = test, type = "prob")
model.rPart.h1n1.pred

auc.h1n1 = auc(roc(model.rPart.h1n1.pred[, 2], test$h1n1_vaccine))
auc.h1n1

model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = test, type = "prob")
model.rPart.seasonal.pred

auc.seasonal = auc(roc(model.rPart.seasonal.pred[, 2], test$seasonal_vaccine))
auc.seasonal

#Con conjuntos dados de train y test 

train_h1n1 = df[,-37]
train_seasonal = df[,-36]

model.rPart.h1n1 = rpart(h1n1_vaccine~., data = train_h1n1)
model.rPart.seasonal = rpart(seasonal_vaccine~., data = train_seasonal)

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = df_test, type = "prob")
model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = df_test, type = "prob")

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = model.rPart.h1n1.pred[,2]
submission$seasonal_vaccine = model.rPart.seasonal.pred[,2]

submission

write_csv(submission, "datos_originales_multi.csv")

#0.7321

#Modelo con una etiqueta

#Unimos etiquetas

df$target = paste(
  df[[36]],
  df[[37]],
  sep=""
) %>% as.factor()

df$h1n1_vaccine = NULL
df$seasonal_vaccine = NULL

#Creacion de conjuntos de train y test

train = sample(1:nrow(df),2*nrow(df)/3)
test = df[-train,]

#Modelo con una etiqueta

model.rPart = rpart(target~., data=df, subset=train)
summary(model.rPart)

#Prediccion

model.rPart.pred = predict(model.rPart, newdata = test, type = "prob")
model.rPart.pred


#Con conjuntos dados de train y test 

model.rPart = rpart(target~., data=df)

model.rPart.pred = predict(model.rPart, newdata = df_test, type = "prob")

prediccion_test = as.data.frame(model.rPart.pred)
prediccion_test = prediccion_test %>% mutate(id=as.numeric(rownames(model.rPart.pred))) %>% relocate(id)
prediccion_split_test = split_target(prediccion_test)
prediccion_split_test = as.data.frame(prediccion_split_test)
prediccion_split_test

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = prediccion_split_test$h1n1_vaccine
submission$seasonal_vaccine = prediccion_split_test$seasonal_vaccine

submission

write_csv(submission, "datos_originales.csv")

#0.7429

#Carga de datos preprocesados

df = as.data.frame(read_csv('train_imputed.csv'))

df_test = as.data.frame(read_csv('test_imputed.csv'))

#Convertir a factor

df = df %>% mutate_if(is.character, as.factor)
df_test = df_test %>% mutate_if(is.character, as.factor)

df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)

#Creacion de conjuntos de train y test

train = sample(1:nrow(df),2*nrow(df)/3)
test = df[-train,]

#Modelo con dos etiquetas (multiclasificador)

model.rPart.h1n1 = rpart(h1n1_vaccine~., data=df[,-37], subset=train)
summary(model.rPart.h1n1)

model.rPart.seasonal = rpart(seasonal_vaccine~., data=df[,-36], subset=train)
summary(model.rPart.seasonal)

#Prediccion

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = test, type = "prob")
model.rPart.h1n1.pred

auc.h1n1 = auc(roc(model.rPart.h1n1.pred[, 2], test$h1n1_vaccine))
auc.h1n1

model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = test, type = "prob")
model.rPart.seasonal.pred

auc.seasonal = auc(roc(model.rPart.seasonal.pred[, 2], test$seasonal_vaccine))
auc.seasonal

#Con conjuntos dados de train y test 

train_h1n1 = df[,-37]
train_seasonal = df[,-36]

model.rPart.h1n1 = rpart(h1n1_vaccine~., data = train_h1n1)
model.rPart.seasonal = rpart(seasonal_vaccine~., data = train_seasonal)

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = df_test, type = "prob")
model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = df_test, type = "prob")

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = model.rPart.h1n1.pred[,2]
submission$seasonal_vaccine = model.rPart.seasonal.pred[,2]

submission

write_csv(submission, "datos_prepro_multi.csv")

#0.7329

#Modelo con una etiqueta


model.rPart = rpart(target~., data=df, subset=train)
summary(model.rPart)

#Prediccion

model.rPart.pred = predict(model.rPart, newdata = test, type = "prob")
model.rPart.pred


#Pruebo con los conjuntos dados de train y test 

model.rPart = rpart(target~., data=df)

model.rPart.pred = predict(model.rPart, newdata = df_test, type = "prob")

prediccion_test = as.data.frame(model.rPart.pred)
prediccion_test = prediccion_test %>% mutate(id=as.numeric(rownames(model.rPart.pred))) %>% relocate(id)
prediccion_split_test = split_target(prediccion_test)
prediccion_split_test = as.data.frame(prediccion_split_test)
prediccion_split_test

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = prediccion_split_test$h1n1_vaccine
submission$seasonal_vaccine = prediccion_split_test$seasonal_vaccine

submission

write_csv(submission, "datos_prepro.csv")

#0.7430

#Ambos clasificadores mejoran con los datos preprocesados ligeramente, 
#mantenemos estos datos y a continuacion quitamos ruido

#Quitar ruido en las dos etiquetas para el multiclasificador 

set.seed(1)
resultado_h1n1 = NoiseFiltersR::IPF(h1n1_vaccine ~ ., data=df)
df_h1n1 = resultado_h1n1$cleanData
str(df_h1n1)

resultado_seasonal = NoiseFiltersR::IPF(seasonal_vaccine ~ ., data=df)
df_seasonal = resultado_seasonal$cleanData
str(df_seasonal)

#Creacion de conjuntos de train y test

train_h1n1 = sample(1:nrow(df_h1n1),2*nrow(df_h1n1)/3)
test_h1n1 = df_h1n1[-train_h1n1,]

train_seasonal = sample(1:nrow(df_seasonal),2*nrow(df_seasonal)/3)
test_seasonal = df_seasonal[-train_seasonal,]

#Modelo con dos etiquetas (multiclasificador)

model.rPart.h1n1 = rpart(h1n1_vaccine~., data=df_h1n1[,-37], subset=train_h1n1)
summary(model.rPart.h1n1)

model.rPart.seasonal = rpart(seasonal_vaccine~., data=df_seasonal[,-36], subset=train_seasonal)
summary(model.rPart.seasonal)

#Prediccion

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = test_h1n1, type = "prob")
model.rPart.h1n1.pred

auc.h1n1 = auc(roc(model.rPart.h1n1.pred[, 2], test_h1n1$h1n1_vaccine))
auc.h1n1

model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = test_seasonal, type = "prob")
model.rPart.seasonal.pred

auc.seasonal = auc(roc(model.rPart.seasonal.pred[, 2], test_seasonal$seasonal_vaccine))
auc.seasonal

#Con conjuntos dados de train y test 

train_h1n1 = df_h1n1[,-37]
train_seasonal = df_seasonal[,-36]

model.rPart.h1n1 = rpart(h1n1_vaccine~., data = train_h1n1)
model.rPart.seasonal = rpart(seasonal_vaccine~., data = train_seasonal)

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = df_test, type = "prob")
model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = df_test, type = "prob")

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = model.rPart.h1n1.pred[,2]
submission$seasonal_vaccine = model.rPart.seasonal.pred[,2]

submission

write_csv(submission, "datos_prepro_ruidoambos_multi.csv")

#0.7653

#Quitando ruido solo en h1n1_vaccine para el multiclasificador (usamos df_h1n1 sin ruido calculado antes para el modelo de h1n1_vaccine
#y usamos el df con los datos con ruido para el modelo de seasonal_vaccine)

#Creo conjuntos de train y test

train_h1n1 = sample(1:nrow(df_h1n1),2*nrow(df_h1n1)/3)
test_h1n1 = df_h1n1[-train_h1n1,]

train_seasonal = sample(1:nrow(df),2*nrow(df)/3)
test_seasonal = df[-train_seasonal,]

#Modelo

model.rPart.h1n1 = rpart(h1n1_vaccine~., data=df_h1n1[,-37], subset=train_h1n1)
summary(model.rPart.h1n1)

model.rPart.seasonal = rpart(seasonal_vaccine~., data=df[,-36], subset=train_seasonal)
summary(model.rPart.seasonal)

#Prediccion

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = test_h1n1, type = "prob")
model.rPart.h1n1.pred

auc.h1n1 = auc(roc(model.rPart.h1n1.pred[, 2], test_h1n1$h1n1_vaccine))
auc.h1n1

model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = test_seasonal, type = "prob")
model.rPart.seasonal.pred

auc.seasonal = auc(roc(model.rPart.seasonal.pred[, 2], test_seasonal$seasonal_vaccine))
auc.seasonal

#Con conjuntos dados de train y test 

train_h1n1 = df_h1n1[,-37]
train_seasonal = df[,-36]

model.rPart.h1n1 = rpart(h1n1_vaccine~., data = train_h1n1)
model.rPart.seasonal = rpart(seasonal_vaccine~., data = train_seasonal)

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = df_test, type = "prob")
model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = df_test, type = "prob")

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = model.rPart.h1n1.pred[,2]
submission$seasonal_vaccine = model.rPart.seasonal.pred[,2]

submission

write_csv(submission, "datos_prepro_ruidoh1n1_multi.csv")

#0.7655

#Quitando ruido solo en seasonal_vaccine para el multiclasificador (usamos df con ruido para el modelo de h1n1_vaccine
#y usamos el df_seasonal calculado sin ruido para el modelo de seasonal_vaccine)

#Creacion de conjuntos de train y test

train_h1n1 = sample(1:nrow(df),2*nrow(df)/3)
test_h1n1 = df[-train_h1n1,]

train_seasonal = sample(1:nrow(df_seasonal),2*nrow(df_seasonal)/3)
test_seasonal = df_seasonal[-train_seasonal,]

#Modelo

model.rPart.h1n1 = rpart(h1n1_vaccine~., data=df[,-37], subset=train_h1n1)
summary(model.rPart.h1n1)

model.rPart.seasonal = rpart(seasonal_vaccine~., data=df_seasonal[,-36], subset=train_seasonal)
summary(model.rPart.seasonal)

#Prediccion

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = test_h1n1, type = "prob")
model.rPart.h1n1.pred

auc.h1n1 = auc(roc(model.rPart.h1n1.pred[, 2], test_h1n1$h1n1_vaccine))
auc.h1n1

model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = test_seasonal, type = "prob")
model.rPart.seasonal.pred

auc.seasonal = auc(roc(model.rPart.seasonal.pred[, 2], test_seasonal$seasonal_vaccine))
auc.seasonal

#Con conjuntos dados de train y test 

train_h1n1 = df[,-37]
train_seasonal = df_seasonal[,-36]

model.rPart.h1n1 = rpart(h1n1_vaccine~., data = train_h1n1)
model.rPart.seasonal = rpart(seasonal_vaccine~., data = train_seasonal)

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = df_test, type = "prob")
model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = df_test, type = "prob")

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = model.rPart.h1n1.pred[,2]
submission$seasonal_vaccine = model.rPart.seasonal.pred[,2]

submission

write_csv(submission, "datos_prepro_ruidoseasonal_multi.csv")

#0.7328

#Quitar ruido en la etiqueta del multiclasificador con solo una etiqueta

set.seed(1)
resultado = NoiseFiltersR::IPF(target ~ ., data=df)
df = resultado$cleanData

#Creacion de conjuntos de train y test

train = sample(1:nrow(df),2*nrow(df)/3)
test = df[-train,]

#Modelo con una etiqueta

model.rPart = rpart(target~., data=df, subset=train)
summary(model.rPart)

#Prediccion

model.rPart.pred = predict(model.rPart, newdata = test, type = "prob")
model.rPart.pred


#Con conjuntos dados de train y test 

model.rPart = rpart(target~., data=df)

model.rPart.pred = predict(model.rPart, newdata = df_test, type = "prob")

prediccion_test = as.data.frame(model.rPart.pred)
prediccion_test = prediccion_test %>% mutate(id=as.numeric(rownames(model.rPart.pred))) %>% relocate(id)
prediccion_split_test = split_target(prediccion_test)
prediccion_split_test = as.data.frame(prediccion_split_test)
prediccion_split_test

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = prediccion_split_test$h1n1_vaccine
submission$seasonal_vaccine = prediccion_split_test$seasonal_vaccine

submission

write_csv(submission, "datos_prepro_ruido.csv")

#0.7423

#Al quitar ruido en ambos clasificadores vemos que obtenemos el mejor resultado con 
#el multiclasificador quitando el ruido solo en h1n1_vaccine

#Ahora realizamos selección de características en ambos clasificadores

#Comenzamos con el multiclasificador (cargamos de nuevo los datos y usamos los datos quitando ruido solo en h1n1_vaccine que fue el mejor resultado)

#Datos 

df = as.data.frame(read_csv('train_imputed.csv'))

df_test = as.data.frame(read_csv('test_imputed.csv'))

#Convertir a factor

df = df %>% mutate_if(is.character, as.factor)
df_test = df_test %>% mutate_if(is.character, as.factor)

df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)

#Quitar ruido

set.seed(1)
resultado_h1n1 = NoiseFiltersR::IPF(h1n1_vaccine ~ ., data=df)
df_h1n1 = resultado_h1n1$cleanData
str(df_h1n1)

#Seleccionamos variables 

evaluator_rPart = function(attributes, data, dependent = "h1n1_vaccine") {
  model = rpart(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x = data %>% select(-h1n1_vaccine, -seasonal_vaccine) %>% as.data.frame(.)
  probs = predict(model, newdata = x, type="prob")
  
  auc(roc(probs[, 2], data$h1n1_vaccine))
}

result.h1n1 = FSelectorRcpp::feature_search(
  attributes = names(df_h1n1)[1:35],
  fun = evaluator_rPart,
  data = df_h1n1,
  parallel = T,
)

result.h1n1$best

#Repetimmos para la otra etiqueta

evaluator_rPart = function(attributes, data, dependent = "seasonal_vaccine") {
  model = rpart(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x = data %>% select(-h1n1_vaccine, -seasonal_vaccine) %>% as.data.frame(.)
  probs = predict(model, newdata = x, type="prob")
  
  auc(roc(probs[, 2], data$seasonal_vaccine))
}

result.seasonal = FSelectorRcpp::feature_search(
  attributes = names(df)[1:35],
  fun = evaluator_rPart,
  data = df,
  parallel = T,
)

result.seasonal$best

#Seleccionamos las que nos han salido como mejores

df_h1n1 = df_h1n1 %>% select("doctor_recc_h1n1", "health_insurance", "opinion_h1n1_vacc_effective", "opinion_h1n1_risk", "opinion_seas_vacc_effective", "opinion_seas_sick_from_vacc", "h1n1_vaccine")

df_seasonal = df %>% select("doctor_recc_seasonal", "opinion_h1n1_vacc_effective", "opinion_h1n1_sick_from_vacc", "opinion_seas_vacc_effective", "opinion_seas_risk", "age_group", "seasonal_vaccine")

df_test_h1n1 = df_h1n1 %>% select("doctor_recc_h1n1", "health_insurance", "opinion_h1n1_vacc_effective", "opinion_h1n1_risk", "opinion_seas_vacc_effective", "opinion_seas_sick_from_vacc", "h1n1_vaccine")

df_test_seasonal = df %>% select("doctor_recc_seasonal", "opinion_h1n1_vacc_effective", "opinion_h1n1_sick_from_vacc", "opinion_seas_vacc_effective", "opinion_seas_risk", "age_group", "seasonal_vaccine")


#Creacion de conjuntos de train y test

train_h1n1 = sample(1:nrow(df_h1n1),2*nrow(df_h1n1)/3)
test_h1n1 = df_test_h1n1[-train_h1n1,]

train_seasonal = sample(1:nrow(df_seasonal),2*nrow(df_seasonal)/3)
test_seasonal = df_test_seasonal[-train_seasonal,]

#Modelo

model.rPart.h1n1 = rpart(h1n1_vaccine~., data=df_h1n1, subset=train_h1n1)
summary(model.rPart.h1n1)

model.rPart.seasonal = rpart(seasonal_vaccine~., data=df_seasonal, subset=train_seasonal)
summary(model.rPart.seasonal)

#Prediccion

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = test_h1n1, type = "prob")
model.rPart.h1n1.pred

auc.h1n1 = auc(roc(model.rPart.h1n1.pred[, 2], test_h1n1$h1n1_vaccine))
auc.h1n1

model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = test_seasonal, type = "prob")
model.rPart.seasonal.pred

auc.seasonal = auc(roc(model.rPart.seasonal.pred[, 2], test_seasonal$seasonal_vaccine))
auc.seasonal

#Con conjuntos dados de train y test 

df_test_h1n1 = df_test %>% select("doctor_recc_h1n1", "health_insurance", "opinion_h1n1_vacc_effective", "opinion_h1n1_risk", "opinion_seas_vacc_effective", "opinion_seas_sick_from_vacc")

df_test_seasonal = df_test %>% select("doctor_recc_seasonal", "opinion_h1n1_vacc_effective", "opinion_h1n1_sick_from_vacc", "opinion_seas_vacc_effective", "opinion_seas_risk", "age_group")


model.rPart.h1n1 = rpart(h1n1_vaccine~., data = df_h1n1)
model.rPart.seasonal = rpart(seasonal_vaccine~., data = df_seasonal)

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = df_test_h1n1, type = "prob")
model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = df_test_seasonal, type = "prob")

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = model.rPart.h1n1.pred[,2]
submission$seasonal_vaccine = model.rPart.seasonal.pred[,2]

submission

write_csv(submission, "datos_prepro_ruidoh1n1_selec_multi.csv")

#0.7651

#Seleccion de características con backward para el multiclasificador

#Datos 

df = as.data.frame(read_csv('train_imputed.csv'))

df_test = as.data.frame(read_csv('test_imputed.csv'))

#Convertir a factor

df = df %>% mutate_if(is.character, as.factor)
df_test = df_test %>% mutate_if(is.character, as.factor)

df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)


#Quitar ruido

set.seed(1)
resultado_h1n1 = NoiseFiltersR::IPF(h1n1_vaccine ~ ., data=df)
df_h1n1 = resultado_h1n1$cleanData


#Seleccionamos variables 

evaluator_rPart = function(attributes, data, dependent = "h1n1_vaccine") {
  model = rpart(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x = data %>% select(-h1n1_vaccine, -seasonal_vaccine) %>% as.data.frame(.)
  probs = predict(model, newdata = x, type="prob")
  
  auc(roc(probs[, 2], data$h1n1_vaccine))
}

result.h1n1 = FSelectorRcpp::feature_search(
  attributes = names(df_h1n1)[1:35],
  fun = evaluator_rPart,
  data = df_h1n1,
  parallel = T,
  type = "backward"
)

result.h1n1$best

evaluator_rPart = function(attributes, data, dependent = "seasonal_vaccine") {
  model = rpart(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x = data %>% select(-h1n1_vaccine, -seasonal_vaccine) %>% as.data.frame(.)
  probs = predict(model, newdata = x, type="prob")
  
  auc(roc(probs[, 2], data$seasonal_vaccine))
}

result.seasonal = FSelectorRcpp::feature_search(
  attributes = names(df)[1:35],
  fun = evaluator_rPart,
  data = df,
  parallel = T,
  type = "backward"
)

result.seasonal$best

#Seleccionamos las que nos han salido como mejores

df_h1n1 = df_h1n1 %>% select(-opinion_seas_risk, -seasonal_vaccine)

df_seasonal = df %>% select(-h1n1_concern, -opinion_h1n1_vacc_effective, -opinion_h1n1_risk, -opinion_seas_risk, -employment_industry, -h1n1_vaccine)


#Creo conjuntos de train y test

train_h1n1 = sample(1:nrow(df_h1n1),2*nrow(df_h1n1)/3)
test_h1n1 = df_h1n1[-train_h1n1,]

train_seasonal = sample(1:nrow(df_seasonal),2*nrow(df_seasonal)/3)
test_seasonal = df_seasonal[-train_seasonal,]

#Modelo

model.rPart.h1n1 = rpart(h1n1_vaccine~., data=df_h1n1, subset=train_h1n1)
summary(model.rPart.h1n1)

model.rPart.seasonal = rpart(seasonal_vaccine~., data=df_seasonal, subset=train_seasonal)
summary(model.rPart.seasonal)

#Prediccion

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = test_h1n1, type = "prob")
model.rPart.h1n1.pred

auc.h1n1 = auc(roc(model.rPart.h1n1.pred[, 2], test_h1n1$h1n1_vaccine))
auc.h1n1

model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = test_seasonal, type = "prob")
model.rPart.seasonal.pred

auc.seasonal = auc(roc(model.rPart.seasonal.pred[, 2], test_seasonal$seasonal_vaccine))
auc.seasonal

#Pruebo con los conjuntos dados de train y test 

df_test_h1n1 = df_test %>% select(-opinion_seas_risk)

df_test_seasonal = df_test %>% select(-h1n1_concern, -opinion_h1n1_vacc_effective, -opinion_h1n1_risk, -opinion_seas_risk, -employment_industry)


model.rPart.h1n1 = rpart(h1n1_vaccine~., data = df_h1n1)
model.rPart.seasonal = rpart(seasonal_vaccine~., data = df_seasonal)

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = df_test_h1n1, type = "prob")
model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = df_test_seasonal, type = "prob")

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = model.rPart.h1n1.pred[,2]
submission$seasonal_vaccine = model.rPart.seasonal.pred[,2]

submission

write_csv(submission, "datos_prepro_ruidoh1n1_selec_backward_multi.csv")

#0.7661

#Seleccionamos caracteristicas para el clasificador con una etiqueta 

#Datos 

df = as.data.frame(read_csv('train_imputed.csv'))

df_test = as.data.frame(read_csv('test_imputed.csv'))

#Convertir a factor

df = df %>% mutate_if(is.character, as.factor)
df_test = df_test %>% mutate_if(is.character, as.factor)

df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)

#Unimos etiquetas

df$target = paste(
  df[[36]],
  df[[37]],
  sep=""
) %>% as.factor()

df$h1n1_vaccine = NULL
df$seasonal_vaccine = NULL


#Quitar ruido

set.seed(1)
resultado = NoiseFiltersR::IPF(target ~ ., data=df)
df = resultado$cleanData


#Seleccionamos variables 

evaluator_rPart = function(attributes, data, dependent = "target") {
  model = rpart(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x = data %>% select(-target) %>% as.data.frame(.)
  probs = predict(model, newdata = x, type="prob")
  
  auc(roc(probs[, 2], data$target))
}

result = FSelectorRcpp::feature_search(
  attributes = names(df)[1:35],
  fun = evaluator_rPart,
  data = df,
  parallel = T,
)

result$best

#Solo obtenemos como mejores "opinion_seas_vacc_effective" y "age_group". 
#Vamos a probar haciendo backward

#Seleccionamos variables 

evaluator_rPart = function(attributes, data, dependent = "target") {
  model = rpart(
    FSelectorRcpp::to_formula(attributes, dependent),
    data = data
  )
  
  x = data %>% select(-target) %>% as.data.frame(.)
  probs = predict(model, newdata = x, type="prob")
  
  auc(roc(probs[, 2], data$target))
}

result = FSelectorRcpp::feature_search(
  attributes = names(df)[1:35],
  fun = evaluator_rPart,
  data = df,
  parallel = T,
  type = "backward"
)

result$best


#Seleccionamos las que nos han salido como mejores

df = df %>% select(-opinion_h1n1_risk, -opinion_seas_risk)

df_test = df_test %>% select(-opinion_h1n1_risk, -opinion_seas_risk)

#Creo conjuntos de train y test

train = sample(1:nrow(df),2*nrow(df)/3)
test = df[-train,]

#Modelo

model.rPart = rpart(target~., data=df, subset=train)
summary(model.rPart)

#Prediccion

model.rPart.pred = predict(model.rPart, newdata = test, type = "prob")
model.rPart.pred


#Pruebo con los conjuntos dados de train y test 

model.rPart = rpart(target~., data=df)

model.rPart.pred = predict(model.rPart, newdata = df_test, type = "prob")

prediccion_test = as.data.frame(model.rPart.pred)
prediccion_test = prediccion_test %>% mutate(id=as.numeric(rownames(model.rPart.pred))) %>% relocate(id)
prediccion_split_test = split_target(prediccion_test)
prediccion_split_test = as.data.frame(prediccion_split_test)
prediccion_split_test

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = prediccion_split_test$h1n1_vaccine
submission$seasonal_vaccine = prediccion_split_test$seasonal_vaccine

submission

write_csv(submission, "datos_prepro_ruido_selec_backward.csv")

#0.7405

#Vemos que el mejor clasificador es el multiclasificador al seleccionar características con backward. 
#A partir de ahora vamos a continuar con este clasificador y vamos a seleccionar los mejores parámetros

#Datos 

df = as.data.frame(read_csv('train_imputed.csv'))

df_test = as.data.frame(read_csv('test_imputed.csv'))

#Convertir a factor

df = df %>% mutate_if(is.character, as.factor)
df_test = df_test %>% mutate_if(is.character, as.factor)

df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)


#Quitar ruido

set.seed(1)
resultado_h1n1 = NoiseFiltersR::IPF(h1n1_vaccine ~ ., data=df)
df_h1n1 = resultado_h1n1$cleanData

#Seleccionamos las variables que nos han salido como mejores

df_h1n1 = df_h1n1 %>% select(-opinion_seas_risk, -seasonal_vaccine)

df_seasonal = df %>% select(-h1n1_concern, -opinion_h1n1_vacc_effective, -opinion_h1n1_risk, -opinion_seas_risk, -employment_industry, -h1n1_vaccine)


#Creo conjuntos de train y test

train_h1n1 = sample(1:nrow(df_h1n1),2*nrow(df_h1n1)/3)
test_h1n1 = df_h1n1[-train_h1n1,]

train_seasonal = sample(1:nrow(df_seasonal),2*nrow(df_seasonal)/3)
test_seasonal = df_seasonal[-train_seasonal,]

#Modelo seleccionando parametros (por defecto minsplit=20, maxdepth=30, cp=0.01)

model.rPart.h1n1 = rpart(h1n1_vaccine~., data=df_h1n1, subset=train_h1n1, control= rpart.control(cp = 0.0001))
summary(model.rPart.h1n1)

model.rPart.seasonal = rpart(seasonal_vaccine~., data=df_seasonal, subset=train_seasonal, control= rpart.control(cp = 0.0001))
summary(model.rPart.seasonal)

#Prediccion

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = test_h1n1, type = "prob")
model.rPart.h1n1.pred

auc.h1n1 = auc(roc(model.rPart.h1n1.pred[, 2], test_h1n1$h1n1_vaccine))
auc.h1n1

model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = test_seasonal, type = "prob")
model.rPart.seasonal.pred

auc.seasonal = auc(roc(model.rPart.seasonal.pred[, 2], test_seasonal$seasonal_vaccine))
auc.seasonal

#Con conjuntos dados de train y test 

df_test_h1n1 = df_test %>% select(-opinion_seas_risk)

df_test_seasonal = df_test %>% select(-h1n1_concern, -opinion_h1n1_vacc_effective, -opinion_h1n1_risk, -opinion_seas_risk, -employment_industry)


model.rPart.h1n1 = rpart(h1n1_vaccine~., data = df_h1n1, control= rpart.control(cp = 0.0001))
model.rPart.seasonal = rpart(seasonal_vaccine~., data = df_seasonal, control= rpart.control(cp = 0.0001))

model.rPart.h1n1.pred = predict(model.rPart.h1n1, newdata = df_test_h1n1, type = "prob")
model.rPart.seasonal.pred = predict(model.rPart.seasonal, newdata = df_test_seasonal, type = "prob")

#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = model.rPart.h1n1.pred[,2]
submission$seasonal_vaccine = model.rPart.seasonal.pred[,2]

submission

write_csv(submission, "datos_prepro_ruidoh1n1_selec_backward_cp0.0001_multi.csv")

#0.7889

#Se ha probado a variar minsplit y maxdepth pero no varía nada.
#Se ha probado tambien cp=0.001 y cp=0.00001, obteniendose respectivamente 0.7780 y 0.7784.
#Nos quedamos con cp=0.0001.

#Por último se va a probar bagging y random forest sobre el mejor clasificador obtenido hasta ahora


#Datos 

df = as.data.frame(read_csv('train_imputed.csv'))

df_test = as.data.frame(read_csv('test_imputed.csv'))

#Convertir a factor

df = df %>% mutate_if(is.character, as.factor)
df_test = df_test %>% mutate_if(is.character, as.factor)

df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)


#Quitar ruido

set.seed(1)
resultado_h1n1 = NoiseFiltersR::IPF(h1n1_vaccine ~ ., data=df)
df_h1n1 = resultado_h1n1$cleanData

#Seleccionamos las variables que nos han salido como mejores

df_h1n1 = df_h1n1 %>% select(-opinion_seas_risk, -seasonal_vaccine)

df_seasonal = df %>% select(-h1n1_concern, -opinion_h1n1_vacc_effective, -opinion_h1n1_risk, -opinion_seas_risk, -employment_industry, -h1n1_vaccine)

df_test_h1n1 = df_test %>% select(-opinion_seas_risk)

df_test_seasonal = df_test %>% select(-h1n1_concern, -opinion_h1n1_vacc_effective, -opinion_h1n1_risk, -opinion_seas_risk, -employment_industry)


#Aplicamos bagging

set.seed(42)

train_bagging = function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples = sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      classifier(h1n1_vaccine~., dato[samples,], control=rpart.control(cp = 0.0001) )
    }
  )
}

predict_bagging = function(models, test) {
  apply(
    array(
      unlist(
        lapply(
          1:length(models),
          function(i) {
            predict(models[[i]], newdata = test, type="prob")
          }
        )
      ),
      dim = c(dim(test)[1], 2, length(models))
    ),
    c(1, 2),
    mean
  )
}





models_h1n1 = train_bagging(
  rpart,
  50,
  df_h1n1
)

preds_h1n1 = predict_bagging(
  models_h1n1,
  df_test_h1n1
)



#Ahora calculamos lo mismo para la seasonal_vaccine

set.seed(42)

train_bagging = function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples = sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      classifier(seasonal_vaccine~., dato[samples,], control=rpart.control(cp = 0.0001) )
    }
  )
}

predict_bagging = function(models, test) {
  apply(
    array(
      unlist(
        lapply(
          1:length(models),
          function(i) {
            predict(models[[i]], newdata = test, type="prob")
          }
        )
      ),
      dim = c(dim(test)[1], 2, length(models))
    ),
    c(1, 2),
    mean
  )
}



models_seasonal = train_bagging(
  rpart,
  50,
  df_seasonal
)

preds_seasonal = predict_bagging(
  models_seasonal,
  df_test_seasonal
)


#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = preds_h1n1[,2]
submission$seasonal_vaccine = preds_seasonal[,2]

submission

write_csv(submission, "datos_prepro_ruidoh1n1_selec_backward_cp0.0001_bagging50.csv")

#0.8240

#Se ha probado cambiando el numero de clasificadores a 5 y 10 tambien, obteniendose
#respectivamente 0.8067 y 0.8153, por lo que nos quedamos con 50.

#Ahora probamos con random forest modificando la funcion de bagging


set.seed(42)

train_bagging = function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples = sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      samplesAttributes = sample(1:(dim(dato)[2]-1), replace = F, size = as.integer((dim(dato)[2] - 1)*0.7))
      classifier(h1n1_vaccine~., dato[samples, c(samplesAttributes, dim(dato)[2])], control=rpart.control(cp = 0.0001))
    }
  )
}

predict_bagging = function(models, test) {
  apply(
    array(
      unlist(
        lapply(
          1:length(models),
          function(i) {
            predict(models[[i]], newdata = test, type="prob")
          }
        )
      ),
      dim = c(dim(test)[1], 2, length(models))
    ),
    c(1, 2),
    mean
  )
}





models_h1n1 = train_bagging(
  rpart,
  130,
  df_h1n1
)

preds_h1n1 = predict_bagging(
  models_h1n1,
  df_test_h1n1
)



#Ahora calculamos lo mismo para la seasonal_vaccine

set.seed(42)

train_bagging = function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples = sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      samplesAttributes = sample(1:(dim(dato)[2]-1), replace = F, size = as.integer((dim(dato)[2] - 1)*0.7))
      classifier(seasonal_vaccine~., dato[samples, c(samplesAttributes, dim(dato)[2])], control=rpart.control(cp = 0.0001))
    }
  )
}

predict_bagging = function(models, test) {
  apply(
    array(
      unlist(
        lapply(
          1:length(models),
          function(i) {
            predict(models[[i]], newdata = test, type="prob")
          }
        )
      ),
      dim = c(dim(test)[1], 2, length(models))
    ),
    c(1, 2),
    mean
  )
}



models_seasonal = train_bagging(
  rpart,
  130,
  df_seasonal
)

preds_seasonal = predict_bagging(
  models_seasonal,
  df_test_seasonal
)


#Submission

submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = preds_h1n1[,2]
submission$seasonal_vaccine = preds_seasonal[,2]

submission

write_csv(submission, "datos_prepro_ruidoh1n1_selec_backward_cp0.0001_rf130.csv")

#0.8310

#Se ha probado tambien con 80 y 100 clasificadores obteniendose respectivamente
#0.8302 y 0.8305. Por tanto nuestro mejor modelo final es este ultimo, utilizando
#random forest con 130 clasificadores con el multiclasificador y el preprocesamiento realizado.

