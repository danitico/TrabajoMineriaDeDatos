#Librerías


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

###############################################################################

#     CLASIFICADOR NO-MULTI

#########################################

##################
#DATOS ORIGINALES
#################

feature=read_csv("training_set_features.csv")
labels=read_csv("training_set_labels.csv")
testp = read_csv("test_set_features.csv")
df=merge(feature, labels, by = "respondent_id")
str(df)

df$target = paste( df[[37]], df[[38]], sep="") %>% as.factor()
df$h1n1_vaccine = NULL
df$seasonal_vaccine = NULL
str(df)

#Pasamos las variables chr a factor
df=df %>% mutate_if(is.character, as.factor)
str(df)

testp=testp %>% mutate_if(is.character, as.factor)



modelC4.5_target = J48(target~., data=df)

modelC4.5_target.pred = predict(modelC4.5_target, testp, type="probability")
modelC4.5_target.pred

modelC4.5_target.pred=as.data.frame(modelC4.5_target.pred)
modelC4.5_target.pred=modelC4.5_target.pred %>% mutate(id=as.numeric(rownames(modelC4.5_target.pred))) %>% relocate(id)
modelC4.5_target.pred


#Separación de probabilidades
prediccion_target_split=split_target(modelC4.5_target.pred)
prediccion_target_split

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = prediccion_target_split[,2]
submission$seasonal_vaccine = prediccion_target_split[,3]
submission

write_csv(submission, "datosoriginales.csv") #SCORE 0.7415

########################
#PREPROCESAMIENTO COMÚN
######################
df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")


df$target = paste( df[[36]], df[[37]], sep="") %>% as.factor()
df$h1n1_vaccine = NULL
df$seasonal_vaccine = NULL
str(df)
df=df %>% mutate_if(is.character, as.factor)
str(df)

testp=testp %>% mutate_if(is.character, as.factor)


modelC4.5_target = J48(target~., data=df)
modelC4.5_target.pred = predict(modelC4.5_target, testp, type="probability")
modelC4.5_target.pred

modelC4.5_target.pred=as.data.frame(modelC4.5_target.pred)
modelC4.5_target.pred=modelC4.5_target.pred %>% mutate(id=as.numeric(rownames(modelC4.5_target.pred))) %>% relocate(id)
modelC4.5_target.pred


#Separación de probabilidades
prediccion_target_split=split_target(modelC4.5_target.pred)
prediccion_target_split


#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = prediccion_target_split[,2]
submission$seasonal_vaccine = prediccion_target_split[,3]
submission

write_csv(submission, "preprocomun.csv") #SCORE 0.07488
#Vemos que hay una mejora por lo que nos quedamos con los datos preprocesados
#para el resto de estudios

#############################
#ELIMINAMOS RUIDO
##########################
df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")

df$target = paste( df[[36]], df[[37]], sep="") %>% as.factor()
df$h1n1_vaccine = NULL
df$seasonal_vaccine = NULL
str(df)
df=df %>% mutate_if(is.character, as.factor)
str(df)
#Quitamos ruido de los datos
set.seed(9)
res=NoiseFiltersR::IPF(target~., data=df, s=2)
df=res$cleanData
str(df)
testp=testp %>% mutate_if(is.character, as.factor)


modelC4.5_target = J48(target~., data=df)
modelC4.5_target.pred = predict(modelC4.5_target, testp, type="probability")
modelC4.5_target.pred

modelC4.5_target.pred=as.data.frame(modelC4.5_target.pred)
modelC4.5_target.pred=modelC4.5_target.pred %>% mutate(id=as.numeric(rownames(modelC4.5_target.pred))) %>% relocate(id)
modelC4.5_target.pred


#Separación de probabilidades
prediccion_target_split=split_target(modelC4.5_target.pred)
prediccion_target_split


#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = prediccion_target_split[,2]
submission$seasonal_vaccine = prediccion_target_split[,3]
submission

write_csv(submission, "ruido.csv") #SCORE 0.7339 
#Observamos que no hay ninguna mejora por lo que seguiremos trabajando con los datos
#con el preprocesado común

##############################
#SELECCIÓN DE CARACTERÍSTICAS
################################
df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")

df$target = paste( df[[36]], df[[37]], sep="") %>% as.factor()
df$h1n1_vaccine = NULL
df$seasonal_vaccine = NULL
str(df)
df=df %>% mutate_if(is.character, as.factor)
str(df)
testp=testp %>% mutate_if(is.character, as.factor)

#Selección característica

x = FSelectorRcpp::information_gain(target ~ ., df)
h1n1_mejores03=FSelectorRcpp::cut_attrs(attrs = x, k = 0.3)

df= df %>% select("opinion_seas_risk", "opinion_seas_vacc_effective", "doctor_recc_seasonal",
                  "doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective",
                  "age_group", "employment_industry", "employment_occupation", "health_insurance", "target")

testp = testp %>% select("opinion_seas_risk", "opinion_seas_vacc_effective", "doctor_recc_seasonal",
                         "doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective",
                         "age_group", "employment_industry", "employment_occupation", "health_insurance")


modelC4.5_target = J48(target~., data=df)
modelC4.5_target.pred = predict(modelC4.5_target, testp, type="probability")
modelC4.5_target.pred

modelC4.5_target.pred=as.data.frame(modelC4.5_target.pred)
modelC4.5_target.pred=modelC4.5_target.pred %>% mutate(id=as.numeric(rownames(modelC4.5_target.pred))) %>% relocate(id)
modelC4.5_target.pred


#Separación de probabilidades
prediccion_target_split=split_target(modelC4.5_target.pred)
prediccion_target_split

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = prediccion_target_split[,2]
submission$seasonal_vaccine = prediccion_target_split[,3]
submission

write_csv(submission, "caract.csv") #SCORE 0.7873
#Tambien se probó para k=0.25 en cut_attrs obteniendo score 0.7833
#y para k=0.35 obteniendo 0.7869

#Por tanto, se va a seguir trabajando con los datos preprocesado y el 30% de las características obtenidas

######################
#SELECCIÓN PARÁMETROS
#######################
 
#En este caso solo se muestra los parámetros del mejor resultado obtenido para no repetir código. 
#El procedimineto que se ha seguido es variar primero la M y comparar con cv los porcentajes de acierto
#y se observó que se obtenía mejor porcentaje para M=8.

#Para ese valor de M, se varió el valor del parámetro C y se obtuvo mejor porcentaje para C=0.5
#Aun así se ha realizado dos subidas:  C=0.5 y C=0.45.
#No se ha podido probar para velores de C superior porque se obtenian warning mensaje por ser C elevado
df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")

df$target = paste( df[[36]], df[[37]], sep="") %>% as.factor()
df$h1n1_vaccine = NULL
df$seasonal_vaccine = NULL
str(df)
df=df %>% mutate_if(is.character, as.factor)
str(df)

testp=testp %>% mutate_if(is.character, as.factor)

df= df %>% select("opinion_seas_risk", "opinion_seas_vacc_effective", "doctor_recc_seasonal",
                  "doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective",
                  "age_group", "employment_industry", "employment_occupation", "health_insurance", "target")

testp = testp %>% select("opinion_seas_risk", "opinion_seas_vacc_effective", "doctor_recc_seasonal",
                         "doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective",
                         "age_group", "employment_industry", "employment_occupation", "health_insurance")
#Para ver que parametros tiene J48()
WOW("J48")
#Observamos y elegimos que dos parametros son mejores para modificar:

#-M: mínimo numero de instancias por hoja. Por defecto M=2
#-C: establece el umbral de confianza para la poda. Por defecto C=0.25
#-r: Utilice la poda de errores reducida.

#ctrl <- trainControl(method="LGOCV", p=0.8, seeds=NA)
#grid <- expand.grid(MinWeights=c(1, 2, 5, 10, 15), NumOpt=5, NumFolds=1:5)
#model.Ripper= train(target~. , data=datos , method='JRip', na.action = na.pass, trControl=ctrl,tuneGrid=grid)
#saveRDS(model.Ripper, "modelRipper.rds")

modelC4.5_target=J48(target~., data=df, control=Weka_control(M=8, C=0.5))
cv_resul=evaluate_Weka_classifier(modelC4.5_target, numFolds = 10)
cv_resul


modelC4.5_target = J48(target~., data=df, control=Weka_control(M=8, C=0.5))
modelC4.5_target.pred = predict(modelC4.5_target, testp, type="probability")
modelC4.5_target.pred

modelC4.5_target.pred=as.data.frame(modelC4.5_target.pred)
modelC4.5_target.pred=modelC4.5_target.pred %>% mutate(id=as.numeric(rownames(modelC4.5_target.pred))) %>% relocate(id)
modelC4.5_target.pred


prediccion_target_split=split_target(modelC4.5_target.pred)
prediccion_target_split


#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = prediccion_target_split[,2]
submission$seasonal_vaccine = prediccion_target_split[,3]

submission

write_csv(submission, "parametros.csv") #SCORE 0.8104
#Para C=0.45 y M=8 se obtuvo 0.7996

#Observamos que hay una mejora, por lo que se considerarán esos parámetros para los siguientes estudios

################
#BAGGING
#############

df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")

df$target = paste( df[[36]], df[[37]], sep="") %>% as.factor()
df$h1n1_vaccine = NULL
df$seasonal_vaccine = NULL
str(df)
df=df %>% mutate_if(is.character, as.factor)
str(df)

testp=testp %>% mutate_if(is.character, as.factor)

#Selección característica

df= df %>% select("opinion_seas_risk", "opinion_seas_vacc_effective", "doctor_recc_seasonal",
                  "doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective",
                  "age_group", "employment_industry", "employment_occupation", "health_insurance", "target")

testp = testp %>% select("opinion_seas_risk", "opinion_seas_vacc_effective", "doctor_recc_seasonal",
                         "doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective",
                         "age_group", "employment_industry", "employment_occupation", "health_insurance")

#Aplicamos bagging

set.seed(42)

train_bagging <- function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      classifier(target~., dato[samples,], control=Weka_control(M=8, C=0.5))
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
      dim = c(dim(test)[1], 4, length(models))
    ),
    c(1, 2),
    mean
  )
}

models_target <- train_bagging(
  J48,
  60,
  df
)

preds_target <- predict_bagging(
  models_target,
  testp
)

modelC4.5_target.pred=as.data.frame(preds_target)
modelC4.5_target.pred=modelC4.5_target.pred %>% mutate(id=as.numeric(rownames(modelC4.5_target.pred))) %>% relocate(id)
modelC4.5_target.pred

#Separación de probabilidades
prediccion_target_split=split_target(modelC4.5_target.pred)
prediccion_target_split

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = prediccion_target_split[,2]
submission$seasonal_vaccine = prediccion_target_split[,3]
submission

write_csv(submission, "Bagging.csv") #SCORE 0.8246
#Tambien se ha probado para number_classifiers=50 obteniendo 0.8240



###########################################################
#------------------------------------------------------
######################################################



####################################################

#   CLASIFICADOR MULTI

#################################################


###################
#DATOS ORGININALES
####################

#Cargamos los datos 
feature=read_csv("training_set_features.csv")
labels=read_csv("training_set_labels.csv")
testp=read_csv("training_set_features.csv")

# Generamos el dataframe
df=merge(feature, labels, by = "respondent_id")
str(df)

#Vamos a realizar el estudio sin realizar preprocesamiento ya que este clasiifcador acepta missing values. 
#Así sabremos con que porcentaje partimos. 

#Pasamos las variables chr a factor

df=df %>% mutate_if(is.character, as.factor)

#Pasamos a factor las variabels de salida 
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)


#Primero vamos a estudiar para el caso de h1n1_vaccine.

dfh1n1=df[-38]

#Existen dos formas de aplicar este clasificador y ver su porcentjae de acierto. 
# Uno es usando un comando que realiza validación cruzada

modelC4.5_h1n1=J48(h1n1_vaccine~., data=dfh1n1)
cv_resul=evaluate_Weka_classifier(modelC4.5_h1n1, numFolds = 10)
cv_resul


#Otra forma es creando yo un train y un test 
set.seed(9)
train = sample(1:nrow(dfh1n1), 2*nrow(dfh1n1)/3)
test = df[-train,]

modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=dfh1n1, subset=train)


modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, test)


#plot(modelC4.5)
modelC4.5_h1n1_vaccine.pred

table(modelC4.5_h1n1_vaccine.pred,test[,37])

#Se obtiene un 17 porciento de error y cerca de un 83% de acierto.


#Lo que nos piden es hallar la porbabilidad de obtener 0 o 1 en cada vacuna. 

modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=dfh1n1, subset=train)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, test, type="probability")
modelC4.5_h1n1_vaccine.pred

auc.h1n1_ = auc(roc(modelC4.5_h1n1_vaccine.pred[, 2], test$h1n1_vaccine))
auc.h1n1_1




#Ahora hacemos lo mismo pero para la salida seasonal_vaccine

dfseasonal=df[-37]
set.seed(9)
train = sample(1:nrow(dfseasonal), 2*nrow(dfh1n1)/3)
test = df[-train,]

modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=dfseasonal, subset=train)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, test, type="probability")
modelC4.5_seasonal_vaccine.pred


#Se obtiene un 23 porciento de error y cerca de un 77% de acierto.
table(modelC4.5_seasonal_vaccine.pred,test[,38])

auc.seasonal = auc(roc(modelC4.5_seasonal_vaccine.pred[, 2], test$seasonal_vaccine))
auc.seasonal 


###################################################################

#Ahora lo hacemos para el train y test de la competión

#################################################

#Partimos desde la carga de los archivos para asegurarnos que no hay ninguna modificación
feature=read_csv("training_set_features.csv")
labels=read_csv("training_set_labels.csv")
testp = read_csv("test_set_features.csv")


df=merge(feature, labels, by = "respondent_id")
str(df)
df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)
testp=testp %>% mutate_if(is.character, as.factor)

#Sacamos dos train, uno para cada variable de salida:

train_h1n1 =df[,-38]
train_seasonal = df[,-37]

#Clasificador para h1n1
modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=train_h1n1)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, testp, type="probability")
modelC4.5_h1n1_vaccine.pred


#Clasificador para seasonal_vaccine
modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=train_seasonal)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testp, type="prob")
modelC4.5_seasonal_vaccine.pred

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = modelC4.5_h1n1_vaccine.pred[,2]
submission$seasonal_vaccine = modelC4.5_seasonal_vaccine.pred[,2]

submission
#Lo guardamos en un .csv para poder realizar la subida
write_csv(submission, "multi_original.csv") #SCORE 0.7306

#########################################################################################
#PREPROCESAMIENTO COMÚN
#########################################################################################

#Cargamos datos
df = as.data.frame(read_csv('train_imputed.csv'))
testp = as.data.frame(read_csv('test_imputed.csv'))

df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)


#Primero vamos a estudiar para el caso de h1n1_vaccine.

dfh1n1=df[-38]


#Creando  un train y un test 
set.seed(9)
train = sample(1:nrow(dfh1n1), 2*nrow(dfh1n1)/3)
test = df[-train,]


#Lo que nos piden es hallar la porbabilidad de obtener 0 o 1 en cada vacuna. 

modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=dfh1n1, subset=train)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, test, type="probability")
modelC4.5_h1n1_vaccine.pred

#Calculamos el auc de obtener 1.
auc.h1n1_1 = auc(roc(modelC4.5_h1n1_vaccine.pred[, 2], test$h1n1_vaccine))
auc.h1n1_1

#Ahora hacemos lo mismo pero para la salida seasonal_vaccine

dfseasonal=df[-37]
set.seed(9)
train = sample(1:nrow(dfseasonal), 2*nrow(dfh1n1)/3)
test = df[-train,]

modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=dfseasonal, subset=train)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, test, type="probability")
modelC4.5_seasonal_vaccine.pred


auc.seasonal = auc(roc(modelC4.5_seasonal_vaccine.pred[, 2], test$seasonal_vaccine))
auc.seasonal 


###################################################################

#Ahora lo hacemos para el train y test de la competión

#################################################

#Partimos desde la carga de los archivos para asegurarnos que no hay ninguna modificación
df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")

#Pasamos las variables chr a factor

df=df %>% mutate_if(is.character, as.factor)
#Pasamos a factor las variabels de salida 
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)
testp=testp %>% mutate_if(is.character, as.factor)

#Sacamos dos train, uno para cada variable de salida:
train_h1n1 =df[,-38]
train_seasonal = df[,-37]

#Clasificador para h1n1
modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=train_h1n1)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, testp, type="probability")
modelC4.5_h1n1_vaccine.pred


#Clasificador para seasonal_vaccine
modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=train_seasonal)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testp, type="prob")
modelC4.5_seasonal_vaccine.pred

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = modelC4.5_h1n1_vaccine.pred[,2]
submission$seasonal_vaccine = modelC4.5_seasonal_vaccine.pred[,2]

submission
#Lo guardamos en un .csv para poder realizar la subida
write_csv(submission, "multi_prepro.csv") #SCORE 0.7739

#Ambos clasificadores mejoran con los datos preprocesados por lo que  
#mantenemos estos datos.

###########################################################################
#ELIMINAMOS RUIDO
##########################################################################

#Probamos primero eliminando el ruido en ambas variables
df = as.data.frame(read_csv('train_imputed.csv'))
testp = as.data.frame(read_csv('test_imputed.csv'))

df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)

#Quitamos ruido de los datos
set.seed(9)
res=NoiseFiltersR::IPF(h1n1_vaccine~., data=df, s=2)
dfh1n1=res$cleanData
str(dfh1n1)

set.seed(9)
resu=NoiseFiltersR::IPF(seasonal_vaccine~., data=df, s=2)
dfseasonal=resu$cleanData
str(dfseasonal)

#Primero vamos a estudiar para el caso de h1n1_vaccine.

dfh1n1=df[-38]

#Creando  un train y un test 
set.seed(9)
trainh1n1 = sample(1:nrow(dfh1n1), 2*nrow(dfh1n1)/3)
testh1n1 = df[-train,]


#Lo que nos piden es hallar la porbabilidad de obtener 0 o 1 en cada vacuna. 

modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=dfh1n1, subset=train)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, test, type="probability")
modelC4.5_h1n1_vaccine.pred

#Calculamos el auc de obtener 1.
auc.h1n1_1 = auc(roc(modelC4.5_h1n1_vaccine.pred[, 2], test$h1n1_vaccine))
auc.h1n1_1

#Ahora hacemos lo mismo pero para la salida seasonal_vaccine

dfseasonal=df[-37]
set.seed(9)
trainseasonal = sample(1:nrow(dfseasonal), 2*nrow(dfh1n1)/3)
testseasonal = df[-train,]

modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=dfseasonal, subset=trainseasonal)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testseasonal, type="probability")
modelC4.5_seasonal_vaccine.pred


auc.seasonal = auc(roc(modelC4.5_seasonal_vaccine.pred[, 2], test$seasonal_vaccine))
auc.seasonal 


#Ahora lo hacemos para el train y test de la competión

#Partimos desde la carga de los archivos para asegurarnos que no hay ninguna modificación
df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")

df=df %>% mutate_if(is.character, as.factor)

#Pasamos a factor las variabels de salida 
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)

testp=testp %>% mutate_if(is.character, as.factor)

#Quitamos ruido
set.seed(9)
res=NoiseFiltersR::IPF(h1n1_vaccine~., data=df, s=2)
dfh1n1=res$cleanData
str(dfh1n1)

set.seed(9)
resu=NoiseFiltersR::IPF(seasonal_vaccine~., data=df, s=2)
dfseasonal=resu$cleanData
str(dfseasonal)

#Sacamos dos train, uno para cada variable de salida:

train_h1n1 =dfh1n1[,-37]
train_seasonal = dfseasonal[,-36]


modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=train_h1n1)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, testp, type="probability")
modelC4.5_h1n1_vaccine.pred


#Ahora calculamos lo mismo para la seasonal_vaccine
modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=train_seasonal)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testp, type="prob")
modelC4.5_seasonal_vaccine.pred

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = modelC4.5_h1n1_vaccine.pred[,2]
submission$seasonal_vaccine = modelC4.5_seasonal_vaccine.pred[,2]
submission

write_csv(submission, "multi_ruido.csv") #SCORE 0.7607

#Vemos que empeora, por lo que realizamos otro intento pero ahora solo eliminando
#el ruido de la variable h1n1 (tiene mayor desbalance).


#Quitamos ruido de los datos
set.seed(9)
res=NoiseFiltersR::IPF(h1n1_vaccine~., data=df, s=2)
dfh1n1=res$cleanData
str(dfh1n1)


modelC4.5_h1n1=J48(h1n1_vaccine~., data=dfh1n1)
cv_resul=evaluate_Weka_classifier(modelC4.5_h1n1, numFolds = 10)
cv_resul


#Otra forma es creando yo un train y un test 
set.seed(9)
trainh1n1 = sample(1:nrow(dfh1n1),2*nrow(dfh1n1)/3)
testh1n1 = df[-trainh1n1,]

modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=dfh1n1, subset=trainh1n1)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, testh1n1, type="probability")
modelC4.5_h1n1_vaccine.pred

auc.h1n1 = auc(roc(modelC4.5_h1n1_vaccine.pred[, 2], testh1n1$h1n1_vaccine))
auc.h1n1 


#Ahora hacemos lo mismo pero para la salida seasonal_vaccine
set.seed(9)
trainseasonal = sample(1:nrow(df),2*nrow(df)/3)
testseasonal = df[-trainseasonal,]

modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=df, subset=trainseasonal)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testseasonal, type="probability")
modelC4.5_seasonal_vaccine.pred


auc.seasonal = auc(roc(modelC4.5_seasonal_vaccine.pred[, 2], testseasonal$seasonal_vaccine))
auc.seasonal 

#Ahora lo hacemos para el train y test de la competición

#Volvemos a cargar los datos para asegurarnos que no haya ningún cambio
df = (read_csv('train_imputed.csv'))
testp = (read_csv('test_imputed.csv'))
df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)

#Quitamos ruido de h1n1
set.seed(9)
res=NoiseFiltersR::IPF(h1n1_vaccine~., data=df, s=2)
dfh1n1=res$cleanData
str(dfh1n1)


#Sacamos dos train, uno para cada variable de salida:

train_h1n1 =dfh1n1[,-37]
train_seasonal = df[,-36]


modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=train_h1n1)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, testp, type="probability")
modelC4.5_h1n1_vaccine.pred


#Ahora calculamos lo mismo para la seasonal_vaccine
modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=train_seasonal)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testp, type="probability")
modelC4.5_seasonal_vaccine.pred

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = modelC4.5_h1n1_vaccine.pred[,2]
submission$seasonal_vaccine = modelC4.5_seasonal_vaccine.pred[,2]

submission

write_csv(submission, "multi_ruido2.csv") #SCORE 0.7637
#Observamos que no se obtiene ninguna mejora eliminando el ruido por lo que no consideraremos este cambio.
#Seguiremos variando el clasificador con el dataset del preprocesamiento común.

###################################################################
#SELECCIÓN DE CARACTERÍSTICAS
#################################################################
df = (read_csv('train_imputed.csv'))
testp = (read_csv('test_imputed.csv'))

df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)



#SELECCIÓN DE CARACTERÍSITCAS


x = FSelectorRcpp::information_gain(h1n1_vaccine ~ ., df[,-37])
h1n1_mejores07=FSelectorRcpp::cut_attrs(attrs = x, k = 0.6)


y = FSelectorRcpp::information_gain(seasonal_vaccine ~ ., df[,-36])
seasonal_mejores07=FSelectorRcpp::cut_attrs(attrs = y, k = 0.6)


#Generamos un datarame para cada uno
dfh1n1= df %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                      "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                      "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                      "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                      "income_poverty", "hhs_geo_region", "h1n1_vaccine")

dfseasonal= df %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                          "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                          "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                          "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                          "opinion_seas_sick_from_vacc", "seasonal_vaccine")    


#Creamos el train y el test
set.seed(9)
trainh1n1 = sample(1:nrow(dfh1n1),2*nrow(dfh1n1)/3)
testh1n1 = df[-trainh1n1,]

set.seed(9)
trainseasonal = sample(1:nrow(dfseasonal), 2*nrow(dfseasonal)/3)
testseasonal = df[-trainseasonal,]

  
modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=dfh1n1, subset=trainh1n1)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, testh1n1, type="probability")
modelC4.5_h1n1_vaccine.pred


auc.h1n1 = auc(roc(modelC4.5_h1n1_vaccine.pred[, 2], testh1n1$h1n1_vaccine))
auc.h1n1 


#Ahora hacemos lo mismo pero para la salida seasonal_vaccine

modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=dfseasonal, subset=trainseasonal)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testseasonal, type="probability")
modelC4.5_seasonal_vaccine.pred

auc.seasonal = auc(roc(modelC4.5_seasonal_vaccine.pred[, 2], testseasonal$seasonal_vaccine))
auc.seasonal 


#Ahora lo hacemos para el train y test de la competición


#Volvemos a cargar los datos
df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")

df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)
testp=testp %>% mutate_if(is.character, as.factor)

#Selección de caracterísitcas:

x = FSelectorRcpp::information_gain(h1n1_vaccine ~ ., df[,-37])
h1n1_mejores06=FSelectorRcpp::cut_attrs(attrs = x, k = 0.6)

y = FSelectorRcpp::information_gain(seasonal_vaccine ~ ., df[,-36])
seasonal_mejores06=FSelectorRcpp::cut_attrs(attrs = y, k = 0.6)

#Generamos un datarame para cada uno
dfh1n1= df %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                      "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                      "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                      "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                      "income_poverty", "hhs_geo_region", "h1n1_vaccine")

dfseasonal= df %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                          "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                          "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                          "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                          "opinion_seas_sick_from_vacc", "seasonal_vaccine")    

#Generamos los testp


testph1n1= testp %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                            "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                            "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                            "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                            "income_poverty", "hhs_geo_region")

testpseasonal= testp %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                                "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                                "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                                "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                                "opinion_seas_sick_from_vacc")    


#Sacamos dos train, uno para cada variable de salida:

train_h1n1 =dfh1n1
train_seasonal = dfseasonal

modelC4.5_h1n1=J48(h1n1_vaccine~., data=dfh1n1)
cv_resul=evaluate_Weka_classifier(modelC4.5_h1n1, numFolds = 10)
cv_resul


modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=train_h1n1)
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, testph1n1, type="probability")
modelC4.5_h1n1_vaccine.pred


#Ahora calculamos lo mismo para la seasonal_vaccine
modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=train_seasonal)
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testpseasonal, type="probability")
modelC4.5_seasonal_vaccine.pred

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = modelC4.5_h1n1_vaccine.pred[,2]
submission$seasonal_vaccine = modelC4.5_seasonal_vaccine.pred[,2]
submission

write_csv(submission, "multi_carac.csv") #SCORE 60% 0.7812
                                         #SCORE 70% 0.7750 #Variamos el valor de la k en la funcion cut_attrs por 0.7
                                         #SCORE 50% 0.7720 #Variamos el valor de la k en la funcion cut_attrs por 0.6
#Vemos que hay mejora por lo que nos quedamos con los datos con el preprocesamiento
#común y el 60% de las mejores caracterísitcas para cada clasificador.

##########################################################
#SELECCIÓN DE PARAMETROS
######################################################


#Lo aplicamos directamente para la competición. 
#En este caso solo se muestra los parámetros del mejor resultado obtenido para no repetir código. 
#Procedimineot que se ha seguido es variar primero la M y comparar con cv los porcentajes de acierto
#y se observó que se obtenía mejor porcentaje para M=8.

#Para ese valor de M, se varió el valor del parámetro C y se obtuvo mejor porcentaje para C=0.45
#Aun así se ha realizado tres subidad: C=0.45, C=0.5 y C=0.3

df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")
df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)
testp=testp %>% mutate_if(is.character, as.factor)


#Generamos un datarame para cada uno
dfh1n1= df %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                      "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                      "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                      "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                      "income_poverty", "hhs_geo_region", "h1n1_vaccine")

dfseasonal= df %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                          "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                          "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                          "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                          "opinion_seas_sick_from_vacc", "seasonal_vaccine")    

#Generamos los testp


testph1n1= testp %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                            "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                            "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                            "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                            "income_poverty", "hhs_geo_region")

testpseasonal= testp %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                                "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                                "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                                "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                                "opinion_seas_sick_from_vacc")    


#Sacamos dos train, uno para cada variable de salida:

train_h1n1 =dfh1n1
train_seasonal = dfseasonal

#Aplicamos cv para comaprar cada modelo y nos quedamos con el de mayor porcentaje de acierto
set.seed(9)
modelC4.5_h1n1=J48(h1n1_vaccine~., data=train_h1n1, control=Weka_control(M=8, C=0.45))
cv_resul=evaluate_Weka_classifier(modelC4.5_h1n1, numFolds = 10)
cv_resul


modelC4.5_h1n1_vaccine = J48(h1n1_vaccine~., data=train_h1n1, control=Weka_control(M=8, C=0.45))
modelC4.5_h1n1_vaccine.pred = predict(modelC4.5_h1n1_vaccine, testph1n1, type="probability")
modelC4.5_h1n1_vaccine.pred


#Ahora calculamos lo mismo para la seasonal_vaccine
modelC4.5_seasonal_vaccine = J48(seasonal_vaccine~., data=train_seasonal, control=Weka_control(M=8, C=0.45))
modelC4.5_seasonal_vaccine.pred = predict(modelC4.5_seasonal_vaccine, testpseasonal, type="prob")
modelC4.5_seasonal_vaccine.pred

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = modelC4.5_h1n1_vaccine.pred[,2]
submission$seasonal_vaccine = modelC4.5_seasonal_vaccine.pred[,2]
submission

write_csv(submission, "multi_param.csv") #SCORE 0.8131
#Para C=0.5 se obtuvo 0.8106
#Para C=0.3 se obtuvo 0.8004

#Por tanto, seguiremos intentando mejorar el clasificador con los datos ocn preprocesamiento común, 
#60% de las mejores caracterísitcas y con los parámetros M=8 y C=0.45.

#########################
#BAGGING
########################

df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")
df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)

testp=testp %>% mutate_if(is.character, as.factor)


#Generamos un datarame para cada uno con las características 
dfh1n1= df %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                      "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                      "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                      "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                      "income_poverty", "hhs_geo_region", "h1n1_vaccine")

dfseasonal= df %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                          "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                          "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                          "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                          "opinion_seas_sick_from_vacc", "seasonal_vaccine")    

#Generamos los test


testph1n1= testp %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                            "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                            "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                            "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                            "income_poverty", "hhs_geo_region")

testpseasonal= testp %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                                "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                                "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                                "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                                "opinion_seas_sick_from_vacc")    


#Sacamos dos train, uno para cada variable de salida:

train_h1n1 =dfh1n1
train_seasonal = dfseasonal

#Aplicamos bagging

set.seed(42)

train_bagging <- function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      classifier(h1n1_vaccine~., dato[samples,], control=Weka_control(M=8, C=0.45) )
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

models_h1n1 <- train_bagging(
  J48,
  40,
  train_h1n1
)

preds_h1n1 <- predict_bagging(
  models_h1n1,
  testph1n1
)

#Ahora calculamos lo mismo para la seasonal_vaccine

set.seed(42)

train_bagging <- function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      classifier(seasonal_vaccine~., dato[samples,], control=Weka_control(M=8, C=0.45) )
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

models_seasonal <- train_bagging(
  J48,
  40,
  train_seasonal
)

preds_seasonal <- predict_bagging(
  models_seasonal,
  testpseasonal
)

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")

submission$h1n1_vaccine = preds_h1n1[,2]
submission$seasonal_vaccine = preds_seasonal[,2]

submission

write_csv(submission, "multi_bagging.csv") #SCORE 0.8388
#Para 10 clasif score 0.8351

###################################
#RANDOM
###############

#Modificamos la función anterior
df=read_csv("train_imputed.csv")
testp=read_csv("test_imputed.csv")
df=df %>% mutate_if(is.character, as.factor)
df$h1n1_vaccine = as.factor(df$h1n1_vaccine)
df$seasonal_vaccine = as.factor(df$seasonal_vaccine)
str(df)

testp=testp %>% mutate_if(is.character, as.factor)

#Generamos un datarame para cada uno
dfh1n1= df %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                      "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                      "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                      "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                      "income_poverty", "hhs_geo_region", "h1n1_vaccine")

dfseasonal= df %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                          "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                          "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                          "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                          "opinion_seas_sick_from_vacc", "seasonal_vaccine")    

#Generamos los testp


testph1n1= testp %>% select("doctor_recc_h1n1", "opinion_h1n1_risk", "opinion_h1n1_vacc_effective","opinion_seas_risk",
                            "health_insurance", "doctor_recc_seasonal", "opinion_seas_vacc_effective", "employment_industry", "employment_occupation",      
                            "health_worker", "h1n1_concern", "h1n1_knowledge", "opinion_h1n1_sick_from_vacc", "chronic_med_condition",      
                            "behavioral_wash_hands", "behavioral_touch_face", "education", "behavioral_face_mask", "child_under_6_months",
                            "income_poverty", "hhs_geo_region")

testpseasonal= testp %>% select("opinion_seas_vacc_effective", "opinion_seas_risk", "doctor_recc_seasonal", "age_group", "opinion_h1n1_risk",     
                                "opinion_h1n1_vacc_effective", "employment_industry", "employment_occupation", "doctor_recc_h1n1",  "chronic_med_condition",      
                                "h1n1_concern", "employment_status",  "health_insurance", "health_worker", "household_children",         
                                "behavioral_touch_face", "h1n1_knowledge", "behavioral_wash_hands", "rent_or_own", "race",                     
                                "opinion_seas_sick_from_vacc")    


#Sacamos dos train, uno para cada variable de salida:

train_h1n1 =dfh1n1
train_seasonal = dfseasonal

#Aplicamos random

set.seed(42)

train_bagging <- function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      samplesAttributes <- sample(1:(dim(dato)[2]-1), replace = F, size = as.integer((dim(dato)[2] - 1)*0.7))
      classifier(h1n1_vaccine~., dato[samples, c(samplesAttributes, dim(dato)[2])], control=Weka_control(M=8, C=0.45))
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

models_h1n1 <- train_bagging(
  J48,
  90,
  train_h1n1
)

preds_h1n1 <- predict_bagging(
  models_h1n1,
  testph1n1
)

#Ahora calculamos lo mismo para la seasonal_vaccine

set.seed(42)

train_bagging <- function (classifier, number_classifiers, dato) {
  lapply(
    1:number_classifiers,
    function (i) {
      samples <- sample(1:dim(dato)[1], replace = T, size = dim(dato)[1])
      samplesAttributes <- sample(1:(dim(dato)[2]-1), replace = F, size = as.integer((dim(dato)[2] - 1)*0.7))
      classifier(seasonal_vaccine~., dato[samples, c(samplesAttributes, dim(dato)[2])], control=Weka_control(M=8, C=0.45))
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

models_seasonal <- train_bagging(
  J48,
  90,
  train_seasonal
)

preds_seasonal <- predict_bagging(
  models_seasonal,
  testpseasonal
)

#Preparamos el archivo para la subida
submission = read_csv("submission_format.csv")
submission$h1n1_vaccine = preds_h1n1[,2]
submission$seasonal_vaccine = preds_seasonal[,2]
submission

write_csv(submission, "multi_randomf.csv") #SCORE 0.8435
#Para number_classifiers=80 se obtuvo #0.8433
