source("src/utils/functions.R")
source("preprocess.R")

library(AUC)
library(RWeka)
library(mice)

library(FSelectorRcpp)

imp=mice(df, maxit=3, meth='pmm', seed=1)

df=complete(imp)



df_labels=collapse_labels(df, 36, 37)
df_labels=df_labels %>% mutate_if(is.character, as.factor)

#pesosG <- FSelectorRcpp::information_gain(target~., df_labels)
#mejores <- FSelectorRcpp::cut_attrs(pesosG, k=0.5)
#mejores[[length(mejores) + 1]]="target"
#df_filter=df_labels %>% select(mejores)
df_filter=df_labels

set.seed(9)
train = sample(1:nrow(df_filter),2*nrow(df_filter)/3)
test = df_filter[-train,]
test_df=df[-train,]


# Aplico el algoritmo Ripper
model.Ripper = JRip(target~., df_filter, subset=train,  control = Weka_control(F=10, O=5))

summary(model.Ripper)


model.Ripper.pred = predict(model.Ripper, newdata = test)

model.Ripper.pred.char=(levels(model.Ripper.pred))[model.Ripper.pred]

h1n1_prediction=vector(mode="numeric", length = length(model.Ripper.pred.char))
seasonal_prediction=vector(mode="numeric", length = length(model.Ripper.pred.char))

for (i in 1:length(model.Ripper.pred.char)) {
  if(model.Ripper.pred.char[[i]]=="10"){
    h1n1_prediction[i]=1
  }
  if(model.Ripper.pred.char[[i]]=="01"){
    seasonal_prediction[i]=1
  }
  if(model.Ripper.pred.char[[i]]=="11"){
    h1n1_prediction[i]=1
    seasonal_prediction[i]=1
  }
}


ROC_h1n1=roc(as.factor(h1n1_prediction), as.factor(test_df[,36]))
acierto_h1n1=auc(ROC_h1n1)
acierto_h1n1

ROC_seasonal=roc(as.factor(seasonal_prediction), as.factor(test_df[,37]))
acierto_seasonal=auc(ROC_seasonal)
acierto_seasonal

mean(c(acierto_h1n1, acierto_seasonal))

model.Ripper

# CV con Ripper
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=10)
# Acierto (no es ROC)
cv_JRip$details[1]
