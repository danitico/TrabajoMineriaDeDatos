source("src/utils/functions.R")
source("preprocess.R")

library(AUC)
#options(java.home="C:\\Program Files (x86)\\Java\\jre1.8.0_291")
library(RWeka)
library(mice)

library(FSelectorRcpp)

library(tidyverse)
library(Amelia)
library(naniar)


#features <- read_csv("src/training_set_featuress.csv")
#labels <- read_csv("src/training_set_labels.csv")

# Merging dataframes
#df <- merge(
#  features,
#  labels,
#  by = "respondent_id"
#)

#df =df %>% select(-respondent_id)


#imp=mice(df, maxit=3, meth='pmm', seed=1)

#df=complete(imp)



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
model.Ripper = JRip(target~., df_filter, subset=train,  control = Weka_control())

summary(model.Ripper)


model.Ripper.pred = predict(model.Ripper, newdata = test, type = 'probability')

prediccion=as.data.frame(model.Ripper.pred)
prediccion=prediccion %>% mutate(id=as.numeric(rownames(prediccion))) %>% relocate(id)

prediccion_split=split_target(prediccion)

ROC_h1n1=roc(as.factor(prediccion_split[,2]), as.factor(test_df[,36]))
acierto_h1n1=auc(ROC_h1n1)
acierto_h1n1

ROC_seasonal=roc(as.factor(prediccion_split[,3]), as.factor(test_df[,37]))
acierto_seasonal=auc(ROC_seasonal)
acierto_seasonal

mean(c(acierto_h1n1, acierto_seasonal))

source("preprocess_test.R")

features_test=features_test %>% mutate_if(is.character, as.factor)
id_test=features_test %>%select(respondent_id)
id_test=id_test[[1]]
features_test=features_test %>%select(-respondent_id)

model.Ripper.pred.test = predict(model.Ripper, newdata = features_test, type = 'probability')

prediccion_test=as.data.frame(model.Ripper.pred.test)
prediccion_test=prediccion_test %>% mutate(id=id_test) %>% relocate(id)

prediccion_split_test=split_target(prediccion_test)
prediccion_split_test=as.data.frame(prediccion_split_test)

write_csv(prediccion_split_test, "submission_JRIP.csv")

# CV con Ripper
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=10)
# Acierto (no es ROC)
cv_JRip$details[1]
