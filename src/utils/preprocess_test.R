source("src/utils/functions.R")
library(Amelia)
library(naniar)


features_test <- read_dataset("src/data/drivendata/test.csv")


# Aquellos que no tengan empleo, tendrán una categoría diferente en employment occupation e industry
features_test <- features_test %>% mutate(
  employment_occupation=ifelse(employment_status != "Employed", "unemployed", employment_occupation),
  employment_industry=ifelse(employment_status != "Employed", "unemployed", employment_industry)
)

#features_test %>% missmap(.)

# Rellenar con unos health insurance aquellos con mayores ingresos y que tengan trabajo
# Despues de esa operación, lo que tengan NA, se pondrá que no tienen seguro
features_test <- features_test %>% mutate(
  health_insurance=if_else(
    income_poverty == "> $75,000" & employment_status == "Employed" & is.na(health_insurance),
    1,
    health_insurance
  )
) %>% mutate(
  health_insurance=if_else(is.na(health_insurance), 0, health_insurance)
)

#features_test %>% missmap(.)

# Sustituir por 0 los valores perdidos en recomendación del doctor de las vacunas
features_test <- features_test %>% mutate(
  doctor_recc_h1n1=if_else(is.na(doctor_recc_h1n1), 0, doctor_recc_h1n1),
  doctor_recc_seasonal=if_else(is.na(doctor_recc_seasonal), 0, doctor_recc_seasonal)
)

#features_test %>% gg_miss_upset(.)

#features_test %>% missmap(.)

indexes <- features_test %>% rownames_to_column() %>% filter(
  is.na(employment_status) & is.na(employment_industry) & is.na(employment_occupation) & is.na(rent_or_own) & is.na(income_poverty)
) %>% select(rowname) %>% unlist(.) %>% as.numeric()

features_test[indexes, "employment_status"] <- "Unemployed"
features_test[indexes, "employment_industry"] <- "unemployed"
features_test[indexes, "employment_occupation"] <- "unemployed"
features_test[indexes, "rent_or_own"] <- "none"
features_test[indexes, "income_poverty"] <- "Below Poverty"

# En el caso de children under 6 months, se entiende que si hay valor perdido, se trata 0
# Puede ser que el encuestador haya visto que sea una persona muy joven y por lo tanto, no tenga hijos

meow <- features_test %>% filter(is.na(child_under_6_months))

#features_test %>% filter(is.na(employment_occupation)) %>% summarise(count=n())
#features_test %>% filter(is.na(employment_industry)) %>% summarise(count=n())
#features_test %>% filter(is.na(income_poverty)) %>% summarise(count=n())
#features_test %>% filter(is.na(doctor_recc_seasonal)) %>% summarise(count=n())




#features_test %>% filter(health_insurance == 0 & is.na(doctor_recc_h1n1)) %>% summarise(count=n())
#features_test %>% filter(health_insurance == 0 & is.na(doctor_recc_seasonal)) %>% summarise(count=n())
