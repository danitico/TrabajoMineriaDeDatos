library(tidyverse)
library(Amelia)
library(naniar)


feature <- read_csv("src/training_set_features.csv")
labels <- read_csv("src/training_set_labels.csv")

# Merging dataframes
df <- merge(
  feature,
  labels,
  by = "respondent_id"
)

# Deleting id variable
df$respondent_id <- NULL

# Ver cuantas columnas hay con missing values
columnsWithMissingValues <- sum(
  apply(
    df,
    2,
    function (x) {
      ifelse( length( which(is.na(x)) ) > 0, 1, 0)
    }
  )
)

# Quitar aquellas columnas que tengan mas de columnsWithMissingValues/2 missing values
df <- df %>% mutate(
  missingCount = apply(
    df,
    1,
    function (x) {
      length( which(is.na(x)) )
    }
  )
) %>% filter(
  missingCount < (columnsWithMissingValues/2)
) %>% mutate(
  missingCount = NULL
)

# df %>% write_csv(., "src/train.csv")


# Aquellos que no tengan empleo, tendrán una categoría diferente en employment occupation e industry
df <- df %>% mutate(
  employment_occupation=ifelse(employment_status != "Employed", "unemployed", employment_occupation),
  employment_industry=ifelse(employment_status != "Employed", "unemployed", employment_industry)
)

df %>% missmap(.)

# Rellenar con unos health insurance aquellos con mayores ingresos y que tengan trabajo
# Despues de esa operación, lo que tengan NA, se pondrá que no tienen seguro
df <- df %>% mutate(
  health_insurance=if_else(
    income_poverty == "> $75,000" & employment_status == "Employed" & is.na(health_insurance),
    1,
    health_insurance
  )
) %>% mutate(
  health_insurance=if_else(is.na(health_insurance), 0, health_insurance)
)

df %>% missmap(.)

# Sustituir por 0 los valores perdidos en recomendación del doctor de las vacunas
df <- df %>% mutate(
  doctor_recc_h1n1=if_else(is.na(doctor_recc_h1n1), 0, doctor_recc_h1n1),
  doctor_recc_seasonal=if_else(is.na(doctor_recc_seasonal), 0, doctor_recc_seasonal)
)

df %>% gg_miss_upset(.)

df %>% missmap(.)

indexes <- df %>% rownames_to_column() %>% filter(
  is.na(employment_status) & is.na(employment_industry) & is.na(employment_occupation) & is.na(rent_or_own) & is.na(income_poverty)
) %>% select(rowname) %>% unlist(.) %>% as.numeric()

df[indexes, "employment_status"] <- "Unemployed"
df[indexes, "employment_industry"] <- "unemployed"
df[indexes, "employment_occupation"] <- "unemployed"
df[indexes, "rent_or_own"] <- "none"
df[indexes, "income_poverty"] <- "Below Poverty"

# En el caso de children under 6 months, se entiende que si hay valor perdido, se trata 0
# Puede ser que el encuestador haya visto que sea una persona muy joven y por lo tanto, no tenga hijos

meow <- df %>% filter(is.na(child_under_6_months))

df %>% filter(is.na(employment_occupation)) %>% summarise(count=n())
df %>% filter(is.na(employment_industry)) %>% summarise(count=n())
df %>% filter(is.na(income_poverty)) %>% summarise(count=n())
df %>% filter(is.na(doctor_recc_seasonal)) %>% summarise(count=n())




df %>% filter(health_insurance == 0 & is.na(doctor_recc_h1n1)) %>% summarise(count=n())
df %>% filter(health_insurance == 0 & is.na(doctor_recc_seasonal)) %>% summarise(count=n())



