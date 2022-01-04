source("src/utils/functions.R")
library(Amelia)
library(naniar)



UNEMPLOYED = "Unemployed"

df <- read_dataset("src/data/drivendata/train.csv")



### FASE 1: DESCARTE DE COLUMNAS DEMASIADO VACÍAS

# Ver cuantas columnas hay con missing values
columnsWithMissingValues <- sum(
  apply(
    df,
    2,
    function(x) any(is.na(x))
  )
)


# Quitar aquellas filas que tengan mas de columnsWithMissingValues/2 missing values

drop_too_empty_rows <- function(dataframe) {
  dataframe %>% mutate(
    missingCount = apply(
      df,
      1,
      function(x) sum(is.na(x))
    )
  ) %>% filter(
    missingCount < (columnsWithMissingValues/2)
  ) %>% mutate(
    missingCount = NULL
  )
}

df <- drop_too_empty_rows(df)



### FASE 2: IMPUTACIÓN DE VALORES

### 2.1.
# Aquellos que no tengan empleo, tendrán una categoría diferente en employment occupation e industry

input_unemployed_occupation_and_industry <- function(dataframe) {
  dataframe %>%
    mutate(
      employment_occupation = as.character(employment_occupation),
      employment_industry = as.character(employment_industry)
      ) %>% 
    mutate(
      employment_occupation=ifelse(employment_status != "Employed", UNEMPLOYED, employment_occupation),
      employment_industry=ifelse(employment_status != "Employed", UNEMPLOYED, employment_industry)
  ) %>% mutate_if(is.character, as.factor)
}

df <- input_unemployed_occupation_and_industry(df)

# df %>% missmap(.)


### 2.2.
# Rellenar con unos health insurance aquellos con mayores ingresos y que tengan trabajo
# Despues de esa operación, lo que tengan NA, poner que no tienen seguro
input_extreme_health_insurance <- function(dataframe) {
  dataframe %>%
    mutate(health_insurance = as.character(health_insurance)
           ) %>% 
    mutate(
    health_insurance=ifelse(
      income_poverty == "> $75,000" & employment_status == "Employed" & is.na(health_insurance),
      "1",
      health_insurance
    )
  ) %>%
    mutate(
      health_insurance=if_else(
        is.na(health_insurance),
        "0",
        health_insurance
    )
  ) %>% 
    mutate(health_insurance = as.factor(health_insurance))
}

df <- input_extreme_health_insurance(df)

# df %>% missmap(.)


### 2.3.
# Sustituir por 0 los valores perdidos en recomendación del doctor de las vacunas
input_vaccine_reccomendation <- function(dataframe) {
  dataframe %>%
    mutate(
      doctor_recc_h1n1 = as.character(doctor_recc_h1n1),
      doctor_recc_seasonal = as.character(doctor_recc_seasonal)
      ) %>% 
    mutate(
      doctor_recc_h1n1 = ifelse(is.na(doctor_recc_h1n1), 0, doctor_recc_h1n1),
      doctor_recc_seasonal = ifelse(is.na(doctor_recc_seasonal), 0, doctor_recc_seasonal)
      ) %>% 
    mutate(
      doctor_recc_h1n1 = as.factor(doctor_recc_h1n1),
      doctor_recc_seasonal = as.factor(doctor_recc_seasonal)
      )
}

df <- input_vaccine_reccomendation(df)

# df %>% gg_miss_upset(.)

# df %>% missmap(.)


### 2.4.
# Entender como desempleados a aquellos encuestados en los que los valores perdidos sigan un cierto patron

input_unemployed_with_common_na <- function(dataframe) {

  unemployed_indexes <- dataframe %>% rownames_to_column() %>% filter(
    is.na(employment_status) & is.na(employment_industry) & is.na(employment_occupation) & is.na(rent_or_own) & is.na(income_poverty)
  ) %>% select(rowname) %>% unlist(.) %>% as.numeric()
  
  levels(dataframe$rent_or_own) = levels(dataframe$rent_or_own) %>% c("Neither")
  
  dataframe[unemployed_indexes, "employment_status"] <- UNEMPLOYED
  dataframe[unemployed_indexes, "employment_industry"] <- UNEMPLOYED
  dataframe[unemployed_indexes, "employment_occupation"] <- UNEMPLOYED
  dataframe[unemployed_indexes, "rent_or_own"] <- "Neither"
  dataframe[unemployed_indexes, "income_poverty"] <- "Below Poverty"
  
  dataframe
}

df <- input_unemployed_with_common_na(df)


# En el caso de children under6 months, se entiende que si hay valor perdido, se trata 0
# Puede ser que el encuestador haya visto que sea una persona muy joven y por lo tanto, no tenga hijos
# Entendemos que aquellos NA en marital status se puede deber a otra situacion (viudo, divorciado)
# En este punto, en donde hay un NA en education significa que no ha recibido educación (en eeuu hay una tasa importante de analfabetismo)
# Respecto a que haya NA en health_worker, se entiende que el encuestador haya visto a lo largo
# de la encuesta que trabaje en otro sector (solo trabajan 8 en ese grupo) y que los demas
# sean jubilados o estudiantes

misc_changes <- function(df) {
  df %>% mutate(
    child_under_6_months=ifelse(
      is.na(child_under_6_months),
      0,
      child_under_6_months
    ),
    marital_status=ifelse(
      is.na(marital_status),
      "other",
      marital_status
    ),
    education=ifelse(
      is.na(education),
      "other",
      education
    ),
    health_worker=ifelse(
      is.na(health_worker),
      0,
      health_worker
    )
  )
}

df <-  misc_changes(df)

# Aquellos que tengan NA en rent_or_own y income_poverty
# se puede deber a jovenes que viven con sus padres (64 entre 18 y 34)
# jubilados que vivan en un asilo y que ya no tengan income como tal (189 que tienen mas de 65)
# gente sin hogar, o gente mayor que siga viviendo con sus padres

no_income_and_home <- function(df) {
  indexes1 <- df %>% rownames_to_column() %>% filter(
    is.na(rent_or_own) & is.na(income_poverty)
  ) %>% select(rowname) %>% unlist(.) %>% as.numeric()

  df[indexes1, "rent_or_own"] <- "none"
  df[indexes1, "income_poverty"] <- "Below Poverty"

  df
}

df <- no_income_and_home(df)
df %>% write_csv("src/data/train_imputed.csv")

### TODO: DE AQUÍ EN ADELANTE NO SE USA

# En el caso de children under 6 months, se entiende que si hay valor perdido, se trata 0
# Puede ser que el encuestador haya visto que sea una persona muy joven y por lo tanto, no tenga hijos

meow <- df %>% filter(is.na(child_under_6_months))

df %>% filter(is.na(employment_occupation)) %>% summarise(count=n())
df %>% filter(is.na(employment_industry)) %>% summarise(count=n())
df %>% filter(is.na(income_poverty)) %>% summarise(count=n())
df %>% filter(is.na(doctor_recc_seasonal)) %>% summarise(count=n())




df %>% filter(health_insurance == 0 & is.na(doctor_recc_h1n1)) %>% summarise(count=n())
df %>% filter(health_insurance == 0 & is.na(doctor_recc_seasonal)) %>% summarise(count=n())



