# R Script for Analysis of Attitudes Toward Immigrants 
# Author: Mano (Emmanuel Dirk) van Holten
# Date: 24 -06 - 2024
# Description: This script includes: Data Pre-processing, Missing Data Analysis, Statistical Models

library(psych)
library(dplyr)
library(tidyr)
library(readxl)
df <- read.csv("your file path")                                                #https://ess.sikt.no/en/?tab=overview

#1.1 Comparing Measurement Modes 
describe(df$mode)
df$mode[df$mode == 9] <- NA                                                     #Defining NA

v1 <- c("imsmetn", "imdfetn", "impcntr")
df <- df %>% mutate(across(all_of(v1), ~ ifelse(. %in% c(7, 8, 9), NA, .)))
v2 <- c("imbgeco", "imueclt", "imwbcnt")
df <- df %>% mutate(across(all_of(v2), ~ replace(., . %in% c(77, 88, 99), NA)))

vars <- c("imsmetn", "imdfetn", "impcntr", "imbgeco", "imueclt", "imwbcnt",     #Subsetting
          "mode")
subset <- df[vars]
compsub <- subset[complete.cases(subset), ]
variables <- c("imsmetn", "imdfetn", "impcntr", "imbgeco", "imueclt", 
               "imwbcnt")
wilcox_results <- list()

for (i in 1:3) {                                                                #Function to compare all modes
  for (j in (i + 1):4) {
    mode1 <- i
    mode2 <- j
    
    mode1_data <- compsub[compsub$mode == mode1, variables]
    mode2_data <- compsub[compsub$mode == mode2, variables]
    
    if (nrow(mode1_data) > 1 && nrow(mode2_data) > 1) {
      test_results <- lapply(variables, function(var) {
        wilcox.test(mode1_data[[var]], mode2_data[[var]])
      })
      
      p_values <- sapply(test_results, function(test) {
        test$p.value
      })
      
      test_results_df <- data.frame(Variable = variables, P_Value = p_values)
      test_results_df$Mode1 <- mode1
      test_results_df$Mode2 <- mode2
      
      wilcox_results[[length(wilcox_results) + 1]] <- test_results_df
    }
  }
}

mode_result <- do.call(rbind, wilcox_results)
print(mode_result)                                                              #Resulting P-values
df <- df[df$edition != 3.1, ]                                                   #Dropping affected modes

#1.2 Missing Data Analysis (for analysis 1)
v3 <- c("imsmetn", "imdfetn", "impcntr", "imbgeco", "imueclt", "imwbcnt")
df <- df %>% mutate(comp_case = ifelse(rowSums(is.na(dplyr::select(., all_of(v3)))) > 0, 0, 1))
df <- df %>%
  mutate(imsmetn_missing = if_else(is.na(imsmetn), 0, 1),
         imdfetn_missing = if_else(is.na(imdfetn), 0, 1),
         impcntr_missing = if_else(is.na(impcntr), 0, 1),
         imbgeco_missing = if_else(is.na(imbgeco), 0, 1),
         imueclt_missing = if_else(is.na(imueclt), 0, 1),
         imwbcnt_missing = if_else(is.na(imwbcnt), 0, 1))
v4 <- c("imsmetn_missing", "imdfetn_missing", "impcntr_missing", 
        "imbgeco_missing", "imueclt_missing", "imwbcnt_missing", 
        "comp_case")
freq <- lapply(df[v4], table)
print(freq)                                                                     #Missingness per variable

complete_cases <- df[complete.cases(df$imsmetn, df$imdfetn, df$impcntr,         #Missingness per country
                     df$imbgeco, df$imueclt, df$imwbcnt, df$cntry), ]
table(complete_cases$cntry)

phi_matrix <- cor(df[v4], method = "pearson")                                   #Missingness correlation matrix
print(phi_matrix)

par(mfrow=c(3, 2))                                                              #Assesing normality
hist(df$imsmetn, main="Histogram of same group", xlab="imsmetn")       
hist(df$imdfetn, main="Histogram of other group", xlab="imdfetn")
hist(df$impcntr, main="Histogram of poor country", xlab="impcntr")
hist(df$imbgeco, main="Histogram of economy", xlab="imbgeco")
hist(df$imueclt, main="Histogram of culture", xlab="imueclt")
hist(df$imwbcnt, main="Histogram of general", xlab="imwbcnt")

vcon <- c("lrscale", "agea", "edulvlb", "hincfel", "health", "domicil")         #Defining continuous variables
vcat <- c("rlgblg", "ctzcntr", "gndr","rshpsts", "mainact")                     #Defining categorical variables

v789 <- c("rlgblg", "ctzcntr", "hincfel", "health", "domicil")                  #Defining NA and recoding direction
df <- df %>% mutate(across(all_of(v789), ~ ifelse(. %in% c(7, 8, 9), NA, .)))
v778899 <- c("rshpsts", "lrscale")
df <- df %>% mutate(across(all_of(v778899), ~ ifelse(. %in% c(77, 88, 99), NA, .)))
df$agea[df$agea == 999] <- NA
df <- df %>%
  mutate(edulvlb = case_when(
    edulvlb == 0 ~ 0,
    between(edulvlb, 100, 199) ~ 1,
    between(edulvlb, 200, 299) ~ 2,
    between(edulvlb, 300, 399) ~ 3,
    between(edulvlb, 400, 499) ~ 4,
    between(edulvlb, 500, 599) ~ 5,
    between(edulvlb, 600, 699) ~ 6,
    between(edulvlb, 700, 800) ~ 7,
    TRUE ~ NA_integer_
  ))

df$gndr[df$gndr == 9] <- NA
df <- df %>%
  mutate(health = recode(health, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1))
df <- df %>%
  mutate(hincfel = recode(hincfel, `1` = 4, `2` = 3, `3` = 2, `4` = 1))
df$rshpsts <- ifelse(df$rshpsts %in% c(1, 2, 3, 4) & !is.na(df$rshpsts), 1, 0)
na_values <- c(66, 77, 88, 99)
df$mainact[df$mainact %in% na_values] <- NA
df$mainact <- ifelse(df$mainact %in% c(1, 2, 5, 6, 7, 8, 9), 1,
                     ifelse(df$mainact %in% c(3, 4), 0, df$mainact))

df$gndr[df$gndr == 2] <- 0
df$ctzcntr[df$ctzcntr == 2] <- 0
df$rlgblg[df$rlgblg == 2] <- 0
breaks <- c(15, 30, 50, 65, 85, Inf)
labels <- c("Age 15-29", "Age 30-49", "Age 50-64", "Age 65-84", "Age 85 over")
df$age <- df$agea
df$agea <- cut(df$agea,
               breaks = breaks,
               labels = labels,
               include.lowest = TRUE,
               right = FALSE)
df$agea <- as.integer(df$agea)

par(mfrow=c(3, 2))                                                              #Assesing normality of continuous variables
hist(df$lrscale, main="Histogram of lrscale", xlab="lrscale")
hist(df$agea, main="Histogram of agea", xlab="agea")
hist(df$edulvlb, main="Histogram of edulvlb", xlab="edulvlb")
hist(df$hincfel, main="Histogram of hincfel", xlab="hincfel")
hist(df$health, main="Histogram of health", xlab="health")
hist(df$domicil, main="Histogram of domicil", xlab="domicil")

df_comp_case_1 <- subset(df, comp_case == 1, select = c(vcon))                  #Averages for complete cases (con)
df_comp_case_0 <- subset(df, comp_case == 0, select = c(vcon))                  #Averages for incomplete cases (con)
summary(df_comp_case_1)
summary(df_comp_case_0)

mannwhitney_results <- lapply(c("lrscale", "agea", "edulvlb", "hincfel",        #Mann-Whitney instead of T-Test (normality)
                                "health", "domicil"),                           
          function(var) wilcox.test(df[[var]] ~ df$comp_case, data = df))
names(mannwhitney_results) <- c("lrscale", "agea", "edulvlb", "hincfel", 
                                "health", "domicil")
mannwhitney_results                                                             #Significance of difference between groups

df_comp_case_1 <- subset(df, comp_case == 1, select = c(vcat))                  #Averages for complete cases (cat)            
df_comp_case_0 <- subset(df, comp_case == 0, select = c(vcat))                  #Averages for incomplete cases (cat)
summary(df_comp_case_1)
summary(df_comp_case_0)

perform_chi_squared <- function(var, group) {
  cross_tab <- table(var, group)
  chi_squared_result <- chisq.test(cross_tab)
  return(chi_squared_result)
}

chi_squared_results <- lapply(vcat, 
                              function(var) perform_chi_squared(df[[var]], 
                              df$comp_case))
names(chi_squared_results) <- vcat
chi_squared_results                                                             #Significance of difference between groups 


#1.3 Multicolinearity (For analysis 1)
library(car)
subc <- df[v3]
subc <- subc[complete.cases(subc), ]
cormat <- cor(subc)
print(cormat)                                                                   #Correlation matrix

vif_values <- numeric(length(subc))
for (i in 1:length(subc)) {
  lm_model <- lm(subc[, i] ~ ., data = subc[, -i])
  vif_values[i] <- 1 / (1 - summary(lm_model)$r.squared)
}
print(vif_values)                                                               #VIF values 

#1.4 Factorability (For analysis 1)
print(cormat) 
describe(subc)
btest <- cortest.bartlett(cormat, n = 34570)
print(btest)                                                                    #Bartlett's test of Sphericity

kmo <- KMO(subc)
print(kmo)                                                                      #Kaiser-Meyer-Olkin values

#1.5 Rescaling and Z-transforming (For analysis 1)
library(cowplot)
df$imsmetn <- dplyr::recode(df$imsmetn, `4` = 1, `3` = 2, `2` = 3, `1` = 4)     #Higher values = more positive
df$imdfetn <- dplyr::recode(df$imdfetn, `4` = 1, `3` = 2, `2` = 3, `1` = 4)
df$impcntr <- dplyr::recode(df$impcntr, `4` = 1, `3` = 2, `2` = 3, `1` = 4)

df <- df %>%                                                                    #Z-transformation
  mutate(
    z_imsmetn = scale(imsmetn),
    z_imdfetn = scale(imdfetn),
    z_impcntr = scale(impcntr),
    z_imbgeco = scale(imbgeco),
    z_imueclt = scale(imueclt),
    z_imwbcnt = scale(imwbcnt)
  )

#1.6 Exploratory Factor Analysis 1
library(EFA.dimensions)
vz <- c("z_imsmetn", "z_imdfetn", "z_impcntr", "z_imbgeco", "z_imueclt", "z_imwbcnt")
sube <- df[vz]
sube <- sube[complete.cases(sube), ]                                            #Subsetting for analysis 
resEFA <- EFA.dimensions::EFA(sube, extraction = 'paf', corkind = 'pearson', 
                              Nfactors = 2, iterpaf = 1000,
                              rotation='oblimin', verbose = TRUE)
print(resEFA)                                                                   #Results of EFA 1

#1.7 Determining Factor retention
library(EFAtools)
par(mfrow=c(1,1))
SCREE(sube, corkind = "pearson", Ncases = NULL, verbose = TRUE)                 #Scree-plot

pa_result <- EFAtools::PARALLEL(                                                #Paralell analysis
  x = sube,
  n_datasets = 1000,
  eigen_type = "EFA",
  cor_method = "pearson",
  decision_rule = 
)
print(pa_result)
pa_result$eigenvalues_EFA

mapr <- MAP(sube, corkind = "pearson", verbose = TRUE)                          #Minimum Average Partial
print(mapr)

#1.8 Investigating the Window of Data Colletion (Since the CFA compares countries)
library(lubridate)
cov <- read.csv("Your file path")                                               #COVID19-data: https://ourworldindata.org/coronavirus
asy <- read_xlsx("Your file path")                                              #Asylum data: https://doi.org/10.2908/MIGR_ASYAPPCTZM

df$inwde <- as.POSIXct(df$inwde, format = "%Y-%m-%d %H:%M:%S")                  #Data preprations
df$year_month <- format(df$inwde, "%Y-%m")

aggregated_df <- aggregate(cbind(imsmetn, imdfetn, impcntr, imbgeco, imueclt,   #Binding data frames
                                 imwbcnt) ~ year_month, 
                                 data = df, FUN = sum)

asy_clean <- asy %>%                                                 
  mutate(across(-Month, ~ as.numeric(.)))
asy_clean[is.na(asy_clean)] <- 0
asy_summarized <- asy_clean %>%
  summarise(across(-Month, sum))
asy_long <- asy_clean %>%
  pivot_longer(-Month, names_to = "year_month", values_to = "value") %>%
  group_by(year_month) %>%
  summarise(total_value = sum(value, na.rm = TRUE))
aggregated_df <- aggregated_df %>%
  left_join(asy_long, by = c("year_month" = "year_month"))

months_to_keep <- c("2020-09", "2020-10", "2021-05", "2021-06", "2021-07", "2021-08",   #Removing unnecessary data
                    "2021-09", "2021-10", "2021-11", "2021-12", "2022-01", "2022-02", 
                    "2022-03", "2022-04", "2022-05", "2022-06", "2022-07", "2022-08", "2022-09")
country_codes <- c("Belgium", "Bulgaria", "Switzerland", "Czechia", "Estonia", "Finland", 
                   "France", "United Kingdom", "Greece", "Croatia", "Hungary", "Ireland", 
                   "Iceland", "Italy", "Lithuania", "Montenegro", "North Macedonia", 
                   "Netherlands", "Norway", "Portugal", "Slovenia", "Slovakia") 
cov_filtered <- cov %>%
  filter(location %in% country_codes) %>%
  select(location, date, new_cases, new_deaths)
cov_filtered <- cov_filtered %>%
  mutate(year_month = format(as.Date(date), "%Y-%m"))
cov_filtered <- cov_filtered %>%
  filter(year_month %in% months_to_keep)
aggregated1_df <- cov_filtered %>%
  group_by(year_month) %>%
  summarise(
    total_new_cases = sum(new_cases, na.rm = TRUE),
    total_new_deaths = sum(new_deaths, na.rm = TRUE)
  ) %>%
  ungroup()
merged_df <- left_join(aggregated_df, aggregated1_df, by = "year_month")        #Merging all Df's

write.csv(merged_df, "agg_df.csv", row.names = FALSE)                           #Exporting to create figures in Tableau

df$recoded_month <- as.integer(factor(df$year_month, levels = unique(df$year_month)))
df <- df %>%
  mutate(
    recoded_month = case_when(
      recoded_month %in% c(1, 2) ~ 1,
      recoded_month %in% c(3, 4, 5) ~ 2,
      recoded_month %in% c(6, 7, 8) ~ 3,
      recoded_month %in% c(9, 10, 11, 12) ~ 4,
      recoded_month %in% c(13, 14, 15, 16) ~ 5,
      recoded_month %in% c(17, 18, 19) ~ 6
    )
  )

df_time <- df %>%
  select(cntry, recoded_month)
table_df <- table(df_time$cntry, df_time$recoded_month)
print(table_df)
chisq.test(table_df)                                                            #Testing if countries differed in data-collection window

df$recoded_month_1 <- ifelse(df$recoded_month == 1, 1, 0)
df$recoded_month_2 <- ifelse(df$recoded_month == 2, 1, 0)
df$recoded_month_3 <- ifelse(df$recoded_month == 3, 1, 0)
df$recoded_month_4 <- ifelse(df$recoded_month == 4, 1, 0)
df$recoded_month_5 <- ifelse(df$recoded_month == 5, 1, 0)
df$recoded_month_6 <- ifelse(df$recoded_month == 6, 1, 0)

timedf <- df[, c("recoded_month_1", "recoded_month_2", "recoded_month_3",       #Testing if differences in variables emerged over time
                 "recoded_month_4", "recoded_month_5", "recoded_month_6", "imsmetn",
                 "imdfetn", "impcntr", "imbgeco", "imueclt", "imwbcnt")]
premod1 <- lm(imsmetn ~ recoded_month_1 + recoded_month_2 + recoded_month_3 +
                recoded_month_5 + recoded_month_6, data = timedf)
premod2 <- lm(imdfetn ~ recoded_month_1 + recoded_month_2 + recoded_month_3 +
                recoded_month_5 + recoded_month_6, data = timedf)
premod3 <- lm(impcntr ~ recoded_month_1 + recoded_month_2 + recoded_month_3 +
                recoded_month_5 + recoded_month_6, data = timedf)
premod4 <- lm(imbgeco ~ recoded_month_1 + recoded_month_2 + recoded_month_3 +
                recoded_month_5 + recoded_month_6, data = timedf)
premod5 <- lm(imueclt ~ recoded_month_1 + recoded_month_2 + recoded_month_3 +
                recoded_month_5 + recoded_month_6, data = timedf)
premod6 <- lm(imwbcnt ~ recoded_month_1 + recoded_month_2 + recoded_month_3 +
                recoded_month_5 + recoded_month_6, data = timedf)
summary(premod6)

df$imdfetn <- ifelse(df$recoded_month == 1, df$imdfetn + 0.150, df$imdfetn)     #Correcting for spurious effects 
df$impcntr <- ifelse(df$recoded_month == 1, df$impcntr + 0.217, df$impcntr)

df$imdfetn <- ifelse(df$recoded_month == 2, df$imdfetn + 0.104, df$imdfetn)
df$impcntr <- ifelse(df$recoded_month == 2, df$impcntr + 0.112, df$impcntr)
df$imueclt <- ifelse(df$recoded_month == 2, df$imueclt + 0.087, df$imueclt)

df$imdfetn <- ifelse(df$recoded_month == 3, df$imdfetn + 0.074, df$imdfetn)
df$impcntr <- ifelse(df$recoded_month == 3, df$impcntr + 0.067, df$impcntr)
df$imbgeco <- ifelse(df$recoded_month == 3, df$imbgeco + 0.224, df$imbgeco)
df$imueclt <- ifelse(df$recoded_month == 3, df$imueclt + 0.201, df$imueclt)
df$imwbcnt <- ifelse(df$recoded_month == 3, df$imwbcnt + 0.254, df$imwbcnt)

df$imsmetn <- ifelse(df$recoded_month == 5, df$imsmetn - 0.162, df$imsmetn)
df$imdfetn <- ifelse(df$recoded_month == 5, df$imdfetn - 0.201, df$imdfetn)
df$impcntr <- ifelse(df$recoded_month == 5, df$impcntr - 0.203, df$impcntr)
df$imbgeco <- ifelse(df$recoded_month == 5, df$imbgeco - 0.360, df$imbgeco)
df$imueclt <- ifelse(df$recoded_month == 5, df$imueclt - 0.606, df$imueclt)
df$imwbcnt <- ifelse(df$recoded_month == 5, df$imwbcnt - 0.496, df$imwbcnt)

df$impcntr <- ifelse(df$recoded_month == 6, df$impcntr + 0.086, df$impcntr)
df$imbgeco <- ifelse(df$recoded_month == 6, df$imbgeco - 0.237, df$imbgeco)
df$imueclt <- ifelse(df$recoded_month == 6, df$imueclt - 0.243, df$imueclt)
df$imwbcnt <- ifelse(df$recoded_month == 6, df$imwbcnt - 0.232, df$imwbcnt)

df <- df %>%                                                                    #Z-transforming once more
  mutate(
    z_imsmetn = scale(imsmetn),
    z_imdfetn = scale(imdfetn),
    z_impcntr = scale(impcntr),
    z_imbgeco = scale(imbgeco),
    z_imueclt = scale(imueclt),
    z_imwbcnt = scale(imwbcnt)
  )

#1.9 Confirmatory factor analysis & country comparison
library(lavaan)
library(openxlsx)

vars <- c("z_imsmetn", "z_imdfetn", "z_impcntr", "z_imbgeco", "z_imueclt",      #Subsetting
          "z_imwbcnt", "cntry")
subset <- df[vars]
compsub <- subset[complete.cases(subset), ]

im.model <- '                                                                   #Model defenition
ethnic =~ z_imsmetn + z_imdfetn + z_impcntr
threat =~ z_imbgeco + z_imueclt + z_imwbcnt
ethnic ~~ threat
'

set.seed(42069)
im.fit <- cfa(im.model, data=compsub, se = "bootstrap", bootstrap = 1000,       #Fitting the model
              rotation = "oblimin")
summary(im.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)
parameterestimates(im.fit, ci = TRUE, level = 0.9)

compsubf <- data.frame(cntry = compsub$cntry)                                   #Computing latent variables
compsubf$ethnicfac <- with(compsub, z_imsmetn + 1.260 * z_imdfetn + 1.172 * z_impcntr)
compsubf$threatfac <- with(compsub, z_imbgeco + 1.053 * z_imueclt + 1.049 * z_imwbcnt)
aggdf <- aggregate(. ~cntry, data = compsubf, FUN = mean)
scaled_data <- apply(aggdf[, -1], 2, function(x) (x - min(x)) /                 #Min-max scaling latent variables
                       (max(x) - min(x))) 
scaled_df <- cbind(aggdf[, 1, drop = FALSE], scaled_data)
country_names <- data.frame(
  cntry = c("BE", "BG", "CH", "CZ", "EE", "FI", "FR", "GB", "GR", "HR", "HU", 
            "IE", "IS", "IT", "LT", "ME", "MK", "NL", "NO", "PT", "SI", "SK"),
  country_name = c("Belgium", "Bulgaria", "Switzerland", "Czech Republic", 
                   "Estonia", "Finland", "France", "United Kingdom", "Greece", 
                   "Croatia", "Hungary", "Ireland", "Iceland", "Italy", "Lithuania", 
                   "Montenegro", "North Macedonia", "Netherlands", "Norway", "Portugal", 
                   "Slovenia", "Slovakia")
)

scaled_df <- merge(scaled_df, country_names, by = "cntry")                              
print(scaled_df)                                                                #Relative factor score per country
excel_file <- "agg_data_att.xlsx" 
write.xlsx(scaled_df, file = excel_file, rowNames = FALSE)                      #Exporting files for Tableau Visualisation

varsabs <- c("imsmetn", "imdfetn", "impcntr", "imbgeco", "imueclt", "imwbcnt", 
             "cntry")
subsetabs <- df[varsabs]
compsubabs <- subsetabs[complete.cases(subsetabs), ]
print(nrow(compsubabs))  
compsubabs$ethnicfac <- with(compsubabs, imsmetn + 1.260 * imdfetn +
                               1.172 * impcntr) / 3  
compsubabs$threatfac <- with(compsubabs, imbgeco + 1.053 * imueclt + 
                               1.049 * imwbcnt) / 3
aggdfabs <- aggregate(cbind(ethnicfac, threatfac) ~ cntry, data = compsubabs, FUN = mean)
country_names <- data.frame(
  cntry = c("BE", "BG", "CH", "CZ", "EE", "FI", "FR", "GB", "GR", "HR", 
            "HU", "IE", "IS", "IT", "LT", "ME", "MK", "NL", "NO", "PT", "SI", "SK"),
  country_name = c("Belgium", "Bulgaria", "Switzerland", "Czech Republic", 
                   "Estonia", "Finland", "France", "United Kingdom", "Greece", 
                   "Croatia", "Hungary", "Ireland", "Iceland", "Italy", "Lithuania", 
                   "Montenegro", "North Macedonia", "Netherlands", "Norway", "Portugal", 
                   "Slovenia", "Slovakia")
)
aggdfabs <- merge(aggdfabs, country_names, by = "cntry")            
print(aggdfabs)                                                                 #Absolute factor scores per country                                                                         
  
#2.1 Data pre-processing Analysis 2: defining NA and recoding direction
df$brncntr <- ifelse(df$brncntr == 1, 1, ifelse(df$brncntr == 2, 0, NA))
df$mocntr <- ifelse(df$mocntr == 1, 1, ifelse(df$mocntr == 2, 0, NA))
df$facntr <- ifelse(df$facntr == 1, 1, ifelse(df$facntr == 2, 0, NA))
df$atchctr[df$atchctr %in% c(77,88,99)] <- NA

df <- df %>%
  mutate(
    across(c(ipeqopt, impdiff, ipfrule, ipudrst, ipbhprp, imptrad, 
             lrnobed, iphlppl), ~ ifelse(. %in% c(7, 8, 9), NA, .))
  )

df$rlgdgr[df$rlgdgr %in% c(77, 88, 99)] <- NA
df$rlgatnd[df$rlgatnd %in% c(77, 88, 99)] <- NA
df$pray [df$pray %in% c(77, 88, 99)] <- NA
df$rlgdnm[df$rlgdnm %in% c(77,88,99)] <- NA

df <- df %>%
  mutate(pray = case_when(
    pray == 1 ~ 7,
    pray == 2 ~ 6,
    pray == 3 ~ 5,
    pray == 4 ~ 4,
    pray == 5 ~ 3,
    pray == 6 ~ 2,
    pray == 7 ~ 1,
    TRUE ~ pray  
  ))
df <- df %>%
  mutate(rlgatnd = case_when(
    rlgatnd == 1 ~ 7,
    rlgatnd == 2 ~ 6,
    rlgatnd == 3 ~ 5,
    rlgatnd == 4 ~ 4,
    rlgatnd == 5 ~ 3,
    rlgatnd == 6 ~ 2,
    rlgatnd == 7 ~ 1,
    TRUE ~ rlgatnd  
  ))
df <- df %>%
  mutate(
    imptrad = dplyr::recode(imptrad, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    ipbhprp = dplyr::recode(ipbhprp, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    ipfrule = dplyr::recode(ipfrule, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    ipeqopt = dplyr::recode(ipeqopt, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    ipudrst = dplyr::recode(ipudrst, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    iphlppl = dplyr::recode(iphlppl, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
  )
df <- df %>%
  mutate(lrnobed = case_when(
    lrnobed == 1 ~ 5,
    lrnobed == 2 ~ 4,
    lrnobed == 3 ~ 3,
    lrnobed == 4 ~ 2,
    lrnobed == 5 ~ 1,
    TRUE ~ lrnobed
  ))

#2.2 Missing Data Analysis (For Analysis 2)
df$complete_case <- ifelse(complete.cases(df[c("agea", "gndr", "brncntr", "facntr", "mocntr", 
                                               "atchctr", "ipeqopt", "ipfrule", "ipudrst", "iphlppl",
                                               "ipbhprp", "imptrad", "lrnobed", "rlgdgr", "rlgatnd", "pray", 
                                               "imsmetn", "imdfetn", "impcntr", "imbgeco", "imueclt", 
                                               "imwbcnt")]), 1, 0)
table(df$complete_case)                                                         #Total missing

vip <- c("agea", "gndr", "brncntr", "facntr", "mocntr",                       
         "atchctr", "ipeqopt", "ipfrule", "ipudrst", "iphlppl",
         "ipbhprp", "imptrad", "lrnobed", "rlgdgr", "rlgatnd", "pray", 
         "imsmetn", "imdfetn", "impcntr", "imbgeco", "imueclt", 
         "imwbcnt")     
for (variable in vip) {                                                         #Assessing Normality for all variables
  hist(df[[variable]], main = paste("Histogram of", variable), xlab = variable)
}

variables_of_interest <- c("imsmetn", "imdfetn", "impcntr", "imbgeco", "imueclt",
                           "imwbcnt", "complete_case")
subset_df <- df[variables_of_interest]
subset_df_complete <- na.omit(subset_df)

df_comp_case_1 <- subset(subset_df_complete, complete_case == 1, 
                         select = c(variables_of_interest))
df_comp_case_0 <- subset(subset_df_complete, complete_case == 0, 
                         select = c(variables_of_interest))
summary(df_comp_case_1)                                                         #Average values non-missing group
summary(df_comp_case_0)                                                         #Average values missing group                               

wilcox_results <- lapply(variables_of_interest, function(var) {
  x <- subset_df_complete[[var]][complete.cases(subset_df_complete[[var]])]
  y <- subset_df_complete$complete_case[complete.cases(subset_df_complete[[var]])]
  wilcox.test(x ~ y)
})
names(wilcox_results) <- variables_of_interest
wilcox_results                                                                  #Man-Whitney-U test (differences between groups)

#2.3 Data imputation
library(mice)
dimput <- mice(visk, m=10)                                                      #Imputation
imcom <- complete(dimput)                                                       #Saving data
aggr(imcom)                                                                     #Visualising missingness

t_test_results <- list()                                                        #Comparing original means with imputed means
for (variable in vip) {
  t_test_result <- t.test(vis[[variable]], imcomp[[variable]])
  t_test_results[[variable]] <- t_test_result
}
t_test_results

plot_density <- function(var_name, df1, df2) {                                  #Comparing original distributions with imputed distributio
  dens1 <- density(df1[[var_name]], na.rm = TRUE)
  plot(dens1, main = paste("Density Plot of", var_name), 
       col = "blue", ylim = c(0, max(dens1$y)))
  
  dens2 <- density(df2[[var_name]], na.rm = TRUE)
  lines(dens2, col = "red", lty = 3)
  return(NULL)
}
for (variable in names(vip)) {
  print(plot_density(variable, vis, imcomp))
}

#2.4 Multicolinearity (for Analysis 2)
imcomp <- imcomp[, !(names(imcomp) %in% c("agea", "gndr"))]
cormat <- cor(imcomp)                                                           #Correlation Matrix
print(cormat[abs(cormat) > 0.70 & cormat != 1])

vif_values <- numeric(length(imcomp))
for (i in 1:length(imcomp)) {
  lm_model <- lm(imcomp[, i] ~ ., data = imcomp[, -i])
  vif_values[i] <- 1 / (1 - summary(lm_model)$r.squared)
}
print(vif_values)                                                               #Variance Inflation Factors 

#2.5 Factorability (For Analysis 2)
cor <- imcomp %>% dplyr::select(brncntr, facntr, mocntr, atchctr, ipeqopt, 
                                ipfrule, ipudrst, iphlppl, ipbhprp, imptrad, 
                                lrnobed, rlgdgr, 
                                rlgatnd, pray, imsmetn, imdfetn, impcntr, 
                                imbgeco, imueclt, 
                                imwbcnt) %>% cor()                              #Correlation Matrix
describe(imcomp)
btest1 <- cortest.bartlett(cor, n = 37611)
print(btest1)                                                                   #Bartlett's test of Sphericity

kmo <- KMO(imcomp)
print(kmo)                                                                      #Kaiser-Meyer-Olkin Values

imcomp <- imcomp[, !(names(imcomp) %in% c("atchctr"))]                       
imcom_scaled <- as.data.frame(scale(imcomp))                                    #Z-transforming once more

#2.6 EFA (For analysis 2)
library(EFA.dimensions)
resEFA <- EFA.dimensions::EFA(imcom_scaled, extraction = 'paf', 
                              corkind = 'pearson', Nfactors = 6, iterpaf = 1000,
                              rotation='oblimin', verbose = TRUE)
print(resEFA)                                                                   #Exploratory Factor Analysis results

#2.7 Factor retention (For analysis 2)
library(EFAtools)
par(mfrow=c(1,1))
SCREE(imcom_scaled, corkind = "pearson", Ncases = NULL, verbose = TRUE)         #Scree plot

pa_result <- PARALLEL(                                                          #Paralell Analysis
  x = imcom_scaled,
  n_datasets = 1000,
  eigen_type = "EFA",
  cor_method = "pearson",
  decision_rule = 
)
print(pa_result)
pa_result$eigenvalues_EFA

mapr <- MAP(imcom_scaled, corkind = "pearson", verbose = TRUE)
print(mapr)                                                                     #Minimum Average Partial

#2.8 Subsetting Datasets Norway, Finland, Italy and Hungary
df_no <- subset(imcom, cntry == "NO")
df_no <- subset(df_no, !(brncntr == 0 | mocntr == 0 | facntr == 0))             #Removing immigrants
df_no <- subset(df_no, !(rlgdnm == 5 | rlgdnm == 6 | rlgdnm == 7 | rlgdnm == 8))#Only keeping Christians and non-believers
df_no <- df_no[, !(names(df_no) %in% c("cntry"))]

df_fin <- subset(imcom, cntry == "FI")
df_fin <- subset(df_fin, !(brncntr == 0 | mocntr == 0 | facntr == 0))
df_fin <- subset(df_fin, !(rlgdnm == 5 | rlgdnm == 6 | rlgdnm == 7 | rlgdnm == 8))
df_fin <- df_fin[, !(names(df_fin) %in% c("cntry"))]

df_it <- subset(imcom, cntry == "IT")
df_it <- subset(df_it, !(brncntr == 0 | mocntr == 0 | facntr == 0))
df_it <- subset(df_it, !(rlgdnm == 5 | rlgdnm == 6 | rlgdnm == 7 | rlgdnm == 8))
df_it <- df_it[, !(names(df_it) %in% c("cntry"))]

df_hu <- subset(imcom, cntry == "HU")
df_hu <- subset(df_hu, !(brncntr == 0 | mocntr == 0 | facntr == 0))
df_hu <- subset(df_hu, !(rlgdnm == 5 | rlgdnm == 6 | rlgdnm == 7 | rlgdnm == 8))
df_hu <- df_hu[, !(names(df_hu) %in% c("cntry"))]

dfno_scaled <- as.data.frame(scale(df_no))                                      #Z-transforming once more
dffin_scaled <- as.data.frame(scale(df_fin))
dfhu_scaled <- as.data.frame(scale(df_hu))
dfit_scaled <- as.data.frame(scale(df_it))

#2.9 Structural Equation Modelling (Analysis 2)
library(lavaan)
library(semTools)

base.model <- '                                                                 #Defining the Base model
    # Define factor for ethnic attitudes
    etti =~ imsmetn + imdfetn + impcntr

    # Define factor for threat attitudes
    ttti =~ imueclt + imbgeco + imwbcnt
    
    # Define factor religiousness
    rel =~ pray + rlgatnd + rlgdgr
    
    # Regresions 
    etti ~ rel 
    ttti ~ rel 
'

sem.model <- '                                                                  #Defining the full model
    # Define factor for ethnic attitudes
    etti =~ imsmetn + imdfetn + impcntr

    # Define factor for threat attitudes
    ttti =~ imueclt + imbgeco + imwbcnt
    
    # Define factor religiousness
    rel =~ pray + rlgatnd + rlgdgr
    
    # Define factor identity
    iden =~ atchctr
    
    # Define factor human values 
    trad =~ imptrad + lrnobed + ipbhprp + ipfrule  
    pos  =~ ipudrst + ipeqopt + iphlppl
    
    # Regressions with control variables
    ttti ~ rel + trad + pos + iden 
    etti ~ rel + trad + pos + iden 
    pos ~ rel 
    trad ~ rel
    iden ~ rel 
'

#2.9.1 SEM NORWAY
set.seed(42069)
baseno.fit <- sem(base.model, data=dfno_scaled, se = "bootstrap", bootstrap = 1000, rotation = "varimax")
summary(baseno.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)             #Base model Fit
parameterestimates(baseno.fit, ci = TRUE, level = 0.9)

no.fit <- sem(sem.model, data=dfno_scaled, se = "bootstrap", bootstrap = 1000, rotation = "varimax")
summary(no.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)                 #Full model Fit 
parameterestimates(no.fit, ci = TRUE, level = 0.9)                          
reliability(no.fit)

#2.9.2 SEM FINLAND
basefin.fit <- sem(base.model, data=dffin_scaled, se = "bootstrap", bootstrap = 1000, rotation = "varimax")
summary(basefin.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)            #Base model fit
parameterestimates(basefin.fit, ci = TRUE, level = 0.9)

fin.fit <- sem(sem.model, data=dffin_scaled, se = "bootstrap", bootstrap = 1000, rotation = "varimax")
summary(fin.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)                #Full model fit
parameterestimates(fin.fit, ci = TRUE, level = 0.9)

#2.9.3 SEM HUNGARY
basehu.fit <- sem(base.model, data=dfhu_scaled, se = "bootstrap", bootstrap = 1000, rotation = "varimax")
summary(basehu.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)             #Base model fit
parameterestimates(basehu.fit, ci = TRUE, level = 0.9)

hu.fit <- sem(sem.model, data=dfhu_scaled, se = "bootstrap", bootstrap = 1000, rotation = "varimax")
summary(hu.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)                 #Full model fit
parameterestimates(hu.fit, ci = TRUE, level = 0.9)

#2.9.4 SEM ITALY
baseit.fit <- sem(base.model, data=dfit_scaled, se = "bootstrap", bootstrap = 1000, rotation = "varimax")
summary(baseit.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)             #Base model fit
parameterestimates(baseit.fit, ci = TRUE, level = 0.9)

it.fit <- sem(sem.model, data=dfit_scaled, se = "bootstrap", bootstrap = 1000, rotation = "varimax")
summary(it.fit, fit.measures = TRUE, ci = TRUE, rsquare = TRUE)                 #Full model fit 
parameterestimates(it.fit, ci = TRUE, level = 0.9)


