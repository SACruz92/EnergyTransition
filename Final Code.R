##load libraries

library(plm)
library(tvReg)
library(lpirfs)
library(dplyr)
library(tseries)
library(xlsx)
library(devtools)
library(hpfilter)
library(neverhpfilter)
library(lmtest)
library(ggplot2)
library(seasonal)
library(stats)
library(spatialreg)
library(multiwayvcov)
library(sandwich)
library(spdep)
library(keras)
library(spdep)
library(sp)
library(xtable)



setwd("D:/Users/SCruz/Desktop/Paper1_vfinal/Output R")

Dados <- read.csv("Input_vfinal.csv", header = TRUE, sep = ";", dec = ",")

Dados$Consumo_Total_Pc <- log((Dados$Consumo_Agricultura+Dados$Consumo_Industria + Dados$Consumo_Domestico + Dados$Consumo_Servicos)/Dados$POP)
Dados$VAB_Total_Pc <- log((Dados$VAB_Agricultura+Dados$VAB_Industria + Dados$VAB_Servicos)/Dados$POP)



Dados$Consumo_Servicos <- log(Dados$Consumo_Servicos)
Dados$Consumo_Domestico_Pc <- log(Dados$Consumo_Domestico/Dados$POP)
Dados$CDD <- log(Dados$CDD)
Dados$HDD <- log(Dados$HDD)
Dados$DA <- log(Dados$DA)
Dados$DB <- log(Dados$DB)
Dados$DC <- log(Dados$DC)
Dados$DD <- log(Dados$DD)
Dados$DE <- log(Dados$DE)
Dados$IA <- log(Dados$IA)
Dados$IB <- log(Dados$IB)
Dados$IC <- log(Dados$IC)
Dados$ID <- log(Dados$ID)
Dados$IE <- log(Dados$IE)
Dados$D1 <- log(Dados$D1)
Dados$D2 <- log(Dados$D2)
Dados$D3 <- log(Dados$D3)
Dados$I1 <- log(Dados$I1)
Dados$I2 <- log(Dados$I2)
Dados$I3 <- log(Dados$I3)
Dados$I4 <- log(Dados$I4)
Dados$I5 <- log(Dados$I5)

Dados$VAB_Servicos <- log(Dados$VAB_Servicos)
Dados$Salarios <- log(Dados$Salarios)



###############################################################################################################
###############################################################################################################
############################################### Modelo Doméstico ###############################################
###############################################################################################################
###############################################################################################################


Dados_Doméstico <- Dados[,c("Regiao","Ano", "Consumo_Domestico_Pc", "CDD" ,"HDD", "DB","Salarios","D1", "POP")]

Var_fd <- c("Consumo_Domestico_Pc", "CDD" ,"HDD", "DB","Salarios","D1", "POP")

Dados_Doméstico_fd <- Dados_Doméstico %>%
  group_by(Regiao) %>%
  mutate(across(all_of(Var_fd), ~ . - lag(.)))

Dados_Doméstico_fd <- slice(Dados_Doméstico_fd, -1)



Modelo_Dom <- plm(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1
                  , data = Dados_Doméstico_fd,index = c("Regiao", "Ano"), model = c("random"))

Modelo_Dom_FE <- plm(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1 
                  , data = Dados_Doméstico_fd,index = c("Regiao", "Ano"), model = c("within"))


phtest(Modelo_Dom_FE,Modelo_Dom)

summary(Modelo_Dom)

Modelo_Dom_TVP <- tvPLM(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1
                        , data = Dados_Doméstico_fd,index = c("Regiao", "Ano"), method = c("random"),  tkernel ="Gaussian"
                        )


in_sample_predictions <- fitted(Modelo_Dom_TVP)


actual_values <- Dados_Doméstico_fd$Consumo_Domestico_Pc

# Calculate MSE
mse <- mean((in_sample_predictions - actual_values)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAE
mae <- mean(abs(in_sample_predictions - actual_values))


n <- length(actual_values)
k <- length(coef(Modelo_Dom_TVP))  # Aproximação: número médio de coeficientes não nulos

logL <- -n/2 * (log(2*pi) + log(mse) + 1)

AIC_Dom <- 2 * k - 2 * logL
BIC_Dom <- log(n) * k - 2 * logL


# Print results
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("AIC_Dom:", AIC_Dom))
print(paste("BIC_Dom:", BIC_Dom))


plot_data <- data.frame(Ano = Dados_Doméstico_fd$Ano,
                        Regiao = Dados_Doméstico_fd$Regiao,
                        Actual = actual_values,
                        Predicted = in_sample_predictions)

ggplot(plot_data, aes(x = Ano, group = Regiao)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "In-Sample Fit: Actual vs. Predicted Growth Rates",
       x = "Year", y = "Growth Rate of Domestic Consumption") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red"))

Modelo_Dom <- confint(Modelo_Dom)
Modelo_Dom_TVP <- confint(Modelo_Dom_TVP)

plot(Modelo_Dom_TVP)

Modelo_Dom_TVP
Modelo_Dom
Modelo_Dom$coefficients
Modelo_Dom_TVP$coefficients
Modelo_Dom_TVP$Lower
Modelo_Dom_TVP$Upper


write.xlsx(Modelo_Dom, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Doméstico.xlsx", 
           sheetName = "Folha1", 
           col.names = TRUE, 
           row.names = TRUE,
           append = FALSE)

write.xlsx(Modelo_Dom_TVP$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Doméstico.xlsx", 
           sheetName = "Folha2", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Dom_TVP$Lower, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Doméstico.xlsx", 
           sheetName = "Folha3", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Dom_TVP$Upper, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Doméstico.xlsx", 
           sheetName = "Folha4", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)


# Create the break dummy variables
Dados_Doméstico_fd$D_2008_before <- ifelse(Dados_Doméstico_fd$Ano < 2012, 1, 0)
Dados_Doméstico_fd$D_2012_after <- ifelse(Dados_Doméstico_fd$Ano >= 2012, 1, 0)

Dados_Doméstico_fd$Wages_D_2008_before <- Dados_Doméstico_fd$Salarios * Dados_Doméstico_fd$D_2008_before
Dados_Doméstico_fd$Wages_D_2012_after <- Dados_Doméstico_fd$Salarios * Dados_Doméstico_fd$D_2012_after

# Running the regression with interaction terms for Salarios
modelo_interactions <- plm(Consumo_Domestico_Pc ~ CDD + HDD  + DB + D1 + 
                             Dados_Doméstico_fd$Wages_D_2008_before + Dados_Doméstico_fd$Wages_D_2012_after -1 , 
                           data = Dados_Doméstico_fd, model = "random")

# View the regression results
summary(modelo_interactions)



# Define rolling window parameters
min_year <- min(Dados_Doméstico_fd$Ano)
max_year <- max(Dados_Doméstico_fd$Ano)

results <- data.frame(Ano = integer(), coef_CDD = numeric(), coef_HDD = numeric(),
                      coef_Salarios = numeric(), coef_DB = numeric(), coef_D1 = numeric())

for (end_year in seq(min_year + 8, max_year, by = 1)) {
  sub_data <- subset(Dados_Doméstico_fd, Ano <= end_year)
  
  modelo_temp <- try(plm(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1 -1,
                         data = sub_data, model = "random"), silent = TRUE)
  
  if (!inherits(modelo_temp, "try-error")) {
    coefs <- coef(modelo_temp)
    results <- rbind(results, data.frame(
      Ano = end_year,
      coef_CDD = ifelse("CDD" %in% names(coefs), coefs["CDD"], NA),
      coef_HDD = ifelse("HDD" %in% names(coefs), coefs["HDD"], NA),
      coef_Salarios = ifelse("Salarios" %in% names(coefs), coefs["Salarios"], NA),
      coef_DB = ifelse("DB" %in% names(coefs), coefs["DB"], NA),
      coef_D1 = ifelse("D1" %in% names(coefs), coefs["D1"], NA)
    ))
  }
}

print(results)

ggplot(results, aes(x = Ano, y = coef_Salarios)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1)) +  # Força o eixo x entre 0 e 1
  labs(title = "Wages Coefficient Evolution Over Expanding Windows",
       x = "Year", y = "Coefficient") +
  theme_minimal()








###############################################################################################################
###############################################################################################################
############################################### Modelo Serviços ###############################################
###############################################################################################################
###############################################################################################################

Dados_Servicos <- Dados[,c("Regiao","Ano", "Consumo_Servicos","CDD", "HDD", "DD","VAB_Servicos", "D2")]

Var_fd <- c("Consumo_Servicos","CDD", "HDD", "DD","VAB_Servicos","D2")

Dados_Servicos_fd <- Dados_Servicos %>%
  group_by(Regiao) %>%
  mutate(across(all_of(Var_fd), ~ . - lag(.)))

Dados_Servicos_fd <- slice(Dados_Servicos_fd, -1)



Modelo_Servicos <- plm(Consumo_Servicos ~ CDD + HDD + VAB_Servicos + DD + D2
                       , data = Dados_Servicos_fd,index = c("Regiao", "Ano"), model = c("random"))

Modelo_Servicos_FE <- plm(Consumo_Servicos ~ CDD + HDD + VAB_Servicos + DD + D2
                          , data = Dados_Servicos_fd,index = c("Regiao", "Ano"), model = c("within"))

phtest(Modelo_Servicos_FE,Modelo_Servicos)

summary(Modelo_Servicos)


# Create the break dummy variables
Dados_Servicos_fd$D_2006_before <- ifelse(Dados_Servicos_fd$Ano <= 2006, 1, 0)
Dados_Servicos_fd$D_2006_after <- ifelse(Dados_Servicos_fd$Ano > 2006, 1, 0)

# Create interaction terms with Salarios
Dados_Servicos_fd$GVA_Services_D_2006_before <- Dados_Servicos_fd$VAB_Servicos * Dados_Servicos_fd$D_2006_before
Dados_Servicos_fd$GVA_Services_D_2006_after <- Dados_Servicos_fd$VAB_Servicos * Dados_Servicos_fd$D_2006_after

# Running the regression with interaction terms for Salarios
modelo_interactions <- plm(Consumo_Servicos ~ CDD + HDD  + DD + D2 + 
                             GVA_Services_D_2006_before + GVA_Services_D_2006_after -1 , 
                           data = Dados_Servicos_fd, model = "random")

# View the regression results
summary(modelo_interactions)



# Define rolling window parameters
min_year <- min(Dados_Servicos_fd$Ano)  # Earliest year in the dataset
max_year <- max(Dados_Servicos_fd$Ano)  # Latest year in the dataset

# Inicializa o data.frame 'results' com as colunas corretas
results <- data.frame(Ano = numeric(0), 
                      coef_CDD = numeric(0),
                      coef_HDD = numeric(0),
                      coef_VAB_Servicos = numeric(0),
                      coef_DD = numeric(0),
                      coef_D2 = numeric(0))





# Expanding window loop
for (end_year in seq(min_year + 8, max_year, by = 1)) {  # Start with 5-year minimum window
  sub_data <- subset(Dados_Servicos_fd, Ano <= end_year)  # Select data up to end_year
  
  # Estimate the model
  modelo_temp <- plm(Consumo_Servicos ~ CDD + HDD + VAB_Servicos + DD + D2 -1,
                     data = sub_data, model = "random")
  
  # Store coefficient estimates for each expanding window
  results <- rbind(results, data.frame(Ano = end_year,
                                       coef_CDD = coef(modelo_temp)["CDD"],
                                       coef_HDD = coef(modelo_temp)["HDD"],
                                       coef_VAB_Servicos = coef(modelo_temp)["VAB_Servicos"],
                                       coef_DD = coef(modelo_temp)["DD"],
                                       coef_D2 = coef(modelo_temp)["D2"]))
}

# Plot the evolution of coefficients
ggplot(results, aes(x = Ano)) +
  geom_line(aes(y = coef_VAB_Servicos, color = "VAB_Servicos")) +
  labs(title = "Coefficient Evolution Over Expanding Windows",
       x = "Year", y = "Coefficient Value") +
  theme_minimal()



Modelo_Servicos_TVP <- tvPLM(Consumo_Servicos ~ CDD +  HDD + VAB_Servicos + DD + D2
                             , data = Dados_Servicos_fd,index = c("Regiao", "Ano"), method = c("random") , tkernel = "Gaussian")


Modelo_Servicos <- confint(Modelo_Servicos)
Modelo_Servicos_TVP <- confint(Modelo_Servicos_TVP)


in_sample_predictions <- fitted(Modelo_Servicos_TVP)


actual_values <- Dados_Servicos_fd$Consumo_Servicos

# Calculate MSE
mse <- mean((in_sample_predictions - actual_values)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAE
mae <- mean(abs(in_sample_predictions - actual_values))

# Calculate MAPE
mape <- mean(abs((actual_values - in_sample_predictions) / actual_values)) * 100


n <- length(actual_values)
k <- length(coef(Modelo_Servicos_TVP))  # Aproximação: número médio de coeficientes não nulos

logL <- -n/2 * (log(2*pi) + log(mse) + 1)

AIC_Ser <- 2 * k - 2 * logL
BIC_Ser <- log(n) * k - 2 * logL

# Print results
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("MAPE:", mape))
print(paste("AIC_Ser:", AIC_Ser))
print(paste("BIC_Ser:", BIC_Ser))


plot(Modelo_Servicos_TVP)

Modelo_Servicos
Modelo_Servicos$coefficients
Modelo_Servicos_TVP$coefficients
Modelo_Servicos_TVP$Lower
Modelo_Servicos_TVP$Upper

write.xlsx(Modelo_Servicos, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Serviços.xlsx", 
           sheetName = "Folha1", 
           col.names = TRUE, 
           row.names = TRUE,
           append = FALSE)

write.xlsx(Modelo_Servicos_TVP$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Serviços.xlsx", 
           sheetName = "Folha2", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Servicos_TVP$Lower, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Serviços.xlsx", 
           sheetName = "Folha3", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Servicos_TVP$Upper, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Serviços.xlsx", 
           sheetName = "Folha4", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

###############################################################################################################
###############################################################################################################
############################################### Modelo Industria ###############################################
###############################################################################################################
###############################################################################################################

Dados_Industria <- Dados[,c("Regiao","Ano", "Consumo_Industria","POP","CDD", "HDD", "IC","I3", "VAB_Industria")]
Dados_Industria$Consumo_Industria <- log(Dados_Industria$Consumo_Industria)
Dados_Industria$VAB_Industria<- log(Dados_Industria$VAB_Industria)

Var_fd <- c("Consumo_Industria","CDD", "HDD", "IC","I3","VAB_Industria")

Dados_Industria_fd <- Dados_Industria %>%
  group_by(Regiao) %>%
  mutate(across(all_of(Var_fd), ~ . - lag(.)))


Dados_Industria_fd <- slice(Dados_Industria_fd, -1)


Modelo_Industria <- plm(Consumo_Industria ~ VAB_Industria + IC + I3
                        , data = Dados_Industria_fd,index = c("Regiao", "Ano"), method = c("random"))

summary(Modelo_Industria)

Modelo_Industria_TVP <- tvPLM(Consumo_Industria ~ VAB_Industria + IC + I3
                              , data = Dados_Industria_fd,index = c("Regiao", "Ano"), method = c("random"), tkernel = "Gaussian")

Modelo_Industria_TVP$coefficients

Modelo_Industria <- confint(Modelo_Industria)
Modelo_Industria_TVP <- confint(Modelo_Industria_TVP)

plot(Modelo_Industria_TVP)

Modelo_Industria
Modelo_Industria$coefficients
Modelo_Industria_TVP$coefficients
Modelo_Industria_TVP$Lower
Modelo_Industria_TVP$Upper



# Create the break dummy variables
Dados_Industria_fd$D_2001_before <- ifelse(Dados_Industria_fd$Ano <= 2001, 1, 0)
Dados_Industria_fd$D_2001_after <- ifelse(Dados_Industria_fd$Ano > 2001, 1, 0)

# Create interaction terms with Salarios
Dados_Industria_fd$GVA_Industria_D_2001_before <- Dados_Industria_fd$VAB_Industria * Dados_Industria_fd$D_2001_before
Dados_Industria_fd$GVA_Industria_D_2001_after <- Dados_Industria_fd$VAB_Industria * Dados_Industria_fd$D_2001_after

# Running the regression with interaction terms for Salarios
modelo_interactions <- plm(Consumo_Industria ~ IC + I3 + 
                             GVA_Industria_D_2001_before + GVA_Industria_D_2001_after -1, 
                           data = Dados_Industria_fd, model = "random")

# View the regression results
summary(modelo_interactions)


# Define rolling window parameters
min_year <- min(Dados_Industria_fd$Ano)  # Earliest year in the dataset
max_year <- max(Dados_Industria_fd$Ano)  # Latest year in the dataset

# Inicializa o data.frame 'results' com as colunas corretas
results <- data.frame(Ano = numeric(0), 
                      coef_VAB_Industria = numeric(0),
                      coef_IC = numeric(0),
                      coef_I3 = numeric(0))

# Expanding window loop
for (end_year in seq(min_year + 8, max_year, by = 1)) {  # Start with 5-year minimum window
  sub_data <- subset(Dados_Industria_fd, Ano <= end_year)  # Select data up to end_year
  
  # Estimate the model
  modelo_temp <- plm(Consumo_Industria ~ VAB_Industria + IC + I3 -1,
                     data = sub_data, model = "random")
  
  # Store coefficient estimates for each expanding window
  results <- rbind(results, data.frame(Ano = end_year,
                                       coef_VAB_Industria = coef(modelo_temp)["VAB_Industria"],
                                       coef_IC = coef(modelo_temp)["IC"],
                                       coef_I3 = coef(modelo_temp)["I3"]))
}

# Plot the evolution of coefficients
ggplot(results, aes(x = Ano)) +
  geom_line(aes(y = coef_VAB_Industria)) +
  labs(title = "Industry's Coefficient Evolution Over Expanding Windows",
       x = "Year", y = "Coefficient Value") +
  theme_minimal()


in_sample_predictions <- fitted(Modelo_Industria_TVP)


actual_values <- Dados_Industria_fd$Consumo_Industria

# Calculate MSE
mse <- mean((in_sample_predictions - actual_values)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Calculate MAE
mae <- mean(abs(in_sample_predictions - actual_values))

# Calculate MAPE
mape <- mean(abs((actual_values - in_sample_predictions) / actual_values)) * 100


n <- length(actual_values)
k <- length(coef(Modelo_Industria_TVP))  # Aproximação: número médio de coeficientes não nulos

logL <- -n/2 * (log(2*pi) + log(mse) + 1)

AIC_Ser <- 2 * k - 2 * logL
BIC_Ser <- log(n) * k - 2 * logL

# Print results
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("MAPE:", mape))
print(paste("AIC_Ser:", AIC_Ser))
print(paste("BIC_Ser:", BIC_Ser))
            
            
write.xlsx(Modelo_Industria,
file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Industria.xlsx", 
sheetName = "Folha1", 
col.names = TRUE, 
row.names = TRUE,
append = FALSE)
            
write.xlsx(Modelo_Industria_TVP$coefficients, 
file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Industria.xlsx", 
sheetName = "Folha2", 
col.names = TRUE, 
row.names = TRUE,
append = TRUE)
            
write.xlsx(Modelo_Industria_TVP$Lower, 
file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Industria.xlsx", 
sheetName = "Folha3", 
col.names = TRUE, 
row.names = TRUE,
append = TRUE)
            
write.xlsx(Modelo_Industria_TVP$Upper, 
file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Industria.xlsx", 
sheetName = "Folha4", 
col.names = TRUE, 
row.names = TRUE,
append = TRUE)
            
            
          
            
            ###############################################################################################################
            ###############################################################################################################
            ############################################### Modelo Total ##################################################
            ###############################################################################################################
            ###############################################################################################################
            
            Dados_Total <- Dados[,c("Regiao","Ano", "Consumo_Total_Pc","CDD", "HDD","VAB_Total_Pc", "DC","IC","D2","I2")]
            
            Var_fd <- c("Consumo_Total_Pc","CDD", "HDD","VAB_Total_Pc", "DC","IC","D2","I2")
            
            Dados_Total_fd <- Dados_Total %>%
              group_by(Regiao) %>%
              mutate(across(all_of(Var_fd), ~ . - lag(.)))
            
            
            Dados_Total_fd <- slice(Dados_Total_fd, -1)
            
            Modelo_Total <- plm(Consumo_Total_Pc ~ CDD + HDD + VAB_Total_Pc + DC + D2 + IC + I2
                                , data = Dados_Total_fd,index = c("Regiao", "Ano"), method = c("random"))
            
            
            summary(Modelo_Total)
            
      
            # Create the break dummy variables
            Dados_Total_fd$D_2012_before <- ifelse(Dados_Total_fd$Ano <= 2012, 1, 0)
            Dados_Total_fd$D_2012_after <- ifelse(Dados_Total_fd$Ano > 2012, 1, 0)
            
            # Create interaction terms with Salarios
            Dados_Total_fd$GVA_Total_D_2012_before <- Dados_Total_fd$VAB_Total_Pc * Dados_Total_fd$D_2012_before
            Dados_Total_fd$GVA_Total_D_2012_after <- Dados_Total_fd$VAB_Total_Pc* Dados_Total_fd$D_2012_after
            
            # Running the regression with interaction terms for Total
            modelo_interactions <- plm(Consumo_Total_Pc ~ CDD + HDD + DC + D2 + IC + I2 +
                                         GVA_Total_D_2012_before + GVA_Total_D_2012_after + -1, 
                                       data = Dados_Total_fd, model = "random")
            
            # View the regression results
            summary(modelo_interactions)
            
            
            
            
            
            
          
            
            Modelo_Total_TVP <- tvPLM(Consumo_Total_Pc ~ CDD + HDD + VAB_Total_Pc + DC + D2 + IC + I2
                                      , data = Dados_Total_fd,index = c("Regiao", "Ano"), method = c("random"), tkernel = "Gaussian")
            
            
            Modelo_Total
            
            Modelo_Total <- confint(Modelo_Total)
            Modelo_Total_TVP <- confint(Modelo_Total_TVP)
            
            plot(Modelo_Total_TVP)
            
            Modelo_Total
            Modelo_Total$coefficients
            Modelo_Total_TVP$coefficients
            Modelo_Total_TVP$Lower
            Modelo_Total_TVP$Upper
            
    
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            # Define rolling window parameters
            min_year <- min(Dados_Total_fd$Ano)  # Earliest year in the dataset
            max_year <- max(Dados_Total_fd$Ano)  # Latest year in the dataset
            
            # Inicializa o data.frame 'results' com as colunas corretas
            results <- data.frame(Ano = numeric(0), 
                                  coef_CDD = numeric(0),
                                  coef_HDD = numeric(0),
                                  coef_VAB_Total_Pc = numeric(0),
                                  coef_DC = numeric(0),
                                  coef_D2 = numeric(0),
                                  coef_IC = numeric(0),
                                  coef_I2 = numeric(0))
            
            # Expanding window loop
            for (end_year in seq(min_year + , max_year, by = 1)) {  # Start with 5-year minimum window
              sub_data <- subset(Dados_Total_fd, Ano <= end_year)  # Select data up to end_year
              
              # Estimate the model
              modelo_temp <- plm(Consumo_Total_Pc ~ CDD + HDD + VAB_Total_Pc + DC + D2 + IC + I2 -1 ,
                                 data = sub_data, model = "random")
              
              # Store coefficient estimates for each expanding window
              results <- rbind(results, data.frame(Ano = end_year,
                                                   coef_CDD = coef(modelo_temp)["CDD"],
                                                   coef_HDD = coef(modelo_temp)["HDD"],
                                                   coef_VAB_Total_Pc = coef(modelo_temp)["VAB_Total_Pc"],
                                                   coef_DC = coef(modelo_temp)["DC"],
                                                   coef_D2 = coef(modelo_temp)["D2"],
                                                   coef_IC = coef(modelo_temp)["IC"], 
                                                   coef_I2 = coef(modelo_temp)["I2"])
              )
            }
            
            # Plot the evolution of coefficients
            ggplot(results, aes(x = Ano)) +
              geom_line(aes(y = coef_VAB_Total_Pc, color = "VAB_Total_Pc", group = 1)) +
              labs(title = "Coefficient Evolution Over Expanding Windows", x = "Year", y = "Coefficient Value") +
              theme_minimal()
            
            
            summary(Modelo_Total_TVP)
            
            in_sample_predictions <- fitted(Modelo_Total_TVP)
            
            actual_values <- Dados_Total_fd$Consumo_Total_Pc
            
            # Calculate MSE
            mse <- mean((in_sample_predictions - actual_values)^2)
            
            # Calculate RMSE
            rmse <- sqrt(mse)
            
            # Calculate MAE
            mae <- mean(abs(in_sample_predictions - actual_values))
            
            # Calculate MAPE
            mape <- mean(abs((actual_values - in_sample_predictions) / actual_values)) * 100
            
            
            n <- length(actual_values)
            k <- length(coef(Modelo_Total_TVP))  # Aproximação: número médio de coeficientes não nulos
            
            logL <- -n/2 * (log(2*pi) + log(mse) + 1)
            
            AIC_Ser <- 2 * k - 2 * logL
            BIC_Ser <- log(n) * k - 2 * logL
            
            # Print results
            print(paste("MSE:", mse))
            print(paste("RMSE:", rmse))
            print(paste("MAE:", mae))
            print(paste("MAPE:", mape))
            print(paste("AIC_Ser:", AIC_Ser))
            print(paste("BIC_Ser:", BIC_Ser))
                        
                        
                        write.xlsx(Modelo_Total, 
                                   file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Total.xlsx", 
                                   sheetName = "Folha1", 
                                   col.names = TRUE, 
                                   row.names = TRUE,
                                   append = FALSE)
                        
                        write.xlsx(Modelo_Total_TVP$coefficients, 
                                   file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Total.xlsx", 
                                   sheetName = "Folha2", 
                                   col.names = TRUE, 
                                   row.names = TRUE,
                                   append = TRUE)
                        
                        write.xlsx(Modelo_Total_TVP$Lower, 
                                   file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Total.xlsx", 
                                   sheetName = "Folha3", 
                                   col.names = TRUE, 
                                   row.names = TRUE,
                                   append = TRUE)
                        
                        write.xlsx(Modelo_Total_TVP$Upper, 
                                   file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/Modelo_Total.xlsx", 
                                   sheetName = "Folha4", 
                                   col.names = TRUE, 
                                   row.names = TRUE,
                                   append = TRUE)
                        
                        
                        
                        ##############################################################################################################
                        ##############################################################################################################
                        ########################################## Unit Root Tests ###################################################
                        ##############################################################################################################
                        ##############################################################################################################
                        
                        UR_Consumo_Doméstico <- data.frame(split(Dados_Doméstico_fd$Consumo_Domestico_Pc, Dados_Doméstico_fd$Regiao))
                        purtest(UR_Consumo_Doméstico, test = "levinlin", lags= 1)
                        
                        UR_Consumo_Servicos <- data.frame(split(Dados_Servicos_fd$Consumo_Servicos, Dados_Servicos_fd$Regiao))
                        purtest(UR_Consumo_Servicos, test = "levinlin", lags= 1)
                        
                        UR_Consumo_Industria <- data.frame(split(Dados_Industria_fd$Consumo_Industria, Dados_Industria_fd$Regiao))
                        purtest(UR_Consumo_Industria, test = "levinlin", lags= 1)
                        
                        UR_Consumo_Total_Pc<- data.frame(split(Dados_Total_fd$Consumo_Total_Pc, Dados_Total_fd$Regiao))
                        purtest(UR_Consumo_Total_Pc, test = "levinlin", lags= 1)
                        
                        
                        
                        
                        UR_Salarios <- data.frame(split(Dados_Doméstico_fd$Salarios, Dados_Doméstico_fd$Regiao))
                        purtest(UR_Salarios, test = "levinlin", lags= 1)
                        
                        UR_GVA_S <- data.frame(split(Dados_Servicos_fd$VAB_Servicos, Dados_Servicos_fd$Regiao))
                        purtest(UR_GVA_S , test = "levinlin", lags= 1)
                        
                        UR_GVA_I <- data.frame(split(Dados_Industria_fd$VAB_Industria, Dados_Industria_fd$Regiao))
                        purtest(UR_GVA_I , test = "levinlin", lags= 1)
                        
                        UR_CDD <- data.frame(split(Dados_Industria_fd$CDD, Dados_Industria_fd$Regiao))
                        purtest(UR_CDD  , test = "levinlin", lags= 1)
                        
                        UR_HDD <- data.frame(split(Dados_Industria_fd$HDD, Dados_Industria_fd$Regiao))
                        purtest(UR_HDD  , test = "levinlin", lags= 1)
                        
                        UR_DB <- data.frame(split(Dados_Doméstico_fd$DB, Dados_Doméstico_fd$Regiao))
                        purtest(UR_DB  , test = "levinlin", lags= 1)
                        
                        UR_DC <- data.frame(split(Dados_Total_fd$DC, Dados_Total_fd$Regiao))
                        purtest(UR_DB  , test = "levinlin", lags= 1)
                        
                        UR_DD <- data.frame(split(Dados_Servicos_fd$DD, Dados_Servicos_fd$Regiao))
                        purtest(UR_DD  , test = "levinlin", lags= 1)

                        UR_IC <- data.frame(split(Dados_Total_fd$IC, Dados_Total_fd$Regiao))
                        purtest(UR_IC  , test = "levinlin", lags= 1)
                        
                        UR_D1 <- data.frame(split(Dados_Doméstico_fd$D1, Dados_Doméstico_fd$Regiao))
                        purtest(UR_D1  , test = "levinlin", lags= 1)
                        
                        UR_D2 <- data.frame(split(Dados_Servicos_fd$D2, Dados_Servicos_fd$Regiao))
                        purtest(UR_D2  , test = "levinlin", lags= 1)

                        
                        UR_I2 <- data.frame(split(Dados_Industria_fd$I3, Dados_Industria_fd$Regiao))
                        purtest(UR_I2  , test = "levinlin", lags= 1)
                        
                        
                        ##############################################################################################################
                        ##############################################################################################################
                        ########################################## Hausman Tests   ###################################################
                        ##############################################################################################################
                        ##############################################################################################################
                        
                        
                        Modelo_Dom <- plm(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1
                                          , data = Dados_Doméstico_fd,index = c("Regiao", "Ano"), model = c("random"))
                        
                        Modelo_Dom_FE <- plm(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1 
                                             , data = Dados_Doméstico_fd,index = c("Regiao", "Ano"), model = c("within"))
                        
                        phtest(Modelo_Dom_FE,Modelo_Dom)
                        
                        
                        Modelo_Servicos <- plm(Consumo_Servicos ~ CDD + HDD + VAB_Servicos + DD + D2
                                               , data = Dados_Servicos_fd,index = c("Regiao", "Ano"), model = c("random"))
                        
                        Modelo_Servicos_FE <- plm(Consumo_Servicos ~ CDD + HDD + VAB_Servicos + DD + D2
                                                  , data = Dados_Servicos_fd,index = c("Regiao", "Ano"), model = c("within"))
                        
                        phtest(Modelo_Servicos,Modelo_Servicos_FE)

                        
                        
                        
                        Modelo_Industria <- plm(Consumo_Industria ~  VAB_Industria + IC + I3
                                                , data = Dados_Industria_fd,index = c("Regiao", "Ano"), method = c("random"))
                        
                        Modelo_Industria_FE <- plm(Consumo_Industria ~  VAB_Industria + IC + I3
                                                   , data = Dados_Industria_fd,index = c("Regiao", "Ano"), method = c("within"))
                        
                        phtest(Modelo_Industria, Modelo_Industria_FE)
                        
                        
                        
                        Modelo_Total <- plm(Consumo_Total_Pc ~ CDD + HDD + VAB_Total_Pc + DC + D2 + IC + I2
                                            , data = Dados_Total_fd,index = c("Regiao", "Ano"), method = c("random"))
                        
                        Modelo_Total_FE <- plm(Consumo_Total_Pc ~ CDD + HDD + VAB_Total_Pc + DC + D2 + IC + I2
                                               , data = Dados_Total_fd,index = c("Regiao", "Ano"), method = c("within"))
                        
                        phtest(Modelo_Total,Modelo_Total_FE)
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
