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



setwd("D:/Users/SCruz/Desktop/Paper1_vfinal/Output R")

Dados <- read.csv("Input.csv", header = TRUE, sep = ";", dec = ",")




Dados$Consumo_Total_Pc <- log((Dados$Consumo_Agricultura+Dados$Consumo_Industria + Dados$Consumo_Domestico + Dados$Consumo_Servicos)/Dados$POP)
Dados$VAB_Total_Pc <- log((Dados$VAB_Agricultura+Dados$VAB_Industria + Dados$VAB_Servicos)/Dados$POP)



Dados$Consumo_Servicos_Pc <- log(Dados$Consumo_Servicos/Dados$POP)
Dados$Consumo_Domestico_Pc <- log(Dados$Consumo_Domestico/Dados$POP)
Dados$Consumo_AgriInd_Pc <- log((Dados$Consumo_Agricultura+Dados$Consumo_Industria)/Dados$POP)
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

Dados$VAB_Servicos_Pc <- log(Dados$VAB_Servicos/Dados$POP)
Dados$Salarios <- log(Dados$Salarios)
Dados$VAB_AgriInd_Pc <- log((Dados$VAB_Industria+Dados$VAB_Agricultura)/Dados$POP)



### Modelo Doméstico
Dados_Doméstico <- Dados[,c("Regiao","Ano", "Consumo_Domestico_Pc", "CDD" ,"HDD", "DA","DB","DC","Salarios","D1", "D2", "POP")]

Var_fd <- c("Consumo_Domestico_Pc", "CDD" ,"HDD", "DA","DB","DC","Salarios","D1", "D2", "POP")

Dados_Doméstico_fd <- Dados_Doméstico %>%
  group_by(Regiao) %>%
  mutate(across(all_of(Var_fd), ~ . - lag(.)))

Dados_Doméstico_fd <- slice(Dados_Doméstico_fd, -1)



Modelo_Dom_TVP_g <- tvPLM(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1
                          , data = Dados_Doméstico_fd,index = c("Regiao", "Ano"), method = c("pooling"),tkernel ="Gaussian")

Modelo_Dom_TVP_e <- tvPLM(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1
                          , data = Dados_Doméstico_fd,index = c("Regiao", "Ano"), method = c("random"),tkernel ="Gaussian")

Modelo_Dom_TVP_t <- tvPLM(Consumo_Domestico_Pc ~ CDD + HDD + Salarios + DB + D1
                          , data = Dados_Doméstico_fd,index = c("Regiao", "Ano"), method = c("within"),tkernel ="Gaussian")


### Modelo Serviços

Dados_Servicos <- Dados[,c("Regiao","Ano", "Consumo_Servicos_Pc","CDD", "HDD","DC", "DD","DE","VAB_Servicos_Pc","D1", "D2", "D3")]

Var_fd <- c("Consumo_Servicos_Pc","CDD", "HDD","DC", "DD","DE","VAB_Servicos_Pc","D1", "D2", "D3")

Dados_Servicos_fd <- Dados_Servicos %>%
  group_by(Regiao) %>%
  mutate(across(all_of(Var_fd), ~ . - lag(.)))

Dados_Servicos_fd <- slice(Dados_Servicos_fd, -1)


Modelo_Servicos_TVP_g <- tvPLM(Consumo_Servicos_Pc ~   CDD + HDD + VAB_Servicos_Pc + DD + D2
                               , data = Dados_Servicos_fd,index = c("Regiao", "Ano"), method = c("pooling") , tkernel = "Gaussian")

Modelo_Servicos_TVP_e <- tvPLM(Consumo_Servicos_Pc ~   CDD + HDD + VAB_Servicos_Pc + DD + D2
                               , data = Dados_Servicos_fd,index = c("Regiao", "Ano"), method = c("random") , tkernel = "Gaussian")

Modelo_Servicos_TVP_t <- tvPLM(Consumo_Servicos_Pc ~   CDD + HDD + VAB_Servicos_Pc + DD + D2
                               , data = Dados_Servicos_fd,index = c("Regiao", "Ano"), method = c("within") , tkernel = "Gaussian")


### Modelo Agricultura


Dados_Agricultura <- Dados[,c("Regiao","Ano", "Consumo_Agricultura","POP","CDD", "HDD", "DE", "IC", "D3", "I3", "IB","I2", "VAB_Agricultura")]
Dados_Agricultura$Consumo_Agricultura_Pc <- log(Dados_Agricultura$Consumo_Agricultura/Dados_Agricultura$POP )
Dados_Agricultura$VAB_Agricultura_Pc <- log(Dados_Agricultura$VAB_Agricultura/Dados_Agricultura$POP)

Var_fd <- c("Consumo_Agricultura_Pc","POP","CDD", "HDD", "DE", "IC", "D3", "I3", "IB","I2", "VAB_Agricultura_Pc")

Dados_Agricultura_fd <- Dados_Agricultura %>%
  group_by(Regiao) %>%
  mutate(across(all_of(Var_fd), ~ . - lag(.)))


Dados_Agricultura_fd <- slice(Dados_Agricultura_fd, -1)



Modelo_Agricultura_TVP_g <- tvPLM(Consumo_Agricultura_Pc ~ CDD + HDD + VAB_Agricultura_Pc + IB + I2
                                  , data = Dados_Agricultura_fd,index = c("Regiao", "Ano"), method = c("pooling"), tkernel = "Gaussian")

Modelo_Agricultura_TVP_e <- tvPLM(Consumo_Agricultura_Pc ~ CDD + HDD + VAB_Agricultura_Pc + IB + I2
                                  , data = Dados_Agricultura_fd,index = c("Regiao", "Ano"), method = c("random"), tkernel = "Gaussian")

Modelo_Agricultura_TVP_t <- tvPLM(Consumo_Agricultura_Pc ~ CDD + HDD + VAB_Agricultura_Pc + IB + I2
                                  , data = Dados_Agricultura_fd,index = c("Regiao", "Ano"), method = c("within"), tkernel = "Gaussian")


### Modelo Industria


Dados_Industria <- Dados[,c("Regiao","Ano", "Consumo_Industria","POP","CDD", "HDD", "IB", "ID", "IC","I2","I3", "I4", "VAB_Industria")]
Dados_Industria$Consumo_Industria_Pc <- log(Dados_Industria$Consumo_Industria/Dados_Industria$POP)
Dados_Industria$VAB_Industria_Pc <- log(Dados_Industria$VAB_Industria/Dados_Industria$POP)

Var_fd <- c("Consumo_Industria_Pc","CDD", "HDD", "IC","I3","VAB_Industria_Pc")

Dados_Industria_fd <- Dados_Industria %>%
  group_by(Regiao) %>%
  mutate(across(all_of(Var_fd), ~ . - lag(.)))


Dados_Industria_fd <- slice(Dados_Industria_fd, -1)




Modelo_Industria_TVP_g <- tvPLM(Consumo_Industria_Pc ~ CDD + HDD + VAB_Industria_Pc + IC + I3
                                , data = Dados_Industria_fd,index = c("Regiao", "Ano"), method = c("pooling"), tkernel = "Gaussian")

Modelo_Industria_TVP_e <- tvPLM(Consumo_Industria_Pc ~ CDD + HDD + VAB_Industria_Pc + IC + I3
                                , data = Dados_Industria_fd,index = c("Regiao", "Ano"), method = c("random"), tkernel = "Gaussian")

Modelo_Industria_TVP_t <- tvPLM(Consumo_Industria_Pc ~ CDD + HDD + VAB_Industria_Pc + IC + I3
                                , data = Dados_Industria_fd,index = c("Regiao", "Ano"), method = c("within"), tkernel = "Gaussian")




### Modelo Total

Dados_Total <- Dados[,c("Regiao","Ano", "Consumo_Total_Pc","CDD", "HDD","VAB_Total_Pc", "DB","DD","DC","IB","IC", "ID","D1","D3","D2","I2","I1","I3","Salarios")]

Var_fd <- c("Consumo_Total_Pc","CDD", "HDD","VAB_Total_Pc", "DB","DD","DC","IB","IC", "ID","D1","D3","D2","I2","I1","I3","Salarios")

Dados_Total_fd <- Dados_Total %>%
  group_by(Regiao) %>%
  mutate(across(all_of(Var_fd), ~ . - lag(.)))


Dados_Total_fd <- slice(Dados_Total_fd, -1)



Modelo_Total_TVP_g <- tvPLM(Consumo_Total_Pc ~ CDD + HDD + VAB_Total_Pc + DC + D2 + IC + I2
                            , data = Dados_Total_fd,index = c("Regiao", "Ano"), method = c("pooling"), tkernel = "Gaussian" )

Modelo_Total_TVP_e <- tvPLM(Consumo_Total_Pc ~ CDD + HDD + VAB_Total_Pc + DC + D2 + IC + I2
                            , data = Dados_Total_fd,index = c("Regiao", "Ano"), method = c("random"), tkernel = "Gaussian")

Modelo_Total_TVP_t <- tvPLM(Consumo_Total_Pc ~ CDD + HDD + VAB_Total_Pc  + DC + D2 + IC + I2
                            , data = Dados_Total_fd,index = c("Regiao", "Ano"), method = c("within"), tkernel = "Gaussian")


Modelo_Dom_TVP_g$coefficients
Modelo_Dom_TVP_e$coefficients
Modelo_Dom_TVP_t$coefficients
Modelo_Servicos_TVP_g$coefficients
Modelo_Servicos_TVP_e$coefficients
Modelo_Servicos_TVP_t$coefficients
Modelo_Agricultura_TVP_g$coefficients
Modelo_Agricultura_TVP_e$coefficients
Modelo_Agricultura_TVP_t$coefficients
Modelo_Industria_TVP_g$coefficients
Modelo_Industria_TVP_e$coefficients
Modelo_Industria_TVP_t$coefficients
Modelo_Total_TVP_g$coefficients
Modelo_Total_TVP_e$coefficients
Modelo_Total_TVP_t$coefficients


write.xlsx(Modelo_Dom_TVP_g$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha1", 
           col.names = TRUE, 
           row.names = TRUE,
           append = FALSE)

write.xlsx(Modelo_Dom_TVP_e$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha2", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Dom_TVP_t$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha3", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Servicos_TVP_g$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha4", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Servicos_TVP_e$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha5", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Servicos_TVP_t$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha6", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Agricultura_TVP_g$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha7", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Agricultura_TVP_e$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha8", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Agricultura_TVP_t$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha9", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)


write.xlsx(Modelo_Industria_TVP_g$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha10", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Industria_TVP_e$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha11", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Industria_TVP_t$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha12", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)



write.xlsx(Modelo_Total_TVP_g$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha13", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Total_TVP_e$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha14", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)

write.xlsx(Modelo_Total_TVP_t$coefficients, 
           file = "D:/Users/SCruz/Desktop/Paper1_vfinal/Output R/AnaliseSensibilidade_4.xlsx", 
           sheetName = "Folha15", 
           col.names = TRUE, 
           row.names = TRUE,
           append = TRUE)
