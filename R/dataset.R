

###################
### BIBLIOTECAS ###
###################

library(tidyverse)
library(lubridate)
library(rbcb)

###################
### PIB - FOCUS ###
###################

indic_pib <- c('Produ\u00e7\u00e3o industrial', 'PIB Agropecu\u00e1ria', 'PIB Industrial', 'PIB Servi\u00e7os', 'PIB Total')
data_inicio <- paste0(year(Sys.Date())-1,'-01-01')

pib <- data.frame()

for (i in 1:length(indic_pib)){
  temp <- get_market_expectations("annual", indic_pib[i],
                                         start_date = data_inicio)
  pib <- bind_rows(pib,temp)
  
}


pib <- 
  pib %>%
  arrange(indic, desc(date), reference_date) %>%
  distinct(indic, date, reference_date, .keep_all = T)

rm(temp,i,indic_pib,data_inicio)

################
### INFLAÇÃO ###
################

indic_inflacao <- c('IGP-DI','IGP-M','INPC','IPA-DI', 'IPA-M','IPCA', 'IPCA-15','Pre\u00e7os administrados por contrato e monitorados')
data_inicio <- paste0(year(Sys.Date())-1,'-01-01')

inflacao <- data.frame()

for (i in 1:length(indic_inflacao)){
  temp <- rbcb::get_annual_market_expectations(indic_inflacao[i],
                                         start_date = data_inicio)
  inflacao <- bind_rows(inflacao,temp)
  
}

inflacao <- 
  inflacao %>%
  arrange(indic, desc(date), reference_date) %>%
  distinct(indic, date, reference_date, .keep_all = T)

rm(temp,i,indic_inflacao,data_inicio)

######################
### CÂMBIO - FOCUS ###
######################

indic_cambio <- 'Câmbio'
data_inicio <- paste0(year(Sys.Date())-1,'-01-01')

cambio <- get_annual_market_expectations(indic_cambio, start_date = data_inicio)

cambio <- 
  cambio %>%
  arrange(indic, desc(date), reference_date) %>%
  distinct(indic, date, reference_date, .keep_all = T)

rm(indic_cambio, data_inicio)

#####################
### SELIC - FOCUS ###
#####################

indic_selic <- 'Selic'
data_inicio <- paste0(year(Sys.Date())-1,'-01-01')

selic <- get_annual_market_expectations(indic_selic, start_date = data_inicio)

selic <- 
  selic %>%
  arrange(indic, desc(date), reference_date) %>%
  distinct(indic, date, reference_date, .keep_all = T)

rm(indic_selic, data_inicio)

#########################
### BALANÇA COMERCIAL ###
#########################

# indic_balanca_comercial <- 'Balança Comercial'
# data_inicio <- paste0(year(Sys.Date())-1,'-01-01')
# 
# balanca_comercial <- get_annual_market_expectations(indic_balanca_comercial, start_date = data_inicio)
# 
# rm(indic_balanca_comercial, data_inicio)

# ##########################
# ### BALANÇO PAGAMENTOS ###
# ##########################
# 
# indic_balanco_pagamentos <- 'Balan\u00e7o de Pagamentos'
# data_inicio <- paste0(year(Sys.Date())-1,'-01-01')
# 
# balanco_pagamentos <- get_annual_market_expectations(indic_balanco_pagamentos, start_date = data_inicio)
# 
# rm(indic_balanco_pagamentos, data_inicio)
# 
# ##################
# ### SIT FISCAL ###
# ##################
# 
# indic_fiscal <- 'Fiscal'
# data_inicio <- paste0(year(Sys.Date())-1,'-01-01')
# 
# fiscal <- get_annual_market_expectations(indic_fiscal, start_date = data_inicio)
# 
# rm(indic_fiscal, data_inicio)