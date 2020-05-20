#######################################################################################################
#####This R Shiny App is to predict financial statement for O&G companies due to COVID 19  #####
#######################################################################################################
## Author: Yushan Zhang
## Contact: yushan_zhang@mckinsey.com

library(tidyr)
library(lubridate)
library(stringr)
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(reshape)
library("zoo")
library(plotly)
rm(list = ls())
options(warn=-1)

PATH = '/Users/Yushan Zhang/Box Sync/Covid19 modeling/0. Team Folders/Yushan/R_code_reprod/'
setwd(PATH)
source(paste0(PATH, 'util_run_company.R'))

###Initialization section
filename = 'COVID19_OG Upstream Model_v2.99.1.xlsx'
production_file = 'Production scenario v2.xlsx' #from Rystad
econ_file = 'Economics cases.xlsx' #from Rystad

product_name <- c('Oil', 'Gas', 'NGL', 'Condensate')
forecast_period <- c('Q42019', 'Q12020', 'Q22020', 'Q32020', 'Q42020', 'Q12021', 'Q22021', 'Q32021', 'Q42021')

###Reduction assumption
reduction_name <- c('Capex', 'Exploration Capex', 'Oil production', 'Gas production', 
                    'NGL production', 'Condensate production', 'Cash dividends paid')
reduction<- data.frame(matrix(0, nrow = length(reduction_name), ncol = 1+length(forecast_period)))
colnames(reduction) <- c('item', forecast_period)
reduction$item <- reduction_name
#assumption input, can change for user
#reduction[reduction$item == 'Exploration Capex', c('Q22020', 'Q32020', 'Q42020')] <- c(0.3, 0.3, 0.3)
# scenario <- 'A1'
# Company_name <- 'Cimarex Energy'


tib <- wrapper(c('Cimarex Energy', 'EOG Resources', 'Apache', 'Continental Resources','Concho Resources',
                 'Devon Energy'), c('A3', 'A1'), forecast_period, reduction)
#tib_long <- convert_long(tib)

# basin analysis to return top 2 basins in the production
basin <- data.frame(matrix(NA, nrow = length(unique(tib$Company)), ncol = 2))
colnames(basin) <- c('company', 'basin')
basin$company <- unique(tib$Company)
for (company_name in unique(tib$Company)){
  basin_output <- basin_analysis('A1', company_name, forecast_period, reduction)
  if (dim(basin_output)[1] >= 3){
    basin[basin$company == company_name, 'basin'] <- paste0(basin_output[2, 'Basin'], '; ', basin_output[3, 'Basin'])
  } else{
    basin[basin$company == company_name, 'basin'] <- basin_output[1, 'Basin']
  }
}


#overview table
area_sum <- c('Interest expense, net', 'Cash dividends paid', 'Capex', 'Exploration Capex', 'Free Cash Flow (FCF)', 'EBITDA')
area_average <- c('Long-term debt, net', 'Accounts payable')
overviewTib <- tib %>% filter(Company %in% unique(tib$Company)) %>% filter(item %in% c(area_sum, area_average))
overviewTib[, forecast_period] <- sapply(overviewTib[, forecast_period], as.numeric)
overviewTib[overviewTib$item %in% area_sum, 'sum2020'] <- (rowSums(overviewTib[overviewTib$item %in% area_sum, forecast_period[2:5]]))
overviewTib[overviewTib$item %in% area_sum, 'sum2021'] <- (rowSums(overviewTib[overviewTib$item %in% area_sum, forecast_period[6:9]]))
overviewTib[overviewTib$item %in% area_average, 'sum2020'] <- (rowMeans(overviewTib[overviewTib$item %in% area_average, forecast_period[2:5]]))
overviewTib[overviewTib$item %in% area_average, 'sum2021'] <- (rowMeans(overviewTib[overviewTib$item %in% area_average, forecast_period[6:9]]))
overviewTib <- overviewTib[, c('item', 'scenario', 'sum2020', 'sum2021', 'Company')]

overviewTib2 <- data.frame(matrix(NA, nrow = length(unique(tib$Company))*2, ncol = 8))
colnames(overviewTib2) <- c('Company', 'Location/Basin', '2020 Outlay', '2020 Excess/gap', '2020 Debt/EBITDA', '2021 Outlay', '2021 Excess/gap', '2021 Debt/EBITDA')
overviewTib2$Company <- rep(unique(tib$Company),2)
overviewTib2$scenario <- c(rep('A1', length(unique(tib$Company))), rep('A3', length(unique(tib$Company))))
for (over_sce in c('A1', 'A3')){
  for (company_name in unique(tib$Company)){
    for (sumyear in c('2020', '2021')){
      outlay_sum <- sum(abs(overviewTib[(overviewTib$Company == company_name) & (overviewTib$scenario == over_sce) & (overviewTib$item %in% c(area_sum[1:4])), paste0('sum', sumyear)]))
      overviewTib2[(overviewTib2$Company == company_name) & (overviewTib2$scenario == over_sce), paste0(sumyear, ' Outlay')] <- round(outlay_sum, 1)
      gap_sum <- overviewTib[(overviewTib$Company == company_name) & (overviewTib$scenario == over_sce) & (overviewTib$item == 'Free Cash Flow (FCF)'), paste0('sum', sumyear)] - outlay_sum
      overviewTib2[(overviewTib2$Company == company_name) & (overviewTib2$scenario == over_sce), paste0(sumyear, ' Excess/gap')] <- round(gap_sum, 1)
      debt_ebit <- (as.numeric(overviewTib[(overviewTib$Company == company_name) & (overviewTib$scenario == over_sce) & (overviewTib$item == 'Accounts payable'), paste0('sum', sumyear)]) +
                      as.numeric(overviewTib[(overviewTib$Company == company_name) & (overviewTib$scenario == over_sce) & (overviewTib$item == 'Long-term debt, net'), paste0('sum', sumyear)]))/
        (overviewTib[(overviewTib$Company == company_name) & (overviewTib$scenario == over_sce) & (overviewTib$item == 'EBITDA'), paste0('sum', sumyear)])
      overviewTib2[(overviewTib2$Company == company_name) & (overviewTib2$scenario == over_sce), paste0(sumyear, ' Debt/EBITDA')] <- round(debt_ebit, 1)
      overviewTib2[(overviewTib2$Company == company_name), 'Location/Basin'] <- basin[basin$company == company_name, 'basin']
    }
  }
}
overviewTib2$category2020 <- overviewTib2$`2020 Excess/gap` / overviewTib2$`2020 Outlay` 
overviewTib2$category2021 <- overviewTib2$`2021 Excess/gap` / overviewTib2$`2021 Outlay` 
overviewTibA1 <- overviewTib2[overviewTib2$scenario == 'A1',]
overviewTibA3 <- overviewTib2[overviewTib2$scenario == 'A3',]
overviewTibA3 <- overviewTibA3[order(overviewTibA3$`2020 Outlay`, decreasing = TRUE), ]
overviewTibA1 <- overviewTibA1[order(overviewTibA1$`2020 Outlay`, decreasing = TRUE), ]
rownames(overviewTibA3) <- NULL
rownames(overviewTibA1) <- NULL
