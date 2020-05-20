library(tidyr)
library(lubridate)
library(stringr)
library(dplyr)
library(readxl)
library(openxlsx)
# PATH = '/Users/Yushan Zhang/Box Sync/Covid19 modeling/0. Team Folders/Yushan/R_code_reprod/'
# setwd(PATH)

# Wrapper to run main function
wrapper <- function(company_name_all, scenario_all, forecast_period_1, reduction){
  ########################### long format excel with name #############
  final_excel <- data_frame()
  for (company in company_name_all){
    for (scenario in scenario_all){
      print(paste0('Company: ', company, scenario))
      output <- runcompany(scenario, company, forecast_period_1, reduction)
      final_excel <- rbind(final_excel, output)
    }
  }
  #show as millions
  final_excel[2:(length(forecast_period_1)+1)] <- sapply(final_excel[2:(length(forecast_period_1)+1)]/1000, as.numeric)
  #select desired significant digits
  final_excel[2:(length(forecast_period_1)+1)] <- format(round(final_excel[2:(length(forecast_period_1)+1)], 1), nsmall = 1)
  final_excel$Color <- paste0(final_excel$Company, '_', final_excel$item, '_', final_excel$scenario)
  final_excel$item2 <- final_excel$item
  return(final_excel)
}


#main function to run
runcompany <- function(scenario, Company_name, forecast_period, reduction){
  if (scenario == 'A1'){
    product_sheet <- '$22'
    corp_borrow_rate_2020 <- (4.367334-3)
    corp_borrow_rate_2021 <- (2.79938-4.367334)
  } else {
    product_sheet <- '$33'
    corp_borrow_rate_2020 <- (3.427848-3)
    corp_borrow_rate_2021 <- (2.99428 - 3.427848)
  }
  
  rowlength <- length(forecast_period)+1  #add extra for $item indication
  
  ################################################################
  ####### read data input, need to adjust for standarizaiton#######
  ################################################################
  
  #read model sheet
  # C1_19Q4 <- read_excel(paste0(PATH, filename), sheet = word(Company_name, 1))
  # input_temp <- data.frame(item = C1_19Q4['...1'], data = C1_19Q4['2019'])
  # names(input_temp)[1] <- 'item'
  # names(input_temp)[2] <- 'Q42019'
  # 
  # prediction <- data.frame(item = input_temp$item[!is.na(input_temp$item)], 
  #                          Q42019 = input_temp$Q42019[!is.na(input_temp$item)])
  # for (period in forecast_period[2:length(forecast_period)]){prediction[[period]] <- NA}
  # prediction[, 2] <- as.numeric(as.character(prediction[, 2]))
  
  standard <- read_excel(paste0(PATH, filename), sheet = 'Input',skip=3)
  prediction <- standard[, c('2019Q4', Company_name)]
  colnames(prediction) <- c('item', 'Q42019')
  prediction$item <- paste0(Company_name, prediction$item)
  prediction$Q42019 <- as.numeric(as.character(prediction$Q42019))
  prediction[is.na(prediction)] <- 0
  
  
  #capex and Exploration Capex read
  econ <- read_excel(paste0(PATH, econ_file), sheet = paste0(product_sheet, ' case'), skip = 5)
  econ = subset(econ, econ$Company == Company_name)
  sapply(econ[, 8:14],as.numeric)
  
  #production prediction read
  production <- read_excel(paste0(PATH, production_file), sheet = product_sheet, skip = 6)
  production = subset(production, production$Company == Company_name)
  production$`Oil and Gas Category`[production$`Oil and Gas Category` == 'Crude Oil'] <- 'Oil'
  sapply(production[, 8:14],as.numeric)
  
  
  ##############################################
  ######## production section prediction #######
  ##############################################
  production_prediction <- data.frame(matrix(NA, nrow = 5, ncol = rowlength))
  colnames(production_prediction) <- c('item', forecast_period)
  production_prediction$item <- c(product_name, 'Total production')
  
  for (product in unique(production$`Oil and Gas Category`)){
    for (timeperiod in forecast_period){
      yearperiod <- substring(timeperiod,  3, 6) #get the year name
      production_quarter = sum(subset(production[yearperiod], production$`Oil and Gas Category` == product), na.rm=T)*365/4 #daily to adjust for quarter
      production_prediction[production_prediction$item == product, timeperiod] <- production_quarter
    }
  }
  production_prediction[is.na(production_prediction)] <- 0
  #use company 10K production
  if (Company_name == 'Apache'){
    production_prediction[production_prediction$item == product_name, forecast_period[1]] <- 
      c(21850, 14908.3, 6475, 0)
  }
  
  production_prediction[production_prediction$item == 'Total production', 2:rowlength] <- 
    colSums(production_prediction[production_prediction$item %in% product_name, 2:rowlength])
  
  
  ##############################################
  ######## market and relaized price ###########
  ##############################################
  marketprice_prediction <- data.frame(matrix(NA, nrow = length(product_name), ncol = rowlength))
  colnames(marketprice_prediction) <- c('item', forecast_period)
  marketprice_prediction$item <- c(product_name)
  
  if (scenario == 'A1'){
    #marketprice_prediction[forecast_period[1]] <- c(56.95667, 14.34000, 20.07320, 34.17400)
    forecast_price <- c(50.81859, seq(15, 25, length.out=6), 25) #from Q12020 to Q42021
  } else {
    #marketprice_prediction[forecast_period[1]] <- c(57.030, 15.780, 31.929, 34.218)
    forecast_price <- c(50.81859, seq(20, 30, length.out=6), 30)
  }
  
  marketprice_prediction[forecast_period[1]] <- c(64.35833, 15.15714, 20.61623, 38.61500)
  marketprice_prediction[marketprice_prediction$item == 'Oil', forecast_period[2:(rowlength-1)]] <- forecast_price 
  marketprice_prediction[marketprice_prediction$item == 'Gas', forecast_period[2:(rowlength-1)]] <- c(11.20355, (0.0183*forecast_price[2:length(forecast_price)] + 1.7798)*6)
  marketprice_prediction[marketprice_prediction$item == 'NGL', forecast_period[2:(rowlength-1)]] <- c(17.26445, (0.1166*forecast_price[2:length(forecast_price)] - 0.0755)*3.745)
  marketprice_prediction[marketprice_prediction$item == 'Condensate', forecast_period[2:(rowlength-1)]] <- forecast_price * 0.6
  
  
  
  #get realized price
  realprice_prediction <- data.frame(matrix(NA, nrow = (length(product_name)), ncol = rowlength))
  colnames(realprice_prediction) <- c('item', forecast_period)
  realprice_prediction$item <- c(product_name)

  for (product in product_name){
    realprice_prediction[realprice_prediction$item == product, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, product)), forecast_period[1]]
    multiplier <- realprice_prediction[realprice_prediction$item == product, forecast_period[1]]/ marketprice_prediction[marketprice_prediction$item == product, forecast_period[1]]
    realprice_prediction[realprice_prediction$item == product, forecast_period[2:(rowlength-1)]] <- marketprice_prediction[marketprice_prediction$item == product, forecast_period[2:(rowlength-1)]] * multiplier
  }  
  realprice_prediction[is.na(realprice_prediction)] <- 0
  
  
  
  ##############################################
  ######## Revenue prediction ###########
  ##############################################
  revenue_name <- c('Sales Revenue', 'Other Revenue', 'Total Revenue')
  revenue_prediction <- data.frame(matrix(NA, nrow = length(revenue_name), ncol = rowlength))
  colnames(revenue_prediction) <- c('item', forecast_period)
  revenue_prediction$item <- revenue_name
  revenue_prediction[1:2, forecast_period[1]] <- prediction[prediction$item %in% (paste0(Company_name, revenue_name[1:2])), forecast_period[1]]

  revenue_mul <- revenue_prediction[revenue_prediction$item == revenue_name[2], forecast_period[1]]/revenue_prediction[revenue_prediction$item == revenue_name[1],forecast_period[1]]
  for (forecast in forecast_period[2:length(forecast_period)]){
    sales_revenue <- sum(realprice_prediction[1:length(product_name), forecast] * production_prediction[1:length(product_name), forecast])
    revenue_prediction[revenue_prediction$item == revenue_name[1], forecast] <- sales_revenue
    revenue_prediction[revenue_prediction$item == revenue_name[2], forecast]<- sales_revenue * revenue_mul
  }
  revenue_prediction[revenue_prediction$item == revenue_name[length(revenue_name)], 2:rowlength] <- 
    colSums(revenue_prediction[revenue_prediction$item %in% revenue_name[1:length(revenue_name)-1], 2:rowlength])
  
  
  ##############################################
  ######## Current liabilities ################
  ##############################################
  curliab_name_flat <- c('Accrued liabilities', 'Other current liabilities',
                         'Derivative instruments current liabilities')
  curliab_name_perc <- c('Accounts payable')
  curliab_name <- c(curliab_name_flat, curliab_name_perc, 'Total current liabilities')
  
  curliab_prediction <- data.frame(matrix(NA, nrow = length(curliab_name), ncol = rowlength))
  colnames(curliab_prediction) <- c('item', forecast_period)
  curliab_prediction$item <- curliab_name
  for (curliab in curliab_name){
    curliab_prediction[curliab_prediction$item == curliab, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, curliab)), forecast_period[1]]
  }
  for (flat in curliab_name_flat){
    curliab_prediction[curliab_prediction$item == flat, 2:rowlength] <- 
      rep(curliab_prediction[curliab_prediction$item == flat, forecast_period[1]], rowlength-1)
  }
  for (perc in curliab_name_perc){
    multiplier <- curliab_prediction[curliab_prediction$item == perc, forecast_period[1]]/production_prediction[dim(production_prediction)[1], forecast_period[1]]
    curliab_prediction[curliab_prediction$item == perc, 2:rowlength] <- multiplier * production_prediction[dim(production_prediction)[1], 2:rowlength]
  }
  
  #derivative instruments as 0 after prediction
  curliab_prediction[curliab_prediction$item == 'Derivative instruments current liabilities', 3:rowlength] <- rep(0, rowlength-2)
  curliab_prediction[curliab_prediction$item == curliab_name[length(curliab_name)], 2:rowlength] <- 
    colSums(curliab_prediction[curliab_prediction$item %in% curliab_name[1:length(curliab_name)-1], 2:rowlength]) 
  
  
  
  ##############################################
  ######## Total liabilities ################
  ##############################################
  totliab_name_flat <- c('Long-term debt, net', 'Asset retirement obligations',
                         'Derivative instruments liabilities', 'Deferred income taxes liabilities',
                         'Other long-term liabilities')
  totliab_name <- c(totliab_name_flat, 'Total liabilities')
  
  totliab_prediction <- data.frame(matrix(NA, nrow = length(totliab_name), ncol = rowlength))
  colnames(totliab_prediction) <- c('item', forecast_period)
  totliab_prediction$item <- totliab_name
  for (totliab in totliab_name){
    totliab_prediction[totliab_prediction$item == totliab, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, totliab)), forecast_period[1]]
  }
  for (flat in totliab_name_flat){
    totliab_prediction[totliab_prediction$item == flat, 2:rowlength] <- 
      rep(totliab_prediction[totliab_prediction$item == flat, forecast_period[1]], rowlength-1)
  }
  #assume Derivative instruments liabilities as 0
  totliab_prediction[totliab_prediction$item == 'Derivative instruments liabilities', 3:rowlength] <- rep(0, rowlength-2)
  
  totliab_prediction[totliab_prediction$item == totliab_name[length(totliab_name)], 2:rowlength] <- 
    colSums(totliab_prediction[totliab_prediction$item %in% totliab_name[1:length(totliab_name)-1], 2:rowlength]) +
    curliab_prediction[curliab_prediction$item == curliab_name[length(curliab_name)], 2:rowlength]
  
  
  
  ##############################################
  ######## Cost and expenses ################
  ##############################################
  cost_name_flat <- c('Impairment')
  cost_name_perc <- c('DD&A', 'Asset retirement obligation', 'Taxes other than income',
                      'Exploratory and dirt well, dry hole', 'Other Expenses (benefits)')
  cost_name_linear <- c('Non-cash compensation', 'Lease operating expenses', 
                        'Transportation, Processing, and Other Operating','SG&A')
  cost_name <- c(cost_name_flat, cost_name_perc, cost_name_linear, 'Total Costs and Expenses')
  
  cost_prediction <- data.frame(matrix(NA, nrow = length(cost_name), ncol = rowlength))
  colnames(cost_prediction) <- c('item', forecast_period)
  cost_prediction$item <- cost_name
  for (cost in cost_name){
    cost_prediction[cost_prediction$item == cost, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, cost)), forecast_period[1]]
  }
  
  for (flat in c(cost_name_flat)){
    cost_prediction[cost_prediction$item == flat, 2:rowlength] <- 
      rep(cost_prediction[cost_prediction$item == flat, forecast_period[1]], rowlength-1)
  }
  
  for (perc in cost_name_perc){
    multiplier <- cost_prediction[cost_prediction$item == perc, forecast_period[1]]/production_prediction[dim(production_prediction)[1], forecast_period[1]]
    cost_prediction[cost_prediction$item == perc, 2:rowlength] <- multiplier * production_prediction[dim(production_prediction)[1], 2:rowlength]
  }
  
  #special Opex equation: (0.0266*oil price + 11.643) = Opex/production
  for (linear in cost_name_linear){
    denom <-  (marketprice_prediction[marketprice_prediction$item == 'Oil', forecast_period[1]] * 0.0266 + 11.643)*
      production_prediction[dim(production_prediction)[1], forecast_period[1]]
    
    for (timeperiod1 in forecast_period[2:(rowlength-1)]){
      nom <- (marketprice_prediction[marketprice_prediction$item == 'Oil', timeperiod1] * 0.0266 + 11.643)* production_prediction[dim(production_prediction)[1], timeperiod1]
      cost_prediction[cost_prediction$item == linear, timeperiod1] <-  cost_prediction[cost_prediction$item == linear, forecast_period[1]]*nom/denom
    }
   }
  cost_prediction[cost_prediction$item == cost_name[length(cost_name)], 2:rowlength] <- 
    colSums(cost_prediction[cost_prediction$item %in% cost_name[1:length(cost_name)-1], 2:rowlength])
  
  
  ##############################################
  ######## Total other expenses ################
  ##############################################
  
  otherexp_name <- c('Loss (gain) on derivative instruments, net', 'Interest expense, net', 'Other, net', 'Total Other (Income) Expense')
  otherexp_prediction <- data.frame(matrix(NA, nrow = length(otherexp_name), ncol = rowlength))
  colnames(otherexp_prediction) <- c('item', forecast_period)
  otherexp_prediction$item <- otherexp_name
  for (otherexp in otherexp_name){
    otherexp_prediction[otherexp_prediction$item == otherexp, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, otherexp)), forecast_period[1]]
  }
  otherexp_prediction[otherexp_prediction$item == otherexp_name[1], 2:rowlength] <- 
    rep(otherexp_prediction[otherexp_prediction$item == otherexp_name[1], forecast_period[1]], rowlength-1)
  
  otherexp_prediction[otherexp_prediction$item == otherexp_name[3], 2:rowlength] <- 
    rep(otherexp_prediction[otherexp_prediction$item == otherexp_name[3], forecast_period[1]], rowlength-1)

  ####!!!!!!!SPECIAL equation for interest income
  for (id_forecast in seq(3, rowlength, 1)){
    mul <- (totliab_prediction[dim(totliab_prediction)[1], id_forecast]) / (totliab_prediction[dim(totliab_prediction)[1], id_forecast-1])
    yearperiod <- substring(forecast_period[id_forecast-1], 3, 6)
    if (yearperiod == '2020'){
      corp_borrow_rate <- corp_borrow_rate_2020
      use_idx <- forecast_period[1]
    } else if (yearperiod == '2021'){
      corp_borrow_rate <- corp_borrow_rate_2021
      use_idx <- forecast_period[5]
    } else {
      print(paste0('Need corp borrow rate for year', yearperiod))
    }
    curdebt <- totliab_prediction[totliab_prediction$item == 'Long-term debt, net', use_idx]
    curexp <- otherexp_prediction[otherexp_prediction$item == 'Interest expense, net', use_idx]
    result <- curdebt * mul * (curexp/curdebt + corp_borrow_rate/400)
    otherexp_prediction[otherexp_prediction$item == 'Interest expense, net', id_forecast] <- result
  }
  
  otherexp_prediction[otherexp_prediction$item == otherexp_name[length(otherexp_name)], 2:rowlength] <- 
    colSums(otherexp_prediction[otherexp_prediction$item %in% otherexp_name[1:length(otherexp_name)-1], 2:rowlength])
  
  
  #net loss
  netincome_name <- c('Income (loss) from continuing operations before income taxes',
                      'Income tax (benefit) expense', 'Net (loss) Income')
  netincome_prediction <- data.frame(matrix(NA, nrow = length(netincome_name), ncol = rowlength))
  colnames(netincome_prediction) <- c('item', forecast_period)
  netincome_prediction$item <- netincome_name
  for (netincome in netincome_name[2:3]){
    netincome_prediction[netincome_prediction$item == netincome, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, netincome)), forecast_period[1]]
  }
  netincome_prediction[netincome_prediction$item == netincome_name[1], 2:rowlength] <- revenue_prediction[dim(revenue_prediction)[1], 2:rowlength] - cost_prediction[dim(cost_prediction)[1], 2:rowlength] - otherexp_prediction[dim(otherexp_prediction)[1], 2:rowlength]
  taxrate <- netincome_prediction[netincome_prediction$item == netincome_name[2], forecast_period[1]] / netincome_prediction[netincome_prediction$item == netincome_name[1], forecast_period[1]]
  print(paste0('tax rate: ', taxrate))
  netincome_prediction[netincome_prediction$item == netincome_name[2], 2:rowlength] <- taxrate * netincome_prediction[netincome_prediction$item == netincome_name[1], 2:rowlength]
  netincome_prediction[netincome_prediction$item == netincome_name[3], 2:rowlength] <- netincome_prediction[netincome_prediction$item == netincome_name[1], 2:rowlength] - netincome_prediction[netincome_prediction$item == netincome_name[2], 2:rowlength]
  
  
  
  
  ##############################################
  ######## Current assests ################
  ##############################################
  curasset_name_flat <- c('Derivative instruments current assets', 'Prepaid expenses', 'Other current assets')
  curasset_name_perc <- c('Oil and gas sales','Other receivable', 'Inventory')
  curasset_name_other <- c('Cash and cash equivalents')
  curasset_name <- c(curasset_name_flat, curasset_name_perc, curasset_name_other, 'Total current assets')
  
  curasset_prediction <- data.frame(matrix(NA, nrow = length(curasset_name), ncol = rowlength))
  colnames(curasset_prediction) <- c('item', forecast_period)
  curasset_prediction$item <- curasset_name
  for (curasset in curasset_name){
    curasset_prediction[curasset_prediction$item == curasset, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, curasset)), forecast_period[1]]
  }
  for (flat in curasset_name_flat){
    curasset_prediction[curasset_prediction$item == flat, 2:rowlength] <- 
      rep(curasset_prediction[curasset_prediction$item == flat, forecast_period[1]], rowlength-1)
  }
  for (perc in curasset_name_perc){
    multiplier <- curasset_prediction[curasset_prediction$item == perc, forecast_period[1]]/revenue_prediction[dim(revenue_prediction)[1], forecast_period[1]]
    curasset_prediction[curasset_prediction$item == perc, 3:rowlength] <- multiplier * revenue_prediction[dim(revenue_prediction)[1], 3:rowlength]
  }
  #cash and cash equivalents
  for (id_forecast in 3:rowlength){
    curasset_prediction[curasset_prediction$item == 'Cash and cash equivalents', id_forecast] <- 
      curasset_prediction[curasset_prediction$item == 'Cash and cash equivalents', id_forecast-1] + 
      netincome_prediction[dim(netincome_prediction)[1], id_forecast]
  }
  
  curasset_prediction[curasset_prediction$item == 'Derivative instruments current assets', 3:rowlength] <- rep(0, rowlength-2)
  
  curasset_prediction[curasset_prediction$item == curasset_name[length(curasset_name)], 2:rowlength] <- 
    colSums(curasset_prediction[curasset_prediction$item %in% curasset_name[1:length(curasset_name)-1], 2:rowlength])
  
  
  
  ##############################################
  ######## Total assests ################
  ##############################################
  totasset_name_flat <- c('Property and equipment, net', 'Derivative instruments total assets',
                          'Deferred income taxes assets','Goodwill','Other assets')
  totasset_name <- c(totasset_name_flat, 'Total assets')
  
  totasset_prediction <- data.frame(matrix(NA, nrow = length(totasset_name), ncol = rowlength))
  colnames(totasset_prediction) <- c('item', forecast_period)
  totasset_prediction$item <- totasset_name
  for (totasset in totasset_name){
    totasset_prediction[totasset_prediction$item == totasset, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, totasset)), forecast_period[1]]
  }
  for (flat in totasset_name_flat){
    totasset_prediction[totasset_prediction$item == flat, 2:rowlength] <- 
      rep(totasset_prediction[totasset_prediction$item == flat, forecast_period[1]], rowlength-1)
  }
  
  totasset_prediction[totasset_prediction$item == 'Derivative instruments total assets', 3:rowlength] <- rep(0, rowlength-2)
  totasset_prediction[totasset_prediction$item == totasset_name[length(totasset_name)], 2:rowlength] <- 
    colSums(totasset_prediction[totasset_prediction$item %in% totasset_name[1:length(totasset_name)-1], 2:rowlength]) + 
    curasset_prediction[curasset_prediction$item == curasset_name[length(curasset_name)], 2:rowlength]
  
  
  
  ###################################################
  #################### Free cash flow ###############
  ####################################################
  name_previous <- c('DD&A','Impairment','Exploratory and dirt well, dry hole')
  fcf_name_previous <- c('DD&A cashflow', 'Impairment cashflow', 'Dry hole cashflow')
  fcf_name_other <- c('Change in Net working capital', 'Other non-cash items',
                      'market to market contracts', 'Capex', 'Exploration Capex')
  fcf_name <- c('Net (loss) Income cashflow', fcf_name_previous, fcf_name_other, 'Free Cash Flow (FCF)')
  
  fcf_prediction <- data.frame(matrix(NA, nrow = length(fcf_name), ncol = rowlength))
  colnames(fcf_prediction) <- c('item', forecast_period)
  fcf_prediction$item <- fcf_name
  for (fcf in fcf_name){
    fcf_prediction[fcf_prediction$item == fcf, forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, fcf)), forecast_period[1]]
  }
  
  #same with netloss
  fcf_prediction[fcf_prediction$item == 'Net (loss) Income cashflow', 2:rowlength] <- 
    netincome_prediction[netincome_prediction$item == 'Net (loss) Income', 2:rowlength]

  #proportion to change with previous
  for (id_previous in seq(1, length(name_previous), 1)){
    if (fcf_prediction[fcf_prediction$item == fcf_name_previous[id_previous], forecast_period[1]] == 0){
      fcf_prediction[fcf_prediction$item == fcf_name_previous[id_previous], 2:rowlength] <- rep(0, rowlength-1)
    } else {
      for (id_forecast in seq(3, rowlength, 1)){
        multiplier <- cost_prediction[cost_prediction$item == name_previous[id_previous], id_forecast] /
          cost_prediction[cost_prediction$item == name_previous[id_previous], forecast_period[1]]
        fcf_prediction[fcf_prediction$item == fcf_name_previous[id_previous], id_forecast] <- multiplier *
          fcf_prediction[fcf_prediction$item == fcf_name_previous[id_previous], forecast_period[1]]
      }
    }
  }

  #change in networking capital
  for (id_forecast in seq(3, rowlength, 1)){
    fcf_prediction[fcf_prediction$item == fcf_name_other[1], id_forecast] <- 
      (curasset_prediction[dim(curasset_prediction)[1], id_forecast] - curliab_prediction[dim(curliab_prediction)[1], id_forecast]) - 
      (curasset_prediction[dim(curasset_prediction)[1], id_forecast-1] - curliab_prediction[dim(curliab_prediction)[1], id_forecast-1])
 
   #Other non-cash items offset with change in non cash compensation
    fcf_prediction[fcf_prediction$item == fcf_name_other[2], id_forecast] <- 
      fcf_prediction[fcf_prediction$item == fcf_name_other[2], forecast_period[1]] +
      (cost_prediction[cost_prediction$item ==  'Non-cash compensation', id_forecast] 
       - cost_prediction[cost_prediction$item == 'Non-cash compensation', forecast_period[1]])
  }
  
  
 
  fcf_prediction[fcf_prediction$item == fcf_name_other[3], 2:rowlength] <- 
    rep(fcf_prediction[fcf_prediction$item == fcf_name_other[3], forecast_period[1]], rowlength-1) 
  
  #CAPEX and OPEX, make assumption for reduction
  for (capexname in fcf_name_other[4:5]){
    for (timeperiod in forecast_period){
      yearperiod <- substring(timeperiod,  3, 6) #get the year name
      econ_quarter = sum(subset(econ[yearperiod], econ$`Economics Group` == capexname),na.rm=T)*1000/4
      fcf_prediction[fcf_prediction$item == capexname, timeperiod] <- econ_quarter * (1-reduction[reduction$item == capexname, timeperiod])
    }
  }
  
  
  fcf_prediction[fcf_prediction$item == fcf_name[length(fcf_name)], 2:rowlength] <- 
    colSums(fcf_prediction[fcf_prediction$item %in% fcf_name[1:length(fcf_name)-1], 2:rowlength])
  
  
  EBITDA_name <- c('EBITDA','Exploration capex','EBITDAX')
  EBITDA_prediction <- data.frame(matrix(NA, nrow = length(EBITDA_name), ncol = rowlength))
  colnames(EBITDA_prediction) <- c('item', forecast_period)
  EBITDA_prediction$item <- EBITDA_name
  EBITDA_prediction[EBITDA_prediction$item == EBITDA_name[1], 2:rowlength] <- revenue_prediction[revenue_prediction$item == 'Total Revenue', 2:rowlength] -
    cost_prediction[cost_prediction$item == 'Total Costs and Expenses', 2:rowlength] + cost_prediction[cost_prediction$item == 'DD&A', 2:rowlength]
  EBITDA_prediction[EBITDA_prediction$item == EBITDA_name[2], 2:rowlength] <- fcf_prediction[fcf_prediction$item == 'Exploration Capex', 2:rowlength] 
  EBITDA_prediction[EBITDA_prediction$item == EBITDA_name[3], 2:rowlength] <- 
    colSums(EBITDA_prediction[EBITDA_prediction$item %in% EBITDA_name[1:length(EBITDA_name)-1], 2:rowlength])
 
    
  Dividends_name <- c('Cash dividends paid')
  Dividends_prediction <- data.frame(matrix(NA, nrow = length(Dividends_name), ncol = rowlength))
  colnames(Dividends_prediction) <- c('item', forecast_period)
  Dividends_prediction$item <- Dividends_name
  Dividends_prediction[Dividends_prediction$item ==Dividends_name[1], forecast_period[1]] <- prediction[prediction$item == (paste0(Company_name, Dividends_name[1])), forecast_period[1]]
  for (id_forecast in seq(3, rowlength, 1)){
    Dividends_prediction[Dividends_prediction$item ==Dividends_name[1], id_forecast] <- 
      Dividends_prediction[Dividends_prediction$item ==Dividends_name[1], id_forecast-1] *
      (1-reduction[reduction$item == Dividends_name[1], id_forecast-1])
  }
  
  
  
  #bind the results together
  revenue_prediction <- bindarea(revenue_prediction, 'Revenue', scenario, Company_name)
  cost_prediction <- bindarea(cost_prediction, 'Cost', scenario, Company_name)
  otherexp_prediction <- bindarea(otherexp_prediction, 'Other exp', scenario, Company_name)
  curasset_prediction <- bindarea(curasset_prediction, 'Current assets', scenario, Company_name)
  totasset_prediction <- bindarea(totasset_prediction, 'Total assets', scenario, Company_name)
  curliab_prediction <- bindarea(curliab_prediction, 'Current liabilities', scenario, Company_name)
  totliab_prediction <- bindarea(totliab_prediction, 'Total liabilities', scenario, Company_name)
  netincome_prediction <- bindarea(netincome_prediction, 'Net income', scenario, Company_name)
  fcf_prediction <- bindarea(fcf_prediction, 'FCF',scenario, Company_name)
  EBITDA_prediction <- bindarea(EBITDA_prediction, 'EBITDA', scenario, Company_name)
  Dividends_prediction <- bindarea(Dividends_prediction, 'Dividends', scenario, Company_name)
  output <- rbind(revenue_prediction, cost_prediction, otherexp_prediction, curasset_prediction, totasset_prediction, 
                  curliab_prediction, totliab_prediction, netincome_prediction, fcf_prediction, EBITDA_prediction, Dividends_prediction)
  return(output)
}



#to bind the data add company and area
bindarea <- function(matrix_name, area_name, scenario_name, Company_name){
  matrix_name$area <- area_name
  matrix_name$Company <- Company_name
  matrix_name$scenario <- scenario_name
  return(matrix_name)
}


# basin result
basin_analysis <- function(scenario, Company_name, forecast_period, reduction){
  if (scenario == 'A1'){
    product_sheet <- '$22'
  } else {
    product_sheet <- '$33'
  }
  prod <- read_excel(paste0(PATH, production_file), sheet = product_sheet, skip = 6)
  prod = subset(prod, prod$Company == Company_name)
  sapply(prod[, 8:14],as.numeric)
  
  Basin_list <- unique(prod$Basin)
  Basin_rank <- data.frame(matrix(NA, nrow = length(Basin_list)+1, ncol = 3))
  colnames(Basin_rank) <- c('Basin', '2019', '2020' )
  Basin_rank$Basin <- c(Basin_list, 'sum')
  for(id_basin in seq(1, length(Basin_list), 1)){
    prod_subset <- prod[prod$Basin == Basin_list[id_basin], ]
    for (yearperiod in c('2019', '2020')){
      prod_year = as.numeric(sum(subset(prod_subset[yearperiod]),na.rm=T))
      Basin_rank[id_basin, yearperiod] <- as.numeric(format(round(prod_year, 2), nsmall = 2))
    }
  }
  Basin_rank[(length(Basin_list)+1), 2:3] <- colSums(Basin_rank[1:length(Basin_list), 2:3])
  Basin_rank$perc2019 <- Basin_rank$`2019` / Basin_rank$`2019`[dim(Basin_rank)[1]]
  Basin_rank$perc2020 <- Basin_rank$`2020` / Basin_rank$`2020`[dim(Basin_rank)[1]]
  Basin_rank$perc2019 <-round(Basin_rank$perc2019, 5)
  Basin_rank$perc2020 <-round(Basin_rank$perc2020, 5)
  Basin_rank$Company <- Company_name
  Basin_rank <- Basin_rank[order(Basin_rank$`2020`, decreasing = TRUE), ]
  return(Basin_rank)
}

#convert table to long format
convert_long <- function(datatib){
  Muts <- as.data.frame(t(datatib))
  Muts <- Muts[-1,]
  colnames(Muts) <- datatib$Color
  Muts$date <- rownames(Muts)
  tib_long <- data.frame()
  for (id_row in seq(1, dim(Muts)[2])){
    temp <- Muts[1:dim(Muts)[1], c('date', colnames(Muts)[id_row])]
    temp$item <- as.character(temp[temp$date == 'item2', 2])
    temp$area <- as.character(temp[temp$date == 'area', 2])
    temp$Company <- as.character(temp[temp$date == 'Company', 2])
    temp$scenario <- as.character(temp[temp$date == 'scenario', 2])
    temp$Color <- paste0(temp[temp$date == 'Company', 2],' ', temp[temp$date == 'item2', 2], ' ',temp[temp$date == 'scenario', 2])
    temp$Color2 <- paste0(temp[temp$date == 'Company', 2],' ', temp[temp$date == 'item2', 2])
    colnames(temp) <- c('date', 'value')
    
    tib_long <- rbind(tib_long, temp[1:length(forecast_period),])
    tib_long[,2] <- as.numeric(as.character(tib_long[,2]))
    tib_long$date <- paste(substring(forecast_period, 3, 6), substring(forecast_period, 1, 2), sep = " ")
    #tib_long$date <- as.yearqtr(tib_long$date, "Q%q-%Y")
  }
  colnames(tib_long) <- c('date', 'value', 'item', 'area', 'Company', 'scenario', 'Color', 'Color2')
  rownames(tib_long) <- NULL
  return (tib_long)
}


