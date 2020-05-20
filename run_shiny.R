library(shiny)
library(RColorBrewer)
library(shinythemes)
library(DT)
library(shinyjs)
#install.packages("shinythemes")
#https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

# PATH = '/Users/Yushan Zhang/Box Sync/Covid19 modeling/0. Team Folders/Yushan/R_code_reprod/'
# setwd(PATH)
source(paste0(PATH, 'util_run_company.R'))

options(shiny.sanitize.errors = TRUE)
colors_modules <- rep(c('#1f77b4', '#ff7f0e','#2ca02c','#d62728','#9467bd','#8c564b','#e377c2','#7f7f7f','#bcbd22','#17becf'), 100)
fcf_plot_size <- 200

# Define UI for dataset viewer app ----
ui <- fluidPage(theme = shinytheme("cosmo"),titlePanel("O&G Cash Flow Diagnostic"),
  useShinyjs(),
  navbarPage("", 
             tabPanel("Companies Overview",
                      mainPanel(tabPanel(tags$hr(width = '1500px'),
                                         h3('A1 Scenario'), 
                                 DT::dataTableOutput("modelTable1", width = '1500px'), 
                                 tags$hr(width = '1500px'), 
                                 h3('A3 Scenario'), 
                                 DT::dataTableOutput("modelTable2", width = '1500px')))), 
             
             tabPanel("Company Deep Dive",
                      div(id ="Sidebar", sidebarPanel(
                        tags$h3("Options for deep dive"),
                        radioButtons("var_comp", label = h4("Company for deep dive"),
                                     choices = c(unique(tib$Company)), selected = unique(tib$Company)[1]),
                        checkboxGroupInput("var_sce", label = h4("Scenario for deep dive"),
                                           choices = c(unique(tib$scenario)), selected = c('A3', 'A1')),
                        radioButtons("var_plot", label = h4("Financial display"),
                                     choices = c('Total Revenue', 'Total Costs and Expenses',
                                                 'Net (loss) Income', 'Free Cash Flow (FCF)', 'EBITDA',
                                                 'EBITDAX'), selected = 'Free Cash Flow (FCF)'))),
                      mainPanel(tabPanel("Company Deep Dive", uiOutput("ui1"), hr(), uiOutput("ui2")))))
)



# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # Return the requested dataset ----
  datasetInput <- reactive({
    tib_temp <- tib %>%
      filter(Company %in% input$var_comp) %>%
      filter(item %in% input$var_plot)
    tib_long <- convert_long(tib_temp)
    tib_long <- tib_long %>% filter(scenario %in% input$var_sce)
    return (tib_long)
  })

  
  output$modelTable1 <- DT::renderDataTable({ #, 
    datatable(caption = 'Outlay and Excess/gap unit: $M', filter = 'top',selection="multiple", 
              overviewTibA1, rownames= FALSE, options=list(autoWidth = FALSE, columnDefs = list(list(visible=FALSE, targets=c(-1,-2,-3))), lengthMenu = c(10, 5))) %>%
      formatStyle(
        "2020 Excess/gap", 'category2020',
        color = styleInterval(c(-1, 0, 1), c('black','black','white','white')),
        backgroundColor = styleInterval(c(-1, 0, 1), c('red', 'pink', 'lightgreen', 'green'))
      ) %>%
    formatStyle(
      "2021 Excess/gap", 'category2021',
      color = styleInterval(c(-1, 0, 1), c('black','black','white','white')),
      backgroundColor = styleInterval(c(-1, 0, 1),c('red', 'pink', 'lightgreen', 'green'))
    ) %>%
    formatStyle(
      '2020 Outlay',
      background = styleColorBar(range(overviewTibA1$`2020 Outlay`), 'lightblue'),
      bacgroundPosition = 'right')%>%
    formatStyle(
      '2021 Outlay',
      background = styleColorBar(range(overviewTibA1$`2021 Outlay`), 'lightblue'),
      bacgroundPosition = 'right') %>%
    formatCurrency(columns = c(3,4,6,7), currency = '', interval = 3, mark = ",", digits = 0) %>%
    formatCurrency(columns = c(5,8), currency = '', interval = 3, mark = ",", digits = 1)
  })
    
  
  output$modelTable2 <- DT::renderDataTable({
    datatable(caption = 'Outlay and Excess/gap unit: $M', filter = 'top',selection="multiple", 
              overviewTibA3, rownames= FALSE, options=list(autoWidth = FALSE, columnDefs = list(list(visible=FALSE, targets=c(-1,-2,-3))), lengthMenu = c(10, 5))) %>%
      formatStyle(
        "2020 Excess/gap", 'category2020',
        color = styleInterval(c(-1, 0, 1), c('black','black','white','white')),
        backgroundColor = styleInterval(c(-1, 0, 1), c('red', 'pink', 'lightgreen', 'green')),
        backgroundSize = '98% 88%'
      ) %>%
      formatStyle(
        "2021 Excess/gap", 'category2021',
        color = styleInterval(c(-1, 0, 1), c('black','black','white','white')),
        backgroundColor = styleInterval(c(-1, 0, 1), c('red', 'pink', 'lightgreen', 'green'))
      ) %>%
      formatStyle(
        '2020 Outlay',
        background = styleColorBar(range(overviewTibA3$`2020 Outlay`), 'lightblue'),
        bacgroundPosition = 'right') %>%
      formatStyle(
        '2021 Outlay',
        background = styleColorBar(range(overviewTibA3$`2021 Outlay`), 'lightblue'),
        bacgroundPosition = 'right') %>%
      formatCurrency(columns = c(3,4,6,7), currency = '', interval = 3, mark = ",", digits = 0) %>%
      formatCurrency(columns = c(5,8), currency = '', interval = 3, mark = ",", digits = 1)
  })
  
  
    
  #tab 2 display
  datasetBar <- reactive({
    area_out <- c('Interest expense, net', 'Cash dividends paid', 'Capex', 'Exploration Capex', 'Free Cash Flow (FCF)')
    Muts <- tib %>% filter(Company %in% input$var_comp) %>%  filter(item %in% area_out)
    Muts[, forecast_period] <- sapply(Muts[, forecast_period], as.numeric)
    Muts$sum2020 <- (rowSums(Muts[, forecast_period[2:5]]))
    Muts$sum2021 <- (rowSums(Muts[, forecast_period[6:9]]))
    Muts <- Muts[, c('item', 'scenario', 'sum2020', 'sum2021')]
    
    Muts2 <- data.frame(matrix(0, nrow = 8, ncol = 6))
    colnames(Muts2) <- area_out
    Muts2$item <- rep(c('2020', '2020 cash', '2021', '2021 cash'), 2)
    for (section in c('Interest expense, net', 'Cash dividends paid', 'Capex', 'Exploration Capex')){
      Muts2[1, section] <- abs(as.numeric(Muts[(Muts$item == section) & (Muts$scenario == 'A1'), 'sum2020']))
      Muts2[3, section] <- abs(as.numeric(Muts[(Muts$item == section) & (Muts$scenario == 'A1'), 'sum2021']))
      Muts2[5, section] <- abs(as.numeric(Muts[(Muts$item == section) & (Muts$scenario == 'A3'), 'sum2020']))
      Muts2[7, section] <- abs(as.numeric(Muts[(Muts$item == section) & (Muts$scenario == 'A3'), 'sum2021']))
    }
    Muts2[2, 'Free Cash Flow (FCF)'] <- Muts[(Muts$item == 'Free Cash Flow (FCF)') & (Muts$scenario == 'A1'), 'sum2020']
    Muts2[4, 'Free Cash Flow (FCF)'] <- Muts[(Muts$item == 'Free Cash Flow (FCF)') & (Muts$scenario == 'A1'), 'sum2021']
    Muts2[6, 'Free Cash Flow (FCF)'] <- Muts[(Muts$item == 'Free Cash Flow (FCF)') & (Muts$scenario == 'A3'), 'sum2020']
    Muts2[8, 'Free Cash Flow (FCF)'] <- Muts[(Muts$item == 'Free Cash Flow (FCF)') & (Muts$scenario == 'A3'), 'sum2021']
    return(Muts2)
  })
  
  
  output$barplot <- renderPlotly({
    Muts <- datasetBar()
    area <- c('Interest expense, net', 'Cash dividends paid', 'Capex', 'Exploration Capex', 'Free Cash Flow (FCF)')
    colours <- c('darkblue', 'blue', 'cornflowerblue', 'cyan', 'coral')
    #colours <- brewer.pal(length(area)+3, "Spectral")
    #display.brewer.pal(length(area)+3, "Spectral")
    fig1 <- plot_ly(orientation = 'h')
    fig2 <- plot_ly(orientation = 'h')
    for (i in 1:length(area)){
      section <- area[i]
      print(section)
      fig1 <- fig1 %>% add_trace(x = Muts[4:1, section], 
                               y = Muts[4:1, 'item'], name = section, type = 'bar',color=colours[i]) #color = as.factor(section))}
      fig2 <- fig2 %>% add_trace(x = Muts[8:5, section], 
                               y = Muts[8:5, 'item'], name = section, type = 'bar', color=colours[i], showlegend = F)}
    fig1 <- fig1 %>% layout(xaxis = list(title = '$M'), yaxis = list(autorange = "reversed"), barmode = 'stack') %>% 
      add_annotations(text = 'A1 Scenario', x = min(Muts[4:1,2:6]), y = -1, font = list(size = 20), showarrow = F)
    fig2 <- fig2 %>% layout(xaxis = list(title = '$M'), yaxis = list(autorange = "reversed"), barmode = 'stack') %>% 
      add_annotations(text = 'A3 Scenario', x = min(Muts[4:1,2:6]), y = -1, font = list(size = 20), showarrow = F)
    fig <- subplot(fig1, fig2, shareY = TRUE, shareX = TRUE)
    fig
})
  
  
  output$lineplot<- renderPlotly({
    validate(
      need(input$var_comp, "Please select a Comapany"),
      need(input$var_sce, "Please select a Scenario"),
      need(input$var_plot, "Please select financial area to display"))
    
    data_plot <- datasetInput()
    p <- plotly::plot_ly(height=fcf_plot_size) 
    for (i in seq(1, length(unique(data_plot$Color2)), 1)){
      data_sub_plot <- data_plot[data_plot$Color2 == unique(data_plot$Color2)[i],]
      data_plot_A3 <- data_sub_plot[data_sub_plot$scenario == 'A3',]
      data_plot_A1 <- data_sub_plot[data_sub_plot$scenario == 'A1',]
      p <- p %>% plotly::add_trace(x= data_plot_A3$date, y= data_plot_A3$value, color = data_plot_A3$Color, 
                                   type = 'scatter', mode = 'lines', line = list(color = colors_modules[i]))
      p <- p %>% plotly::add_trace(x= data_plot_A1$date, y= data_plot_A1$value, color = data_plot_A1$Color, 
                                   type = 'scatter', mode = 'lines', line = list(dash='dash',color = colors_modules[i]))
    }
    p %>% layout(title = "$ M", yaxis = list(title = '$M'))
    p 
  })

  
  
  output$ui1 <- renderUI({
    tagList(h3(paste0(input$var_comp, " ", input$var_plot, " Forecast 2020 - 2021")), h4('Unit: $M'), plotlyOutput("lineplot", height = fcf_plot_size))
  })
  
  output$ui2 <- renderUI({
    tagList(h3(paste0(input$var_comp, " Cash Position")), h4('Unit: $M'), plotlyOutput("barplot"))
  })
  
} 

# Create Shiny app ----
shinyApp(ui = ui, server = server)





# output$distPlot <- renderPlot({
#   data_plot <- datasetInput()
#   ggplot(data_plot, aes(x = date)) +
#     geom_line(aes(y = value, color = Color)) +
#     theme(legend.position = "bottom") +
#     xlab("Time Period") + ylab("$ M") +
#     theme_bw() +
#     theme(axis.title.x = element_text(size = rel(1.5)),
#           axis.title.y = element_text(size = rel(1.5)),
#           legend.text = element_text(size = rel(1.5)))
# }, height = 400, width = 600)

# plot1<-ggplot(data)...
# ggplotly(plot1)

# ui <- fluidPage(
#   theme = shinytheme("united"),
#   titlePanel("O&G Cash Flow Model"),
#     
#   sidebarLayout(
#     sidebarPanel(
#       tags$h3("Options for deep dive"),
#       radioButtons("var_comp", label = h4("Company for deep dive"),
#                          choices = c(unique(tib$Company)), selected = unique(tib$Company)[1]),
#       checkboxGroupInput("var_sce", label = h4("Scenario for deep dive"),
#                          choices = c(unique(tib$scenario)), selected = c('A3', 'A1')),
#       radioButtons("var_plot", label = h4("Financial display"),
#                          choices = c('Total Revenue', 'Total Costs and Expenses',
#                                      'Net (loss) Income', 'Free Cash Flow (FCF)', 'EBITDA',
#                                      'EBITDAX'), selected = 'Free Cash Flow (FCF)')),
#  
#     # Main panel for displaying outputs ----
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Companies Overview",
#                  h2("Companies overview"),
#                  h3('A1 Scenario'),
#                  DT::dataTableOutput("modelTable1"), 
#                  tags$hr(),
#                  h3('A3 Scenario'),
#                  DT::dataTableOutput("modelTable2")), 
#         
#         tabPanel("Company Deep Dive",
#                  h4("Company Deep Dive"),
#                  uiOutput("ui1"),
#                  hr(),
#                  uiOutput("ui2"))
#     )
#   )
# )
# )



#tab 1 display
# datasetOverviewA3 <- reactive({
#   overviewTib2 <- overviewTib2[overviewTib2$scenario == 'A3' , ]
#   return(overviewTib2[, 1: (dim(overviewTib2)[2]-1)])
# })
# 
# output$modelTable1 <- renderTable({
#   datasetOverviewA1() 
# }, digits = 1)
# 
# output$modelTable2 <- renderTable({
#   datasetOverviewA3()
# }, digits = 1)