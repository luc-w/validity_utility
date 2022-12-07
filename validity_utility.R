# Luc Watrin (2019)
# luc.watrin@uni-ulm.de

# load packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(markdown)

# load formulas to compute relevant data
source('formulae.R')

# load validity data
validity_data <- read.csv2('validity_data.csv')
val1 <- setNames(validity_data$val1, validity_data$method1)

# define ui
ui <- dashboardPage(

  # header
  dashboardHeader(title = 'VALIDITY & UTILITY', 
                  titleWidth = 300),
  
  # sidebar
  dashboardSidebar(
    
    width = 300,
    
    checkboxInput(inputId = 'expert', 
                  label = HTML('<p style="color:white;">Expert mode</p>')),
    
    sliderInput(inputId = 'Applicants',
                label = HTML('<p style="color:white;">total # of applications</p>'),
                min = 2,
                max = 1000,
                value = 800),
    
    sliderInput(inputId = 'StaffRequirement',
                label = HTML('<p style="color:white;"># of candidates selected in 1st step</p>'),
                min = 1,
                max = 1000,
                value = 300),
    
    sliderInput(inputId = 'BaseRate',
                label = HTML('<p style="color:white;">estimated base rate</p>'),
                min = 0,
                max = 100,
                value = 30,
                post  = ' %'),
    
    # define sliders/inputs which depend on the amateur vs. expert checkbox above
    conditionalPanel(
      
      condition = 'input.expert == false',
      
          selectInput(inputId = 'Validity_ama', 
                      label = HTML('<p style="color:white;">2nd selection method</p>'),
                      choices = val1,
                      selected = 'GMA Tests')),
    
    conditionalPanel(
      
      condition = 'input.expert == true',
      
          sliderInput(inputId = 'Validity',
                      label = HTML('<p style="color:white;">validity of 2nd selection method</p>'),
                      min = 0,
                      max = 1,
                      value = 0.3)),
    
          sliderInput(inputId = 'StaffRequirement2',
                      label = HTML('<p style="color:white;"># of candidates selected in 2nd step</p>'),
                      min = 0,
                      max = 1000,
                      value = 50),
    
    conditionalPanel(
    
        condition = 'input.expert == false',
      
          selectInput(inputId = 'Validity_ama2', 
                      label = HTML('<p style="color:white;">2nd selection method</p>'),
                      choices = NULL)),
      
    conditionalPanel(
      
        condition = 'input.expert == true',
      
          sliderInput(inputId = 'Validity2',
                      label = HTML('<p style="color:white;">incremental validity of 2nd selection method:</p>'),
                      min = 0,
                      max = 1,
                      value = 0.3),
          selectInput(inputId = 'abs_rel', 
                      label = '',
                      c('absolute' = 'abs',
                        'relative' = 'rel'),
                      selected = 'abs'))),
  
  # body
  dashboardBody(
    
      #shinyDashboardThemes(theme = 'onenote'),
    
      includeMarkdown('main.md'),
    
      # define the main panel that contains all the output. Latter depends on the amateur vs. expert checkbox
      mainPanel(
        
        conditionalPanel(
          
            condition = 'input.expert == false',
            box(title = '1st step',
                plotlyOutput('barplot1_ama')),
            box(title = '2nd step',
                plotlyOutput('barplot2_ama'))),
        
        conditionalPanel(
          
            condition = 'input.expert == true',
            box(title = '1st step',
                plotlyOutput('barplot1_exp')),
            box(title = '2nd step',
                plotlyOutput('barplot2_exp'))),
        
        includeMarkdown('ref.md'))))


# Define server logic required to draw the plots
server <- function(input, output, session) {
  
  # create dataset for the first plot in amateur mode
  df1_ama <- reactive({
    status <- c('hired','rejected')
    decision_right <- c(round(true_positives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity_ama)), 0), 
                        round(true_negatives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity_ama)), 0))
    decision_wrong <- c(round(false_positives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity_ama)), 0), 
                        round(false_negatives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity_ama)), 0))
    df1_ama <- data.frame(status, decision_right, decision_wrong)})
  
  # create dataset for the second plot in amateur mode
  df2_ama <- reactive({
    status <- c('hired','rejected')
    decision_right <- c(round(true_positives(input$StaffRequirement, input$StaffRequirement2, df1_ama()$decision_right[1]/input$StaffRequirement, as.numeric(input$Validity_ama2)), 0), round(true_negatives(input$StaffRequirement, input$StaffRequirement2, df1_ama()$decision_right[1]/input$StaffRequirement, as.numeric(input$Validity_ama2)), 0))
    decision_wrong <- c(round(false_positives(input$StaffRequirement, input$StaffRequirement2, df1_ama()$decision_right[1]/input$StaffRequirement, as.numeric(input$Validity_ama2)), 0), round(false_negatives(input$StaffRequirement, input$StaffRequirement2, df1_ama()$decision_right[1]/input$StaffRequirement, as.numeric(input$Validity_ama2)), 0))
    df2_ama <- data.frame(status, decision_right, decision_wrong)})
  
  # create dataset for the first plots (absolute and relative) in expert mode
  df1_exp <- reactive({
    if(input$abs_rel == 'rel'){
      status <- c('hired','rejected')
      decision_right <- c(round(true_positives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity))/input$StaffRequirement, 2), 
                          round(true_negatives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity))/(input$Applicants-input$StaffRequirement),2))
      decision_wrong <- c(round(false_positives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity))/input$StaffRequirement, 2), 
                          round(false_negatives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity))/(input$Applicants-input$StaffRequirement),2))
      df1_exp <- data.frame(status, decision_right, decision_wrong)
      
    } else {
      
      status <- c('hired','rejected')
      decision_right <- c(round(true_positives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity)), 0), 
                          round(true_negatives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity)), 0))
      decision_wrong <- c(round(false_positives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity)), 0), 
                          round(false_negatives(input$Applicants, input$StaffRequirement, input$BaseRate/100, as.numeric(input$Validity)), 0))
      df1_exp <- data.frame(status, decision_right, decision_wrong)
      
    }
  })
  
  # create dataset for the second plots (absolute and relative) in expert mode
  df2_exp <- reactive({
    if(input$abs_rel == 'rel'){
      status <- c('hired','rejected')
      decision_right <- c(round(true_positives(input$StaffRequirement, input$StaffRequirement2, df1_exp()$decision_right[1], input$Validity2)/input$StaffRequirement2, 2), 
                          round(true_negatives(input$StaffRequirement, input$StaffRequirement2, df1_exp()$decision_right[1], input$Validity2)/(input$StaffRequirement-input$StaffRequirement2),2))
      decision_wrong <- c(round(false_positives(input$StaffRequirement, input$StaffRequirement2, df1_exp()$decision_right[1], input$Validity2)/input$StaffRequirement2, 2), 
                          round(false_negatives(input$StaffRequirement, input$StaffRequirement2, df1_exp()$decision_right[1], input$Validity2)/(input$StaffRequirement-input$StaffRequirement2),2))
      df2_exp <- data.frame(status, decision_right, decision_wrong)
      
    } else {
      
      status <- c('hired','rejected')
      decision_right <- c(round(true_positives(input$StaffRequirement, input$StaffRequirement2, df1_exp()$decision_right[1]/input$StaffRequirement, input$Validity2), 0), round(true_negatives(input$StaffRequirement, input$StaffRequirement2, df1_exp()$decision_right[1]/input$StaffRequirement, input$Validity2), 0))
      decision_wrong <- c(round(false_positives(input$StaffRequirement, input$StaffRequirement2, df1_exp()$decision_right[1]/input$StaffRequirement, input$Validity2), 0), round(false_negatives(input$StaffRequirement, input$StaffRequirement2, df1_exp()$decision_right[1]/input$StaffRequirement, input$Validity2), 0))
      df2_exp <- data.frame(status, decision_right, decision_wrong)
    }
  })
  
  
  # create first plot in amateur mode
  output$barplot1_ama <- renderPlotly({
    plot_ly(data = df1_ama()) %>%
      add_trace(x = ~status, y = ~decision_right,
                type = 'bar', name='right decision',
                text = ~decision_right, textposition = 'auto',
                hoverinfo = 'skip',
                marker = list(color = '#83AF9B')) %>%
      add_trace(x = ~status, y = ~decision_wrong, 
                type = 'bar', name='wrong decision',
                text = ~decision_wrong, textposition = 'auto',
                hoverinfo = 'skip',
                marker = list(color = '#FE4365')) %>%
      layout(yaxis = list(title = ''), xaxis = list(title = ''), barmode = 'stack') %>%
      config(displayModeBar = FALSE)
  })
  
  # create second plot in amateur mode
  output$barplot2_ama <- renderPlotly({
    plot_ly(data = df2_ama()) %>%
      add_trace(x = ~status, y = ~decision_right,
                type = 'bar', name='right decision',
                text = ~decision_right, textposition = 'auto',
                hoverinfo = 'skip',
                marker = list(color = '#83AF9B')) %>%
      add_trace(x = ~status, y = ~decision_wrong, 
                type = 'bar', name='wrong decision',
                text = ~decision_wrong, textposition = 'auto',
                hoverinfo = 'skip',
                marker = list(color = '#FE4365')) %>%
      layout(yaxis = list(title = ''), xaxis = list(title = ''), barmode = 'stack') %>%
      config(displayModeBar = FALSE)
    
  })
  
  # create first plots (absolute or relative) in expert mode
  output$barplot1_exp <- renderPlotly({
    
    if(input$abs_rel == 'rel'){
      plot_ly(data = df1_exp()) %>%
        mutate(decision_right = 100*decision_right,
               decision_wrong = 100*decision_wrong) %>%
        add_trace(x = ~status, y = ~decision_right,
                  type = 'bar', name='right decision',
                  text = ~paste(decision_right,'%'), textposition = 'auto',
                  hoverinfo = 'skip',
                  marker = list(color = '#83AF9B')) %>%
        add_trace(x = ~status, y = ~decision_wrong, 
                  type = 'bar', name='wrong decision',
                  text = ~paste(decision_wrong, '%'), textposition = 'auto',
                  hoverinfo = 'skip',
                  marker = list(color = '#FE4365')) %>%
        layout(yaxis = list(title = '', ticksuffix = '%'), xaxis = list(title = ''), barmode = 'stack') %>%
        config(displayModeBar = FALSE)
      
    } else {
      
      plot_ly(data = df1_exp()) %>%
        add_trace(x = ~status, y = ~decision_right,
                  type = 'bar', name='right decision',
                  text = ~decision_right, textposition = 'auto',
                  hoverinfo = 'skip',
                  marker = list(color = '#83AF9B')) %>%
        add_trace(x = ~status, y = ~decision_wrong, 
                  type = 'bar', name='wrong decision',
                  text = ~decision_wrong, textposition = 'auto',
                  hoverinfo = 'skip',
                  marker = list(color = '#FE4365')) %>%
        layout(yaxis = list(title = ''), xaxis = list(title = ''), barmode = 'stack') %>%
        config(displayModeBar = FALSE)
      
    } 
  })
  
  # create second plots (absolute or relative) in expert mode
  output$barplot2_exp <- renderPlotly({
    if(input$abs_rel == 'rel'){
      plot_ly(data = df2_exp()) %>%
        mutate(decision_right = 100*decision_right,
               decision_wrong = 100*decision_wrong) %>%
        add_trace(x = ~status, y = ~decision_right,
                  type = 'bar', name='right decision',
                  text = ~paste(decision_right, '%'), textposition = 'auto',
                  hoverinfo = 'skip',
                  marker = list(color = '#83AF9B')) %>%
        add_trace(x = ~status, y = ~decision_wrong, 
                  type = 'bar', name='wrong decision',
                  text = ~paste(decision_wrong, '%'), textposition = 'auto',
                  hoverinfo = 'skip',
                  marker = list(color = '#FE4365')) %>%
        layout(yaxis = list(title = '', ticksuffix = '%'), xaxis = list(title = ''), barmode = 'stack') %>%
        config(displayModeBar = FALSE)
      
    } else {
      
      plot_ly(data = df2_exp()) %>%
        add_trace(x = ~status, y = ~decision_right,
                  type = 'bar', name='right decision',
                  text = ~decision_right, textposition = 'auto',
                  hoverinfo = 'skip',
                  marker = list(color = '#83AF9B')) %>%
        add_trace(x = ~status, y = ~decision_wrong, 
                  type = 'bar', name='wrong decision',
                  text = ~decision_wrong, textposition = 'auto',
                  hoverinfo = 'skip',
                  marker = list(color = '#FE4365')) %>%
        layout(yaxis = list(title = ''), xaxis = list(title = ''), barmode = 'stack') %>%
        config(displayModeBar = FALSE)
      
    }  
  })
  
  
  # create intermediate dataset which only contains the rows which match the selection of Validity_ama
  dft <- reactive({validity_data %>% filter(val1 == as.numeric(input$Validity_ama))})
  
  # update available options in the second validity dropdown menu depending on the method selected in the first validity dropdown (amateur mode)
  observeEvent(input$Validity_ama,{
    updateSelectInput(session,'Validity_ama2',
                      choices=setNames(dft()$val2, dft()$method2))})
  
}

shinyApp(ui, server)