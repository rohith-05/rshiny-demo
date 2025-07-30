library(shiny)
library(formatters)
library(gt)
library(tidyverse)
library(rlistings)
library(shinythemes)
library(DT)

ui <- navbarPage(theme = shinytheme("darkly"),"MS123 Analysis",
                   tabPanel("Data",
                            mainPanel(
                              tabsetPanel(
                                tabPanel("subject data",
                                         mainPanel(
                                           dataTableOutput("s1")
                                         )),
                             
                            tabPanel("Data",
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel("AE",
                                                  mainPanel(
                                                    dataTableOutput("adae")
                                                  )))
                            ))))),
                 tabPanel("Table",
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Lab Table",
                                       mainPanel(
                                         verbatimTextOutput("Table 1",
                                                            width = 14)
                                       )),
                              
                              tabPanel("Data",
                                       mainPanel(
                                         tabsetPanel(
                                           tabPanel("Saftey Table",
                                                    mainPanel(
                                                      dataTableOutput("ae")
                                                    )))
                                       )))))
                 )

server <- function(input,output, session)
{
 output$s1 <- renderDT(ex_adsl, options = list(pageLength = 5)) 
 output$adae <- renderDT(ex_adae, options = list(pageLength = 5)) 
 data <- reactive({ex_adlb %>%
     group_by(USUBJID,PARAMCD) %>%
     arrange(USUBJID,PARAMCD, AVAL)%>%
     mutate(
       #first.
       MIN = if_else(row_number(AVAL) == 1, "Y", ""),
       #last.
       MAX = if_else(row_number(AVAL) == n(), "Y", "")
     )%>% filter(SUBJID =="id-105")%>% select(USUBJID,PARAMCD,AVAL,AVISIT, MIN, MAX)})
 output$Table1 <- renderPrint({
   lsting <- as_listing(
     df = data(),
     disp_cols = c( "PARAMCD","AVAL", "MIN", "MAX"),
     key_cols = c("USUBJID", "AVISIT"),
     main_title = "Lab listing",
     subtitles = c("Other sub titles1", "Other sub titles2"),
     main_footer = c("Footnote1", "Footnote2"),
     prov_footer = "Source:ADLB, data:"
   )
   lsting
 })
 
 output$plot1 <- renderPlotly({
   plot_ly(data = data(), x = ~get(input$x), y = ~get(input$y), color = ~get(input$color), type = "scatter", mode = "markers")%>%
     layout(
       title = "Customized Scatter Plot",
       xaxis = list(title = input$x),
       yaxis = list(title = input$y)
     )
 })
}
shinyApp(ui, server)