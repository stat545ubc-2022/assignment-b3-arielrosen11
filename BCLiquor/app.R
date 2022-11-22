library(shiny)
library(tidyverse)
library(shinythemes)

bcl <- read_csv("~/Desktop/BCLiquor/bcl-data.csv")

ui <- fluidPage(
  theme = shinytheme("cerulean"), 
  titlePanel("BC Liquor Store Data"),
  h5("Ariel Rosen STAT 545B Assignment B-3"),
  br(),
  a(href = "https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", "Link to the original data set"), 
  sidebarPanel(
  sliderInput("priceInput", "Price", 0, 100,
              value = c(25, 40), pre = "$"),
  radioButtons("typeInput", "Type", 
               choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"))
  ),
  
  mainPanel(
    tabsetPanel(tabPanel("Plot", plotOutput("alcohol_hist")), 
                 tabPanel("Table", tableOutput("data_table"))
                #Using tabsetPanel to create separate tabs for the plot and the table which makes it easier to see the slider
                #and buttons at the same time as seeing the output table or plot
  ))
)

server <- function(input, output) {
  filtered_data <- 
    reactive({bcl%>% filter(Price > input$priceInput[1] & 
                              Price < input$priceInput[2] &
                              Type == input$typeInput)
      })
  
  output$alcohol_hist <-renderPlot({
     filtered_data()%>%
           ggplot(aes(Alcohol_Content))+ 
           geom_histogram(colour="darkblue", fill="lightblue")#can wrap ggplot call in curly braces if you want it to be on more than 1 line 
    }) 
  
  output$data_table <- 
    renderTable({
   filtered_data()
    })
}
shinyApp(ui = ui, server = server)


