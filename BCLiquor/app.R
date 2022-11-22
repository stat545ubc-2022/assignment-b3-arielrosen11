library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)

bcl <- read_csv("~/Desktop/BCLiquor/bcl-data.csv")

ui <- fluidPage(
  theme = shinytheme("cerulean"), 
  titlePanel("BC Liquor Store Data"),
  h5("Ariel Rosen STAT 545B Assignment B-3"),
  br(),
  a(href = "https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", "Link to the original data set"), 
  sidebarPanel(
  sliderInput("priceInput", "Price", 0, 200,
              value = c(25, 60), pre = "$"),
  radioButtons("typeInput", "Type", 
               choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")), 
  selectInput("countryInput",  #adding a list of countries in to select from using a select input dropdown 
              label = "Country", 
                     choices = list("CANADA",
                                    "UNITED STATES OF AMERICA", 
                                    "FRANCE", 
                                    "IRELAND", 
                                    "ITALY", 
                                    "BRAZIL",
                                    "UNITED KINGDOM", 
                                    "SPAIN", 
                                    "GERMANY", 
                                    "PORTUGAL", 
                                    "ARGENTINA", 
                                    "ISRAEL", 
                                    "BELGIUM", 
                                    "MEXICO", 
                                    "CZECH REPUBLIC", 
                                    "AUSTRALIA",
                                    "SOUTH AFRICA", 
                                    "CHINA", 
                                    "CHILE", 
                                    "NETHERLANDS", 
                                    "JAPAN",
                                    "CUBA", 
                                    "GREECE", 
                                    "BULGARIA", 
                                    "POLAND",
                                    "NEW ZEALAND"), 
                     selected = "CANADA")),
  
  mainPanel(
    tabsetPanel(tabPanel("Plot", plotOutput("alcohol_hist")), 
                 tabPanel("Table",dataTableOutput("data_table"))
                #Using tabsetPanel to create separate tabs for the plot and the table which makes it easier to see the slider
                #and buttons at the same time as seeing the output table or plot
                #Using dataTableOutput to make the table interactive which is useful for example to search for specific features of the liquor
  ))
)

server <- function(input, output) {
  filtered_data <- 
    reactive({bcl%>% filter(Price > input$priceInput[1] & 
                              Price < input$priceInput[2] &
                              Type == input$typeInput &
                              Country == input$countryInput) #adding an option to choose which country the alcohol is from which is useful to 
                                                             #look at alcohol content and price in products from specific regions of the world 
      })
  
  output$alcohol_hist <-renderPlot({
     filtered_data()%>%
           ggplot(aes(Alcohol_Content))+ 
           geom_histogram(colour="darkblue", fill="lightblue")
    }) 
  
  output$data_table <- #using renderDataTable to make the table interactive which is useful for example to search for specific features of the liquor 
    renderDataTable({
   filtered_data()
    })
}
shinyApp(ui = ui, server = server)


