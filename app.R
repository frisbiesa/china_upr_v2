library(tidyverse)
library(shiny)
library(maps)
library(sf)
library(spData)
library(lubridate)

debt_stock <- read.csv("debt_stock_china.csv") %>% 
  select(-country_code, -ISO)


worlde <- world %>% 
  rename(country = name_long)
  

mapamundi <- left_join(worlde, debt_stock, by = "country")



ui <- fluidPage(
  fluidRow(
    column(width = 12,
           align = "center",
           tags$h1("Evolution of sovereign debt to China", 
                   tags$style('head { face: bold }'))
    )
  ), 
  fluidRow(
    column(width = 12,
           align = "center",
           sliderInput(inputId = "date", 
                           label = "Date:", 
                           min = 2000 , 
                           max = 2017, 
                           value = 2000)
             ),
    column(width = 12,
           align = "center",
           plotOutput("map", 
                      width = "100%")
             )
           )
  )


server <- function(input, output) {
  
  data <- reactive({
    mapamundi %>% 
      filter(year == input$date) 
  })
  
  output$map <- renderPlot({
    ggplot() +
      geom_sf(data = world) +
      geom_sf(data = data(), aes(fill = china_debt_gdp), alpha = 3/5) +
      labs(title = "", 
           fill = "",
           caption = "Source: sonnet") +
      theme_minimal() +
      theme(axis.text.x = element_blank(), 
            axis.text.y = element_blank(),
            plot.caption = element_text(size = 12)) +
      guides(color = FALSE, 
             alpha = FALSE,
             fill = guide_colorbar(barwidth = 0.5, 
                                   barheight = 10, title.position = "left",
                                   title.theme = element_text(angle = 90),
                                   title.hjust = .5))
   
  },height = 600, width = 600)
  
}



shinyApp(ui = ui, server = server)



