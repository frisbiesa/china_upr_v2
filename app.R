library(tidyverse)
library(shiny)
library(maps)
library(sf)
library(spData)
library(lubridate)

## data on china 

debt_stock <- read.csv("debt_stock_china.csv") %>% 
  select(-country_code, -ISO)

comtrade <- rbind(read.csv("comtrade_2018.csv"), read.csv("comtrade_2013.csv"), 
                  read.csv("comtrade_2008.csv"), read.csv("comtrade_2003.csv"))

comtrade_china_total <- comtrade %>% 
  select(Year, Trade.Flow, Partner, Partner.ISO, Trade.Value..US..) %>% 
  rename(year = Year, trade_flow = Trade.Flow, country = Partner, ISO = Partner.ISO, total_usd = Trade.Value..US..) %>% 
  pivot_wider(names_from = "trade_flow", values_from = "total_usd") %>% 
  filter(country != "World")

worlde <- world %>% 
  rename(country = name_long)
  
mapamundi <- left_join(worlde, debt_stock, by = "country") %>% 
  left_join(comtrade_china_total, by = c("country", "year"))


ui <- fluidPage(
  fluidRow(
    column(width = 12,
           align = "center",
           tags$h1("Financial and trade flows with China in the developing world", 
                   tags$style('head { face: bold }'))
    )
  ), 
  fluidRow(
    column(width = 12,
           align = "center",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "variable",
                           label = "Select a variable to visualize",
                           choices = c("Debt to china as percentage of GDP", 
                                       "Imports from China", 
                                       "Exports to China")),
               sliderInput(inputId = "date", 
                           label = "Date:", 
                           min = 2000, 
                           max = 2017, 
                           value = 2017)),
             mainPanel(
               plotOutput("map", 
                          width = "100%")
             ), fluid = TRUE
           )
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
      geom_sf(data = data(), aes(fill = { if (input$variable == "Debt to china as percentage of GDP") {
        china_debt_gdp 
      } else if (input$variable == "Imports from China") {
        Export
      } else if (input$variable == "Exports to China") {
        Import
      }}), alpha = 3/5) +
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



