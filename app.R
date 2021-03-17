library(tidyverse)
library(shiny)
library(maps)
library(spData)
library(lubridate)
library(devtools)
library(sp)
library(plotly)
library(shinyWidgets)

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

mapamundi <- left_join(worlde, debt_stock, by = "country")
mapamundi$debt_usd <- str_remove_all(mapamundi$debt_usd, ",")
mapamundi$debt_usd <- as.numeric(mapamundi$debt_usd)

ui <- fluidPage(
  fluidRow(
    column(width = 12,
           align = "center",
           tags$h1("Financial and trade flows with China in the developing world",
                   tags$style('head { face: bold }'))
    )
  ),
  fluidRow(
    column(width = 6,
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
                           value = 2000)
             )
    ,
    column(width = 6,
           selectInput(inputId = "fill",
                       label = "select variable",
                       c("aggregate debt" = "debt_usd",
                         "debt to GDP" = "china_debt_gdp"))
           ),

    fluidRow(
      column(width = 12,
           align = "center",
           plotlyOutput("map",
                      width = "100%")
             )
           )
  )
)


server <- function(input, output) {

  data <- reactive({
    mapamundi %>%
      filter(year == input$date) %>%
      rename(graph_variable = input$fill)
  })


  scale <- reactive({
    if (input$fill == "debt_usd"){
      s <- c(1, 200000000)
    }
    else if (input$fill == "china_debt_gdp"){
      s <- c(1, 30)
    }
  })

  plot <- reactive({
  plt <- ggplot() +
    geom_sf(data = world) +
    geom_sf(data = data(),
            aes(group = year, fill = graph_variable),
            alpha = 3/5) +
    labs(title = "",
         fill = "",
         caption = "Source: 'Chinas Overseas Lending,' NBER, Sebastian Horn, Carmen M. Reinhart & Christoph Trebesch") +
    scale_fill_viridis_c(option = "magma", limits = scale()) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.caption = element_text(size = 12))+
    guides(color = FALSE, alpha = FALSE,
           fill = guide_colorbar(barwidth = 0.5,
                                 barheight = 10, title.position = "left",
                                 title.theme = element_text(angle = 90),
                                 title.hjust = .5))
  plt <- ggplotly(plt, tooltip= c("country", "fill"))
  })

  output$map <- renderPlotly({plot()})

}

shinyApp(ui = ui, server = server)
