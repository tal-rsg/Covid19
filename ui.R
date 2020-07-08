#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(leaflet)
library(plotly)

# Define UI for application that draws a histogra
    fluidPage(

        navbarPage(
            theme = shinytheme("darkly"),  # <--- To use a theme, uncomment this
            "COVID-19 on World highlight Rio",
            tabPanel("Confirmed",
                     mainPanel(
                         tabsetPanel(
                             tabPanel("World",
                                      h2("Casos pelo mundo"),
                                      h4("Base de dados: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases"),
                                      fluidRow(
                                        column(width = 6, h4("Casos no Mundo", align = "center"), DT::dataTableOutput("table")),
                                        column(width = 6, h4("Mapa Mundo", align = "center"), leafletOutput("map"))
                                        ),
                                      fluidRow(
                                        column(width = 6, h4("Curva Crescimento", align= "center"), plotlyOutput("curva")),
                                        column(width = 6, h4("Curva Logarítimica", align = "center"),plotlyOutput("loga"))
                                      )
                                      
                             ),
                             tabPanel("Rio de Janeiro",
                                      h2("Confirmados e Óbitos no Estado do Rio"),
                                      h4("Base de dados: https://data.brasil.io/dataset/covid19/_meta/list.html"),
                                      fluidRow(
                                        column(width = 6, h4("Casos no Estado do RIo", align = "center"), leafletOutput("maprj")),
                                        column(width = 6, h4("Principais Estados BR", align = "center"), plotlyOutput("graphestados"))
                                      ),
                                      h4("Casos nos Municípios de Três Rios e Paraíba do Sul"),
                                      fluidRow(
                                      column(width = 6,h4("Casos diários em TR e PS", align = "center"), plotlyOutput("curvatrps")),
                                      column(width = 6,h4("Logarítimica em TR e PS", align = "center"),   plotlyOutput("logtrps"))
                             
                     )
                         )
            )
            )
            ),
            tabPanel("Deaths", 
                     mainPanel(
                       tabsetPanel(
                         tabPanel("World",
                                  h2("Óbitos pelo mundo"),
                                  h4("Base de dados: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases"),
                                  tableOutput("obt"),
                                  h2("Mapa"),
                                  leafletOutput("map_obt"),
                                  h2("Curva"),
                                  plotlyOutput("curva_obt"),
                                  h2("Curva Logarítimica"),
                                  plotlyOutput("loga_obt")
                         )
                         )#,
         #   tabPanel("Navbar 3", "This panel is intentionally left blank")
        )
     )
  )
)



