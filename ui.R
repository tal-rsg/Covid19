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
            "COVID-19 on World highlight Brazil",
            tabPanel("Confirmed",
                     mainPanel(
                         tabsetPanel(
                             tabPanel("World",
                                      h2("Casos pelo mundo"),
                                      h4("Base de dados: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases"),
                                      DT::dataTableOutput("table"),
                                      h2("Mapa"),
                                      leafletOutput("map"),
                                      h2("Curva"),
                                      plotlyOutput("curva"),
                                      h2("Curva Logarítimica"),
                                      plotlyOutput("loga")
                             ),
                             tabPanel("TR e PS", 
                                      h2("Casos nos Municípios de Três Rios e Paraíba do Sul"),
                                      fluidRow(
                                      column(width = 6,h4("Casos diários", align = "center"), plotlyOutput("curvatrps")),
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



