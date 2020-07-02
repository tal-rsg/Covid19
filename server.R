#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rvest)
library(rmarkdown)
library(leaflet)
library(fs)
library(plotly)
library(lubridate)
library(ggplot2)
library(shinythemes)
library(googlesheets4)
library(httr)
library(jsonlite)
library(jsonlite)
library(DT)

#lendo arquivos estado rj www.brasil.io

get_rj = GET("https://brasil.io/api/dataset/covid19/caso_full/data?state=RJ&had_cases=True&page_size=10000")


content_rj <- content(get_rj, "text", encoding = "utf-8")

json_rj <- fromJSON(content_rj,flatten = TRUE)

df_rj <- as_tibble(json_rj$results) %>% 
    mutate(date = ymd(date))

df_rj %>% glimpse()

df_rj %>% 
    filter(city %in% c("Três Rios", "Paraíba do Sul")) %>% 
    mutate(Diario = lag(last_available_confirmed),
           Conf_daily=last_available_confirmed-Diario) %>% 
    plot_ly(x = ~date, y = ~last_available_confirmed) %>%
    add_lines(linetype = ~city) #%>% 
    #layout(yaxis = list(type = "log"))


#lendo arquivos mundiais
dir_ls("data")

df_world <- read_csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv")

df_world_obt <- read_csv("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv")


#lendo arquivos no Google Sheets
gs4_deauth()
df_tr <- read_sheet("https://docs.google.com/spreadsheets/d/1fK3V_5Jy5om4J6drY5UG_ctnH18pyyVy7h3QAIQcPeI/edit#gid=1134744633")

#df_tr <- read_csv("data/Dados TR - Plan1.csv")

df_tr %>% glimpse()

df_tr2 <- df_tr %>% 
    select(1,2) %>%
    mutate(Diario = lag(Confirmados),
           Conf_daily=Confirmados-Diario,
           Cidade="Três Rios")

#Data= as.Date(mdy(Data)))

df_tr2 %>%  glimpse()


df_ps <- read_sheet("https://docs.google.com/spreadsheets/d/145m2NTHssbkoYjOBkL9sJOTsr3JiNI0YsbIHRhD8lpM/edit#gid=811385160", sheet = "Paraiba do sul")

#df_ps <- read.csv("data/Dados PS - Paraiba do sul.csv")

df_ps2 <- df_ps %>% 
    select(1,2) %>% 
    mutate(Diario = lag(Confirmados),
           Conf_daily=Confirmados-Diario,
           Cidade="Paraiba do Sul")
           
#Data= as.Date(mdy(Data)))

df_ps2 %>% glimpse()

df_trps <- union_all(df_tr2, df_ps2)

df_world2 <- gather(df_world, 'Data', 'Valor', 5:length(df_world) )

df_world_obt2 <- gather(df_world_obt, 'Data', 'Valor', 5:length(df_world_obt))

df_world3 <- df_world2 %>% 
    mutate(Data2 = str_replace_all(Data, "/", "-")) %>% 
    mutate(Data3= as.Date(mdy(Data2)))

df_world_obt3 <- df_world_obt2 %>% 
    mutate(Data2 = str_replace_all(Data, "/", "-")) %>% 
    mutate(Data3= as.Date(mdy(Data2)))


df_world4 <- df_world3 %>% 
    select(2,3,4,5,6,8) %>% 
    rename(Country=`Country/Region`) %>% 
    arrange(Data3) %>% 
    mutate(Diario = lag(Valor),
           Conf_daily=Valor-Diario,
           Valor2 = format(Valor, big.mark=","))
           

df_world_obt4 <- df_world_obt3 %>% 
    select(2,3,4,5,6,8) %>% 
    rename(Country=`Country/Region`) %>% 
    arrange(Data3) %>% 
    mutate(Diario = lag(Valor),
           Conf_daily=Valor-Diario,
           Valor2 = format(Valor, big.mark=","))


df_map <- df_world4 %>% 
    as_tibble() %>% 
    select(1,2,3,5,6) %>% 
    filter(Data3 >= today() - days(1)) %>% 
    mutate(Casos = as.double(Valor),
           Valor2 = format(Valor, big.mark=","))


df_map_obt <- df_world_obt4 %>% 
    as_tibble() %>% 
    select(1,2,3,5,6) %>% 
    filter(Data3 >= today() - days(1)) %>% 
    mutate(Casos = as.double(Valor),
           Valor2 = format(Valor, big.mark=","))


#filtra pais confirmados
df_teste <- df_world4 %>% 
    filter(Country %in% c("Germany","Russia","Spain","UK","Brazil","Italy")) %>%
    group_by(Country, Data3) %>% 
    summarize(Casos=sum(Valor)) %>% 
    mutate(Diario = lag(Casos),
           Conf_daily=Casos-Diario)

#filtra pais obitos
df_obitos <- df_world_obt4 %>% 
    filter(Country %in% c("US","United Kingdom","Italy","France","Spain","Brazil")) %>%
    group_by(Country, Data3) %>% 
    summarize(Casos=sum(Valor)) %>% 
    mutate(Diario = lag(Casos),
           Conf_daily=Casos-Diario)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$map <- renderLeaflet({ 
        pops <- str_glue("<b> {df_map$Country} </b> <br/>
                 Casos confirmados: {df_map$Valor2}")
        
        df_map %>%
            leaflet() %>%
            addTiles() %>% 
            # utilizando o sqrt para melhor visualizar os lugares com menos casos
            addCircles(radius=~sqrt(as.double(Valor)) * 1000, weight=0.8, popup=~pops, color = c('blue'))
        #addCircleMarkers(radius = ~sqrt(as.double(Total)), weight=1, color = c('red'), popup=~pops)
    })
    
    output$map_obt <- renderLeaflet({ 
        pops <- str_glue("<b> {df_map_obt$Country} </b> <br/>
                 Óbitos confirmados: {df_map_obt$Valor2}")
        
        df_map_obt %>%
            leaflet() %>%
            addTiles() %>% 
            # utilizando o sqrt para melhor visualizar os lugares com menos casos
            addCircles(radius=~sqrt(as.double(Valor)) * 1000, weight=0.8, popup=~pops, color = c('red'))
        #addCircleMarkers(radius = ~sqrt(as.double(Total)), weight=1, color = c('red'), popup=~pops)
    })
    
table_world <- reactive(
   DT::datatable(
        df_map %>% 
           select(1,7) %>% 
            rename(Casos=Valor2) %>%
            arrange(desc(Casos)) %>% 
            head(10),
        rownames = FALSE,
        options = list(
            dom = 'frtp',
            style = "bootstrap",
            lengthMenu = c(seq(5, 150, 5))
            
        )
    ) %>% formatStyle (c("Country", "Casos"), color = 'white', backgroundColor = '#988380', fontWeight = 'bold')
)    
    
    output$table <- DT::renderDataTable({
       
        # table_world <- df_map %>% 
         #   select(1,7) %>% 
          #  rename(Casos=Valor2) %>%
          #  arrange(desc(Casos)) %>% 
          #  head(10)
        
        table_world()
    })
    
    output$obt <- renderTable({
        data2 <- df_map_obt %>% 
            select(1,7) %>% 
            rename(Obitos=Valor2) %>% 
            arrange(desc(Obitos))
        
        data2  %>% head(10)
    })
    
    output$curva <- renderPlotly({
        plot_ly(df_teste, x = ~Data3, y = ~Casos) %>%
            add_lines(linetype = ~Country)
        
    })
    
    output$curva_obt <- renderPlotly({
        plot_ly(df_obitos, x = ~Data3, y = ~Casos) %>%
            add_lines(linetype = ~Country)
        
    })
    
    output$loga <- renderPlotly({
        plot_ly(df_teste, x = ~Data3, y = ~Casos) %>%
            add_lines(linetype = ~Country) %>% 
            layout(df_teste, yaxis = list(type = "log"))
    })
    
    output$loga_obt <- renderPlotly({
        plot_ly(df_obitos, x = ~Data3, y = ~Casos) %>%
            add_lines(linetype = ~Country) %>% 
            layout(df_teste, yaxis = list(type = "log"))
    })
    
    output$curvatrps <- renderPlotly({
        plot_ly(df_trps, x = ~Data, y = ~Confirmados) %>%
            add_lines(linetype = ~Cidade)
        
    })
    
    output$logtrps <- renderPlotly({
        plot_ly(df_trps, x = ~Data, y = ~Confirmados) %>%
            add_lines(linetype = ~Cidade) %>% 
            layout(df_trps, yaxis = list(type = "log"))
    })
})
