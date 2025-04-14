# Carregar pacotes necessários
library(dplyr)
library(tidyr)
library(data.table)
library(scales)
library(markdown)
library(htmlwidgets)
library(shinyWidgets)
library(RColorBrewer)
library(knitr)
library(maps)
library(bslib)
library(shiny)
library(tidyverse)
library(readr)
library(colourpicker)

# Carregar e ajustar os dados
dados_tratados <- read_csv("Dados_auxiliares/dados_apos_coluna_mes.csv", 
                           quote = "\"", 
                           locale = locale(encoding = "UTF-8"), 
                           show_col_types = FALSE)

dados <- dados_tratados %>%
  mutate(
    PPG = as.numeric(PPG),
    RPG = as.numeric(RPG),
    APG = as.numeric(APG)
  ) %>%
  slice(-507) %>%
  mutate(
    PPG = replace_na(PPG, 0),
    RPG = replace_na(RPG, 0),
    APG = replace_na(APG, 0)
  ) %>%
  filter(!is.na(Peso))

# Interface do usuário

ui <- fluidPage(
  titlePanel("Dashboard: Gráfico em Linha"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Selecione a variável do eixo X:", 
                  choices = c("PPG", "APG", "RPG", "Altura", "Peso")),
      selectInput("y_var", "Selecione a variável do eixo Y:", 
                  choices = c("PPG", "APG", "RPG", "Altura", "Peso")),
      colourInput("line_color", "Selecione a cor da linha:", value = "blue"),
      sliderInput("x_lim", "Limites do eixo X:", 
                  min = 0, max = 30, value = c(0, 30)),
      sliderInput("y_lim", "Limites do eixo Y:", 
                  min = 0, max = 30, value = c(0, 30))
    ),
    
    mainPanel(
      plotOutput("line_plot")
    )
  )
)

server <- function(input, output) {
  output$line_plot <- renderPlot({
    ggplot(data = dados, aes_string(x = input$x_var, y = input$y_var)) +
      geom_line(color = input$line_color) +
      xlim(input$x_lim) +
      ylim(input$y_lim) +
      labs(x = input$x_var, y = input$y_var, title = "Gráfico em Linha") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
