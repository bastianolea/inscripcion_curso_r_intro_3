library(surveydown)

# base de datos
# https://surveydown.org/docs/storing-data

# para configurar:
# sd_db_config()

# db <- sd_db_connect(ignore = TRUE)
db <- sd_db_connect()

ui <- sd_ui()

server <- function(input, output, session) {
  
  # # pregunta condicional
  # sd_show_if(
  #   input$contacto == "si" ~ "correo" 
  # )
  
  sd_server(db = db)
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
