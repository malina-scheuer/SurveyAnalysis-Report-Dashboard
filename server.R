library(shinydashboard)

# External R script as data source

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$module_selection <- renderText(
    input$module_selection
    
  )
  
# Value boxes for dimensions
  output$relevance <- renderValueBox({
    valueBox(
      "Relevance",
      value = {x <- input$module_selection
      round(data_modules$relevance[data_modules$course_module==x])},
      color = if ({x <- input$module_selection
      round(data_modules$relevance[data_modules$course_module==x])} < 80) {
        "orange"} else {"olive"}
          )
    })
  output$skills <- renderValueBox({
    valueBox(
      "Skill development",
      value = {x <- input$module_selection
      round(data_modules$skills[data_modules$course_module==x])},
      color = if ({x <- input$module_selection
      round(data_modules$skills[data_modules$course_module==x])} < 80) {
        "orange"} else {"olive"}
    )
  })
  output$TQ <- renderValueBox({
    valueBox(
      "Teaching quality",
      value = {x <- input$module_selection
      round(data_modules$TQ_f[data_modules$course_module==x])},
      color = if ({x <- input$module_selection
      data_modules$TQ_f[data_modules$course_module==x]} < 80) {
        "orange"} else {"olive"}
    )
  })
  output$difficulty <- renderValueBox({
    valueBox(
      "difficulty",
      value = {x <- input$module_selection
      data_modules$lev_difficulty[data_modules$course_module==x]}
    )
  })


})


