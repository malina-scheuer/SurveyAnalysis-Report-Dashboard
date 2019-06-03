library(shinydashboard)
#library(leaflet)

source("data.R") 

# Header
  header <- dashboardHeader(
    title = "Course evaluation"
  )

# Sidebar
  sidebar <- dashboardSidebar(
    sidebarMenu(
  
      menuItem("Module",
               tabName = "module")
    )
  )

# Body
  body <- dashboardBody(
    # Page for particular module / course
    tabItems(
      tabItem(tabName="module",
              
              # Selection menu for the module
              selectInput(
                inputId = "module_selection",
                label = "Module evaluation",
                choices = data_modules$course_module,
                width="100%"),
              
              # Output of the module name
              # textOutput("module_selection"),
        
        fluidRow(
          # all value boxes
          valueBoxOutput("relevance", width=4),
          valueBoxOutput("skills",width=4),
          valueBoxOutput("TQ", width = 4),
          valueBoxOutput("difficulty", width = 4)
          )
        )
      ),
    
    # Style change for the body (custom CSS)
    tags$head(
      tags$style(
        HTML('
            /* navbar (rest of the header) */
                .skin-blue .main-header .navbar {
                                  background-color: #4891DC;
                                  }
             /* logo  */
                .skin-blue .main-header .logo {
                                  background-color: #4891DC; 
                                  }
            /* logo when hovered */
                .skin-blue .main-header .logo:hover {
                                  background-color: white;
                                  color: #4891DC;
                                  }
            /* active selected tab in the sidebarmenu */
                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
                                  background-color: #ecf0f5;
                                  color: #4891DC;
                                  }
           /* toggle button when hovered  */                    
                 .skin-blue .main-header .navbar .sidebar-toggle:hover {
                                  background-color: #ecf0f5;
                                  color: black;
                                  }
                              
          /* main sidebar */
                 .skin-blue .main-sidebar {
                                  background-color: #4891DC;
                                  }
          /* other links in the sidebarmenu */
                   .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                  background-color: #eeeee;
                                  color: #ffffff;
                                  }
           /* other links in the sidebarmenu when hovered */
                    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                  background-color: white;
                                  color: black;
                                  }  
            ') ) )
  )

# General UI (put all parts together)
ui <- dashboardPage(
  header,
  sidebar,
  body
  )
