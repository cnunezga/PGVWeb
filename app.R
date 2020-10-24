###############################################################################
###############################################################################
###############################################################################
##                                                                           ##
## This is a Shiny web application. You can run the application by clicking  ##
## the 'Run App' button above.                                               ##  
##                                                                           ##
###############################################################################
###############################################################################
###############################################################################

library(shiny)
library(shinydashboard)
library(DT)
library(tibble)
library(vcfR)
library(DT)
library(RSQLite)

# Define UI for application 

## Define dashboardHeader

header <- dashboardHeader(
  title = "Option Menu"
  # , uiOutput("logoutbtn")
  )

## Define dashboardSidebar

sidebar <- dashboardSidebar(
  sidebarUserPanel("User Name",
                   subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # Image file should be in www/ subdir
                   image = "userimage.png"
  ),
  # Input: Select a file ----
  fileInput("Proband", "Choose VCF File",
            accept = c("text/plain",".vcf")),
  actionButton("button", "Convert!"),
  sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
             badgeColor = "green"),
    menuItem("Charts", icon = icon("bar-chart-o"),
             menuSubItem("Sub-item 1", tabName = "subitem1"),
             menuSubItem("Sub-item 2", tabName = "subitem2")
    )
  )
)

body <- dashboardBody(
  DT::dataTableOutput("vcfData"),
  tabItems(
    tabItem("dashboard",
            div(p("Dashboard tab content"))
    ),
    tabItem("widgets",
            "Widgets tab content"
    ),
    tabItem("subitem1",
            "Sub-item 1 tab content"
    ),
    tabItem("subitem2",
            "Sub-item 2 tab content"
    )
  )
  
)




body <- dashboardBody(
  DT::dataTableOutput("test")
)

ui <- dashboardPage(
  header,
  sidebar,
  body,
  title = "PGVWeb"
)

# Define server logic required to draw a histogram
#server = function(input, output) { }

server <- function(input, output) {
  
  observeEvent(input$button, {
    req(input$Proband)
    inFile <- input$Proband
    
    if (is.null(inFile))
      return(NULL)
      
    vcf <- read.vcfR(inFile$datapath)
    Z <- vcfR2tidy(vcf, format_fields = c("GT", "DP"))
    
    info <- Z$fix
    
    gt <- extract.gt(vcf)
    
    
    bc <- as_tibble(data.frame(Z$fix, Z$gt))
    
    output$test <- DT::renderDataTable({
      
      DT::datatable(
        info,
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 650,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       # paging = TRUE,
                       # pageLength = 25,
                       buttons = list('excel',
                                      list(extend = 'colvis', targets = 0, visible = FALSE)),
                       dom = 'lBfrtip',
                       fixedColumns = TRUE), 
        rownames = FALSE)
    })  
    
  })
  
  
}


# Run the application 
shinyApp(ui, server)
#runApp(list(ui = ui, server = server), launch.browser = TRUE)
