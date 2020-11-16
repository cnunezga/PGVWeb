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
library(splitstackshape)
library(base)
library(stringi)
options(shiny.maxRequestSize=7000*1024^2)


# Define UI for application 


############################
##    Define LoginPage    ##
############################

Logged = FALSE;

loginpage <-  tagList(
  div(id = "login", 
      wellPanel(
        shiny::h2("LOG IN", class = "text-center", 
                  style = "padding-top: 0;color:#333; font-weight:600;"),
        textInput("userName", placeholder="Username",
                  label = tagList(icon("user-circle"), "Username")),
        passwordInput("passwd", placeholder="Password", 
                      label = tagList(icon("unlock-alt"), "Password")),
        br(),
        div(
          style = "text-align: center;",
          actionButton("Login", "SIGN IN", 
                       style = "color: white; background-color:#3c8dbc;
                       padding: 10px 15px; width: 150px; 
                       cursor: pointer; font-size: 18px; 
                       font-weight: 600;"),
          shinyjs::hidden(
            div(id = "nomatch",
                tags$p("Oops! Incorrect username or password!",
                       style = "color: red; font-weight: 600; 
                       padding-top: 5px;font-size:16px;", 
                       class = "text-center")))
        )
      )
  ),
  
  tags$style(type="text/css", 
             "#login {
             font-size:13px;
             text-align:left; 
             position:absolute; 
             width: 500px; 
             max-width: 100%; 
             padding: 20px;
             top:40%;left:50%;
             margin-top:-100px;
             margin-left:-150px;}")
)

header <- dashboardHeader( title = "PGVWeb", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(
  tags$head(tags$style(HTML('.content-wrapper, .main-footer, .right-side {margin-left: 120px;}'))),
  uiOutput("sidebarpanel")) 
body <- dashboardBody(uiOutput("body"), shinyjs::useShinyjs())

ui<-dashboardPage(title = "PGVWeb",
                  skin = "red", header, sidebar, body)

############################
## Define dashboardHeader ##
############################

header <- dashboardHeader(
  
  title = "Option Menu",
  
  uiOutput("logoutbtn")
  
)


###################################
## Define DashboardSidebarOutput ##
###################################

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    
    #menuItem("Main Menu", tabName = "dashboard", icon = icon("dashboard")),
    
    #########################################
    ########## Uploading VCF Files ##########
    #########################################
    
    menuItem("VCFiles", tabName = "vcf", icon = icon("upload"),
             
             ## Define sidebar to search concrete vcf case in the database 
             
             sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                               label = "Search...", icon = shiny::icon("search")
             ),
             
             ## Define an Item to upload only proband
             
             menuItem("Single case", tabName = "proband",
                      fileInput("Proband", "Choose VCF Proband File",
                                accept = c("text/plain",".vcf"))
             ),
             
             ## Define an Item to upload trio case
             
             #menuItem("Trio Case", tabName = "trio",
             #        fileInput("Proband", "Choose VCF Proband File", 
             #                 accept = c("text/plain",".vcf")),
             #         fileInput("Mother", "Choose VCF Mother File",
             #                   accept = c("text/plain",".vcf")),
             #         fileInput("Father", "Choose VCF Father File",
             #                   accept = c("text/plain",".vcf"))
                      
             #),
             
             ## Define the button to upload files
             
             actionButton("button", "Upload Files")        
    ),
    
    menuItem("Filters", icon = icon("th"), tabName = "filters")
  ),
  
  sidebarMenu(
    menuItemOutput("menuitem")
  )
  
  #sidebarSearchForm(textId = "searchText2", buttonId = "searchButton2",
  #                  label = "Search..."
  #)
)

##################################
##  Define DashboardBodyOutput  ##
##################################

body <- dashboardBody(
  
             
  fluidRow(
    
    ############################
    ##        VCF Data        ##
    ############################
    
    column(width = 12,
           DT::dataTableOutput("test")
    )
    
    ############################
    ##  Variant Information   ##
    ############################
    
    #column (width = 5,
    #        box(width = 12,
    #            title = "Aditional Information", status = "warning", collapsible = TRUE,
    #            sliderInput("slider", "Number of observations:", 1, 100, 50)
    #        )
    #),
    #textOutput("prueba") 
  )
  
)

# Define SERVER for application

server = (function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged)
  
  # Cheking if the user exists in the database
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          query <- sprintf({"
            SELECT rowid 
            FROM users 
            WHERE username='%s' and password ='%s'"}, 
                           Username, Password, serialize=F) 
          db   <- RSQLite::dbConnect(RSQLite::SQLite(), dbname="db.sqlite")
          user <- RSQLite::dbGetQuery(db, query) 
          RSQLite::dbDisconnect(db)
          if ( length(user$rowid)==1 ) {
            USER$Logged <- TRUE
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, 
                            animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, 
                                                 time = 1, animType = "fade"))
          }
          } 
        }
      }    
  })

  # Implementing the sidebarpanel 
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE ){sidebar}
  })
  
  # Implementing the body 
  output$body <- renderUI({
    if (USER$Logged == TRUE ) {body}
    else {
      loginpage
    }
  })
  
  #####################################
  #####        VCF INFORMATION    #####
  #####################################
  
  observeEvent(input$button, {
    req(input$Proband)
    inFile <- input$Proband
    
    if (is.null(inFile))
      return(NULL)
    
    vcf1Data <- read.vcfR(inFile$datapath)
    #Extract genotype
    gt <- extract.gt(vcf1Data, element = 'GT')
    #Extract allele frequency
    ad <- extract.gt(vcf1Data, element = 'AD')
    allele1 <- masplit(ad, record = 1, delim=",", sort = 0)
    allele2 <- masplit(ad, record = 2, delim=",", sort = 0)
    ad1 <- allele1 / (allele1 + allele2)
    ad2 <- allele2 / (allele1 + allele2)
    
    #Heterozygotes
    hets <- is_het(gt)
    for (i in 1:nrow(hets)) {
      if (hets[i] == TRUE) {hets[i] = "Het"}
      else {hets[i] = "Hom"}
    }
    
    #Extract info
    Z <- vcfR2tidy(vcf1Data, info_only = TRUE)
    data_vcf <- cSplit(Z$fix, "ANN", "|", stripWhite = FALSE)
    d <- c("Allele", "Annotation",  "AnnotationImpact","GeneName", "GeneID" ,
           "FeatureType", "FeatureID" ,"TranscriptBioType", "Rank" , "HGVS.c", 
           "HGVS.p", "cDNA.pos/cDNA.length","CDS.pos/CDS.length", 
           "AA.pos/AA.length","Distance", "ERRORS/WARNINGS/INFO")
    colnames(data_vcf)[68:83] <- d
    data_vcf <- data_vcf[,1:83]
    
    data_vcf2 <- cbind(data_vcf[,c(1,2,4,5,71,74,77:78,69,3,37,47:49,53:54,57:67,70,13,6)])
    
    
    bc <- as_tibble(data.frame(data_vcf2))
    
    output$test <- DT::renderDataTable({
      
      DT::datatable(
        bc,
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        options = list(scrollY = 650,
                       scrollX = 500,
                       deferRender = TRUE,
                       scroller = TRUE,
                       #paging = TRUE,
                       #pageLength = 15,
                       buttons = list('excel',
                                      list(extend = 'colvis', targets = 0, 
                                           visible = FALSE)),
                       dom = 'lBfrtip',
                       fixedColumns = TRUE,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#F9B198', 'color': '#fff'});",
                         "}"
                       )),
        rownames = FALSE)
    })  
    
  })
  
  output$res <- renderText({
    paste("You've selected:", input$tabs)
  })
  
  # Saving user session information
  
  users_data <- data.frame(START = Sys.time()) #USERS = session$user
  
  # This code will be run after the client has disconnected
  session$onSessionEnded(function() { #userID = users_data$USERS
    #if(userID==1){
    users_data$END <- Sys.time()
    # Write a file in your working directory
    write.table(x = users_data, file = file.path(getwd(), "users_data.txt"),
                append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
    #}
  })
  
  # Log-out the application
  output$logoutbtn <- renderUI({
    req(USER$Logged)
    tags$li(a(icon("faS fa-sign-out-alt"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
})


# Run the application

shinyApp(ui = ui, server = server)
