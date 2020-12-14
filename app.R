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
library(RSQLite)
library(splitstackshape)
library(base)
library(stringi)
library(data.table)
library(plyr)
library(dplyr)
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
    
    ## DEFINE FILTERS 
    menuItem("Filters", icon = icon("th"), tabName = "filters",
             # HPO
             selectizeInput(
               "hpoterms", label = "Choose HPO terms",
               choices = NULL,
               selected = "",
               multiple = TRUE
             ),
             
             # Customize VCF filter
             menuItem("Variants Filtering", tabName = "filter1",
                      checkboxGroupInput("checkGroup", label = "",
                                         choices = list(
                                           "Coding variant" = 1, 
                                           "Splicing variant" = 2, 
                                           "3' prime UTR variant" = 3,
                                           "5' prime UTR variant" = 4,
                                           "Intron variant" = 5),
                                         selected = c(1,2))
             ),
             
             # Frequency filter (<0.05 default)
             menuItem("Frequency", tabName = "filter2",
                      numericInput("gnomad", label = "gnomAD",
                                   value = 0.05, min = 0, max = 1, step = 0.001)
                      # numericInput("exac", label = "ExAC",
                      #              value = 0.05)
                      
             ),
             
             menuItem("ClinVar Filtering", tabName = "filter3",
                      checkboxGroupInput("checkGroup2", label = "",
                                         choices = list(
                                           "Benign" = 1,
                                           "Likely benign" = 2,
                                           "Uncertain significance" = 3,
                                           "Likely pathogenic" = 4,
                                           "Pathogenic" = 5,
                                           "Conflicting" = 6,
                                           "Other" = 7),
                                         selected = c(3,4,5,6,7))
             ),
             
             menuItem("Inheritance Filtering", tabName = "filter4",
                      checkboxGroupInput("checkGroup3", label = "",
                                         choices = list(
                                           "Autosomal Dominant" = 1,
                                           "Autosomal Recessive" = 2,
                                           "XLinked" = 3,
                                           "XLinked Dominant" = 4,
                                           "XLinked Recessive" = 5,
                                           "YLinked" = 6,
                                           "Somatic Mutation" = 7,
                                           "Somatic Mosaicism" = 8,
                                           # "Mitochondrial" = 9,
                                           "Multifactorial" = 9,
                                           "Other" = 10),
                                         selected = c(1,2,3,4,5))
             ),
             
             actionButton("filter", "Apply filters")
             
    ),
    
    #Upload Gene Coverage File
    menuItem("Coverage", tabName = "cov", icon = icon("dashboard"),
             
             ## Define an Item to upload coverage file
             fileInput("coverage", "Choose txt Coverage File",
                       accept = c("text/plain",".txt"))
             #, actionButton("coverage", "Apply Coverage File")
             
    )
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
    
    column(width = 8,
           box(width = 12,
               title = "Variants table",collapsible = TRUE,
               DT::dataTableOutput("test")
           )
           
    ),
    
    ############################
    ##      Gene Coverage     ##
    ############################
    
    column (width = 4,
             box(width = 12, 
                 title = "Gene Coverage", status = "warning", collapsible = TRUE,
                 DT::dataTableOutput("coverage")
                 # textOutput("coverage")
                 #     sliderInput("slider", "Number of observations:", 1, 100, 50)
             )
             
    )
    , textOutput("prueba") 
    ,textOutput("res")
  )
  
)

# Define SERVER for application

# Conecting to the HPO database and creating HPO term list

con2 <- dbConnect(SQLite(), dbname="db2.sqlite")
hpo <- dbGetQuery(con2, "SELECT * FROM HPOdb")
hpo_omim <- dbGetQuery(con2, "SELECT * FROM HPO_OMIM")
hpo_omim$EntrezGeneID <- as.character(hpo_omim$EntrezGeneID)
hpo_omim$MIMNumber <- as.character(hpo_omim$MIMNumber)
dbDisconnect(con2)
selectList <- c(paste(hpo$HPOTermName,hpo$HPOTermID,sep = ", "))



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
  
  observe({
    req(input$sidebarItemExpanded)
    if (input$sidebarItemExpanded == "Filters") {
      updateSelectizeInput(session = session, inputId = 'hpoterms', 
                           choices = c(selectList), server = TRUE)
    }
  })

  
  #####################################
  #####        VCF INFORMATION    #####
  #####################################
  tabla <- reactiveValues(vcf=NULL)
  
  
  observeEvent(input$button, {
    req(input$Proband)
    inFile <- input$Proband
    
    if (is.null(inFile))
      return(NULL)
    
    ## CONECTAR CON SQLite
    
    print("leyendo el vcf")
    start_time <- Sys.time()
    
    vcfData <- read.vcfR(inFile$datapath)
    
    end_time <- Sys.time()
    print("vcf leido")
    print(end_time - start_time)
    #####################################
    #####   EXTRACTING VCF INFO     #####
    #####################################
    print("extrayendo gt")
    start_time <- Sys.time()
    
    
    #Extract genotype
    gt <- extract.gt(vcfData, element = 'GT')
    #Extract allele frequency
    ad <- extract.gt(vcfData, element = 'AD')
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
    
    
    end_time <- Sys.time()
    print("gt extraido")
    print(end_time - start_time)
    
    print("vcftidy")
    start_time <- Sys.time()
    
    #Extract info
    Z <- vcfR2tidy(vcfData, info_only = TRUE)
    
    end_time <- Sys.time()
    print("vcftidy finalizado")
    print(end_time - start_time)
    
    print("split")
    start_time <- Sys.time()
    
    vcfInfo <- cSplit(Z$fix, "ANN", "|", stripWhite = FALSE)
    
    end_time <- Sys.time()
    print("split finalizado")
    print(end_time - start_time)
    
    vcfInfo <- cbind(vcfInfo[,1:86], round(ad2,2), hets)
    d <- c("AF_gnomAD_raw","Homozygotes","CADD","DANN","GERP","MutationTaster",
           "Polyphen2","SIFT","FATHMM","PhastCons30","PhyloP30","Allele", 
           "Annotation",  "AnnotationImpact","GeneName", "GeneID" ,
           "FeatureType", "FeatureID" ,"TranscriptBioType", "Rank" , "HGVS.c", 
           "HGVS.p", "cDNA.pos/cDNA.length","CDS.pos/CDS.length", 
           "AA.pos/AA.length","Distance", "ERRORS/WARNINGS/INFO", 
           "AlleleFrequency", "Zigosity")
    colnames(vcfInfo)[60:88] <- d
    
    #names(vcfInfo)[names(vcfInfo) == "CLNSIG"] <- "ClinVar"
    
    vcfInfo$ALLELEID <- as.character(vcfInfo$ALLELEID)
    vcfInfo %>% mutate_if(is.factor, as.character) -> vcfInfo
    
    
    #####################################
    ##### ADDING NEW INFORMATION VCF ####
    #####################################
    
    
    #####################################################################################
    annotatePhenotype <- function(df,db) {
      # df -> dataframe a anotar
      # db -> base de datos OMIM HPO
      # OUTPUT ->  dataframe con columnas anadidas
      
      MIMNumberVec<-rep("",nrow(df))
      InheritanceVec<-rep("",nrow(df))
      PhenotypesVec<-rep("",nrow(df))
      HPOTermNameVec <- rep("",nrow(df))
      HPOTermIDVec <- rep("",nrow(df))
      
      for (i in 1:nrow(df)){
        
        x<-df$GENE_ID[i]
        
        if (x=="" | x=="NA"| is.na(x)) next
        ids <- unlist(strsplit(x, ","))
        
        MIMNumber <- ""
        Inheritance <- ""
        Phenotypes <- ""
        HPOTermName <- ""
        HPOTermID <- ""
        
        for (id in ids) {
          
          ma<-match(id,db$EntrezGeneID)
          MIMNumber <- c(MIMNumber,db[ma,"MIMNumber"])
          Inheritance <- c(Inheritance,db[ma,"Inheritance"])
          Phenotypes <- c(Phenotypes,db[ma,"Phenotypes"])
          HPOTermName <- c(HPOTermName,db[ma,"HPOTermName"])
          HPOTermID <- c(HPOTermID,db[ma,"HPOTermID"])
        }
        
        MIMNumber <- MIMNumber[(MIMNumber!="" & !is.na(MIMNumber))]
        MIMNumberVec[i]<-paste(MIMNumber, collapse=", ")
        
        Inheritance <- Inheritance[(Inheritance!="" & !is.na(Inheritance))]
        InheritanceVec[i]<-paste(Inheritance, collapse=", ")
        
        Phenotypes <- Phenotypes[(Phenotypes!="" & !is.na(Phenotypes))]
        PhenotypesVec[i]<-paste(Phenotypes, collapse=", ")
        
        HPOTermName <- HPOTermName[(HPOTermName!="" & !is.na(HPOTermName))]
        HPOTermNameVec[i]<-paste(HPOTermName, collapse=", ")
        
        HPOTermID <- HPOTermID[(HPOTermID!="" & !is.na(HPOTermID))]
        HPOTermIDVec[i]<-paste(HPOTermID, collapse=", ")
      }
      
      df$MIMNumber <- MIMNumberVec
      df$Inheritance <- InheritanceVec
      df$Phenotypes <- PhenotypesVec
      df$HPOTermName <- HPOTermNameVec
      df$HPOTermID <- HPOTermIDVec
      
      return(df)
    }
    #####################################################################################
    
    print("anadiendo omim-hpo")
    start_time <- Sys.time()
    
    vcfInfo<-annotatePhenotype(vcfInfo,hpo_omim)
    
    end_time <- Sys.time()
    print(end_time - start_time)
    
    print("omim-hpo anadido")
    
    #####################################
    ##### ADDING NEW INFORMATION VCF ####
    #####################################
    
    print("arreglando la db")
    start_time <- Sys.time()
    
    for (i in 1:nrow(vcfInfo)) {
      if (is.na(vcfInfo$CLNSIG[i]) == FALSE && 
          grepl("Conflicting_interpretations_of_pathogenicity", 
                vcfInfo$CLNSIG[i], fixed = TRUE) == TRUE) {
        
        vcfInfo$CLNSIG[i] <- vcfInfo$CLNSIGCONF[i]
      }
    }
    
    gene_id <- vcfInfo$GENE_ID
    allele_id <- vcfInfo$ALLELEID
    rs <- vcfInfo$ID
    
    # Making links
    print("anadiendo links")
    
    
    makingLinks <- function(df) {
      
      GeneVec<-rep("",nrow(df))
      ClinVarVec<-rep("",nrow(df))
      AF_gnomADVec<-rep("",nrow(df))
      dbSNPVec <- rep("",nrow(df))
      MIMNumberVec <- rep("",nrow(df))
      
      for (i in 1:nrow(df)){
        
        GeneID <- df$GENE_ID[i]
        AlleleID <- df$ALLELEID[i]
        ID <- df$ID[i]
        GeneName <- df$GeneName[i]
        CLNSIG <- df$CLNSIG[i]
        AF_gnomAD_raw <- df$AF_gnomAD_raw[i]
        MIMNumber_raw <- unique(df$MIMNumber[i])
        
        Gene <- ""
        ClinVar <- ""
        AF_gnomAD <- ""
        dbSNP <- ""
        MIMNumber <- ""
        
        if(!is.na(ID))            {dbSNP <- paste0('<a href=\"https://www.ncbi.nlm.nih.gov/snp/',ID,'\" target=\"_blank\">',ID,'</a>')}
        if(!is.na(GeneName))      {Gene <- paste0('<a href=\"https://www.ncbi.nlm.nih.gov/gene?Db=gene&amp;Cmd=DetailsSearch&amp;Term=',GeneID,'\" target=\"_blank\">',GeneName,'</a>')}
        if(!is.na(CLNSIG))        {ClinVar <- paste0('<a href=\"https://www.ncbi.nlm.nih.gov/clinvar/?term=',AlleleID,'[alleleid]\" target=\"_blank\">',CLNSIG,'</a>')} 
        if(!is.na(AF_gnomAD_raw)) {AF_gnomAD <- paste0('<a href=\"https://gnomad.broadinstitute.org/variant/',ID,'?dataset=gnomad_r2_1_controls\" target=\"_blank\">',AF_gnomAD_raw,'</a>')} 
        if(!is.na(MIMNumber_raw)) {MIMNumber <- paste0('<a href=\"https://www.omim.org/entry/',MIMNumber_raw,'\" target=\"_blank\">',MIMNumber_raw,'</a>')} 
        
        GeneVec[i] <- Gene
        ClinVarVec[i] <- ClinVar
        AF_gnomADVec[i] <- AF_gnomAD
        dbSNPVec[i] <- dbSNP
        MIMNumberVec[i] <- MIMNumber
        
      }
      
      df$Gene <- GeneVec
      df$ClinVar <- ClinVarVec
      df$AF_gnomAD <- AF_gnomADVec
      df$dbSNP <- dbSNPVec
      df$MIMNumber <- MIMNumberVec
      
      return(df)
    }
    
    vcfInfo <- makingLinks(vcfInfo)
    
    end_time <- Sys.time()
    print("link listos")
    print(end_time - start_time)
    # 
    # vcfInfo$MIMNumber <- sapply(vcfInfo$MIMNumber, function(x)
    #   toString(tags$a(href=paste0("https://www.omim.org/entry/", x),
    #                   x, target="_blank" )))
    
    #####################################
    #####        FILTERING VCF      #####
    #####################################
    
    print("Filtrando por calidad")
    print(nrow(vcfInfo))
    ##QUALITY FILTERS (AUTOMATIC)
    
    vcfInfo <- vcfInfo[-c(which(
      vcfInfo$QD < 2.0 | #Quality by Depth
        vcfInfo$MQ < 40.0 | #MappingQuality
        vcfInfo$MQRankSum < -12.5 | #Z-scoreMappingQuality
        vcfInfo$FS > 60.0 | #Fisher Test to detect Strand-bias
        vcfInfo$SOR > 3.0 | #Symmetric Odds Ratio to detect Strand-bias
        vcfInfo$QUAL < 30.0 | #Quality
        vcfInfo$ReadPosRankSum < -8.0 | #Z-score read position bias
        grepl("unknown_transcript_1", vcfInfo$FeatureID, fixed = TRUE) == TRUE |
        grepl("XR", vcfInfo$FeatureID, fixed = TRUE) == TRUE |
        grepl("XM", vcfInfo$FeatureID, fixed = TRUE) == TRUE)),]
    
    end_time <- Sys.time()
    print("VCF filtrado")
    print(nrow(vcfInfo))
    print(end_time - start_time)
    
    
    #####################################
    #####      SAVING VCF INFO      #####
    #####################################
    
    #Saving into reactive value
    tabla$vcf <- vcfInfo
    
    #Saving in to the sqlite database
    
    # con <- dbConnect(SQLite(), dbname="db.sqlite")
    # numbercase <- "5693"
    # statement2 <- paste("INSERT INTO", user, "VALUES ('",numbercase,"')")
    # 
    # dbExecute(con, statement2)
    # 
    # vcfInfo_load <- as_tibble(data.frame(vcfInfo))
    # analisis2_load <- as.data.frame(analisis2)
    # 
    # dbWriteTable(con, numbercase, analisis2_load)
    # 
    # vcfInfo <- dbWriteTable(con, "HPO_OMIM", hpo_omim, rownames = TRUE)
    
    #observeEvent(input$filter, {
    
  })
  
  #######################################
  #####    PRIORIZATION VARIANTS    #####
  #######################################
  
  observeEvent(input$filter, {
    # output$test <- DT::renderDataTable({
    
    #Charging reactive value
    
    req(tabla$vcf)
    print("filtrando con filtros customizables")
    start_time <- Sys.time()
    
    vcfInfo <- tabla$vcf
    print(nrow(vcfInfo))
    
    
    #####################################
    #####        FILTERING VCF      #####
    #####################################
    
    ##CUSTOM FILTERS
    
    print("Aplicando fitros customizables")
    start_time <- Sys.time()
    
    ## HPO FILTERS
    
    HPOFilterID <- input$hpoterms
    if (length(HPOFilterID) != 0) {
      hp <- ""
      hpoanalysis <- data.frame()
      for (i in HPOFilterID) { 
        hp <- substr(i, 1, regexpr(",", i, fixed=TRUE, useBytes = TRUE)-1)
        df <- vcfInfo[which(grepl(hp,vcfInfo$HPOTermName, fixed = TRUE) == TRUE),]
        l <- list(hpoanalysis, df)
        hpoanalysis <- rbindlist(l)
      }
      vcfInfo <- hpoanalysis
    }
    
    
    ## VARIANT TYPE FILTERS
    print( "filtros del typoe de variante")
    filters <- paste(input$checkGroup,collapse="")
    paste0(filters)
    
    
    analysis1 <- data.frame()
    analysis2 <- data.frame()
    analysis3 <- data.frame()
    analysis4 <- data.frame()
    analysis5 <- data.frame()
    
    if (grepl("1",filters, fixed = TRUE) == TRUE) {
      analysis1 <- vcfInfo[which(
        grepl("conservative_inframe_deletion",
              vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("conservative_inframe_insertion",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("disruptive_inframe_deletion",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("disruptive_inframe_insertion",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("frameshift_variant",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("missense_variant",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("start_lost",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("stop_gained",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("stop_lost",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("synonymous_variant",
                vcfInfo$Annotation, fixed = TRUE) == TRUE),]
    }
    
    if (grepl("2",filters, fixed = TRUE) == TRUE) {
      analysis2 <- vcfInfo[which(
        grepl("splice_region_variant",
              vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("splice_acceptor_variant",
                vcfInfo$Annotation, fixed = TRUE) == TRUE |
          grepl("splice_donor_variant",
                vcfInfo$Annotation, fixed = TRUE) == TRUE),]
    }
    
    if (grepl("3",filters, fixed = TRUE) == TRUE) {
      analysis3 <- vcfInfo[which(
        grepl("3_prime_UTR_variant",
              vcfInfo$Annotation, fixed = TRUE) == TRUE),]
    }
    
    if (grepl("4",filters, fixed = TRUE) == TRUE) {
      analysis4 <- vcfInfo[which(
        grepl("5_prime_UTR_variant",
              vcfInfo$Annotation, fixed = TRUE) == TRUE),]
    }
    
    if (grepl("5",filters, fixed = TRUE) == TRUE) {
      analysis5 <- vcfInfo[which(
        grepl("5_prime_UTR_variant",
              vcfInfo$Annotation, fixed = TRUE) == TRUE),]
    }
    
    AnalysisList <- list(analysis1,analysis2,analysis3, analysis4, analysis5)
    analysis <- rbindlist(AnalysisList)
    analysis <- unique(analysis)
    print(nrow(analysis))
    # analysis <- rbind(analysis1,analysis2,analysis3, analysis4, analysis5)
    
    
    # analysis <- rbind(Benign, LikelyBenign, Uncertain, LikelyPathogenic, 
    #                   Pathogenic, Conflicting, Other, NotClinVar)
    
    
    print("filtros de frecuencia")
    #Filtros de frecuencia
    analysis <- analysis[which(is.na(analysis$AF_gnomAD_raw) == TRUE |
                                 round(as.numeric(analysis$AF_gnomAD_raw),2) <= 
                                 input$gnomad),]
    print(nrow(analysis))
    # end_time <- Sys.time()
    # print("vcf filtrado automatico")
    # print(end_time - start_time)
    
    
    
    #####################################
    #####      PRIORIZATION VCF     #####
    #####################################
    
    filters3 <- paste(input$checkGroup3,collapse="")
    paste0(filters3)
    
    AutosomalDominant <- data.frame()
    AutosomalRecessive <- data.frame()
    XLinkedDominant <-data.frame()
    XLinkedRecessive <- data.frame()
    XLinked <- data.frame()
    YLinked <- data.frame()
    SomaticMutation <- data.frame()
    SomaticMosaicism <- data.frame()
    Multifactorial <- data.frame()
    # Mitochondrial <- data.frame()
    OtherInheritance <- data.frame()
    
    if (grepl("1",filters3, fixed = TRUE) == TRUE) {
      AutosomalDominant <- analysis[which(grepl("AD",analysis$Inheritance,
                                                fixed = TRUE) == TRUE),]
    }
    if (grepl("2",filters3, fixed = TRUE) == TRUE) {
      AutosomalRecessive <- analysis[which(grepl("AR",analysis$Inheritance,
                                                 fixed = TRUE) == TRUE),]
    }
    if (grepl("3",filters3, fixed = TRUE) == TRUE) {
      XLinked <- analysis[which(grepl("XL",analysis$Inheritance,
                                      fixed = TRUE) == TRUE),]
    }
    if (grepl("4",filters3, fixed = TRUE) == TRUE) {
      XLinkedDominant <- analysis[which(grepl("XLD",analysis$Inheritance,
                                              fixed = TRUE) == TRUE),]
    }
    if (grepl("5",filters3, fixed = TRUE) == TRUE) {
      XLinkedRecessive <- analysis[which(grepl("XLR",analysis$Inheritance,
                                               fixed = TRUE) == TRUE),]
    }
    if (grepl("6",filters3, fixed = TRUE) == TRUE) {
      YLinked <- analysis[which(grepl("YL",analysis$Inheritance,
                                      fixed = TRUE) == TRUE),]
    }
    if (grepl("7",filters3, fixed = TRUE) == TRUE) {
      SomaticMutation <- analysis[which(grepl("Smu",analysis$Inheritance,
                                              fixed = TRUE) == TRUE),]
    }
    if (grepl("8",filters3, fixed = TRUE) == TRUE) {
      SomaticMosaicism <- analysis[which(grepl("Smo",analysis$Inheritance,
                                               fixed = TRUE) == TRUE),]
    }
    # if (grepl("9",filters3, fixed = TRUE) == TRUE) {
    #   Mitochondrial <- analysis[which(grepl("Mi",analysis$Inheritance,
    #                                         fixed = TRUE) == TRUE),]
    # }
    if (grepl("9",filters3, fixed = TRUE) == TRUE) {
      Multifactorial <- analysis[which(grepl("Mu",analysis$Inheritance,
                                             fixed = TRUE) == TRUE),]
    }
    if (grepl("10",filters3, fixed = TRUE) == TRUE) {
      OtherInheritance <- analysis[which(
        grepl("DR",
              analysis$Inheritance, fixed = TRUE) == TRUE |
          grepl("PD",
                analysis$Inheritance, fixed = TRUE) == TRUE |
          grepl("PR",
                analysis$Inheritance, fixed = TRUE) == TRUE |
          grepl("DD",
                analysis$Inheritance, fixed = TRUE) == TRUE |
          grepl("IC",
                analysis$Inheritance, fixed = TRUE) == TRUE |
          grepl("ICB",
                analysis$Inheritance, fixed = TRUE) == TRUE |
          is.na(analysis$Inheritance) == TRUE |
          analysis$Inheritance == "-"),]
    }
    
    
    
    AnalysisList3 <- list(XLinkedDominant,AutosomalDominant,XLinkedRecessive,
                          XLinked,AutosomalRecessive,YLinked,SomaticMutation,
                          SomaticMosaicism,Multifactorial,
                          OtherInheritance)
    
    analysis <- rbindlist(AnalysisList3)
    
    analysis <- unique(analysis)
    # analysis <- rbind(AutosomalDominant,AutosomalRecessive,XLinkedDominant,
    #                   XLinkedRecessive,XLinked,YLinked,SomaticMutation,
    #                   SomaticMosaicism,Multifactorial,Mitochondrial,
    #                   OtherInheritance)
    
    # FILTROS CLINVAR (incluyentes)
    print("filtros clinvar")
    filters2 <- paste(input$checkGroup2,collapse="")
    paste0(filters2)
    
    Benign <- data.frame()
    LikelyBenign <- data.frame()
    Benign_LikelyBenign <- data.frame()
    Uncertain <- data.frame()
    LikelyPathogenic <- data.frame()
    LikelyPathogenic_Pathogenic <- data.frame()
    Pathogenic <- data.frame()
    Conflicting <- data.frame()
    Other <- data.frame()
    NotClinVar <- data.frame()
    
    # PRIORIZATION FUNCTION
    
    priorization <- function(df) {
      effect <- c("HIGH","MODERATE","MODIFIER","LOW")
      df$AnnotationImpact <- factor(df$AnnotationImpact, levels=effect)
      df <- df[with(df,order(AnnotationImpact,-as.numeric(CADD))),]
      return(df)
    }
    
    #B/LB
    if (grepl("12",filters2, fixed = TRUE) == TRUE) {
      Benign <- analysis[which(analysis$CLNSIG == "Benign"),]
      Benign <- priorization(Benign)
      LikelyBenign <- analysis[which(analysis$CLNSIG == "Likely_benign"),]
      LikelyBenign <- priorization(LikelyBenign)
      Benign_LikelyBenign <- analysis[which(analysis$CLNSIG == "Benign/Likely_benign"),]
      Benign_LikelyBenign <- priorization(Benign_LikelyBenign)
    } else {
      if (grepl("1",filters2, fixed = TRUE) == TRUE) {
        Benign <- analysis[which(analysis$CLNSIG == "Benign"),]
        Benign <- priorization(Benign)
      }
      if (grepl("2",filters2, fixed = TRUE) == TRUE) {
        LikelyBenign <- analysis[which(analysis$CLNSIG == "Likely_benign"),]
        LikelyBenign <- priorization(LikelyBenign)
      }
    }
    
    #VSI
    if (grepl("3",filters2, fixed = TRUE) == TRUE) {
      Uncertain <- analysis[which(analysis$CLNSIG == "Uncertain_significance"),]
      Uncertain <- priorization(Uncertain)
    }
    
    #P/LP
    if (grepl("45",filters2, fixed = TRUE) == TRUE) {
      LikelyPathogenic <- analysis[which(analysis$CLNSIG == "Likely_pathogenic"),]
      LikelyPathogenic <- priorization(LikelyPathogenic)
      Pathogenic <- analysis[which(analysis$CLNSIG == "Pathogenic"),]
      Pathogenic <- priorization(Pathogenic)
      LikelyPathogenic_Pathogenic <- analysis[which(analysis$CLNSIG == "Pathogenic/Likely_pathogenic"),]
      LikelyPathogenic_Pathogenic <- priorization(LikelyPathogenic_Pathogenic)
    } else {
      if (grepl("4",filters2, fixed = TRUE) == TRUE) {
        LikelyPathogenic <- analysis[which(analysis$CLNSIG == "Likely_pathogenic"),]
        LikelyPathogenic <- priorization(LikelyPathogenic)
      }
      if (grepl("5",filters2, fixed = TRUE) == TRUE) {
        Pathogenic <- analysis[which(analysis$CLNSIG == "Pathogenic"),]
        Pathogenic <- priorization(Pathogenic)
      }
    }
    
    #CONFLICTING
    
    if (grepl("6",filters2, fixed = TRUE) == TRUE) {
      Conflicting <- analysis[which(
        grepl("Likely_pathogenic(",
              analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("Pathogenic(",
                analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("Uncertain_significance(",
                analysis$CLNSIG, fixed = TRUE) == TRUE),]
      Conflicting <- priorization(Conflicting)
    }
    
    # OTHER 
    
    if (grepl("7",filters2, fixed = TRUE) == TRUE) {
      Other <- analysis[which(
        grepl("drug_response",
              analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("association",
                analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("risk_factor",
                analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("protective",
                analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("Affects",
                analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("other",
                analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("not_provided",
                analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("conflicting_data_from_submitters",
                analysis$CLNSIG, fixed = TRUE) == TRUE |
          grepl("-",
                analysis$CLNSIG, fixed = TRUE) == TRUE),]
      Other <- priorization(Other)
    }
    
    NotClinVar <- analysis[which(is.na(analysis$CLNSIG) == TRUE),]
    NotClinVar <- priorization(NotClinVar)
    
    AnalysisList2 <- list(Pathogenic,LikelyPathogenic_Pathogenic,LikelyPathogenic,
                          Uncertain,Conflicting,Other,NotClinVar,Benign,
                          Benign_LikelyBenign, LikelyBenign)
    
    analysis <- rbindlist(AnalysisList2)
    print(nrow(analysis))
    
    analysis <- unique(analysis)
    
    AnalysisCol <- c('CHROM','POS','REF','ALT','Gene','FeatureID','HGVS.c',
                     'HGVS.p','Rank','AlleleFrequency','Zigosity','Annotation',
                     'dbSNP','ClinVar','Phenotypes','Inheritance', 'MIMNumber',
                     'GENE_ID','AF_ESP','AF_EXAC','AF_gnomAD','Homozygotes',
                     'AnnotationImpact','CADD','GERP','MutationTaster',
                     'Polyphen2','SIFT','FATHMM','PhastCons30','PhyloP30',
                     'QUAL', 'DP')
    
    analysis <- analysis[,..AnalysisCol]
    
    
    end_time <- Sys.time()
    print(nrow(analysis
    ))
    print("filtros customizables aplicados")
    print(end_time - start_time)
    # analysis <- analysis[,c(1,2,4:5,89,77,80:81,79,72,92,90,95,94,93,10:11,91,
    # 61,73,62,64:70,87:88,6,30)]
    
    
    # #ORDERING
    # chromList <- c(sprintf("chr%d", 1:22), "chrX", "chrY")
    # #create a factor, level it by d2
    # analysis$CHROM <- factor(analysis$CHROM, levels=chromList)
    # 
    # analysis <- analysis[order(analysis$CHROM),]
    
    bc <- as_tibble(data.frame(analysis))
    print(nrow(bc))
    
    # # analysis1 <- cbind(analysis[,c(1,2,4:5,89,77,80:81,79,72,92,90,95,94,93,
    # #                               10:11,91,61,73,62,64:70,87:88,6,30)])
    # 
    # bc <- as_tibble(data.frame(analysis1))
    
    
    end_time <- Sys.time()
    print("df arreglada")
    print(end_time - start_time)
    #####################################
    #####      PRINT THE TABLE      #####
    #####################################
    
    output$test <- DT::renderDataTable({
      
      DT::datatable(
        bc,
        escape = FALSE,
        filter = 'top', extensions = c('Buttons', 'Scroller'),
        selection = 'single',
        options = list(scrollY = 650,
                       scrollX = 500,
                       autoWidth = T,
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
      #})  
      
    })
    
    #######################################
    #####     TABLE GENE COVERAGE     #####
    #######################################
    
    output$coverage <- DT::renderDataTable({
      req(input$coverage)
      CovFile <- input$coverage
      cov <- read.table(CovFile$datapath, sep = "\t", header = TRUE)
      cov$perc_covered <- round(cov$perc_covered,2)
      colnames(cov) <- c("GENE_ID", "Percentage_Covered", "Low_Coverage_Regions")
      
      
      s <- input$test_rows_selected
      
      geneCov <- cov[which(cov$GENE_ID == analysis$GENE_ID[s]),]
      # if (nrow(geneCov) != 0) {
      #   geneCov$GENE_ID <- analysis$GeneName[s]
      # }
      
      if (!is.null(cov)) {
        
        DT::datatable(
          geneCov,
          class = "row-border", 
          filter = "none", 
          rownames = FALSE,
          options = list(paging = FALSE, 
                         searching = FALSE, 
                         info = FALSE, 
                         ordering = FALSE,
                         autoWidth = TRUE, 
                         autoFill = TRUE, 
                         lengthMenu = FALSE,
                         columnDefs=list(list(targets='_all', class="dt-center"))))
        
      }
      
      
    })
    
    
  })
  
  #output$prueba <- renderText({
  #  paste("You've selected:", input$hpoterms, input$filter1, input$coding)
  #})
  
  output$prueba <- renderPrint({
    f <- function (x) {
      return(x)
      #if (grepl("1", x, fixed = TRUE) == TRUE && 
      #    grepl("2", x, fixed = TRUE)) {
      #  "coding and splicing"
      #}
    }
    f(input$checkGroup)
    #input$checkGroup
  })
  
  ######################################
  #####  SAVING USER SESSION INFO  #####
  ######################################

  
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
  
  
  ######################################
  #####       LOG-OUT THE APP      #####
  ######################################
  
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
