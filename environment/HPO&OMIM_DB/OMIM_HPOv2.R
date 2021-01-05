library(RSQLite)

##############################################
###   CONNECTING TO THE OMIM-HPO DATABSE   ###
##############################################

con2 <- dbConnect(SQLite(), dbname="db2.sqlite")

############################
##    Loading OMIM Data   ##
############################
omim <- read.csv("genemap2.txt", 
                 sep = "\t", comment.char = "#", header = FALSE,as.is = TRUE)
colnames(omim) <- c("Chromosome",	"Start", "End", "CytoLocation",	
                    "ComputedCytoLocation",	"MIMNumber",	"GeneSymbols",	
                    "GeneName",	"ApprovedSymbol",	"EntrezGeneID",	
                    "EnsemblGeneID",	"Comments",	"Phenotypes",	
                    "MouseGeneSymbol")
omim$Inheritance <- "-"

##Adding new column for the inheritance
for (i in 1:nrow(omim)) {
  
  pheno <- omim$Phenotypes[i]

  if (pheno!="") {
        inher<-c("")
        if (grepl("Autosomal dominant",pheno,ignore.case = TRUE)) {inher <- c(inher, "AD")}
        if (grepl("Autosomal recessive",pheno,ignore.case = TRUE)) {inher<-c(inher, "AR")}
        if (grepl("Pseudoautosomal dominant",pheno,ignore.case = TRUE)) {inher <- c(inher, "PD")}
        if (grepl("Pseudoautosomal recessive",pheno,ignore.case = TRUE)) {inher <- c(inher, "PR")}
        if (grepl("Digenic dominant",pheno,ignore.case = TRUE)) {inher <- c(inher, "DD")}
        if (grepl("Digenic recessive",pheno,ignore.case = TRUE)) {inher <- c(inher, "DR")}
        if (grepl("Isolated cases",pheno,ignore.case = TRUE)) {inher <- c(inher, "IC")}
        if (grepl("Inherited chromosomal imbalance",pheno,ignore.case = TRUE)) {inher <- c(inher, "ICB")}
        if (grepl("Multifactorial",pheno,ignore.case = TRUE)) {inher <- c(inher, "Mu")}
        if (grepl("Somatic mosaicism",pheno,ignore.case = TRUE)) {inher <- c(inher, "SMo")}
        if (grepl("Somatic mutation",pheno,ignore.case = TRUE)) {inher <- c(inher, "SMu")}
        if (grepl("X-linked",pheno,ignore.case = TRUE)) {inher <- c(inher, "XL")}
        if (grepl("X-linked dominant",pheno,ignore.case = TRUE)) {inher <- c(inher, "XLD")}
        if (grepl("X-linked recessive",pheno,ignore.case = TRUE)) {inher <- c(inher, "XLR")}
        if (grepl("Autosomal dominant",pheno,ignore.case = TRUE)) {inher <- c(inher, "AD")}
        if (grepl("Autosomal dominant",pheno,ignore.case = TRUE)) {inher <- c(inher, "AD")}
        if (grepl("Y-linked",pheno,ignore.case = TRUE)) {inher <- c(inher, "YL")}
        # Lo quito porque OMIM no lo usa y da falsos hits
        #if (grepl("Mitochondrial",pheno,ignore.case = TRUE)) {inher <- c(inher, "Mi")}
        
                
        inher <- unique(inher[(inher!="-" & inher!="" & !is.na(inher))])
        if (length(inher)==0 || inher=="") {inher<-"-"}
        omim$Inheritance[i]<-paste(inher, collapse=", ")
  }
  
}


##Delete rows which gene_id is empty
omim<-omim[!is.na(omim$EntrezGeneID),]

############################
##    Loading HPO Data    ##
############################

hpo <- read.csv("genes_to_phenotype.txt", 
                sep = "\t", comment.char = "#", header = FALSE,as.is = TRUE)
colnames(hpo) <- c("EntrezGeneId", "EntrezGeneSymbol", "HPOTermName", 
                   "HPOTermID", "FrequencyRaw", "FrequencyHPO", "INFO", "GD", 
                   "DiseaseOMIM")


hpo<-hpo[!duplicated(hpo[c(1,3)]),]

hpodf <- aggregate(.~EntrezGeneId,hpo ,FUN = toString)

for (i in 1:nrow(hpodf)) {
  hpodf$EntrezGeneSymbol[i]<- unique(unlist(strsplit(hpodf$EntrezGeneSymbol[i],", ")))
  hpodf$GD[i]<- unique(unlist(strsplit(hpodf$GD[i],", ")))
  hpodf$FrequencyRaw[i]<- unique(unlist(strsplit(hpodf$FrequencyRaw[i],", ")))
  hpodf$INFO[i]<- unique(unlist(strsplit(hpodf$INFO[i],", ")))
}


##############################
## Loading Data into SQLite ##
##############################

b <- dbSendQuery(conn = con2,
                 "DROP TABLE IF EXISTS OMIM")
dbClearResult(b)
b <- dbSendQuery(conn = con2,
                 "DROP TABLE IF EXISTS HPO")
dbClearResult(b)
b <- dbSendQuery(conn = con2,
                 "DROP TABLE IF EXISTS HPOdb")
dbClearResult(b)

omim_db <- dbWriteTable(con2, "OMIM", omim)
hpo_db <- dbWriteTable(con2, "HPO", hpodf)
hpo_db2 <- dbWriteTable(con2, "HPOdb", hpo)




#############################
## Making relational Table ##
#############################

hpo_omim <- as.data.frame(dbGetQuery(con2, "SELECT OMIM.EntrezGeneId, OMIM.MIMNumber, 
                       OMIM.GeneSymbols, OMIM.Phenotypes, OMIM.Inheritance, 
                       HPO.EntrezGeneSymbol, HPO.HPOTermName, HPO.HPOTermID, 
                       HPO.DiseaseOMIM FROM OMIM 
                       LEFT JOIN HPO USING(EntrezGeneId)
                       UNION ALL
                       SELECT OMIM.EntrezGeneId, OMIM.MIMNumber, 
                       OMIM.GeneSymbols, OMIM.Phenotypes, OMIM.Inheritance, 
                       HPO.EntrezGeneSymbol, HPO.HPOTermName, HPO.HPOTermID, 
                       HPO.DiseaseOMIM FROM HPO
                       LEFT JOIN OMIM USING(EntrezGeneId)
                       WHERE OMIM.EntrezGeneId IS NULL"))

##############################
## Loading Data into SQLite ##
##############################
b <- dbSendQuery(conn = con2,
                 "DROP TABLE IF EXISTS HPO_OMIM")
dbClearResult(b)

hpo_omim <- dbWriteTable(con2, "HPO_OMIM", hpo_omim, rownames = TRUE)


#if (exists("hpo_omim") == FALSE) {print("Este n?mero ya existe en la base de datos")}



###############################################
##  DISCONNECTING FROM THE OMIM-HPO DATABSE  ##
###############################################

dbDisconnect(con2)



