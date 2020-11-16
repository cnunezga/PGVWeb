library(RSQLite)

##############################################
######     CONNECTING TO THE DATABSE    ######
##############################################

con <- dbConnect(SQLite(), dbname="db.sqlite")

### CREATING TABLE USERS IF NOT EXISTIS ###
table_users <- dbSendQuery(conn = con,
                           "CREATE TABLE IF NOT EXISTS users
                           (username TEXT PRIMARY KEY UNIQUE,
                           password TEXT NOT NULL,
                           name TEXT NOT NULL,
                           email TEXT NOT NULL UNIQUE)")
dbClearResult(table_users)

### INSERT USERS ###
dbExecute(con, 
          "INSERT INTO users 
          VALUES ('Carmen', 'TFM2020', 'Carmen Nunez', 'cnunezga@uoc.edu')")

### CREATING TABLE USER ###
user <- "Carmen"
statement1 <- paste("CREATE TABLE IF NOT EXISTS", user, 
                    "(numbercase TEXT UNIQUE)")
table_cases <- dbSendQuery(conn = con, statement1)
dbClearResult(table_cases)


dbDisconnect(con)
