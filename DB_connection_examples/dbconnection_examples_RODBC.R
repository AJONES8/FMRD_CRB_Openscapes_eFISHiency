##Setting up DB connection
#Works on shiny server

######
#Once loaded packages edit your user name and database specifics and run from here down
#You have to put in your user name
Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
sole <- RODBC::odbcConnect(dsn="sole", uid="ajones", pwd="PASSWORD", believeNRows=FALSE)
sqlQuery(sole,"SET ROLE ALL")