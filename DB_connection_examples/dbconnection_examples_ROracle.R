##Setting up DB connection
#Setting up connections to SOLE and NOVA using ROracle. This can be tricky to set up, and I believe only works with R 3.6. Check out this walk through (https://medium.com/analytics-vidhya/how-to-install-roracle-on-windows-10-144b0b923dac). It covers most of the things you need to get ROracle installed (changing path variables, installing Rtools, having the Oracle instant client).

######
#Once loaded packages edit your user name and database specifics and run from here down
#You have to put in your user name
library(ROracle) #database

usr <- c("user name") #e.g., ajones
#Asks for you pswd in pop up (no need to edit here)
#pswd <- .rs.askForPassword('Password') #For a pop up window
pswd <- ('PASSWORDHERE')

#Database specifics
drv <- dbDriver("Oracle")
host <- "sole.nefsc.noaa.gov"
port <- 1526
sid <- "sole"

#Putting all of that together
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")

## Use username/password authentication.
con <- dbConnect(drv, username = usr, password = pswd,
                 dbname = connect.string)

#Setting up a connection to nova
drv <- dbDriver("Oracle")
host <- "nova.nefsc.noaa.gov"
port <- 1526
sid <- "nova"


connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")

con.nova <- dbConnect(drv, username = usr, password = pswd,
                      dbname = connect.string)

