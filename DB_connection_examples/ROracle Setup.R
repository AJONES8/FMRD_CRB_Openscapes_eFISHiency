#ROracle setup

#Just so next time I remember
#I need: version specific Rtools (web), the devtools library (web),
#the instant client (from Oracle), the sdk instant client (from Oracle), as well as the version specific package (from Oracle)
#Once RTools and devtools are installed I can then set the path variables (see below)

#Looking at current PATH (must be CAPS)
Sys.getenv('PATH')

#Adding these to the current path (can be done in GUI too)
Sys.setenv(PATH = paste("C:\\Users\\andrew.jones\\Downloads\\instantclient-basic-windows.x64-11.2.0.4.0\\instantclient_11_2", Sys.getenv("PATH"), sep=";"))
Sys.setenv(PATH = paste("C:\\Rtools\\bin", Sys.getenv("PATH"), sep=";"))

#These are new variables of there own (can be done in GUI too)
Sys.setenv(OCI_LIB64='C:\\Users\\andrew.jones\\Downloads\\instantclient-basic-windows.x64-11.2.0.4.0\\instantclient_11_2')
Sys.setenv(OCI_INC='C:\\Users\\andrew.jones\\Downloads\\instantclient-sdk-windows.x64-19.3.0.0.0dbru\\instantclient_19_3\\sdk\\include')


#Then I can install the package from source
install.packages("C:/Users/andrew.jones/Downloads/ROracle_1.3-2.tar.gz", repos = NULL, type = "source")