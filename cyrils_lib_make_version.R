# Get Version of the Library
VERSION<-"cyrils"
version.number<-"1.0" 
version.date<- as.character(Sys.Date())

# Build DESCRIPTION file
filename<-"cyrils/DESCRIPTION"
cat("Package: cyrils\nTitle: A R package of useful functions\nVersion: ",file=filename)
cat(version.number,file=filename,append=TRUE)
cat("\nDate: ",file=filename,append=TRUE)
cat(version.date,file=filename,append=TRUE)
cat("\n",file=filename,append=TRUE)
cat("Author: C. Marsh\n",file=filename,append=TRUE)
cat("Description: A set of R functions for manipulating data and distributions\n",file=filename,append=TRUE)
cat("Maintainer: contact Author <craig.marsh10@gmail.com>\n",file=filename,append=TRUE)
cat("LazyData: true\n",file=filename,append=TRUE)
cat("Imports:\n",file=filename,append=TRUE)
cat("\t ggplot2,\n\t reshape2\n",file=filename,append=TRUE)

# Create R function to return R library version number
filename<-"cyrils/R/Version.R"
cat("\"Version\"<-\n",file=filename)
cat("function() {\n",file=filename,append=T)
cat(paste("return(\"",version.date,"\")\n",sep=""),file=filename,append=T)
cat("}\n",file=filename,append=T)

# Write a .html file to report version number for the Wiki
cat(paste("Version",VERSION),file="cyrils.html")

# Call script to transfer data-raw -> data as .RData
setwd("C:/Work/Software/MiscFunsForR/cyrils/data-raw/")
source("CopyOverToData.R")
# Exit
q()

