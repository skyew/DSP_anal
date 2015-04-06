#load Nasis data into R

# have a nasis_local ODBC connection

#setup
install.packages("RODBC", dep=TRUE)
install.packages('soilDB', dep=TRUE)
install.packages("SSOAP", repos = "http://www.omegahat.org/R", type="source")
install.packages("cluster")
install.packages("ape")

# change options to 32-bit
#The ODBC connection to an Access database only functions using a 32-bit version of R. The fetchPedonPC(dsn) command, therefore, only functions using a 32-bit version of R. If RStudio is using the 64-bit version of R, the user must follow these steps: Choose "Options" from the tools dropdown menu, select the "Change." button next to the R Version, and select the radio button "Use your machine's default version of R (32-bit)." The user then selects "OK" and is prompted to quit and re-open RStudio.

library(soilDB)
library(aqp)
library(plyr)
library(ggplot2)

f <- fetchNASIS()

table(f@site$pedon_id)

#data

dsp <- read.csv("C:/Users/skye.wills/Documents/DSP/DSP_data/dsp_data.csv")

#labels
dsp_labels <- read.csv("C:/Users/skye.wills/Documents/DSP/DSP_data/dsp_label.csv")

PROJECT<-"ID_Threebear"

out.loc <- paste0("~/DSP/DSP_data/output/", PROJECT, "/")

pr <- which(dsp$Name==PROJECT)
dsp_pr <- dsp[pr,]




dsp_userid <- dsp_pr$UserPedonID
qq <- toString(shQuote(dsp_userid))
ql <- paste(qq, collapse=",")


#select project portion of pedons

p <- which(f@site$pedon_id==ql)
f.p

plot(f.p)

