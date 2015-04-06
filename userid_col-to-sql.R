#takes column from csv file and transforms it into a character string delimeted by commas with quorations
#for use in SQL queries

dsp <- read.csv("C:/Users/skye.wills/Documents/DSP/DSP_data/dsp_data.csv")
dsp_userid <- dsp$UserPedonID
qq <- toString(shQuote(dsp_userid))
ql <- paste(qq, collapse=",")
writeChar(ql, con="C:/Users/skye.wills/Documents/DSP/DSP_data/userid" )



#lc <- paste(dsp_userid, collapse=",")
#write.table(dsp_userid, file = "C:/Users/skye.wills/Documents/DSP/DSP_data/userid.csv", sep = ",", row.names=F, col.names=F)

