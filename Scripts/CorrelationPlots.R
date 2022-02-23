#building scatter plots and correlation plots for pixel data

library(stringr)
library(psych)

#import NB bayes data 

NB.PixelNumbers<-read.csv("OutputData/NB.pixelresults.csv")

#move the .png for filename

NB.PixelNumbers$filename<-str_sub(NB.PixelNumbers$filename,1,nchar(NB.PixelNumbers$filename)-4)



#create proportion column

NB.PixelNumbers$Proportions<-NB.PixelNumbers$LesionPixels/NB.PixelNumbers$LeafPixels



#import truth data


Truth.Numbers<-read.csv("InputData/Truthdata.csv ")

#Truth.Numbers<-Truthdata


#merge by file and image name 

Fulldataframe<-merge(Truth.Numbers,NB.PixelNumbers,by.x = "image.name", by.y = "filename")

View(Fulldataframe)

#make plot 
pdf("OutputData/Correlationplots.pdf")
pairs.panels(Fulldataframe[,-1], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()
