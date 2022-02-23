#naieve bayes run across a directory

#libraries
library(klaR)
library(ggplot2)
library(stringr)
library(png)
library(e1071)
library(readr)
library(imager)
library("grid")
library("gridExtra")






#get directory name 
getwd()

#create list for empty results

ImagePixelNumbers<-data.frame()

#list files in directory change this to what ever directory you need
#will give you a list of directories
#list.dirs()

ListofImages<-list.files("./InputImages")

#for loop starting with each file 
i<-1
for (i in 1:length(ListofImages)) {
  print(paste(ListofImages[i]," is running"))

Imagename<- ListofImages[i]
#import image 

Image <- load.image(paste0('InputImages/',Imagename))

#store as data.frame

DF.Image<-as.data.frame(Image,wide="c")

#multiply by 255 *png are in percent 
DF.Image[,3:5]<-DF.Image[,3:5] *255

#converto to HSV
DF.Image[,3:5] <- t(rgb2hsv(t(DF.Image[,3:5])))

colnames(DF.Image)<-c("x","y","R","G","B","GAMMA")
#predict on image

pred <- predict(nb_mod, DF.Image,type="class")





#add label to class
DF.Image.labeled <- cbind(DF.Image,pred$class)
colnames(DF.Image.labeled)<-c("x","y","R","G","B","GAMMA","label")




### create a data frame of labels and the posteior probability of each class 
colors <- data.frame(
  label = pred$class, 
  background = pred$posterior[,"background"],
  leaf = pred$posterior[,"leaf"],
  lesion = pred$posterior[,"lesion"]
)

#this is to replace with mean colors we potenially can just multiply the posterior by 255 and take a weighted average 
AVG.colors<-aggregate(.~label, data=colors, mean)

#multiply by 255 for color 
AVG.colors[,2:4]<-AVG.colors[,2:4]#*255

colnames(AVG.colors)<- c("label", "red","green","blue")




# merge color codes on to df
# IMPORTANT: we must maintain the original order of the df after the merge!
DF.Image.labeled$order = 1:nrow(DF.Image.labeled)


DF.Image.labeled.2 <- merge(DF.Image.labeled, AVG.colors, by="label",all = TRUE, sort = FALSE)

DF.Image.labeled.2 <- cbind(DF.Image.labeled.2, colors)

#print probability color
# get probability color channel values for each row of the df.
R <- matrix(DF.Image.labeled.2$background, nrow=dim(Image)[2])
G <- matrix(DF.Image.labeled.2$leaf, nrow=dim(Image)[2])
B <- matrix(DF.Image.labeled.2$lesion, nrow=dim(Image)[2])

# reconstitute the segmented image in the same shape as the input image
Image.segmented <- array(dim=c(dim(Image)[1],dim(Image)[2],3))
Image.segmented[,,1] <- DF.Image.labeled.2$background
Image.segmented[,,2] <- DF.Image.labeled.2$leaf
Image.segmented[,,3] <- DF.Image.labeled.2$lesion
# View the result 
#this is the result you're using to infer if you need more sampling.

#red = background, green = leaf, blue = lesion

png(paste0("OutputImages/ProbabilityColor_",Imagename,".png"))
grid.raster(Image.segmented)
dev.off()


#summary table this gives the number of pixels 

LeafPixels<-summary(pred$class)[2]
LesionPixels<-summary(pred$class)[3]



results<-c(Imagename,LeafPixels,LesionPixels)

ImagePixelNumbers<-rbind(ImagePixelNumbers,results)
print(paste(ListofImages[i]," is finished"))



} #stop for loop here !



colnames(ImagePixelNumbers)<-c("filename","LeafPixels","LesionPixels")

#output Image pixel numbers

write.csv(ImagePixelNumbers,"OutputData/NB.pixelresults.csv",row.names=FALSE)



