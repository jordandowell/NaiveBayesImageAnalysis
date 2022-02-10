#Script 1-B using segmented images to derive training and test data for naieve bayes
  #Using balanced datasets
  #install as necessary
library(klaR)
library(ggplot2)
library(stringr)
library(png)
library(e1071)
library(readr)
library(imager)
# load the PNG into an RGB image object
getwd()
#import background data
background <- read.table("InputData/background (1).csv", sep = ",", header = TRUE)
x <- str_split_fixed(background$X92.92.101, ",", 3)
View(x)

colnames(x) <-c("R", "G", "B")
background <- x

#import chlorosis data
chlorosis <- read.table("InputData/chlorosis.csv", sep = ",", header = TRUE)
x <- str_split_fixed(chlorosis$X221.205.86, ",", 3)
View(x)

colnames(x) <-c("R", "G", "B")
chlorosis <- x

#import leaf data
leaf <- read.table("InputData/leaf.csv", sep = ",", header = TRUE)
x <- str_split_fixed(leaf$X143.155.80, ",", 3)
View(x)

colnames(x) <-c("R", "G", "B")
leaf <- x

#import lesion data
lesion <- read.table("InputData/lesion.csv", sep = ",", header = TRUE)
x <- str_split_fixed(lesion$X122.110.77, ",", 3)
View(x)

colnames(x) <-c("R", "G", "B")
lesion <- x

#label adding
background<-cbind(background,label="background")
chlorosis<-cbind(chlorosis,label="chlorosis")
leaf<-cbind(leaf,label="leaf")
lesion<-cbind(lesion,label="lesion")

#Plant <- readPNG("Documents/Kliebenstein Lab/InputImages/T1_C_leafnolesion.png")
# reshape image into a data frame
# df = data.frame(
#   red = matrix(Plant[,,1], ncol=1),
#   green = matrix(Plant[,,2], ncol=1),
#   blue = matrix(Plant[,,3], ncol=1),
#   gamma = matrix(Plant[,,4], ncol=1)
# # )
# 
# #take note of what the dataframe looks like. 
# #you can just import your dataset with RGB and create a secondary column thats all Gamma ==1 
# 
# 
# #remove pixels with 0 or 1 
# # #because some of the image is just black or white you want to remove pixels that are fixed 
# # #i.e. each colored section should have non 1 or 0 values
# # df<-df[df$red != 0,]
# # df<-df[df$red != 1,]
# 
# #add column with name 
# #feel free to change these names 
# #this provides a factor for the class we want to predict
# # df$label<- as.factor("leafnolesion")
# # LeafNoLesion_pixels<-df
# 
# 
# #done with importing what a leaf looks like 
# 
# #import lesion data the same details as the leaf apply here 
# 
# Plant <- readPNG("Documents/Kliebenstein Lab/InputImages/T1_C_lesion.png")
# 
# 
# # reshape image into a data frame
# df = data.frame(
#   red = matrix(Plant[,,1], ncol=1),
#   green = matrix(Plant[,,2], ncol=1),
#   blue = matrix(Plant[,,3], ncol=1),
#   gamma = matrix(Plant[,,4], ncol=1)
# )
# 
# #remove pixels with 0 or 1 
# 
# df<-df[df$red != 0,]
# df<-df[df$red != 1,]
# 
# #add column with name 
# df$label<- as.factor("lesion")
# Lesion_pixels<-df
# 
# 
# #done with importing what a lesion looks like 
# 
# #import background data the same details as the leaf apply here 
# 
# Plant <- readPNG("Documents/Kliebenstein Lab/InputImages/T1_C_background.png")
# 
# 
# # reshape image into a data frame
# df = data.frame(
#   red = matrix(Plant[,,1], ncol=1),
#   green = matrix(Plant[,,2], ncol=1),
#   blue = matrix(Plant[,,3], ncol=1),
#   gamma = matrix(Plant[,,4], ncol=1)
# )
# 
# #remove pixels with 0 or 1 
# 
# df<-df[df$red != 0,]
# df<-df[df$red != 1,]
# 
# #add column with name 
# df$label<- as.factor("background")
# background_pixels<-df
# 
# 
# #if we look at the dimensions of the data we can see that they are unbalanced
# #we define the prior probability of classification as the proportion of the training data
# #if you have an unbalanced data set this can potentially lead to errors. 
# #refer to Script 1-A to look at balanced data
# 
# #first position of dim refers to the number of pixels
# dim(Lesion_pixels)
# dim(LeafNoLesion_pixels)
# dim(background_pixels)
# 
# 
# #to make out data set balanced we are going to delete every other pixel 
# #until we have approximately equal datasets
# 
# #e.g. lesion is the smallest data set 
# #so we only need to delete from leaf and background
# #we delete every other pixel with the assumption that pixels close to each other are similar
# # so we end up with a good range
# #this could potentially be replaced with a PCA result 
# #identifying the N number of pixels that explain the most color variation 
# 
# #delete every n-th row based on the smallest dataset
# Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
# 
# LeafNoLesion_pixels2<- Nth.delete(LeafNoLesion_pixels,2)
# LeafNoLesion_pixels3<-Nth.delete(LeafNoLesion_pixels2,2)
# LeafNoLesion_pixels4<-Nth.delete(LeafNoLesion_pixels3,2)
# LeafNoLesion_pixels5<-Nth.delete(LeafNoLesion_pixels4,2)
# 
# 
# background_pixels2<-Nth.delete(background_pixels,2)
# background_pixels3<-Nth.delete(background_pixels2,2)
# background_pixels4<-Nth.delete(background_pixels3,2)
# background_pixels5<-Nth.delete(background_pixels4,2)
# 
# #the results should be approximate not necessarily exact
# 
# 
# #create test and training data 
# 
# background$label <- "background"
# chlorosis$label <- "chlorosis"
# leaf$label <- "leaf"
# lesion$label <- "lesion"


set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample.lesion <- sample.int(n = nrow(lesion), size = floor(.75*nrow(lesion)), replace = F)
train.lesion <- lesion[sample.lesion, ]
test.lesion  <- lesion[-sample.lesion, ]


set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample.leaf <- sample.int(n = nrow(leaf), size = floor(.75*nrow(leaf)), replace = F)
train.leaf <- leaf[sample.leaf, ]
test.leaf  <- leaf[-sample.leaf, ]
#LeafNoLesion was LeafNoLesion_pixels5

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample.background <- sample.int(n = nrow(background), size = floor(.75*nrow(background)), replace = F)
train.background <- background[sample.background, ]
test.background  <- background[-sample.background, ]
#background was background_pixels5

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample.chlorosis <- sample.int(n = nrow(chlorosis), size = floor(.75*nrow(chlorosis)), replace = F)
train.chlorosis <- chlorosis[sample.chlorosis, ]
test.chlorosis  <- chlorosis[-sample.chlorosis, ]


############Combine lesion and leaf data

train.FullLeaf_pixels<-data.frame(rbind(train.leaf,train.lesion,train.background))
#make sure that label is a factor and not a character
train.FullLeaf_pixels$label<-as.factor(train.FullLeaf_pixels$label)
train.FullLeaf_pixels[,1:3]<-sapply(train.FullLeaf_pixels[,1:3],as.numeric)


test.FullLeaf_pixels<-data.frame(rbind(test.leaf,test.lesion,test.background))
#make sure that label is a factor and not a character
test.FullLeaf_pixels$label<-as.factor(test.FullLeaf_pixels$label)
test.FullLeaf_pixels[,1:3]<-sapply(test.FullLeaf_pixels[,1:3],as.numeric)
#convert RGB to hsv

train.FullLeaf_pixels[,1:3] <- t(rgb2hsv(t(train.FullLeaf_pixels[,1:3])))

test.FullLeaf_pixels[,1:3] <- t(rgb2hsv(t(test.FullLeaf_pixels[,1:3])))



#Run naieve bayes
#label~. means to predict label from all avaliable data in the dataset
nb_mod <- NaiveBayes(label ~ ., usekernel=T, data=train.FullLeaf_pixels)
#here we are using the predict function to predict on the independent test set

pred <- predict(nb_mod, test.FullLeaf_pixels,type="class")
#this is the same however we then have the predicted probabilities of each class
#the e1071 needs to be used to get raw values ignore this for now 
#raw.pred <-predict(nb_mod, test.FullLeaf_pixels,type="raw")



#assess the confusion matrix for trainig set 

#left is predicted
#top is observed
tab <- table(pred$class, test.FullLeaf_pixels$label)
caret::confusionMatrix(tab) 

# Plot density of each feature using nb_mod
#change legend plot to T or F if you want to remove the label. 
#this could be tweaked the legend takes up a lot of space
opar = par(mfrow=c(2, 2), mar=c(4,0,0,0))
plot(nb_mod,legendplot = F,cex=0.2)  
par(opar)

# Plot the Confusion Matrix
#i wouldnt use this there are a lot of pixels so it gets overloaded. 
#  test$pred <- pred$class
# ggplot(test, aes(Species, pred, color = Species)) +
#   geom_jitter(width = 0.2, height = 0.1, size=2) +
#   labs(title="Confusion Matrix", 
#        subtitle="Predicted vs. Observed", 
#        y="Predicted", 
#        x="Observed")

#i still need to add a way to recapitulate the image 



#import image



Image <- load.image('InputImages/Alex_60_19_S2_91_.png')

#store as data.frame

DF.Image<-as.data.frame(Image,wide="c")

#multiply by 255 *png are in percent 
DF.Image[,3:5]<-DF.Image[,3:5] *255

#converto to HSV
DF.Image[,3:5] <- t(rgb2hsv(t(DF.Image[,3:5])))

colnames(DF.Image)<-c("x","y","R","G","B","GAMMA")
#predict on image

pred <- predict(nb_mod, DF.Image,type="class")


View(DF.Image)



library("grid")
library("gridExtra")







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

png("OutputImages/ProbabilityColor.png")
grid.raster(Image.segmented)
dev.off()


#stop here !




