#Script 1-A using segmented images to derive training and test data for naieve bayes
#Using unbalanced datasets
#install as necessary
library(e1071)
library(ggplot2)
library(png)
library(klaR)
# load the PNG into an RGB image object


Plant <- readPNG("InputImages/T1_C_leafnolesion.png")


# reshape image into a data frame
df = data.frame(
  red = matrix(Plant[,,1], ncol=1),
  green = matrix(Plant[,,2], ncol=1),
  blue = matrix(Plant[,,3], ncol=1),
  gamma = matrix(Plant[,,4], ncol=1)
)

#take note of what the dataframe looks like. 
#you can just import your dataset with RGB and create a secondary column thats all Gamma ==1 


#remove pixels with 0 or 1 
#because some of the image is just black or white you want to remove pixels that are fixed 
#i.e. each colored section should have non 1 or 0 values
df<-df[df$red != 0,]
df<-df[df$red != 1,]

#add column with name 
#feel free to change these names 
#this provides a factor for the class we want to predict
df$label<- as.factor("leafnolesion")
LeafNoLesion_pixels<-df


#done with importing what a leaf looks like 

#import lesion data the same details as the leaf apply here 

Plant <- readPNG("InputImages/T1_C_lesion.png")


# reshape image into a data frame
df = data.frame(
  red = matrix(Plant[,,1], ncol=1),
  green = matrix(Plant[,,2], ncol=1),
  blue = matrix(Plant[,,3], ncol=1),
  gamma = matrix(Plant[,,4], ncol=1)
)

#remove pixels with 0 or 1 

df<-df[df$red != 0,]
df<-df[df$red != 1,]

#add column with name 
df$label<- as.factor("lesion")
Lesion_pixels<-df


#done with importing what a lesion looks like 

#import background data the same details as the leaf apply here 

Plant <- readPNG("InputImages/T1_C_background.png")


# reshape image into a data frame
df = data.frame(
  red = matrix(Plant[,,1], ncol=1),
  green = matrix(Plant[,,2], ncol=1),
  blue = matrix(Plant[,,3], ncol=1),
  gamma = matrix(Plant[,,4], ncol=1)
)

#remove pixels with 0 or 1 

df<-df[df$red != 0,]
df<-df[df$red != 1,]

#add column with name 
df$label<- as.factor("background")
background_pixels<-df


#if we look at the dimensions of the data we can see that they are unbalanced
#we define the prior probability of classification as the propotion of the training data
#if you have an unbalanced data set this can potenially lead to errors. 
#refer to Script 1-A to look at balanced data

#first position of dim refers to the number of pixels
dim(Lesion_pixels)
dim(LeafNoLesion_pixels)
dim(background_pixels)




#create test and training data 

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample.lesion <- sample.int(n = nrow(Lesion_pixels), size = floor(.75*nrow(Lesion_pixels)), replace = F)
train.lesion <- Lesion_pixels[sample.lesion, ]
test.lesion  <- Lesion_pixels[-sample.lesion, ]


set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample.leaf <- sample.int(n = nrow(LeafNoLesion_pixels), size = floor(.75*nrow(LeafNoLesion_pixels)), replace = F)
train.leaf <- LeafNoLesion_pixels[sample.leaf, ]
test.leaf  <- LeafNoLesion_pixels[-sample.leaf, ]

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample.background <- sample.int(n = nrow(background_pixels), size = floor(.75*nrow(background_pixels)), replace = F)
train.background <- background_pixels[sample.background, ]
test.background  <- background_pixels[-sample.background, ]

############Combine lesion and leaf data

train.FullLeaf_pixels<-rbind(train.leaf,train.lesion,train.background)
#make sure that label is a factor and not a character
train.FullLeaf_pixels$label<-as.factor(train.FullLeaf_pixels$label)


test.FullLeaf_pixels<-rbind(test.leaf,test.lesion,test.background)
#make sure that label is a factor and not a character
test.FullLeaf_pixels$label<-as.factor(test.FullLeaf_pixels$label)

#convert RGB to hsv

train.FullLeaf_pixels[,1:3] <- t(rgb2hsv(t(train.FullLeaf_pixels[,1:3])))

test.FullLeaf_pixels[,1:3] <- t(rgb2hsv(t(test.FullLeaf_pixels[,1:3])))



#Run naieve bayes
#label~. means to predict label from all avaliable data in the dataset
nb_mod <- NaiveBayes(label ~ ., usekernel=T, data=train.FullLeaf_pixels)
#here we are using the predict function to predict on the independent test set

pred <- predict(nb_mod, test.FullLeaf_pixels,type="class")
#this is the same however we then have the predicted probabilities of each class
#this can be used to diagnose model bias 
#the e1071 needs to be used to get raw values ignore this for now 
#raw.pred <-predict(nb_mod, test.FullLeaf_pixels,type="raw")



#assess the confusion matrix for trainig set 

#left is predicted
#top is observed
tab <- table(pred, test.FullLeaf_pixels$label)
caret::confusionMatrix(tab) 

# Plot density of each feature using nb_mod
#change legend plot to T or F if you want to remove the label. 
#this could be tweaked the legend takes up a lot of space
#take note that they are all on the same scale 
#so its hard to get any information about lesion idenfication
opar = par(mfrow=c(2, 2), mar=c(4,0,0,0))
plot(nb_mod,legendplot = F,cex=0.2)  
par(opar)

# Plot the Confusion Matrix
#i wouldnt use this there are a lot of pixels so it gets overloaded. 
test$pred <- pred$class
ggplot(test, aes(Species, pred, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Confusion Matrix", 
       subtitle="Predicted vs. Observed", 
       y="Predicted", 
       x="Observed")

#i still need to add a way to recapitulate the image 
#and color pixels based on predicted class

