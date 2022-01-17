#Script 1: importing image
#packages used
library("png")
library("grid")
library("gridExtra")
library("EBImage")

#library(CRImage)

#ensure that all images in dataset are cropped. 

# load the PNG into an RGB image object
Plant <- readPNG("InputImages/Iris.png")

#save dimensions of the image 

Plate.row<-dim(Plant)[1]
Plate.col<-dim(Plant)[2]

# reshape image into a data frame
Plant.RBGg <- data.frame(
  red = matrix(Plant[,,1], ncol=1),
  green = matrix(Plant[,,2], ncol=1),
  blue = matrix(Plant[,,3], ncol=1),
  gamma = matrix(Plant[,,4],ncol = 1)
)


# Convert Image from rgb to hsv 
# The 14-bit pixel value range is 0:16383  ]
Plant.hsv <- t(rgb2hsv(t(Plant.RBGg[,1:3]), maxColorValue = 16383))

dim(Plant.RBGg)

#reshape image
Plant.HSVg <- data.frame(
hue = matrix(Plant.hsv[,1], ncol=1),
saturation = matrix(Plant.hsv[,2], ncol=1),
value = matrix(Plant.hsv[,3], ncol=1),
gamma = matrix(Plant[,,4],ncol = 1)
)



dim(Plant.HSVg)





#end of Script!!!!!!!


# This image is 2192px x 4656 x 3 array
dim(Plant)
View(Plant)


# reshape image into a data frame
df <- data.frame(
  red = matrix(Plant[,,1], ncol=1),
  green = matrix(Plant[,,2], ncol=1),
  blue = matrix(Plant[,,3], ncol=1),
  gamma = matrix(Plant[,,4],ncol = 1)
)

#at this step we can convert to HSV but save that for next week 
#convert RGB to HSV

#add HSV to dataframe

#create a 2nd dataframe to store hsv colors and gamma 

#lets use K-means to get an estimate of color distribution 
### compute the k-means clustering
K = kmeans(df,12)
df$label = K$cluster
### Replace the color of each pixel in the image with the mean 
### R,G, and B values of the cluster in which the pixel resides:
# get the coloring
?kmeans
#changed this to HSV and now need to get colors 

colors = data.frame(
  label = 1:nrow(K$centers), 
  R = K$centers[,"red"],
  G = K$centers[,"green"],
  B = K$centers[,"blue"],
  GAMMA = K$centers[,"gamma"]
)
# merge color codes on to df
# IMPORTANT: we must maintain the original order of the df after the merge!
df$order = 1:nrow(df)
df = merge(df, colors)
df = df[order(df$order),]
df$order = NULL

df
#rebuild the image

# get mean color channel values for each row of the df.
R = matrix(df$R, nrow=dim(Plant)[1])
G = matrix(df$G, nrow=dim(Plant)[1])
B = matrix(df$B, nrow=dim(Plant)[1])
GAMMA = matrix(df$GAMMA, nrow=dim(Plant)[1])
# reconstitute the segmented image in the same shape as the input image
Plant.segmented = array(dim=dim(Plant))
Plant.segmented[,,1] = R
Plant.segmented[,,2] = G
Plant.segmented[,,3] = B
Plant.segmented[,,4] = GAMMA
# View the result
grid.raster(Plant.segmented)





# get mean color channel values for each row of the df.
#H = matrix(colors$H, nrow=dim(Plant)[1])
#S = matrix(colors$S, nrow=dim(Plant)[1])
#V = matrix(colors$V, nrow=dim(Plant)[1])
#GAMMA = matrix(df$GAMMA, nrow=dim(Plant)[1])

# reconstitute the segmented image in the same shape as the input image
Plant.segmented = array(dim=c(186,491,3))
Plant.segmented[,,1] = H
Plant.segmented[,,2] = S
Plant.segmented[,,3] = V
Plant.segmented[,,4] = gamma
# View the result
grid.raster(Plant.segmented)




#lets use K-means to get an estimate of color distribution 
### compute the k-means clustering
K = kmeans(df,12)
df$label = K$cluster
### Replace the color of each pixel in the image with the mean 
### R,G, and B values of the cluster in which the pixel resides:
# get the coloring
colors = data.frame(
  label = 1:nrow(K$centers), 
  R = K$centers[,"red"],
  G = K$centers[,"green"],
  B = K$centers[,"blue"],
  GAMMA = K$centers[,"gamma"]
)
# merge color codes on to df
# IMPORTANT: we must maintain the original order of the df after the merge!
df$order = 1:nrow(df)
df = merge(df, colors)
df = df[order(df$order),]
df$order = NULL


#rebuild the image

# get mean color channel values for each row of the df.
R = matrix(df$R, nrow=dim(Plant)[1])
G = matrix(df$G, nrow=dim(Plant)[1])
B = matrix(df$B, nrow=dim(Plant)[1])
GAMMA = matrix(df$GAMMA, nrow=dim(Plant)[1])
# reconstitute the segmented image in the same shape as the input image
Plant.segmented = array(dim=dim(Plant))
Plant.segmented[,,1] = R
Plant.segmented[,,2] = G
Plant.segmented[,,3] = B
Plant.segmented[,,4] = GAMMA
# View the result
grid.raster(Plant.segmented)



# perform PCA on the mandril data and add the uv coordinates to the dataframe
PCA = prcomp(df[,c("red","green","blue")], center=TRUE, scale=TRUE)
df$u = PCA$x[,1]
df$v = PCA$x[,2]
df$w <- PCA$x[,3]
# Inspect the PCA
# most of the cumulative proportion of variance in PC2 should be close to 1. 
summary(PCA)
#Importance of components:
#                          PC1    PC2     PC3
#Standard deviation     1.3903 0.9536 0.39695
#Proportion of Variance 0.6443 0.3031 0.05252
#Cumulative Proportion  0.6443 0.9475 1.00000
# mandrill
ggplot(df, aes(x=u, y=v, col=rgb(red,green,blue))) + 
  geom_point(size=2) + scale_color_identity()
ggplot(df, aes(x=v, y=w, col=rgb(red,green,blue))) + 
  geom_point(size=2) + scale_color_identity()

# segmented mandrill
ggplot(df, aes(x=u, y=v, col=rgb(R,G,B))) + 
  geom_point(size=2) + scale_color_identity()

ggplot(df, aes(x=v, y=w, col=rgb(R,G,B))) + 
  geom_point(size=2) + scale_color_identity()

