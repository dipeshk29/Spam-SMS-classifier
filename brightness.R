# img = imager image
# bright = positive or negative number indicating "m"
library(imager)
setwd("C:/Users/MTH/Documents/MTH208_dipesh/assignment-2-dipeshk29-main/assignment-2-dipeshk29-main")
img<-load.image("campus.jpeg")
plot(img)
change_brightness <- function(img, bright = .2)
{
  if(bright<0){
    bright=0
  }
  else{
    bright=1
  }
  col.mat<-as.array(img[,,1,])
  dims<-dim(col.mat)
  change<-array(0,dim=c(dims[1],dims[2], dims[3]))
  for(i in 1:dims[1]){
    for(j in 1:dims[2]){
      for(k in 1:dims[3]){
        change[i,j,k]<-col.mat[(i+bright) , (j+bright)  ) , (k+bright)]
        
      }
      
    }
  }
  
  
  changed_img <- as.cimg(change)
  plot(chnaged_image)# make sure to return imager image object
  return(changed_img)
}

change_brightness(img,bright = .2)










# Load necessary library
library(imager)

# Set working directory (adjust path as needed)
setwd("C:/Users/MTH/Documents/MTH208_dipesh/assignment-2-dipeshk29-main/assignment-2-dipeshk29-main")

# Load the image
img <- load.image("campus.jpeg")
plot(img)

# Function to change brightness
change_brightness <- function(img, bright = 0.2) {
  # Ensure brightness value is within a valid range
  if (bright < -1) {
    bright <- -1
  } else if (bright > 1) {
    bright <- 1
  }
  
  # Extract the image array and its dimensions
  col.mat <- as.array(img[,,1,])
  dims <- dim(col.mat)
  
  # Create an array to hold the changed image data
  change <- array(0, dim = c(dims[1], dims[2], dims[3]))
  
  # Adjust the brightness for each pixel
  for (i in 1:dims[1]) {
    for (j in 1:dims[2]) {
      for (k in 1:dims[3]) {
        # Apply brightness adjustment and clip to [0, 1] range
        new_value <- col.mat[i, j, k] + bright
        change[i, j, k] <- pmin(pmax(new_value, 0), 1)
      }
    }
  }
  
  # Convert array back to image
  changed_img <- as.cimg(change)
  
  # Plot and return the changed image
  plot(changed_img)
  return(changed_img)
}

# Apply the brightness change
change_brightness(img, bright = 0.2)
