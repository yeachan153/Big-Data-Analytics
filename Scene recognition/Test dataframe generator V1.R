library(dplyr)
library(readr)
library(tidyr)
library(jpeg)
library(ggplot2)
library(purrr)

##################################
# Paints images
paintImage = function(img,..., colors=1:3){
  img[,,-colors]=0; rasterImage(img,...)
}

# Reads in jpeg as df
readJPEG_as_df <- function(path, featureExtractor = I) {
  img = readJPEG(path)
  # image dimensions
  d = dim(img) 
  # add names to the array dimensions
  dimnames(img) = list(x = 1:d[1], y = 1:d[2], color = c('r','g','b')) 
  # turn array into a data frame 
  df  <- 
    as.table(img) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    # make the final format handier and add file name to the data frame
    mutate(file = basename(path), x = as.numeric(x)-1, y = as.numeric(y)-1) %>%
    mutate(pixel_id = x + 28 * y) %>% 
    rename(pixel_value = Freq) %>%
    select(file, pixel_id, x, y, color, pixel_value)
  # extract features 
  df %>%
    featureExtractor
}

##################################

# /home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition
path = readline("Enter working directory" )
setwd(path)

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 
list.files(path = path)
dir(path)
dir("images_split")

skies = dir("images_split/cloudy_sky/", full.names = TRUE)
rivers = dir("images_split/rivers/", full.names = TRUE)
sunsets = dir("images_split/sunsets/", full.names = TRUE)
trees = dir("images_split/trees_and_forest/", full.names = TRUE)
test_set = dir("images_split/test_set/", full.names = TRUE)
# 
# str(skies)
# str(rivers)
# str(sunsets)
# str(trees)
# str(test_set)
# 
# # Importing images - intensities of red, green and blue. 3 layers of 
# # matrixes - one for red, one for green, one for blue
# img = readJPEG(rivers[4])
# glimpse(img)
# 
# # See the images - set up the canvas
# plot.new()
# plot.window(c(0,2), c(0,2))
# 
# # paint the images in separate color channels
# paintImage(img, 0, 1, 1, 2) # original image
# paintImage(img, 1, 1, 2, 2, colors=1) # red channel
# paintImage(img, 0, 0, 1, 1, colors=2) # green channel
# paintImage(img, 1, 0, 2, 1, colors=3) # blue channel
# 
# # array dimensions
# d = dim(img) 
# 
# # add names to the array dimensions
# dimnames(img) = list(x = 1:d[1], y = 1:d[2], color = c('r','g','b')) 
# 
# # turn array into a data frame 
# as.table(img) %>% 
#   as.data.frame(stringsAsFactors = FALSE) 
# 
# # Test the readJPEG function
# readJPEG_as_df(sunsets[1]) %>% head()

peekImage = . %>% spread(color, pixel_value) %>%  mutate(x=rev(x), color = rgb(r,g,b)) %>%  {ggplot(., aes(y, x, fill = color)) + geom_tile(show.legend = FALSE) + theme_light() + 
    scale_fill_manual(values=levels(as.factor(.$color))) + facet_wrap(~ file)}
# 
# readJPEG_as_df(rivers[2]) %>% peekImage
# readJPEG_as_df(sunsets[18]) %>% peekImage
# 
# readJPEG_as_df(sunsets[18]) %>% ggplot(aes(pixel_value, fill=color)) + geom_density(alpha=0.5)
# 
# bind_rows(
#   readJPEG_as_df(rivers[2]) %>% mutate(category = "rivers"), 
#   readJPEG_as_df(sunsets[10]) %>% mutate(category = "sunsets"), 
#   readJPEG_as_df(trees[122]) %>% mutate(category = "trees_and_forest"), 
#   readJPEG_as_df(skies[20]) %>% mutate(category = "cloudy_sky")
# ) %>% 
#   ggplot(aes(pixel_value, fill=color)) + geom_density(alpha=0.5, col=NA) + facet_wrap(~ category)

# load first 10 images from each category
# Rivers  = map_df(rivers[1:10], readJPEG_as_df) %>% mutate(category = "rivers")
# Sunsets = map_df(sunsets[1:10], readJPEG_as_df) %>% mutate(category = "sunsets")
# Skies   = map_df(skies[1:10], readJPEG_as_df) %>% mutate(category = "cloudy_sky")
# Trees   = map_df(trees[1:10], readJPEG_as_df) %>% mutate(category = "trees_and_forest")
# 
# # histograms over 10 images from each class
# bind_rows(Rivers, Sunsets, Skies, Trees) %>% 
#   ggplot(aes(pixel_value, fill=color)) + geom_histogram(bins=30, alpha=0.5) + facet_wrap(~category)

nr = nc = 3

myFeatures  <- . %>% # starting with '.' defines the pipe to be a function 
  group_by(file, X=cut(x, nr, labels = FALSE)-1, Y=cut(y, nc, labels=FALSE)-1, color) %>%
  summarise(
    m = mean(pixel_value),
    s = sd(pixel_value),
    min = min(pixel_value),
    max = max(pixel_value),
    q25 = quantile(pixel_value, .25),
    q75 = quantile(pixel_value, .75)
  ) 

# readJPEG_as_df(skies[4], myFeatures) %>% head(10)

# let's image what we have been calculating: compare the original image with the computed features
# gridExtra::grid.arrange(
#   readJPEG_as_df(skies[4]) %>% peekImage,
#   readJPEG_as_df(skies[4], myFeatures) %>% transmute(pixel_value=q75,x=2-X,y=Y,color=color) %>% peekImage,
#   readJPEG_as_df(rivers[4]) %>% peekImage,
#   readJPEG_as_df(rivers[4], myFeatures) %>% transmute(pixel_value=q75,x=2-X,y=Y,color=color) %>% peekImage,
#   layout_matrix = matrix(1:4,2,byrow=T)
# )

# To wide df
# readJPEG_as_df(sunsets[1], myFeatures) %>%
#   gather(feature, value, -file, -X, -Y, -color) %>% 
#   unite(feature, color, X, Y, feature) %>% 
#   spread(feature, value) %>% 
#   mutate(category = "sunset")

# because we need to reshape from long to wide format multiple times lets define a function:
myImgDFReshape = . %>%
  gather(feature, value, -file, -X, -Y, -color) %>% 
  unite(feature, color, X, Y, feature) %>% 
  spread(feature, value)

Sunsets = map_df(sunsets, readJPEG_as_df, featureExtractor = myFeatures) %>% 
  myImgDFReshape %>%
  mutate(category = "sunsets")
Trees = map_df(trees, readJPEG_as_df, featureExtractor = myFeatures) %>% 
  myImgDFReshape %>%
  mutate(category = "trees_and_forest")
Rivers = map_df(rivers, readJPEG_as_df, featureExtractor = myFeatures) %>% 
  myImgDFReshape %>%
  mutate(category = "rivers")
Skies = map_df(skies, readJPEG_as_df, featureExtractor = myFeatures) %>% 
  myImgDFReshape %>%
  mutate(category = "cloudy_sky")
Test = map_df(test_set, readJPEG_as_df, featureExtractor = myFeatures) %>% 
  myImgDFReshape %>%
  mutate(category = NA)


Train = 
  bind_rows(Sunsets, Trees, Rivers, Skies, Test) %>% 
  select(file, category, everything())

path2 = "/home/yeachan153/Desktop/BDS/R/Big Data Analytics/Big-Data-Analytics/Scene recognition/Train_V1.csv"
write.csv(Train, path2, row.names = F, fileEncoding = "UTF-8")

# head(Train)









