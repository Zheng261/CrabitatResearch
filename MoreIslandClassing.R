install.packages("glcm")
library(glcm)
#testimg <- brick("palmyrahdquestion.jpg")
testimgbw <- raster("palmyrahdquestionbw.jpg")
#plotRGB(testimgbw)
plot(testimgbw)


glcmtestimg <- glcm(testimgbw,window=c(3,3))

