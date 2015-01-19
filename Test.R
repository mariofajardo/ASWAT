source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")
biocLite('CRImage')
library(EBImage)
library(CRImage)
img <- readImage('DATA/test_slack.jpg')

display(img)

display(img[600:800,260:500,3]>.5)
a<-!img[600:800,260:500,3]>.5

y <- closing(a, makeBrush(5, shape='disc'))

display(y)
## bwlabel
z <- bwlabel(y)
display(normalize(z), title='agregates')

## recolor agregates in colors
cols = c('black', sample(rainbow(max(z))))
zrainbow = Image(cols[1+z], dim=dim(z))
display(zrainbow, title='agregates (recolored)')




computeFeatures.shape(z)


