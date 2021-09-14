library(tidyverse)
library(methods)
library(grid)
library(scales)

#import data
tuesdata <- tidytuesdayR::tt_load('2021-03-30')
allShades <- tuesdata$allShades

#create list of chosen brands for visualization: organized with help of computer and hand to capture range of brands with varying average lightness and number of available products
brandslist <- c("Marc Jacobs Beauty", "COOLA", "FLOWER Beauty", "Winky Lux", "BLK/OPL", "NUDESTIX", "Almay", "La Mer", "Anastasia Beverly Hills", "Rare Beauty by Selena Gomez", "Yves Saint Laurent", "Shiseido", "NYX Professional Makeup", "FENTY BEAUTY by Rihanna", "Clinique", "Dior")

#create tibble narrowed by brandslist
brands <- allShades %>% filter(brand %in% brandslist)
brandslightness <- brands %>% group_by(brand) %>% summarize(mean = mean(lightness), sd = sd(lightness), n = n()) %>% arrange(mean)
brandsn <- brandslightness %>% arrange(n)

#calculate min and max lightness
minlightness <- min(brandslightness$mean)
maxlightness <- max(brandslightness$mean)

#create function for calculating number of branches
branchcalculate <- function(br) {
  b = which(brandsn$brand == br$brand[1])
  if(b <= 4) { return(8) }
  else if(b >= 5 && b <= 8) { return(9) }
  else if(b >= 9 && b <= 12) { return(10) }
  else { return(11) }
}

UnitAngle <- function(origin, point) {
  if((point[1] >= origin[1]) & (point[2] >= origin[2])) { angle = atan2(abs(point[2] - origin[2]), abs(point[1] - origin[1])) }
  else if((point[1] <= origin[1]) & (point[2] >= origin[2])) { angle = pi - atan2(abs(point[2] - origin[2]), abs(point[1] - origin[1])) }
  else if((point[1] <= origin[1]) & (point[2] <= origin[2])) { angle = pi + atan2(abs(point[2] - origin[2]), abs(point[1] - origin[1])) }
  else { angle = -1*atan2(abs(point[2] - origin[2]), abs(point[1] - origin[1])) }
  return(angle)
}

EuclideanDistance <- function(a, b) {
  distance = sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2)
  return(distance)
}

Interpolate <- function(min, max, value) {
  percent = (value - min)/(max - min)
  return(percent)
}

treenode <- setRefClass("treenode", fields = list(coord = "numeric", branchlength = "numeric", angle = "numeric", children = "list"))

nextcoord <- function(center, branchdirection, angle, branchlength) {
  low = branchlength*runif(1, min = 0.7, max = 0.9)
  high = branchlength*runif(1, min = 1, max = 1.1)
  radius = sample(c(low, high), 1, prob = c(0.95, 0.05))
  
  leftangle = angle + runif(1, min = 0, max = pi/4)
  rightangle = angle - runif(1, min = 0, max = pi/4)
  
  if(branchdirection == 1) { angle = leftangle }
  else if(branchdirection == 2) { angle = rightangle }
  else { angle = sample(c(leftangle, rightangle), 1) }
  
  newx = center[1] + radius*cos(angle)
  newy = center[2] + radius*sin(angle)
  if(newx < 0.05) { newx = newx + 0.01 }
  else if(newx > 0.95) { newx = newx - 0.01 }
  if(newy < 0.05) { newy = newy + 0.01 }
  else if(newy > 0.95) { newy = newy - 0.01 }
  return(c(newx, newy))
}

generatetree <- function(n, coord, angle, branchlength) {
  children = vector(mode = "list", length = 1)
  if (n > 1) {
    branchnumber = sample(c(2, 3), 1, prob = c(0.9, 0.1))
    for(i in seq(1:branchnumber)) {
      newcoord = nextcoord(coord, i, angle, branchlength)
      newangle = UnitAngle(coord, newcoord)
      newbranchlength = EuclideanDistance(newcoord, coord)
      children <- c(children, generatetree(n-1, newcoord, newangle, newbranchlength))
    }
  }
  
  if(length(children) > 1) {
    children <- children[- 1]
  }
  
  newnode <- treenode$new(coord = coord, children = children, branchlength)
  return(newnode)
}

drawbranch <- function(parentnode, childnode, cycles) {
  x1 = parentnode$coord[1]
  y1 = parentnode$coord[2]
  x2 = childnode$coord[1]
  y2 = childnode$coord[2]
  z1 = (x1 + x2)/2 + runif(1, min = -0.005, max = 0.005)
  z2 = (y1 + y2)/2 + runif(1, min = -0.005, max = 0.005)
  xcoord = c(x1, z1, z1, x2)
  ycoord = c(y1, z2, z2, y2)
  branchthickness = 1.2*Interpolate(12, 1, cycles)
  grid.bezier(x = xcoord, y = ycoord, gp = gpar(lwd = branchthickness))
}

drawleaf <- function(node, brand) {
  x1 = node$coord[1]
  y1 = node$coord[2]
  
  Sample = slice_sample(brand)
  Scale = Interpolate(max(brand$lightness), min(brand$lightness), Sample$lightness)
  r1 = 0.005 + 0.015*Scale
  
  color = Sample$hex
  grid.circle(x = x1, y = y1, r = r1, gp = gpar(col = color, fill = color))
}

walknode <- function(node, brand, cycles) {
  for(i in node$children) {
    if(!is.null(node$children[[1]])) { drawbranch(node, i, cycles) }
    else { drawleaf(node, brand) }
    walknode(i, brand, cycles + 1)
  }
}

#select company
s <- sample(1:16, 1)
brand <- brands %>% filter(brand == brandslightness$brand[s])

#setup trunk height and general tree sizing
percentlightness <- Interpolate(16, 1, s)
y <- 0.1 + 0.2*percentlightness
z <- 0.1 + 0.05*percentlightness
brandname <- brand$brand[1]  

#setup seed
x <- sample(1:123456, 1)
set.seed(x)

#generate tree
basetree <- generatetree(branchcalculate(brand), c(0.5, y), pi/2, z)

#draw trees
grid.newpage()
grid.curve(0.5, 0, 0.5, y, gp = gpar(lwd = 1.2))
walknode(basetree, brand, 1)
