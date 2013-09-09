#R version 2.10.0 (2009-10-26)
#Copyright (C) 2009 The R Foundation for Statistical Computing
#ISBN 3-900051-07-0

#installing n loading packages needed.
install.packages('ggplot2')
install.packages('ReadImages')
library('rimage') #test for 2.10(?)
library('ReadImages')
library('ggplot2')


#get directory
getwd()

#load the graph
dev.new()
ggplot <- read.jpeg(system.file("data", "gg_01.jpg", package="ReadImages"))
plot(ggplot)

#calibrating with four points, two on x axis, two on the y axis
calpoints <- locator(n=4,type='p',pch=4,col='blue',lwd=2)
as.data.frame(calpoints)

#get the location of the points put on the graph
data <- locator(type='p',pch=1,col='red',lwd=1.2,cex=1.2)
as.data.frame(data)


#calibrate function
calibrate = function(calpoints,data,x1,x2,y1,y2)
{
	x  <- calpoints$x[c(1,2)]
	y  <- calpoints$y[c(3,4)]
	cx <- lm(formula = c(x1,x2) ~ c(x))$coeff
	cy <- lm(formula = c(y1,y2) ~ c(y))$coeff
	data$x <- data$x*cx[2]+cx[1]
	data$y <- data$y*cy[2]+cy[1]
	return(as.data.frame(data))
}


true.data <- calibrate(calpoints,data,1,5,1,5)

dev.new()
plot(true.data,type='b',pch=1,col='blue',lwd=1.1,bty='l')



###################################END##############################################
#in function format
ReadAndCal = function(fname)
{
	img <- read.jpeg(fname)
	plot(img)
	calpoints <- locator(n=4,type='p',pch=4,col='blue',lwd=2)
	return(calpoints)
}

DigitData = function(color='red') locator(type='p',pch=1,col=color,lwd=1.2,cex=1.2)

Calibrate = function(calpoints,data,x1,x2,y1,y2)
{
	x  <- calpoints$x[c(1,2)]
	y  <- calpoints$y[c(3,4)]
	cx <- lm(formula = c(x1,x2) ~ c(x))$coeff
	cy <- lm(formula = c(y1,y2) ~ c(y))$coeff
	data$x <- data$x*cx[2]+cx[1]
	data$y <- data$y*cy[2]+cy[1]

	return(as.data.frame(data))
}

###################################END##############################################
   x        y
1 282.59540 234.5394
2 264.92406 256.9684
3 108.60074 300.4670
4  54.90707 247.4530
5 167.05207 118.3164
6 183.36407 116.2774
7 255.40873 133.2690
8 265.60373 152.9794


#confirmation- re-generating plot

my.x<- c(true.data$x, true.data$y, col ="red")


my.x <- c(282.5954,
264.92406,
108.60074,
54.90707,
167.05207,
183.36407,
255.40873,
265.60373,
54.90707,
167.05207,
183.36407,
255.40873,
265.60373 )

my.y <-c(234.5394,
256.9684,
300.467,
247.453,
118.3164,
116.2774,
133.269,
152.9794,
247.453,
118.3164,
116.2774,
133.269,
152.9794)
points(m.x,my.y,col="red")

###################################END##############################################

#concatinating the resutls
#set all attributes to zero

#


