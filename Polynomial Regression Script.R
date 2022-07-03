# MarinStatsLectures - marin.mike@gmail.com
# Read in the "NEW" LungCapData2...
# (this data has a 'nice' non-linearity to work with)
LungCapData2 <- read.table("LungCapData2.csv", sep=",", header=T)
# and attach the data
attach(LungCapData2)
# ask for a summary of the data
summary(LungCapData2)

# make a plot of LungCap vs. Height
plot(Height, LungCap, main="Polynomial Regression", las=1)

# now, let's fit a linear regression
model1 <- lm(LungCap ~ Height)
summary(model1)
# and add the line to the plot...make it thick and red...
abline(model1, lwd=3, col="red")

# first, the WRONG WAY...
model2 <- lm(LungCap ~ Height + Height^2)
summary(model2)

# now, the RIGHT WAY...
model2 <- lm(LungCap ~ Height + I(Height^2))
summary(model2)

# or, create Height^2, and then include this in model...it's the same!
HeightSquare <- Height^2
model2again <- lm(LungCap ~ Height + HeightSquare)
summary(model2again)

# or, use the "poly" command...it's the same!
model2againagain <- lm(LungCap ~ poly(Height, degree=2, raw=T))
summary(model2againagain)

# let's remind ourselves of the polynomial model we fit
summary(model2)

# now, let's add this model to the plot, using a thick blue line
lines(smooth.spline(Height, predict(model2)), col="blue", lwd=3)

# test if the model including Height^2 i signif. better than one without
# using the partial F-test
anova(model1, model2)

# try fitting a model that includes Height^3 as well
model3 <- lm(LungCap ~ Height + I(Height^2) + I(Height^3))
summary(model3)

# now, let's add this model to the plot, using a thick dashed green line
lines(smooth.spline(Height, predict(model3)), col="green", lwd=3, lty=3)

# and, let's add a legend to clarify the lines
legend(46, 15, legend = c("model1: linear", "model2: poly x^2", "model3: poly x^2 + x^3"), 
       col=c("red", "blue", "green"), lty=c(1,1,3), lwd=3, bty="n", cex=0.9)

# let's test if the model with Height^3 is signif better than one without
anova(model2, model3)
