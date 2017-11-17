\name{logihist}
\alias{logihist}
\alias{logibox}
\alias{logidot}
\title{
Combined Graphs for Logistic Regression
}
\description{
Plot combined graphs for logistic regressions in the ggplot2 system.
}
\usage{
logihist(x, y, scale.hist = 5, breaks = "Sturges", counts = TRUE, intervalo = 0,
   ylab2 = "Frequency", fillb = 1, colob = 1, sizeb = 1, pglm = FALSE, se = FALSE,
   sizeglm = 1, colglm = 1)
logibox(x, y, boxwd = 0.1, wiswd = 0.05, displac = 0, sizepat = 1, fillb = NA, colob = 1,
   colpat = 1, colbig = 1, colmed = 1, sizeb = 1, sizebig = 1, sizemed = 2, colout = 1,
   sizeout = 1, shapeout = 1, pglm = FALSE, se = FALSE, sizeglm = 1, colglm = 1)
logidot(x, y, incre = NULL, sizedot = NULL, coldot = 1, shapedot = 1, pglm = FALSE,
   se = FALSE, sizeglm = 1, colglm = 1)
}
 
\arguments{
  \item{x}{
Either the predictor variable or a glm object.
}
  \item{y}{
If \code{x} is a predictor variable, \code{y} is the response variable, i.e., either a logic (FALSE and TRUE) vector or a vector of 1's and 0's.
}
  \item{scale.hist}{
Value to scale the maximum vertical size of histograms.
}
  \item{breaks}{
How to compute the breakpoints for the histograms. See \code{\link[graphics]{hist}} in package \pkg{graphics}.
}
  \item{counts}{
Add counts above histogram bars (not implemented in  this version).
}
  \item{intervalo}{
Width of histogram bins.
}
  \item{ylab2}{
Label for the y-axis of the histograms.
}
  \item{fillb}{
Color to fill the histogram bars or the boxes of the boxplots. Either a single value or a vector witth different values for the 0 and 1 cases.
}
  \item{colob}{
Color to delineate the histogram bars or the boxes of the boxplots. Either a single or double value.
}
 \item{colpat}{
Color of  the line representing the whiskers in boxplots.
}
  \item{colbig}{
Color of  the line representing the end of whiskers in boxplots.
}
  \item{colmed}{
Color of  the line representing the median in boxplots.
}
  \item{colout}{
Color of  the line representing the outliers in boxplots.
} 
 \item{sizeb}{
Width of the lines that delineate the histogram bars or the boxes of the boxplots.
} 
\item{boxwd}{
Width of the boxes in boxplots.
}
  \item{wiswd}{
Size of the end of whiskers in boxplots.
}
   \item{sizebig}{
Width of  the line representing the end of whiskers in boxplots.
}
  \item{sizepat}{
Width of  the line representing the whiskers in boxplots.
}
  \item{sizemed}{
Width of  the line representing the median in boxplots.
}
 \item{sizeout}{
Size of the points representing the outliers in boxplots.
}
\item{shapeout}{
Shape of the points representing the outliers.  See the entry for \code{pch} in  \code{\link[graphics]{points}}.
}
 \item{displac}{
Vertical displacement of boxplots along the y-axis.
}
  \item{incre}{
Separation among points in the dotplot.
}
  \item{sizedot}{
Size of points in the dotplot.
}
  \item{coldot}{
Color of points in the dotplot.
}
  \item{shapedot}{
Shape of the points in the dotplot.  See the entry for \code{pch} in  \code{\link[graphics]{points}}.
}
  \item{pglm}{
Draw the fitted logistic curve? (TRUE or FALSE).
}
  \item{se}{
Draw the standard error around the fitted logistic curve?(TRUE or FALSE).
}
  \item{sizeglm}{
Width of the line for the fitted fitted logistic curve.
}
  \item{colglm}{
Color of the line for the fitted fitted logistic curve.
}
}


\details{
The use of functions \code{logihist}, \code{logibox} or  \code{logidot} will  render a combined graph for logistic regression. Either a double histogram, a double boxplot or a double dotplot, which could be modified or integrated with other graphical elements of \code{ggplot2}.
}
\note{
\code{logidot} would try to find the optimal size of dots to avoid overlaping. The default computed sizes and separation could be fine-tunned by the arguments \code{sizedot} and \code{incre}.
}
\value{
A combined graph for logistic regression.
}
\references{
\href{http://esapubs.org/bulletin/backissues/086-1/bulletinjan2005.htm#et}{De la Cruz Rot, M. 2005. Improving the Presentation of Results of Logistic Regression with R. ESA Bulletin 86:41-48.} 
}
\author{
Marcelino de la Cruz
}

\seealso{
The original implementation of the \code{\link[popbio]{logi.hist.plot}} function for the R graphic sytem in the package \pkg{popbio}.
}
\examples{

# Get some data from package popbio
data(aq.trans, package="popbio")
aq.trans$survived<-aq.trans$fate!="dead"
a<-subset(aq.trans, leaf<50 & stage!="recruit", c(leaf,survived))


# Transform the "survived" variable in numeric
# This is necesary to correctly plot the combined graph.
 
a$survived <-a$survived*1

# Fit a logistic regression
glm.a<- glm(survived~leaf, data=a, family=binomial)


# Histograms
# From individual variables
# Modifying the appearence of histogram bins
logihist(a$leaf,a$survived)
logihist(a$leaf,a$survived, fillb="blue")
logihist(a$leaf,a$survived, fillb="blue", colob="blue")
logihist(a$leaf,a$survived, fillb=c("orange","blue"), colob=c("orange","blue"))
logihist(a$leaf,a$survived, fillb=c("orange","blue"), colob=c("orange","blue"),
scale.hist=2)
logihist(a$leaf,a$survived, fillb=c("orange","blue"), colob=c("orange","blue"),
  breaks=seq(0,50, by=2))

# Modifying the combined plot using functions from ggpplot2

logihist(a$leaf,a$survived, fillb=NA, sizeb=0.5, ylab2="Number of plants", )+
   ylab("survived")+xlab("leaf")+ stat_smooth(method = "glm", method.args =
   list(family = "binomial"), se=TRUE, size=1, colour="black")+theme_light()


# The same fromm a glm object

logihist(glm.a, fillb=NA, sizeb=0.5, ylab2="Number of plants", se=TRUE)+ 
           theme_light()



# Dotplots
# Double dotplot from the individual variables, with different symbols for
#  0 and 1 categories (black and void, redand solid respectively).

logidot(a$leaf,a$survived,coldot=c(1,2),shapedot=c(1,19))

# Try with a larger dot.
logidot(a$leaf,a$survived,coldot=c(1,2),shapedot=c(1,19), sizedot=1)

# Add the fitted logistic curve and change the presentation using
# respectively an stat and a theme from ggplot2 

logidot(a$leaf,a$survived, coldot=c(1,2),shapedot=c(1,19),  sizedot=1)+
    stat_smooth(method = "glm", method.args = list(family = "binomial"),
           se=FALSE, size=2)+theme_light()+ylab("leaf")+xlab("survived")

# Get the same graphic directly form the fitted glm object
logidot(glm.a, coldot=c(1,2),shapedot=c(1,19),  sizedot=1, sizeglm=2,
           colglm="blue")+theme_light()



# Boxplots
logibox(a$leaf,a$survived)+theme_light()+ stat_smooth(method = "glm",
           method.args = list(family = "binomial"),se=FALSE )+
	   geom_jitter(height=0.02, size=0.5, colour="pink", alpha=0.5)



}
\keyword{ ~aplot  }