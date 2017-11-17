\name{logi.hist.plot2}
\alias{logi.hist.plot2}

\title{ Plot logistic regression  }
\description{ Plot combined graphs for logistic regressions 
  }
\usage{
logi.hist.plot2(independ, depend, logi.mod = 1, type = "dit", 
boxp = TRUE, rug = FALSE, ylabel = "Probability", ylabel2 = "Frequency", 
xlabel = "", mainlabel = "", las.h = 1, counts = FALSE, cex.p = 1, 
pch.dit = 1, incre = 0.02, ...)
}

\arguments{
  \item{independ}{ explanatory variable }
  \item{depend}{ dependent variable, typically a logical vector }
  \item{logi.mod}{ type of fitting, 1 = logistic; 2 = "gaussian" logistic }
  \item{type}{ type of representation, "dit" = dot plot; "hist" = histogram}
  \item{boxp}{ TRUE = with box plots, FALSE = without}
  \item{rug}{ TRUE = with rug plots, FALSE = without }
  \item{ylabel}{ y-axis label }
  \item{ylabel2}{ 2nd y-axis label }
  \item{xlabel}{ x-axix label }
  \item{mainlabel}{ overall title for plot }
  \item{las.h}{ orientation of axes labels (0 = vertical, 1 = horizontal }
  \item{counts}{ add counts above histogram bars }
  \item{cex.p}{ size of points in dotplots }
  \item{pch.dit}{ Either an integer specifying a symbol or a single character to be used as the default in plotting points. See \code{\link[graphics]{points}} for possible values and their interpretation. }
  \item{incre}{ increment, i.e., separation between neighbour points in the dotplot. }
  \item{\dots}{ additional options passed to logi.hist }
}

\value{
  A combined logistic regression plot
}
\references{
\href{http://esapubs.org/bulletin/backissues/086-1/bulletinjan2005.htm#et}{De la Cruz Rot, M. 2005. Improving the Presentation of Results of Logistic Regression with R. ESA Bulletin 86:41-48.} 
}
\author{ M. de la Cruz Rot }
\note{ This is a new version of function logi.hist.plot already available in the package \pkg{popbio}. In this new version, control of points in the dot plot is provided by the arguments \code{cex.p}, \code{pch.dit} and \code{incre}.  }
\examples{

data(aq.trans, package="popbio")

aq.trans$survived<-aq.trans$fate!="dead"

a<-subset(aq.trans, leaf<50 & stage!="recruit", c(leaf,survived))

logi.hist.plot2(a$leaf,  a$survived, 
type="hist", boxp=FALSE, counts=TRUE, int=10, 
ylabel="Survival probability", ylabel2="Number of plants", 
 xlab="Number of leaves" )

b<-glm(survived ~ leaf, binomial, data=a)
 summary(b)


}
\keyword{ ~survey  }
