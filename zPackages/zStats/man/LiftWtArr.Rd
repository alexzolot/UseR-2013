\name{LiftWtArr}
\alias{LiftWtArr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lift and ROC plots and stats with weights
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
LiftWtArr(y, yhat, wt = 0 * y + 1, costFN = 10, nPlot = le(y), PlotXrel = F, clr = rep(c("red", "brown", "green", "blue", "violet", "black"), 4), pch = rep(1:25, 2), pmax = 100, ymax = 100, kCostPlot = 3, xlim = c(0, 1), ylim = c(0, 1), plotAll = F, wCairo = T, wPoints = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{yhat}{
%%     ~~Describe \code{yhat} here~~
}
  \item{wt}{
%%     ~~Describe \code{wt} here~~
}
  \item{costFN}{
%%     ~~Describe \code{costFN} here~~
}
  \item{nPlot}{
%%     ~~Describe \code{nPlot} here~~
}
  \item{PlotXrel}{
%%     ~~Describe \code{PlotXrel} here~~
}
  \item{clr}{
%%     ~~Describe \code{clr} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{pmax}{
%%     ~~Describe \code{pmax} here~~
}
  \item{ymax}{
%%     ~~Describe \code{ymax} here~~
}
  \item{kCostPlot}{
%%     ~~Describe \code{kCostPlot} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{plotAll}{
%%     ~~Describe \code{plotAll} here~~
}
  \item{wCairo}{
%%     ~~Describe \code{wCairo} here~~
}
  \item{wPoints}{
%%     ~~Describe \code{wPoints} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, yhat, wt = 0 * y + 1, costFN = 10, nPlot = le(y), 
    PlotXrel = F, clr = rep(c("red", "brown", "green", "blue", 
        "violet", "black"), 4), pch = rep(1:25, 2), pmax = 100, 
    ymax = 100, kCostPlot = 3, xlim = c(0, 1), ylim = c(0, 1), 
    plotAll = F, wCairo = T, wPoints = T, ...) 
{
    nxTot = length(y)
    nx = length(y)
    print(nx)
    nx1 = nx + 1
    y = nu(ch(y))
    ory = order(-y, na.last = NA)
    y.sort = y[ory]
    wty.s = wt[ory]
    Positive = wty.cum = c(0, cumsum(wty.s))
    wty.ncum = 1 * wty.cum/wty.cum[nx + 1]
    TP = y.cum = c(0, cumsum(y.sort * wty.s))
    y.ncum = 1 * y.cum/y.cum[nx + 1]
    FP = c(0, cumsum((1 - y.sort) * wty.s))
    p = 1:nx1/nx1
    LiftAreaY = cumsum(c(0, wty.s) * (y.ncum - wty.ncum)/100)[nx1]/wty.cum[nx]
    xp = c(1, floor((1:nPlot) * (nx + 1)/nPlot))
    if (wCairo) {
        libra(Cairo)
        CairoWin(width = 15, height = 10)
    }
    else windows()
    if (PlotXrel == "zzz") {
        xplot = wt.ncum[xp]
        print(xplot)
        print(y.ncum[xp])
        print(yhat.ncum[xp])
    }
    plot(wty.ncum[xp], y.ncum[xp], col = "blue", pch = 1, type = "l", 
        lty = 1, , xlim = xlim, ylim = ylim, xlab = "Positive, \%", 
        ylab = " normalized y.cum  (TP), \%", main = paste("Lift ", 
            deparse(substitute(yhat))), ...)
    abline(h = 0:5/5, v = 0:5/5, col = "grey89")
    j = 0
    LiftArea = NULL
    LiftAreaRel = NULL
    cv = NULL
    cost = data.frame(zz = 0 * y.cum)
    ROC = list()
    for (i in names(yhat)) {
        j = j + 1
        cat("\np2.", j, i, "\n")
        yhat[, i] = nu(ch(yhat[, i]))
        or = order(-yhat[, i], rnorm(nrow(yhat)), na.last = NA)
        y.sort = y[or]
        yhat.sort = yhat[or, i]
        wt.s = wt[or]
        Positive = wt.cum = c(0, cumsum(wt.s))
        wt.ncum = 1 * wt.cum/wt.cum[nx + 1]
        TP = y.s.cum = c(0, cumsum(y.sort * wt.s))
        y.s.ncum = 1 * y.s.cum/y.s.cum[nx + 1]
        FP = c(0, cumsum((1 - y.sort) * wty.s))
        ROC[[i]] = df(FPR = FP/FP[nx + 1], TPR = TP/TP[nx + 1])
        LiftArea[i] = cumsum(c(0, wt.s) * (y.s.ncum - wt.ncum)/100)[nx1]/wt.cum[nx]
        LiftAreaRel[i] = LiftArea[i]/LiftAreaY
        cv[i] = CVwt(y, yhat[, i], wt)
        lines(wty.ncum[xp], y.s.ncum[xp], lty = 3, col = clr[j], 
            pch = pch[j], type = "l", ...)
        if (wPoints) 
            lines(wty.ncum[sm <- sample(xp, 20)], y.s.ncum[sm], 
                lty = 3, col = clr[j], pch = pch[j], type = "p", 
                ...)
        cost[i] = wt.cum + sum(costFN * wt * y) - c(0, cumsum((1 + 
            costFN) * wt.s * y.sort))
    }
    abline(0, 1, col = "darkgreen", lty = 2)
    points(c(0, 100), c(0, 100), pch = 3, cex = 4, col = "red")
    or = order(-LiftArea)
    legend("bottomright", names(yhat)[or], lwd = 3, lty = 1, 
        col = clr[or], pch = pch[or], bty = "n")
    if (plotAll) {
        if (0) {
            cost$zz = NULL
            y.min = min(cost)
            x.min = which.min(unlist(cost))\%\%nrow(cost)
            windows()
            or = order(-apply(cost, 2, min))
            matplot(wt.cum/sum(wt), cost[, or], cex = 0.4, xlim = xlim, 
                xlab = "Positive, \%", ylab = "Cost", ylim = range(cost[xlim[1] <= 
                  wt.cum/sum(wt) & xlim[2] >= wt.cum/sum(wt), 
                  or]), col = clr[or], pch = pch[or], main = "Cost")
            legend("bottomright", names(cost)[or], lwd = 3, lty = 1, 
                col = clr[or], pch = pch[or], bty = "n")
        }
        windows()
        or = order(-LiftArea)
        plot(0:1, 0:1, type = "l", lty = 2, main = "ROC", xlim = xlim, 
            ylim = ylim, xlab = "FalsePositiveRate", ylab = "TruePositiveRate", 
            col = "grey75")
        for (i in or) lines(ROC[[i]], col = i, pch = pch[i], 
            lwd = 2, type = "l")
        if (wPoints) {
            for (i in or) lines(ROC[[i]][(sample(1:nx, 20)), 
                ], col = i, pch = pch[i], lwd = 2, type = "p")
            legend("bottomright", names(yhat)[or], lwd = 3, lty = 1, 
                col = or, pch = pch[or], bty = "n")
        }
        else {
            legend("bottomright", names(yhat)[or], lwd = 3, lty = 1, 
                col = or, bty = "n")
        }
    }
    return(z = list(lift = data.frame(LiftArea, LiftAreaRel, 
        cv, gini = LiftAreaRel, auc = 0.5 * (LiftAreaRel + 1)), 
        LiftAreaY = LiftAreaY, cost = cost, ROC = ROC))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
