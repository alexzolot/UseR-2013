\name{plot1y.grid}
\alias{plot1y.grid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot all variables ECDF / mosaic by binary y
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot1y.grid(da, xx = names(da), y = "y", Cairo = F, f = I, ma = "", orderCateg = T, manyPics = F, imgRoot = "../imgManyPics")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{da}{
%%     ~~Describe \code{da} here~~
}
  \item{xx}{
%%     ~~Describe \code{xx} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{Cairo}{
%%     ~~Describe \code{Cairo} here~~
}
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{ma}{
%%     ~~Describe \code{ma} here~~
}
  \item{orderCateg}{
%%     ~~Describe \code{orderCateg} here~~
}
  \item{manyPics}{
%%     ~~Describe \code{manyPics} here~~
}
  \item{imgRoot}{
%%     ~~Describe \code{imgRoot} here~~
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
function (da, xx = names(da), y = "y", Cairo = F, f = I, ma = "", 
    orderCateg = T, manyPics = F, imgRoot = "../imgManyPics") 
{
    nx = le(xx)
    n1 = floor(sqrt(nx))
    n2 = ceiling(nx/n1)
    libra(grid)
    libra(gridBase)
    if (manyPics) {
        n1 = n2 = 1
    }
    catt("plot1y.grid:", nx, n1, n2)
    .y = da[, y]
    ny = 2
    if (class(.y) \%in\% cn("character factor")) 
        try({
            da[, y] = .y = nuf(.y) - 1
            ny = le(unique(.y))
        }, s = F)
    res = list()
    (if (Cairo) 
        wwc
    else ww)(24, 12)
    par(mar = c(0, 0, 1, 0), oma = c(0, 0, 1, 0), no.readonly = TRUE)
    plot.new()
    mtext(ma, , 1)
    if (!manyPics) 
        pushViewport(viewport(layout = gl <- grid.layout(nrow = n1, 
            ncol = n2), height = unit(0.85, "npc")))
    for (j1 in 1:nx) {
        if (manyPics) 
            (if (Cairo) 
                wwc
            else ww)(24, 12)
        j = xx[j1]
        j1 = j1 - 1
        .i = j1\%/\%n2
        .j = j1 - .i * n2
        catt(j1, j, "    ", .i, .j, typeof(j), typeof(da[, j]))
        ks = list(statistic = 0, p.value = 0)
        try({
            ks = ks.test(sus(da, .y == 0)[, j], sus(da, .y == 
                1)[, j])
            catt(ks$statistic, ks$p.value)
            res[[j]][["ks"]] = ks$statistic[[1]]
        })
        if (!manyPics) 
            vp1 <- viewport(layout.pos.col = .j + 1, layout.pos.row = .i + 
                1)
        if (!manyPics) 
            pushViewport(vp1)
        if (!manyPics) 
            fg = gridFIG()
        else fg = c(0.1, 0.9, 0.1, 0.9)
        catt(.i, .j, "  |  ", fg)
        catt("fg = ", fg)
        par(new = TRUE, fig = fg, mar = c(2, 2, 2, 1) + 0.1)
        if (class(da[, j]) \%in\% cn("character factor")) {
            try({
                tb <- table(da[, c(j, y)])
                if (orderCateg) 
                  tb = tb[order(tb[, 1]/(0.001 + tb[, 2])), ]
                plot(as.table(tb), main = j, col = 2:3)
                ch = chisq.test(tb)
                str(ch)
                res[[j]][["chisq"]] = ch$p.value
                legend("bottomleft", eval(substitute(expression(P(chi^2) == 
                  .ch), list(.ch = ch$p.value))))
            })
        }
        else {
            try({
                with(x <- ord2(da[, c(y, j)], sf("\%s \%s", j, 
                  y)), {
                  plot((f(x[, j])), cumsumn1(x[, y]), col = 1, 
                    lwd = 2, ty = "l")
                  polygon(f(c(x[x[, y] == 0, j][1], x[x[, y] == 
                    0, j], rev(x[x[, y] == 1, j]), x[x[, y] == 
                    1, j][1])), c(0, cumsumn1(x[x[, y] == 0, 
                    y]), rev(cumsumn1(x[x[, y] == 1, y])), 0), 
                    col = rgb(0, 0, 0, 0.1))
                  for (iy in 1:ny - 1) lines(f(x[x[, y] == iy, 
                    j]), cumsumn1(x[x[, y] == iy, y]), col = 2 + 
                    iy, lwd = 3)
                  mtext(j, , 1, cex = 0.9, font = 2)
                })
                if (!manyPics) {
                  pushViewport(viewport(0.78, 0.55, 0.2, 0.2))
                  par(new = T, fig = gridFIG(), mar = c(0, 0, 
                    0, 0))
                }
                else par(new = T, fig = c(0.8, 0.9, 0.2, 0.4), 
                  mar = c(0, 0, 3, 0))
                pie(c(ks$statistic, 1 - ks$statistic), labels = "", 
                  col = c("grey30", "white"))
                mtext(sf("ks= \%4.3f ", ks$statistic), 1)
                if (!manyPics) 
                  popViewport()
            })
        }
        if (!manyPics) 
            popViewport()
        if (manyPics) 
            HHp(j, dirr = imgRoot, fNameWithCapt = T)
    }
    popViewport()
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
