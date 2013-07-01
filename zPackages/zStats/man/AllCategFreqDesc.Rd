\name{AllCategFreqDesc}
\alias{AllCategFreqDesc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Analyze and Transform Categorical variables
%%  ~~function to do ... ~~
}
\description{Replace rare categories to "Other"
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AllCategFreqDesc(ds = tr3[, head(sus(sp3$tb, class == "character")$.id, 5)], n = 9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
%%     ~~Describe \code{ds} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (ds = tr3[, head(sus(sp3$tb, class == "character")$.id, 
    5)], n = 9) 
{
    m = min(n, nrow(ds))
    res = df(matrix("=", m + 1, ncol(ds)))
    names(res) = na(ds)
    for (j in 1:ncol(ds)) {
        catt(j, na(ds)[j])
        xx = CategFreqDesc(ds[, j], 0, threshOther = 0)
        x = xx$tb[1:m, ]
        res[, j] = c(sf("\%7s :: \%5.1f", xx$nVal, xx$ext2), sf("\%7.1f \%-10s", 
            100 * x$p, substr(x$x, 1, 95)))
    }
    print(format(res, justify = "left"))
    invisible(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
