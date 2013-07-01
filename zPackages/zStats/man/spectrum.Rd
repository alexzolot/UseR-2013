\name{spectrum}
\alias{spectrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Basic stats for all cols in data frame
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spectrum(ds, nValMax = 20, longestvalCut = 25, getext2P = T, id = "mobile", vAmt = "PRICE", toUpp = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ds}{
%%     ~~Describe \code{ds} here~~
}
  \item{nValMax}{
%%     ~~Describe \code{nValMax} here~~
}
  \item{longestvalCut}{
%%     ~~Describe \code{longestvalCut} here~~
}
  \item{getext2P}{
%%     ~~Describe \code{getext2P} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{vAmt}{
%%     ~~Describe \code{vAmt} here~~
}
  \item{toUpp}{
%%     ~~Describe \code{toUpp} here~~
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
function (ds, nValMax = 20, longestvalCut = 25, getext2P = T, 
    id = "mobile", vAmt = "PRICE", toUpp = F) 
{
    res = list()
    cat("nrow=", nrow(ds), "\n")
    res$tb = d = ldply(ds, function(x) {
        df(class = class(x)[1], Missing = sum(is.na(x)), notMissing = sum(!is.na(x)), 
            not0 = sum(x != 0, na.rm = T), nValues = le(unique(x)), 
            ext2 = ext2(x), ext2P = -1, shortCateg = (le(unique(x)) <= 
                nValMax), maxNchar = max(nchar(ch(x))), longestval = substr(ch(x[which.max(nchar(ch(x)))]), 
                1, longestvalCut), {
                if (class(x)[1] \%in\% c("character", "factor", 
                  "POSIXlt", "Date")) 
                  t(NA * summary(-777)[1:6])
                else if (class(x)[1] == "Date") 
                  (summary(x)[1:6])
                else t(summary(x)[1:6])
            })
    })
    if (toUpp) 
        names(ds) = toupper(names(ds))
    if (toupper(vAmt) \%in\% na(ds)) {
        for (j in 1:ncol(ds)) {
            x = ds[, j]
            tb = aggregate(ds[, vAmt], list(a = ch(x)), function(y) {
                amt = sum(1e-06 + y, na.rm = T)
                amt
            })[, 2]
            p = 1 * tb/sum(tb)
            res$tb$ext2P[j] = 1/sum(p^2)
        }
    }
    else res$tb$ext2P = NULL
    print(res$tb)
    for (j in na(ds[, d$shortCateg])) {
        cat("\n", j)
        print(table(ds[, j], useNA = "always"), exclude = NULL)
        res$shortCateg[[j]] = (table(ds[, j], useNA = "always"))
    }
    invisible(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
