\name{CategFreqDesc}
\alias{CategFreqDesc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Analyze and Transform 1 Categorical variable
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CategFreqDesc(x, n = 9, threshOther = 0.05, verb = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{threshOther}{
%%     ~~Describe \code{threshOther} here~~
}
  \item{verb}{
%%     ~~Describe \code{verb} here~~
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
function (x, n = 9, threshOther = 0.05, verb = F) 
{
    tb = df(table(x, useNA = "always"))
    tb$p = tb$Freq/sumn(tb$Freq)
    tb$`p,\%` = tb$p * 100
    tb = srt(tb, ~-Freq)
    if (threshOther == 0) 
        threshOther = tb$p[min(n, nrow(tb))]
    tb$isOther = with(tb, p < threshOther | 1:nrow(tb) > n)
    xwOther = ifelse(x \%in\% sus(tb, isOther)$x, "other", ch(x))
    xwOther[is.na(xwOther)] = "NA"
    if (n > 0) 
        print(head(srt(tb, ~-Freq), n))
    res = list(tb = tb, xwOther = xwOther, nVal = nrow(tb), ext2 = ext2(x))
    if (verb) 
        str(res)
    invisible(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
