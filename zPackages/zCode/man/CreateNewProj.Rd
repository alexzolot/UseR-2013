\name{CreateNewProj}
\alias{CreateNewProj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create New Project from Template
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateNewProj(newProj.name = "newProjTemplName", Templ.dir = "T:/work/UseR-2013/99_commonR/newProjTemplName", root = "T:/work/UseR-2013")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newProj.name}{
%%     ~~Describe \code{newProj.name} here~~
}
  \item{Templ.dir}{
%%     ~~Describe \code{Templ.dir} here~~
}
  \item{root}{
%%     ~~Describe \code{root} here~~
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
function (newProj.name = "newProjTemplName", Templ.dir = "T:/work/UseR-2013/99_commonR/newProjTemplName", 
    root = "T:/work/UseR-2013") 
{
    sw(sf("\%s/\%s", root, newProj.name))
    dir.create("in")
    dir.create("out")
    gw()
    for (f in dir(Templ.dir, patt = "newProjTemplName.*")) {
        catt(60, f, sf("\%s/\%s/\%s", root, newProj.name, sub("zz", 
            newProj.name, f)))
        catt("f=", f)
        if (grepl("newProjTemplName", f) & !grepl("doc.?$", f)) {
            fsub(fin = fp(Templ.dir, f), fout = sub("newProjTemplName", 
                newProj.name, f), fileShow = F, newProjTemplName = sf("\%s", 
                newProj.name), `00-00-00` = DT())
        }
        else file.copy(fp(Templ.dir, f), fp(gw(), sub("newProjTemplName", 
            newProj.name, f)))
    }
    expl()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
