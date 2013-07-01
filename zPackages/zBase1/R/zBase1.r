# svn: https://subversion.assembla.com/svn/azsvn/
# https://code/svn/medio/analytics/trunk/src/main/R/zBase.r  ticket: #MED-158
# https://code/svn/medio/analytics/m3/datascience/iusv2/trunk/src/main/R/zBase.r
# License  : GPL-2  

require(zBase0)



#' misc  aliases and functions.
#'
#' \tabular{ll}{
#' Package: \tab zBase\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2012-11-06\cr
#' License: \tab GPL-2\cr
#' LazyLoad: \tab yes\cr
#' }
#' @name zBase - package
#' @aliases aliases
#' @docType package
#' @title Miscellaneous aliases and functions
#' @author Alex Zolotovitski \email{azolotovitski@medio.com}
#' @references
#' \url{http://roxygen.org/roxygen.pdf}
#' @keywords package
#' @seealso \code{\link{aliases}}
#' @examples
#' pas(letters %+% LETTERS) 

# http://roxygen.org/roxygen.pdf
if(0){
	libra(plyr)
	libra(roxygen2)
	libra(RUnit)
	checkEquals("aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ"
				, pas(letters %+% LETTERS,,'') )
	roxygen()
	roxygenise('m:/80_RPack/zBase', 'm:/80_RPack/zBase')
	gw()
	sw('out')
	sw('m:/80_RPack')
	sw('m:/80_RPack/zBase')
	gzfile(file, "r")
	gzfile= function(description = "", open = "")unzip(description)
	zip
}


#File ~/zFunctions.R
#   30 '%+%' = function(x,y) paste(x,y,sep="")
#  221 '%<=%' = function(x,calcx){
#  211 '%=%' = function(x,calcx){
#  762            upper.panel = function(x,y){ a<- seq(0,2*pi,length=100);
#   72      inte= function(N, n) if(n > N/2) 1:N else c(1:n, N-(n:1)+1)
#  777 all0= function(x){all( abs(x) < 1e-8 )}
#  687 analQuantiles= function(y,wt){
#  208 calcc = function(doit,dat='zz'){if(calcs){catt(calcs,dat, dat %in% ls());doit} };       #    !exists(dat)     'SpeTestInd' %in% ls()
#   27 catf  = function(...) cat(sprintf(...))
#   26 catt  = function(...) {cat(...); cat('\n'); flush.console()}
#   93 cn= function(cnn) unlist(strsplit(cnn,'[ ,\n\t\\+]+'))  # col.names= cn(cnn)
#  722 CorrelCircle= function(pc){ #  Correl Circle === # pc=prcomp(atn[sel, xx])
#  737 CorrelCircle3= function(pc){ #  Correl Circle === # pc=prcomp(atn[sel, xx])
#  394 CV = function(y, yhat){   #======================================= CV ====
#  398 CVwt = function(y, yhat, wt){   #======================================= CV ====
#  255 displayInTable <- function(tclarray, title="", height=-1, width=-1, nrow=-1, ncol=-1, xSc=F) {
#  274 dsInTable <- function(ds, title="", height=-1, width=-1, nrow=-1, ncol=-1, digits=2, xSc=F) {
#  149 fDate= function(Date) format(Date,'%m-%d')  # to print on axis
#  145 fid= function(ds,cols)apply(ds[,cols],1,pas) #       id= fid(dn,cn('MediumId TrafficType'))
#  779 fregTAG= function(xx) as.formula ("TAG~" %+% paste(xx, collapse="+"))
#   32 fsize = function(f)file.info(f)$ size /1e6
#   97 gcc= function()base ::gc(reset=T)
#   71 heta = function(x, nr=8, nc=4){ # head + tail
#   80 hists = function(ds= ds.sim){
#  157 hLink= function(URL)gsub('(.*)', '<a href="http://\\1">\\1</a>', URL)
#  264 if(xSc) xscr <-tkscrollbar(tt,orient="horizontal", command= function(...)tkxview(table1,...))
#  296 if(xSc) xscr <-tkscrollbar(tt,orient="horizontal", command= function(...)tkxview(table1,...))
#  262 if(xSc) xscrollcommand= function(...) tkset(xscr,...),
#  294 if(xSc) xscrollcommand= function(...) tkset(xscr,...),
#   96 isNum= function(x)typeof(x)!="character"
#  148 LastDayOfWeek= function(week) as.Date('2010-01-02')  + 7 * week
#  160 libra= function(lib){li=deparse(substitute(lib));
#  165 libras = function(reqpk=c("locfit","tkrplot", "xtable")){
#  405 Lift = function(y,yhat,clr='red',...){
#  441 LiftWt = function(y,yhat,wt,nPlot=100, clr='red',...){
#  526 LiftWtArr = function(y,yhat,wt=one(y),nPlot=100,
#   45 lo= function(file='.RData'){load(file=file);lss()}
#   29 logg  = function(...) cat(format(Sys.time(), "%Y-%b-%d %H:%M:%S "),..., '\n', file = "log.txt", append = T)
#   34 lss0= function()print(sort(sapply(ls(envir = .GlobalEnv), function(x) object.size(get(x))/1e6)),signif=2, scipen=99)
#   50 me= function() {print(paste('R is using', mem<- memory.size(), 'MB out of limit', memory.limit(), 'MB'));mem}
#  706 MergeByName= function(x,y){
#  121 Model= function(glm){or= order(pValue <- summary(glm)$coef[, "Pr(>|t|)"])
#  180 newwin = function(nrows= 2, ncols= 2, tytle= 'SpendHabit', doit, pos=0, mar0= c(4,4,4,2)+.2,...){
#   94 nmsv= sNames= function(ds, sep=' ')paste(names(ds),sep=' ',collapse=sep) #inverse to cn()
#   95 norm= function(x)(x-min(x, na.rm =T))/diff(range(x, na.rm =T))
#   42 nut= function(...)as.numeric(tclvalue(...))
#   92 one= function(x)rep(1,le(x))
#  209 plott = function(doit){if(plots)doit };    plots = T
#  380 prinb= function(){ options(printcmd ="lpr -Psdoprt05"); dev.print()}
#  379 princ= function(){ options(printcmd ="lpr -Psdoclr04"); dev.print()}
#  672 PrintModel= function(coef,lrmodel,x){
#   31 renamee= function(ds, oldname, newname){names(ds)[names(ds)==oldname]=newname; names(ds)}
#   36 rmall= function()rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv) # rmall()
#  383 ROC= function(y,yhat,wt,np=100,...){ #lift curve
#  104 rtsv= function(s, header=F,...){ds= read.table(s, header=header,   stringsAsFactors =F, sep='\t', comment.char = "",...)
#   98 rwith.na= function(ds)apply(ds,1,function(x)any(is.na(x)))
#   28 sf  = function(...) sprintf(...)
#  232 spectr= function(ds=ds, nValMax=50){  # -------------------------------------------------------
#  109 SQL2vars= Scope2vars= function(s, sep=' ') 'names()= cn(\'' %+% paste(gsub('\\w+\\.',''
#  115 SQL2vars= Scope2vars= function(s, sep=' ') 'names()= cn(\'' %+% paste(gsub('\\w+\\.',''
#   43 st= function(...)system.time(...)[[3]]
#   99 tab= function(x)table(x, useNA = "ifany")
#  195 toTe = function(header='', doit, totcl= T,...){ # header=''; doit={}    toTe('AAA',{})
#  151 week= function(Date)as.numeric(Date - as.Date('2010-01-03'),"days")  %/% 7  +1
#  128 weighted.var <- function(x, w, na.rm = FALSE) {
#  648 yScore= function(coef,x){    #coef=lmr9$coef x=ats  coef=c;
#  657 yScoreSc= function(coef, scales, x){    #coef=lmr9$coef x=ats  coef=c;
#  265 yscr <- tkscrollbar(tt,command= function(...)tkyview(table1,...))
#  297 yscr <- tkscrollbar(tt,command= function(...)tkyview(table1,...))
#  263 yscrollcommand= function(...) tkset(yscr,...))
#  295 yscrollcommand= function(...) tkset(yscr,...))
#  102 zlog10= function(x, k=.001) log10(k *diff(quantile(x, c(.01,.99),na.rm = T))+ x)# log10(k *IQR(x, na.rm = T)+ x)
#  103 zlogit= function(x, k=.001){e=diff(quantile(x, c(.01,.99),na.rm = T));  log((k *e+ x)/(1+ k *e- x))}
#  310 zqqplot= function(x,y,colx, n=1e9, bLog=F,xlab = deparse(substitute(x)),
#  325 zqqplotWt= function(x,y,colx, wx, n=1e9, bLog=F,xlab = deparse(substitute(x)),

# aliases ===

#sw('out'); gw()

d.o= dev.off


ma= as.matrix
nuc= function(...) nu(ch(...))  # fa -> nu
atl= function(...) attach(list(...))  #  attach(list(...))  #  xxx env ? to assign values to pars of func
classes= function(ds) laply(ds, class)




invi= invisible

if(0){
	`na < -` <- 1  #names
	assign("na<-", names)
	`na<-` <- 1
	str(`na<-` )
	ls()
}

tra= transform

# one liners ===

logg  = function(...) cat(format(Sys.time(), "%Y-%b-%d %H:%M:%S "),..., '\n', file = "log.txt", append = T)

renamee= function(ds, oldname, newname){names(ds)[names(ds)==oldname]=newname; names(ds)}
fsize = function(f)file.info(f)$ size / 2^20
#lss= function()print(sort(sapply(ls(envir = .GlobalEnv), function(x) object.size(get(x))/1e6)),signif=2, scipen=99)
lss0= function()print(sort(sapply(ls(envir = .GlobalEnv), function(x) object.size(get(x))/1e6)),signif=2, scipen=99)
#lss()
zeval= evall= function(s)eval(parse(text= s), envir=.GlobalEnv)

execf= function(fmt,...)shell(sf(fmt,...), wait=T, intern = T)

merge3= function(x,y,z, ...) {u= merge(x,y, ...); u= merge(u,z, ...); u}


rou= function(x, dig=3) round(x, dig)


nut= function(...)as.numeric(tclvalue(...))


#libra(Cairo);  
wwc= function(width = 12, height = 11, pointsize = 12, res=96, ...) CairoWin(width = width, height = height, pointsize = pointsize,...)
wws= function(width = 12, height = 11, ma='', doit,  pointsize = 12, res=96, dirr='../img', ...){
	.iPicS<<- max(nu(gsub('^Pic_(\\d+)[^\\d]*\\.svg$','\\1', dir(dirr, patt='.svg$')))) + 1
			if(is.na(.iPicS) || .iPicS < 1) .iPicS<<- 1
			fn<- sf("%s/Pic_%s.svg", dirr, .iPicS)
			svg(width = width, height = height, pointsize = pointsize, filename=fn, ...)
			on.exit(d.o())
			catf('\nSVG to %s. %s\n', fn, ma)
			e= try({doit})
} 

wc= function(...) '77 dummy wc' # dummy

rt= function(f, nrows=1e6,...)read.table('../' %+% f,  header=F, stringsAsFactors =F, sep='\t', nrows = nrows,...) # debug !!! xxx
rtd= function(f, nrows=1e6,...)read.delim('../' %+% f,  header=F, stringsAsFactors =F, sep='\t', nrows = nrows,...) # debug !!! xxx
rtsv= function(s, header=F,...){ds= read.table(s, header=header,   stringsAsFactors =F, sep='\t', comment.char = "",...)
	str(ds)
	ds
}

ttt<- textToTable <- function(text, ...)
{
	dfr <- read.table(tc <- textConnection(text), ...)
	close(tc)
	dfr
}

plotl= function(j) if(j==1) plot else lines  # Example: for(j in 1:4) plotl(j) (.01:5, (.01:5)^  (3-j), col=j, ty='o', ma='Example')


#== tail and short elements of a list to print ==
last= function(x, first=le(x)) x[first:le(x)]	
# last(x<- list(a=1, b=1:30, c='aa', d=1:5, e='eee', f=1:2 %o% 5:7), 4)
# tail(x<- list(a=1, b=1:30, c='aa', d=1:5, e='eee', f=1:2 %o% 5:7), 4)
shorts= function(x, maxle=9){les= laply(x, NROW);  list(a=ldply(x[les==1]), b=x[1<les & les < maxle])}
# shorts(x<- list(a=1, b=1:30, c='aa', d=1:5, e='eee', f=1:2 %o% 5:7))


if(0){
	libra(testthat)
	eee= expect_equal
	eet= expect_true	
}else{
	eee= expect_equal= cat
	eet= expect_true= cat
}

SS= function(x, y=0) sum(unlist((suss(,'id', x)- y)^2), na.rm=T)
if(0){ #  ===
	SS(LOut0,0)  # [1] 463228 
	SS(LOut0)    # [1] 463228 
}  # -----



#pr= print
prrr= function(x) print(format(df(x), justify = "left"))
fll= function(x)format(x, justify = "left" )



ordd= function(ds,col) ds[order(-ds[,col]),] ## desc many cols
ord= function(ds,col){if(substr(col,1,1)=='-') ds[order(-ds[,substr(col,2,99)]),] else ds[order(ds[,col]),]} ## 1 col only
ord= function(ds,col){if(substr(col,1,1)=='-') ds[rev(order(ds[,substr(col,2,99)])),] else ds[order(ds[,col]),]} ## 1 col only
ord2= function(ds,col= '-cAmt -Amt'){cols= strsplit(col, ' ')[[1]]
	if(substr(cols[1],1,1)=='-') v1= - ds[,substr(cols[1],2,99)] else v1= ds[,cols[1]];
	if(substr(cols[2],1,1)=='-') v2= - ds[,substr(cols[2],2,99)] else v2= ds[,cols[2]];
	ds[order(v1,v2),]
}

ab= function(a=0,b=1,col='gray60',lty=2,lwd=2,...)abline(a, b, col=col, lty=2,...)


one= function(x)rep(1, le(x))

norm= function(x)(x-min(x, na.rm =T))/diff(range(x, na.rm =T))
norm0= function(x) x/max(x, na.rm =T)
isNum= function(x)typeof(x)!="character"
gcc= function()base ::gc(T, reset=T) # garb collect
rows.with.na= function(ds)apply(ds, 1, function(x)any(is.na(x)))
tab= function(x, useNA = "ifany")table(x, useNA = useNA)
tab.df= function(...)df(tab(...))
fid= function(ds, cols=names(ds))if(le(cols)<2)return(ds[,cols])else return(apply(ds[,cols],1,pas)) # id= fid(dn,cn('MediumId TrafficType'))
summar= function(.data, .variables, .fun = summarize, ...) ddply(.data, .variables, summarize,...)
mt= function(s,...)mtext(s, 2, cex=.7, line=-1,...)
heec= function(ds, ...) {hee(srt(ds, ~ -Clicks), ...)}
heee= dsh= function(ds, h=5){print(dim(ds)); pr(str(ds)); pr(head(ds,h)); catf('# %s[, cn("%s")]\n',deparse(substitute(ds)),nmsv(ds))}

#nonUnique= function(x) {tb=tab(x); df(cnt=tb[tb>1])} # nonUnique(c(1,2,3,2,4,5,4))
nonUnique= function(x) {tb=df(tab(x, useNA = "always")); df(cnt=tb[tb[,2]>1, ])} # nonUnique(c(1,2,3,2,4,5,4))
# 2 4 
# 2 2 


lg1= function(x) log10(1+x)
logit= function(p)log(p/(1-p))
zlog10= function(x, k=.001) log10(k *diff(quantile(x, c(.01,.99),na.rm = T))+ x)# log10(k *IQR(x, na.rm = T)+ x)
zlogit= function(x, k=.001){e=diff(quantile(x, c(.01,.99),na.rm = T));  log((k *e+ x)/(1+ k *e- x))}

sumn= function(x)sum(nu(x), na.rm=T)
maxn= function(x)max(x, na.rm=T)
mn= function(...)mean(..., na.rm=T)

ad= as.Date
ct= as.POSIXct
fromUNIXt= function(x)structure(x, class= c("POSIXt", "POSIXct"))


# print aggregate memory usage statistics
me= function() {print(paste('R is using', mem<- memory.size(), 'MB out of limit', memory.limit(), 'MB'));mem}



rmmm= function(maxObjSize=.5, ...) invisible(mdply(ls(envir =.GlobalEnv,... ),function(a){aa=get(a); b=df()
						if(object.size(aa)/1e6 > maxObjSize){catf('rm  %-20s %-30s\n', a, sNames(aa)); do.call(rm,list(a),envir =.GlobalEnv)}
						if( class(aa)[1] %in% c('data.frame','matrix')){catf('rm  %-20s %5s %3s %-30s\n', a, nrow(aa), ncol(aa),substr(sNames(aa),1,4999))
							do.call(rm, list(a), envir =.GlobalEnv)
						}}))



# head + tail
heta = function(x, nr=8, nc=4){ # head + tail
	inte= function(N, n) if(n > N/2) 1:N else c(1:n, N-(n:1)+1)
	print(dim(x))
	#print(x[c(1:n, nrow(x)-(n:1)+1), c(1:n, ncol(x)-(n:1)+1)])
	print(x[inte(nrow(x), nr), inte(ncol(x), nc)])
	flush.console()
} #--heta
#heta(df)


wc= function(f='out/woPV/0222/*.tsv'){sh<- shell(paste('c:/cygwin/bin/wc.exe -l ', f, '*', sep=''), wait=T ,  intern=T)}
#? wc= function(f='out/woPV/0222/*.tsv'){sh<- shell(sf('c:/cygwin/bin/wc.exe -l %s*', f), wait=T ,  intern=T)}




#libra(R2HTML)

#== functions for HTML output ==
# HHT= HTML
HHInit= function(append=FALSE){
	if(append){assign(".HTML.file", fp(gw(), 'index.html'), env = .GlobalEnv)
		.iPic<<- max(nu(gsub('^Pic_(\\d+)[^\\d]*\\.png$','\\1', dir(patt='.png$'))))+1
		
	}else {
	HTMLInitFile(outdir = gw(), filename='index'
	#HHInit= function(){HTMLInitFile(outdir = ".", filename='index'
			, CSSFile="http://www.stat.ucl.ac.be/R2HTML/Pastel.css",useGrid=T,useLaTeX=F) # http://www.stat.ucl.ac.be/R2HTML/
	#HHd(Sys.Date())
	#catt(format(Sys.Date()," %Y-%b-%d"),  '<br/><br/>', file=fp(gw(), .HTML.file), append=T)
	#assign("HTMLenv", new.env(parent = .GlobalEnv), envir = .GlobalEnv)
	#rm(HTMLenv)
	.iPic<<- 0
	}
	.main=''
	catt(format(Sys.Date()," %Y-%b-%d"),  '<br/><br/>', file= .HTML.file, append=T)

	.HTML.file
}


HHt= function(...){cat('<br/>', file= .HTML.file, append=T); HTML.title(...)}
HHpr= HTMLp= function(x,...) HTML(sf('<pre> %s </pre>',x), ...)
#HHp= function(capt='', Width = 1200, Height = 800, ...){
HHp.bak= function(capt='', Width = par("din")[1] , Height =  par("din")[2], ...){
	op= options(); options(error=dummy)
	try({HTMLplot(file=  .HTML.file
						, GraphFileName = .GraphFileName<<- sf('Pic_%s', .iPic<<- .iPic+1)
						, Caption=sf('Plot %s. %s', .iPic, capt), Width = Width, Height =Height, Align = "left",...) #, Width = 800, Height = 600, GraphPointSize=1)
				dev.off()
			}, s=T)
	options(op)
	catf('%s. %s', .GraphFileName, capt)
	invisible(sf('%s. %s', .GraphFileName, capt))
}


HHpm= function(Capt='', side=0,...){if(side %in% 1:4)mtext(Capt, side, .3,...); HHp(Capt, dirr='../img',...)} 

HHa= function(URL)gsub('(.*)', '<a href="http://\\1">\\1</a>', URL)


HHd= function(x,...){ # typeof(ds) typeof(dtb)
	if(class(x)=="list") for(xx in na(x)) {catt(444,xx); zHTML(x[[xx]],...);return()}
	if(class(x)=="integer" && le(x)>1) x= as.data.frame(x)
	HTML(x, align="left", captionalign="top", big.mark=',', digits=4,...)  
}
HHf= function(...)HHd(sf(...))
if(0){
	.HTML.file; browseURL(.HTML.file); HTMLStop()
	
	dir.create(file.path(gw(),"HTML/img"), rec=T)
	fout= HTMLInitFile(outdir = "HTML", filename='index', CSSFile="http://www.stat.ucl.ac.be/R2HTML/Pastel.css",useGrid=T,useLaTeX=F) # http://www.stat.ucl.ac.be/R2HTML/

	
	HTMLStart(file.path(gw(),"HTML"),echo=T)
	as.title("Manipulation vectors")
	1:10
	sum(1:10)
	c(1:10,rep(3,4))
	p= plot(sin, -pi, 2*pi, main="Sinus")
	HTMLplot(file=.HTML.file, GraphDirectory=file.path(gw(),"HTML"), Caption="Look at this curve!")
	HTMLStop()
	expl()	
}

showInOpera= function(x= .HTML.file) {
    if(class(x)== "data.frame")x= x$H2 
    cmd= sf('C:\\Progra~2\\Opera\\opera -newtab %s', paste(x, sep=' ', collapse=' '))
	sh= shell(cmd, wait=F, intern = F) #      sh         
}
# showInOpera(cn('zolot.us msn.com'))
# showInOpera()

# HHs= function()(expl(file.path(gw(),.HTML.file)))
HHs= function()expl(.HTML.file)




plotm= function(main=NULL,...){.main<<- main; graphics:::plot(main=main, ...)}  # for HHp()
#plot=graphics:::plot

		

toXL= ToXL= function(ds, dsn= deparse(substitute(ds)),wbName='OutR_1.xlsx', toCSV=TRUE, startRow1 = 4, startCol1 =2, toOpen=F, ...){ # see http://www.mirai-solutions.com/site/index.cfm?id_art=69890&actMenuItemID=31456&vsprache/EN/Mirai_Solutions___XLConnect_.cfm
    #ds=cast(agg, Z~ F,  fun.aggregate=sum, value ='pBClicks') ; dsn='Tab1.1Ys.pBClicks'
	#write.csv(ds, file=dsn %+% '.csv')
	write.csv(ds, file= sf('%s.csv', dsn))
	gcc()
    libra(XLConnect)
    wb <- loadWorkbook(wbName, create = T)
    createSheet(wb, name = dsn)
	
	# TODO: createName(wb, name =dsn, formula = "mtcars!$A$1")
	
    writeWorksheet(wb, ds, sheet = dsn, startRow = startRow1+3, startCol = startCol1)
    writeWorksheet(wb, dsn, sheet = dsn, startRow = startRow1, startCol = startCol1, header=FALSE)
    if(dsn != deparse(substitute(ds))) writeWorksheet(wb, deparse(substitute(ds)), sheet = dsn, startRow = startRow1+1, startCol = startCol1, header=FALSE)
    saveWorkbook(wb)
    xlcFreeMemory()
    catt(':: ToXL: ', dsn,'saved to: %s', fp(gw(), wbName))
    if(toOpen)shell(sf('explorer /root,%s\\%s', gsub('/','\\\\', gw()),wbName))
    invisible()
}

#tocsv= function(ds, dsn= deparse(substitute(ds)) %+% '.csv',...){write.csv(ds, file=dsn, quote =F, row.names = F,...); 
expls= function(x=gw())shell(sf('start %s',gsub('/','\\\\', x)))


# 1 min exercise ====
Timer= function(dt=32, n=2) for(i in 1:n){Sys.sleep(dt); cat('\a'); expl('C:\\Users\\alexzol.REDMOND.000\\Music\\bell17.mp3') }


strReverse <- function(x) sapply(lapply(strsplit(x, ""), rev), paste, collapse="")
#strReverse(c('av','ext'))

Model= function(glm){or= order(pValue <- summary(glm)$coef[, "Pr(>|t|)"])
	catt('model=', gsub('Intercept','1',gsub(':',':',gsub('\\+ -','-',paste(names(glm$coef)[or], sep='*', collapse=' + ')))))
	catt('y= ', gsub('Intercept','1',gsub(':','*',gsub('\\+ -','-',paste(glm2$coef[or],names(glm2$coef)[or], sep='*', collapse=' + ')))))
	return(data.frame(nm= names(pValue),r= rank(pValue))[or,])
}


weighted.var <- function(x, w, na.rm = FALSE) { #weighted.mean
	if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
	sum.w <- sum(w)
	sum.w2 <- sum(w^2)
	mean.w <- sum(x * w) / sum(w)
	(sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm = na.rm)
}


#LastDayOfWeek= function(week) as.Date('2010-01-02')  + 7 * week
LastDayOfWeek= function(week) as.Date('2011-01-01')  + 7 * week
fDate= function(Date) format(Date,'%m-%d')  # to print on axis

#week= function(Date)as.numeric(Date - as.Date('2010-01-03'),"days")  %/% 7  +1
week= function(Date)as.numeric(Date - as.Date('2011-01-01'),"days")  %/% 7  +1
#     week(as.Date('2010-08-07'))
#    
#     LastWeek= week(Sys.Date())- 1

ct= as.POSIXct


newwin=  function(nrows= 2, ncols= 2, tytle= '', doit, pos=0, mar0= c(4,4,4,2)+.2, Cairo=1, pngFile='', ...){
	#windows(width=16, height=10, xpos=30+ 20* pos, ypos=30+ 20* pos, title= tytle)
	if(pngFile==''){
		if(Cairo) {libra(Cairo); CairoWin(...)}
		else windows(xpos=30+ 20* pos, ypos=30+ 20* pos, title= tytle,...)
	}else{
		if(Cairo) {libra(Cairo); CairoPNG(pngFile, 800, 800, ...)}
		else png(pngFile, xpos=30+ 20* pos, ypos=30+ 20* pos, title= tytle,...)
	}
	
	op <- par(mfrow=c(nrows, ncols), mar = mar0 + .1)
	e= try({doit})
	if (inherits(e, "try-error")) {catt('Newwin Err.666'); return()} # stop("Newwin Error !")
	if (le(e)>0)if (nu(e[[1]])== -99) {catt('Newwin Err.999'); dev.off(); return()}
	
	par(op);
	#try(mtext(sf("%s, %s",tytle, Sys.time()), side=3, line=3, font=2) )   # , size=1.5
	try(mtext(tytle, , side=3, line=3, font=2))   # , size=1.5
	try(mtext( Sys.time(), side=3, line=3, cex=.8, adj=1, font=3))   # , size=1.5
	#dev.off()
} # newwin(,3,'zzz',{plot(1:3); plot(1:9);plot(zz) },wi=15,he=10)



# Alexandre Zolotovitski   
# Sep 8
{ #block to run #8
#==== Lift Curve Wt Array  =================================================
	LiftWtArr.Old = function(y,yhat,wt=one(y),nPlot=100,
			clr=c('red','brown','green','blue','violet','black'),
			pmax=100,ymax=100,
			...){
		nxTot=length(y)
		#nx=length(y.sort); print(nx); nx1=nx+1
		
		nx=length(y); print(nx); nx1=nx+1
		
		
		ory=order(-y, na.last = NA); y.sort=y[ory] ;
		wty.s=wt[ory];
		wty.cum=c(0,cumsum(wty.s));            wty.ncum=100 * wty.cum/wty.cum[nx+1]
		y.cum=c(0,cumsum(y.sort*wty.s));         y.ncum=100 * y.cum/y.cum[nx+1]
		
		
		#subset to plot
		p=1:nx1/nx1
		#LiftArea=cumsum(c(0,wty.s)*abs(y.ncum-wty.ncum)/100)[nx+1]/(nx) # wromg? no wght
		LiftAreaY=cumsum(c(0,wty.s)*(y.ncum-wty.ncum)/100)[nx1]/wty.cum[nx]
		
		# xp=round((1:nPlot)*(nx+1)/nPlot)
		#xp=floor((1:nPlot)*(nx+1)/nPlot)
		xp=c(1,floor((1:nPlot)*(nx+1)/nPlot))
		
		#cat(cbind(wty.ncum,y.ncum ))
#print(cbind(xp, y.sort, wty.ncum, y.ncum ))
		
		#     print(xp)
		if(F){xplot= wt.ncum[xp]
			print(xplot)
			print(y.ncum[xp])
			print( yhat.ncum [xp])
		}
		x11()
		plot(wty.ncum[xp], y.ncum[xp],col='blue',pch=1,type="l", lty=1,
				xlab=paste('% rank ', deparse(substitute(yhat))),
				ylab=paste('% cum', deparse(substitute(y))),
				xlim=c(0,pmax), ylim=c(0,ymax),
				main=paste("Lift ", deparse(substitute(yhat))), #' [ CV=',cv,']' ),
				...)
		
		j=0
		LiftArea=NULL ;LiftAreaRel=NULL;   cv=NULL;
		for (i in names(yhat)){j=j+1;cat("\np2.",j,i);
			or=order(-yhat[,i], -y, na.last = NA); #print(or);
			y.sort=y[or] ; #print(cbind( y.sort,yhat.sort))
			yhat.sort=yhat[or,i];
			wt.s=wt[or];
			
			yhat.cum=c(0,cumsum(y.sort*wt.s));           yhat.ncum=100 * yhat.cum/yhat.cum[nx+1]
			wt.cum=c(0,cumsum(wt.s));                    wt.ncum=100 * wt.cum/wt.cum[nx+1]
			LiftArea[i]=cumsum(c(0,wt.s)*(yhat.ncum-wt.ncum)/100)[nx1]/wt.cum[nx]
			LiftAreaRel[i]=LiftArea[i]/LiftAreaY
			# cat("p3.",j,i,  LiftArea[i],  LiftAreaRel[i])
			cv[i]=CVwt(y,yhat[,i],wt); # =signif(CVwt(y,yhat[,i],wt),4);
			
			lines(wty.ncum[xp], yhat.ncum[xp], lty=3,
					col=clr[j], pch=18+j ,type="o"
			)
		}
		abline(0,1,col='darkgreen',lty=2)
		points(c(0,100), c(0,100), pch = 3, cex = 4, col = "red")
		
		# mtext(paste('2nxTot=',nxTot,' nx.reg=',nx, 'LiftArea=',signif(LiftArea,3)),
		#     side=3,cex=.8)
		legend(.6*pmax, 0, yjust=0, names(yhat), lwd=3, lty=1, col=clr )
		
		return(z=list(lift=data.frame(LiftArea,LiftAreaRel,cv),
						LiftAreaY=LiftAreaY))
		
	}
	
	if(F){  #test
		n=20
		p=1:n
		x=p
		y=2*x +30*rnorm(n); y[y<0]= 0;
		nPlot=10
		clr="red"
		
		set.seed(1)
		wt=.1+runif(n)
		yhat=2*x
		np=10 # 100
		y2h=1.5*x+30*rnorm(n)
		
		plot(y~x)
		lines(yhat ~ x)
		lines(y2h ~ x, col="red")
		lines(lm$fitted.values ~ x, col="green")
		lm=lm(y~x)
		
		yyhat=data.frame(yhat,y2h, lm$fitted.values)
		
		np=nPlot
		print(CVwt(y, yhat, wt))
		print(CVwt(y, y2h, wt))
		LW=LiftWtArr(y,yyhat,wt,nPlot=np, main="zz")
		print(LW)
		print(LW$lift$LiftArea)
		str(LW)
		
		#if(F){
		LiftWt(y,yhat=yhat,wt,nPlot=np, clr='red')
		x11();plot.new()
		LiftWt(y,yhat=y2h,wt,nPlot=np, clr='blue',add=T)
		LiftWt(y,yhat=y2h,wt,nPlot=np, clr='blue')
	}
}




# ===
# load("~zRsave")
# ls()



fe= file.edit


# The End. -----------------------------------------------------------------



nope= function(){ #=========================================================
	
	Neff <-  function(x, w, na.rm = FALSE) {
		if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
		sum.w <- sum(w)
		sum.w2 <- sum(w^2)
		mean.x <- sum(x * w) / sum.w
		mean.v= (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.x)^2, na.rm = na.rm)
		return(mean.x*(1- mean.x) / mean.v)
	}
	
	fid= function(ds,cols){if(le(cols)<2)return(ds[,cols]) else return(apply(ds[,cols],1,pas))} #       id= fid(dn,cn('MediumId TrafficType'))
	
	
	
	toTe = function(header='', doit, totcl= T,...){ # header=''; doit={}    toTe('AAA',{})
		txt <- capture.output( { catt(header,'\n -----------------------------------' )
					doit
				})
		#if(totcl){
		tt2 <- tktoplevel()
		tkwm.title(tt2,header)
		tkgrid(te<<- tktext(tt2, font="courier 10" ),sticky='news')
		tkdelete(te,'1.0','end'); tkinsert(te,"end",paste(txt,collapse="\n"))
		#} else print(txt)
	}
	
	#calcc = function(doit,dat='zz'){if(dat=='zz' & calcs |!(dat %in% ls()) )doit };    calcs = F    #    !exists(dat)
	calcc = function(doit, dat='zz'){if(calcs){catt(calcs,dat, dat %in% ls());doit} };       #    !exists(dat)     'SpeTestInd' %in% ls()
	plott = function(doit){if(plots)doit };    plots = T
	
	'%=%' = function(x,calcx){
		sx= deparse(substitute(x)) ; sfile=  sx %+% '.RData'; # catt(333,sx)#, str(calcx),recalc) #, sx %+% ' <<- '  %+% deparse(substitute(calcx))) #   getwd() %+% '/' %+% sx %+% '.RData'
		if( recalc > 1 & file.exists(sfile)) try(if(!file.remove(sfile) )catt('Err: cant remove file!!'))   #    |
		if((!exists(sx)| recalc == 1) & file.exists(sfile)) {catt('444 trying load',sx); try({load(sfile,envir =.GlobalEnv  )})} # ; ls(sx)
		if( !exists(sx)| recalc > 1) { catt('888 recalculating...',sx)
			assign(sx, calcx, envir=.GlobalEnv   );
			do.call('save', list(sx, file= sfile))
		}
	}
	
	'%<=%' = function(x,calcx){
		sx= deparse(substitute(x)) ; sfile=  sx %+% '.RData'; # catt(333,sx)#, str(calcx),recalc) #, sx %+% ' <<- '  %+% deparse(substitute(calcx))) #   getwd() %+% '/' %+% sx %+% '.RData'
		if( T | recalc > 1 & file.exists(sfile)) try(if(!file.remove(sfile) )catt('Err: cant remove file!!'))   #    |
		if((!exists(sx)| recalc == 1) & file.exists(sfile)) {catt('444 trying load',sx); try({load(sfile,envir =.GlobalEnv  )})} # ; ls(sx)
		if(  T | !exists(sx)| recalc > 1) { catt('888 recalculating...',sx)
			assign(sx, calcx, envir=.GlobalEnv   );
			do.call('save', list(sx, file= sfile))
		}
	}
	
	
	
	
	all0= function(x){all( abs(x) < 1e-8 )}
	
	fregTAG= function(xx) as.formula ("TAG~" %+% paste(xx, collapse="+"))
	
	
	
	MergeByName= function(x,y){
		cf=merge(x,y,all=T, by ="row.names")
		rownames(cf)= cf$Row.names; cf$Row.names=NULL
		
		n1=deparse(substitute(x)); if(is.data.frame(x))n1=names(x);
		n2=deparse(substitute(y)); if(is.data.frame(y))n2=names(y);
		
		names(cf)=c(n1,n2)
		
		cf
	}
	
	#print( MergeByName(coef18,coef11))
	#print( MergeByName(cf,coef11))
		
	#  Correl Circle === # pc=prcomp(atn[sel, xx])
	CorrelCircle= function(pc){ #  Correl Circle === # pc=prcomp(atn[sel, xx])
		a<- seq(0,2*pi,length=100)
		x11()
		plot( cos(a), sin(a), type='l', lty=3,
				xlab='comp 1', ylab='comp 2', main="Correl Circle, noHot1D" , xlim=c(- 1.2, 1.2), ylim=c(-1.2, 1.2))
		v <- t(pc$rotation)[1:2,]
		arrows(0,0, v[1,], v[2,], col='red')
		text(v[1,], v[2,],colnames(v),cex=.5)
		
		#print(pc$rotation)
		print(pc)
		#plot(pc)
	}
	
	
	
	CorrelCircle3= function(pc){ #  Correl Circle === # pc=prcomp(atn[sel, xx])
		a<- seq(0,2*pi,length=100)
		x11()
		par(mfrow=c(2,2))
		for ( i in 1:3){ c1=substr("232",i,i); c2=substr("113",i,i);
			plot( cos(a), sin(a), type='l', lty=3,
					xlab=paste('comp',c1), ylab=paste('comp',c2),
					xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2))
			v <- t(pc$rotation)[c(as.numeric(c1),as.numeric(c2)),]; #print(v)
			arrows(0,0, v[1,], v[2,], col='red', length = 0.1)
			# text(v[1,], v[2,],colnames(v),cex=.9)
			text(v[1,], v[2,],LETTERS[1:dim(pc$rotation)[2]],cex=1)
		}
		
		print(pc)
		print(data.frame(L=LETTERS[1:dim(pc$rotation)[2]],pc$rotation[,1:4]))
		#print(data.frame(L=LETTERS[1:dim(pc$rotation)[2]],pc)
		#    pc$L=  LETTERS[1:dim(pc$rotation)[2]]
		plot(pc)
	}
	
	if(F){
		CorrelCircle3(pc)
		
		pairs(pc$rotation[,1:3], gap=0,
				upper.panel = function(x,y){ a<- seq(0,2*pi,length=100);
					par(new = TRUE)
					plot( cos(a), sin(a), type='l', lty=3,
							#xlab='comp 1', ylab='comp 2', #main="Correl Circle, noHot1D" ,
							xlim=c(-1.2, 1.2), ylim=c(- 1.2, 1.2))
					#v <- t(pc$rotation)[1:2,]
					arrows(0,0,x,y, col='red', length = 0.1)
					#text(x,y,rownames(pc$rotation),cex=.9)
					text(x,y,LETTERS[1:dim(pc$rotation)[1]],cex=2)
				},
				lower.panel = NULL
		)
	}
	
	
	PrintModel= function(coef,lrmodel,x){
		select(lrmodel)
		print(paste("ym=",lrmodel$ym, " Inter=",lrmodel$Inter))
		a=NULL
		for (i in names(coef) ){
			a=rbind(a,list(i,coef[i],lrmodel$xm[i],lrmodel$scales[i],mean(x[,i]),sd(x[,i])))
		}
		a=data.frame(a);#  print(a);
		names(a)=c("Name", "coef", "xm","scale", "mean", "sd")
		rownames(a)=a$Name; a$Name=NULL  ; print (a)
		return(a)
	}
	#PrintModel(coef18,lmr18,atnNO[atnNO$Train,])
	
	
	analQuantiles= function(y, wt){
		y1=y[y>0]; wt1=wt[y>0]
		wq1=    wtd.quantile ( y1,weights=wt1,probs=c(0, .25, .5, .75, 1))
		#               print(wq1)           
		wqL=log(wq1)
		nl1=(2*wq1[3]-wq1[2]-wq1[4])/(wq1[4]-wq1[2])
		nlL=(2*wqL[3]-wqL[2]-wqL[4])/(wqL[4]-wqL[2])
		if( is.na(nl1)){return(as.data.frame(cbind(deparse(substitute(y)),type="NA",wq1)))}
		if(is.na(nlL)){return(as.data.frame(cbind(deparse(substitute(y)),type="NAL",wq1,wqL)))}
		
		if(abs(nl1)>abs(nlL)) {type="Log";q1=wqL[2];q3=wqL[4]} else {type="Lin";q1=wq1[2];q3=wq1[4]}
		q13=q3-q1
		#return(data.frame(cbind(deparse(substitute(y)),type, nl1, nlL, q1, q3)))
		return(data.frame(cbind(type, signif(nl1,4), signif(nlL,4), signif(q1,4), signif(q3,4), signif(q13,4))))
		#return( list( deparse(substitute(y)),type, nl1, nlL, q1, q3 ))
	}
	
	
# Score
	yScore= function(coef,x){    #coef=lmr9$coef x=ats  coef=c;
		yhat=rep(0, dim(x)[1])
		for (i in names(coef) ){ print(i);    
			yhat=yhat+ as.numeric (coef[i])*x[,i]                  
		}
		return(yhat)
	}
	
# Score Scaled
	yScoreSc= function(coef, scales, x){    #coef=lmr9$coef x=ats  coef=c;
		cat("Score Scaled\n"); print(cbind(coef,scales))
		yhat=as.matrix(x[,names(coef)]) %*% as.matrix(coef/scales)
		return(yhat)
		
		if(F){ #equival prev
			yhat=rep(0, dim(x)[1]);
			for (i in names(coef) ){ print(i);    
				yhat=yhat+ as.numeric(coef[i])*x[,i]/scales[i]                  
			}
			return(yhat)
		}
	}
	

	
	## displayInTable
	## displayInTable ## http://bioinf.wehi.edu.au/~wettenhall/RTclTkExamples/tktable.html
	displayInTable <- function(tclarray, title="", height=-1, width=-1, nrow=-1, ncol=-1, xSc=F) {
		tt <- tktoplevel()
		tkwm.title(tt,title)
		
		tkgrid(tklabel(tt, text= title))
		table1 <- tkwidget(tt,"table",rows=nrow,cols=ncol,titlerows=1,titlecols=1,
				height=height+1,width=width+1,colwidth=9,
				if(xSc) xscrollcommand= function(...) tkset(xscr,...),
				yscrollcommand= function(...) tkset(yscr,...))
		if(xSc) xscr <-tkscrollbar(tt,orient="horizontal", command= function(...)tkxview(table1,...))
		yscr <- tkscrollbar(tt,command= function(...)tkyview(table1,...))
		
		tkgrid(table1,yscr)
		tkgrid.configure(yscr,sticky="nsw")
		if(xSc) tkgrid(xscr,sticky="new")
		tkconfigure(table1,variable=tclarray,background="white",selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col")
		return (table1)
	}
	
	dsInTable <- function(ds, title="", height=-1, width=-1, nrow=-1, ncol=-1, digits=2, xSc=F) {
		tclarray <- tclArray()
		dm=dim(ds)
		for(i in 1:dm[1])tclarray[[i,0]] = rownames(ds)[i]
		for(j in 1:dm[2])tclarray[[0,j]] = colnames(ds)[j]
		for(i in 1:dm[1])for(j in 1:dm[2]) tclarray[[i,j]]= ifelse(is.na(ds[i,j]),'NA',ifelse(is.numeric(ds[i,j]),round(ds[i,j], digits), ch(ds[i,j])))
		
		if( title=="") title= deparse(substitute(ds))
		
		nrow=dm[1]+1; ncol=dm[2]+1
		
		require(tcltk)
		tclRequire("Tktable")
		
		tt <- tktoplevel(width=width)
		tkwm.title(tt, title)
		
		tkgrid(tklabel(tt, text= deparse(substitute(ds))))
		table1 <- tkwidget(tt,"table",rows=nrow,cols=ncol,titlerows=1,titlecols=1,
				height=height+1,width=width+1,colwidth=9,
				if(xSc) xscrollcommand= function(...) tkset(xscr,...),
				yscrollcommand= function(...) tkset(yscr,...))
		if(xSc) xscr <-tkscrollbar(tt,orient="horizontal", command= function(...)tkxview(table1,...))
		yscr <- tkscrollbar(tt,command= function(...)tkyview(table1,...))
		
		tkgrid(table1,yscr)
		tkgrid.configure(yscr,sticky="nsw")
		if(xSc) tkgrid(xscr,sticky="new")
		tkconfigure(table1,variable=tclarray,background="white",selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"",resizeborders="col")
		
#  tkgrid(tk2table(tt,variable=tclarray,rows = nrow(ds)+1,cols = ncol(ds)+1, titlerows = "1"
#     , selectmode = "extended", colwidth = 30,         background = "white"))
		return (table1)
	} #-- dsInTable()
	
	
	zqqplot= function(x,y,colx, n=1e9, bLog=F,xlab = deparse(substitute(x)),
			ylab = deparse(substitute(y)), ...){
		#x should be alltel, y -Cox
		if(bLog) {xlab=paste("ln",xlab);ylab=paste("ln",ylab); colx=colx[x>0]; x=log(x[x>0]); y=log(y[y>0]);  }
		nx=length(x); ny=length(y) ; n=min(nx,ny,n)
		
		i=order(x); xs=x[i];  clr=colx[i];  if(n<nx){xs=xs[(1:n)*nx/n]; clr=clr[(1:n)*nx/n]}
		#if(n<nx){ clr=clr[approx(1:nx, xs, n)$x]; xs=approx(1:nx, xs, n)$y;}
		ys=sort(y);   if (n<ny) {ys=ys[(1:n)*ny/n]} #  ys=approx(1:ny, ys, n)$y
		if (length(xs)!=length(ys)){ print(c(length(xs),length(ys))); plot((1:9),-(1:9));text(c(length(xs),length(ys)));return(); }
		plot(xs,ys,col=clr,xlab = xlab, ylab = ylab,  ...)
	}
	
#qq plot with wghts
	zqqplotWt= function(x,y,colx, wx, n=1e9, bLog=F,xlab = deparse(substitute(x)),
			ylab = deparse(substitute(y)), ...){
		#x should be alltel, y -Cox
		if(bLog) {xlab=paste("ln",xlab);ylab=paste("ln",ylab); colx=colx[x>0]; x=log(x[x>0]); y=log(y[y>0]);  }
		nx=length(x); ny=length(y) ; n=min(nx,ny,n)
		
		i=order(x); xs=x[i];  clr=colx[i]; ys=sort(y);wx=wx[i]
		wcx=cumsum(wx); wcxr=wcx/sum(wx)
		wcyr=(1:ny)/ny
		w=wcxr*ny; #if(w<1)w=1;
		ys1=ys[wcxr*ny];
		#plot(wcxr,xs,col=clr,xlab = xlab, ylab = ylab,  ...)
		#lines(wcyr,ys)
		if(1==0){   if(n<nx){xs=xs[(1:n)*nx/n]; clr=clr[(1:n)*nx/n]}
			#if(n<nx){ clr=clr[approx(1:nx, xs, n)$x];
			# xs=approx(1:nx, xs, n)$y;}
			ys=sort(y);   if (n<ny) {ys=ys[(1:n)*ny/n]} #  ys=approx(1:ny, ys, n)$y
		}
		nxs=length(xs); nys1=length(ys1);
		if (nxs>nys1){ys1[nxs]=0; cat("pt1.",nxs,nys1,"\n"); nys1=length(ys1)} #ys1[nxs-1]}
		if (nxs!=nys1 | nys1==0){ print(c(nx,length(wcxr),ny,nxs,nys1,is.na(ys[1]), is.na (ys[nxs]))); plot((1:9),-(1:9));text(c(length(xs),length(ys)));return(); }
		plot(xs,ys1,col=clr,xlab = xlab, ylab = ylab, ...) #, pch="."
		
	}
# test zqqplotWt ====
#n=400
#at$wght=50-49*at$TAG;
#zqqplotWt(at$TIMRWM,cx$TIMRWM,at$colr,at$wght,n, bLog=F)
	
	
	
# test zqqplot ====
	if(F){
		x11()
		par(mfrow=c(2,2), ask=F, new=F)
		n=50
		zqqplot(axs,cxs,clr, bLog=F)
		zqqplot(ax,cx,clr, bLog=F)
		zqqplot(ax,cx,clr, bLog=T)
		zqqplot(ax,cx,clr, bLog=T)
		
		zqqplot(axs,cxs,clr,n, bLog=F)
		zqqplot(ax,cx,clr,n, bLog=F)
		zqqplot(ax,cx,clr,n, bLog=T)
		zqqplot(ax,cx,clr,n, bLog=T)
		
		n=400
		at$wght=500-499*at$TAG;
		zqqplotWt(at$TIMRWM,cx$TIMRWM,at$colr,at$wght,n, bLog=F)
	}
	
	
	
	princ= function(){ options(printcmd ="lpr -Psdoclr04"); dev.print()}
	prinb= function(){ options(printcmd ="lpr -Psdoprt05"); dev.print()}
	
	
	ROC= function(y,yhat,wt,np=100,...){ #lift curve
		print(np)
		i=order(-yhat);  yhs=yhat[i];  ys  =y[i];  wts =wt[i]
		ycs =cumsum(ys*wts)/sum(ys*wts)
		wtcs=cumsum(wts)/sum(wts)
		n   =length(wtcs)
		ycsn=ycs[(1:np)*n/np];wtcsn=wtcs[(1:np)*n/np]
		plot(ycsn~wtcsn,...)
		abline(a=0,b=1,col="green")
	}
	
	CV = function(y, yhat){   #======================================= CV ====
		return (  sum((y-yhat)^2, na.rm=T) / sum((y-mean(y,na.rm=T))^2,na.rm=T))
	}
	
	CVwt = function(y, yhat, wt){   #======================================= CV ====
		ym=sum(y*wt,na.rm=T)/sum(wt,na.rm=T)
		return (  sum(wt*(y-yhat)^2, na.rm=T) / sum(wt*(y-ym)^2,na.rm=T)  )
	}
	
	
#==== Lift Curve =================================================
	Lift = function(y,yhat,clr='red',...){
		#y=K;yhat=eK
		cv=signif(CV(y,yhat),3);
		nxTot=length(y)
		
		or=order(-yhat, -y, na.last = NA); #print(or);
		
		y.sort=y[or] ; #print(cbind(y.sort,yhat.sort))
		yhat.sort=yhat[or];
		nx=length(y.sort); print(nx)
		
		y.cum=cumsum (y.sort);         y.ncum=100 * y.cum/y.cum[nx]
		yhat.cum=cumsum(yhat.sort);     yhat.ncum=100 * yhat.cum/yhat.cum[nx]
		
		p=1:nx/nx
		LiftArea=cumsum(abs(y.ncum/100-p))[nx]/(nx-1)
		
		matplot(100* 1:nx/nx, cbind( y.ncum, yhat.ncum),
				main=paste("Lift ",deparse(substitute(yhat)),' [ CV=',cv,']' ),
				col=c(clr,'blue'), pch=1:2 ,
				xlab=paste('% rank ',deparse(substitute(yhat))),
				ylab=paste('% cum',deparse(substitute(y))),type="pl",lty=1:2,...
		)
		abline(0,1,col='darkgreen',lty=2)
		mtext(paste('nxTot=',nxTot,' nx.reg=',nx, 'LiftArea=',signif(LiftArea,3)),
				side=3,cex=.8)
		
		return(LiftArea)
	}
	
#Lift(K,eK)
#Lift(S,eS, clr='black',add=T)
	
	{ #block to run #7
		
		#==== Lift Curve Wt  =================================================
		LiftWt = function(y,yhat,wt,nPlot=100, clr='red',...){
			cv=signif(CVwt(y,yhat,wt),3);
			nxTot=length(y)
			
			or=order(-yhat, -y, na.last = NA); #print(or);
			
			y.sort=y[or] ; #print(cbind(y.sort,yhat.sort))
			yhat.sort=yhat[or];
			wt.s=wt[or];
			nx=length( y.sort); print(nx);
			
			yhat.cum=cumsum(y.sort*wt.s);           yhat.ncum=100 * yhat.cum/yhat.cum[nx]
			wt.cum=cumsum(wt.s);                    wt.ncum=100 * wt.cum/wt.cum[nx]
			
			ory=order(-y, na.last = NA); y.sort=y[ory] ;
			wty.s=wt[ory];
			wty.cum=cumsum(wty.s);                  wty.ncum=100 * wty.cum/wty.cum[nx]
			y.cum=cumsum(y.sort*wty.s);         y.ncum=100 * y.cum/y.cum[nx]
			
			p=1:nx/nx
			LiftArea=cumsum(abs(y.ncum/100-p))[nx]/(nx-1)
			
			xp=round((1:nPlot)*nx/nPlot)
			print(xp)
			xplot=wt.ncum[xp]
			print(xplot)
			print(y.ncum[xp])
			print( yhat.ncum[xp])
			plot(xplot, yhat.ncum[xp],
					main=paste("Lift ",deparse(substitute(yhat)),' [ CV=',cv,']' ),
					col=clr, pch=2 ,
					xlab=paste('% rank ',deparse(substitute(yhat))),
					#         ylab=paste('% cum',deparse(substitute(y))),type="pl",lty=1:2,...
					ylab=paste('% cum',deparse(substitute(y))),type="p",lty=2
			)
			abline(0,1,col='darkgreen',lty=2)
			lines(wty.ncum[xp],wty.ncum[xp],col='blue',pnc=1,type="l",lty=1)
			
			
			mtext(paste('nxTot=',nxTot,' nx.reg=',nx, 'LiftArea=',signif(LiftArea,3)),
					side=3,cex=.8)
			
			return(LiftArea)
		}
		
		
		#test
		if(F){
			n=40
			p=1:n
			x=p
			y=2*x +30*rnorm(n)
			nPlot=10
			clr="red"
			
			set.seed(1)
			wt=4+rnorm(n)
			yhat=2*x
			np=20 # 100
			y2h=1.5*x+30*rnorm(n)
			
			plot(y~x)
			lines(yhat ~ x)
			lines(y2h ~ x, col="red")
			
			
			
			print(CVwt(y, yhat, wt))
			print(CVwt(y, y2h, wt))
			LiftWt(y,yhat,wt,nPlot=np, clr='red',main="zz")
			LiftWt(y,yhat=yhat,wt,nPlot=np, clr='red')
			x11();plot.new()
			LiftWt(y,yhat=y2h,wt,nPlot=np, clr='blue',add=T)
			LiftWt(y,yhat=y2h,wt,nPlot=np, clr='blue')
			
		}
	} #block to run #7 =========================
	
	
	##  http://addictedtor.free.fr/graphiques/graphcode.php?graph=155
	ShowColorNames= function(patt='', ord=F){
		pa= par(mar=c(0,0,0,0), cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
		colorNames = colors()
		colorNames = grep(patt, colorNames, v=T)  ## AZ
		if(ord) colorNames = colorNames[order(nchar(colorNames))]  ## AZ
		
		windows(wi=15, he=9)
		plot.new()
		for (j in 1:60) { for (i in 1:11) {
				kolor = colorNames[(i-1)*60+j]
				rect(i/11,j/60,(i-1)/11,(j-1)/60,col=kolor)
				text((i-0.5)/11,(j-0.5)/60,kolor,col=ifelse(mean(col2rgb(kolor))<120,"white","black"))
			}}
		
		mtext("Color names for colors(grDevices)",3,line=0,cex=2)
#		
#		par(mar=c(3,0,3,0), cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
#		plot.new()
#		mtext("Color names for grDevices and colorRamps packages",3,cex=2)
		par(pa)		
		if(ex<- 0){
			ShowColorNames('3$')
			ShowColorNames('2$')
			ShowColorNames('[a-z]2$')
			ShowColorNames('[a-z]$')
			ShowColorNames('')
			plot(1:80, pch=16, cex=3, col=grep('[a-z]4$',colors(), v=T))
			text(1:80,1:80,  cex=1, t<- grep('[a-z]4$',colors(), v=T), col=t, adj=c(3,0))
		
			
			ColNames= grep('deep',colors(), v=T);
			ColNames= grep('dark',colors(), v=T);
			ColNames= grep('light',colors(), v=T);
			ColNames= c(grep('light.*3',colors(), v=T), grep('dark.*2',colors(), v=T));
			ColNames= grep('pale',colors(), v=T);
			
			ColNames= grep('[a-z]3$',colors(), v=T); 
			ColNames= grep('[a-z]4$',colors(), v=T); 
			paste(ColNames, sep=' ', collapse=' ')
			# [1] "antiquewhite4 aquamarine4 azure4 bisque4 blue4 brown4 burlywood4 cadetblue4 chartreuse4 chocolate4 coral4 cornsilk4 cyan4 darkgoldenrod4 darkolivegreen4 darkorange4 darkorchid4 darkseagreen4 darkslategray4 deeppink4 deepskyblue4 dodgerblue4 firebrick4 gold4 goldenrod4 gray4 green4 grey4 honeydew4 hotpink4 indianred4 ivory4 khaki4 lavenderblush4 lemonchiffon4 lightblue4 lightcyan4 lightgoldenrod4 lightpink4 lightsalmon4 lightskyblue4 lightsteelblue4 lightyellow4 magenta4 maroon4 mediumorchid4 mediumpurple4 mistyrose4 navajowhite4 olivedrab4 orange4 orangered4 orchid4 palegreen4 paleturquoise4 palevioletred4 peachpuff4 pink4 plum4 purple4 red4 rosybrown4 royalblue4 salmon4 seagreen4 seashell4 sienna4 skyblue4 slateblue4 slategray4 snow4 springgreen4 steelblue4 tan4 thistle4 tomato4 turquoise4 violetred4 wheat4 yellow4"
			# [1] "antiquewhite3 aquamarine3 azure3 bisque3 blue3 brown3 burlywood3 cadetblue3 chartreuse3 chocolate3 coral3 cornsilk3 cyan3 darkgoldenrod3 darkolivegreen3 darkorange3 darkorchid3 darkseagreen3 darkslategray3 deeppink3 deepskyblue3 dodgerblue3 firebrick3 gold3 goldenrod3 gray3 green3 grey3 honeydew3 hotpink3 indianred3 ivory3 khaki3 lavenderblush3 lemonchiffon3 lightblue3 lightcyan3 lightgoldenrod3 lightpink3 lightsalmon3 lightskyblue3 lightsteelblue3 lightyellow3 magenta3 maroon3 mediumorchid3 mediumpurple3 mistyrose3 navajowhite3 olivedrab3 orange3 orangered3 orchid3 palegreen3 paleturquoise3 palevioletred3 peachpuff3 pink3 plum3 purple3 red3 rosybrown3 royalblue3 salmon3 seagreen3 seashell3 sienna3 skyblue3 slateblue3 slategray3 snow3 springgreen3 steelblue3 tan3 thistle3 tomato3 turquoise3 violetred3 wheat3 yellow3"

			ColNames= ColNames[c(2,5,6,8,9,10,11,13,14,17,20,21,23,27,31, 40,44,47,51,52,56,59,60,61,69,72,77)]
			ColNames= cn("aquamarine3 blue3 brown3 
			cyan3  darkorchid3 deepskyblue3  green3 
			 magenta3 orange3  springgreen3 orangered3 palevioletred3 
			plum3 purple3 red3 slateblue3 chocolate3 indianred3 deeppink3")

			ColNames= c(brewer.pal(8,"Dark2"), cn("aquamarine3 blue3 brown3 
			cyan3  darkorchid3 deepskyblue3  green3 
			magenta3 orange3  springgreen3 orangered3 palevioletred3 
			plum3 purple3 red3 slateblue3 chocolate3 indianred3 deeppink3"))
			palette(ColNames)
			
			ColNames= sort(adjustcolor(ColNames, alpha.f = .7))
			ColNames= sort(adjustcolor(brewer.pal(8,"Dark2"), alpha.f = .7))
			
			palette(adjustcolor(brewer.pal(8,"Dark2"), alpha.f = .7))

			ii= 1:le(ColNames)
			plot(ii /10, ii %% 10, pch=16, cex=28, col=ColNames, xlim=c(-250,3), ylim=c(1,11))
			text(ii /10, ii %% 10,  cex=13, t<- sf('%s %s',  ii,  ColNames), col=ColNames, adj=c(1.1,0), font=2)
			
		}
	}
	
	

	
	if(0)hists = function(ds= ds.sim){
			newwin(2,2,'r&C Means',{
						hist(colMeans(ds), breaks=40)
						hist(rowMeans(ds), breaks=340)
						hist(log(colMeans(ds)), breaks=40)
						hist(log(rowMeans(ds)), breaks=340)
#                       hist(colSums(ds), breaks=40)
#                       hist(rowSums(ds), breaks=340)
					})
		} #--ds
	
	
	
	# http://rayfd.wordpress.com/2007/05/20/10-eclipse-navigation-shortcuts-every-java-programmer-should-know/
	# ^Q  Go to the last edit location 
	# ^E go other editors
	# CTRL+Shift+G, which searches the workspace for references to the selected method or variable
	
	
	


}# nope ------------------------------------

if(0){
	rmDF(); lsDF()
	Sys.getenv()
	Sys.getenv(c("R_HOME", "R_PAPERSIZE", "R_PRINTCMD", "HOST"))
	names(s <- Sys.getenv())
	t(Sys.getenv(c("JAVA_HOME", "PATH")))
	Sys.getenv(c("JAVA_HOME"))
	Sys.getenv(c("PATH"))
	# [1] "R:\\R-2.14.2-Eclipse\\bin\\x64;C:\\Program Files\\Microsoft HPC Pack 2008 R2\\Bin\\;c:\\Rtools\\bin;c:\\Rtools\\MinGW\\bin;c:\\Rtools\\MinGW64\\bin;C:\\Program Files\\Common Files\\Microsoft Shared\\Windows Live;C:\\Program Files (x86)\\Common Files\\Microsoft Shared\\Windows Live;C:\\GTK\\bin;C:\\Program Files (x86)\\MiKTeX 2.8\\miktex\\bin;C:\\windows\\system32;C:\\windows;C:\\windows\\System32\\Wbem;C:\\windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Program Files\\System Center Operations Manager 2007\\;C:\\Program Files (x86)\\Windows Live\\Shared;C:\\arc\\exe\\UnxUtils\\usr\\local\\wbin;C:\\arc\\exe\\UnxUtils\\bin;C:\\Program Files (x86)\\Microsoft Application Virtualization Client;C:\\Program Files (x86)\\ggobi;C:\\Program Files (x86)\\QuickTime\\QTSystem\\;C:\\cygwin\\bin;c:\\z\\exe\\R-2.13.0-port\\bin;C:\\Program Files (x86)\\Microsoft Team Foundation Server 2010 Power Tools\\;C:\\Program Files (x86)\\Microsoft Team Foundation Server 2010 Power Tools\\Best Practices Analyzer\\;C:\\Program Files (x86)\\Microsoft SQL Server\\100\\Tools\\Binn\\;C:\\Program Files\\Microsoft SQL Server\\100\\Tools\\Binn\\;C:\\Program Files\\Microsoft SQL Server\\100\\DTS\\Binn\\;C:\\Program Files (x86)\\Microsoft SQL Server\\100\\Tools\\Binn\\VSShell\\Common7\\IDE\\;C:\\Program Files (x86)\\Microsoft Visual Studio 9.0\\Common7\\IDE\\PrivateAssemblies\\;C:\\Program Files (x86)\\Microsoft SQL Server\\100\\DTS\\Binn\\;C:\\Program Files (x86)\\CVSNT\\"

	# [1] "C:\\Program Files\\Java\\jre6"

	Sys.setenv(PATH="R:\\R-2.14.2-Eclipse\\bin\\x64;C:\\Program Files\\Microsoft HPC Pack 2008 R2\\Bin\\;c:\\Rtools\\bin;c:\\Rtools\\MinGW\\bin;c:\\Rtools\\MinGW64\\bin;C:\\Program Files\\Common Files\\Microsoft Shared\\Windows Live;C:\\Program Files (x86)\\Common Files\\Microsoft Shared\\Windows Live;C:\\GTK\\bin;C:\\Program Files (x86)\\MiKTeX 2.8\\miktex\\bin;C:\\windows\\system32;C:\\windows;C:\\windows\\System32\\Wbem;C:\\windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Program Files\\System Center Operations Manager 2007\\;C:\\Program Files (x86)\\Windows Live\\Shared;C:\\arc\\exe\\UnxUtils\\usr\\local\\wbin;C:\\arc\\exe\\UnxUtils\\bin;C:\\Program Files (x86)\\Microsoft Application Virtualization Client;C:\\Program Files (x86)\\ggobi;C:\\Program Files (x86)\\QuickTime\\QTSystem\\;C:\\cygwin\\bin;c:\\z\\exe\\R-2.13.0-port\\bin;C:\\Program Files (x86)\\Microsoft Team Foundation Server 2010 Power Tools\\;C:\\Program Files (x86)\\Microsoft Team Foundation Server 2010 Power Tools\\Best Practices Analyzer\\;C:\\Program Files (x86)\\Microsoft SQL Server\\100\\Tools\\Binn\\;C:\\Program Files\\Microsoft SQL Server\\100\\Tools\\Binn\\;C:\\Program Files\\Microsoft SQL Server\\100\\DTS\\Binn\\;C:\\Program Files (x86)\\Microsoft SQL Server\\100\\Tools\\Binn\\VSShell\\Common7\\IDE\\;C:\\Program Files (x86)\\Microsoft Visual Studio 9.0\\Common7\\IDE\\PrivateAssemblies\\;C:\\Program Files (x86)\\Microsoft SQL Server\\100\\DTS\\Binn\\;C:\\Program Files (x86)\\CVSNT\\;C:\\Program Files\\Java\\jre6\bin")
	
	
	# profiling ===
	Rprof(NULL)
	sm= summaryRprof()
	sus(sm$by.total, total.pct>10)
	
	
	options(error=recover)
	options(error=dump.frames)
	options(error=NULL)
	
	
	
	##== auto correction ===
	theFile= quote('Z:/gitRepo/gitRepo/git1/git/zBase.r')
	s= readLines(theFile)
	
#	# replace <- to  = 
#	for(i in  grep('<\\-', s)){catt('# ',i, (s[i]))
##		s[i]= gsub('<\\-', '=', s[i])
##		catt('# ', i, (s[i]), '\n')
##		
#	}
	
	# add blank after = and ,
	for(i in  grep('([=\\,])([0-9a-zA-Z])', s)){catt('# ',i, (s[i]))
		s[i]= gsub('([=\\,])([0-9a-zA-Z])', '\\1 \\2', s[i])
		catt('# ', i, (s[i]), '\n')
		
	}
	
	# drop blank in x = 
	for(i in  grep('([0-9a-zA-Z]) +(=[^=])', s)){catt('# ',i, (s[i]))
		s[i]= gsub('([0-9a-zA-Z]) +(=[^=])', '\\1\\2', s[i])
		catt('# ', i, (s[i]), '\n')
		
	}
	
	writeLines(s, gsub('[rR]$','b.R', theFile))
	file.edit(gsub('[rR]$','b.R', theFile))
	
	
	#s= readLines(theFile)
	for(i in grep('^[^#]*(randomForest|tuneRF)\\(', s)){catt('# ',i, (s[i]))
		s[i]=sf('  run({ %s })   # auto-generated', s[i], gsub(' *#.*$','',s[i]))
	}
	
	writeLines(s, gsub('[rR]$','b.R', theFile))
	file.edit(gsub('[rR]$','b.R', theFile))
	
	
	gf('===|---')
	gf('cat')
	gf('#=')
	gf('HHp')
	gf('randomForest')
	
	gw()
	lsDF()
	rmDF()
	
	cat('\a\a\a'); alarm(); expl("C:/Users/Public/Music/Sample Music/Kalimba.mp3")
	gcc()
	
	
	#= for Medio - install list s/w ===
	ff= dir('z:/exe', rec=F, patt='^[^\\.]+$', include.dirs = T)
	showInOpera(sf('www.google.com?q=download+%s',ff[2:4]))
	for(f in ff[2:4])exec(sf('start firefox.exe http://www.bing.com?q=download++%s',f))
	
	Sys.getenv('computername') 
	#[1] "T510-1004-2"

	install.packages('rJava')	
	install.packages('rj')

#= test apache ===
	source('~/work/s00_commonR/zBase.r')
	gw()
	sw('~/work/99_test/out')
	HHInit()
	# gw: sw("/workplace/azolotovitski/work/99_test")
	# [1] "/workplace/azolotovitski/work/99_test/index.html"

	HHt('99_Test')
	# 

	HHd('test1')
	plot((1:9)^2)
	HHp('just plot')
	#HHs()
	#	system() 
	#	HHs
	#	expl
	dir()
	# [1] "index.html" "Pic_1.png" 

	#system('sudo cp * /var/www/html/99_test/')
	# 
	dir('/var/www/html/99_test/')
	
	HHt('add more')
	plot((1:99)^-2)
	HHp('plot 2')
	#system('sudo cp -r ../99_test /var/www/html/')
	system('cp -r ../99_test /var/www/html/az/')
	system('cp -r ../out /var/www/html/az/')
	dir(); dir('..'); gw()
	dir('/var/www/html/99_test/')
	
	installed.packages()
	libra(rmr)
	
	#= RHadoop  ===
	#=  https://github.com/RevolutionAnalytics/RHadoop/wiki/rmr  ===
	# pre:  Rcpp, RJSONIO (0.95-0 or later recommended), itertools and digest
	libra(Rcpp)       # -
	#	  Installing Rcpp package on Windows and Centos5
	#	  https://groups.google.com/forum/?fromgroups#!topic/brumail/rvvKt6b3gEs
	#	cd /usr/local/RevoR
	#	749  sudo wget http://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_0.9.10.tar.gz
	#	753  tar -xvf Rcpp_0.9.10.tar.gz
	#	758  sudo R CMD INSTALL -l /usr/local/lib64/Revo-6.0/R-2.14.2/lib64/R/library  Rcpp
	
	libra(RJSONIO)    # OK
	libra(itertools)  # OK
	libra(digest)     # OK

	
	}
	
#
