#' misc  aliases and functions.
#'
#' \tabular{ll}{
#' Package: \tab zBase\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2011-11-06\cr
#' License: \tab GPL-2\cr
#' LazyLoad: \tab yes\cr
#' }
#' @name zBase - package
#' @aliases aliases
#' @docType package
#' @title Miscellaneous aliases and functions
#' @author Alex Zolotovitski \email{alexzol@microsoft.com}
#' @references
#' \url{http://roxygen.org/roxygen.pdf}
#' @keywords package
#' @seealso \code{\link{aliases}}
#' @examples
#' pas(letters %+% LETTERS) 

# http://roxygen.org/roxygen.pdf
if(0){
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

#aliases ==
gw= getwd
sw= setwd
sw= function(sDir){
	sDir= gsub('/$','',sDir)
	if(!file.exists(sDir)) dir.create(sDir)
	stopifnot(file.exists(sDir))
	setwd(sDir)
	str(gw())
}
#sw('out'); gw()

d.o= dev.off

nu= as.numeric
ch= as.character
fa= as.factor
df= data.frame
ma= as.matrix

le= length
he=head
sus= subset
summ= summary
na= names
tra= transform
trb= traceback
brr= browser

fp= file.path

# one liners ===
catt  = function(...) {cat(...); cat('\n'); flush.console()}
catf  = function(...) cat(sprintf(...))
sf  = function(...) sprintf(...)
logg  = function(...) cat(format(Sys.time(), "%Y-%b-%d %H:%M:%S "),..., '\n', file = "log.txt", append = T)
'%+%' = function(x,y) paste(x, y, sep= "")
renamee= function(ds, oldname, newname){names(ds)[names(ds)==oldname]=newname; names(ds)}
fsize = function(f)file.info(f)$ size /1e6
#lss= function()print(sort(sapply(ls(envir = .GlobalEnv), function(x) object.size(get(x))/1e6)),signif=2, scipen=99)
lss0= function()print(sort(sapply(ls(envir = .GlobalEnv), function(x) object.size(get(x))/1e6)),signif=2, scipen=99)
#lss()
rmall= function()rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv) # rmall()
nin= '%-%' = function(x,y) x[!(x %in% y)] # x not in y 1:5 %-%  4:9 # `%-%` 
zeval= evall= function(s)eval(parse(text= s), envir=.GlobalEnv)

exec= function(s)shell(s, wait=T, intern = T)
execf= function(fmt,...)shell(sf(fmt,...), wait=T, intern = T)



nut= function(...)as.numeric(tclvalue(...))
st= function(...)system.time(...)[[3]]

sa= function(file='.RData'){save.image(file=file); catf("Image saved: lo('%s/%s')  at  %s\n",gw(),file,Sys.time())}
lo= function(file='.RData'){load(file=file, .GlobalEnv);lss()}



rt= function(f, nrows=1e6,...)read.table('../' %+% f,  header=F, stringsAsFactors =F, sep='\t', nrows = nrows,...) # debug !!! xxx
rtd= function(f, nrows=1e6,...)read.delim('../' %+% f,  header=F, stringsAsFactors =F, sep='\t', nrows = nrows,...) # debug !!! xxx
rtsv= function(s, header=F,...){ds= read.table(s, header=header,   stringsAsFactors =F, sep='\t', comment.char = "",...)
	str(ds)
	ds
}


#pr= print
pr= function(x){catf('== %s =',deparse(substitute(x))); print(x)}
prr= function(x)for(xx in x)catt(xx)

ordd= function(ds,col) ds[order(-ds[,col]),] ## desc many cols
ord= function(ds,col){if(substr(col,1,1)=='-') ds[order(-ds[,substr(col,2,99)]),] else ds[order(ds[,col]),]} ## 1 col only
ord2= function(ds,col= '-cAmt -Amt'){cols= strsplit(col, ' ')[[1]]
	if(substr(cols[1],1,1)=='-') v1= - ds[,substr(cols[1],2,99)] else v1= ds[,cols[1]];
	if(substr(cols[2],1,1)=='-') v2= - ds[,substr(cols[2],2,99)] else v2= ds[,cols[2]];
	ds[order(v1,v2),]
}

ab= function(a=0,b=1,col='gray60',lty=2,lwd=2,...)abline(a, b, col=col, lty=2,...)


one= function(x)rep(1,le(x))
cn= function(cnn) unlist(strsplit(cnn,'[ ,\n\t\\+]+'))  # col.names= cn(cnn)
nmsv= sNames= function(ds, sep=' ')paste(names(ds),sep=' ',collapse=sep) #inverse to cn()
norm= function(x)(x-min(x, na.rm =T))/diff(range(x, na.rm =T))
isNum= function(x)typeof(x)!="character"
gcc= function()base ::gc(reset=T) # garb collect
rows.with.na= function(ds)apply(ds,1,function(x)any(is.na(x)))
tab= function(x)table(x, useNA = "ifany")
pas= function(x,sep='_',collapse='_')paste(x,sep=sep,collapse=collapse)
fid= function(ds,cols=names(ds))if(le(cols)<2)return(ds[,cols])else return(apply(ds[,cols],1,pas)) # id= fid(dn,cn('MediumId TrafficType'))
summar= function(.data, .variables, .fun = summarize, ...) ddply(.data, .variables, summarize,...)
mt= function(s,...)mtext(s, 2, cex=.7, line=-1,...)
dhe= function(ds, h=9){catt(NROW(ds),' rows'); print(head(ds,h)); catf('# %s[, cn("%s")]\n',deparse(substitute(ds)),nmsv(ds))}
hee= dhe= function(ds, h=9){catt(NROW(ds),' rows'); print(head(ds,h)); catf('# he(sus(%s, , sel= cn("%s")), 5)\n',deparse(substitute(ds)),nmsv(ds))}
#dhe= function(ds, h=5){print(dim(ds)); head(ds,h); print(nmsv(ds))}
heee= dsh= function(ds, h=5){print(dim(ds)); pr(str(ds)); pr(head(ds,h)); catf('# %s[, cn("%s")]\n',deparse(substitute(ds)),nmsv(ds))}


logit= function(p)log(p/(1-p))
zlog10= function(x, k=.001) log10(k *diff(quantile(x, c(.01,.99),na.rm = T))+ x)# log10(k *IQR(x, na.rm = T)+ x)
zlogit= function(x, k=.001){e=diff(quantile(x, c(.01,.99),na.rm = T));  log((k *e+ x)/(1+ k *e- x))}




# print aggregate memory usage statistics
me= function() {print(paste('R is using', mem<- memory.size(), 'MB out of limit', memory.limit(), 'MB'));mem}

# create function to return matrix of memory consumption
lss<-  function(verb=T){ #object.sizes <-
	#llss= rev(sort(sapply(ls(envir=.GlobalEnv), function (object.name)
	llss= (sort(sapply(ls(envir=.GlobalEnv), function (object.name)
									round(object.size(get(object.name))/1048576,2)) ))
	#if(verb)newwin(1,3,'Mem',{
	if(verb)({
						#barplot(llss, main="Memory usage by object", ylab="Bytes", xlab="Variable name",
						#col=heat.colors(length(object.sizes())))
						#dotchart(llss, main="Memory usage by object, in M", xlab="Bytes")
						pie(llss, labels = paste(names(llss), llss) , main="Memory usage by object, of tot " %+% me()) #round(llss/1e6,2))
					})
	print(llss[1])    #print(llss)     
	do.call(str,list(get(names(llss)[1]) ))
	pr(llss)
	catf('1888. rm(%s)\n',pas(rev(names(llss)),collapse=','))
	invisible(llss)
}
# lss()

#list of data.frames
lsDF= function(...) invisible(mdply(ls(envir =.GlobalEnv,... ),function(a){aa=get(a); b=df()
                        if( class(aa)[1] %in% c('data.frame','matrix')){catf('%-20s %5s %3s %-30s\n', a, nrow(aa), ncol(aa),sNames(aa))
                            b= df(ds=a, nr=nrow(aa), nc=ncol(aa), vars= sNames(aa))}
                        b
                    }))
#a= lsDF(); a

rmDF= function(...) invisible(mdply(ls(envir =.GlobalEnv,... ),function(a){aa=get(a); b=df()
                        if( class(aa)[1]=='data.frame'){catf('rm  %-20s %5s %3s %-30s\n', a, nrow(aa), ncol(aa),sNames(aa))
                            do.call(rm,list(a),envir =.GlobalEnv)
                    }}))


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


libra= function(lib){li=deparse(substitute(lib));
	if(!require(li, character.only = T, quietly =T))
		install.packages(li, repos = "http://cran.stat.ucla.edu/")
	do.call(library,list(li))
	catf('# demo(%s); example(%s)', li, li)
}



libras = function(reqpk=c("locfit","tkrplot", "xtable")){
	misspk= reqpk %-% installed.packages()[,1]

	if(length(misspk)>0) {catt(paste('\n\n***************************************************',
						'\n**\n**  We are going to install packages:"', paste(misspk,collapse =', '),'" .',
						'\n**\n***************************************************', sep=''), call. = F)
		ANSWER <- readline("Are you agree to install the packages (y/n)?")
		if (substr(ANSWER, 1, 1) == "n"){stop("Execution impossible w/o the packages !", call. = F)
		}else{install.packages(misspk, repos = "http://cran.stat.ucla.edu/")}
	}
	
	for(p in reqpk){catt('255. Call library ',p); do.call(library,list(p,character.only =T))}
} #--
#libras(c("randomForest","varSelRF"))

libra(R2HTML)

HT= HTML
Ht= HTML.title
Hp= HTMLp= function(x,...) HTML(sf('<pre> %s </pre>',x), ...)
#HH= function(capt='',...) HTMLplot(GraphFileName =  sf('Pic_%s',iPic<<- iPic+1), Caption=capt, Align = "left",...) #, Width = 800, Height = 600, GraphPointSize=1)
HH= function(capt='',...) HTMLplot(GraphFileName =  sf('Pic_%9.0f', runif(1,,1e9)), Caption=capt, Align = "left",...) #, Width = 800, Height = 600, GraphPointSize=1)


Hd= zHTML= function(x,...){ # typeof(ds) typeof(dtb)
	if(class(x)=="list") for(xx in x) {catt(444,xx); zHTML(x[[xx]],...);return()}
	if(class(x)=="integer" && le(x)>1) x= as.data.frame(x)
	HTML(x, align="left", captionalign="top", big.mark=',', digits=4,...)  
}

hLink= function(URL)gsub('(.*)', '<a href="http://\\1">\\1</a>', URL)

# .HTML.file HTMLStop()

showInOpera= function(x= .HTML.file) {
    if(class(x)== "data.frame")x= x$H2 
    cmd= sf('C:\\Progra~2\\Opera\\opera -newtab %s', paste(x,sep=' ',collapse=' '))
	sh= shell(cmd, wait=F, intern = F) #      sh         
}
# showInOpera(cn('zolot.us msn.com'))
# showInOpera()

toXL= ToXL= function(ds, dsn= deparse(substitute(ds)),wbName='OutR_1.xlsx', toCSV=TRUE, startRow1 = 9, startCol1 =2, toOpen=F, ...){ # see http://www.mirai-solutions.com/site/index.cfm?id_art=69890&actMenuItemID=31456&vsprache/EN/Mirai_Solutions___XLConnect_.cfm
    #ds=cast(agg, Z~ F,  fun.aggregate=sum, value ='pBClicks') ; dsn='Tab1.1Ys.pBClicks'
	#write.csv(ds, file=dsn %+% '.csv')
	write.csv(ds, file= sf('%s.csv', dsn))
	gcc()
    libra(XLConnect)
    wb <- loadWorkbook(wbName, create = T)
    createSheet(wb, name = dsn)
	
	# TODO createName(wb, name =dsn, formula = "mtcars!$A$1")
	
    writeWorksheet(wb, ds, sheet = dsn, startRow = startRow1+3, startCol = startCol1)
    writeWorksheet(wb, dsn, sheet = dsn, startRow = startRow1, startCol = startCol1, header=FALSE)
    if(dsn != deparse(substitute(ds))) writeWorksheet(wb, deparse(substitute(ds)), sheet = dsn, startRow = startRow1+1, startCol = startCol1, header=FALSE)
    saveWorkbook(wb)
    xlcFreeMemory()
    catt('577, ToXL: ', dsn,'saved to dir:', gw())
    if(toOpen)shell(sf('explorer /root,%s\\%s', gsub('/','\\\\', gw()),wbName))
    invisible()
}

#tocsv= function(ds, dsn= deparse(substitute(ds)) %+% '.csv',...){write.csv(ds, file=dsn, quote =F, row.names = F,...); 
tocsv= function(ds, dsn= sf('%s.csv', deparse(substitute(ds)))  ,...){write.csv(ds, file=dsn, quote =F, row.names = F,...); 
    catf("Saved: read.csv('%s/%s')",gw(),dsn)} 

expl= function(x=gw())shell(sf('explorer %s',gsub('/','\\\\', x)))

# 1 min exercise ====
Timer= function(dt=32, n=2) for(i in 1:n){Sys.sleep(dt); cat('\a'); expl('C:\\Users\\alexzol.REDMOND.000\\Music\\bell17.mp3') }


JS= JaccardSimilarity= function(x,y) {
	r=0
	try({if(le(x)==0||le(y)==0)return(0)});
	try({if(is.na(x)||is.na(y))return(0)});
	try({r=     le( y[match(x, y, nomatch = 0)])/le(unique(c(x,y)))},s=T)
	r= le( y[match(x, y, nomatch = 0)])/le(unique(c(x,y)))
	r}
#JS(m[,1]$Divs, m[,2]$Divs)     # [1] 0.8182
#
#     libra(RecordLinkage)
#    
#     jarowinkler("Andreas","Anreas")
#     > # compare one string with several others:
#                 levenshteinSim("Andreas",c("Anreas","Andeas"))
#     > # compare two vectors of strings:
#                 jarowinkler(c("Andreas","Borg"),c("Andreas","Bork"))

strReverse <- function(x) sapply(lapply(strsplit(x, ""), rev), paste, collapse="")
#strReverse(c('av','ext'))

Model= function(glm){or= order(pValue <- summary(glm)$coef[, "Pr(>|t|)"])
	catt('model=', gsub('Intercept','1',gsub(':',':',gsub('\\+ -','-',paste(names(glm$coef)[or], sep='*', collapse=' + ')))))
	catt('y= ', gsub('Intercept','1',gsub(':','*',gsub('\\+ -','-',paste(glm2$coef[or],names(glm2$coef)[or], sep='*', collapse=' + ')))))
	return(data.frame(nm= names(pValue),r= rank(pValue))[or,])
}

gf= gtf= function(patt=' ===', f= "clipboard", withLineNumb= T){ # grep pattern in the file #f= theFile
	catt(3099,'============================ gtf:', f)
	s= readLines(f, warn=F)
	ii= grep(patt,s, v=F)
	#print(ii)
	for(i in ii)catf('%4s. %s\n', i, s[i])
	invisible(s[ii])
	
	# gtf()
	# gtf('fun',"clipboard")
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
#     week(as.Date('2010-08-08'))
#    
#     LastWeek= week(Sys.Date())- 1


newwin = function(nrows= 2, ncols= 2, tytle= '', doit, pos=0, mar0= c(4,4,4,2)+.2,...){
	#windows(width=16, height=10, xpos=30+ 20* pos, ypos=30+ 20* pos, title= tytle)
	windows(xpos=30+ 20* pos, ypos=30+ 20* pos, title= tytle,...)
	op <- par(mfrow=c(nrows, ncols), mar = mar0 + .1)
	e= try({doit})
	#if (inherits(e, "try-error")) tkmessageBox(message="Newwin Error",  icon="error")
	if (inherits(e, "try-error")) stop("Newwin Error !")
	#if (inherits(e, "try-error") | e== -99) {dev.off();  return()}
	#if (inherits(e, "try-error")| le(e)==0) {catt('Newwin Err.911', str(e)); dev.off(); return()}
	if (le(e)>0)if (nu(e[[1]])== -99) {catt('Newwin Err.999'); dev.off(); return()}
	
	par(op)
	try(mtext(sf("%s, %s",tytle, Sys.time()), side=3, line=3, font=2) )   # , size=1.5
	#dev.off()
}  # newwin(,3,'zzz',{plot(1:3); plot(1:9);plot(zz) },wi=15,he=10)



spectr=     function(ds=ds, nValMax=50){  # -------------------------------------------------------
	res=list()
	cat('nrow=',nrow(ds),'\n')
	shortCateg = c()
	
	res$tb= data.frame()
	for(i in 1:ncol(ds)) {nVal=length(table(ds[,i]));
		cat(i, nVal, sum(!is.na(ds[,i])), 'not miss. ', sum(is.na(ds[,i])), 'NA ',colnames(ds)[i], '\n',sep='\t');
		if(nVal < nValMax)shortCateg=append(shortCateg,c(i))
		d= data.frame(var=colnames(ds)[i], nVal=nVal, notMiss=sum(!is.na(ds[,i])), Miss= sum(is.na(ds[,i])))
		res$tb= rbind(res$tb, d)
	}
	shortCateg
	names(ds)[shortCateg]
	
	for(i in shortCateg) {        #tofix: hardcoded
		cat('\n',i, colnames(ds)[i], '\n',sep='\t')
		print(table(ds[,i], useNA="always"), exclude = NULL)
		res[colnames(ds)[i]]= as.data.frame(table(ds[,i]))
	}
	
	cat('c(',names(ds),')\n',sep='", "')
	return(res)
}

# Alexandre Zolotovitski   
# Sep 8
{ #block to run #8
#==== Lift Curve Wt Array  =================================================
	LiftWtArr = function(y,yhat,wt=one(y),nPlot=100,
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




srt= sortt= sort.data.frame <- function(x, by){
    # Author: Kevin Wright
    # with some ideas from Andy Liaw
    # http://tolstoy.newcastle.edu.au/R/help/04/07/1076.html
    
    # x: A data.frame
    # by: A one-sided formula using + for ascending and - for descending
    #     Sorting is left to right in the formula
    
    # Useage is:
    # library(nlme);
    # data(Oats)
    # sort(Oats, by= ~nitro-Variety)
   #brr()
    
    if(by[[1]] != "~")
        stop("Argument 'by' must be a one-sided formula.")
 	
    # Make the formula into character and remove spaces
    formc <- as.character(by[2]) 
    formc <- gsub(" ", "", formc) 
    # If the first character is not + or -, add +
    if(!is.element(substring(formc, 1, 1), c("+", "-")))
        formc <- paste("+", formc, sep = "")
    
    # Extract the variables from the formula
    vars <- unlist(strsplit(formc, "[\\+\\-]"))    
    vars <- vars[vars != ""] # Remove any extra "" terms
    
    # Build a list of arguments to pass to "order" function
    calllist <- list()
    pos <- 1 # Position of + or -
    for(i in 1:length(vars)){
        varsign <- substring(formc, pos, pos)
        pos <- pos + 1 + nchar(vars[i])
        if(is.factor(x[, vars[i]])){
            if(varsign == "-") {
                calllist[[i]] <- -rank(x[, vars[i]])
            } else {
                calllist[[i]] <- rank(x[, vars[i]])
            }
        } else {
            if(varsign == "-") {
                calllist[[i]] <- -x[, vars[i]]
            } else {
                calllist[[i]] <- x[,vars[i]]
            }
        }
    }
    return(x[do.call("order", calllist), ])
}
if(0){
    dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
                         levels = c("Low", "Med", "Hi"), ordered = TRUE),
                x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
                z = c(1, 1, 1, 2))
    # To sort descending by z and ascending by b: str(~ -z + b)
	sort(dd, by = ~ -z + b)
	srt(dd,  ~ -z + b)
	srt(dd,   -z + b)
	sort(dd)
	debug(sort.data.frame)
	undebug(sort.data.frame)
	libra(tcltk)
	libra(debug)
	mtrace(sort.data.frame)
}



# The End.
if(0){    #..................................
	# www.zolot.us/cgi-bin/CTReg.r
	installed.packages()[,1]
	
	ls()
	setwd('S:\\52_InnoCentive\\4_ProbabilisticModelingSpendingHabits\\z')
	setwd('../')
	getwd()          #"S:/52_InnoCentive/FieldTrialDataQC/z"
	save.image(file = "all.Rdata") ;
	unlink("all.Rdata")
	save(sds,res, file = "S:/52_InnoCentive/FieldTrialDataQC/z/sds.res.Rdata");
	dput(sds, file = "S:/52_InnoCentive/FieldTrialDataQC/z/sds.txt")
	sds= dget("S:/52_InnoCentive/FieldTrialDataQC/z/sds.txt");  unlink("sds.txt")
	res= dget("S:/52_InnoCentive/FieldTrialDataQC/z/res2.txt");  unlink("res.txt")
	unlink("S:/52_InnoCentive/FieldTrialDataQC/z/res.txt")
	
	unlink("sds.res.Rdata")
	load(file = "all.Rdata")
	load(file = "sds.Rdata")
	load(file = "sds.res.Rdata")
	rm(ds,dss,meuse)
	rm(list = ls())
	spectr(sds)
	environment()
	slotNames(wrld_simpl)
	slotNames(wrld_simpl@data)
	Sys.getenv()
	commandArgs()
	traceback()
	
	source('http://rbenchmark.googlecode.com/svn/trunk/benchmark.r')
	benchmark({ y_ty0 = aggregate(sds$y, list(EN=sds$Entry_Name), mean)
				pred2= sqldf("select sds.*, y_ty0.x as avg_y   \
								from sds left join y_ty0                  \
								on sds.Entry_Name = y_ty0.EN")
			}, replications = 1)
	# or
	catt(1622, system.time({
						x=rnorm(100000)
					}))
	
    s= readLines("./SpendHabit.r")
    #for(i in grep('^[^#]*(= func|<- func)', s))catf('# %4s %s\n', i, (s[i]))
    for(i in grep('^[^#]*(= func|<- func|#.*===|##)', s))catf('# %4s %s\n', i, (s[i]))
    
    s= readLines("./SpendHabit.r")
    #for(i in grep('^[^#]*(= func|<- func)', s))catf('# %4s %s\n', i, (s[i]))
    for(i in grep('^[^#]*(= func|<- func|#.*===|##)', s))catf('# %4s %s\n', i, (s[i]))
    
    
	
	
	#sorted list of functions
	s= readLines("m:/zmFunctions.r")
	i= grep('^[^#]*(= func|<- func)',s)
	#out=data.frame(i=i,s=s[i])
	out= data.frame(i=i,s=gsub('^ +','',s[i]))  #ltrim
	outs= (out[order(out[,2]),]) # strtrim
	writeLines(sf('# %4s %s',outs[,1],outs[,2]))
	
	gtf('= function','m:/zmFunctions.r')
	gtf('^[^#]*(= func|<- func)','m:/zmFunctions.r')
	
	
	
	
	#out=data.frame(i=i,s=s[i])
	out= data.frame(i=i,s=gsub('^ +','',s[i]))  #ltrim
	outs= (out[order(out[,2]),]) # strtrim
	writeLines(sf('# %4s %s',outs[,1],outs[,2]))
	
	
	
	theFile= 'M:/93_AegisPubScore/93_Aeg_PubSc-Report.r'
	
	
	system('python E:/arc/exe/txt2tags-2.5/ztxt2tags.py -t html --toc S:/52_InnoCentive/4_ProbabilisticModelingSpendingHabits/z/SpendHabit.r ')
	system('python E:/arc/exe/asciidoc-8.5.1/zasciidoc.py -a toc S:/52_InnoCentive/4_ProbabilisticModelingSpendingHabits/z/SpendHabit.r ')
	system('python E:/arc/exe/asciidoc-8.5.1/zasciidoc.py  S:/52_InnoCentive/4_ProbabilisticModelingSpendingHabits/z/SpendHabit.r ', ignore.stderr = T)
	system('python E:/arc/exe/asciidoc-8.5.1/asciidoc.py  S:/52_InnoCentive/4_ProbabilisticModelingSpendingHabits/z/SpendHabit.r.azz ')
	
}


nope= function(){
	
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
	calcc = function(doit,dat='zz'){if(calcs){catt(calcs,dat, dat %in% ls());doit} };       #    !exists(dat)     'SpeTestInd' %in% ls()
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
	
	
	analQuantiles= function(y,wt){
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
	
	
	ttt<- textToTable <- function(text, ...)
	{
		dfr <- read.table(tc <- textConnection(text), ...)
		close(tc)
		dfr
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
	
	
	
}# nope

iPic= 1 # TODO: clean HH() to get rid of this
