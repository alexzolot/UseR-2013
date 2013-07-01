# svn: https://subversion.assembla.com/svn/azsvn/
# https://code/svn/medio/analytics/trunk/src/main/R/zBase.r  ticket: #MED-158
# https://code/svn/medio/analytics/m3/datascience/iusv2/trunk/src/main/R/zBase.r
# License  : GPL-2  


#=====  Funcs for Tutorial UseR!-2013  =====
# xxx : saa, sa() -> sa()

#====  aliases and 1-liners  ====
ch= as.character
fa= factor
nu= as.numeric
df= data.frame

le= length
he= head
sus= subset
summ= summary


cn= function(cnn,  sep='[ ,\n\t\\+]+') unlist(strsplit(cnn, sep))  # b='',`%a%` = '%c%' =  col.names= cn(cnn)  'aa b 1'  %a% 1  aa b 1'  cn('aa b 1')
# cn('aa bb')

na= names
dummy= function(...) invisible()
nc= function(ds) pas(na(ds))
ncc= function(patt = "", pattNeg = "^0z", ds) pas(gna(patt, pattNeg, ds))
nc= nmsv= sNames= function(ds, sep=' ')paste(names(ds), sep=' ', collapse=sep) #inverse to cn()
pr= function(x){catf('== %s =',deparse(substitute(x))); print(x)}
prr= function(x, ma='') {catf('\n&&& %s == %s ==\n', ma, deparse(substitute(x))); for(xx in x) catt(xx); catt('-------------------------\n')}

pas= function(x,sep=' ',collapse=' ')paste(x,sep=sep,collapse=collapse)
suss= function(patt='', pattNeg='^$', ...) {x<-subset(...); x[,grepl(patt, colnames(x)) & !grepl(pattNeg, colnames(x))]} # ex: suss('Se', 'Wi',  iris, Sepal.Length < 5 )
dett= function(patt= '^pac') catf('\ndetach("%s",  character.only = TRUE)', grep(patt, search(), v=T))  ## detach redundant packages

st= function(...){s= system.time(...)[[3]]; aaa(); s}
nuf= function(...) nu(fa(...))  # char -> fa -> nu

onWin= nchar(Sys.getenv('computername')) > 0
ww= if(onWin) windows else x11

brr= browser  #function(s='', ...){catt(s); browser(...)}  # 
trb= traceback
fp= file.path

DT= DateTime= timestamp= function(format = "%Y-%m-%d %H:%M:%S") strftime(Sys.time(), format) #ex: timestamp(); DT()

hee= function(ds, h=9){catt(NROW(ds),' rows'); print(hh<-head(ds,h)); catf('# he(suss(,, %s, , sel= cn("%s")), 5)\n',deparse(substitute(ds)),nmsv(ds));invisible(hh)}


aaa= function(n=20) for(i in 1:n) {cat('\aaa '); flush.console()}  # sound when done 

sf= sprintf
catf= function(...) cat(sprintf(...))
catt= function(...) {cat(...); cat('\n'); invisible(flush.console())} # catt(cars[1])

'%+%' = function(x, y) paste(x, y, sep= "")

exec= function(s) shell(s, wait=T, intern = T)
expl= function(x= gw()) shell(sf('start explorer %s', gsub('/','\\\\', x)))


gw= function(){catf('gw: sw("%s")\n', gw<- getwd()); invisible(gw)} # gw()

sw= function (sDir) {
	dir.create(sDir, rec=T)
	setwd(sDir)
	catf('sw: Work dir set to: %s;  gw()\n', gw())
}


#saa= function(ds='', file= sf('%s.RData', deparse(substitute(ds)))  ,...){catt(dsn); if(ds=='') save.image(file=file) else save(ds, file=file); 
saa= function(ds, file= deparse(substitute(ds)) ,...){file= sf('%s.RData', file)
		save( list = if(missing(ds)) ls(all=TRUE) else  deparse(substitute(ds)), file=file); 
		catf("%s:: Saved: load('%s/%s'); sw('%2$s')", Sys.time(), getwd(), file); if(missing(ds)) catt('  # rmDF(); lsDF(); dir(); expl()')} 
#	saa(,'111')
## 2013-06-18 12:15:31:: Saved: load('m:/80_ChurnSim/out/111.RData'); sw('m:/80_ChurnSim/out')  # rmDF(); lsDF(); dir(); expl()
#	
#	saa()
## 2013-06-18 12:15:38:: Saved: load('m:/80_ChurnSim/out/.RData'); sw('m:/80_ChurnSim/out')  # rmDF(); lsDF(); dir(); expl()
#	
#	saa(users3)
## 2013-06-18 12:15:46:: Saved: load('m:/80_ChurnSim/out/users3.RData'); sw('m:/80_ChurnSim/out')
	
	
sa= function(file=''){save.image(file=sf('%s.RData', file)); catf("%s:: Image saved: lo('%s/%s.RData'); sw('%2$s')  # rmDF(); lsDF(); dir(); expl()\n", Sys.time(), gw(), file)}
lo= function(file='.RData'){load(file=file, .GlobalEnv); lss(); cat('\a\a\a'); alarm();} # expl("C:/Users/Public/Music/Sample Music/Kalimba.mp3")} # expl("Z:/exe/testVoice.vbs")}
loo= function(patt='.RData'){sw('../out'); gw(); srt(ldply(dir(patt=patt, all.files =T), function(f) c(mtime=ch(file.info(f)$mtime), size=round(file.info(f)$size/1e6), lo=sf('lo("%s")',f))), ~mtime)}

nin= '%-%' = function(x, y) x[!(x %in% y)] # x not in y 1:5 %-%  4:9 # `%-%` 

# returns matrix of memory consumption
lss<-  function(verb=T){ #object.sizes <-
	#llss= rev(sort(sapply(ls(envir=.GlobalEnv), function (object.name)
#	llss= (sort(sapply(ls(envir=.GlobalEnv), function (object.name)
#									round(object.size(get(object.name))/1048576,2)) ))
	
	llss= (sort(sapply(ls(envir=.GlobalEnv), function (object.name){x=1e-11
									try({x=round(object.size(get(object.name))/1048576, 2) },s=T); x
								})))
	
	
	#if(verb)newwin(1,3,'Mem',{
	if(verb)try({
					#barplot(llss, main="Memory usage by object", ylab="Bytes", xlab="Variable name",
					#col=heat.colors(length(object.sizes())))
					#dotchart(llss, main="Memory usage by object, in M", xlab="Bytes")
					pie(llss, labels = paste(names(llss), llss) , main="Memory usage by object, of tot " %+% me()) #round(llss/1e6,2))
				}, s=T)
	print(llss[1])    #print(llss)     
	do.call(str,list(get(names(llss)[1]) ))
	pr(llss)
	catf(':: rm(%s)\n', pas(rev(names(llss)), collapse=','))
	invisible(llss)
}
# x=lss(); x[x== 1e-11]


# list of data.frames == aa=1:9
lsDF= function(...){ b= mdply(ls(envir= .GlobalEnv, ... ), function(a){catt(a); aa=get(a); b=df(); cl<- substr(class(aa)[1],1,4)
				if(any(class(aa) %in% c('data.frame','matrix','list',"metaMDS", "itemsets", "rules", "transactions") | is.list(aa))){
					catf('rm( %-23s ) # %5.1f %5s %5s %3s %-30s\n', a, round(object.size(aa)/1e6, 1), cl, NROW(aa), NCOL(aa), substr(sNames(aa),1,1999))
					b= df(ds=sf('rm(%17s)',a), size=nu(object.size(aa))/1e6, class=cl, nr=NROW(aa), nc=NCOL(aa)
							, vars=substr(sNames(aa),1,99), ds0=a)[1,]
				}
			})
	if(nrow(b)>0){ print(hee(bs<- srt(b, ~-size)[,-1], 20), width=1600)
		catf('rm(%s)\n', pas(b$ds0,collapse=', '))
		catf('rm(%s)\n', pas(bs$ds0,collapse=', '))
		catf('save(%s\n, file="%s")\n', pas(b$ds,collapse=', '), gw())
	}
	
	gc(T,T)
	invisible(b)	
}
#a= lsDF(); a

rmall= function()rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv) # rmall()


rmDF= function(...) invisible(mdply(ls(envir =.GlobalEnv,... ),function(a){aa= get(a); b= df()
						if( class(aa)[1] %in% c('list')){catf('rm  %-20s %-30s\n', a, sNames(aa)); do.call(rm, list(a), envir =.GlobalEnv)}
						if( class(aa)[1] %in% c('data.frame','matrix')){catf('rm  %-20s %5s %3s %-30s\n', a, nrow(aa), ncol(aa), substr(sNames(aa),1,4999))
							do.call(rm, list(a), envir= .GlobalEnv)
						}}))

libra= function(libs, verb=TRUE){
	libs= if(!grepl('\\"|\'| ', li<- deparse(substitute(libs)))) li else cn(libs)  # now libs is a char vector

	misspk= nin(libs, installed.packages()[,1])
	
	if(le(misspk) > 0) {install.packages(misspk, repos= "http://cran.stat.ucla.edu/", dependencies= T)}
	
	for(li in libs){catt('libra:: Call library ', li) 
		do.call(library, list(li, character.only =T))
		if(verb) catf('libra:: demo(%s); example(%1$s); vignette("%1$s")\n', li)
	}
} #--

if(0){
	libra(libs= "locfit tkrplot xtable")
	libra(cn("randomForest varSelRF"), verb=T)
	libra("randomForest varSelRF", verb=T)
	libra(randomForest, verb=T)
	
	.libPaths()
	library()
	pas(dir(.libPaths()))
	# [1] "abind akima bak base bitops boot brew Cairo caTools class cluster codetools colorspace datasets DBI Defaults devtools dichromat digest evaluate fImport foreach forecast formatR fracdiff gam gdata ggplot2 gmodels gplots graphics grDevices grid gtable gtools httpuv httr iterators itertools KernSmooth knitr labeling lattice lme4 locfit markdown MASS Matrix memoise methods mgcv munsell nlme nnet plyr png proto quadprog R2HTML randomForest rCharts RColorBrewer Rcpp RcppArmadillo RCurl reshape2 rj rj.gd rJava RJDBC RJSONIO roxygen2 rpart RUnit scales shiny stats stats4 stringr survival testthat timeDate timeSeries tkrplot tools tseries utils whisker XLConnect xtable yaml zoo"

	pas(options('defaultPackages')[[1]])
	# [1] "datasets utils grDevices graphics stats methods"
}


tocsv= function(ds, dsn= sf('%s.csv', deparse(substitute(ds))), ...){catt(dsn); write.csv(ds, file=dsn, quote=F, row.names= F); 
	catf("Saved: %s= read.csv('%s/%s'); expl('%2$s/%3$s')\n ",deparse(substitute(ds)), gw(), dsn)} 
totsv= function(ds, dsn= sf('%s.tsv', deparse(substitute(ds))), ...){write.table(ds, file=dsn, quote=F, row.names= F, sep='\t', ...); 
	catf("Saved: %s= read.delim('%s/%s'); expl('%2$s/%3$s')\n ", deparse(substitute(ds)), gw(), dsn)} 



srt= sortt= sort.data.frame= function(x, by){
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

# hee(srt(CO2, ~conc - uptake))


