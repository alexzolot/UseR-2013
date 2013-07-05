# Author   : Alex Zolotoviski, azolotovitski@medio.com
# Created  : 2013-06-24 02:55:55
# License  : GPL-2  

#=====  Funcs for Tutorial UseR!-2013  =====
#=====  Funcs for data ansalysis, Descr Statistics and Predictive Modeling  =====

require(zBase0)

plotGLMCoeff= function(glm1, maxPVal= .082, nmax=12, obfusc=F, cox=F,...) {#==  
	co= summ(glm1)$ coefficients
	if(cox) co=co[,-2]
	or= order(-abs(co[,3]))
	co= co[or,][1:min(nrow(co), nmax), ]
	co= co[co[,4]< maxPVal,]
	n= nrow(co)
	set.seed(555)
	if(obfusc)rownames(co)='v.' %+% substr(rownames(co),1,1) %+% sample(1:999, nrow(co))
	z= 1.96
	catt('ylim:', range(c(co[,3]- z, co[,3]+ z)))
	plot(1:n, co[,3], xlim=c(.8, nrow(co)+ .9), ylim=range(c(co[,3]- z, co[,3]+ z)), xaxt='n'
			, pch=20, cex=1.7, xlab='', ylab='Coefficients (t-normalized)'
			, ma=sf('Coefficients of Regression\n %s', pas(ch(glm1$call))))
	text(1:n +.05, co[,3]+(-1)^ (1:nrow(co)), gsub('\\`','', rownames(co)), adj=0, cex=.9, col= 2+1:nrow(co))
	segments(1:n, co[,3]- z, , co[,3] + z, lty=2, lwd=2)
	abline(h=0, col=1)
	legend('bottom', '95% Conf Int',  lty=2)
	co
}#--
#plotGLMCoeff(glm1= lm1, maxPVal= .082, nmax=12, obf=T)

#libra(RJDBC)     # for hive_conn

hive_conn= function(hostname= 'localhost', port=10001, lib_dir='/usr/local/churn/lib' ){
	hive_class_path= fp(lib_dir, dir(lib_dir))
	drv= JDBC( 'org.apache.hadoop.hive.jdbc.HiveDriver', classPath= hive_class_path, "`" )
	server= sf( "jdbc:hive://%s:%s/default", hostname, port)
	conn = dbConnect( drv, server )
	invisible(conn)
}

hSel= function(.conn=conn, SE='SELECT', sel, ... ){  ## hive Select
	try({.conn<<- conn<<- hive_conn('dlhive01.cloud.msrch', port = 10000, lib_dir='M:/77_ChurnProd/lib')
			    dbSendQuery(.conn, 'Use gree' )
			}, s=T)
	catt('hSel:', sf('%s %s', SE, sf(sel, ...)))
	dbGetQuery(.conn, sf('%s %s', SE, sf(sel, ...)))
} 

hSelC= function(.conn=conn, SE='SELECT', sel, ... ){  ## hive Select
	try({.conn<<- conn<<- hive_conn('dlhive01.cloud.msrch', port = 10000, lib_dir='M:/77_ChurnProd/lib')
			    dbSendQuery(.conn, 'Use churn' )
				dbSendQuery(conn, 'Add Jar /home/ykeselman/Hive/lib/csv-serde.jar' )
			}, s=T)
	catt('hSel:', sf('%s %s', SE, sf(sel, ...)))
	dbGetQuery(.conn, sf('%s %s', SE, sf(sel, ...)))
} 

hist.pan= function(.br=50)function(x, ...) {
		usr <- par("usr"); on.exit(par(usr))
		par(usr = c(usr[1:2], 0, 1.5) )
		h <- hist(x, plot = FALSE, br= .br)
		breaks <- h$breaks; nB <- length(breaks)
		y <- h$counts; y <- y/max(y)
		rect(breaks[-nB], 0, breaks[-1], y, col="cyan")
	}

sc.pan= function(onescale=F) function(x, y, ...) {#nx= nlevels(x); ny= nlevels(y)
		nx= le(unique(x)); ny= le(unique(y))
		if(nx < 9 && ny >9) boxplot(y~x, varwidth =T, add =T,...)
		else if(nx > 9 && ny < 9) boxplot(x~y, varwidth =T, horizontal=T, add =T,...)
		else if(nx < 9 && ny < 9) boxplot(jitter(nu(y))~x, varwidth =T, add =T,...)
		else if(onescale) points(y~x, xlim=0:1, ylim=0:1, ...) else points(y~x,...)
	}
if(0){
	ww(12,11)
	pairs(gu3[,-c(1:4)], col= topo.colors(8)[1+ gu3$tot_churn_status]  #, col= topo.colors(10)[1+ 8 * norm0(gu3$tot_churn_status)]
			, diag.panel= hist.pan, lower.panel= sc.pan(), upper.panel= sc.pan())	
	mtext('Pairs of ... by ...', 3, 3)
	HHp('...', , F)
}


hglm= function(.glm, n=7){
	or= order(-abs(summ(.glm) $ coefficients [, 3]))
	hee( summ(.glm) $ coefficients[or,], n)  # for lm, , family = gaussian
} 



RF.Importance= RFI= function(rf= res.pred.idSeas$rf.id.seas.Slope, nPlot= 20, ma2='', cex.names=1.2,...) {
	rfi<- rf$importance; rfi= rfi[or<- order(- rfi),]; hee(rfi, 200)
	if(nPlot > 0){
		ww(wi=13, he=12)
		op=par(mar=c(4,20,3,1))
		barplot(head(rfi, nPlot), horiz = T, las=2, cex.names=cex.names, main="RF, Importance" %+% ma2,... ); par(op)
	}
	invisible(df(i=1:NROW(rfi), var=names(rfi), rfi))
}


ext2= function(x) {tb= table(ch(x), useNA = "always"); if(le(tb)==0) return(0) # all NA
	p= 1.0 * tb/sum(tb); 1/sum(p^2)}

spectrum= function (ds, nValMax = 20, longestvalCut= 25, getext2P= T, id='mobile', vAmt='PRICE', toUpp=F) # ds = da; nValMax = 5
{
	res = list()
	cat("nrow=", nrow(ds), "\n")
	
	res$tb= d= ldply(ds,  function(x){ #cat(deparse(substitute(x))); 
				df(class= class(x)[1] #, type=typeof(x) #
						, Missing= sum(is.na(x)), notMissing= sum(!is.na(x)), not0= sum(x != 0, na.rm=T), nValues= le(unique(x)), ext2= ext2(x)
						, ext2P= -1
						, shortCateg = (le(unique(x)) <= nValMax), maxNchar= max(nchar(ch(x)))
						, longestval= substr(ch(x[which.max(nchar(ch(x)))]),1,longestvalCut)
						, { if(class(x)[1] %in% c('character','factor', 'POSIXlt', 'Date'))  t(NA*summary(-777)[1:6]) else   if(class(x)[1] =='Date')  (summary(x)[1:6]) else  t(summary(x)[1:6])}
				)})
#	str(res$tb)
#	unique(res$tb)
#	le(duplicated(res$tb))
#	res$tb[!duplicated(res$tb),]
	
	if(toUpp) names(ds)= toupper(names(ds))
	if(toupper(vAmt) %in% na(ds)){
		for (j in 1:ncol(ds)) { # catt(j, na(ds)[j])
			x= ds[,j]
			tb= aggregate(ds[, vAmt], list(a=ch(x)), function(y){amt=sum(.000001+y, na.rm=T); amt} )[,2]; p= 1.0 * tb / sum(tb) 
			res$tb$ext2P[j]= 1/sum(p^2)
		}
	} else res$tb$ext2P= NULL
	
	print(res$tb)
	
	for (j in na(ds[,d$shortCateg])) {
		cat('\n', j)
		print(table(ds[, j], useNA = "always"), exclude = NULL)
		res$shortCateg[[j]] = (table(ds[, j], useNA = "always"))
	}
	
	invisible(res)
	
	# example: spectrum(mtcars)
}

#ldply(tr2,   function(x){ df(class= class(x)[1])})
#t(summary(tr2$DT0)[1:6][1])
#str(summary(tr2$DT0)[1:6])
#str(class(tr2$DT0)[1:6])


CategFreqDesc= function(x, n= 9, threshOther= .05, verb=F) {tb=df(table(x, useNA ='always'))
	#tb$FreqNorm= norm(tb$Freq)
	tb$p= tb$Freq/sumn(tb$Freq)
	tb$`p,%`= tb$p * 100.
	tb= srt(tb, ~-Freq)
	# brr()
	if(threshOther==0) threshOther= tb$p[min(n, nrow(tb))]
	
	#tb$isOther= with(tb, p < threshOther)
	#oth= sus(tb, p < threshOther)$x
	# xwOther=ifelse(x %in% oth, 'other', ch(x))
	#brr()
	tb$isOther= with(tb, p < threshOther | 1:nrow(tb) > n)  # DT()	# [1] "2013-05-16 12:53:44"
	xwOther= ifelse(x %in% sus(tb, isOther)$x, 'other', ch(x))
	
	xwOther[is.na(xwOther)]='NA'
	if(n>0) print(head(srt(tb, ~-Freq),n))
	res= list(tb=tb, xwOther=xwOther, nVal=nrow(tb), ext2=ext2(x))
	if(verb) str(res)
	invisible(res)
}
#example: (cf= CategFreqDesc(cars$dist, n= 9, threshOther= .05))


AllCategFreqDesc= function(ds= tr3[, head(sus(sp3$tb, class=='character')$.id, 5)], n= 9) {
	m= min(n, nrow(ds))
	res= df(matrix('=', m+1, ncol(ds)))
	names(res)= na(ds)
	
	for(j in 1:ncol(ds)){ # j=1
		catt(j, na(ds)[j])
		xx= CategFreqDesc(ds[,j], 0, threshOther=0)
		x= xx$tb[1:m,]
		
		res[, j]= c(sf('%7s :: %5.1f', xx$nVal, xx$ext2), sf('%7.1f %-10s', 100*x$p, substr(x$x,1,95)))
	}
	print(format(res, justify = "left"))
	invisible(res)
}
#example:  AllCategFreqDesc(ds= df(ChickWeight), n= 5)  AllCategFreqDesc(ds= ChickWeight, n= 5)  str(ChickWeight)  # TODO: check



dropp = function(ds, vars) ds[, !(colnames(ds) %in% vars)]
#example:  x= dropp(iris, cn('Sepal.Length Petal.Width')); str(x)


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



BA= BayesAvg= function(x, y, mn=sum(nu(x))/sum(nu(y)), a=1) (nu(x)+a)/(nu(y)+a/mn)  ## http://en.wikipedia.org/wiki/Bayesian_average


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
            if(varsign == "-") { calllist[[i]] <- -rank(x[, vars[i]])
            } else {             calllist[[i]] <-  rank(x[, vars[i]])
            }
        } else {
            if(varsign == "-") { calllist[[i]] <- -x[, vars[i]]
            } else {             calllist[[i]] <-  x[,vars[i]]
            }
        }
    }
    return(x[do.call("order", calllist), ])
}

if(0){ #== test srt()
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


#==  Cross-Validation  ===
CVwt = function(y, yhat, wt= 1+ 0*y){   #======================================= CV ====
#	ym=sum(y*wt, na.rm=T) / sum(wt, na.rm=T)
#	return (sum(wt*(y-yhat)^2, na.rm=T) / sum(wt*(y-ym)^2,na.rm=T)  )
	ym=sumn(y*wt) / sumn(wt)
	return (sumn(wt*(y-yhat)^2) / sumn(wt*(y-ym)^2)  )
}


#===  Lift and ROC curves  ===
LiftWtArr3= function(yhat, y= yhat[,1], ...)LiftWtArr(y, yhat,...)
LiftWtArr2= function(yhat, y= yhat$y, ...)LiftWtArr(y, yhat,...)


LiftWtArr= function(y, yhat, wt= 0*y + 1, costFN=10, nPlot= le(y), PlotXrel=F,
		clr=rep(c('red','brown','green','blue','violet','black'), 4),
		pch= rep(1:25,2),
		pmax=100, ymax=100, kCostPlot=3, xlim=c(0,1.00), ylim=c(0,1.00), plotAll=F, wCairo=T, wPoints=T, ...){
	
	nxTot=length(y)
	#nx=length(y.sort); print(nx); nx1=nx+1
	
	nx=length(y); print(nx); nx1=nx+1
	
	y=nu(ch(y))  # for factors
	
	ory=order(-y, na.last = NA); y.sort=y[ory] ;
	wty.s=wt[ory];									# normalized:
	Positive= wty.cum=c(0, cumsum(wty.s));        wty.ncum=1.00 * wty.cum/wty.cum[nx+1]
	TP= y.cum=c(0,cumsum(y.sort*wty.s));         y.ncum=1.00 * y.cum/y.cum[nx+1]
	FP= c(0,cumsum((1-y.sort)*wty.s));
	
	# Lift: TP vs Pos; ROC:  TP vs FP
	
	#subset to plot
	p=1:nx1/nx1
	#LiftArea=cumsum(c(0,wty.s)*abs(y.ncum-wty.ncum)/100)[nx+1]/(nx) # wromg? no wght
	LiftAreaY=cumsum(c(0, wty.s) * (y.ncum-wty.ncum)/100)[nx1]/wty.cum[nx]
	
	# xp=round((1:nPlot)*(nx+1)/nPlot)
	#xp=floor((1:nPlot)*(nx+1)/nPlot)
	xp=c(1,floor((1:nPlot)*(nx+1)/nPlot))
	
	if(wCairo){
		libra(Cairo)
		CairoWin(width = 15, height = 10)
	} else windows()
#	plot(wty.ncum[xp], y.ncum[xp],col='blue',pch=1,type="l", lty=1,
#			xlab=paste('% rank ', deparse(substitute(yhat))),
#			ylab=paste('% cum', deparse(substitute(y))),
#			xlim=c(0,pmax), ylim=c(0,ymax),
#			main=paste("Lift ", deparse(substitute(yhat))), #' [ CV=',cv,']' ),
#			...)
	
	if(PlotXrel=='zzz'){xplot= wt.ncum[xp]
		print(xplot)
		print(y.ncum[xp])
		print( yhat.ncum [xp])
	}	
	
	plot(wty.ncum[xp], y.ncum[xp],col='blue',pch=1,type="l", lty=1,
			#xlab=paste('% rank ', deparse(substitute(yhat))),
			#ylab=paste('% cum', deparse(substitute(y))),
			#xlim=c(0,pmax/15), ylim=c(0,ymax/4),
			#xlim=c(0,pmax), ylim=c(0,ymax),
			, xlim=xlim, ylim= ylim, xlab='Positive, %', ylab= ' normalized y.cum  (TP), %'
			, main=paste("Lift ", deparse(substitute(yhat))), #' [ CV=',cv,']' ),
			...)
	abline(h=0:5 /5, v=0:5 /5, col='grey89')
	
	j=0
	LiftArea=NULL; LiftAreaRel=NULL; cv=NULL; cost=data.frame(zz=0*y.cum); ROC=list()
	
	for (i in names(yhat)){j=j+1;cat("\np2.",j,i, "\n");
		#or=order(-yhat[,i], -y, na.last = NA); #print(or);
		
	    yhat[,i]=nu(ch(yhat[,i]))  # for factors
		
		or=order(-yhat[,i], rnorm(nrow(yhat)), na.last = NA); #print(or);
		y.sort=y[or] ; #print(cbind( y.sort,yhat.sort))
		yhat.sort=yhat[or,i];
		wt.s=wt[or];
		
		Positive= wt.cum= c(0, cumsum(wt.s));              wt.ncum= 1.00 * wt.cum/wt.cum[nx+1]
		TP= y.s.cum= c(0, cumsum(y.sort*wt.s));            y.s.ncum= 1.00 * y.s.cum/y.s.cum[nx+1]
		FP= c(0, cumsum((1-y.sort)*wty.s));
		
		ROC[[i]]= df(FPR= FP/FP[nx+1], TPR= TP/TP[nx+1])
		
		#  http://en.wikipedia.org/wiki/Receiver_operating_characteristic  ==========================
		# Lift: TP vs Pos; ROC:  TP vs FP
		# ROC curve: plot of the Sensitivity, or true positive rate,  Sens= TPR , Spec=TNR =  1 - FPR
		# vs. false positive rate (1 − Specificity or 1 − true negative rate)
		#   Presicion = TP/Posit , Recall = TP/Bad = TP/(TP+FN)
		#=============================================================================================
		
		
		LiftArea[i]= cumsum(c(0,wt.s)*(y.s.ncum-wt.ncum)/100)[nx1]/wt.cum[nx]
		LiftAreaRel[i]= LiftArea[i]/LiftAreaY
		# cat("p3.",j,i,  LiftArea[i],  LiftAreaRel[i])
		cv[i]=CVwt(y,yhat[,i],wt); # =signif(CVwt(y,yhat[,i],wt),4);
		
		
		#lines(wty.ncum[xp], y.s.ncum[xp], lty=3, col=clr[j], pch=18+j ,type="o")
		lines(wty.ncum[xp], y.s.ncum[xp], lty=3, col=clr[j], pch=pch[j], type="l",...)
		if(wPoints) lines(wty.ncum[sm<- sample(xp,20)], y.s.ncum[sm], lty=3, col=clr[j], pch=pch[j], type="p",...) 
		cost[i]= wt.cum + sum(costFN*wt*y) -  c(0,cumsum((1+costFN)*wt.s*y.sort)) # (1+costFN)*y.s.cum
	}
	abline(0,1,col='darkgreen',lty=2)
	points(c(0,100), c(0,100), pch = 3, cex = 4, col = "red")
	
	or=order(-LiftArea);
	#legend('bottomright', names(yhat)[or], lwd=3, lty=1, col=clr[or], pch=18+or, bty='n')
	legend('bottomright', names(yhat)[or], lwd=3, lty=1, col=clr[or], pch=pch[or], bty='n')
	
	
	if(plotAll){  
		if(0){#cost plot ======
			cost$zz=NULL
			y.min= min(cost)
			x.min= which.min(unlist(cost)) %% nrow(cost)
			
			windows() # Cost =====
			or=order(-apply(cost,2,min));
			#matplot(cost[,or], cex=.4, xlim=x.min*c(.5, 2), ylim= y.min*c(1/kCostPlot, kCostPlot), col=clr[or], pch=pch[or])
			#matplot(wt.cum, cost[,or], cex=.4, xlim=wt.cum[x.min*c(.5, 2)], ylim= y.min*c(1/kCostPlot, kCostPlot), col=clr[or], pch=pch[or])
			matplot(wt.cum/sum(wt), cost[,or], cex=.4, xlim=xlim,   xlab='Positive, %', ylab= 'Cost'
					, ylim= range(cost[xlim[1]<=wt.cum/sum(wt) & xlim[2]>=wt.cum/sum(wt),  or])
					, col=clr[or], pch=pch[or], main='Cost')
			legend('bottomright', names(cost)[or], lwd=3, lty=1, col=clr[or], pch=pch[or], bty='n')
		}
		
		
		windows()  # ROC =====
		or=order(-LiftArea);
		
		plot(0:1, 0:1, type='l', lty=2, main='ROC', xlim=xlim, ylim=ylim, xlab='FalsePositiveRate', ylab='TruePositiveRate', col='grey75')
		#ab(col='grey75')
		
		#for (i in names(yhat))lines(ROC[[i]], col=clr[1:le(ROC)])
		#palette(rep(pal8,3))
		for (i in or) lines(ROC[[i]], col=i, pch=pch[i], lwd=2,type='l')
		if(wPoints) {
			for (i in or)lines(ROC[[i]][(sample(1:nx,20)),], col=i, pch=pch[i], lwd=2,type='p')
			#legend('bottomright', names(yhat), col=1:le(ROC), lwd=2)
			legend('bottomright', names(yhat)[or], lwd=3, lty=1, col=or, pch=pch[or], bty='n')
		}else{legend('bottomright', names(yhat)[or], lwd=3, lty=1, col=or,  bty='n')}
	}  
	
	return(z=list(lift=data.frame(LiftArea, LiftAreaRel, cv, gini= LiftAreaRel, auc= .5*(LiftAreaRel + 1)),
					LiftAreaY=LiftAreaY, cost=cost, ROC=ROC))
	
}


#gna= function(patt='', pattNeg='^0z', x) names(x)[grepl(patt, names(x)) & !grepl(pattNeg, names(x))]  # ex: gna('Se', 'Wi', iris)
gna= function(patt='', pattNeg='^0z', x) colnames(x)[grepl(patt, colnames(x)) & !grepl(pattNeg, colnames(x))]  # ex: gna('Se', 'Wi', iris)
gnal=lgna= function(patt='', pattNeg='^0z', x) grepl(patt, names(x)) & !grepl(pattNeg, names(x))  # ex: lgna('Se', 'Wi', iris)
susp= function(ds, patt, pattNeg) ds[grepl(patt, na(ds)) & !grepl(pattNeg, na(ds))]  # ex: susp( iris, 'Se', 'Wi')  # to delete ??
suss= function(patt='', pattNeg='^0z$', ...) {x= subset(...); x[, grepl(patt, colnames(x)) & !grepl(pattNeg, colnames(x))]} # ex: suss('Se', 'Wi',  iris, Sepal.Length < 5 )
hees= function(...)hee(suss(...))
cumsumrev= function(x) cumsum(rev(x)) #ex: cumsum(rev(1:5))

lg2=  function(x) log2(1+x)
cumsumn=  function(x) cumsum(x) / sumn(x)
cumsumn1=  function(x) cumsum(x * 0 +1) / sumn(x*0+1)


plot1y.grid= function(da, xx=names(da), y='y', Cairo=F, f=I, ma='', orderCateg=T, manyPics=F, imgRoot='../imgManyPics'){nx=le(xx); n1= floor(sqrt(nx)); n2= ceiling(nx/n1)
	libra(grid)
	libra(gridBase)
	
	if(manyPics){n1= n2= 1}
	
	catt('plot1y.grid:',nx,n1,n2)
	
	.y= da[,y]; ny=2
	if(class(.y) %in% cn("character factor")) try({da[, y]= .y= nuf(.y) - 1; ny=le(unique(.y))}, s=F)
	#newwin(n1, n2, sf('Quantile plot 2, %s', deparse(substitute(f))) ,{
	res= list()
	
	(if(Cairo) wwc else ww) (24,12) # windows()
	par(mar=c(0,0,1,0), oma=c(0,0,1,0), no.readonly=TRUE)  #
	plot.new() 
	mtext(ma,,1)
	
	# init layout
	if(!manyPics)  pushViewport(viewport(layout= gl <- grid.layout(nrow=n1, ncol=n2), height =unit(.85, 'npc' )))   # grid.show.layout(gl)
	
	for(j1 in 1:nx ){ # j1=2
		if(manyPics)  (if(Cairo) wwc else ww) (24,12) # windows()
		#if(j1>3) stop()
		j= xx[j1]
		j1= j1-1
		.i= j1 %/% n2 # 9%/%4	-> 2
		
		.j= j1 - .i * n2
		# j= 'total_event'
		
		
		catt(j1, j, '    ', .i, .j, typeof(j), typeof(da[,j])) #, max(da[j])); 
		ks= list(statistic=0, p.value= 0)
		try({ks= ks.test(sus(da, .y==0)[,j], sus(da, .y==1)[,j])
					catt(ks$ statistic, ks$ p.value)
					#res[[j]]= list(ks= ks$ statistic[[1]])
					res[[j]][['ks']]= ks$ statistic[[1]]
				}) 
		
		
		if(!manyPics) vp1 <- viewport(layout.pos.col=.j+1, layout.pos.row=.i+1) 
		
		# access the first position
		if(!manyPics) pushViewport(vp1)
		
		# start new base graphics in first viewport
		if(!manyPics) fg= gridFIG() else fg=c(.1, .9, .1,.9)
		catt(.i,.j,'  |  ',  fg)
		catt('fg = ',  fg)
		par(new=TRUE, fig=fg, mar=c(2,2,2,1)+.1)
		
		#plot((1:9)^2, ma=sf('zzz %s %s', i,j))
		
		
		#if(typeof(da[,j])=="character"){try({plot(table(da[,j]), main=j)})
		if(class(da[,j]) %in% cn("character factor")){try({tb<- table(da[,c(j,y)])
						if(orderCateg) tb= tb[order(tb[,1]/(.001+tb[,2])),]
						
						plot(as.table(tb), main=j, col=2:3) 
						ch=chisq.test(tb)
						str(ch)
						res[[j]][['chisq']]= ch$p.value
						#legend('bottomleft', sf('chisq.P=%s ', ch$p.value))
						legend('bottomleft', eval(substitute(expression(P(chi^2)  == .ch ), list(.ch= ch$p.value))))
					})  # str(table(da[,c(j,y)]))
			#stop()
		}else{  # numeric
			try({
						with(x<- ord2(da[, c(y, j)],  sf('%s %s', j , y)), {
									#plot(jitter(f(x[,j])), col= x[,y]+2, main= '', xlab='', ylab='')
									
									plot((f(x[,j])), cumsumn1(x[, y]), col=1, lwd=2, ty='l')
									polygon(f(c(x[x[,y]==0, j][1],    x[x[,y]==0, j], rev(x[x[,y]==1, j]), x[x[,y]==1, j][1]))
										    , c(0, cumsumn1(x[x[,y]==0, y]), rev(cumsumn1(x[x[,y]==1, y])),0), col=rgb(0,0,0, .1))
									#points(jitter(f(x[,j])), cumsumn1(x[, y]), col=adjustcolor(palette()[x[,y]+2], .02))
									
									#lines(f(x[x[,y]==0, j]), cumsumn1(x[x[,y]==0, y]), col=2, lwd=3)
									#lines(f(x[x[,y]==1, j]), cumsumn1(x[x[,y]==1, y]), col=3, lwd=3)
									for(iy in 1:ny -1) lines(f(x[x[,y]==iy, j]), cumsumn1(x[x[,y]==iy, y]), col=2+iy, lwd=3)
									mtext(j,,1,cex=.9, font=2)
								})
						
						#legend('bottomright', sf('ks= %4.3f ', ks$ statistic))
						
						#==  pie plot insert  ==
						if(!manyPics){pushViewport(viewport(.78, .55, .2, .2))
							par(new = T, fig=  gridFIG(), mar=c(0,0,0,0))
	    				} else par(new = T, fig=  c(.8, .9, .2, .4), mar=c(0,0,3,0))
						pie(c(ks$ statistic, 1-ks$ statistic), labels ='', col=c('grey30', 'white'))
    					mtext(sf('ks= %4.3f ', ks$ statistic), 1)
						if(!manyPics)popViewport()
					})
		}		
		if(!manyPics)popViewport()
		if(manyPics) HHp(j, dirr =imgRoot, fNameWithCapt=T)
	}	
	popViewport()
	res
}

#plot1y.grid(da= first.day.412.wide.2[,-1], y='churned', Cairo=F, f=I, manyPics=T, imgRoot='../imgManyPics')
