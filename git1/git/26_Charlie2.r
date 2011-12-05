# /cygdrive/m/mob
#
#Alex Zolotovitski ? to me 
#data prepared by scripts collus6.script, collus6-cont.script

{ ## init ###################
	# parameters =====
	options(help='html', digits=4, width=220, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe')
	
#	        #source('m:/zmFunctions.r')
#	        setwd("M:/26_Charlie"); 
#			source('M:/zmFunctions.r')
#			  #load(file = ".Rdata")
			

	
	#functions ===
    library(zBase)
	libra(plyr) # TODO - incl dependence in zBase and REclipse
	libra(caret) # TODO add comment about libras
	
	
	sw('M:/26_Charlie/2') # alias sw();  '/' instead of '\'
 
	spectr= function (ds, nValMax = 20) # ds = da; nValMax = 5
	{
		res = list()
		cat("nrow=", nrow(ds), "\n")
		
		res$tb= d= ldply(ds,  function(x)df(class= class(x)
									, Missing= sum(is.na(x)), notMissing= sum(!is.na(x)), not0= sum(x != 0), nValues= le(unique(x))
									, shortCateg = (le(unique(x)) <= nValMax) 
									#, {print(summary(x)); if(class(x)!='character') t(summary(x)[1:6]) else rep(7,6)}
									, { if(class(x)!='character') t(summary(x)[1:6]) else  t(summary(-777)[1:6])}
				))
		print(d)
		
		for (j in na(ds[,d$shortCateg])) {
			cat('\n', j)
			print(table(ds[, j], useNA = "always"), exclude = NULL)
			res$shortCateg[[j]] = (table(ds[, j], useNA = "always"))
		}

		invisible(res)
		
		# example: spectr(mtcars)
	}

	newwin=  function(nrows= 2, ncols= 2, tytle= '', doit, pos=0, mar0= c(4,4,4,2)+.2, Cairo=1, ...){
		#windows(width=16, height=10, xpos=30+ 20* pos, ypos=30+ 20* pos, title= tytle)
		if(Cairo) {libra(Cairo); CairoWin(...)}
		else windows(xpos=30+ 20* pos, ypos=30+ 20* pos, title= tytle,...)
		
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
	}
	
	
	lsDF= function(...){ b= mdply(ls(envir =.GlobalEnv,... ),function(a){aa=get(a); b=df(); cl<- substr(class(aa)[1],1,2)
					if(any(class(aa) %in% c('data.frame','matrix','list',"metaMDS"))){
						catf('%-23s %2s %5s %3s %-30s\n', a, cl, NROW(aa), NCOL(aa),sNames(aa))
						b= df(ds=a, class=cl, nr=NROW(aa), nc=NCOL(aa), vars= sNames(aa))[1,]
					}
					
				})
		catf('rm(%s)\n', pas(b$ds,collapse=', '))
		catf('save(%s\n, file="%s")\n', pas(b$ds,collapse=', '), gw())
		invisible(b)	
	}
	
	palette(adjustcolor(cn('grey80 green3 red2'), alpha.f = .6))
	
}	
	
	# read data
	da = read.csv('in/PaymentFraud_NumericData.csv', header=T, stringsAsFactors =F)
	
	##==  Data Understanding  ==
	
	str(da)
	# 'data.frame':	5780 obs. of  48 variables:
	
	
	hee(da)
	# he(sus(da, , sel= cn("isBad ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt AccountId ScoreUtcDate AggregationStatusId BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign isCatched")), 5)
	xx= cn("ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt AccountId 
		AggregationStatusId BudgetTypeChanged PaymentInstrChanged DisplayURLChanged 
		KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay 
		MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay 
		MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt 
		SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt 
		SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay 
		SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay 
		SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords 
		KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory 
		KwdMaxPercentInNewCategoryPerCampaign")

	
	
	sp= spectr(da)
	
	# to drop:
	#	AccountId -?
	#	AggregationStatusId - no variance 
	#   ScoreUtcDate - date - 1 week

	tab(da$ScoreUtcDate)
	# 11/20/2011 11/21/2011 11/22/2011 11/23/2011 11/24/2011 11/25/2011 11/26/2011 
	#        753        944        990        940        747        699        707 


	xx2= cn("ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign")
	yxx2= cn("isBad isCatched  ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign")
	yy= cn("isBad isCatched")
	
	
	# 3 forms of grep()  ==
	xxId= grep('^is|Id$', xx, v=T)
	spectr(da[,  grep('^is|Id|Date|ed$', na(da), ignore.case = T)]) # ServiceLevelId - categorical
	spectr(da[, !grepl('^is|Id|Date|ed$', na(da), ignore.case = T)])
	
	# Aggregation ==
	d.mn= ddply(sus(da,, -ScoreUtcDate), .(AccountId), colwise(mean, yxx2))  #,colwise(sd)(x)
	d.sd= ddply(sus(da,, -ScoreUtcDate), .(AccountId), colwise(  sd, yxx2)	)  #,colwise(sd)(x)
	hee(d.sd)
	
	d.sdsd= sapply(d.sd, sd, na.rm=T) 
	#                             AccountId                                 isBad                             isCatched                        ServiceLevelId                        CustomerTypeId 
	#                          625712.15051                               0.00000                               0.10847                               0.00000                               0.00000 
	#                       PaymentOptionId                              LimitAmt                     BudgetTypeChanged                   PaymentInstrChanged                     DisplayURLChanged 
	#                               0.00000                               0.00000                               0.06033                               0.10449                               0.17281 
	#                             KwdPerDay                     MaxKwdPerCampaign                        SpikeKwdPerDay                   SpikeKwdPerCampaign                          BudgetPerDay 
	#                           33245.96280                           15904.54732                              15.51369                               2.50682                          585873.07359 
	# ...

	
	
	lsDF()
	# d.mn                  1734  46 AccountId isBad isCatched ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign
	# d.sd                  1734  46 AccountId isBad isCatched ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign
	# da                    5780  48 isBad ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt AccountId ScoreUtcDate AggregationStatusId BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign isCatched
	
	sa()
	# Image saved: lo('M:/26_Charlie/2/.RData')  at  2011-12-03 12:19:06


	
	# are there diff Y for accId ?
	hee(sus(d.sd, isBad>0))
	# 0  rows

	
	hee(sus(da,,-ScoreUtcDate),20) # sorted by LimitAmt  ?
	# 5780 obs. of  48 variables


	{ # often used in data exploration
		head(da)
		table(da$isBAD)
		#    0    1 
		# 1846  307 
		
		spectr(da)
		typeof(da$BudgetTypeChanged)	
		su= summary(da)
		su
		str(su)
		str(da)
	}
	


plot1= function(da, xx, Cairo=F, f=I){nx=le(xx); n1= floor(sqrt(nx)); n2= ceiling(nx/n1)
	newwin(n1, n2,sf('qnorm, %s', deparse(substitute(f))) ,{
				for(j in xx){ # j= 'ServiceLevelId'
					catt(j,typeof(da[,j]), max(da[j])); 
					#qqnorm(da[,j], main=j)
					with(x<- ord2(da[,c('isBad', j)], j %+% ' isBad'), {plot(jitter(f(x[,j])), col= isBad+2, main= j, xlab='', ylab='')})
				}			
	}, Cairo=Cairo, width=18, height=12)	
}

plot1(da, xx)
plot1(da, xxId)
plot1(d2, xxId)



plot1(da, sus(xx,   grepl('^is|^Has|Id$|Date|ed$', xx, ignore.case= T)))


plot1(da, sus(xx, ! grepl('^is|^Has|Id$|Date|ed$', xx, i= T)))  # abbrev parameters
plot1(da, sus(xx, ! grepl('^is|^Has|Id$|Date|ed$', xx, i= T)), f=log1p)  # log1p

#=  data preparation - cleaning and transf ======
dL= da[, -grep('Date|AggregationStatusId', na(da))]

# categorical to numerical - dummy variables 
dL$ServiceLevelID= ifelse(dL$ServiceLevelId==1, 0, 1); # so we don't need dummy vars

# transformation and
# normalization (or Centering and Scaling) 
dL[, -grep('AccountId', na(dL))]= sapply(log2(1 + dL[, -grep('AccountId', na(dL))]), norm)
	str(dL)
	sapply(dL, max, na.rm=T) 


	plot1(dL, na(dL))  # log1p
	plot1(dL, na(dL), f=log1p)  
	
	# Aggregation ==
	dL.mn= ddply(dL, .(AccountId), colwise(mean))  #,colwise(sd)(x)
	sapply(dL.mn, max, na.rm=T)
	sapply(dL.mn, mean, na.rm=T)
	sapply(da, mean, na.rm=T)

	sa()
	# Image saved: lo('M:/26_Charlie/2/.RData')  at  2011-12-05 00:35:46

	mean(dL.mn$isBad, na.rm=T)
	# [1] 0.2976

	mean(da$isBad, na.rm=T)
	# [1] 0.1349
/////\\\\\\\\\\\\\\\\

	dL.mnn= ddply(dL.mn, .(AccountId),  colwise(norm))  #,colwise(sd)(x)
		dL.mnn= df(sapply( sus(dL.mn,,- AccountId), norm))
		hee(dL.mnn)
		tab(dL.mnn$isBad)
		# x
		#    0    1 
		# 1218  516 

		tab(d.mn$isBad)
		# x
		#    0    1 
		# 1218  516 
		
		tab(da$isBad)
		# x
		#    0    1 
		# 5000  780 
		
		tab(dL$isBad)
		# x
		#    0    1 
		# 5000  780 

		
\\\

# Correlations
pairs(sus(da,, grepl('^is|^Has|Id$|ed$', na(da), i= T)))
pairs(sus(da,, ! grepl('^is|^Has|Id$|Date|ed$', na(da), i= T)))
hee(sus(da, ,! grepl('^is|^Has|Id$|Date|ed$', na(da), i= T)))

libra(corrplot)
#libra(Cairo)
#CairoWin(width = 15, height = 12, rescale = "R") #fixed

plotCorr= function(da, xx= ! grepl('^is|^Has|Id$|Date|ed$', na(da), i= T), title= 'numerical Predictors') {
	corr <- cor(da[,xx])
	perm= corrplot(corr, method="ellipse", order = "hclust", title=sf('\n\nCorrelation of %s', title), addnum.col='black', addrect =T)
	da.perm= da[,xx][,perm]
	cor.diag2= sapply(1:ncol(da.perm), function(j)cor(da.perm[, j], da.perm[, if(j>1) j-1 else ncol(da.perm)]))

	df(var=na(da.perm), cor.prev= cor.diag2)
}
perm= plotCorr(da)
perm= plotCorr(da)
perm= plotCorr(dL, na(dL))

plotCorr(da, grepl('^is|^Has|Id$|ed$', na(da), i= T) & ! grepl('StatusId|AccountId', na(da)), 'Categorical')
plotCorr(da, ! grepl('^is|^Has|Id$|Date|ed$', na(da), i= T), 'Numerical')
plotCorr(da, grep('Spi', na(da)), 'Spikes')
plotCorr(da, ! grepl('Spike|^is|^Has|Id$|Date|ed$', na(da), i= T), 'Numerical w/o Spikes')

plotCorr(dL, grepl('^is|^Has|Id$|ed$', na(dL), i= T) & ! grepl('StatusId|AccountId', na(dL)), 'Categorical')
plotCorr(dL, ! grepl('^is|^Has|Id$|Date|ed$', na(dL), i= T), 'Numerical')
plotCorr(dL, grep('Spi', na(dL)), 'Spikes')
plotCorr(dL, ! grepl('Spike|^is|^Has|Id$|Date|ed$', na(dL), i= T), 'Numerical w/o Spikes')




#=  change signs of xx to have positive corr with y ===
das= da[,c(yy, xx.no.corr)]
signs= sign(cor(das, das$isBad))

for(j in seq_along(signs))das[,j]= signs[j] *das[,j] 

plotCorr(das, c(yy, xx.no.corr), 'xx.no.corr * signs')


# TODO ----



# drop high corr ==
perm= plotCorr(da, ! grepl('^isB|^isC|StatusId|AccountId|Date', na(da)), 'All xx')

xx.no.corr= ch(sus(perm, abs(cor.prev) < .8)$var)
#  [1] "LimitAmt"                     "CustomerTypeId"               "IsMsftAccount"                "DisplayURLChanged"            "KwdNewCategoryCount"          "KwdCountInNewCategory"       
#  [7] "SpikeContentAmtPerDay"        "SpikeBroadAmt"                "BudgetTypeChanged"            "BlackListKwdCount"            "SpikeContentAmt"              "SpikeExactAmt"               
# [13] "PaymentInstrChanged"          "SpikeBudgetAmtPerDay"         "ServiceLevelId"               "MaxContentAmt"                "MaxExactAmt"                  "KwdCountInExistingCategory"  
# [19] "BudgetPerDay"                 "KwdExistingCategoryCount"     "KwdPerDay"                    "BudgetPerKwdPerDay"           "SpikeBudgetPerKwdPerDay"      "SpikeBudgetPerKwdPerCampaign"

plotCorr(da, c(yy, xx.no.corr), 'xx.no.corr')

#=  change signs of xx to have positive corr with y ===
das= da[,c(yy, xx.no.corr)]
signs= sign(cor(das, das$isBad))

for(j in seq_along(signs))das[,j]= signs[j] *das[,j] 

plotCorr(das, c(yy, xx.no.corr), 'xx.no.corr * signs')


#  http://caret.r-forge.r-project.org/Classification_and_Regression_Training.html

libra(caret)  # Classification_and_Regression_Training

#http://cran.r-project.org/web/packages/caret/vignettes/caretMisc.pdf
vignette(all = FALSE)
browseVignettes('caret')
v= vignette(, 'caret')
print(v$results)
#	caretMisc                                                         caret Manual -- Data and Functions (source, pdf)
#	caretSelection                                                    caret Manual -- Variable Selection (source, pdf)
#	caretTrain                                                        caret Manual -- Model Building (source, pdf)
#	caretVarImp                                                       caret Manual -- Variable Importance (source, pdf)
v$results[,'Item']
# [1] "caretMisc"      "caretSelection" "caretTrain"     "caretVarImp"   
Stangle(v$file)



prr(vignette(, 'caret'))
vignette('caretMisc')
vignette('caretVarImp')
vignette('caretSelection')
vignette('caretTrain')


hee(model.matrix(isBad ~ ., data = da[, c('isBad', xx.no.corr)]))

#= Creating Dummy Variables
dummies= dummyVars(isBad ~ ., data = da[, c('isBad', xx.no.corr)])
hee(predict(dummies, newdata = da))


nzv= nearZeroVar(da, saveMetrics= TRUE)
str(nzv)
plot1(da, rownames(nzv)[nzv$nzv], f=log1p)

#= Identifying Correlated Predictors ==
	descrCor <- cor(da[, c(xx2)])
	summary(descrCor[upper.tri(descrCor)])
	da.highlyCorr <- findCorrelation(descrCor, cutoff = .75)
	da.filtered <- da[, c(xx2)][,-da.highlyCorr]
	descrCor2 <- cor(da.filtered)
	summary(descrCor2[upper.tri(descrCor2)])

# for dL ====
corr <- cor(dL[, -grep('is|AccountId', na(dL))])
summary(corr[upper.tri(corr)])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.9880 -0.0005  0.0684  0.1550  0.2670  1.0000 

dL.highlyCorr <- findCorrelation(corr, cutoff = .9)
dL.filtered <- dL[, -grep('is|AccountId', na(dL))] [,-dL.highlyCorr]  # 5780 obs. of  26 variables:
corr2 <- cor(dL.filtered)
summary(corr2[upper.tri(corr2)])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.5860 -0.0074  0.0511  0.0870  0.1660  0.8340 

sa()
# Image saved: lo('M:/26_Charlie/2/.RData')  at  2011-12-04 19:15:48
str(dL.filtered)


#' Linear Dependencies
comboInfo <- findLinearCombos(da[, c(xx2)])
comboInfo <- findLinearCombos(dL)
comboInfo
# $linearCombos
# list()

xxy= dL.filtered
xxy$y= dL$isBad

# do not need ==
`Centering and Scaling`={
	y=da$isBad
	
	set.seed(55)
	inTrain <- sample(seq(along = y), length(y)/2)
	
	training <- da.filtered[inTrain,]
	test <- da.filtered[-inTrain,]
	y.train <- y[inTrain]
	y.test <- y[-inTrain]
	
	libra(e1071)
	
	pre= preProcess(da.filtered, method = c("BoxCox", "center", "scale"))
	da.norm= predict(pre, da.filtered) 
	da.norm$isBad= da$isBad
	plot1(da.norm, c('isBad', na(da.norm)))
	
	
	pre= preProcess(d.mn[, c(xx2)][,-da.highlyCorr], method = c("BoxCox", "center", "scale"))
	da.norm= predict(pre, d.mn[, c(xx2)][,-da.highlyCorr]) 
	da.norm$isBad= d.mn$isBad
	plot1(da.norm, c('isBad', na(da.norm)))
	
	
	
	preProcValues <- preProcess(training, method = c("BoxCox", "center", "scale"))
	preProc.train <- predict(preProcValues, training)
	preProc.test <- predict(preProcValues, test)
}


`Variable Selection`= {
	# libra(FSelector)

	## Feature Selection with the Boruta Package http://www.jstatsoft.org/v36/i11/paper 
	libra(Boruta)
	
	#Takes some time, so be patient
	st({Bor.xxy= Boruta(y~., data=xxy, doTrace=2)})
	# Initial round 1: ..........
	# Initial round 2: ..........
	# Initial round 3: ..........
	# Final round: ..........
	#  21  attributes confirmed after this test:  CustomerTypeId LimitAmt PaymentInstrChanged MaxKwdPerCampaign SpikeKwdPerCampaign MaxBudgetPerCampaign SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign SpikeExactAmt MaxPhraseAmt MaxBroadAmt SpikeBroadAmt MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay IsMsftAccount KwdNewCategoryCount KwdMaxPercentInNewCategoryPerCampaign 
	# 
	#  1  attributes rejected after this test:  BudgetTypeChanged 
	# 
	# [1] 1495
	
	
	Bor.xxy$ finalDecision
	#                        CustomerTypeId                              LimitAmt                     BudgetTypeChanged                   PaymentInstrChanged                     MaxKwdPerCampaign 
	#                             Confirmed                             Confirmed                              Rejected                             Confirmed                             Confirmed 
	#  ...
	# Levels: Tentative Confirmed Rejected
	
	
	xx.conf= with(Bor.xxy, na(finalDecision)[finalDecision== 'Confirmed']) 
	#  [1] "CustomerTypeId"                        "LimitAmt"                              "PaymentInstrChanged"                   "MaxKwdPerCampaign"                     "SpikeKwdPerCampaign"                  
	#  [6] "MaxBudgetPerCampaign"                  "SpikeBudgetAmtPerCampaign"             "BudgetPerKwdPerDay"                    "MaxBudgetPerKwdPerCampaign"            "SpikeBudgetPerKwdPerDay"              
	# [11] "SpikeBudgetPerKwdPerCampaign"          "SpikeExactAmt"                         "MaxPhraseAmt"                          "MaxBroadAmt"                           "SpikeBroadAmt"                        
	# [16] "MaxContentAmt"                         "SpikeContentAmt"                       "SpikeContentAmtPerDay"                 "IsMsftAccount"                         "KwdNewCategoryCount"                  
	# [21] "KwdMaxPercentInNewCategoryPerCampaign"
	

	set.seed(55)
	cat('Random forest run on all original raw attributes:\n');
	print(rf.raw<- randomForest(da[, -grep('is|AccountId|Date', na(da))], da$isBad))
		# or rft= tuneRF(xx, y, stepFactor=1.2, doBest=T)
	#                Type of random forest: regression
	#                      Number of trees: 500
	# No. of variables tried at each split: 13
	# 
	#           Mean of squared residuals: 0.01062
	#                     % Var explained: 90.9
	# other run:
	#           Mean of squared residuals: 0.01083
	#                     % Var explained: 90.72


	set.seed(55)
	cat('Random forest run on all original ln attributes:\n');
	print(rf.L<- randomForest(dL[, -grep('is|AccountId', na(dL))], dL$isBad))
	randomForest(x = dL[, -grep("is|AccountId", na(dL))], y = dL$isBad) 
	#	Type of random forest: regression
	#	Number of trees: 500
	#	No. of variables tried at each split: 13
	#	
	#	Mean of squared residuals: 0.01051
	#	% Var explained: 91
	# 
	#           Mean of squared residuals: 0.01048
	#                     % Var explained: 91.02
	
	
	cat('Random forest run on all with low correlation attributes:\n');
	print(randomForest(y~., data=xxy));
	# 
	# Call:
	#  randomForest(formula = y ~ ., data = xxy) 
	#                Type of random forest: regression
	#                      Number of trees: 500
	# No. of variables tried at each split: 7
	# 
	#           Mean of squared residuals: 0.009904
	#                     % Var explained: 91.52
	
	cat('Random forest run only on confirmed attributes:\n');
	set.seed(55)
	print(rf.conf<- randomForest(getConfirmedFormula(Bor.xxy), data=xxy));
	# 
	# Call:
	#  randomForest(formula = getConfirmedFormula(Bor.xxy), data = xxy) 
	#                Type of random forest: regression
	#                      Number of trees: 500
	# No. of variables tried at each split: 7
	# 
	#           Mean of squared residuals: 0.009831
	#                     % Var explained: 91.58
	# other runs:
	# 
	#           Mean of squared residuals: 0.009876
	#                     % Var explained: 91.54
	#			Mean of squared residuals: 0.009931
	#						% Var explained: 91.49


	getConfirmedFormula(Bor.xxy)	
	# y ~ CustomerTypeId + LimitAmt + PaymentInstrChanged + MaxKwdPerCampaign + 
	#     SpikeKwdPerCampaign + MaxBudgetPerCampaign + SpikeBudgetAmtPerCampaign + 
	#     BudgetPerKwdPerDay + MaxBudgetPerKwdPerCampaign + SpikeBudgetPerKwdPerDay + 
	#     SpikeBudgetPerKwdPerCampaign + SpikeExactAmt + MaxPhraseAmt + 
	#     MaxBroadAmt + SpikeBroadAmt + MaxContentAmt + SpikeContentAmt + 
	#     SpikeContentAmtPerDay + IsMsftAccount + KwdNewCategoryCount + 
	#     KwdMaxPercentInNewCategoryPerCampaign

		cat('Tuned Random forest run only on confirmed attributes:\n');
		(rf.tu= tuneRF(xxy[, xx.conf], xxy$y, stepFactor=1.2, doBest=T))
		# mtry = 7  OOB error = 0.01163 
		# Searching left ...
		# mtry = 6 	OOB error = 0.01069 
		# 0.08079 0.05 
		# mtry = 5 	OOB error = 0.01144 
		# -0.07007 0.05 
		# Searching right ...
		# mtry = 8 	OOB error = 0.01017 
		# 0.0483 0.05 
		# 
		# Call:
		#  randomForest(x = x, y = y, mtry = res[which.min(res[, 2]), 1]) 
		#                Type of random forest: regression
		#                      Number of trees: 500
		# No. of variables tried at each split: 8





	plotRFImportance= function(rf0= rf.conf){ 
		x11(); plot(rf0)
		rfi<- rf0$importance; rfi[or<- order(- rfi),]
		x11(); op=par(mar=c(3,19,3,1))
		barplot(rfi[or<- order(- rfi)], names.arg=rownames(rfi)[or], horiz = T, las=2,main="RF, Importance" )
		par(op)
		str(rf0); summary(rf0); # print(rf0)
	}
	plotRFImportance(rf0= rf.conf)
	plotRFImportance(rf0= rf.raw)
	
	sa()
	# Image saved: lo('M:/26_Charlie/2/.RData')  at  2011-12-04 22:41:41


}





#' Data Splitting
set.seed(55)
set1index <- createDataPartition(y, p = .8, list = FALSE, times = 5)
hee(set1index)
set1 <- y[set1index]
round(table(set1)/length(set1), 2)
# set1
#    0    1 
# 0.87 0.13 

round(table(y)/length(y), 2)
# y
#    0    1 
# 0.87 0.13 


#' Visualizing Data
plot(2:8)
featurePlot(da.filtered, y)
featurePlot(dL, y)

sa()
# Image saved: sw('M:/26_Charlie/2/')  lo('M:/26_Charlie/2/.RData')  at  2011-12-03 21:36:04
\\\\

`MDS plot` = {
	libra(vegan)
	
# time consuming
	st({mds <- metaMDS(sus(da.norm,, -isBad), distance='euclidean', k=2, 
						trymax=10, autotransform=FALSE)})
	st({mds <- metaMDS(sus(da.norm,, -isBad), k=2,	trymax=10)})
# Run 0 stress 0.08727 
# Run 1 stress 0.4213 
# Run 2 stress 0.06842 
# ... New best solution
# ... procrustes: rmse 0.02296  max resid 0.2882 
# Run 3 stress 0.4213 
# Run 4 stress 0.09187 
# Run 5 stress 0.4213 
# Run 6 stress 0.07405 
# Run 7 stress 0.06363 
# ... New best solution
# ... procrustes: rmse 0.01743  max resid 0.4192 
# Run 8 stress 0.4213 
# Run 9 stress 0.4206 
# Run 10 stress 0.421 
# [1] 78.46
# Warning message:
# In metaMDS(sus(da.norm, , -isBad), distance = "euclidean", k = 2,  :
#   'comm' has negative data: 'autotransform', 'noshare' and 'wascores' set to FALSE
	
	
# [1] 39.76
	
	st({mds <- metaMDS(sus(da.norm,, -isBad), k=2,	trymax=10)})
# Run 0 stress 0.2724 
# Run 1 stress 0.2864 
# Run 2 stress 0.286 
# Run 3 stress 0.287 
# Run 4 stress 0.2864 
# Run 5 stress 0.2865 
# Run 6 stress 0.2864 
# Run 7 stress 0.2881 
# Run 8 stress 0.2877 
# Run 9 stress 0.2884 
# Run 10 stress 0.2874 
# [1] 195.4
# Warning messages:
# 1: In metaMDS(sus(da.norm, , -isBad), k = 2, trymax = 10) :
#   'comm' has negative data: 'autotransform', 'noshare' and 'wascores' set to FALSE
# 2: In distfun(comm, method = distance, ...) :
#   results may be meaningless because data have negative entries in method bray
	
	
	
	hee(sus(da.norm,, -isBad))
	
	plot(mds)
	
	PlotMDS(mds, da.norm, cond= (d.mn$LimitAmt > 0), sentTQ=NULL)
	
	plot(mds, type='n', main='\n\nmetaMDS, distance="euclidean", norm')  
	text(1* mds$species,, na(da.norm), col='blue1', cex=4, font=2)
	symbols(mds$ points,,
			, bg = da.norm$isBad + 2
			, circ= d.mn$LimitAmt^.15, inc=.3, add=T)

}
# not useful

lsDF()
sa() # Image saved: lo('M:/26_Charlie/2/.RData')  at  2011-12-04 10:52:59





newwin(6,8,'qnorm-log',{
			for(j in xx){catt(j,typeof(da[,j]), max(da[j])); 
				qqnorm(log1p(da[,j]), main=j)
			}			
},2,width=22, height=14)




# Log if neccessary =====

daL= data.frame(y= da$isBAD)
for(j in names(da)){catt(j,typeof(da[,j]), max(da[j])); 
	if(max(da[j]) > 2) daL[,"L" %+% j]= log10(1+da[,j]) else if(j != 'isBAD') daL[,"b" %+% j]= da[,j]
}


zqqnorm= function(y, leg= F, pch=16,...){ # y= log10(da.p$Conversions+1) ; dy<- .0001*diff(range(y))
	or= order(y)
	plot(qnorm(((1:length(y)-.5))/length(y)), y[or], col= b <- 1+daL$y[or]
		, xlab='Quantile Norm'
		, ylab='' # sprintf('log10( %s )',gsub('^.*\\$','',deparse(substitute(y)))) 
		, cex=.2* b, pch=pch,...)
	if(leg)legend('topleft', ' ' %+% ch(10*(11:1)-10), fill= topo.colors(102)[10*(11:1)-9], horiz = F, title = 'ConvR rel', bty = "n", y.intersp=0.5,cex=.8 )
}

newwin(5,7,'qnorm daL',{
			for(j in names(daL)){catt(j,typeof(daL[,j]), max(daL[j])); 
				zqqnorm(daL[,j], main=j)
			}			
}, width=22, height=14)

# normalization =
#daLn= apply(daL, 2, function(x) x/max(x))
daLn= data.frame(apply(daL, 2, function(x) x/max(x)))

#libra(plyr)
#daLn= mdply(daL,  function(x) x/max(x))

newwin(5,7,'qnorm daLn',{
			for(j in na<- unlist(dimnames(daLn)[2])){catt(j,typeof(daLn[,j]), max(daLn[,j])); 
				zqqnorm(daLn[,j], main= j)
			}			
}, width=22, height=14)


ds= daLn
#-- end clear data

# Prediction Model ======
libra(randomForest)
#	rf= randomForest(Name ~ RetentionTime, data = na.omit(dsb),  na.action=na.roughfix, keep.forest=T, importance=T)
#	yhat.rf= predict(rf,  dsb[,c('SampleName','RetentionTime')])
#	
#	ftable(dsb$Name, yhat.rf)

libra(gbm) # Generalized Boosted Regression Modeling
#libra(svm)
libra(BayesTree)
libra(e1071) # naiveBayes svm nnet
libra(nnet)
libra(MASS) # stepAIC


n.fold=10
nr.good= sum(ds$y == 0)
nr.bad = sum(ds$y != 0)


SampleFold= function(seed= 55, n.fold= 10, y= ds$y){
	fold= y
        nr.good= sum(y == 0)
        nr.bad = sum(y != 0)
	set.seed(seed)
	
	fold[y == 1]= sample(rep(1:n.fold, ceiling(nr.bad  / n.fold)), nr.bad)   
	fold[y == 0]= sample(rep(1:n.fold, ceiling(nr.good / n.fold)), nr.good) 
	                                                                                                
	print(table(y, fold))
	return(fold)
}

fold= SampleFold(seed=1, n.fold=10)
#       1   2   3   4   5   6   7   8   9  10
#   0 185 184 183 185 185 185 185 185 184 185
#   1  30  31  31  31  31  31  30  31  31  30
head(fold)

yhat= data.frame(y= ds$y)

st({
	for(i.fold in 1:n.fold){ # i.fold=2
		catt('i.fold=',i.fold)
		tr= ds[fold!=i.fold,]; va= ds[fold==i.fold,]
		
	
		#neural nets
		st('nenet',{nenet= best.nnet(y~., data = tr,  size = 20, rang = 0.1, maxit = 200); #head(yhat)
		    yhat$nenet[fold==i.fold]= predict(nenet, va)
        })
		#next

		st('nenet25',{nenet25= best.nnet(y~., data = tr,  size = 25, rang = 0.1,	decay = 5e-4, maxit = 200)
		    yhat$nnet25[fold==i.fold]= predict(nenet25, va)	
        })

		st('nenet10',{nenet10= best.nnet(y~., data = tr,  size = 10, rang = 0.1,	decay = 5e-4, maxit = 600)
		    yhat$nnet10[fold==i.fold]= predict(nenet10, va)
         })
		# GLM - logistic regression
		glm1= glm(y ~ ., data = tr, family=binomial)
		#bBudgetTypeChanged+bPaymentIn1strChanged+bDisplayURLChanged+LKwdPerDay+LMaxKwdPerCampaign+LSpikeKwdPerDay+LSpikeKwdPerCampaign+LBudgetPerDay+LMaxBudgetPerCampaign+
		#	LSpikeBudgetAmtPerDay+LSpikeBudgetAmtPerCampaign+LBudgetPerKwdPerDay+LMaxBudgetPerKwdPerCampaign+LSpikeBudgetPerKwdPerDay+LSpikeBudgetPerKwdPerCampaign+
		#   LMaxExactAmt+LSpikeExactAmt+LSpikeExactAmtPerDay+LSpikeExactAmtPerCampaign+LMaxPhraseAmt+LSpikePhraseAmt+LSpikePhraseAmtPerDay+LSpikePhraseAmtPerCampaign+LMaxBroadAmt+LSpikeBroadAmt+LSpikeBroadAmtPerDay+LSpikeBroadAmtPerCampaign+LMaxContentAmt+LSpikeContentAmt+
		#	LSpikeContentAmtPerDay+LSpikeContentAmtPerCampaign+bIsMsftAccount+LIncrementSpikeBudgetAmtPerDay, data = tr)
		
		#glm1= glm(y ~ . + LSpikeBudgetAmtPerDay*LSpikeBudgetAmtPerCampaign*LMaxBudgetPerCampaign, data = tr, family=binomial)
		glm1= stepAIC(glm1, trace = 0)
		yhat$glm[fold==i.fold]= predict(glm1, va)
		summary(glm1)
		
		
		glm2= glm(y ~ LSpikeBudgetAmtPerDay*LSpikeBudgetAmtPerCampaign*LMaxBudgetPerCampaign + . , data = tr, family=binomial)
		glm2= stepAIC(glm2, trace = 0)
		yhat$glm2[fold==i.fold]= predict(glm2, va)
		summary(glm2)
		
		rpart= best.rpart(y~., data = tr)
		yhat$rpart[fold==i.fold]= predict(rpart, va)				
		
		
		nb= naiveBayes(y ~ ., data = tr)
		yhat$naiveBayes[fold==i.fold]= predict(nb, va, type = "raw")[,2]- predict(nb, va, type = "raw")[,1]	
		summary(nb); print(nb);


			
	
		st('rf',{
    		rf= randomForest(y ~ ., data=tr,  na.action=na.roughfix, keep.forest=T, importance=T)
    		yhat$rf[fold==i.fold]= predict(rf, va)	
    		if(0){
    			plot(rfi <- rf$importance)
    			or= order(-rfi[,1])
    			rfi= rfi[or,]
    			#text(rf$importance, rownames(rf$importance), col=1:nrow(rf$importance))
    			shortX= gsub('[a-z]','',rownames(rfi))
    			text(rfi, shortX, col=1:nrow(rfi), cex=.9)
    			legend('topleft', shortX %+% ' - '%+% rownames(rfi), text.col=1:nrow(rfi), cex=.5)
    		}	
        })
    		
        st('rft',{
    		rft= tuneRF(tr[,-1], tr[,1], stepFactor=1.2, doBest=T)
    		yhat$rft[fold==i.fold]= predict(rft, va)
    		if(0){ 
    			plot(rft); rfi<- rft$importance; 
    		    x11(); op=par(mar=c(3,15,3,1)); barplot(rfi[or<- order(- rfi)], names.arg=rownames(rfi)[or], horiz = T, las=2,main="RF, Importance)" ); par(op)
    		    str(rft); summary(rft); # print(rft)
    	    }
        })
		gb= gbm(y ~ ., distribution = "bernoulli", 
				data = tr,  n.trees = 500,  shrinkage = 0.0001, 
				interaction.depth = 4, bag.fraction = .5, train.fraction = 1, cv.folds = 1, keep.data = T, 
				verbose = F)
		yhat$gb[fold==i.fold]= predict(gb, va,  n.trees =  500)
		
		
		# svm (support vector machine) ===
		# svm requires tuning
        st('svm',{
    		x.svm.tune <- tune(svm, y~., data = tr,
    				ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)),
    				tunecontrol = tune.control(sampling = "fix"))
    		# display the tuning results (in text format)
    		x.svm.tune
    		# If the tuning results are on the margin of the parameters (e.g., gamma = 2^-8), then widen the parameters.
    		bestParam= x.svm.tune$ best.parameters
    		x.svm <- svm(y~., data = tr, cost= bestParam$cost, gamma= bestParam$gamma)
    		yhat$svm[fold==i.fold]= predict(x.svm, va)
    		# plot(x.svm.tune); str(x.svm.tune); summary(x.svm.tune); # print(bart)
        })
			
        st('bart',{
    		bart1= bart(tr[,-1], tr[,1], va[,-1],   ntree=3, ndpost=50, nskip=2) # minimal parameters for high speed
    		yhat$bart[fold==i.fold]= apply(bart1$yhat.test, 2, mean)
    		# plot(bart); str(bart); summary(bart); # print(bart)  plot.bart
        })
	}#--for(i.fold)
	
}) # st	
#Execution time: 9922 
dhe(yhat)
gw(); sa()


{   #Ensembling ===
	n.fold= 15
	yhat$lass= NULL
	ma= as.matrix
	
	fold= SampleFold(seed= 55, n.fold=n.fold)
	head(fold)
	
	ma.yhat=ma(yhat[,-1])
	
	lass.beta=data.frame()
	
	libra(lars) # lars - lasso regr
	
	for(i.fold in 1:n.fold){ # i.fold=2
		catt('i.fold=',i.fold)
		tr= ds[fold!=i.fold,]; va= ds[fold==i.fold,]
		
        #lass= lars(ma.yhat[fold!=i.fold, -1], ma.yhat[fold!=i.fold, 1], type = "lasso" )
        lass= lars(ma.yhat[fold!=i.fold, -1], yhat[fold!=i.fold, 1], type = "lasso" )
        summary(lass); #plot(lass)
		print(lass$beta[,])
		#catt(lass$ lambda, lass$beta[5,])
		for(ila in nrow(lass$beta):1)if(all(lass$beta[ila,]>=0)){catt('ila=',ila);break}
#		yhat$lass[fold==i.fold]=  unlist(ma.yhat[fold ==i.fold,-1] %*% lass$beta[ila,])# - c('y','lass')
#		lass.beta= rbind(lass.beta, lass$beta[ila,])
		
		yhat$lass[fold==i.fold] = predict.lars(lass, ma.yhat[fold ==i.fold,-1], s=ila, type="fit", mode = "step")$fit
		lass.beta= rbind(lass.beta, coef(lass, s=ila, mode="step"))
	}
	
	names(lass.beta)= colnames(lass$beta)
	
	
	lass.beta
	boxplot(lass.beta, main='Lasso: reg coefficients by fold')
	head(yhat,20)# yhat$nenet yhat$naiveBayes
	
}

LiftWtArr= function(y, yhat, wt= 0*y + 1, costFN=10, nPlot= le(y), PlotXrel=F,
		clr=rep(c('red','brown','green','blue','violet','black'), 4),
		pch= rep(1:25,2),
		pmax=100, ymax=100, kCostPlot=3, ...){
	
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

	x11()
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
			xlab=paste('% rank ', deparse(substitute(yhat))),
			ylab=paste('% cum', deparse(substitute(y))),
            #xlim=c(0,pmax/15), ylim=c(0,ymax/4),
            xlim=c(0,pmax), ylim=c(0,ymax),
            main=paste("Lift ", deparse(substitute(yhat))), #' [ CV=',cv,']' ),
			...)
	
	j=0
	LiftArea=NULL; LiftAreaRel=NULL; cv=NULL; cost=data.frame(zz=0*y.cum);
	
	for (i in names(yhat)){j=j+1;cat("\np2.",j,i);
		#or=order(-yhat[,i], -y, na.last = NA); #print(or);
		or=order(-yhat[,i], rnorm(nrow(yhat)), na.last = NA); #print(or);
		y.sort=y[or] ; #print(cbind( y.sort,yhat.sort))
		yhat.sort=yhat[or,i];
		wt.s=wt[or];
		
		y.s.cum= c(0,cumsum(y.sort*wt.s));            y.s.ncum= 100 * y.s.cum/y.s.cum[nx+1]
		wt.cum= c(0,cumsum(wt.s));                    wt.ncum= 100 * wt.cum/wt.cum[nx+1]
		LiftArea[i]= cumsum(c(0,wt.s)*(y.s.ncum-wt.ncum)/100)[nx1]/wt.cum[nx]
		LiftAreaRel[i]= LiftArea[i]/LiftAreaY
		# cat("p3.",j,i,  LiftArea[i],  LiftAreaRel[i])
		cv[i]=CVwt(y,yhat[,i],wt); # =signif(CVwt(y,yhat[,i],wt),4);
		
		
		#lines(wty.ncum[xp], y.s.ncum[xp], lty=3, col=clr[j], pch=18+j ,type="o")
		lines(wty.ncum[xp], y.s.ncum[xp], lty=3, col=clr[j], pch=pch[j] ,type="o",...)
		cost[i]= wt.cum + sum(costFN*wt*y) -  c(0,cumsum((1+costFN)*wt.s*y.sort)) # (1+costFN)*y.s.cum
	}
	abline(0,1,col='darkgreen',lty=2)
	points(c(0,100), c(0,100), pch = 3, cex = 4, col = "red")
	
	or=order(-LiftArea);
	#legend('bottomright', names(yhat)[or], lwd=3, lty=1, col=clr[or], pch=18+or, bty='n')
	legend('bottomright', names(yhat)[or], lwd=3, lty=1, col=clr[or], pch=pch[or], bty='n')
	
	
	
	#cost plot
	cost$zz=NULL
	y.min= min(cost)
	x.min= which.min(unlist(cost)) %% nrow(cost)
	
	x11()
	or=order(-apply(cost,2,min));
    #matplot(cost[,or], cex=.4, xlim=x.min*c(.5, 2), ylim= y.min*c(1/kCostPlot, kCostPlot), col=clr[or], pch=pch[or])
    matplot(wt.cum, cost[,or], cex=.4, xlim=wt.cum[x.min*c(.5, 2)], ylim= y.min*c(1/kCostPlot, kCostPlot), col=clr[or], pch=pch[or])
    legend('bottomright', names(cost)[or], lwd=3, lty=1, col=clr[or], pch=pch[or], bty='n')
	
	
	return(z=list(lift=data.frame(LiftArea,LiftAreaRel,cv),
					LiftAreaY=LiftAreaY, cost=cost))
	
}


Lift= LiftWtArr(yhat[,1], yhat[,-1], costFN=100, clr=rep(1:7,3), kCostPlot=3, cex=.3)

# variable costFN
Lift= LiftWtArr(yhat[,1], yhat[,-1], costFN=exp(ds$LBudgetPerDay), clr=rep(1:7,3), kCostPlot=3)

		clr=rep(1:6,3)
		pch=rep(0:9,3)
		cost= Lift$cost
		or=order(-apply(cost,2,min));
		#matplot(cost[,or], cex=.4, xlim=x.min*c(.5, 2), ylim= y.min*c(1/kCostPlot, kCostPlot), col=clr[or])
		matplot(cost[,or], cex=.6, col=clr[or],pch=pch[or]) #,  xlim=c(200, 800), ylim=c(300,700))
		legend('bottomleft', names(cost)[or], col=clr[or],pch=pch[or], bty='n' )# lwd=3, lty=1,
		
save.image()
load(file = ".Rdata")

or=order(-yhat$rft);
write.csv(yhat[or,], file='yhat.csv')


		
		if(0){    #..................................
			names(installed.packages()[,1])
			libra(Rcmdr) ; Commander() # http://www.jstatsoft.org/v14/i09/paper
			library(help=modreg)
			library(help=Rcmdr)
			
			options(chmhelp=T, digits=5)
			
			ls()
			setwd('S:\\52_InnoCentive\\1_FieldTrialDataQC\\z\\sent3\\data')
			setwd('../')
			getwd()          #"S:/52_InnoCentive/FieldTrialDataQC/z"
			save.image() ;
			save.image(file = "all.Rdata") ;
			unlink("all.Rdata")
			save(sds,res, file = "S:/52_InnoCentive/FieldTrialDataQC/z/sds.res.Rdata");
			dput(sds, file = "S:/52_InnoCentive/FieldTrialDataQC/z/sds.txt")
			sds= dget("S:/52_InnoCentive/FieldTrialDataQC/z/sds.txt");  unlink("sds.txt")
			res= dget("S:/52_InnoCentive/FieldTrialDataQC/z/res2.txt");  unlink("res.txt")

			load(file = ".Rdata")
			rm(ds,dss,meuse)
			rm(list=ls())
			spectr(sds)
			environment()
			slotNames(wrld_simpl)
			Sys.getenv()
			commandArgs()
			traceback()
			
			source('http://rbenchmark.googlecode.com/svn/trunk/benchmark.r')
			benchmark({ y_ty0 = aggregate(sds$y, list(EN=sds$Entry_Name), mean)
						pred2= sqldf("select sds.*, y_ty0.x as avg_y   \
										from sds left join y_ty0                  \
										on sds.Entry_Name = y_ty0.EN")
					}, replications = 1)
			
			s= readLines("zRExcel\\zReGUI_FieldTrialQC_all.r")
			for(i in grep('^[^#]*(= func|<- func)', s))catt('# ',i, (s[i]))
			
			s= readLines("../Solution.r")
			for(i in grep('[Dd]escr', s))catt('# ',i, (s[i]))
			
			memory.limit(size=4000)            # in M
			memory.limit()
			round(memory.limit()/1048576.0, 2) # in M
			
			memory.size(max =T)
			
			
# print aggregate memory usage statistics
			print(paste('R is using', memory.size(), 'MB out of limit', memory.limit(), 'MB'))
			
			
			source('http://yihui.name/en/wp-content/uploads/2010/08/Npp_R_Auto_Completion.r')
#   R.xml will be generated under your current work directory: getwd()
			
		}
		

