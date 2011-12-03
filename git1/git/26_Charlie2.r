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
	sw('M:/26_Charlie/2') # alias sw();  '/' instead of '\'
 
	spectr= function (ds, nValMax = 20) # ds = da; nValMax = 5
	{
		res = list()
		cat("nrow=", nrow(ds), "\n")
		
		res$tb= d= ldply(ds,  function(x)df(class= class(x)
									, Missing= sum(is.na(x)), notMissing= sum(!is.na(x)), not0= sum(x != 0), nValues= le(unique(x))
									, shortCateg = (le(unique(x)) <= nValMax) 
									#, {print(summary(x)); if(class(x)!='character') t(summary(x)[1:6]) else rep(7,6)}
									, {print(summary(x)); if(class(x)!='character') t(summary(x)[1:6]) else  t(summary(-777)[1:6])}
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
 
	# read data
	da = read.csv('in/PaymentFraud_NumericData.csv', header=T, stringsAsFactors =F)
	
	##==  Data Understanding  ==
	
	str(da)
	# 'data.frame':	5780 obs. of  48 variables:
	
	
	hee(da)
	# he(sus(da, , sel= cn("isBad ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt AccountId ScoreUtcDate AggregationStatusId BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign isCatched")), 5)
	xx= cn("ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt AccountId AggregationStatusId BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign")

	sp= spectr(da)
	da$AccountId
	da$AggregationStatusId  #ScoreUtcDate
	
	xx2= cn("ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign")
	yxx2= cn("isBad ServiceLevelId CustomerTypeId PaymentOptionId LimitAmt BudgetTypeChanged PaymentInstrChanged DisplayURLChanged KwdPerDay MaxKwdPerCampaign SpikeKwdPerDay SpikeKwdPerCampaign BudgetPerDay MaxBudgetPerCampaign SpikeBudgetAmtPerDay SpikeBudgetAmtPerCampaign BudgetPerKwdPerDay MaxBudgetPerKwdPerCampaign SpikeBudgetPerKwdPerDay SpikeBudgetPerKwdPerCampaign MaxExactAmt SpikeExactAmt SpikeExactAmtPerDay SpikeExactAmtPerCampaign MaxPhraseAmt SpikePhraseAmt SpikePhraseAmtPerDay SpikePhraseAmtPerCampaign MaxBroadAmt SpikeBroadAmt SpikeBroadAmtPerDay SpikeBroadAmtPerCampaign MaxContentAmt SpikeContentAmt SpikeContentAmtPerDay SpikeContentAmtPerCampaign IsMsftAccount BlackListKwdCount HasNewKeywords KwdNewCategoryCount KwdExistingCategoryCount KwdCountInNewCategory KwdCountInExistingCategory KwdMaxPercentInNewCategoryPerCampaign")
	
	d2= ldply(da[,-c('ScoreUtcDate')], .(AccountId), function(x)df(mean, sd)	)
	d2= ddply(sus(da,,-ScoreUtcDate), .(AccountId), function(x)df(colMeans(x))	)  #,colwise(sd)(x)
	d2= ddply(sus(da,,-ScoreUtcDate), .(AccountId), function(x)df(mean(x[1]))	)  #,colwise(sd)(x)
	d2= ddply(sus(da,,-ScoreUtcDate), .(AccountId), function(x)colwise(sd,xx2)	)  #,colwise(sd)(x)
	d2= ddply(sus(da,,-ScoreUtcDate), .(AccountId), function(x)colwise(mean,xx2)	)  #,colwise(sd)(x)
	
	d2= ddply(sus(da,,-ScoreUtcDate), .(AccountId), colwise(mean,yxx2), colwise(sd,xx2)	)  #,colwise(sd)(x)
	d2= ddply(sus(da,,-ScoreUtcDate), .(AccountId), colwise(sd,yxx2)	)  #,colwise(sd)(x)
	hee(d2)
	
	# are there diff Y for accId ?
	hee(sus(d2, isBad>0))
	# 0  rows

	
	hee(sus(da,,-ScoreUtcDate),20) # sorted by LimitAmt  ?
	
	#'data.frame':	2153 obs. of  34 variables:
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


newwin(6,8,'qnorm',{
			for(j in xx){ # j= 'ServiceLevelId'
				catt(j,typeof(da[,j]), max(da[j])); 
				#qqnorm(da[,j], main=j)
				with(x<- ord(da[,c('isBad', j)], j), {plot(x[,j], col= isBad+1, main= j, xlab='', ylab='')})
			}			
},width=18, height=12)

newwin(5,7,'qnorm-log',{
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
		

