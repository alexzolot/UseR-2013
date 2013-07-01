#! /usr/bin/Rscript --vanilla --slave
# Project  : newProjTemplName
# File     : newProjTemplName/newProjTemplName.r
# Author   : Alex Zolotoviski, alex@zolot.us
# Created  : 00-00-00
# License  : GPL-2
###############################################################################

{ #== init ===
	rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
	
	options(help='html', digits=4, width=2160, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe'
			, error= NULL) # options(error= recover) options(error=dump)  #or options(error=NULL)

	onWin= Sys.getenv('R_PLATFORM')==''
	if(onWin){root= 'T:/work/UseR-2013'; 	memory.limit(size=9000)}  else root= '/home/azolotovitski/work'   # memory.limit()
	
	#source(file.path(root, 'lib/zBase0.r'))  
	source('T:/work/UseR-2013/lib/zBase0.r')  # xxx: hard coded path to zBase.r
	source('T:/work/UseR-2013/lib/zBase1.r')  
	source('T:/work/UseR-2013/lib/zCode.r')  
	source('T:/work/UseR-2013/lib/zStats.r')  
	source('T:/work/UseR-2013/newProjTemplName/newProjTemplName.fun.r')  
	
	libra(plyr) 
	#libra(R2HTML) 
	#libra(XLConnect)
	
	libra(RColorBrewer)  # display.brewer.all()
	palette(c(adjustcolor(cn('grey50 green3 red2'), alpha.f = .6), brewer.pal(8,"Dark2")))  ##ex: plot(1:19, pch=16, col=1:19, cex=3)
	
	#libra(Defaults)
	#	setDefaults(legend, bty='n')
	#	setDefaults(symbols, inc=.15)
	#libra(randomForest)
	#libra(plotrix)  # addtable2plot
	
	#libra(ggplot2)
	#libra(scales)
	#libra(RJDBC)     # for hive_conn
	
	proot= fp(root, 'newProjTemplName')  # project root
	sw(fp(proot, 'out'))
	
	#libra(Cairo);  
		
	theFile= fp(proot, 'newProjTemplName.r')
	
	HHp.bak= HHp #; HHp=dummy; # HHp= HHp.bak   # to rerun w/o change images
	sa.bak= sa   #; sa=dummy ; # sa= sa.bak   # to rerun w/o change images

	
	# catf('\ndetach("%s",  character.only = TRUE)', grep('^pac', search(), v=T))
	# gff('saved', theFile)
	# rmDF(); loo(); lo(); lsDF();
} #--
##########################################################

#====   newProjTemplName  ====

if(0){#== Data Inventory  ==
	gw() 
	dir() # expl() 
}

{#== Data Exploration  ==
}

{#== Predictive Modeling ==
}

{#== Reports ==
}


if(0){   #== Misc
	theFile= 'm:/newProjTemplName/newProjTemplName.r'
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	
	theFile= fp(proot, 'newProjTemplName.r')
	ccc= function()code2HTML(theFile)
	
	CreateNewProj(newProj.name= 'zzz', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplNa me', root='T:/work/UseR-2013')
} #--
