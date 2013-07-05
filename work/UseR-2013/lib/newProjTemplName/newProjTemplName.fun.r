#! /usr/bin/Rscript --vanilla --slave
# Project  : newProjTemplName
# File     : newProjTemplName/newProjTemplName.fun.r  - functions for the project
# Author   : Alex Zolotoviski, azolotovitski@medio.com
# Created  : 00-00-00
# License  : Copyrite Medio Systems Inc. 
###############################################################################

if(0){ #== init ===
	options(help='html', digits=4, width=2160, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe'
			, error= NULL) # options(error= recover) options(error=dump)  #or options(error=NULL)
	
	onWin= Sys.getenv('R_PLATFORM')==''
	if(onWin){root= 'T:/work/UseR-2013'; 	memory.limit(size=9000); memory.limit()}  else root= '/home/azolotovitski/work'
	
	#source(file.path(root, '99_commonR/zBase0.r'))  
	source('T:/work/UseR-2013/99_commonR/zBase0.r')  # xxx: hard coded path to zBase.r
	source('T:/work/UseR-2013/99_commonR/zBase1.r')  # xxx: hard coded path to zBase.r
	source('T:/work/UseR-2013/99_commonR/zCodeTools.fun.r')  # xxx: hard coded path to zBase.r
	
	libra(plyr) 
	#libra(R2HTML) 
	#libra(XLConnect)
	
	libra(Defaults)
	setDefaults(legend, bty='n')
	setDefaults(symbols, inc=.15)
	
	libra(RColorBrewer)  # display.brewer.all()
	palette(c(adjustcolor(cn('grey50 green3 red2'), alpha.f = .6), brewer.pal(8,"Dark2")))  ##ex: plot(1:19, pch=16, col=1:19, cex=3)
	
	libra(randomForest)
	#libra(plotrix)  # addtable2plot
	
	#libra(ggplot2)
	#libra(scales)
	
	proot= fp(root, 'newProjTemplName')  # project root
	sw(fp(proot, 'out'))
	
	#libra(Cairo);  
	
	theFile= fp(proot, 'newProjTemplName.r')
	
	HHp.bak= HHp # HHp=dummy; HHp= HHp.bak   # to rerun w/o change images
	sa.bak= sa   # sa=dummy; sa= sa.bak   # to rerun w/o change images
	
	libra(RJDBC)     # for hive_conn
	
	hive_conn= function(hostname= 'localhost', port=10001, lib_dir='/usr/local/churn/lib' ){
		hive_class_path= fp(lib_dir, dir(lib_dir))
		drv= JDBC( 'org.apache.hadoop.hive.jdbc.HiveDriver', classPath= hive_class_path, "`" )
		server= sf( "jdbc:hive://%s:%s/default", hostname, port)
		conn = dbConnect( drv, server )
		invisible(conn)
	}
	
	hSel= function(.conn=conn, SE='SELECT', sel, ... ){
		try({.conn<<- conn<<- hive_conn('dlhive01.cloud.msrch', port = 10000, lib_dir='M:/77_ChurnProd/lib')
					dbSendQuery(.conn, 'Use gree' )}, s=T)
		catt('hSel:', sf('%s %s', SE, sf(sel, ...)))
		dbGetQuery(.conn, sf('%s %s', SE, sf(sel, ...)))
	} 
	
	
	# gff('saved', theFile)
	# rmDF(); loo(); lo(); lsDF();
} #--
##########################################################





if(0){   #== Misc
	theFile= 'm:/newProjTemplName/newProjTemplName.fun.r'
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	
	theFile= fp(proot, 'newProjTemplName.r')
	theFile= fp(proot, 'newProjTemplName.fun.r')
	ccc= function()code2HTML(theFile)
} #--
