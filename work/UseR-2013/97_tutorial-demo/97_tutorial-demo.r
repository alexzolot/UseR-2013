#! /usr/bin/Rscript --vanilla --slave
# Project  : 97_tutorial-demo
# File     : 97_tutorial-demo/97_tutorial-demo.r
# Author   : Alex Zolotoviski, azolotovitski@medio.com
# Created  : 2013-06-24 02:55:55
# License  : GPL-2  
###############################################################################

## before:  CreateNewProj(newProj.name= '97_tutorial-demo', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplName', root='T:/work/UseR-2013')


{ #== init ===
	rm(list= ls(envir= .GlobalEnv), envir= .GlobalEnv)  ## clean .GlobalEnv
	
	options(help='html', digits=4, width=2160, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe'
			, error= NULL) # options(error= recover) options(error=dump)  # or options(error=NULL)

	onWin= Sys.getenv('R_PLATFORM')==''
	if(onWin){root= 'T:/work/UseR-2013'; 	memory.limit(size=9000)}  else root= '/home/azolotovitski/work'  # memory.limit()
	
	if(fromSrc <- 1){
		#source(file.path(root, 'lib/zBase0.r'))  
		source('T:/work/UseR-2013/lib/zBase0.r')  # xxx: hard coded path to zBase.r
		source('T:/work/UseR-2013/lib/zCode.r')  
		#source('T:/work/UseR-2013/lib/zBase1.r')  
		#source('T:/work/UseR-2013/lib/zStats.r')  
	} else {
		library(zBase0)
		libra(zCode)
		'%+%' = function(x, y) paste(x, y, sep= "")
	}

	
	libra(plyr)  ##  = install if necessary && require

	#libra(R2HTML) 
	#libra(XLConnect)
	
	#	libra(Defaults)
	#	setDefaults(legend, bty='n')
	#	setDefaults(symbols, inc=.15)
	
	libra(RColorBrewer)  # ww(); display.brewer.all()
	palette(c(adjustcolor(cn('grey50 green3 red2'), alpha.f = .9), brewer.pal(8,"Dark2")))  ##ex: plot(1:19, pch=16, col=1:19, cex=3)
	
	#libra(randomForest)
	#libra(plotrix)  # addtable2plot
	
	libra(ggplot2)  
	# libra:: Call library  ggplot2
	

	#libra(scales)
	#libra(Cairo);  
	
	proot= fp(root, '97_tutorial-demo')  # project root
	sw(fp(proot, 'out'))
	
		
	theFile= fp(proot, '97_tutorial-demo.r')
	
	sg.bak = sg   ; sg = dummy    # to rerun w/o overwrite images   
	sgj.bak= sgj  ; sgj= dummy    # to rerun w/o overwrite js images   
	sa.bak = sa   ; sa = dummy    # to rerun w/o overwrite out data 
	
	if(toSavePicsAndOtFiles <- 0){
		sg= sg.bak      
		sgj= sgj.bak    
		sa= sa.bak      
	}

	# dett()   ## detach redundant packages
	# gff('saved:')
	# rmDF(); loo(); lo(); lsDF(F);

} #--
##########################################################


# TODO: no plot init()

#=  Classic Header 1
#=  Classic Header 2  =
#==  Classic Header 3  ==
#===  Classic Header 4  ===
#====  Classic Header 5  =====


#====  Demo Code2HTML() - Header 1 ====
#===  Example Header 2 ===
#==  Example Header 3  ==
#=  Example Header 4 . Helper functions  = 

if(0){
	#'  ===  Key points and helpers  ===
 
	#'  == Create Project       #  Wrapper for     
           CreateProject()      #  Create new Project
           libra()              #  install.packages + library()
           theFile              #  global variable - current R code
	                               
	#'  == Remind objects:         
           DT()                 #  strftime(Sys.time()) - current Date and Time
           st({})               #  system.time + play sound - for long executed blocks
           hee()                #  nrow + head
           sg()                 #  dev.print  -  save graphics to .png file
           srm()                #  save & remove
           ##  ^RV              #  copy output from console to the code
	                               
	#'  == Save state:             
           sa()                 #  save.image
           Code2HTML()          #  R code theFile to html R Work Journal
           MakeRWJournals()     #     - the same for many R files to create
           createRWJalbum()     #  	 albums of galleries	
           ReleaseOut()         #  move all output to a DateTime-version folder before new data
	#'       ///                #  exit location -  mark place in the file 
	     
	#'  == Restore  state:
           rmall()              #  rm all
           rmDF()               #  rm datafames and lists
           init                 #  initialise environment
           loo(); gff('saved:') #  find saved data
           lo()                 #  load saved data
           lsDF()               #  ls  data frames
	                               
	#'     Convenience, aliases #  Wrapper for    
           tocsv()              #  write.csv 
           totsv()              #  write.table
           suss()               #  subset + grep
           gre2()               #  grep
           df()					#  data.frame
		   
	#'  == Code in top and bottom of the R file.  - "cache", "parking lot"
}



#==  Code2HTML() Features:  ==
#'		1. Transforms .R file into  self-documented  .html file, containing all R code with output pics, headers and table of contents.
#'		2. The titles in body and contents are clickable to navigate from contents to body and back.
#'		3. The pics are clickable to resize.
#'		4. The html file has partly R syntax highlighted.  It is possible to do the full R syntax highlighting in resulting html, but the result file becomes almost twice heavier.
#'		5. Parts of the result html file could be folded.
#'		6. If you in browser fold TOC, select all, copy and paste to a text editor,  you should get the pure original R file.
#'		7. If you in browser select all, copy and paste to a MS Word, pics are resizable.

#== Eclipse + StatET vs RStudio  ==
' 	Pro:
	-	Ctrl+R, V
	-   rectangular selection Alt-Shift-A
	-	Search  ^H
	-	Multi-win, monitors
	-	Also py, java, html, pig, hive, svn, git,  …
	-	Full screen view on click 
'



{ #===  Block Header 2  === 2013-07-03 12:18:09"

	
	{ #==    Block Header 3  ==  DT()  2013-06-24 13:46
		st({  ## looong calculation
					for(i in 1 ){   # 1:5e4
						x= 1:1000 / 100
						y= x^2 + rnorm(le(x),, 9)
					}
		})  #   5.05



		hee(df(x, y))
		# 1000  rows
		#      x       y
		# 1 0.01 -1735.9
		# 2 0.02   757.7
		# 3 0.03   670.2
		# # he(suss(,, df(x, y), , sel= cn("x y")), 5)


		ww()  ## open window
		plot(x, y, ma= expression(Noisy ~ x^2  ~', ' ~ sigma ~ '= 9'))
		lines(lowess(y ~ x), lty=3, lwd=4, col=3)
		sg('Noisy x^2')
	    # Pic_1. Noisy   $ x^2, \; \sigma= 1000 \quad ( \LaTeX ) $ 

		# xPic_3. Noisy x^2   ## redundant pic
		
		#=  "roxigen" type comments and TODO,  Header 4  =
		#' Some commenting text after "roxigen" type comments
		#' can be here
		#' xxx: We need To Do something - include funcs from zBase.r; wrong borders of block 3
		#' TODO: something else - demo for hee(), sa(), lo(), lsDF()
	} #--
	
	
	#==  Usage of  $ @LaTeX $ and quoted multiline text  == 
	{ 
		'
		Any text can be 
		between quotes or backticks, but if we need 
		to use $ @LaTeX $ formulae, we should use symbol "@" instead of "\\",
		including inline  $ y= 5 @sqrt{x_4^3} $ as well as stand along equations: 
		
		@begin{aligned}
		@nabla @times @vec{@mathbf{B}} - @, @frac1c@, @frac{@partial@vec{@mathbf{E}}}{@partial t} & = @frac{4@pi}{c}@vec{@mathbf{j}} @@   @nabla @cdot @vec{@mathbf{E}} & = 4 @pi @rho @@
		@nabla @times @vec{@mathbf{E}}@, + @, @frac1c@, @frac{@partial@vec{@mathbf{B}}}{@partial t} & = @vec{@mathbf{0}} @@
		@nabla @cdot @vec{@mathbf{B}} & = 0 @end{aligned}
		
		$$  g@frac{d^2u}{dx^2} + L@sin u = 0 ; @quad  @sum_{n=1}^@infty {1@over n^2} = {@pi^2@over 6}  $$
		or
		@[
		@int@limits_{x^2 + y^2 @leq R^2} f(x,y)@,dx @,dy
		= @int@limits_{@theta=0}^{2@pi}@ @int@limits_{r=0}^R f(r@cos@theta,r@sin@theta) r@,dr@,d@theta  
		@]
		or
		$$
		@int@limits_{x^3 + y^2 @leq R^2} f(x,y)@,dx @,dy
		= @int@limits_{@theta=0}^{2@pi}@ @int@limits_{r=0}^R f(r@cos@theta,r@sin@theta) r@,dr@,d@theta  
		$$
		
		'
		
		`
		    or between backticks:
			<div style="font-size: 250%;">
		    $$ y = @sum{x_i^2}  $$ 
	
	        $ @href{http://www.forkosh.com/mimetextutorial.html}{try} @quad @href{http://www.codecogs.com/latex/eqneditor.php}{@LaTeX} $ 
	
			during  <a href="http://www.edii.uclm.es/~useR-2013/"> UseR! - 2013</a>
			</div>
		`= 0
		
		
		#	 2. Behind reqular "#" or roxigen "#'" comments we can use single "\"  
		#		in  $ \LaTeX $  expressions, including inline  $ y= 5 \sqrt{x_4^3} $ as well as stand alone equations. 
					                                                                                                                                                                                 
		
		#'			or behind roxigen "#'" comments: 
		#'			
		#'			$$  g\frac{d^2u}{dx^2} + L\sin u = 0 ; \quad  \sum_{n=1}^\infty {1\over n^2} = {\pi^2\over 6}  $$
		#'			or
		#'		$$	\int\limits_{x^2 + y^2 \leq R^2} f(x,y)\,dx\,dy  = \int\limits_{\theta=0}^{2\pi}\ \int\limits_{r=0}^R f(r\cos\theta,r\sin\theta) r\,dr\,d\theta   $$
		
	}



	plot2= function(){ #==    Block Header 3 - 2 - inside function definition ======================
		group <<- sample(cn('A B'), le(x), re=T)
		y2 <<- - x^2 + 20 * (group == 'B') + rnorm(le(x),, 5)
		y3 <<- x^2 * cos(x) + 10 * (group == 'B') + rnorm(le(x),, 5)
		
		ww()
		#plot(x, y2, ma='Noisy -x^2', col= 1+ nuf(group), pch=20)
		plot(x, y3, ma='Noisy x^2 * cos(x)', col= 1+ nuf(group), pch=20)
		lines(lowess(y2 ~ x), lty=2, lwd=4, col=6)
		lines(lowess(y3 ~ x), lty=2, lwd=4, col=7)
	} #--
	plot2()
	
	sg('Noisy  $ -x^2 $ ',, F)
	# Pic_2. Noisy  $ -x^2 $ 

	# xPic_3. Noisy   $  -x^2 $ 
	
	{ #==  ggplot,   Block Header 3 ==
		ww()
		str(x)
		str(y2)
		str(group)
		qplot(x, y2, col= fa(group), main='Noisy -x^2', geom=cn('point smooth'))
		#  ggplot(df(x, y2), aes(x = x, y = y2)) +	geom_point()
		sg('ggplot: Noisy  $ x^2 $ ', gg=T)
		# Pic_3. qqplot: Noisy  $ - x^2 $ 

		# xPic_2. qqplot: Noisy - x^2
	} #--
	
	sa()
	# 2013-07-03 12:39:18:: Image saved: lo('T:/work/UseR-2013/97_tutorial-demo/out/ABC.RData'); sw('T:/work/UseR-2013/97_tutorial-demo/out')  # rmDF(); lsDF(); dir(); expl()

	cc()	
	
	##  now we can leave the project ----

	
	xyy= df(x, y1= y, y2, group)
	hee(xyy)
	# 1000  rows
	#      x       y1      y2 group
	# 1 0.01  15.5917 24.7899     B
	# 2 0.02   3.1837  2.0923     A
	# 3 0.03   3.1982  7.3430     A
	# 4 0.04  -4.8503 30.4973     B
	# # he(suss(,, xyy, , sel= cn("x y1 y2 group")), 5)


	hee(suss('y',, xyy, y1 > 5, sel= cn("x y1 y2 group")), 5)
	# 793  rows
	#        y1     y2
	# 1  15.592 24.790
	# 6   8.528 22.679
	# 8  20.299 14.569
	# 11  8.393 -9.475
	# 17 12.018 -2.363



	
	ww()
	plot(xyy, col= 1+nu(xyy$group))
	mtext('Pairs',,3)
	sg('Pairs xyy')
	# Pic_4. Pairs xyy
	

	sa('.with_xyy')  ##  xyy is large and rare used
	# 2013-07-04 19:25:54:: Image saved: lo('T:/work/UseR-2013/97_tutorial-demo/out/.with_xyy.RData'); sw('T:/work/UseR-2013/97_tutorial-demo/out')  # rmDF(); lsDF(); dir(); expl()
	

	if (0) { #==  Examples  ==
		
		ca= cars
		co= CO2
		ir= iris
		ba= plyr::baseball
		
		lsDF(0)
		
		#'     Convenience, aliases  #  Wrapper for    
		tocsv(ca)               #  write.csv 
		totsv(co)               #  write.table
		na(ir)                  #  names
		nc(ir)                  #  paste names
		suss('^S', 'W', ir, Sepal.Length >5) #  subset + grep(names())
		hee(srt(suss('^S', 'W', ir, Sepal.Length >5), ~- Sepal.Length))#  head + order + suss
		
		na(ba)                  #  names
		hee(ba)
		# 21699  rows
		#            id year stint team lg  g  ab  r  h X2b X3b hr rbi sb cs bb so ibb hbp sh sf gidp
		# 4   ansonca01 1871     1  RC1    25 120 29 39  11   3  0  16  6  2  2  1  NA  NA NA NA   NA
		# 44  forceda01 1871     1  WS3    32 162 45 45   9   4  0  29  8  0  4  0  NA  NA NA NA   NA
		# 68  mathebo01 1871     1  FW1    19  89 15 24   3   1  0  10  2  1  2  0  NA  NA NA NA   NA
		# # he(suss(,, ba, , sel= cn("id year stint team lg g ab r h X2b X3b hr rbi sb cs bb so ibb hbp sh sf gidp")), 5)
        
	    xx= ncc('id|g|b$', 'X', ba)  # paste + gre + colnames 
		# [1] "id lg g ab sb bb ibb gidp"
	
	    cn(xx)                       # strsplit 
		# [1] "id"   "lg"   "g"    "ab"   "sb"   "bb"   "ibb"  "gidp"



		hee(srt(suss('id|g|b$', 'X', ba, g >160), ~id))  #  head + order + suss
		# 220  rows
		#              id lg   g  ab sb bb ibb gidp
		# 43076 aaronha01 NL 161 631 31 78  18   11
		# 43826 allendi01 NL 162 632  3 67  13    8
		# 44582 allendi01 NL 161 619 15 74   6   13
		# 68530 alomaro01 AL 161 637 53 57   3    5
		# 48170 alomasa01 AL 162 672 35 49   2    7
		# 49084 alomasa01 AL 162 689 39 41   4    7
		# 47712  alouma01 NL 162 698 22 42   9    5
		# 69623 baergca01 AL 161 657 10 35  10   15
		# 70191 bagweje01 NL 162 586 10 84  13   17
		# # he(suss(,, srt(suss("id|g|b$", "X", ba, g > 160), ~id), , sel= cn("id lg g ab sb bb ibb gidp")), 5)

		gre2()                #  grep
		
	}
	
	if(1){ #==  rCharts   == 
		#== Installation of rCharts ==
		# libra(devtools)
		#		package 'brew' successfully unpacked and MD5 sums checked
		#		package 'formatR' successfully unpacked and MD5 sums checked
		#		package 'markdown' successfully unpacked and MD5 sums checked
		#		package 'httr' successfully unpacked and MD5 sums checked
		#		package 'RCurl' successfully unpacked and MD5 sums checked
		#		package 'memoise' successfully unpacked and MD5 sums checked
		#		package 'whisker' successfully unpacked and MD5 sums checked
		#		package 'roxygen2' successfully unpacked and MD5 sums checked
		#		package 'knitr' successfully unpacked and MD5 sums checked
		#		package 'devtools' successfully unpacked and MD5 sums checked
		
		# install_github('rCharts', 'ramnathv')  # nOK
		
		# libra(RJSONIO)
		# libra(yaml)
		#  C:\>z\eclipse\R-2.14.2\bin\x64\R.exe  --vanilla CMD INSTALL C:\Users\azolotovitski\Downloads\rCharts-master\rCharts-master
	
		
		require(rCharts)
		p1 <- hPlot(y2 ~ x,  size="x", group="group", data=xyy, type = "line", dom='zzz', title = "rChart", subtitle = "xyy, by group")  # http://www.polychartjs.com/demo?bubble_chart
		p1$chart(zoomType = "xy")
		p1$show()
		
		HHjp(, "rCharts xyy, by group")
		# jPic_1. rCharts xyy, by group
		
	}
	
	 cc()
	 expl("file://T:/work/UseR-2013/97_tutorial-demo/97_tutorial-demo.r.htm")

	
	
} #--





if(0){   #== Misc   (cache, parking lot)
	theFile= 'T:/work/UseR-2013/97_tutorial-demo/97_tutorial-demo.r'
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	
	theFile= fp(proot, '97_tutorial-demo.r')
	
	CreateNewProj(newProj.name= '95_abc', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplName', root='T:/work/UseR-2013')
	cc()
	ReleaseOut(vers='.b', exec= T)
	ReleaseOut(vers='', exec= T)
	
	source(theFile)
} #--
