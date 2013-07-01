#! /usr/bin/Rscript --vanilla --slave
# Project  : 97_tutorial-demo
# File     : 97_tutorial-demo/97_tutorial-demo.r
# Author   : Alex Zolotoviski, alex@zolot.us
# Created  : 2013-06-24 02:55:55
# License  : GPL-2  
###############################################################################

## before:  CreateNewProj(newProj.name= '97_tutorial-demo', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplName', root='T:/work/UseR-2013')


{ #== init ===
	rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
	
	options(help='html', digits=4, width=2160, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe'
			, error= NULL) # options(error= recover) options(error=dump)  #or options(error=NULL)

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
	}
	
	
	
	libra(plyr)  ##  = install if necessary && require

	#libra(R2HTML) 
	#libra(XLConnect)
	
	#	libra(Defaults)
	#	setDefaults(legend, bty='n')
	#	setDefaults(symbols, inc=.15)
	
	libra(RColorBrewer)  # display.brewer.all()
	palette(c(adjustcolor(cn('grey50 green3 red2'), alpha.f = .9), brewer.pal(8,"Dark2")))  ##ex: plot(1:19, pch=16, col=1:19, cex=3)
	
	#libra(randomForest)
	#libra(plotrix)  # addtable2plot
	
	libra(ggplot2)  # no package called 'digest'
	#libra(scales)
	#libra(Cairo);  
	
	proot= fp(root, '97_tutorial-demo')  # project root
	sw(fp(proot, 'out'))
	
		
	theFile= fp(proot, '97_tutorial-demo.r')
	
	HHp.bak= HHp   ; HHp=dummy ; #  HHp= HHp.bak   # to rerun w/o change images
	HHjp.bak= HHjp ; HHjp=dummy; #  HHjp= HHjp.bak   # to rerun w/o change images
	sa.bak= sa     ; sa=dummy  ; #  sa= sa.bak   # to rerun w/o change images


	# dett()   ## detach redundant packages
	# gff('saved', theFile)
	# rmDF(); loo(); lo(); lsDF();

} #--
##########################################################

# TODO: room around (-)


#====  Demo Code2HTML() - Header 1 ====
#'  === Key points 
#'  == CreateProject()
#'     init, libra()
#' 
#'  == Remind objects:
#'     DT(), st(), hee(), HHp()
#'  
#'  == Save state:
#'     sa()
#'     Code2HTML()
#'     exit location
#'  
#'  == Restore  state:
#'     rmall(); rmDF()
#'     init()
#'     loo()
#'     gff('saved')
#'     lo()
#'     lsDF()

#===  Example Header 2  ===
#==  Example Header 3  ==
#=  Example Header 4  =

#==  Features:  ==
#'		1. Transforms .R file into  self-documented  .html file, containing all R code with output pics, headers and table of contents.
#'		2. The titles in body and contents are clickable to navigate from contents to body and back.
#'		3. The pics are clickable to resize.
#'		4. The html file has partly R syntax highlighted.  It is possible to do the full R syntax highlighting in resulting html, but the result file becomes almost twice heavier.
#'		5. Parts of the result html file could be folded.
#'		6. If you in browser fold TOC, select all, copy and paste to a text editor,  you should get the pure original R file.
#'		7. If you in browser select all, copy and paste to a MS Word, pics are resizable.

#=  Classic Header 1
#=  Classic Header 2  =
#==  Classic Header 3  ==
#===  Classic Header 4  ===
#====  Classic Header 5  =====




{ #===  Block Header 2  === 
	
	{ #==    Block Header 3  ==  DT()  2013-06-24 13:46
		st({  ## looong calculation
					n= 100
					#for(i in 1:1e6){
					for(i in 1:1e1){
							x= 1:n
						y= x^2 + rnorm(le(x),, 1000)
					}
		})  #  13.04

		hee(df(x, y))
		# 100  rows
		#   x       y
		# 1 1  -78.76
		# 2 2  308.67
		# 3 3 -279.14
		# # he(suss('y',, df(x, y), y < 0, sel= cn("x y")), 5)



		
		ww()  ## open window
		plot(x, y, ma= expression(Noisy ~ x^2~', ' ~ sigma ~ '= 1000'))
		lines(lowess(y ~ x), lty=3, lwd=2, col=3)
		HHp('Noisy x^2')
	    # Pic_1. Noisy   $ x^2, \; \sigma= 1000 \quad ( \LaTeX ) $ 

		# xPic_1. Noisy x^2   ## redundant pic
		
		#=  Header 4  =
		#' Some commenting text
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
	'
	
	`
	or between backticks:
	
	<div style="font-size: 250%;">
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
	</div>
	
    $ @href{http://www.forkosh.com/mimetextutorial.html}{try} @quad @href{http://www.codecogs.com/latex/eqneditor.php}{@LaTeX} $ 
	
	`= 0
	
	
#	 2. Behind reqular "#" or roxigen "#'" comments we can use single "\"  
#		in  $ \LaTeX $  expressions, including inline  $ y= 5 \sqrt{x_4^3} $ as well as stand alone equations. 
#			
#			                                                                                                                                                                                 

#'			or behind roxigen "#'" comments: 
#'			
#'			$$  g\frac{d^2u}{dx^2} + L\sin u = 0 ; \quad  \sum_{n=1}^\infty {1\over n^2} = {\pi^2\over 6}  $$
#'			or
#'		$$	\int\limits_{x^2 + y^2 \leq R^2} f(x,y)\,dx\,dy  = \int\limits_{\theta=0}^{2\pi}\ \int\limits_{r=0}^R f(r\cos\theta,r\sin\theta) r\,dr\,d\theta   $$

}



	plot2= function(){ #==    Block Header 3 - 2 - inside function  ==================================================== 
		group <<- sample(cn('A B'), le(x), re=T)
		y2 <<- - x^2 + 3000 * (group == 'B') + rnorm(le(x),, 2000)
		
		ww()
		plot(x, y2, ma='Noisy -x^2', col= 1+ nuf(group), pch=20)
		lines(lowess(y2 ~ x), lty=3, lwd=3, col=6)
	} #--
	plot2()
	
	HHp('Noisy  $ -x^2 $ ',, F)
	# Pic_2. Noisy  $ -x^2 $ 

	# xPic_3. Noisy   $  -x^2 $ 
	
	{ #==  Block Header 3 - 3  - ggplot  ==
		ww()
		qplot(x, y2, col= fa(group), main='Noisy -x^2', geom=cn('point smooth'))
		HHp('qqplot: Noisy  $ x^2 $ ')
		# Pic_4. qqplot: Noisy  $ - x^2 $ 

		# xPic_2. qqplot: Noisy - x^2
	} #--
	
	sa()
	# 2013-06-24 14:54:46:: Image saved: lo('T:/work/UseR-2013/97_tutorial-demo/out/.RData'); sw('T:/work/UseR-2013/97_tutorial-demo/out')  # rmDF(); lsDF(); dir(); expl()
	cc()
	##  now we can leave the project ----

	
	xyy= df(x, y1= y, y2, group)
	hee(xyy)
	# 100  rows
	#   x      y1      y2 group
	# 1 1  -78.76  1833.8     B
	# 2 2  308.67  -911.7     A
	# 3 3 -279.14   136.7     A
	# # he(suss(,, xyy, , sel= cn("x y1 y2 group")), 5)
	he(suss('y',, xyy, y1 > 0, sel= cn("x y1 y2 group")), 5)

	
	ww()
	plot(xyy, col= 1+nu(xyy$group))
	mtext('Pairs',,3)
	HHp('Pairs xyy')
	# Pic_3. Pairs xyy
	

	sa('with_xyy')  ##  xyy is large and rare used
	# 2013-06-24 14:56:15:: Image saved: lo('T:/work/UseR-2013/97_tutorial-demo/out/with_xyy.RData'); sw('T:/work/UseR-2013/97_tutorial-demo/out')  # rmDF(); lsDF(); dir(); expl()
	
	
	{ #==  rCharts   == 
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
		#p1 <- hPlot(y1 ~ x,  size="x", group="group", data=xyy, type = "bubble", dom='zzz', title = "rChart", subtitle = "xxy")  # http://www.polychartjs.com/demo?bubble_chart
		#p1 <- rPlot(conc ~ uptake | Treatment, data = CO2, color = "Plant", size="uptake", type = "point", dom='zzz')  # http://www.polychartjs.com/demo?bubble_chart
		p1$chart(zoomType = "xy")
		p1$show()
		
		HHjp(p1, "rCharts xyy, by group")
		# jPic_1. rCharts xyy, by group
		
	}
	
	cc()
	# expl("file://T:/work/UseR-2013/97_tutorial-demo/97_tutorial-demo.r.T.htm")

	
	
} #--





if(0){   #== Misc
	theFile= 'T:/work/UseR-2013/97_tutorial-demo/97_tutorial-demo.r'
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	
	theFile= fp(proot, '97_tutorial-demo.r')
	ccc= function()code2HTML(theFile)
	ccc()
	
	CreateNewProj(newProj.name= '94_zzz', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplName', root='T:/work/UseR-2013')
} #--
