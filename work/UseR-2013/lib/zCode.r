# Author   : Alex Zolotoviski, azolotovitski@medio.com
# Created  : 2013-06-24 02:55:55
# License  : GPL-2  


#=====  Funcs for Tutorial UseR!-2013  =====
#=====  Helper Funcs for work with code and file system  =====

# require(zBase0)   # if executed after, overwrites source('zBase')


cc= code2HTML= code2HTMLjQuery= function(.theFile= theFile, img='img', FullSyntaxHighlight= FALSE
								, classicHeaders=FALSE, show=TRUE, toSave=TRUE, outSuffix='.htm') {
	if(FullSyntaxHighlight){   #== Full syntax highlight ==
		catt('FullSyntaxHighlight')
		libra(highlight)
		s1= highlight(.theFile, NULL, renderer = renderer_html( document = TRUE ))
		s1= gsub('\n$','',s1)
		writeLines(s1, .theFile %+% '.FSH.htm')
	}else{s1= readLines(.theFile, warn=F)
	}  #--
	
	picss= list()
 	s1= gsub('<(\\s)', '&lt;\\1', s1)  # we suppose no blanks after "<" in <tag    in the input R code
	s1= replaceTagsOutSq(s1)  # we suppose  <tag>  only in `` in the  R code
	s1= gsub('@@', '\\\\\\\\', s1)  # drop escapes for LaTeX
	s1= gsub('(^|[^x"])@', '\\1\\\\', s1)  # drop escapes for LaTeX
	
	
	#==  Set id  ==
	for(i in 1:le(s1)){
		if(grepl('^\\s*#\\s* Pic_\\d+', s1[i])) picss[[ch(i)]]= s1[i]
		
		s1[i]= gsub('^([^\\~]*# *"?)(jPic_\\d+)(.*)','\n\n <iframe src="img/\\2.htm" width="100%" height="600px"></iframe>\n\n \\1\\2\\3' , s1[i])
		

		s1[i]= gsub('^([^\\~]*# *"?)(Pic_\\d+)(.*)', sf('
								<img id="%s" class="pic" src="%s/\\2.png" width=700 onClick="resize(%1$s);" ondblclick="ShowImg(\'\\2\', \'\\3\');" name="\\2\\3"/>   
								<span class="capt" onClick="goto(\'tnTOC%1$s\');" ondblclick="ShowImg(\'\\2\', \'\\3\')">\\1\\2\\3</span>',  i, img), s1[i])
		s1[i]= gsub('(.*\\s*#==== )(.*)( =+.*)$', sf('\\1<span class="H1" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span>',i,i),  s1[i])
		s1[i]= gsub('(.*\\s*#=== )(.*)( =+.*)$', sf('\\1<span class="H2" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span>',i,i),  s1[i])
		s1[i]= gsub('(.*\\s*#== )(.*)( =+.*)$' , sf('\\1<span class="H3" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span>',i,i),  s1[i])
		s1[i]= gsub('(.*\\s*#= )(.*)( =+.*)$'  , sf('\\1<span class="H4" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span>',i,i),  s1[i])
		s1[i]= gsub('(.*\\s*#=+)([^=]*)$'      , sf('\\1<span class="H5" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span>',i,i),  s1[i])
		s1[i]= gsub('id="(\\d+)" ', 'id="\\1" title="line \\1" ',  s1[i])
	}	
	
	
	if(classicHeaders) for(i in 1:5) {s1= gsub(sf('class="H%s"', i),  sf('class= "H%s"', 6-i), s1)} 
	
	#= div for code folding ===
	depth= countDepth2(s1)
	imgFold= '<img src=\'https://ui.netbeans.org/docs/ui/code_folding/cf_minus.gif\'/>'
	
	s1= ifelse(depth!= 0, gsub('^([^#]*\\{[^\\{\\}]*)', sf('\\1 <a href="javascript:ToggleFold(\'D77\')" class="aD">%s</a><div class="D77">', imgFold), s1), s1)

	s1= gsub('^(.* class="H(\\d)".* id="(\\d+).*)D77(.*) class="D77"' , '\\1D\\3\\4 class="D\\2" id="D\\3"', s1)
	s1= ifelse(depth!= 0, gsub('([^#\\{\\}]*)\\}', '\\1</div><b>}</b>', s1), s1)
	
	attr(s1,".theFile")=  .theFile
	s1= subSingleQuote2div(quote='^[^\\`]*`[^\\`]*$', pattNeg='\'\\`\'|\"\\`\"', repl=c('<span class="sq">', '</span>'), s1)  # starts from `, single ` in line
	s1= gsub(" \\$ (.+) \\$ ",' &nbsp;&nbsp; $ \\1 $ &nbsp;&nbsp; ', s1)    #  inline math formula
	
	
	#==  comments, TODO and xxx  ==
	s1= gsub("^(\\s*#\')(.*)$",'<span class="comment2">\\1</span><span class="text">\\2</span>', s1)  
	s1= gsub('(#[^\'=-].*)$','<span class="comment">\\1</span>', s1)  # code2HTML3()
	s1= gsub('^(.*)(#=+)(.*)$','\\1<span class="comment2">\\2</span>\\3', s1)  # code2HTML3()
	s1= gsub('^(.*)(#\\-\\-)(.*)$','\\1<span class="comment2">\\2</span>\\3', s1)  # code2HTML()
	s1= gsub('^(.*# ?.*)((xxx|TODO):.*)(</span.*)$','\\1<font color="red">\\2</font>\\4', s1)
	
	main= gsub('^([^#]+= ?function)(.*)$','<span class="fun">\\1</span>\\2', s1)  
	
	#==  prepare Table of Contents  ==	
	toc= grep('img id=|<H\\d+|"H[1-5]', s1, v=T)
	toc= gsub('"capt"', '"captTOC"', toc)
	toc= gsub('class="pic"', 'class="tnTOC"', toc)
	toc= gsub('700', '400', toc)  		# pics -> thumbnails
	toc= gsub('resize', 'goto', toc)
	toc= gsub('id="(\\d+)"', 'id="tnTOC\\1"', toc)
	toc= gsub("goto\\('tn(\\d+)'\\)", "goto('\\1')", toc)
	toc= gsub('\\{?( *<H\\d.*</H\\d>).*', '\\1', toc)  # drop braces in TOC
	toc= gsub('(.*)\\{', '<br/>\\1', toc)		# drop braces in TOC
	toc= gsub('<a.*$', '', toc) # drop ends in TOC
	toc= gsub('(.* class= *"H.".*)', '<br/>\\1', toc) # insert new lines
	
	
	#==  prepare Gallery  ==
	pics= laply(na(picss), function(np){p=  picss[[np]]; sf('<img id="tn%s" class="imgGal" src="img/%s.png" name="%s" height=100> '
						, np, gsub('.* (Pic_[0-9]+).*', '\\1', p)
						, gsub('# *', '', p))})
	
	header= c('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en"><head><link rel="shortcut icon" href="/favicon.ico">

				<!-- we need these 2 lines for the case if no Internet during the tutorial -->
				<script src="T:/mathjax-MathJax-v2.2-8-g727332c/mathjax-MathJax-727332c/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
				<script src="T:/work/UseR-2013/lib/jquery-1.10.1.min.js"></script>
				<!--  -->
                
				<script src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
				<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>


				<!--script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.js"></script-->
             
				 <script type="text/x-mathjax-config">  MathJax.Hub.Config({ tex2jax: { inlineMath: [[" $ "," $ "]] } }) </script>
				
				<!-- script>
					$(function(){
					//$(".imgGal").draggable() ; //.parent().resizable();
					$(".dimg").resizable().draggable(); 
					// $(".pic").draggable();  //.resizable();;
					});
				</script -->
				
				
				<script>
				$(function(){
					var d= document

				//var imgFold= "<img src=\'https://ui.netbeans.org/docs/ui/code_folding/cf_minus.gif\'/>"  // "(-)"
				var imgFold= $(".aD:first").html()

				$("img.imgGal")
					.mouseover(function() {show(this)} )
					.mouseout(function() {show(0)} )
				    .click(function() {goto(this.id.replace("tn",""))})
	                .dblclick(function() {ShowImg(this.name.replace(/\\..*/, ""),  this.name.replace(/.*?\\./, "")); })
				;
				$(".aD").click(function() { th= $(this)
				th.next("div").toggle();
				//alert(th.html()+ "\\n" + imgFold)

				th.html(th.html()== imgFold ? "..." : imgFold)
				})
                $("#aToggleAll").click(function() {th= $(this)
		        if( th.text() =="(+)") {
		        $("div").show(); th.html(imgFold); $("a:not(.comments)").html(imgFold)   
		        } else { $("div").hide(); th.html("(+)"); $("a:not(.comments)").html("(+)")}
                })
                //   $(".aToggleAllPic").click(function() {$("img").toggle() })
				
				function toggleD(a, a2, di){ if( a.text() == "(+)") {
				di.show(); a.html(imgFold); a2.html(imgFold)  
				} else { di.hide(); a.html("(+)"); a2.html("...")}
				}
				
				function ToggleComments2(){ 
				var tt = document.getElementsByClassName("comment2"); 
				var t0= tt[0].style.fontSize;
				di= (t0 == "3px" || t0 == "") ? "12px" : ((t0 == "12px")? "1px" : "3px") ;
				fc=  di == "1px" ? "white" : "green";
				for (var i = 0; i < tt.length; i++) {tt[i].style.fontSize = di; tt[i].style.color=fc;}
				}

                $(".aToggle.DD1").click(function() {toggleD($(this), $(".H1 + + .aD"), $(".D1")) })
                $(".aToggle.DD2").click(function() {toggleD($(this), $(".H2 + + .aD"), $(".D2")) })
                $(".aToggle.DD3").click(function() {toggleD($(this), $(".H3 + + .aD"), $(".D3")) })
                $(".aToggle.DD4").click(function() {toggleD($(this), $(".H4 + + .aD"), $(".D4")) })
                $(".aToggle.TOC").click(function() {toggleD($(this), $(".zz"), $("div.TOC")) })
                $(".aToggleAllPic").click(function() {toggleD($(this), $(".zz"), $("img")) })
                $(".aToggleComments").click(function() {
					ToggleComments2(); return;

					c2= $(".comment2");	fs= c2.eq(0).css("font-size");
					var tt = document.getElementsByClassName("comment2"); var t0= tt[0].style.fontSize;
					//alert(fs + " | " + tt[0].innerHTML + " zz " + t0)
					//alert(fs + " | " + t0.innerHTML)
					//fsn= (fs == "3px" || fs == "") ? "12px" : ((fs == "12px")? "1px" : "3px");
					fsn= (fs == "30px" || fs == "") ? "120px" : ((fs == "120px")? "10px" : "30px");
					//alert(fs + " | " + tt[0].innerHTML + " t0: " + t0 + " fsn:" + fsn+ "|"+ " sp:" + $(".comment2:first + span").text())
					$("#out").text(" || fs:" +fs + " | " + tt[0].innerHTML + " t0: " + t0 + " fsn:" + fsn+ "|"+ " sp:" + $(".comment2:first + span").text());
					$(".comment2").css({"font-size": fsn, "font-size-adjust": .1});
					//$(".comment2").css("font-size", "xx-small");
					$(".comment2").text("zz");
					$(".comment2").css("color",  c2.css("font-size")== "1px" ? "white" : "green")
					})
				
				});
				</script>
				
				<script><!--
				var d= document;
				
				function goto(i) {w= d.location= "#" + i;}
				
				function ShowSVG(p){w= window.open("","","fullscreen=yes")
				w.document.write("<html><iframe src=\'./img/"  + p + ".svg\' width=1200  height=800/></html>")
				w.focus()
				}
			    
				function ShowImg(p, capt){w= window.open("","","type=fullWindow,fullscreen,location=\'\',height="+ screen.height+ ",width="+screen.width)
				w.document.write("<html><img src=\'./img/"  + p + ".png \' /><br/>" + p +  capt + "</html>")
				w.document.title= p + capt;
				w.focus()
				}
				
				function resize(i) {e=d.getElementById(i); w= e.width;
				e.style.width= (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? "100%" : 700;
				}
				
				
				function ToggleFold(d) {}
				
				
				function show(e){
				$("#show").css({display : (e==0)? "none" : "block", border: (e==0)? "0" : "2px solid blue"})
				
				if(e != 0){
                x= Math.min(e.offsetLeft+ e.width + 10, 500) //x= e.x+ e.width + 10;// ; if(x > 500) x= 500;
                y= e.offsetTop+ e.height + 10               //y= e.y+ e.height + 10
				
				$("#show").css({left: x, top: y})
				$("#showPic").attr("src", e.src)
				$("#showTxt").html("<br/>" +  e.name)  //  +  e.alt)
				
				/* show properties of e 
				a=""
				for(k in e) if(1 || e[k] > 10 && e[k]+" " < "a") a= a + "      e[" + k + "]= " + e[k];
				alert (a);
				/**/
			
				};
				}
				
				//--></script>
				
				<style>  /*  www.w3schools.com/cssref/css_colornames.asp  www.tizag.com/cssT/border.php */
				div.main {font-family: monospace; white-space: pre;}
                .D1, .D2, .D3, .D4, .D5  {background-color:rgba(255,240,240, .2); border-style:ridge; margin:5px; padding:15px;-moz-border-radius:5px; border-radius:5px} /*div {; opacity: 0.3; background-color:GhostWhite; border-left-style:ridge;}  */
				div.TOC {background-color:rgb(248,248,255); }	
				div.D5 {font-size:8px;} 
				//.sq {font-size:12px; font-family: Arial; color:DodgerBlue } 
				center, .capt {font-size:12px; font-weight:bold; font-family: Arial; margin:auto; text-align:center;}
				.captTOC {font-size:12px; font-weight:bold; font-family: Arial; align:left}
				H1,  H2,  H3,  H4,  H5 {color:blue; font-family: Arial; color:teal;}
				.H1, .H2, .H3, .H4, .H5 {color:teal; font-family: Arial; font-weight:bold;  display:inline-block; display:-moz-inline-box;}
				H1, .H1 {font-size:36px;  margin-top:36px;  margin-bottom:8px;  margin-left:0px; } 
				H2, .H2 {font-size:28px;  margin-top:28px;  margin-bottom:5px;  margin-left:20px;} 
				H3, .H3 {font-size:24px;  margin-top:15px;  margin-bottom:3px;  margin-left:40px;}
				H4, .H4 {font-size:18px;  margin-top:12px;  margin-bottom:2px;  margin-left:60px;}
				H5, .H5 {font-size:14px;  margin-top:10px;  margin-bottom:1px;  margin-left:80px;}
				img.pic, .captTOC, img.tnTOC {margin-top:0px;   margin-bottom:0px;  margin-left:80px;}
				span.captTOC, img.tnTOC { display: block;}
				.D77 { display:inline; }
				img, .aD, iframe {border-style:none; border:0}
				/* .dimg{height:90px;  display:inline-block;} /* position:fixed;  position:absolute;*/ 
                .imgGal{margin-top:1px;   margin-bottom:1px;  margin-left:1px; height:100px} /*height:100%; max-width:100%; max-height:100%; }/* position:absolute; fixed;*/ 
				
				.fun {color:indigo; font-weight:bold;}
				.comment {color:green; align:left}
				.comment2 {color: rgb(100,200,100); font-size:3px; -webkit-text-size-adjust: none;}
				.text, .sq {color: teal; font-size:12px;}
				</style>
				</head><body>') 
		
		footer=	'<div id= "show" style= "position: absolute; left:-1000px; top:1px; border:solid blue; padding:5; display:none; background-color:white; -moz-border-radius: 10px; border-radius: 10px;">
				<img id="showPic" src="" height="400px" style="left:0px"/><br/>
				<span id="showTxt"></span>
				</div></body></html>'

		out= c(header, '		# <a href="#" id= "aToggleAll">Fold</a> All |
				<a href="#" class= "aToggleAllPic">Toggle</a> all pics | 
				<a href="#" class= "aToggle TOC">Fold</a> contents |
				<a href="#" class= "aToggle DD2">Fold</a> H2 |
				<a href="#" class= "aToggle DD3">Fold</a> H3 |
				<a href="#" class= "aToggle DD4">Fold</a> H4 |
				<a href="#" class= "aToggleComments">ToggleComments</a> <span id="out"></span>
				<div class="TOC" id="0"> <H3>Gallery for  ', .theFile, ' </H3>'
			, pics, '<hr/><H2>Contents</H2>', toc, '<hr/></div> <p/><br/><br/>
					<!-- pre --><div class="main">', main, '</div><!-- /pre --><br>##   The HTML output of ', .theFile, ' was created on ', DT(), '; <a href="http://www.mathjax.org/demos/scaling-math/">test MathJax </a>'
			, footer)

		if(toSave){
			writeLines(out, .theFile %+% outSuffix)
			catf('expl("file://%s")', .theFile %+% outSuffix)
			if(show) expl("file://" %+% .theFile %+% outSuffix)
		}
	invisible(list(theFile=.theFile, header=header, pics=pics, toc=toc, main=main, footer=footer, out=out))
}
#cc= function()code2HTML(theFile) #  cc()




CleanSparePics= function() { #==  Clean spare Pics  ==
	ff=dir('.', 'png')
	#~  [1] "Pic_1.png"      "Pic_10.png"     "Pic_11 (2).png" "Pic_11.png"     "Pic_12 (2).png" "Pic_12.png"     "Pic_13 (2).png" "Pic_13.png"     "Pic_14 (2).png" "Pic_14.png"     "Pic_15.png"     "Pic_16.png"     "Pic_17.png"     "Pic_18.png"     "Pic_19.png"     "Pic_20.png"     "Pic_21.png"     "Pic_22.png"     "Pic_23.png"     "Pic_24.png"     "Pic_25.png"     "Pic_26.png"     "Pic_27.png"     "Pic_28.png"     "Pic_29.png"     "Pic_3.png"      "Pic_31.png"     "Pic_32.png"     "Pic_33.png"     "Pic_34.png"     "Pic_35.png"     "Pic_36.png"     "Pic_37.png"     "Pic_38.png"     "Pic_39.png"     "Pic_4.png"      "Pic_40.png"     "Pic_41.png"     "Pic_43.png"     "Pic_44.png"     "Pic_45.png"     "Pic_46.png"     "Pic_47.png"     "Pic_48.png"     "Pic_49.png"     "Pic_50.png"     "Pic_51.png"     "Pic_52.png"     "Pic_53.png"     "Pic_54.png"     "Pic_55.png"     "Pic_6.png"      "Pic_7.png"      "Pic_8.png"      "Pic_9.png"     
	
	ps= gsub('^.*(Pic_\\d+).*$', '\\1', ff)
	s=readLines(theFile)
	used= gsub('^.*(Pic_\\d+).*$', '\\1', grep('Pic_', s, v=T))
	spare= unique(nin(ps, used))
	spare.files= laply(ff, function(f) any(laply(spare, function(s) grepl(s, f))))
	any(laply(spare, function(s) grepl(s, 'Pic_1.png')))
	gw()
	# gw: sw("m:/95_TMob_LTV-2smpAct/in")
	
	dir.create('spare')
	for(f in ff[spare.files])file.rename(f, sf('spare/%s', f))
} #--


ReleaseOut= function(vers='', exec=F) {
	stopifnot(basename(gw()) ==  "out" && dirname(gw()) == dirname(theFile))
	
	outRelDir= sf('../out-%s%s', DT("%Y-%m-%d.%H-%M"), vers)
	outFile= sf('%s/%s-%s.htm', gsub( basename(theFile), basename(outRelDir), theFile), basename(theFile), sf('%s%s', DT("%Y-%m-%d.%H-%M"), vers))
	catf('\nreleaseOut: outRelDir= %s, outFile=%s\n', outRelDir, outFile)
	catt('../img', ' --> ', sf('%s/img', outRelDir))
	catt(sf('%s.htm', theFile), ' --> ', outFile)
	
	if(exec){
		dir.create(outRelDir)
		stopifnot(file.copy(theFile, outRelDir))
		stopifnot(file.rename('../img', sf('%s/img', outRelDir)))
		try(file.rename(sf('%s.htm', theFile), outFile), s=T)
		sw('..', showWarnings=F)
		stopifnot(file.rename('out', gsub('\\.\\.', '.', outRelDir) %+% '/out'))
		sw('out')
	}
}
# ReleaseOut(vers='.b', exec=T)



#' =  make RWJournals =
#' res<<-  is  produced as a side effect for case of error in cycle!!!
MakeRWJournals= function(root='T:', patt='.*', pattNeg='^$', exec=F, ...) {
	warning('List res<<-  is produced as a side effect for case of error in cycle, if(exec) !!!')
	.res= list(); attr(.res, "par")=list(...)
	if(exec) res <<- .res
	on.exit(invisible(.res))
	for(f in  gre2(patt, pattNeg, dir(root, all.files =T, patt='\\.r$', recursive= T))) {
		catf('%3s. %s\n', le(.res), fp(root, f))
		if(exec) res[[f]] <<- .res[[f]] <- code2HTMLjQuery(.theFile=fp(root, f), ...)  else .res[[f]]= 1
	}
	invisible(.res)
}
if(0) RWJournals= MakeRWJournals(root='M:', patt='71_UseR-2013-Tutorial.*59.zz', pattNeg='zExtraPacks|999|scripts|library|lib|fun|Base|code2HTML|86_testShiny'
		, exec=F, show=T, toSave=T, outSuffix='.b.htm')


createRWJalbum= function(RWJournals, fout= '../all.Pic.htm', outSuffix= attr(.res, "par")$outSuffix){  # outSuffix='.b.htm', outSuffix='.htm') {
	out= c(RWJournals[[1]]$header
			, sf('<script>
					$(function(){
								$("h3").each( function() {
											var $th = $(this), href =  this.textContent.replace(/.* ([^ ]+) */, "$1")+ "%s";
											$th.wrap("<a href=\'file:///" + href + "\' >")
										});
								$("img.imgGal").each( function() {
											var $th = $(this), href =  this.alt +"%1$s#"  + this.id.replace("tn","");
											$th.wrap("<a href=\'file:///" + href + "\' >")
										});
						$("h3").dblclick(function() {w= window.open(this.textContent.replace(/.* ([^ ]+) */, "$1")+ "%1$s", "", "fullscreen=yes");  w.focus() })
						$("img.imgGal").dblclick(function() {w= window.open(this.alt +"%1$s#" + this.id.replace("tn",""), "","fullscreen=yes");  w.focus() })
					  });
					</script>', outSuffix)
			, unlist(llply(RWJournals, function(x)c(' <H3>Gallery <SMALL> for  ', (x$theFile), '</SMALL></H3>'  #basename(x$theFile)
										, gsub('src="', sf(' alt="%s" src="file:///%1$s/../', x$theFile), x$pics)
								)))
			, RWJournals[[1]]$footer)
	writeLines(out, fout)
	expl(fout) ; catf('expl("%s")', tools:::file_path_as_absolute(fout))
}	
# createRWJalbum(RWJournals, fout='../all.Pic.35.htm')
# createRWJalbum(RWJournals.42b, fout='../RAlbum.42b.htm', outSuffix='.htm')


gff= function(patt=' ===', pattNeg='gff', f= theFile, withLineNumb= T){ # grep pattern in the file
	catt(3099,'============================ gff:', f)
	s= readLines(f, warn=F)
	ii= grepl(patt, s) & !grepl(pattNeg, s)
	x<- sf('%4s %s', if(withLineNumb) 1:le(s) %+% '.' else '', sub('^\\s*', '',s))[ii]
	prr(x,,F)
	invisible(x)   #ex: gff('IPH2ClickCountThreshold *=', 'OUTPUT|@EnableDebugg', f="M:/73_ShivaAegisAdjust/AegisCustomDataSourceView.templ-J.script")
}

#'  set of funcs to extract regex, wrappers for regexpr
rege= function(patt, x) {y=regexpr(patt, x, 1);  substr(x, y, y + attr(y, "match.length")-1)}
#rege('b.', cn('abcd xy 23b67b8'))
# [1] "bc" ""   "b6"

# or
rege= function(patt, x) {yy= regexec(patt, x, 1); laply(seq_along(yy), function(i){y=yy[[i]][1]; substr(x[i], y, y + attr(yy[[i]], "match.length")[1]-1)})}
#rege('b.', cn('abcd xy 23b67b8'))
# [1] "bc" ""   "b6"

#'   wrappers for regexpr
grege1= function(patt, x) {yy= gregexpr(patt, x, 1)[[1]];  laply(seq_along(yy), function(i){y=yy[i];  substr(x, y, y + attr(yy, "match.length")[i]-1)})}
#grege1('b.', 'abcdbnm')
#grege1('x', 'abcdbnm')
# [1] "bc" "bn"
#  1 
# "" 

# nOK grege= function(patt, x) {yy= gregexpr(patt, x); str(yy); llply(yy, function(z)laply(seq_along(z), function(i){y=z[i];  substr(x, y, y + attr(z, "match.length")[i]-1)}))}

grege= function(patt, x) llply(x, function(z)grege1(patt, z))
# grege('b.', cn('abcd xy 23b67b8 absbeb3'))



#= sub ... in file, used by CreateProj =
fsub= function(fin="M:/83_ScopeR/AegisCustomDataSourceView.script"
		, fout= gsub('$', '-copy$', fin)
		, fileShow= F
		, ...){
	args <- as.list(substitute(list(...)))[-1L]
	
	catt('fin=', fin, ' fout=', fout)
	if(file.exists(fout)){warning(sf('fout %s exists; Do nothing!', fout)); return()}
	
	fi= file(fin, "r")
	s= readLines(fi, warn = F)
	#
	close(fi)
	#	catt(11111)
	#	catt(names(args))
	#	str(args)
	
	for(key in names(args)) s= gsub(key, eval(args[[key]], envir= sys.parent()), s)
	
	catt('fsub: Creating OutputFile:', fout)
	#catf('::: s= gsub(\'\', \'\',  s)\n::: writeLines(he(s,9999), con =\'%s\')', fout)
	#print(s[80:100])
	
	#brr()
	
	if(fout != '') {writeLines(head(s,99999), con = fout)
		#if(fileShow)file.edit(fout)
		if(fileShow)browseURL('file://' %+% fout)
	}
	invisible(s)
}

CreateProj= CreateProject= CreateNewProj= function(newProj.name= 'newProjTemplName', Templ.dir= 'T:/work/UseR-2013/99_commonR/newProjTemplName', root='T:/work/UseR-2013') {
	sw(sf('%s/%s', root, newProj.name))
	dir.create('in')
	dir.create('out')
	
	gw()
	

	for(f in dir(Templ.dir,  patt='newProjTemplName.*')){
		catt(60, f, sf('%s/%s/%s', root, newProj.name, sub('zz', newProj.name, f)))
		#file.copy(fp(Templ.dir,f), gw())
		catt('f=', f)
		#if(grepl('zz', f)) fsub(fin= f
		if(grepl('newProjTemplName', f) & !grepl('doc.?$', f)) {
			fsub(fin= fp(Templ.dir,f)
						, fout= sub('newProjTemplName', newProj.name, f)
						, fileShow= F
						#, zzz= sf('m:/%s/%1$s\\.r', newProj.name)
						#, zzz= sf('%s', newProj.name)
						, newProjTemplName= sf('%s', newProj.name)
						, `00-00-00`= DT())
	
		} else 	file.copy(fp(Templ.dir,f), fp(gw(), sub('newProjTemplName', newProj.name, f)), overwrite= F)	
		
		#file.rename(f, sub('newProjTemplName', newProj.name, f))
		#file.remove(f)
	} 
	
	expl()	
}

if(0) { #= Jun 23, 2013
	# CreateNewProj(newProj.name= '72_Hack', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	# CreateNewProj(newProj.name= '71_UseR-2013-Cont', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	# CreateNewProj(newProj.name= '71_UseR-2013-Tutorial', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	# CreateNewProj(newProj.name= '71_UseR-2013-Mod2Prod', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	# CreateNewProj(newProj.name= 'newProjTemplName', Templ.dir= 'R:/work/R-svn-ass/00_commonR/71_TestProjTemplate/zProjTempl', root='M:')
	CreateNewProj(newProj.name= '97_tutorial-demo', Templ.dir= 'T:/work/UseR-2013/99_commonR/newProjTemplName', root='T:/work/UseR-2013')
	CreateNewProj(newProj.name= '96_aaa', Templ.dir= 'T:/work/UseR-2013/99_commonR/newProjTemplName', root='T:/work/UseR-2013')
}


{ #==  lists of funcs  ==
	#== list of all functions in memory  ==
	allFunInMem= function() sus(ldply(ls(envir= .GlobalEnv), function(x)df(x, cl=class(get(x)))), cl=='function')
	# hee(allFunInMem())
	# 209  rows
	#                   x       cl
	# 1               %-% function
	# 2               %+% function
	# 8               aaa function
	# 9                ab function
	# 10               ad function
	# 11 AllCategFreqDesc function
	# 12      allFunInMem function
	# 13              atl function
	# 14               BA function
	# # he(sus(allFunInMem(), , sel= cn("x cl")), 5)
	
	
	countPatt= function(s= c('{ {b}', 'n s{}', 'n abbab s'), patt='ab'){
		s= gsub( patt, '@', s)
		#catt(s)
		s= gsub('[^@]' , '', s)
		nchar(s)
	} 
	# countPatt(s= c('{ {b}ab', 'n s{}', 'n abbab s'), patt='.b')
	# [1] 2 0 2

	ListPatt= function(s= c('{ {b}ab', 'n s{}', 'n abbab s'), patt='.b'){
		s= gsub( sf('.*?(%s).*?', patt), '#\\1#', s)  #print(s)
		s= grep(patt, unlist(strsplit(s, '#')), v=T)
		x= tab.df(s)
		x
	} 
	# ListPatt(s= c('{ {b}ab', 'n s{}', 'n abbcb s'), patt='.b')
	#    x Freq
	# 1 {b    1
	# 2 ab    2
	# 3 cb    1
	
	
	countDepth2= function(s= c('{{b}', 'n s{}', 'n s'), ch1='{', ch2='}', woComments=T){
		countChar1= function(s= c('{ {b}', 'n s{}', 'n s'), ch1='{') nchar(gsub(sf('[^%s]',ch1), '', s))
		
		if(woComments) s= gsub('#.*', '', s)
		
		n1= countChar1(s, ch1)
		n2= countChar1(s, ch2)
		invisible(depth<- n1-n2)
	}
	
	
	if(0){
		
		#TODO: treat sequence of  #, ', "  correctly
		
		(countDepth2(s= c('{ {b}', 'n s{}}', 'n s'), ch1='{', ch2='}'))
		# [1]  1 -1  0
		
		s2= gsub('#.*', '', s1)
		hee(x<- df(countDepth2(s1), countDepth2(s2), inc=countDepth2(s1)- countDepth2(s2), cs=cumsum(countDepth2(s2))), 100)
		sus(x, inc>0)
		sum(countDepth2(s2))
		# [1] -1
	}

	
	listFuncDef= function(theFile=  "R:/work/R-svn-ass/00_commonR/zBase.r", stoplist='^(c|if|for|function|with|within|exp|log|list|legend|lm|ifelse|invisible|ecdf|lines|plot|min|max|na\\.omit|f|expression|options|rgamma|runif|round|str|unique)$'){
		s1= readLines(theFile, warn=F)  
		catt('le(s1)=', le(s1))
		x= ListPatt(s= s1, patt='([A-z0-9_\\.]+ *= *)+function\\(' )  #	print(x)
		x$x= gsub(' *= function\\(', '', x$x)
		x= sus(x, !grepl(stoplist, x))
	}
	
	if(0){
		x= listFuncDef(theFile=  "R:/work/R-svn-ass/00_commonR/zBase.r")
		hee(srt(x, ~-Freq), 99)
		in.zBase= unlist(strsplit(x$x, c('=', ' ')))
		pas(in.zBase)
		# [1] "` aaa ab all0 AllCategFreqDesc analQuantiles atl BA= BayesAvg calcc CategFreqDesc catf catt cc  code2HTML cc= code2HTMLjQuery classes CleanSparePics cn command CorrelCircle CorrelCircle3 CreateNewProj cumsumrev CV CVwt dropp DT= DateTime= timestamp dummy exec execf expl expls ext2 fDate fid fll fregTAG fromUNIXt fsize fsub gcc gf  gtf gff gna gnal=lgna gw gzfile hee  dhe heec heee  dsh hees heta hglm HHa HHd HHf HHInit HHp.bak HHp HHp  HHp2 HHp2 HHpm HHpr= HTMLp HHs HHt hist.pan hists hive_conn hLink hSel hSelC inte isNum JS  JaccardSimilarity last LastDayOfWeek lg1 libra libras Lift LiftWt LiftWtArr LiftWtArr.Old LiftWtArr LiftWtArr2 LiftWtArr3 lo logg logit loo lsDF lss lss0 maxn me merge3 MergeByName mn Model mt nc nc  nmsv  sNames ncc newwin newwin=  function( nmsv  sNames nonUnique nope norm norm0 nuc nuf nut one ord ord2 ordd pas plotGLMCoeff plotl plotm plott pr prinb princ PrintModel prr prrr renamee RF.Importance  RFI rmall rmDF rmmm ROC rou rows.with.na rt rtd rtsv rwith.na sa saa sc.pan sf shorts ShowColorNames showInOpera spectr SQL2vars= Scope2vars srt  sortt  sort.data.frame SS st summar sumn susp suss tab.df tab Timer tocsv toTe totsv toXL= ToXL upper.panel wc week wwc wws xscrollcommand yScore yScoreSc yscrollcommand zeval= evall zlog10 zlogit zqqplot zqqplotWt"
	}

	
	listFuncUsage= function(theFile=  "m:/80_ChurnSim/80_ChurnSim.r", stoplist='^(c|if|for|function|with|within|exp|log|list|legend|lm|ifelse|invisible|ecdf|lines|plot|min|max|na\\.omit|f|expression|options|rgamma|runif|round|str|unique)$'){
		s1= readLines(theFile, warn=F)
		x= ListPatt(s= s1, patt='[A-z0-9_\\.]+\\(' )
		x$x= gsub('\\($', '', x$x)
		x= sus(x, !grepl(stoplist, x))
		x
	}
	
	if(0){
		x= listFuncUsage(theFile=  "m:/80_ChurnSim/80_ChurnSim.r")
		hee(srt(x, ~-Freq), 30)
		
		fs= dir('M:/', recursive=T, patt='\\.[rR]$')
		fs= fs[!grepl('^(999|scripts)', fs)]
		fs= 'M:/' %+% fs
		st({x2= ldply(fs, listFuncUsage)})  # 20.92 1.62
		
		x3= ddply(x2, .(x), function(x)df(nf=nrow(x), Freq=sumn(x$Freq)))
		str(x3)
		# 'data.frame':	3321 obs. of  3 variables:
		#  $ x   : chr  "." "..." ".attr" ".C" ...
		#  $ nf  : int  305 1 1 1 2 1 1 1 2 1 ...
		#  $ Freq: num  2064 1 1 2 2 ...
		hee(srt(x3, ~-Freq), 99)
		# 3321  rows
		#               x  nf  Freq
		# 408        catt 580 14951
		# 2907     tkgrid 341 12077
		# 2738        sum 545 10368
		# 1804      names 609  8701
		# 1993      paste 677  8307
		# 2194      print 646  7766
	
		in.zBase.Freq= sus(srt(x3, ~-Freq), x %in% in.zBase & x %in% allFunInMem()$x)
		hee(in.zBase.Freq, 99)
		srt(head(in.zBase.Freq, 19), ~x)
		#           x  nf  Freq
		# 404    catf 231  1108
		# 408    catt 580 14951
		# 479      cn 163  7469
		# 1195    gff  77   731
		# 1282     gw 100   664
		# 1310    hee  80  1467
	}
}

#' really it used for backticks rather than single quotes : 
subSingleQuote2div= function(quote='^\\s*`\\s*$', pattNeg='\'\\`\'|\"\\`\"', repl=c('<div class="sq">', '</div>'), s1= readLines(theFile, warn=F)){
	x= grepl(patt=quote, s1) & !grepl(patt=pattNeg, s1)
	if(sum(x) %% 2 != 0) warning(sf('odd quotes in %s: %s', attr(s1,"theFile"), pas(which(x))))
	
	s1[x]= repl %+%  s1[x]  # %+% repl
	s1
}



# do we need  HHInit ??
#' wrapper for dev.print  -  save graphics to .png file
sg= HHp= function(capt=.main, Width = dev.size(units = "px")[1] , off= T,  Height =  dev.size(units = "px")[2]
		, GraphPointSize = 12, dirr='../img', type= "cairo", res=96, dev=0, fNameWithCapt=F, gg=F, ...){ # type= "windows"
	op= options(); on.exit(options(op))  #; options(error=dummy)
	if(!file.exists(dirr)) dir.create(dirr)
	.iPic<<- max(0, nu(gsub('^Pic_(\\d+).*\\.png$', '\\1', dir(dirr, patt='.png$'))), na.rm=T)
	# catt('--------------------------------------- HHp: old iPic=', .iPic)
	GraphFileName=  if(fNameWithCapt) sf('Pic_%s. %s', .iPic<<- .iPic+1, capt) else  sf('Pic_%s', .iPic<<- .iPic+1)

	AbsGraphFileName= sf('%s/%s/%s.png', gw(), dirr, GraphFileName)
	catt('HHp: printing to ', AbsGraphFileName)
	
	if(dev>0) dev.set(dev)
	if(gg){try({  	ggsave(AbsGraphFileName)}, s=F)
	} else { dev.print(device = png, file = AbsGraphFileName, 
				width= Width, height = Height, pointsize= GraphPointSize, units="px", type= type,...)
	}
	
	if(exists('.HTML.file'))	cat(sf('<p align="left"><img src="img/%s.png"  border="0" width="%s" height="%s"/><br/>
								<span class="caption>%s</span><br/></p>/n', GraphFileName, Width, Height, capt)
				, file = .HTML.file, append = TRUE)
	# dir(fp(.HTML.file,'../../img'))
	
	if(off) dev.off()
	
	options(op)
	catf('Saved to: %s. %s\n', AbsGraphFileName, capt)
	catf('%s. %s\n', GraphFileName, capt)
	invisible(sf('%s. %s', GraphFileName, capt))
}

#'  save rCharts graphics to jPic_dd.htm file
sgj= HHjp= function(p= p1, capt='', dirr='../img', absPath=T) {
	.ijPic<<- max(0, nu(gsub('^jPic_(\\d+).*\\.htm$','\\1', dir(dirr, patt='.htm$'))), na.rm=T)
	GraphFileName=  sf('jPic_%s', .ijPic<<- .ijPic+1)
	AbsGraphFileName= if(absPath) sf('%s/%s.htm', dirr, GraphFileName)  else sf('%s/%s/%s.htm', gw(), dirr, GraphFileName)
	p$save(AbsGraphFileName) 
	catf('Saved to: expl("%s"). %s\n', AbsGraphFileName, capt)
	catf('%s. %s\n', GraphFileName, capt)
	invisible(sf('%s. %s', GraphFileName, capt))
}
# HHjp(p1)
# Saved to: expl("m:/80_ChurnSim/out/../img/jPic_10.htm"). 
# jPic_10. 

#'===  buid all examples from  rCharts   ===
# 	HH= function(p1=a) HHjp(p= p1, capt='', dirr='C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img')


if(0){ #==  rCharts  ==
	libra(devtools)
	install_github('rCharts', 'ramnathv')
	
	install.packages('C:/Users/azolotovitski/Downloads/ramnathv-rCharts-b1061ab.tar.gz', repos = NULL, type='source')
	install.packages('C:/Users/azolotovitski/Downloads/rCharts-master.zip', repos = NULL, type='source')
	install.packages('C:/Users/azolotovitski/Downloads/rCharts-master.zip', repos = NULL, type='win.binary')
	install.packages()
	
	#  C:\>z\eclipse\R-2.14.2\bin\x64\R.exe  --vanilla CMD INSTALL C:\Users\azolotovitski\Downloads\rCharts-master\rCharts-master
	
	
	require(rCharts)
	libra(rCharts)
	
	names(iris) = gsub("\\.", "", names(iris))
	p1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = "Species", size="PetalWidth", type = "point", dom='zzz')  # http://www.polychartjs.com/demo?bubble_chart
	p1$addParams(width = 550, dom='zzz')
	p1$printChart("chart1")  # use p1$show() from your R console
	str(p1)
	str(p1$show())
	
	#== Example 2: NVD3
	
	p2 <- nvd3Plot(SepalLength ~ SepalWidth, group = 'Species', data = iris, type = 'scatterChart')
	p2$set(width = 550)
	p2$printChart('chart2')
	p2$show()
	
	#Example 3: MorrisJS
	
	data(economics, package = 'ggplot2')
	dat = transform(economics, date = as.character(date))
	p3 <- mPlot(x = "date", y = list("psavert", "uempmed"), data = dat, type = 'Area',
    		labels = list('Savings Rate', 'Median Duration of Unemployment'), pointSize = 0)
	p3$printChart('chart3')
	printChart(p3)
	p3$show()
	str(p3)
	
	#==  Create All Examples  ==
	root= 'C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries'
	df(fs<- dir(root, rec=T,patt='^[^z].*R$'))
	#s= llply(fp(root,fs), readLines)
	s= llply(fp(root,fs), function(f){s= readLines(f); list(f=f, n=le(s), s=s)})
	ldply(s, function(s)with(s, df(f,n)))
	#                                                                                    f   n
	# 1        C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/highcharts/examples.R  85
	# 2  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example1.R   9
	# 3  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example2.R   5
	# 4  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example3.R  12
	# 5  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example4.R  24
	# 6  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example5.R  12
	# 7  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example6.R   6
	# 8  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example7.R   4
	# 9  C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example8.R   8
	# 10 C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/leaflet/examples/example9.R   6
	# 11           C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/morris/examples.R  32
	# 12             C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/nvd3/examples.R  40
	# 13       C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/polycharts/examples.R  50
	# 14         C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/rickshaw/example2.R  30
	# 15         C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/rickshaw/examples.R  68
	# 16          C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\libraries/xcharts/examples.R  23
	s2= unlist(llply(s, function(s)s$s))  # 414
	writeLines(s2, fp(root,'../az/zAllExamples.R'))
	expl(root)
	expl('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img')
	
	
	
	#HH= function(p1=a) HHjp(p= p1, capt='', dirr='C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img')
	
	fs= dir('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img')
	#s= llply(fp('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img', fs), function(f)s=c('<H2>', f, '</H2></br>', readLines(f), '</br>') )
	s= llply(sf('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\img\\%s', fs), function(f) 
				sf('<H2>%s</H2><iframe src="%1$s" width="800px" height="600px"></iframe></br>', f) )
	s= unlist(s)  #chr [1:51830]
	writeLines(s, 'C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\allExamples.htm')
	expl('C:\\z\\eclipse\\R-2.14.2\\library\\rCharts\\az\\allExamples.htm')
}


if(0) {#===  Work with Eclipse installation  ===
	d.st='M:\\71_UseR-2013-Cont\\eclipse-Stand'
	d.te='M:\\71_UseR-2013-Cont\\eclipse-tester'
    pl.st= dir(fp(d.st,'plugins'))
	pl.te= dir(fp(d.te,'plugins'))
	
	nin(pl.st, pl.te)
	nin(pl.te, pl.st)
	
	# no vers
	pl.te.nv= gsub('\\.w?\\d+\\-?.+\\.jar', '', pl.te)
	
	plst2= df(pl=pl.st, plnv= gsub('\\.w?\\d+\\-?.+\\.jar', '', pl.st))
	plte2= df(pl=pl.te, plnv= gsub('\\.w?\\d+\\-?.+\\.jar', '', pl.te))
	nin(plst2$plnv, plte2$plnv)
	nin(plte2$plnv, plst2$plnv)
	comm= intersect(plst2$plnv, plte2$plnv)
	
	tomove= sus(plst2, !(plnv %in% comm)) $pl
	
	for(f in tomove){catt(f); catt(fp(d.st, 'plugins', f), fp(d.st,'plugins/bak', f))}
	for(f in tomove){catt(f); file.rename(fp(d.st, f),fp(d.st,'bak', f))}
	x= file.rename(fp(d.st, 'plugins', tomove),fp(d.st, 'plugins/bak', tomove))
	sum(!x)
	
	
	#  features
	fe.st= dir(fp(d.st,'features'))
	fe.te= dir(fp(d.te,'features'))
	
	fest2= df(fe=fe.st, fenv= gsub('\\.[^\\.]+$', '', fe.st))
	fete2= df(fe=fe.te, fenv= gsub('\\.[^\\.]+$', '', fe.te))
	nin(fest2$fenv, fete2$fenv)
	nin(fete2$fenv, fest2$fenv)
	comm= intersect(fest2$fenv, fete2$fenv)
	tomove= sus(fest2, !(fenv %in% comm)) $fe
	
	x= file.rename(fp(d.st, 'features', tomove),fp(d.st, 'features/bak', tomove))
	sum(!x)
	# [1] 0
}
	

replaceTagsOutSq= function(s= readLines('M:/85_Otto/85_Otto.r', warn=F)){
#	comme = ifelse(grepl('#', s), sub('.*?(#.*)', '\\1', s), '')
#	s= no.comme = sub('#.*', '', s)
	
	#s3= unlist(strsplit('~#~' %+% s,''))  # now # is \n
	s3= strsplit(pas(s,'~#~','~#~'),'')[[1]]  # now # is \n
	inbt= cumprod((-1)^ (s3=='`'))== -1
	insq= cumprod((-1)^ (s3=="'"))== -1
	s3[!insq & !inbt]= sub('>', '&gt;', sub('<', '&lt;', s3[!insq & !inbt]))
	s4= strsplit(pas(s3, '', ''), '~#~') [[1]] #[-1]
	#s4= s4 %+% comme
	s4
}

# 
# s= replaceTagsOutSq(readLines('M:/85_Otto/85_Otto.r', warn=F)); prr(tail(s))
s= replaceTagsOutSq(c(" <aa '<bb'   `<ee`  <cc", " <aa '<bb'   `<ee`  <cc")); prr(s)

if(0){   #== Misc
	theFile= 'R:/work/R-svn-ass/00_commonR/zCodeTools.fun.r'
	listFuncUsage(theFile, stoplist='^(c|if|for|function|with|within|exp|log|list|legend|lm|ifelse|invisible|ecdf|lines|plot|min|max|na\\.omit|f|expression|options|rgamma|runif|round|str|unique)$')
	listFuncUsage(theFile, stoplist='^(c)$')
	
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	
	theFile= fp(proot, '80_ChurnSim.r')
	#ccc= function()code2HTML(theFile)
	#cc= function()code2HTMLjQuery(theFile) #  cc()
	cc()
	
	shell('start explorer file:\\\\m:\\80_ChurnSim\\80_ChurnSim.r.jQ.htm')
} #--

