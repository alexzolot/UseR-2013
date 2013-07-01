\name{code2HTML}
\alias{code2HTML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Creates R Work Journal
%%  ~~function to do ... ~~
}
\description{ Creates R Work Journal
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
code2HTML(theFile = "m:/zzz.r", img = "img", FullSyntaxHighlight = FALSE, classicHeaders = FALSE, show = TRUE, toSave = TRUE, outSuffix = ".htm")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theFile}{
%%     ~~Describe \code{theFile} here~~
}
  \item{img}{
%%     ~~Describe \code{img} here~~
}
  \item{FullSyntaxHighlight}{
%%     ~~Describe \code{FullSyntaxHighlight} here~~
}
  \item{classicHeaders}{
%%     ~~Describe \code{classicHeaders} here~~
}
  \item{show}{
%%     ~~Describe \code{show} here~~
}
  \item{toSave}{
%%     ~~Describe \code{toSave} here~~
}
  \item{outSuffix}{
%%     ~~Describe \code{outSuffix} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (theFile = "m:/zzz.r", img = "img", FullSyntaxHighlight = FALSE, 
    classicHeaders = FALSE, show = TRUE, toSave = TRUE, outSuffix = ".htm") 
{
    if (FullSyntaxHighlight) {
        catt("FullSyntaxHighlight")
        libra(highlight)
        s1 = highlight(theFile, NULL, renderer = renderer_html(document = TRUE))
        s1 = gsub("\n$", "", s1)
        writeLines(s1, theFile \%+\% ".FSH.htm")
    }
    else {
        s1 = readLines(theFile, warn = F)
    }
    picss = list()
    s1 = gsub("<(\\s)", "&lt;\\1", s1)
    s1 = replaceTagsOutSq(s1)
    s1 = gsub("@@", "\\\\\\\\", s1)
    s1 = gsub("(^|[^x\"])@", "\\1\\\\", s1)
    for (i in 1:le(s1)) {
        if (grepl("^\\s*#\\s* Pic_\\d+", s1[i])) 
            picss[[ch(i)]] = s1[i]
        s1[i] = gsub("^([^\\~]*# *\"?)(jPic_\\d+)(.*)", "\n\n <iframe src=\"img/\\2.htm\" width=\"100\%\" height=\"600px\"></iframe>\n\n \\1\\2\\3", 
            s1[i])
        s1[i] = gsub("^([^\\~]*# *\"?)(Pic_\\d+)(.*)", sf("\n\t\t\t\t\t\t\t\t<img id=\"\%s\" class=\"pic\" src=\"\%s/\\2.png\" width=700 onClick=\"resize(\%1$s);\" ondblclick=\"ShowImg('\\2', '\\3');\" name=\"\\2\\3\"/>   \n\t\t\t\t\t\t\t\t<span class=\"capt\" onClick=\"goto('tnTOC\%1$s');\" ondblclick=\"ShowImg('\\2', '\\3')\">\\1\\2\\3</span>", 
            i, img), s1[i])
        s1[i] = gsub("(.*\\s*#==== )(.*)( =+.*)$", sf("\\1<span class=\"H1\" id=\"\%s\" onClick=\"goto('tn\%s');\">\\2</span> <span class=\"comment2\">\\3</span>", 
            i, i), s1[i])
        s1[i] = gsub("(.*\\s*#=== )(.*)( =+.*)$", sf("\\1<span class=\"H2\" id=\"\%s\" onClick=\"goto('tn\%s');\">\\2</span> <span class=\"comment2\">\\3</span>", 
            i, i), s1[i])
        s1[i] = gsub("(.*\\s*#== )(.*)( =+.*)$", sf("\\1<span class=\"H3\" id=\"\%s\" onClick=\"goto('tn\%s');\">\\2</span> <span class=\"comment2\">\\3</span>", 
            i, i), s1[i])
        s1[i] = gsub("(.*\\s*#= )(.*)( =+.*)$", sf("\\1<span class=\"H4\" id=\"\%s\" onClick=\"goto('tn\%s');\">\\2</span> <span class=\"comment2\">\\3</span>", 
            i, i), s1[i])
        s1[i] = gsub("(.*\\s*#=+)([^=]*)$", sf("\\1<span class=\"H5\" id=\"\%s\" onClick=\"goto('tn\%s');\">\\2</span> <span class=\"comment2\">\\3</span>", 
            i, i), s1[i])
    }
    catt("666 picss:")
    print(picss)
    if (classicHeaders) 
        for (i in 1:5) {
            s1 = gsub(sf("class=\"H\%s\"", i), sf("class= \"H\%s\"", 
                6 - i), s1)
        }
    depth = countDepth2(s1)
    imgFold = "<img src='https://ui.netbeans.org/docs/ui/code_folding/cf_minus.gif'/>"
    s1 = ifelse(depth != 0, gsub("^([^#]*\\{[^\\{\\}]*)", sf("\\1 <a href=\"javascript:ToggleFold('D77')\" class=\"aD\">\%s</a><div class=\"D77\">", 
        imgFold), s1), s1)
    s1 = gsub("^(.* class=\"H(\\d)\".* id=\"(\\d+).*)D77(.*) class=\"D77\"", 
        "\\1D\\3\\4 class=\"D\\2\" id=\"D\\3\"", s1)
    s1 = ifelse(depth != 0, gsub("([^#\\{\\}]*)\\}", "\\1</div><b>}</b>", 
        s1), s1)
    attr(s1, "theFile") = theFile
    s1 = subSingleQuote2div(quote = "^[^\\`]*`[^\\`]*$", pattNeg = "'\\`'|\"\\`\"", 
        repl = c("<span class=\"sq\">", "</span>"), s1)
    s1 = gsub(" \\$ (.+) \\$ ", " &nbsp;&nbsp; $ \\1 $ &nbsp;&nbsp; ", 
        s1)
    s1 = gsub("^(\\s*#')(.*)$", "<span class=\"comment2\">\\1</span><span class=\"text\">\\2</span>", 
        s1)
    s1 = gsub("(#[^'=-].*)$", "<span class=\"comment\">\\1</span>", 
        s1)
    s1 = gsub("^(.*)(#=+)(.*)$", "\\1<span class=\"comment2\">\\2</span>\\3", 
        s1)
    s1 = gsub("^(.*)(#\\-\\-)(.*)$", "\\1<span class=\"comment2\">\\2</span>\\3", 
        s1)
    s1 = gsub("^(.*# ?.*)((xxx|TODO):.*)(</span.*)$", "\\1<font color=\"red\">\\2</font>\\4", 
        s1)
    main = gsub("^([^#]+= ?function)(.*)$", "<span class=\"fun\">\\1</span>\\2", 
        s1)
    toc = grep("img id=|<H\\d+|\"H[1-5]", s1, v = T)
    prr(toc[1:15], 666)
    toc = gsub("\"capt\"", "\"captTOC\"", toc)
    toc = gsub("class=\"pic\"", "class=\"tnTOC\"", toc)
    toc = gsub("700", "400", toc)
    toc = gsub("resize", "goto", toc)
    toc = gsub("id=\"(\\d+)\"", "id=\"tnTOC\\1\"", toc)
    toc = gsub("goto\\('tn(\\d+)'\\)", "goto('\\1')", toc)
    toc = gsub("\\{?( *<H\\d.*</H\\d>).*", "\\1", toc)
    toc = gsub("(.*)\\{", "<br/>\\1", toc)
    toc = gsub("<a.*$", "", toc)
    toc = gsub("(.* class= *\"H.\".*)", "<br/>\\1", toc)
    prr(toc[1:15], "777.TOC:")
    pics2 = laply(na(picss), function(np) {
        p = picss[[np]]
        sf("<img id=\"tn\%s\" class=\"imgGal\" src=\"img/\%s.png\" name=\"\%s\" height=100> ", 
            np, gsub(".* (Pic_[0-9]+).*", "\\1", p), gsub("# *", 
                "", p))
    })
    pics = pics2
    header = c("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\"><head><link rel=\"shortcut icon\" href=\"/favicon.ico\">\n\n\t\t\t\t<!-- we need these 2 lines for the case if no Internet during the tutorial -->\n\t\t\t\t<script src=\"T:/mathjax-MathJax-v2.2-8-g727332c/mathjax-MathJax-727332c/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>\n\t\t\t\t<script src=\"T:/work/UseR-2013/lib/jquery-1.10.1.min.js\"></script>\n\t\t\t\t<!--  -->\n                \n\t\t\t\t<script src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>\n\t\t\t\t<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js\"></script>\n\n\n\t\t\t\t<!--script src=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.3/jquery-ui.js\"></script-->\n             \n\t\t\t\t <script type=\"text/x-mathjax-config\">  MathJax.Hub.Config({ tex2jax: { inlineMath: [[\" $ \",\" $ \"]] } }) </script>\n\t\t\t\t\n\t\t\t\t<!-- script>\n\t\t\t\t$(function(){\n\t\t\t\t//$(\".imgGal\").draggable() ; //.parent().resizable();\n\t\t\t\t$(\".dimg\").resizable().draggable(); \n\t\t\t\t// $(\".pic\").draggable();  //.resizable();;\n\t\t\t\t});\n\t\t\t\t</script -->\n\t\t\t\t\n\t\t\t\t\n\t\t\t\t<script>\n\t\t\t\t$(function(){\n\t\t\t\t\tvar d= document\n\n\t\t\t\t//var imgFold= \"<img src='https://ui.netbeans.org/docs/ui/code_folding/cf_minus.gif'/>\"  // \"(-)\"\n\t\t\t\tvar imgFold= $(\".aD:first\").html()\n\n\t\t\t\t$(\"img.imgGal\")\n\t\t\t\t\t.mouseover(function() {show(this)} )\n\t\t\t\t\t.mouseout(function() {show(0)} )\n\t\t\t\t    .click(function() {goto(this.id.replace(\"tn\",\"\"))})\n\t                .dblclick(function() {ShowImg(this.name.replace(/\\..*/, \"\"),  this.name.replace(/.*?\\./, \"\")); })\n\t\t\t\t;\n\t\t\t\t$(\".aD\").click(function() { th= $(this)\n\t\t\t\tth.next(\"div\").toggle();\n\t\t\t\t//alert(th.html()+ \"\\n\" + imgFold)\n\n\t\t\t\tth.html(th.html()== imgFold ? \"...\" : imgFold)\n\t\t\t\t})\n                $(\"#aToggleAll\").click(function() {th= $(this)\n\t\t        if( th.text() ==\"(+)\") {\n\t\t        $(\"div\").show(); th.html(imgFold); $(\"a:not(.comments)\").html(imgFold)   \n\t\t        } else { $(\"div\").hide(); th.html(\"(+)\"); $(\"a:not(.comments)\").html(\"(+)\")}\n                })\n                //   $(\".aToggleAllPic\").click(function() {$(\"img\").toggle() })\n\t\t\t\t\n\t\t\t\tfunction toggleD(a, a2, di){ if( a.text() == \"(+)\") {\n\t\t\t\tdi.show(); a.html(imgFold); a2.html(imgFold)  \n\t\t\t\t} else { di.hide(); a.html(\"(+)\"); a2.html(\"...\")}\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\tfunction ToggleComments2(){ \n\t\t\t\tvar tt = document.getElementsByClassName(\"comment2\"); \n\t\t\t\tvar t0= tt[0].style.fontSize;\n\t\t\t\tdi= (t0 == \"3px\" || t0 == \"\") ? \"12px\" : ((t0 == \"12px\")? \"1px\" : \"3px\") ;\n\t\t\t\tfc=  di == \"1px\" ? \"white\" : \"green\";\n\t\t\t\tfor (var i = 0; i < tt.length; i++) {tt[i].style.fontSize = di; tt[i].style.color=fc;}\n\t\t\t\t}\n\n                $(\".aToggle.DD1\").click(function() {toggleD($(this), $(\".H1 + + .aD\"), $(\".D1\")) })\n                $(\".aToggle.DD2\").click(function() {toggleD($(this), $(\".H2 + + .aD\"), $(\".D2\")) })\n                $(\".aToggle.DD3\").click(function() {toggleD($(this), $(\".H3 + + .aD\"), $(\".D3\")) })\n                $(\".aToggle.DD4\").click(function() {toggleD($(this), $(\".H4 + + .aD\"), $(\".D4\")) })\n                $(\".aToggle.TOC\").click(function() {toggleD($(this), $(\".zz\"), $(\"div.TOC\")) })\n                $(\".aToggleAllPic\").click(function() {toggleD($(this), $(\".zz\"), $(\"img\")) })\n                $(\".aToggleComments\").click(function() {\n\t\t\t\t\tToggleComments2(); return;\n\n\t\t\t\t\tc2= $(\".comment2\");\tfs= c2.eq(0).css(\"font-size\");\n\t\t\t\t\tvar tt = document.getElementsByClassName(\"comment2\"); var t0= tt[0].style.fontSize;\n\t\t\t\t\t//alert(fs + \" | \" + tt[0].innerHTML + \" zz \" + t0)\n\t\t\t\t\t//alert(fs + \" | \" + t0.innerHTML)\n\t\t\t\t\t//fsn= (fs == \"3px\" || fs == \"\") ? \"12px\" : ((fs == \"12px\")? \"1px\" : \"3px\");\n\t\t\t\t\tfsn= (fs == \"30px\" || fs == \"\") ? \"120px\" : ((fs == \"120px\")? \"10px\" : \"30px\");\n\t\t\t\t\t//alert(fs + \" | \" + tt[0].innerHTML + \" t0: \" + t0 + \" fsn:\" + fsn+ \"|\"+ \" sp:\" + $(\".comment2:first + span\").text())\n\t\t\t\t\t$(\"#out\").text(\" || fs:\" +fs + \" | \" + tt[0].innerHTML + \" t0: \" + t0 + \" fsn:\" + fsn+ \"|\"+ \" sp:\" + $(\".comment2:first + span\").text());\n\t\t\t\t\t$(\".comment2\").css({\"font-size\": fsn, \"font-size-adjust\": .1});\n\t\t\t\t\t//$(\".comment2\").css(\"font-size\", \"xx-small\");\n\t\t\t\t\t$(\".comment2\").text(\"zz\");\n\t\t\t\t\t$(\".comment2\").css(\"color\",  c2.css(\"font-size\")== \"1px\" ? \"white\" : \"green\")\n\t\t\t\t\t})\n\t\t\t\t\n\t\t\t\t});\n\t\t\t\t</script>\n\t\t\t\t\n\t\t\t\t<script><!--\n\t\t\t\tvar d= document;\n\t\t\t\t\n\t\t\t\tfunction goto(i) {w= d.location= \"#\" + i;}\n\t\t\t\t\n\t\t\t\tfunction ShowSVG(p){w= window.open(\"\",\"\",\"fullscreen=yes\")\n\t\t\t\tw.document.write(\"<html><iframe src='./img/\"  + p + \".svg' width=1200  height=800/></html>\")\n\t\t\t\tw.focus()\n\t\t\t\t}\n\t\t\t    \n\t\t\t\tfunction ShowImg(p, capt){w= window.open(\"\",\"\",\"type=fullWindow,fullscreen,location='',height=\"+ screen.height+ \",width=\"+screen.width)\n\t\t\t\tw.document.write(\"<html><img src='./img/\"  + p + \".png ' /><br/>\" + p +  capt + \"</html>\")\n\t\t\t\tw.document.title= p + capt;\n\t\t\t\tw.focus()\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\tfunction resize(i) {e=d.getElementById(i); w= e.width;\n\t\t\t\te.style.width= (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? \"100\%\" : 700;\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\t\n\t\t\t\tfunction ToggleFold(d) {}\n\t\t\t\t\n\t\t\t\t\n\t\t\t\tfunction show(e){\n\t\t\t\t$(\"#show\").css({display : (e==0)? \"none\" : \"block\", border: (e==0)? \"0\" : \"2px solid blue\"})\n\t\t\t\t\n\t\t\t\tif(e != 0){\n                x= Math.min(e.offsetLeft+ e.width + 10, 500) //x= e.x+ e.width + 10;// ; if(x > 500) x= 500;\n                y= e.offsetTop+ e.height + 10               //y= e.y+ e.height + 10\n\t\t\t\t\n\t\t\t\t$(\"#show\").css({left: x, top: y})\n\t\t\t\t$(\"#showPic\").attr(\"src\", e.src)\n\t\t\t\t$(\"#showTxt\").html(\"<br/>\" +  e.name)  //  +  e.alt)\n\t\t\t\t\n\t\t\t\t/* show properties of e \n\t\t\t\ta=\"\"\n\t\t\t\tfor(k in e) if(1 || e[k] > 10 && e[k]+\" \" < \"a\") a= a + \"      e[\" + k + \"]= \" + e[k];\n\t\t\t\talert (a);\n\t\t\t\t/**/\n\t\t\t\n\t\t\t\t};\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\t//--></script>\n\t\t\t\t\n\t\t\t\t<style>  /*  www.w3schools.com/cssref/css_colornames.asp  www.tizag.com/cssT/border.php */\n\t\t\t\tdiv.main {font-family: monospace; white-space: pre;}\n                .D1, .D2, .D3, .D4, .D5  {background-color:rgba(255,240,240, .2); border-style:ridge; margin:5px; padding:15px;-moz-border-radius:5px; border-radius:5px} /*div {; opacity: 0.3; background-color:GhostWhite; border-left-style:ridge;}  */\n\t\t\t\tdiv.TOC {background-color:rgb(248,248,255); }\t\n\t\t\t\tdiv.D5 {font-size:8px;} \n\t\t\t\t//.sq {font-size:12px; font-family: Arial; color:DodgerBlue } \n\t\t\t\tcenter, .capt {font-size:12px; font-weight:bold; font-family: Arial; margin:auto; text-align:center;}\n\t\t\t\t.captTOC {font-size:12px; font-weight:bold; font-family: Arial; align:left}\n\t\t\t\tH1,  H2,  H3,  H4,  H5 {color:blue; font-family: Arial; color:teal;}\n\t\t\t\t.H1, .H2, .H3, .H4, .H5 {color:teal; font-family: Arial; font-weight:bold;  display:inline-block; display:-moz-inline-box;}\n\t\t\t\tH1, .H1 {font-size:36px;  margin-top:36px;  margin-bottom:8px;  margin-left:0px; } \n\t\t\t\tH2, .H2 {font-size:28px;  margin-top:28px;  margin-bottom:5px;  margin-left:20px;} \n\t\t\t\tH3, .H3 {font-size:24px;  margin-top:15px;  margin-bottom:3px;  margin-left:40px;}\n\t\t\t\tH4, .H4 {font-size:18px;  margin-top:12px;  margin-bottom:2px;  margin-left:60px;}\n\t\t\t\tH5, .H5 {font-size:14px;  margin-top:10px;  margin-bottom:1px;  margin-left:80px;}\n\t\t\t\timg.pic, .captTOC, img.tnTOC {margin-top:0px;   margin-bottom:0px;  margin-left:80px;}\n\t\t\t\tspan.captTOC, img.tnTOC { display: block;}\n\t\t\t\t.D77 { display:inline; }\n\t\t\t\tiframe {border-style:none}\n\t\t\t\t/* .dimg{height:90px;  display:inline-block;} /* position:fixed;  position:absolute;*/ \n                .imgGal{margin-top:1px;   margin-bottom:1px;  margin-left:1px; height:100px} /*height:100\%; max-width:100\%; max-height:100\%; }/* position:absolute; fixed;*/ \n\t\t\t\t\n\t\t\t\t.fun {color:indigo; font-weight:bold;}\n\t\t\t\t.comment {color:green; align:left}\n\t\t\t\t.comment2 {color: rgb(100,200,100); font-size:3px; -webkit-text-size-adjust: none;}\n\t\t\t\t.text, .sq {color: teal; font-size:12px;}\n\t\t\t\t</style>\n\t\t\t\t</head><body>")
    footer = "<div id= \"show\" style= \"position: absolute; left:-1000px; top:1px; border:solid blue; padding:5; display:none; background-color:white; -moz-border-radius: 10px; border-radius: 10px;\">\n\t\t\t\t<img id=\"showPic\" src=\"\" height=\"400px\" style=\"left:0px\"/><br/>\n\t\t\t\t<span id=\"showTxt\"></span>\n\t\t\t\t</div></body></html>"
    out = c(header, "\t\t# <a href=\"#\" id= \"aToggleAll\">Fold</a> All |\n\t\t\t\t<a href=\"#\" class= \"aToggleAllPic\">Toggle</a> all pics | \n\t\t\t\t<a href=\"#\" class= \"aToggle TOC\">Fold</a> contents |\n\t\t\t\t<a href=\"#\" class= \"aToggle DD2\">Fold</a> H2 |\n\t\t\t\t<a href=\"#\" class= \"aToggle DD3\">Fold</a> H3 |\n\t\t\t\t<a href=\"#\" class= \"aToggle DD4\">Fold</a> H4 |\n\t\t\t\t<a href=\"#\" class= \"aToggleComments\">ToggleComments</a> <span id=\"out\"></span>\n\t\t\t\t<div class=\"TOC\" id=\"0\"> <H3>Gallery for  ", 
        theFile, " </H3>", pics, "<hr/><H2>Contents</H2>", toc, 
        "</div><p/><br/><br/>\n\t\t\t\t<!-- pre --><div class=\"main\">", 
        main, "</div><!-- /pre --><br>##   The HTML output of ", 
        theFile, " was created on ", DT(), "; <a href=\"http://www.mathjax.org/demos/scaling-math/\">test MathJax </a>", 
        footer)
    if (toSave) {
        writeLines(out, theFile \%+\% outSuffix)
        catf("expl(\"file://\%s\")", theFile \%+\% outSuffix)
        if (show) 
            expl("file://" \%+\% theFile \%+\% outSuffix)
    }
    invisible(list(theFile = theFile, header = header, pics = pics, 
        toc = toc, main = main, footer = footer, out = out))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
