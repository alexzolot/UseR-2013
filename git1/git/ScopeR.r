#source('M:/zmFunctions.r')
library('zBase')
options(help='html', digits=4, width=160, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe')
options(ScopeFolder='m:/scope1'
        , TFexe='"C:/Program Files (x86)/Microsoft Visual Studio 10.0/Common7/IDE/tf.exe"' 
    	, vc='http://cosmos05.osdinfra.net:88/cosmos/adCenter.BICore.Fraud'
		, prior= 1000
        , msalias='alexzol')
options()$ScopeFolder
getOption('ScopeFolder')

libra(plyr)
wc= function(f='out/woPV/0222/*.tsv') 2 # ToDo {sh<- shell(paste('c:/cygwin/bin/wc.exe -l ', f, '*', sep=''), wait=T ,  intern=T)}
#	exec= function(s)shell(s, wait=T, intern = T)
#	execf= function(fmt,...)shell(sf(fmt,...), wait=T, intern = T)
#	HH= function(capt='',...) HTMLplot(GraphFileName =  sf('Pic_%9.0f', runif(1,,1e9)), Caption=capt, Align = "left",...) #, Width = 800, Height = 600, GraphPointSize=1)


scope= function(cmd= '-submit', ScopeFolder= options()$ScopeFolder
			, vc= options()$vc,...) {
    cmd= sf('%s/scope.exe %s -vc %s ', ScopeFolder, cmd, vc)
    logg('Scope Executing', cmd)
    sh= exec(cmd)
    catf('\nscope result: %s', head(sh,399))
    invisible(sh)

#	if(0)scope(ScopeFolder='m:/scope1'
#            , vc='http://cosmos05.osdinfra.net:88/cosmos/adCenter.BICore.Prod2'
#            , cmd='dir /local/prod/pipelines/Aegis/PublisherCollusion/2011/09/11/Debug/') 
}

week= function(Date= Sys.Date()) as.numeric(Date - as.Date('2011-01-01'),"days")  %/% 7  +1
if(0)str(week())

LastSaturdayOfWeek= LastDayOfWeek= function(Week= week(Sys.Date()-6))  as.Date('2011-01-01')  + 7 * Week
if(0)str(LastSaturdayOfWeek())

ScopeDT= function(MRDate, MRTime)  strptime(MRTime, "%m/%d/%Y %I:%M:%S %p") +
			                      (strptime(MRDate, "%m/%d/%Y %I:%M:%S %p") -
				                   strptime('01/01/0001 12:00:00 AM', "%m/%d/%Y %I:%M:%S %p"))

dir.scope= function(ScopeFolder = options()$ScopeFolder
        , CosmosDir='/local/prod/pipelines/Aegis/PublisherCollusion/2011/09/11/Debug/'
        , vc= options()$vc){
    #sc.out= shell(sf('%s/scope.exe dir %s -vc %s', ScopeFolder, CosmosDir, vc), wait=T, intern = T)
    sc.out= scope(sf('dir %s', CosmosDir), ScopeFolder, vc)
    sc.outStreams= df(stream= gsub('.+(http:.+) $','\\1', grep('Stream', sc.out, v=T)) 
            , file= gsub('.+/([^/]+) $','\\1', grep('Stream', sc.out, v=T))
            , len= nu(gsub('.+:(.+)$','\\1', grep('Committed Length', sc.out, v=T)))
            , Created= gsub('.+: (.+)$','\\1', grep('Creation Time', sc.out, v=T))
            , Updated= gsub('.+: (.+)$','\\1', grep('Published Update Time', sc.out, v=T))
            , Expired= gsub('.+: (.+)$','\\1', grep('Expiration Time', sc.out, v=T)))
    sc.outStreams
}
if(0)dir.scope(ScopeFolder = "m:/Scope1"
	  , CosmosDir='/local/prod/pipelines/Aegis/PublisherCollusion/2011/09/11/Debug/'
      , vc='http://cosmos05.osdinfra.net:88/cosmos/adCenter.BICore.Prod2')
#                                                                                                                                                              stream
# 1                  http://cosmos05.osdinfra.net:88/cosmos/adCenter.BICore.Prod2/local/prod/pipelines/Aegis/PublisherCollusion/2011/09/11/Debug/JoinedEventsData.csv
# 2               http://cosmos05.osdinfra.net:88/cosmos/adCenter.BICore.Prod2/local/prod/pipelines/Aegis/PublisherCollusion/2011/09/11/Debug/IPStatsForConnComps.csv
# 3                   http://cosmos05.osdinfra.net:88/cosmos/adCenter.BICore.Prod2/local/prod/pipelines/Aegis/PublisherCollusion/2011/09/11/Debug/CDLForCollusion.csv
#                                    file         len              Created              Updated             Expirated
# 1                  JoinedEventsData.csv 12072623544 9/12/2011 6:39:46 PM 9/12/2011 6:36:46 PM 10/27/2011 6:54:46 PM
# 2               IPStatsForConnComps.csv       45808 9/12/2011 6:39:46 PM 9/12/2011 6:37:57 PM 10/27/2011 6:54:46 PM
# 3                   CDLForCollusion.csv         436 9/12/2011 6:39:48 PM 9/12/2011 6:39:31 PM 10/27/2011 6:54:48 PM
   

RunJob0= function(TheCodeFolder= 'M:/93_AegisPubScore', ScopeFolder = options()$ScopeFolder
					, msalias= options()$msalias, vc= options()$vc
					, ScopeTempl.script= "../93_PubSc_wPV-templ.script",
					ScopePref= "93_PubSc_wPV",
					RRep= '../dummy.r',
					ndays= 1, Timy='dai', CosmosDir= '' # "/my/93_PubSc_wPV/w21-05-28-wPV"
					, Rep.vers= 'wPV', LastDay= "", RepWeek=0) { #LastDay= "2011-03-05"
		
		try({TheCodeFolder= file.path(TheCodeFolder, dir_name)},T)
		catt('433.RunJob: TheCodeFolder=', TheCodeFolder)
		
		setwd(TheCodeFolder);
		source('../zmFunctions.r') 	 #load(file = ".Rdata")	# save.image()	
		SetState= function(s){dput(state<<- s, file="state.txt"); catt('RunJob: SetState: ==== State set to ', state); logg('== State set to ', state); state} 
		GetState= function(){ if(!file.exists('state.txt'))SetState('Init')
					state= dget("state.txt"); catt('RunJob: GetState: State=', state); state} #  unlink("state.txt");
		
		
		# 0) define report date ===
        if(LastDay== ''){
    	    catt('777,RepWeek=',RepWeek)
    		if(RepWeek== 0) RepWeek= week(Sys.Date()-6) # [1] 9
    		#RepWeek= week(as.Date('2011-02-24')) # 8 for Kamran's [1] 9 # <<<------------------ change date if necessary <==
    	    catt('888,RepWeek=',RepWeek)
		    LastDay= LastDayOfWeek(RepWeek) # [1] "2011-03-05"
        }  else LastDay= as.Date(LastDay)

		(sLastDay= fDate(LastDay) %+% Rep.vers) # [1] "03-05"  # Sat of the Last Week
		mmdd= substr(LastDay, 6, 99)
		
		catt('899,sLastDay=',sLastDay)
		#brr('88888')
		
		if(secondir<- T)(sLastDay= format(LastDay,'w%W-%m-%d-') %+% Rep.vers) # <<<------------------------ change out dir sLastDay<============
		
		# 1) Creates local and Cosmos output Directories
		if(!file.exists(sLastDay))dir.create(sLastDay)
		stopifnot(file.exists(sLastDay))
		
		AegColluDir.tsv<<-  sLastDay
		catt('944. Output dir :',sLastDay)
		logg('944. Output dir :',sLastDay)
		
		scName= sLastDay
		if(CosmosDir== '' ) CosmosDir= sf('/my/%s/%s', ScopePref, scName)#, mmdd)
		
		setwd(sLastDay); getwd()
		state= GetState() 
		
		if(0) { d=dir('.', 'Clk*')  # small dummy files 
			for(f in d) {s= he(readLines(f),500); writeLines(s,f)}
			SetState('ScopeOutDownloaded')
		}
		
		
		
		
		# 2) Creates Scope script from template
		
		
		#'scope -encode -u phx\alexzol'
		
		if(state[1]== 'Init'){
			nweeks=0
			ss= readLines(ScopeTempl.script, warn = F)
			for(w in 0:nweeks){ # w=0; 
                day<<- LastDay - 7*w
                ndays<<- ndays
                catt(288, w, ch(day<- LastDay - 7*w))
                
                if(1){
                    s=ss
                    #s= gsub('_hourly_', '_daily_', s)
                    if(dummy<- F){ #fast debug run                   # <<<------------------------ debug hourly run
                        s= gsub('"dai"', '"hour"', ss)
                        s= gsub('@"_1W_"', '@"_1H_"', s)        
                        s= gsub('@StartDate', sf('@"%s"',day), s)       
                    }
                    
                    s= gsub('@EndDate', sf('@"%s"', day), s)
                    s= gsub('@StartDate', sf('@"%s"', day- ndays + 1), s)
                    s= gsub('@Timy', sf('@"%s"', Timy), s)
                    #scName= format(Sys.time(), ScopePref %+% '_%m%d-%H%M') # 'AegCollu_%m%d-%H%M%S')
                    #               scName= sLastDay
                    #       
                    #               CosmosDir= sf('/my/%s/%s', ScopePref, scName)#, mmdd)
                    s= gsub('@root', sf('@"%s/"', CosmosDir, scName), s)
                    #mmdd= substr(day, 6, 99)
                    #s= gsub('(@EndDate).Substring(5)', sf('@"%s"',day), s)
                    
                    he(s,40)
                    #writeLines(he(s,9999), con = f<- sf('%s/93_PubSc_noPV_%s.script',TheCodeFolder, fDate(day)))
                    #writeLines(he(s,9999), con = f<- sf('88_AegCollu-%s.script', fDate(day)))
                    #writeLines(he(s,9999), con = f<- gsub('(Templ)?\\.script', fDate(day) %+% '\\.script', ScopeTempl.script))
                    writeLines(he(s,9999), con = f<- gsub('\\.script', fDate(day) %+% '\\.script', ScopeTempl.script))
                    file.copy(f, gw())
                    #file.show(f)                   
                }


	if(0)f= transform.scopeScript(script=ScopeTempl.script, fileShow= T
			, hourly=(Timy=='hour'), outScript= f, outScriptDir=gw()
            , StartDate= sf('@"%s"', day- ndays + 1), EndDate=sf('@"%s"', day)
            , root= sf('@"%s/"', CosmosDir, scName))

#transform.scopeScript(script="M:/83_ScopeR/AegisCustomDataSourceView.script", fileShow= F
#        , hourly=F, outScriptFile= gsub('^.*\\/(.+)', '\\1', script), outScriptDir=tempdir(), ...)	
				
#    cmd= sf('%s/scope.exe -submit -vc "http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore" -notify %s@microsoft.com,alexzol@microsoft.com -u phx\\%s -f %s -i %s -p 900'
#            , ScopeFolder, msalias, msalias,scName, f)
    
#cmd0= sf('%s/scope.exe -submit -vc %s -notify %s@microsoft.com,alexzol@microsoft.com -f %s -i %s -p 900'
#        , ScopeFolder, vc, msalias, scName, f)

cmd= sf('-submit  -notify %s@microsoft.com -f %s -i %s -p %s'
        				, msalias, scName, f, prior)


				catt('1155. Executing ', cmd)
				logg('Executing', cmd)
				
				#Zengpan:
				# cmd= sf('%s/scope.exe -submit -vc "_http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore" -notify %s@microsoft.com,alexzol@microsoft.com  -f %s -i %s -p 900'
				#			,ScopeFolder,msalias, scName, f)
				
				
#sh= shell(cmd0, wait=T, intern = T)
sh= scope(cmd)

				catt(1655, sh)
				logg(sh)
				if(!any(grepl(' successfully', sh)) ){for(s in sh)catt(s); stop('No  successfully!!!')}
                
                
				sjobId= grep('Script submitted, job Id', sh, v=T)
				jobId= strsplit(sjobId[[1]],' ')[[1]]
				logg(jobId)
				
				jobId= jobId[[le(jobId)]]	# [1] jobId=  "1727b4bc-9bc4-48cb-b507-3e7687ba2408"
				SetState(c('ScopeSubmitted',jobId, scName)) 
			}	
		}	
		
		
		
		state= GetState()
		if(state[1]== 'ScopeSubmitted'){ # monitor Scope execution ===
			
			jobId= state[2]
			scName= state[3]
			
			#sh= shell(sf('%s/scope.exe -joblist -vc http://cosmos04.osdinfra.net:88/cosmos/adcenter.BICore -u phx\\%s', ScopeFolder, msalias), wait=T, intern = T)
			#Zengpan:
			sh= shell(sf('%s/scope.exe -joblist -vc %s', ScopeFolder, vc), wait=T, intern = T)
			
			he(sh,10)
			
			
			#ii= grep(sf('Job Name.*%s',scName),sh)
			ii= grep(jobId,sh)
			#for(i in ii)catt(sh[(i-3):(i+3)])
			for(i in ii)try({print(sh[(i):(i+5)])}, s=T)
			
			(ScopeJobStatus= strsplit(sh[i+1],' ')[[1]][3])
			
            #if(ScopeJobStatus != 'CompletedSuccess'){stop("ScopeJobStatus != 'CompletedSuccess' , Wait  :( ")}
    
    if(ScopeJobStatus == 'CompletedFailure'){SetState(c('Init','after Scope Failure')); try(stop("'Scope: CompletedFailure'  :( \n State is set to Init"), s=T)
    } else if(ScopeJobStatus != 'CompletedSuccess'){try(stop("ScopeJobStatus != 'CompletedSuccess' , Wait  :( "), s=T)
    } else  SetState(c('ScopeCompletedSuccess',jobId))
			
			#he(sh,20)
		}	
		
		
		
		if(state[1]== 'ScopeCompletedSuccess'){ # download scope results === xxx
		#if(state[1]== 'ScopeOutDownloaded'){ # download scope results ===
				
			#download scope files
			sc.out= shell(sf('%s/scope.exe dir %s -vc %s',
							ScopeFolder, CosmosDir, vc), wait=T, intern = T)
			sc.outStreams= df(stream= gsub('.+(http:.+) $','\\1', grep('Stream', sc.out, v=T)), 
					file= gsub('.+/([^/]+) $','\\1', grep('Stream', sc.out, v=T)), 
					len= nu(gsub('.+:(.+)$','\\1', grep('Committed Length', sc.out, v=T)))) 
			sink('log.txt', append =T); pr(sc.outStreams); sink()
			
			#down.files= cn('nConversions ClkPub ClkPubAU ClkPubA_DURL99'); 
			flist= list()
			for(f in sus(sc.outStreams, len< 1e8)$stream ){catt(1566, flist[f]<- ff<-  gsub('.+/([^/]+)$','\\1', f))
				cmd=sf('%s/scope.exe copy %s %s/%s -vc %s'
						, ScopeFolder, f, getwd(), ff, vc)
				catt(1577, cmd)
				sh= shell(cmd, wait=T, intern = T)
				catt(1622,sh)
			}
			print(flist)
			
			
			#check input files
			#try({(wc(sf('*tsv',getwd())))}, s=T)
			
			SetState(c('ScopeOutDownloaded',getwd()))
			
			sa()
			
		}
		
		if(state[1]%in% c('ScopeOutDownloaded', 'RError')){ # calc score === xxx
            if(RRep %in% c('this', 'self')){SetState(c('ScopeOutDownloaded',state[2], RRep)); return(state)}
            
			setwd(AegColluDir.tsv<- state[2])
			
			e= try(source(RRep),s=T)
            if (inherits(e, 'try-error')) SetState(c('RError',state[2], RRep))
            else SetState('Scored')
		}
		
		#stop('Scored - Done!') ## some folder hard coded below =======
		
		if(state[1]== 'Scored'){ # upload scope reports ===
			dir(sf('\\\\transfer\\transfer\\%s\\AegCollu',msalias))
			
			sh= shell(sf('del \\\\transfer\\transfer\\%s\\AegCollu\\*.* /S /Q',msalias), wait=T, intern = T)
			sh
		
			#sh= shell(sf('robocopy %s T:\\PubScore   /XF *.csv *.tsv *.txt *.RData /S', getwd()), wait=T, intern = T)
			sh= shell(sf('robocopy %s \\\\transfer\\transfer\\%s\\%s   /XF *.csv *.tsv *.txt *.RData /S', getwd(),msalias, ScopePref), wait=T, intern = T)
			sh
			
			
			#upload result files to Cosmos
			flistUp= list()
			for( f in dir('.', '*.csv')){catt(2566, flistUp[f]<- ff<-  sf('%s', f))
				cmd=sf('%s/scope.exe copy %s/%s http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore/%s/up/%s	-vc http://cosmos04.osdinfra.net:88/cosmos/adcenter.BICore -u phx\\%s'
						, ScopeFolder, getwd(),f, CosmosDir,f, msalias)
				catt(2577, cmd)
				sh= shell(cmd, wait=T, intern = T)
				catt(2622,sh)
			}
			flistUp	
			
			
			SetState('RepUploaded')
		}
        return(state<- GetState())
} #RunJob()



if(0){
    RunJob(TheCodeFolder= 'M:/93_AegisPubScore', ScopeFolder= 'm:/Scope1',
            ScopeTempl.script= "../93_PubSc_wPV-templ.script",
            ScopePref= "93_PubSc_wPV",
            RRep= '../dummy.r',
            ndays= 1, Timy='hour',
            Rep.vers= 'wPV') 
    

    
    
    transform.scopeScript(script='M:/93_AegisPubScore/93_PubSc_wPV-Loop-templ.script', fileShow= T
            , hourly=T, outScript= 'test.script', outScriptDir=gw(), StartDate= sf('@"%s"', '2111-11-11'), EndDate=sf('@"%s"', '2012-12-12')
            , root= sf('@"%s/"', 'my'))
    transform.scopeScript(script='M:/93_AegisPubScore/93_PubSc_wPV-Loop-templ.script', fileShow= T
            , hourly=T, outScript= 'test.script', outScriptDir=gw(), StartDate= sf('"%s"', '2111-11-11')
            , EndDate=sf('"%s"', '2012-12-12')
            , root= sf('"%s/"', '"my"'))
}

##########################################################
RunJob= function(LocalRoot= 'M:/test', CosmosRoot= "/my/test"
        , ScopeFolder= options()$ScopeFolder, msalias= options()$msalias, vc= options()$vc, prior= options()$prior
		, ScopeTempl.script= "../93_PubSc_wPV-templ.script"
		, JobName= "test99", Job.Version= 'wPV'
		, RRep= '../dummy.r', RepCopyToDir= ''
		, TimeWindow='3h'
		, LastDay= ch(Sys.Date()-1), toDebug= F) { 
	#TimeWindow='7h'
	Timy=   ifelse(grepl('[hH]$',TimeWindow),'hour', 'dai') 
	ndays=  ifelse(grepl('[hH]$',TimeWindow), 1, nu(gsub('.$','',TimeWindow)))
	nhours= if(grepl('[hH]$',TimeWindow)) nu(gsub('.$','',TimeWindow)) else 24
    #CosmosRootOut= CosmosRoot %+% '/'  %+% JobName
	#CosmosRootOut= sf('%s/%s', CosmosRoot, JobName)
	CosmosRootOut= CosmosRoot
	
	argss= list(); 
	for(f in names(formals()))try({argss[[f]]= eval(parse(text= f))}, s=T)
#	catt('RunJob: List of Arguments:')
#	str(argss)
	
	#dump all vars
	varnames<- ls()
	dump= lapply(varnames, function(x)eval(parse(text= x)))
	names(dump)= varnames
	for(nd in names(dump))if(nd %in% names(formals()))dump[[nd]]=NULL
	#for(d in dump)if(names(d) %in% names(formals()))d=NULL
	
	
	if(toDebug){
		catt('RunJob: List of locals:')
		str(dump)
		#return()	
	}
	
	
	
	if(!file.exists(LocalRoot))dir.create(LocalRoot)
	stopifnot(file.exists(LocalRoot))
	
	catt('RunJob: Setting wd to  LocalRoot=', LocalRoot)
	setwd(LocalRoot);
	
	source('m:/zmFunctions.r')   #load(file = ".Rdata") # save.image()  xxx
	
	logg= function(...){catt(...); cat(format(Sys.time(), "%Y-%b-%d %H:%M:%S "),..., '\n', file = "log.txt", append = T)}
	loggState= function(s,State){catt('loggState:',s);str(State);
		cat(format(Sys.time(), "%Y-%b-%d %H:%M:%S "),s, file = "log.txt", append = T)
		for(s in names(State))cat(sf('    $%+10s : %s\n', s, State[s]),file = "log.txt", append = T)
	}
	
	#SetState= function(s){dput(State<<- s, file="State.txt"); catt('RunJob: SetState: ==== State set to ', str(State)); logg('== State set to ', str(State)); State} 
	SetState= function(s, change= T){
		if(change){State=GetState()
			for(x in names(s))State[x]=s[x]; 
			loggState('RunJob: SetState: ==== State changes :', s); 
		}else State=s
		
		dput(State, file="State.txt"); 
		loggState('RunJob: SetState: ==== State set to ', State); 
		State
	} 
	GetState= function(){ if(!file.exists('State.txt'))SetState(list(state='Init', dump=dump, LocJobSubdir=LocJobSubdir), change=F)
		State= dget("State.txt"); 
		if(toDebug) {catt('RunJob: GetState: State='); str(State)}
		State
	} #  unlink("State.txt");
	
	# 0) define report date ===
	LastDay= as.Date(LastDay)
	
	
	
	
	(LocJobSubdir= sf('%s.%s-%s', format(LastDay,'%m-%d'), TimeWindow, Job.Version)) # [1] "03-05"  # Sat of the Last Week
	scopeSubdir= LocJobSubdir
	catt('899,LocJobSubdir=', LocJobSubdir)
	
	# 1) Creates local and Cosmos output Directories
	if(!file.exists(LocJobSubdir))dir.create(LocJobSubdir)
	stopifnot(file.exists(LocJobSubdir))
	
	#AegColluDir.tsv<<-  LocJobSubdir
	logg('RunJob: Output dir :',LocJobSubdir)
	
	scName= sf('%s-%s', JobName, scopeSubdir)
	#if(CosmosRootOut== '' ) CosmosRootOut= sf('/my/%s/%s', JobName, scopeSubdir)#, mmdd)
	CosmosOut= sf('%s/%s', CosmosRootOut, scopeSubdir)
	
	setwd(LocJobSubdir); getwd()
	State= GetState() 
	
	
	
	
	# 2) Creates Scope script from template
	
	#'scope -encode -u phx\alexzol'
	
	if(State$state== 'Init'){
		nweeks=0
		ss= readLines(ScopeTempl.script, warn = F)
		ndays<<- ndays
		f= gsub('\\.script', '.' %+%  fDate(LastDay) %+% '.' %+%  TimeWindow 
						%+% '-' %+%  Job.Version %+% '\\.script', ScopeTempl.script)
		if(0){
			s=ss
			#s= gsub('_hourly_', '_daily_', s)
			if(dummy<- F){ #fast debug run                   # <<<------------------------ debug hourly run
				s= gsub('"dai"', '"hour"', ss)
				s= gsub('@"_1W_"', '@"_1H_"', s)        
				s= gsub('@StartDate', sf('@"%s"',LastDay), s)       
			}
			
			s= gsub('@EndDate',   sf('@"%s"', LastDay), s)
			s= gsub('@StartDate', sf('@"%s"', LastDay- ndays + 1), s)
			s= gsub('@EndHour',   sf('@"%02d"', 8+nhours), s) #nhours=1
			s= gsub('@Timy',      sf('@"%s"', Timy), s)
			
			s= gsub('@root',      sf('@"%s"', CosmosOut), s)
			
			
			he(s,40)
			writeLines(he(s,9999), con = f)
			file.copy(f, gw())
			#file.show(f)                   
		}
		
		if(1)s= transform.scopeScript(script=ScopeTempl.script, fileShow= T
					, hourly=(Timy=='hour'), outScript= gsub('.*?([^\\/]*)$', '\\1', f) , outScriptDir=gw()
					, StartDate= sf('@"%s"', LastDay- ndays + 1), EndDate=sf('@"%s"', LastDay)
					, EndHour= sf('@"%02d"', 8+nhours)
					, root= sf('@"%s/"', CosmosOut)
					, Timy=sf('@"%s"',Timy))
		
#            sh= scope(ScopeFolder=ScopeFolder, vc=vc
#                    , sf('submit -notify %s@microsoft.com,alexzol@microsoft.com -f %s -i %s -p 900'
#                            ,  msalias, scName, f))
		exeScopeScript= file.path(gw(),gsub('.*?([^\\/]*)$', '\\1', f))
		
		#brr()
		
		sh= scope(ScopeFolder=ScopeFolder, vc=vc
				, sf('submit -notify %s@microsoft.com,alexzol@microsoft.com -f %s -i %s -p 900'
						,  msalias, scName, exeScopeScript)) 
		#brr()
		if(!any(grepl(' successfully', sh)) ){prr(s); stop('RunJob:Scope: Not successfully!!!')}
		
		
		sjobId= grep('Script submitted, job Id', sh, v=T)
		jobId= strsplit(sjobId[[1]],' ')[[1]]
		logg(jobId)
		
		jobId= jobId[[le(jobId)]]   # [1] jobId=  "1727b4bc-9bc4-48cb-b507-3e7687ba2408"
		#SetState(c('ScopeSubmitted',jobId, scName)) 
		SetState(list(state='ScopeSubmitted', exeScopeScript=exeScopeScript, jobId= jobId, scName=scName, wd=gw()
						, hlink=sf('%s/_Jobs/%s', vc, jobId))) 
	}   
	
	
	
	State= GetState()
	if(State$state== 'ScopeSubmitted'){ # monitor Scope execution ===
		
		jobId= State$jobId
		scName= State$scName
		
		#sh= shell(sf('%s/scope.exe -joblist -vc http://cosmos04.osdinfra.net:88/cosmos/adcenter.BICore -u phx\\%s', ScopeFolder, msalias), wait=T, intern = T)
		#Zengpan:
		#scope(cmd='-submit', ScopeFolder='m:/scope1', vc='http://cosmos05.osdinfra.net:88/cosmos/adCenter.BICore.Fraud',...) {
		sh= scope(cmd='-joblist', ScopeFolder=ScopeFolder, vc=vc)
		
		he(sh,10)
		
		
		#ii= grep(sf('Job Name.*%s',scName),sh)
		ii= grep(jobId,sh)
		#for(i in ii)catt(sh[(i-3):(i+3)])
		for(i in ii)try({print(sh[(i):(i+5)])}, s=T)
		
		SetState(list(Scope.rc= sh[(ii):(ii+5)]))
		
		(ScopeJobStatus= strsplit(sh[i+1],' ')[[1]][3])
		
		#if(ScopeJobStatus != 'CompletedSuccess'){stop("ScopeJobStatus != 'CompletedSuccess' , Wait  :( ")}
		
		if(ScopeJobStatus == 'CompletedFailure'){SetState(list(state='Init',jobId='after Scope Failure', scName=scName), change=T); try(stop("'Scope: CompletedFailure'  :( \n State is set to Init"), s=T)
			#if(ScopeJobStatus == 'CompletedFailure'){SetState(c('Init','after Scope Failure')); try(stop("'Scope: CompletedFailure'  :( \n State is set to Init"), s=T)
			
			
		} else if(ScopeJobStatus != 'CompletedSuccess'){try(stop("ScopeJobStatus != 'CompletedSuccess,CompletedFailure' ; Wait  :( "), s=T)
		} else  SetState(list(state='ScopeCompletedSuccess',jobId=jobId, scName=scName,ScopeJobStatus=ScopeJobStatus, hlink=sf('%s/_Jobs/%s', vc, jobId), sh=sh[(ii):(ii+5)]))
		
		#he(sh,20)
	}   
	
	
	State= GetState() 
	if(State$state== 'ScopeCompletedSuccess'){ # download scope results === xxx
		#if(State$state== 'ScopeOutDownloaded'){ # download scope results ===
		
		#download scope files
		sc.out= scope(cmd=sf('dir %s', CosmosOut), ScopeFolder=ScopeFolder, vc=vc)
		
		
		sc.outStreams= df(stream= gsub('.+(http:.+) $','\\1', grep('Stream', sc.out, v=T)), 
				file= gsub('.+/([^/]+) $','\\1', grep('Stream', sc.out, v=T)), 
				len= nu(gsub('.+:(.+)$','\\1', grep('Committed Length', sc.out, v=T)))) 
		sink('log.txt', append =T); pr(sc.outStreams); sink()
		
		#down.files= cn('nConversions ClkPub ClkPubAU ClkPubA_DURL99'); 
		flist= list()
		for(f in sus(sc.outStreams, len< 1e8)$stream ){catt(1566, flist[f]<- ff<-  gsub('.+/([^/]+)$','\\1', f))
			sh= scope(cmd=sf('copy %s %s/%s',  f, getwd(), ff),  ScopeFolder=ScopeFolder, vc=vc)
			catt(1622,sh)
		}
		print(flist)
		
		
		#check input files
		#try({(wc(sf('*tsv',getwd())))}, s=T)
		
		#SetState(c('ScopeOutDownloaded',getwd()))
		SetState(list(state='ScopeOutDownloaded', wd=getwd(), flist=flist))
		
		#sa()
		
	}
	
	
	State= GetState() 
	if(State$state %in% c('ScopeOutDownloaded', 'RError', "Scored", "RepUploaded",'RReporting','RReported' )){ # calc score === xxx  debug
		
		#if(State$state %in% c('ScopeOutDownloaded', 'RError')){ # calc score === xxx
		
		
# create output dirs ==
		#repdir= sf('%s/%s/outR/rep',State$dump$argss$LocalRoot, State$ LocJobSubdir)
		repdir= sf('%s/outR/rep',gw())
		SetState(list(repdir=repdir))
		
		#brr()
		
		if(file.exists(repdir)) {logg(sf('Directory %s exists', repdir))
		}else{
			dir.create('outR'); 
			rc= dir.create('outR/rep'); 
			#dir.create('outR/htm/img'); 
			if(rc) logg(sf('Directory %s is created.', repdir))
		}
		stopifnot(file.exists(repdir))	
		
		
		
		#	if(RRep %in% c('', 'this', 'self')){SetState(list(state='ScopeOutDownloaded',jobId=State$jobId, RRep=RRep)); return(State)}
		
		if(RRep %in% c('', 'this', 'self')){SetState(list(state='RReporting', RRep=RRep)); return(State)}
		
		#setwd(AegColluDir.tsv<- State$jobId)
	    wd=gw()
		if(grepl('\\.[rR]$', RRep)) e= try(source(RRep),s=T)
		else e= do.call(RRep, list(State=State))
		#this can change gw()
		sw(wd)
	
		
		if (inherits(e, 'try-error')) SetState(list(state='RError',  RRep=RRep, err=e))
		else SetState(list(state='RReported', RRep=RRep))
	}
	
#stop('Scored - Done!') ## some folder hard coded below =======
#return(State<- GetState()) # xxx debug
	
	State= GetState() 
	if(State$state== 'RReported'){ # upload scope reports ===
		
		if(RepCopyToDir != ''){ #copy reports to \\\\transfer\\transfer
#			if(!file.exists(RepCopyToDir))dir.create(RepCopyToDir)
#			if(!dir.exists(RepCopyToDir))dir.create(RepCopyToDir)
#			brr()
#			
#			stopifnot(file.exists(RepCopyToDir)) 
			dir.create(fp(RepCopyToDir, State$LocJobSubdir), rec=T)
			stopifnot(file.exists(fp(RepCopyToDir, State$LocJobSubdir))) 
			
			dir(fp(RepCopyToDir, State$LocJobSubdir))
			
			sh= execf('del %s\\*.* /S /Q', fp(RepCopyToDir, State$LocJobSubdir))
			sh
			
			#sh= shell(sf('robocopy %s T:\\PubScore   /XF *.csv *.tsv *.txt *.RData /S', getwd()), wait=T, intern = T)
			sh= execf('robocopy %s %s   /XF *.csv *.tsv *.txt *.RData /S', State$repdir, fp(RepCopyToDir, State$LocJobSubdir))
			sh			
		}
		
		
		
		#upload result files to Cosmos
		repdir= State$repdir
		CosmRepDir= sf('%s/report', CosmosOut)
		#brr()
		
		flistUp= list()
		#for( f in dir('.', '*.csv')){catt(2566, flistUp[f]<- ff<-  sf('%s', f))
		for( f in dir(repdir)){catt(2566, flistUp[f]<- ff<-  sf('%s', f))
			sh= scope(cmd=sf('copy %s/%s  %s/%s/%s',  repdir,f,  vc,CosmRepDir,f),  ScopeFolder=ScopeFolder, vc=vc)
		}
		flistUp 
		#brr()
		
		SetState(list(state='RepUploaded', CosmRepDir=CosmRepDir, flistUp=flistUp))
	}
	return(State<- GetState())
} #RunJob()

if(0){
	testRep= function(State) {
		catt('\ntestRep:')
		#str(State)
		stop('testRep errrror')
	}
	
	jo= RunJob(LocalRoot= 'M:/83_ScopeR/testdir', ScopeFolder = "m:/Scope1", msalias='alexzol'
			, vc="http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore.Fraud"
			, ScopeTempl.script= "M:/83_ScopeR/83_ScopeRDemo-Templ.script"
			, JobName= "ScopRtest", Job.Version= '5'
			, RRep= 'testRep' #'RReport' #, '' # 
			, TimeWindow='1d'
			, CosmosRoot= "/my/ScopRtest"
			, LastDay= '2011-10-21') 
	str(jo)
}

#ReadAllTSV= function(folder=gw(), patt='\\..+\\.tsv', sep = "\t", row.max= 800000, MaxLen=1e8){
ReadAllTSV= function(folder='M:/87_AegPubScor2/test', patt='\\..+\\.csv'
			, pattNeg='zzz', sep = ","
			, row.max= 800000, size.max=1e8){
    # 
    #folder='M:/87_AegPubScor2/test'; patt='csv$'; sep = ","; row.max= 800000; size.max=1e8
    ff= df(fn= f<- dir(folder, patt), ds=gsub("^([^\\.]+).+$",'\\1', f)
        , rows= nu(sapply(strsplit(sapply(file.path(folder, f), wc), ' '), function(x)x[1]))
        , size=file.info(file.path(folder, f))$size); print(ff)
    #                                fn           ds   rows
    # 1       AggAdv.1d_07-31_08-06.tsv       AggAdv  39313
    # 2  AggPC2_9999.1d_07-31_08-06.tsv  AggPC2_9999   9999
    # 3     AggPC999.1d_07-31_08-06.tsv     AggPC999   9999
    # 4       AggPub.1d_07-31_08-06.tsv       AggPub    702
    # 5       Clicks.1d_07-31_08-06.tsv       Clicks 226032
    # 6  Join00_9999.1d_07-31_08-06.tsv  Join00_9999   9999
    # 7 JoinPC0_9999.1d_07-31_08-06.tsv JoinPC0_9999   9999
    # 8 nConversions.1d_07-31_08-06.tsv nConversions      1
    # 9           NN.1d_07-31_08-06.tsv           NN      2
    
    
    suffs= unique(gsub('.*?\\.','', ff$fn))
    
    if(le(suffs)!=1){catt('le(suffs)!=1 ,  suffs:', suffs); stop('le(suffs)!=1 ')}
    # le(suffs)!=1 ,  suffs: 1W_04-17_04-23.tsv 1W_04-23_04-23.tsv
    # Error: le(suffs)!=1 
    
    suff= suffs[1]  #
    
    ff1= ff[grep(suff, ff$fn),]
    #                                fn           ds   rows
    # 1       AggAdv.1d_07-31_08-06.tsv       AggAdv  39313
    # 2  AggPC2_9999.1d_07-31_08-06.tsv  AggPC2_9999   9999
    # 3     AggPC999.1d_07-31_08-06.tsv     AggPC999   9999
    # 4       AggPub.1d_07-31_08-06.tsv       AggPub    702
    # 5       Clicks.1d_07-31_08-06.tsv       Clicks 226032
    # 6  Join00_9999.1d_07-31_08-06.tsv  Join00_9999   9999
    # 7 JoinPC0_9999.1d_07-31_08-06.tsv JoinPC0_9999   9999
    # 8 nConversions.1d_07-31_08-06.tsv nConversions      1
    # 9           NN.1d_07-31_08-06.tsv           NN      2
    
    #or
    
    #for(f in(dir('..','.*04-23_04-23.tsv'))){catt(f)
    #   sh<- shell(paste('c:/cygwin/bin/mv.exe  ../', f, ' ../1H', sep=''), wait=T ,  intern=T)
    #}
    
    
    if(0){    # create output dirs ==
       if(file.exists('outR/htm/img')) {logg(sf('Directory %s/img/ exists', file.path(gw(),'outR/htm')))
        }else{
            dir.create('outR'); 
            dir.create('outR/htm'); 
            rc= dir.create('outR/htm/img'); 
            if(rc) logg(sf('Directory %s/outR/htm/img/ is created.', gw()))
        }
        stopifnot(file.exists('outR/htm/img'))
        sw('outR'); gw()  # <-- must be before rt() ; lo(); lss()
    }
  
    
    ## reading all tsv to R dataframes
     # <------ parameter (all ds but  IP3s)
    
    for(i in 1:nrow(ff1)){ # i=4
        x=ff1[i,];print(x);print(x[1,])
        with(x[1,],{if(0 < rows & rows < row.max & size < size.max & !grepl(pattNeg, fn)) 
                        # 
                        
                   # assign(ch(ds), do.call('rtd', list(ch(fn),rows), envir= .GlobalEnv )
                assign(ch(ds), do.call('read.delim', list(file.path(folder,ch(fn)), header = F, sep = sep), envir= .GlobalEnv )
                , envir= .GlobalEnv )})
    }
    print(ff1)
    
}#ReadAllTSV
if(0){rmDF(); ReadAllTSV();lsDF(); gw()}


GetScriptParams= function(script="M:/83_ScopeR/AegisCustomDataSourceView.script",
		outdir='', fileShow= T){
	s1= readLines(script, warn = F)
	s= paste(s1, collapse=' ')
	#s= gsub('\\/', ' ', s)
	s2= strsplit(s, split='@@')[[1]] # split to lines
	s2= s2[!grepl(' |\\/', s2)] 
	s2= sort(unique(s2))

	
	s3= sf('s= gsub("@@%s@@", "", s)\n',s2)
	catt(s3)	
	
	s5= paste(sf('%s=,',s2))
	catt(s5)	
	
	s4= sf('s= gsub("@@%s@@", %s, s)\n', s2, s2)
	catt(s4)	
	
	if(0)GetScriptParams(script="M:/83_ScopeR/AegisCustomDataSourceView.script",
			outdir='', fileShow= T)
}
#  s= gsub("@@p_CI_VERSION@@", "", s)
#  s= gsub("@@p_CurrentMinusTwoDateInDateTime@@", "", s)
#  s= gsub("@@p_DATA_SOURCE@@", "", s)
#  s= gsub("@@p_DayMinus2@@", "", s)
#  s= gsub("@@p_DE_MR_DSV@@", "", s)
#  s= gsub("@@p_EnableDebugging@@", "", s)
#  s= gsub("@@p_End_Date@@", "", s)
#  s= gsub("@@p_KVP_VIEW_PATH@@", "", s)
#  s= gsub("@@p_MonthMinus2@@", "", s)
#  s= gsub("@@p_PubCollOutputDir@@", "", s)
#  s= gsub("@@p_Publisher_Collusion_ClickCount_Threshold@@", "", s)
#  s= gsub("@@p_Publisher_Scoring_Distribution_Reference@@", "", s)
#  s= gsub("@@p_ReferenceDataFilesRoot@@", "", s)
#  s= gsub("@@p_SideStreamsLocation@@", "", s)
#  s= gsub("@@p_SplitLogsRootPath@@", "", s)
#  s= gsub("@@p_Start_Date@@", "", s)
#  s= gsub("@@p_StreamExpiry@@", "", s)
#  s= gsub("@@p_YearMinus2@@", "", s)


transform.scopeScript= function(script="M:/83_ScopeR/AegisCustomDataSourceView.script"
		, fileShow= F
        , hourly= F
		, outScriptFile= gsub('^.*\\/(.+)', '\\1', script)
		, outScriptDir=tempdir(), ...){
    args <- as.list(substitute(list(...)))[-1L]
    
    if(substr(script,1,1)== '$'){
        script= file.show.TFS(TFSpath= gsub('(^.*)\\/(.+)', '\\1', script)
                , fname= gsub('^.*\\/(.+)', '\\1', script), outDir= tempdir(), fShow=F)
    }

    s= readLines(script, warn = F)
    s= gsub("@@p_PubCollOutputDir@@", '"/my/testShiva"', s)

    for(key in names(args)) s= gsub(sf("@@%s@@", key), eval(args[[key]], envir= sys.parent()), s)
    for(key in names(args)) s= gsub(sf("@%s\\b", key), eval(args[[key]], envir= sys.parent()), s) # environment(fun = RunJob) parent.frame()
    
    if(hourly){
		s= gsub("_daily_view", '_hourly_view', s)
		s= gsub("@Timy", '"hour"', s)
		if(!any(grepl("THE_DATE",s))){
            s= gsub("END_DATE", 'START_HOUR = "09", END_HOUR = "09", THE_DATE', s)
        }
    } else {s= gsub('_hourly_view', 'daily_view',  s)
		    s= gsub("@Timy", '"dai"', s)
	}
    
    outScript1<- file.path(outScriptDir, outScriptFile)
    
    catt('Creating OutputFile:',outScript1)
    #print(s[80:100])
    writeLines(he(s,9999), con = outScript1)
    if(fileShow)file.edit(outScript1)
    invisible(s)
}

if(0){
    transform.scopeScript(script="M:/83_ScopeR/AegisCustomDataSourceView.script", fileShow= T
            , hourly=T, p_Start_Date='2011-11-11', p_End_Date='2011-12-12')
    
    transform.scopeScript(script="M:/93_AegisPubScore/93_ConvDownlift.script", fileShow= T
            , hourly=T, StartDate='"2011-11-11"', EndDate='"2011-12-12"')
    
    
    transform.scopeScript(script="M:/93_AegisPubScore/93_ConvDownlift.script", fileShow= T
            , hourly=T, StartDate=sf('"2011-11-%s"',99), EndDate='"2011-12-12"')
    transform.scopeScript(script="$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisScorer.script"
        , fileShow= T, hourly=T, StartDate=sf('"2011-11-%s"',99), StartDateString=sf('"2011-11-%s"',88), EndDate='"2011-12-12"')
}


if(0){
    s= file.show.TFS(TFSpath= '$/BI/TQAlgos/Main/Aegis/PubFraud/'
            , fname= 'Pubs4989score.script', outDir= tempdir())
    
    s= file.show.TFS(TFSpath= '$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope'
            , fname= 'AegisScorer.script', outDir= tempdir())
    
    s1= transform.scopeScript(script= s, fileShow= T, outScriptDir="M:/87_AegPubScor2/test/test2"
            , hourly=T, p_Year="2011"
            ,p_Month="10"
            ,p_Day="07"
            ,p_CFR_Internal_DSV="metadataCFR_Internal__9_5.dsv"
            ,p_DE_MR_DSV="metadataDE_MR__10_1.dsv"
            ,p_SplitLogsRootPath="/shares/adCenter.BICore.prod2/prod/splitlogs"
            ,p_CF_Stats_Location="/shares/adCenter.BICore.prod2/prod/StagingArea0"
            ,p_Aegis_Stats_Location="/my/testShivaAegis"
            ,p_SideStreamsLocation="/shares/searchWebLoad/IndexV2/sidestream"
            ,p_Aegis_Views_Location="/local/prod/pipelines/Aegis"
            ,p_KVP_VIEW_PATH="/shares/adCenter.BICore.views/apsviews/PROD"
            ,p_MAX_PARTITIONS="199"
            ,p_CI_VERSION="ci/33777"
            ,p_DATA_SOURCE="PROD"
            ,p_Aegis_DailyStats_DataAvailableDate="2011-04-18"
            ,p_Store_MonthlyWeekly_Stats="true"
            ,p_Enable_CF_Scoring="true"
            ,p_Enable_Publisher_Collusion="true"
            ,p_Enable_Publisher_Scoring="true"
            ,p_Enable_PublisherScoring_Training="true"
            ,p_Publisher_Collusion_ClickCount_Threshold="1"
            ,p_Publisher_Collusion_InputStats_Window="7"
            ,p_Publisher_Scoring_InputStats_Window="1"
            ,p_EnableDebugging="true"
            ,p_FastDebug="true"
            ,p_Bing_DataSource="uber")
}



NamesFromScript= PrepReadScript3= function(script="M:/93_AegisPubScore/93_ConvDownlift.script",
        outdir='M:/93_AegisPubScore/03-19/wPV/out_1w', fileShow= T){
	libra(plyr)
    pr(wc(file.path(outdir,'*.tsv')))
    
    if(substr(script,1,1)=='$'){
        fname= gsub('^.*\\/([^\\/]+)','\\1',script)
        TFSpath= gsub('(^.*)\\/([^\\/]+)','\\1',script)
        script= file.show.TFS(TFSpath, fname, fShow=F)
    }
    s1= readLines(script, warn = F)
    s1= gsub('(\\/\\/).*$', '', s1) #drop comments
    s1= gsub('EXPORT|DISTINCT', '', s1) 
    s1= gsub('(FROM|WHERE|ORDER)', 'FROM', s1)
    s1= gsub(':[a-z\\?]+([, ])', '\\1', s1) # drop types from EXTRACT
    s1= gsub('EXTRACT', 'SELECT', s1) 
    
    
    s= paste(s1,collapse=' ')
    s2=strsplit(s, split=';|#ENDIF|#IF\\(.*?\\)|FROM')[[1]] # split to lines
    s2= gsub('\\s+', ' ', s2) # drop multiple blanks
    s2= gsub(':[a-z]+([, ])', '\\1', s2) # drop types from EXTRACT
    s2= grep('SELECT|OUTPUT', s2, v=T) 
    
    for(i in 1:10) s2= gsub('\\([^\\(]*?\\)', '', s2) # drop (...)

    
    s2= gsub('(\\,)[^\\,]+? AS', '\\1', s2) # drop  ... AS    
    s2= gsub('(SELECT)[^\\,]+? AS', '\\1', s2) # drop  ... AS    
    vars= ifelse(grepl('SELECT',s2), gsub('.*SELECT ', '', s2), '')
    vars= gsub(' *$', '', gsub('^ *', '', vars))#drop blanks start and end
    vars= gsub('\\, *', ' ', vars) #drop commas
    
    s3= 'names(' %+% gsub('( *=? *SELECT ?)', ')= cn(\\"', s2) %+% '")'
    s3= gsub(' = \\)', ')', s3) 
    s3= gsub('\\( ', '(', s3) 
    s3= gsub('\\\\', '', s3) 
    s3= gsub(' ?" ?', '"', s3) 
    s3= gsub(', ', ' ', s3) 
    dsname= c('',gsub('names\\((.*?)\\).+', '\\1', s3)) 
    ldply(dsname, function(x){y=-1; try({y=nrow(get(x, envir=.GlobalEnv))},s=T);y})
    
    s4= ifelse(grepl('OUTPUT',s3), '    dhe(' %+% dsname %+% ',3) #' %+% nrow(try(get(dsname, envir=.GlobalEnv),s=T)),  s3)
    for(s in s4)catt(s)
    if(fileShow)file.edit(script)
	
	
	for(s in s4) try(eval(parse(text=s),envir= .GlobalEnv ), s=T)
    print(lsDF())
	n=le(s3)
	#invisible(df(out=grepl('OUTPUT',s3)[-1], code=s4[-le(s4)]))
	invisible(sus(df(out=grepl('OUTPUT',s3)[-1]+0
                    , dsname= gsub('names\\((.*?)=? *\\).+', '\\1', s3)[-n]
                    , vars=vars[-n]
                    , code=s3[-n])
			, !grepl('OUTPUT',dsname)))
	#    #return(s4)
#    
#    
#    grep('IF', s2, v=T) # drop multiple blanks
#    grep('MeanConvRate', s2, v=T) # drop multiple blanks
#    
#    
#    s2= gsub('\\(.*?\\)', '', s2) # drop (...)
#
#    s2= gsub('(SELECT |\\,)[^\\,]+? AS', '\\1', s2) # drop  ... AS
#    s1=s2
#    
#    
#    tx=''; flag=0
#    for(i in 1:le(s1)){ss= s1[i]
#        if(grepl('(SELE|EXTRA)',ss)){
#            if((y<- gsub('^(.*)((SELE|EXTRA).+)', '\\1', ss)) != ''){catt('#  ',y,  SQL2vars2(tx)); tx=''; flag=1}
#            tx=''; flag=1; #catt('\#--', SQL2vars(tx))
#        }
##        if(grepl('(=$)',ss)){
##            if((y<- gsub('^(.*)((=).+)', '\\1', ss)) != ''){catt('#  ',y,  SQL2vars2(tx)); tx=''; flag=1}
##            tx=''; flag=1; #catt('\#--', SQL2vars(tx))
##        }
#        if(flag) tx= tx %+% strsplit(ss,'FROM')[[1]][1]
#        if(grepl('(FROM|ORDER|WHERE|;)', ss)){flag=0}
#        if(grepl('@root|OUTPUT', ss)){d= gsub('(.*)"(.+)"(.*)','\\2', ss); ## output
#            f= grep(d, dir(outdir,'.tsv'), v=T); if(le(f)==0)f=''
#            nrow=1e6; try({nrow= nu(gsub('\\s*(\\d+)(.*M:.*$)','\\1', wc(file.path(outdir,f)), pe=T))[1]},s=T)
#            catt(sf('%s= d= rt("%s", nrow=%s);\n  names(%s)=names(d)=%s;\n  dhe(%s, 5)\n', d, f, nrow, d, SQL2vars2(tx),d))
#        }
#    }    
}

if(0){
    s= PrepReadScript3(script="$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisScorer.script",
            outdir='', fileShow= T) 
    
    s= PrepReadScript3(script="$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisCustomDataSourceView.script",
            outdir='', fileShow= T) 
}

nope= function(){
    s= PrepReadScript3(script="M:/87_AegPubScor2/2011-09-13 from Shiva/SRank3/AegisCustomDataSourceView.script",
            outdir='M:/87_AegPubScor2/test', fileShow= F)
    s= PrepReadScript3(script="M:/83_ScopeR/83_ScopeRDemo-Templ.script",
            outdir='', fileShow= F)
    s= PrepReadScript3(script="M:/93_AegisPubScore/93_PubSc_noPV-Pos-templ05-28.script",
            outdir='', fileShow= F)
    edit(head(s$code,999))
    
    s= PrepReadScript3(script="M:/83_ScopeR/AegisCustomDataSourceView.script",
            outdir='', fileShow= F)
    
    dir('m:/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/')
    # [1] "AegisCustomDataSourceView.script" "AegisScorer.script"               "AegisScorerLocalRun.cmd"          "AegisScorerSubmit.cmd"           
    # [5] "AegisStatsAggregatorView.script"  "LonghornStats.script"             "ReferenceDataFiles"              
    
    s= PrepReadScript3(script="m:/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisCustomDataSourceView.script",
            outdir='', fileShow= F)
    s= PrepReadScript3(script="m:/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisScorer.script",
            outdir='', fileShow= T)
    
    file.edit("m:/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisCustomDataSourceView.script")
    
    tf= tempfile()
    td= tempdir()
    fname= 'AegisScorer.script'
    cmd= '"C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\Common7\\IDE\\tf.exe" dir $/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope'
    cmd= '"C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\Common7\\IDE\\tf.exe" view $/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisScorer.script'
    cmd= '"C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\Common7\\IDE\\tf.exe" view /output:z:/tmp $/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisScorer.script'
    cmd= sf('"C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\Common7\\IDE\\tf.exe" view /output:%s $/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisScorer.script',tf)
    cmd= sf('"C:\\Program Files (x86)\\Microsoft Visual Studio 10.0\\Common7\\IDE\\tf.exe" view /output:%s/%s $/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/%s',td, fname,fname)
    catt('1155. Executing ', cmd)
    sh= shell(cmd, wait=T, intern = T)
    file.edit('z:/tmp')
    file.edit(tf)
    file.edit(file.path(td,fname))
    edit(paste(sh, collapse='\n\r'))
    edit(sh)
    sh= shell(cmd, wait=T, intern = T, tra=T)
}

file.show.TFS= function(TFSpath, fname, outDir= tempdir(), fShow=T, TFexe= options()$TFexe){
    execf('%s view /output:%s %s'
            , TFexe, file.path(outDir, fname), file.path(TFSpath, fname))
    if(fShow)file.edit(file.path(outDir,fname))
    return(file.path(outDir,fname))
}
if(0)file.show.TFS(TFSpath= '$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope'
            , fname= 'AegisScorer.script', outDir= tempdir())
if(0)file.show.TFS(TFSpath= '$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisScorer.script'
            , fname= '', outDir= tempdir()) #nOK
if(0)file.show.TFS(TFSpath= ''   #nOK
            , fname= '$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope/AegisScorer.script', outDir= tempdir())
# [1] "C:\\Users\\ALEXZO~1.000\\AppData\\Local\\Temp\\RtmpmYVs3O/AegisScorer.script"

dir.TFS= function(TFSdir, TFexe= options()$TFexe) execf('%s dir %s', TFexe, TFSdir)

if(0)dir.TFS(TFSdir= '$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope')
if(0)dir.TFS(TFSdir= '$/BI/Pipeline/Rel/10.3/Private/CFR')
#  [1] "$/BI/Pipeline/Rel/10.3/Private/CFR/Microsoft.CFR.Aegis/AegisScorerScope:" "$ReferenceDataFiles"                                                     
#  [3] "AegisCustomDataSourceView.script"                                         "AegisScorer.script"                                                      
#  [5] "AegisScorerLocalRun.cmd"                                                  "AegisScorerSubmit.cmd"                                                   
#  [7] "AegisStatsAggregatorView.script"                                          "LonghornStats.script"                                                    
#  [9] ""                                                                         "7 item(s)"                                                               


# wc(file.path(outdir, "*.tsv")) ===
# character(0)
if(0){
    names(MRLogData)= cn("ClickId UserAgent")
    dhe(MRLogData,3) #
    names(SAConversionsData)= cn("RGUID AdId Conversion")
    dhe(SAConversionsData,3) #
    names(JoinedEventsData)= cn("RGUID SurrogateKey SecondLevelDomain ClientIP Clicks.ClickId AdvertiserAccountId MediumId FraudQualityBand PublisherAccountId PublisherId Referrer RelatedToAccountId RelationshipId MMO PublisherKey UserId UserAgentId UserAgent Click Week Conversion TimeToClick AdPosition AdCount AmountPreDiscount AmountAfterDiscount")
    dhe(JoinedEventsData,3) #
    names(JoinedEventsDataForPubCollusion)= cn("*")
    dhe(JoinedEventsDataForPubCollusion,3) #
    names(IPStatsForConnComps)= cn("ClientIP NumUserAgent NumSecondLevelDomain ClickCount")
    dhe(IPStatsForConnComps,3) #
    names(IPStatsForClickScoring)= cn("ClientIP NumUserAgent NumSecondLevelDomain ClickCount")
    dhe(IPStatsForClickScoring,3) #
    names(SuspiciousIPSLDForConnComps)= cn("SecondLevelDomain ClientIP ImpressionCount ClickCount ConversionCount NumUserAgent")
    dhe(SuspiciousIPSLDForConnComps,3) #
    names(SuspiciousIPSLDForClickScoring)= cn("SecondLevelDomain ClientIP ImpressionCount ClickCount ConversionCount NumUserAgent")
    dhe(SuspiciousIPSLDForClickScoring,3) #
    names(MeanConvRate)= cn("MeanConversionRate")
    dhe(MeanConvRate,3) #
    names(SuspiciousSLD)= cn("SecondLevelDomain ClickCount ConversionCount")
    dhe(SuspiciousSLD,3) #
    names(ConversionRateForEachCollusion)= cn("CollusionId CollusionType ClickCountForCollusion ConversionCountForCollusion ConversionRate")
    dhe(ConversionRateForEachCollusion,3) #
    names(CDLForCollusion)= cn("CollusionId CollusionType CDL")
    dhe(CDLForCollusion,3) #
    names(AegisPublisherCollusionResult)= cn("KeyType Key CollusionId CollusionType CDL")
    names(IPGreyListForWeakPublisherCollusion)= cn("DISTINCT ClientIP")
    names(PageViewData)= cn("PV_RGUID PV_SurrogateKey PV_PublisherId PV_SecondLevelDomain PV_AdUnitId PV_MMO PV_AdvertiserAccountId PV_Referrer PV_ClientIP PV_PublisherKey")
    names(FilteredClickOnlyStats)= cn("FCL_PublisherId FCL_SecondLevelDomain FCL_MMO FCL_PublisherKey FCL_ClickCount FCL_AmountAfterDiscount FCL_LogNumIPClick FCL_CoEffVariationTimeToClick")
    dhe(FilteredClickOnlyStats,3) #
    names(FilteredClickPVStats)= cn("FCLPVS_PublisherId FCLPVS_SecondLevelDomain FCLPVS_MMO FCLPVS_PublisherKey FCLPVS_PVCount FCLPVS_EmptyReferrerCount CTR RPM PercentageOfMissingReferrers")
    dhe(FilteredClickPVStats,3) #
    names(FilteredClickPVPIIStats)= cn("FCLPVPIIS_PublisherId FCLPVPIIS_SecondLevelDomain FCLPVPIIS_MMO FCLPVPIIS_PublisherKey FCLPVPIIS_PVCount PercentageOfMissingIPs")
    dhe(FilteredClickPVPIIStats,3) #
    names(PubScoringStats)= cn("PublisherId SecondLevelDomain MMO PublisherKey LogNumIPClick CoEffVariationTimeToClick CTR RPM PercentageOfMissingReferrers PercentageOfMissingIPs")
    dhe(PubScoringStats,3) #
    names(DistributionReference)= cn("Number RefAdCount RefAdPos RefClicks RefSClicks RefPClicks")
    dhe(DistributionReference,3) #
    names(PublisherBlackList)= cn("PublisherId")
    dhe(PublisherBlackList,3) #
    names(DistribDataAtPos)= cn("PublisherId SecondLevelDomain MMO PublisherKey AdCount AdPosition Clicks")
    dhe(DistribDataAtPos,3) #
    names(DistribDataAtAdCount)= cn("PublisherId SecondLevelDomain MMO PublisherKey AdCount Clicks")
    dhe(DistribDataAtAdCount,3) #
    names(DistribPosAdCount)= cn("PublisherId SecondLevelDomain MMO PublisherKey AdCount DiffDistrib Clicks")
    dhe(DistribPosAdCount,3) #
    names(KolmogorovDistributionData)= cn("PublisherId SecondLevelDomain MMO PublisherKey KolmogorovDistribution")
    dhe(KolmogorovDistributionData,3) #
    names(SideStreamsSet)= cn("DocumentUrl DocID shinglePrint Language InLinkCount InLinkDomainCount MSNBotContentStatusType OriginalHTTPResponseStatus DownloadTime IPAddress RedirectedToURL DocumentType Country RedirectClassifierResult StaticRank1_16 DomainRank16 SpamJunkRuleID SpamUrl SpamPageConfidenceV4 JunkPageConfidenceV3 DocFlags LayoutShingle4SJRE")
    names(SideStreams)= cn("SideStreamURL StaticRank SpamConfidence JunkConfidence")
    names()= cn("url StaticRank3_16 StaticRank2_16 StaticRank1_16 JunkPageConfidenceV3 SpamPageConfidenceV4")
    names(SideStreams)= cn("SideStreamURL StaticRank SpamConfidence JunkConfidence")
    dhe(SideStreams,3) #
    names(PubIdSLDScoreData)= cn("PublisherId SecondLevelDomain MMO StaticRank SpamConfidence LogNumIPClick CTR CoEffVariationTimeToClick KolmogorovDistribution RPM PercentageOfMissingIPs PercentageOfMissingReferrers PublisherY")
    # Warning message:
    # running command 'C:\windows\system32\cmd.exe /c c:/cygwin/bin/wc.exe -l M:/87_AegPubScor2/test/*.tsv*' had status 1 
    
    # wc(file.path(outdir, "*.tsv")) ===
    # character(0)
    
    lsDF()
    
    for(ss in s) try(eval(parse(text=ss),envir= .GlobalEnv ), s=T)
    
    # DistribDataAtAdCount 10117   6 PublisherId SecondLevelDomain MMO PublisherKey AdCount Clicks
    # DistribDataAtPos     42370   7 PublisherId SecondLevelDomain MMO PublisherKey AdCount AdPosition Clicks
    # DistribPosAdCount    10117   7 PublisherId SecondLevelDomain MMO PublisherKey AdCount DiffDistrib Clicks
    # DistributionReference    50   6 Number RefAdCount RefAdPos RefClicks RefSClicks RefPClicks
    # ff                      10   4 fn ds rows size               
    # ff1                     10   4 fn ds rows size               
    # FilteredClickOnlyStats  1731   8 FCL_PublisherId FCL_SecondLevelDomain FCL_MMO FCL_PublisherKey FCL_ClickCount FCL_AmountAfterDiscount FCL_LogNumIPClick FCL_CoEffVariationTimeToClick
    # FilteredClickPVPIIStats  1731   6 V1 V2 V3 V4 V5 V6             
    # FilteredClickPVStats  1731   9 FCLPVS_PublisherId FCLPVS_SecondLevelDomain FCLPVS_MMO FCLPVS_PublisherKey FCLPVS_PVCount FCLPVS_EmptyReferrerCount CTR RPM PercentageOfMissingReferrers
    # KolmogorovDistributionData  1705   5 PublisherId SecondLevelDomain MMO PublisherKey KolmogorovDistribution
    # PublisherBlackList       6   1 PublisherId                   
    # PubScoringStats       1731  10 PublisherId SecondLevelDomain MMO PublisherKey LogNumIPClick CoEffVariationTimeToClick CTR RPM PercentageOfMissingReferrers PercentageOfMissingIPs
    # rfi                      9   1                               
    # x                        1   4 fn ds rows size               
    
    lsDF()
    
    sh<- shell(paste('c:/cygwin/bin/wc.exe -l ', f, '*', sep=''), wait=T ,  intern=T)
    
    sh<- shell(paste('c:/cygwin/bin/wc.exe -l ', f, '*', sep=''), wait=T ,  intern=T)
#    PS M:\84_700badIPs> Get-Content  "Active FlaggedIPs_Tests.csv"|Measure-Object -line
#    %SystemRoot%\system32\WindowsPowerShell\v1.0\powershell.exe
    
    d= dir('M:/84_700badIPs')
    f= 'M:/84_700badIPs/84_700badIPs.r'
    sh<- shell(paste('powershell.exe -command "Get-Content ', f, '| Measure-Object -line" ', sep=''), wait=T ,  intern=T)
    sh<- shell(paste('powershell.exe -command \'Get-Content ', f, '| Measure-Object -line\' ', sep=''), wait=T ,  intern=T)
    
    file.info(dir('M:/84_700badIPs'))
    file.info('M:/84_700badIPs/84_700badIPs.r')
    
    ldply(dir('M:/84_700badIPs'), function(fname) df(fname,file.info(file.path('M:/84_700badIPs',fname))))
}


#download scope files
DownloadScopeFiles= function(ScopeFolder= options()$ScopeFolder
		, CosmosDir='/local/prod/pipelines/Aegis/PublisherCollusion/2011/09/11/Debug/'
		, LocDir='', MaxLen=1e7, patt='[tc]sv$'
		, pattNeg='zzz'
		, vc= options()$vc){
#        sc.out= shell(sf('%s/scope.exe dir %s -vc %s',
#                        ScopeFolder, CosmosDir, vc), wait=T, intern = T)
#        sc.outStreams= df(stream= gsub('.+(http:.+) $','\\1', grep('Stream', sc.out, v=T)), 
#                file= gsub('.+/([^/]+) $','\\1', grep('Stream', sc.out, v=T)), 
#                len= nu(gsub('.+:(.+)$','\\1', grep('Committed Length', sc.out, v=T)))) 
	
	sc.outStreams= dir.scope(ScopeFolder, CosmosDir, vc)
	sink('log.txt', append =T); pr(sc.outStreams); sink()
	
	
	if(!file.exists(LocDir))dir.create(LocDir)
	stopifnot(file.exists(LocDir))        
	
	
	#down.files= cn('nConversions ClkPub ClkPubAU ClkPubA_DURL99'); 
	flist= list()
	for(f in sus(sc.outStreams, len< MaxLen & grepl(patt, file) & !grepl(pattNeg, file))$stream ){
		catt(1566, flist[f]<- ff<-  gsub('.+/([^/]+)$','\\1', f))
		cmd= sf('%s/scope.exe copy %s %s/%s -vc %s'
				, ScopeFolder, f, LocDir, ff, vc)
		catt(1577, cmd)
		sh= shell(cmd, wait=T, intern = T)
		catt(1622, sh)
	}
	print(flist)
	
	
	#check input files
	#try({(wc(sf('*tsv',getwd())))}, s=T)    
	ff= ldply(dir(LocDir), function(fname) df(fname, file.info(file.path(LocDir,fname))))
	catf('Files from %s   %s  with Length < %d\nin folder %s now:\n', vc, CosmosDir, MaxLen, LocDir)
	print(ff)
	ff
}
if(0){
	DownloadScopeFiles(ScopeFolder='m:/Scope1'
			, CosmosDir='/local/prod/pipelines/Aegis/PublisherScoring/2011/09/24/Debug/'
			#, LocDir='"M:/87_AegPubScor2/2011-09-13 from Shiva/24"'   # no blanks in path
			, LocDir='M:/87_AegPubScor2/test'
			, MaxLen=1e8, vc='http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore.Fraud')
	
	DownloadScopeFiles(ScopeFolder='m:/Scope1'
			, CosmosDir='/my/84_700badIPs/'
			#, LocDir='"M:/87_AegPubScor2/2011-09-13 from Shiva/24"'   # no blanks in path
			, LocDir='M:/84_700badIPs/test1'
			, MaxLen=1e8, vc='http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore')
}



#download scope files
Upload2Scope= function(ScopeFolder= options()$ScopeFolder
		, CosmosDir='/my/tmp/'
		, LocDir='',  patt='[tc]sv$'
		, pattNeg='zzz'
		, vc= options()$vc){
#        sc.out= shell(sf('%s/scope.exe dir %s -vc %s',
#                        ScopeFolder, CosmosDir, vc), wait=T, intern = T)
#        sc.outStreams= df(stream= gsub('.+(http:.+) $','\\1', grep('Stream', sc.out, v=T)), 
#                file= gsub('.+/([^/]+) $','\\1', grep('Stream', sc.out, v=T)), 
#                len= nu(gsub('.+:(.+)$','\\1', grep('Committed Length', sc.out, v=T)))) 
	
	sc.Streams= dir.scope(ScopeFolder, CosmosDir, vc)
	sink('log.txt', append =T); pr(sc.Streams); sink()
	
	
	if(!file.exists(LocDir))stop(sf('LocDir %s does not exist',  LocDir))

	
	#down.files= cn('nConversions ClkPub ClkPubAU ClkPubA_DURL99'); 
	flist= list()
	for(f in sus(ff<- dir(LocDir), grepl(patt, ff) & !grepl(pattNeg, ff))){
		catt(1566, flist[f]<- ff<-  gsub('.+/([^/]+)$','\\1', f))

		sh= scope(sf('copy %s/%s %s/%s/%s ', LocDir,f,  vc,CosmosDir, f))
		catt(1622, sh)
	}
	#print(flist)
	
	
	#check input files
	#try({(wc(sf('*tsv',getwd())))}, s=T)    
	#ff= ldply(dir(LocDir), function(fname) df(fname, file.info(file.path(LocDir,fname))))
	#catf('Files from %s   %s  with Length < %d\nin folder %s now:\n', vc, CosmosDir, MaxLen, LocDir)
	catf('Files from %s uploaded to   %s/%s :\n',LocDir,  vc, CosmosDir)
	print(flist)
	
	pr(dir.scope(ScopeFolder, CosmosDir, vc))
	
	ff

}
if(0){
	Upload2Scope(ScopeFolder='m:/Scope1'
			, CosmosDir='/my/tmp/'
			, LocDir='M:/87_AegPubScor2/test', patt='^P.*csv'
			, vc='http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore.Fraud')

	
	DownloadScopeFiles(ScopeFolder='m:/Scope1'
			, CosmosDir='/my/84_700badIPs/'
			#, LocDir='"M:/87_AegPubScor2/2011-09-13 from Shiva/24"'   # no blanks in path
			, LocDir='M:/84_700badIPs/test1', patt='^D.*csv'
			, MaxLen=1e8, vc='http://cosmos04.osdinfra.net:88/cosmos/adCenter.BICore')
	expl('M:/87_AegPubScor2/test')
}



######################################################
##########################################################
