# First thing first: loading the libraries
library(RSelenium)
library(xml2)
library(rvest)
library(devtools)
library(wdman)
library(stringr)
library(httr)
library(keyring)
library(rebus, verbose=FALSE)
library(lubridate, verbose=FALSE)
library(seleniumPipes, verbose=FALSE)
library(gtools) # Allows smartbind

# Due dilligence: cleaning the environment.
rm(list=ls())

# Setting the time boundaries that interest us.
startdate<-"2018-07-01"
enddate<-today()
today<-today()
# This one is going to be useful later on but needs to be set now so as to appear outside of the main function.
# The exact date itself is not relevent.
maxDate<-"2018-07-01"

studioz<- c("https://app.eversports.com/admin/XXXX/","https://app.eversports.com/admin/XXXX/",
            "https://app.eversports.com/admin/XXXX/","https://app.eversports.com/admin/XXXX/")
namez <- c("CIT", "SHS", "MZ","DAR")
ztudioz <- data.frame(studioz, namez)


# In order to be nice and loopy, the whole script is set as a function. This appeared a necessary step as this
# version of Selenium has an unfortunate tendency to crash. Making it a function allows to relauch the script
# easily after a crash.
whole_thing<-function(){ 
  #REVERSE    

  #REVERSE
for (tudioz in ztudioz[,1]){
  #tudioz <- ztudioz[,1]  
  
    # As the script has a tendency to crash, just setting a starting date is not sufficient as it would be
    # if we could expect the script to do everything in one single go. As a result, it is necessary to
    # determine what has already been scraped and what remains to be. This implies loading the existing
    # data and finding the most recent event that has been scraped. Due to the structure of the site, doing
    # this implies determining two variables: 1) what was the last day scraped and 2) what was the last event
    # scraped.
    
    # To be on the safe side, I decided not to write all the data in a single document but to write one file
    # per event. As a result, multiple .csv files have to be loaded. This complexifies the reading process
    # a little and forces to create a list of files and then to cast that list into a single data frame.
    file_list <- list.files(paste("~/Documents/KPIs (do not delete)/",subset(ztudioz,ztudioz$studioz==tudioz)$namez,sep=""), pattern="*.csv", full.names=TRUE) #load all the files in the data_sales folder
    exmaster <- lapply(file_list, read.csv, sep=",")
    exmaster<-do.call(smartbind, exmaster)
    
    # Collect all the days that have already been covered so as to remove them from the list of days left to scrap.
    exDates<-ifelse(exmaster$date=="2018-12","2018-12-05",as.vector(exmaster$date))
    #zob<-as.Date(as.factor(zob[1]))
    exDates<-unique(exDates)
    # A small issue arises here as there is no garantee that the script crashed at the end of a day. The last day
    # scraped thus needs to be kept (and as a consequence removed from the exDates vector which only includes fully
    # scraped days).
    maxDate<-max(as.Date(exDates))
    print(maxDate)
    exDates<-subset(exDates,as.Date(exDates)<maxDate)
    
    # Session IDs must also be collected in order to know which events have been already scraped and which have not.
    # Unlike dates, sessions IDs are not naturally sorted, it is thus necessary to have a full list of which have
    # already been scraped and which have not, a single breakpoint would not do the trick.
    exIDs<-as.data.frame(unique(exmaster$session_ID))
    #subset(exIDs,exIDs[,1]==4835747)
    #must collect this one by hand
    
    # Due to the tendency of the session on the website to end unexpectedly, the entering of the username and the WP
    # had to be turned into a function that could be called regularly.
    accessEV<-function(){
        # Indication of the box that will be targetted.
        webElem = remDr$findElement(using = 'name', 'username')
        # This box has to be cleared as there a preexisting filling.
        webElem$clearElement()
        # Entering the email address of the administrator.
        webElem$sendKeysToElement(list('XXXXXX'))
        webElem = remDr$findElement(using = 'name', 'password')
        webElem$clearElement()
        # To avoid the password to be read directly from the file, it is protected using the keyring library. Note
        # that there exist an option to password-protect this information, but it would require to enter a password
        # every single time the script is run which is burdensome considering its tendency to crash.
        webElem$sendKeysToElement(list(key_get("YYYYYY")))
        webElem = remDr$findElement(using = 'class', 'btn')
        webElem$submitElement()
        # When scraping, it is necessary to put the system to sleep on a regular basis. This lengthens the process
        # quite considerably but it also prevents the servers of the site being scraped from receiving too many
        # queeries in a limited amount of time.
        Sys.sleep(1) ###sleep
        # At this step (specially in the early stages of development), it is necessary to confirm visually that
        # Selenium has effectively gained access to the targeted site. Taking a screenshot does just that.
        remDr$screenshot(TRUE)
        # As mentioned, the session on the site has a tendency to end abruptly. I use this signal to indicate that
        # the session was ended and then restarted. See below for more.
        print("through1")}
    
    ####################### TO BE DELETED ########################
    ### Creating the "master" dataframe
    #master<-data.frame(A = 1,B = 1,C = 1,D = 1,AA = 1,BB = 1,CC = 1,DD = 1)
    #colnames(master)<-c("data-session-participant-id","data-participant-id","data-user-id","data-clubgroupuser-id",
    #"contract","trainer","session","session_ID")
    #master <- master[-c(1),]
    
    datefile<-as.data.frame(format(seq(as.Date(startdate), as.Date(enddate), by="days"), format="%d.%m.%Y"))
    datefile[,2]<-format(seq(as.Date(startdate), as.Date(enddate), by="days"), format="%Y-%m-%d")
    datefile<-datefile[!datefile[,2] %in% exDates,]
    
    datefile$wday<-weekdays(as.Date(datefile[,2]))
    datefile<-subset(datefile, datefile$wday==datefile[1,3])
    datefile<-as.character(datefile[,1])
    
    ### Does not work any more
    eCaps = list(chromeOptions = list(args = c('--headless', '--disable-gpu', '--window-size=1280,800')))
    driver <- rsDriver(browser=c("chrome"), chromever="74.0.3729.6", extraCapabilities = eCaps)
    cDrv = chrome()
    remDr = remoteDriver(browserName = "chrome", port = 4567, extraCapabilities = eCaps)
    remDr$open()
    
    #If problem with port already in use: open Terminal, paste sudo lsof -i :4567 and then sudo kill -4567
    
    #cDrv = chrome()
    #eCaps = list(chromeOptions = list(
    #args = c('--headless', '--disable-gpu', '--window-size=1280,800')
    #))
    #remDr = remoteDriver(browserName = "chrome", port = 4567,
    #extraCapabilities = eCaps)
    #remDr$open()
    
    remDr$navigate("https://app.eversports.com/")
    remDr$screenshot(display = TRUE)
    
    accessEV()
    #remDr$screenshot(TRUE)
    
  for (daties in datefile) { 
      # REVERSE
    #daties<-datefile[1]    
    
    remDr$navigate(paste(tudioz,"classes", sep=""))  
        #remDr$screenshot(display = TRUE)
        
        ### Navigate in the day table and collect infos
        
        webElem = remDr$findElement(using = 'id', 'filter-fromdate')
        webElem$clearElement()
        webElem$sendKeysToElement(list(daties))
        Sys.sleep(1)
        remDr$screenshot(TRUE)
        webElem$sendKeysToActiveElement(list(key = "enter"));
        Sys.sleep(1)
        remDr$screenshot(TRUE)
        webElem$sendKeysToActiveElement(list(key = "enter"))
        Sys.sleep(1)
        remDr$screenshot(TRUE)
        webElem$sendKeysToActiveElement(list(key = "enter"))
        Sys.sleep(1)
        remDr$screenshot(TRUE)
        print(daties)
        
        #Create a db with the session id of all the classes du jour
        text_base1 = read_html(remDr$getPageSource()[[1]])
        list_tr = html_nodes(text_base1, "tr")
        data.session.id=xml_attrs(list_tr, "data-session-id")
        
        #remove classes with 0 student
        list_td = html_nodes(text_base1, "td")
        ortraum=as.data.frame(html_text(list_td, ".room"))
        ortraum <- as.data.frame(ortraum[seq(7, nrow(ortraum), 9), ])
        ortraum$nrow<-rownames(ortraum)
        ortraum[,1]<-as.character(ortraum[,1])
        ortraum[,1]<-ifelse(ortraum[,1]=="","unbekannte",ortraum[,1])
        
        participant=as.data.frame(html_text(list_td, "#participant-count.text-center"))
        participant <- as.data.frame(gsub("/.*","",participant[seq(5, nrow(participant), 9), ]))
        participant$nrow<-rownames(participant)
        
        buchbar <- as.data.frame(html_text(html_nodes(text_base1,"span")))
        buchbar<- subset(buchbar, buchbar[,1]=="buchbar"|buchbar[,1]=="abgesagt")
        
        ###Retrieve data from individual page
        M<-length(data.session.id)
        
        JJJ<-as.data.frame(c(1:M))
        
        for (nb in 1:M) {
            JJJ[nb,]<-ifelse(length(data.session.id[[nb]])<2,0,1)
        }
        
        links<-subset(JJJ,JJJ[,1]>0)
        links[,1]<-as.numeric(as.vector(rownames(links)))
        links[,2]<-0
        
        for (j in 1:length(links[,1])){
            links[j,2]<-data.session.id[[links[j,1]]][["data-session-id"]]
            #JK<-data.session.id[[k]][["data-session-id"]]
        }
        
        links$nrow<-1:length(links[,1])
        links<-merge(links,participant,by=c("nrow"))
        links<-merge(links,ortraum,by=c("nrow"))
        links<-subset(links,links[,4]!=0)
        
        colnames(buchbar)<-c("buchbar")
        buchbar<-buchbar[1:length(links[,1]),]
        links<-cbind(links,buchbar)
        links<-subset(links,links$buchbar=="buchbar")
        
        links<-links[!links$V2 %in% exIDs[,1],]
       # i<-links$V2[1]
        for (i in links$V2) { 
        #REVERSE
            #### IF MUST RELOAD
            remDr$navigate("https://app.eversports.com/")
            Sys.sleep(1)
            checkurl<-remDr$getCurrentUrl()
            tudioz
            links_session<-paste(tudioz,"class/checkin/",i,sep="")
            remDr$navigate(links_session)
            Sys.sleep(3)
            checkurl<-remDr$getCurrentUrl()
            ZOB2=ifelse(checkurl==links_session,function(){print("through2")}, function(){accessEV()})
            ZOB2()
            remDr$getCurrentUrl()
            
            text_base2 = read_html(remDr$getPageSource()[[1]])
            
            list_studio = html_nodes(text_base2, "span")
            list_studio = html_text(html_nodes(text_base2, ".dropdown-toggle"))
              
            #data_studio=html_text(list_studio, "header-facility-name")
            #studio<-data_studio[1]
            
            list_tr = html_nodes(text_base2, "tr")
            
            data_participant_id=xml_attrs(list_tr, ".data-participant-id")
            
            L<-length(data_participant_id)
            
            DDD<-as.data.frame(c(1:L))
            
            for (nb in 1:L) {
                DDD[nb,]<-ifelse(length(data_participant_id[[nb]])<12,0,1)
            }
            
            FFF<-subset(DDD,DDD[,1]>0)
            
            D<-length(FFF[,1])
            dataset <- data.frame(A = 1:D,B = 1:D,C = 1:D,E = 1:D)
            
            super_data<-c("data-session-participant-id","data-participant-id","data-user-id","data-clubgroupuser-id")
            colnames(dataset)<-super_data
            
            FFF[,1]<-as.numeric(as.vector(rownames(FFF)))
            FFF[,2]<-1:length(FFF[,1])
            #dataset<-dataset[-c(1), ]
            #`data-session-participant-id`
            for (nb in FFF[,1]) {
                #for (nb2 in FFF[,2]) {
                #for (ccc in super_data) {
                dataset$`data-session-participant-id`[nb-1]<-
                as.numeric(as.vector(data_participant_id[[nb]][["data-session-participant-id"]]))
                
                dataset$`data-participant-id`[nb-1]<-
                as.numeric(as.vector(data_participant_id[[nb]][["data-participant-id"]]))
                
                dataset$`data-user-id`[nb-1]<-
                as.numeric(as.vector(data_participant_id[[nb]][["data-user-id"]]))
                
                dataset$`data-clubgroupuser-id`[nb-1]<-
                as.numeric(as.vector(data_participant_id[[nb]][["data-clubgroupuser-id"]]))
                
                #dataset$ccc[nb2]<-as.numeric(as.vector(data_participant_id[[nb]][[ccc]]))
            }
            #  }
            #}
            #data_participant_id[[nb]][["data-session-participant-id"]]
            
            #dataset<-subset(dataset,as.numeric(as.vector(dataset$`data-session-participant-id`))>
            #length(data_participant_id))
            
            list_tr = html_nodes(text_base2, "select")
            contract3 = html_text(list_tr, ".participation-payment-select")
            dataset$contract<-0
            for (nb in 1:nrow(dataset)) {
                dataset$contract[nb]<-contract3[(nb+1)]
            }
            dataset$contract<-substr(dataset$contract,start=13, stop=250)
            dataset$nbstudent<-nrow(dataset)
            
            ##Changed
            list_session = html_nodes(text_base2, ".session-name")
            #session<-substr(list_session[2],start=59, stop=1000)
            #session<-gsub('<.*>','',session)
            #dataset$session<-session
            
            type<-gsub('<.*">','',list_session)
            type<-gsub('<.*1>','',type)
            
            session<-html_text(html_nodes(text_base2, ".admin-checkin-header-bottom__event-details"))
            session<-substr(session, start=0,stop=24)
            date_session<-substr(session, start=0, stop=10)
            startzeit<-substr(session, start=12, stop=16)
            endzeit<-substr(session, start=20, stop=24)
            
            #Changed
            list_trainer = html_nodes(text_base2, ".session-trainer")
            #trainer<-gsub('<div class="session-trainer">Lehrer: ','',list_trainer)
            #trainer<-gsub('</div>','',trainer)
            #dataset$trainer<-trainer[1]
            
            trainer<-gsub('<span class="session-trainer">Lehrer ','',list_trainer)
            trainer<-gsub(')</span>','',trainer)
            trainer<-substr(trainer, start=2, stop=1000)            
            dataset$trainer<-trainer
            
            colnames(links)<-c("nrow","not_important","session_ID","nbstudent_true","room")
            room<-subset(links, links$session_ID==i)
            room<-room$room
            
            dataset$session<-session
            dataset$trainer<-trainer         
            dataset$session_ID<-i
            dataset$studio<-subset(ztudioz, ztudioz$studioz==tudioz)$namez
            dataset$startzeit<-startzeit
            dataset$endzeit<-endzeit
            dataset$type<-type
            dataset$date<-as.Date(date_session,format="%d.%m.%Y")
            
            #master<-rbind(master,dataset)
            
            dataset$contract<-ifelse(dataset$contract=="","kein produkt",dataset$contract)
            
            ZOB<-dataset
            
            dataset$room<-room
            #master<-merge(dataset,links,by=c("session_ID"))
            
            ###Eliminer les dates en trop
            
            dataset$date<-as.Date(substr(dataset$session,start=0,stop=10),format="%d.%m.%Y")
            chronocheck<-ifelse(unique(as.Date(dataset$date))<(today),function(){goon()},function(){})
            goon<-function(){
            write.csv(dataset, file=paste("~/Documents/KPIs (do not delete)/",subset(ztudioz,ztudioz$studioz==tudioz)$namez,"/class_ID_",i,"_",dataset$date[1],
                                          ".csv", sep=""), na="")
            }
            chronocheck()   
            print(i)
        }}}
            } #REVERSE
    #remDr$close()

try(whole_thing())
#print("let's begin")
#.rs.restartR()

while(maxDate<enddate){try(whole_thing())
    #print("let's do it again")
      try(.rs.restartR())
}

#.rs.restartR()
