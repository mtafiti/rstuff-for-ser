
#load custom graphing library for serena stuff
# filename: relative to project dir. 'scoped/saliva/serena_log.SERVER.SCOPED.15.03.log'
loadAndCleanSerenaLogFile <- function(filename,processSampling=F){
  
  loadedFile <- stream_in(file(filename))

  loadedFile_filtered <- loadedFile[!(is.na(loadedFile$serverprofile$memory$RSS) | loadedFile$serverprofile$memory$RSS==""), ]

  if ( (length(row(loadedFile_filtered))) < 1) {
    
    #its a client file log from serena
    loadedFile_filtered <- loadedFile[!(is.na(loadedFile$clientprofile$memory$RSS) | loadedFile$clientprofile$memory$RSS==""), ]

    #clean the times to show in seconds (milliseconds too high)
    if (processSampling){ 
      #bug with 60000 and 60000 entries in client profile, process after every 10 iterations
      idxx = seq(1, nrow(loadedFile_filtered), by = 10)
      loadedFile_filtered = loadedFile_filtered[idxx,]
      
    }
    
    loadedFile_filtered$clientprofile$ttime = convertToSeconds(loadedFile_filtered$clientprofile$ttime)
    
    if(exists('loadedFile_filtered$clientprofile$atime') == T)
     { loadedFile_filtered$clientprofile$atime = convertToSeconds(loadedFile_filtered$clientprofile$atime) }
    
    loadedFile_filtered$clientprofile$memoryRSS = convertToMB(loadedFile_filtered$clientprofile$memory$RSS)
    loadedFile_filtered$clientprofile$memoryHeapTotal = convertToMB(loadedFile_filtered$clientprofile$memory$HeapTotal)
    loadedFile_filtered$clientprofile$memoryHeapUsed = convertToMB(loadedFile_filtered$clientprofile$memory$HeapUsed)
    
    #show some stats in the data
    serenaSeeStats(loadedFile_filtered$clientprofile, "VUB Access Case Study - Client profile stats", filename)
    
    #add seconds to colnames
    #colnames(loadedFile_filtered)[5] <- paste(colnames(loadedFile_filtered)[5], "secs")
    
    } else {
      #clean the times to show in seconds (milliseconds too high)
      loadedFile_filtered$serverprofile$ttime = convertToSeconds(loadedFile_filtered$serverprofile$ttime)

      loadedFile_filtered$serverprofile$memoryRSS = convertToMB(loadedFile_filtered$serverprofile$memory$RSS)
      loadedFile_filtered$serverprofile$memoryHeapTotal = convertToMB(loadedFile_filtered$serverprofile$memory$HeapTotal)
      loadedFile_filtered$serverprofile$memoryHeapUsed = convertToMB(loadedFile_filtered$serverprofile$memory$HeapUsed)
      
      serenaSeeStats(loadedFile_filtered$serverprofile, "VUB Access Case Study - Server profile stats", filename)
  }
  
  loadedFile_filtered
}

################# visualize chart #################
createSerenaChart <- function(filteredData, colx, coly){
  
  #TODO
  #h1 <- hPlot(x = "joins", y = "factsadded", data = dk, type = "line")
}


##################utility functions ################
serenaProfileMakeProfile <- function(cprofile,sprofile, key, val){
  ssp = flatten(sprofile$serverprofile) #scoped
  usp = flatten(sprofile$serverprofile) #uscoped
  
  if (is.null(ssp) && is.null(usp)){
    print('checking if client profile')
    ssp = flatten(sprofile$profile) #scoped
    usp = flatten(sprofile$profile) #uscoped
  }
  
  #filter according to val
  filtered_ssp = ssp[ssp[key] == val,]
  filtered_usp = ssp[ssp[key] == val,]

  #data.frame(noscope = r(filtered_usp),scope = r(filtered_ssp))
  mylist <- list()
  mylist[['scope']] <- filtered_ssp[1,] #take first value if many
  mylist[['noscope']] <- filtered_usp[1,]
  
  mylist
}


serenaSeeStats <- function(mydata, title='Descriptive Statistics', info='.summary'){
  filename = gsub(' ', '_', paste(info,'.txt', sep=''))
                  
  stargazer(mydata, type = "text", digits=1, 
            title=title,out=filename)
}


convertToMB <- function(bytes){
  
  bytes / 1048576
}

convertToSeconds <- function(milliseconds){
  
  trunc(milliseconds / 1000)
}