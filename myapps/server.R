require(rCharts)
require(shiny)

library(jsonlite)

source('serenaCharting.R')
source('projscripts/multiLineChart.R')
source('projscripts/multiBarChart.R')


plotWithActivationNoSampling = function(inp, outp){
  
  scopedsalivaserverfile = 'serenalogs/same/scope/saliva/vub_access.server.scoped.19.2.log' #'serenalogs/same/scope/saliva/20hrsrun1/vub_access.server.scoped.17.2.log'
  scopedsalivaclientfile = 'serenalogs/same/scope/saliva/vub_access.client.scoped.19.2.log' #'serenalogs/same/scope/saliva/20hrsrun1/vub_access.client.scoped.17.2.log'
  
  noscopedsalivaserverfile = 'serenalogs/same/noscope/saliva/vub_access.server.20.2.NOscope.log'
  noscopedsalivaclientfile = 'serenalogs/same/noscope/saliva/vub_access.client.NOscope.20.2.log'
  
  scopedsalivaserver_ = loadAndCleanSerenaLogFile(scopedsalivaserverfile)
  scopedsalivaclient_ = loadAndCleanSerenaLogFile(scopedsalivaclientfile)
  
  noscopedsalivaserver_ = loadAndCleanSerenaLogFile(noscopedsalivaserverfile)
  noscopedsalivaclient_ = loadAndCleanSerenaLogFile(noscopedsalivaclientfile)
  
  server.logcolumns = c('factsadded', 'joins', 'ttime', 'memoryRSS', 'memoryHeapUsed')
  server.yaxes = c('No. of facts asserted', 'No of joins', 'Total time (s)', 'Memory (RSS) (MBs)', 'Memory (Heap used) (MBs)')
  
  client.logcolumns = c('factsadded', 'atime', 'ttime')
  client.yaxes = c('No. of facts asserted', 'Notification time (s)', 'Total time (s)')
  
  graph.titles = c('Comparison of Facts Asserted', 'Comparison of Joins Performed', 
                   'Comparison of Memory', 'Comparison of Activation Time', 'Comparison of Total Time')
  
  graph.serverxaxis =  c('No. of activations')
  graph.clientxaxis =  c('No. of notifications')
  
  #charts.labels <- data.frame(server.logcolumns, server.yaxes, client.logcolumns, 
  #                            client.yaxes, graphs.titles, stringsAsFactors=FALSE)
  
  i = 1; #loop index
  colsNo = seq_along(server.logcolumns) #no of columns: calculate offsets
  
  scopedStatsRowRange = (1:length(scopedsalivaserver_$serverprofile$activations))

  noscopedStatsRowRange = (1:length(noscopedsalivaserver_$serverprofile$activations))

  activStep = 50
  
  #calculate the correct axis scale according to the larger  
  #xaxis is 50-interval activations 
  scopecpas = scopedsalivaserver_$serverprofile$activations[scopedStatsRowRange]
  
  scopecpas2 = vector("list", length(scopecpas))
  for (i in seq_along(scopecpas)){
    if (is.na(scopecpas[i])) { scopecpas2[i] = scopecpas2[[i - 1]] + activStep }  else { 
      scopecpas2[i] = scopecpas[i] }
  }
  
  for (i in colsNo){ 
    local({
      currServCol = server.logcolumns[i]
      currServAxisName = server.yaxes[i]
      currXaxisName = graph.serverxaxis[1]
      
      outp[[paste('myServerAChart', i, sep='')]] <- renderChart2({
        
        p <- plotMultiBarChart(noscopedsalivaserver_$serverprofile, scopedsalivaserver_$serverprofile, 
                               scopecpas2, 
                               currServCol, scopedStatsRowRange, currXaxisName, currServAxisName, 'Server')#plot first x values
        
        
        p$set(width=600)
        
        p
      })
      
    })#local
    
  }
  
  #xaxis is 50-interval activations 
  noscopecpas = noscopedsalivaclient_$clientprofile$activations[noscopedStatsRowRange]
  
  noscopecpas2 = vector("list", length(noscopecpas))
  for (i in seq_along(noscopecpas)){
    if (is.na(noscopecpas[i])) { noscopecpas2[i] = noscopecpas2[[i - 1]] + activStep }  else { 
      noscopecpas2[i] = noscopecpas[i] }
  }
  
  clientColsNo = seq_along(client.logcolumns)
  for (i in clientColsNo){ 
    local({
      
      currClientCol = client.logcolumns[i]
      currServAxisName = client.yaxes[i]
      currXaxisName = graph.clientxaxis[1]
      
      outp[[paste('myClientAChart', i, sep='')]] <- renderChart2({
        
        p <- plotMultiBarChart(noscopedsalivaclient_$clientprofile, scopedsalivaclient_$clientprofile, 
                               noscopecpas2, 
                               currClientCol, noscopedStatsRowRange, currXaxisName, currServAxisName, 'Clients')
        
        p$set(width=600)
        
        p
      })
    })#local
    
  }
  
}

plotWith10MinSampling = function(inp, outp){
  scopedsalivaserverfile = 'serenalogs/same/10minsampling/valid/vub_access.server.scoped.24.2.log'
  scopedsalivaclientfile = 'serenalogs/same/10minsampling/valid/vub_access.client.scoped.24.2.log' 
  
  noscopedsalivaserverfile = 'serenalogs/same/10minsampling/valid/vub_access.server.NOscope.25.2.log'
  noscopedsalivaclientfile = 'serenalogs/same/10minsampling/valid/vub_access.client.NOscope.25.2.log'
  
  scopedsalivaserver_ = loadAndCleanSerenaLogFile(scopedsalivaserverfile)
  scopedsalivaclient_ = loadAndCleanSerenaLogFile(scopedsalivaclientfile)
  
  noscopedsalivaserver_ = loadAndCleanSerenaLogFile(noscopedsalivaserverfile)
  #this has 60k sampling, scoped client has 600k, so filter every 10 rows
  noscopedsalivaclient_ = loadAndCleanSerenaLogFile(noscopedsalivaclientfile)
  
  server.logcolumns = c('factsadded', 'joins', 'activations', 'memoryRSS', 'memoryHeapUsed')
  server.yaxes = c('No. of facts asserted', 'No. of joins', 'No. of activations', 'Memory (RSS) (MBs)', 'Memory (Heap used) (MBs)')
  
  client.logcolumns = c('factsadded', 'activation')
  client.yaxes = c('No. of facts asserted', 'No. of notifications')
  
  graph.titles = c('Comparison of Facts Asserted', 'Comparison of Joins Performed', 
                   'Comparison of Memory', 'Comparison of Total Time')
  
  graph.serverxaxis =  c('Time (s)')
  graph.clientxaxis =  c('Time (s)')
  
  #charts.labels <- data.frame(server.logcolumns, server.yaxes, client.logcolumns, 
  #                            client.yaxes, graphs.titles, stringsAsFactors=FALSE)
  
  i = 1; #loop index
  colsNo = seq_along(server.logcolumns) #no of columns: calculate offsets
  
  scopedStatsRowRange = (1:length(scopedsalivaserver_$serverprofile$ttime))
  
  noscopedStatsRowRange = (1:length(noscopedsalivaserver_$serverprofile$ttime))
  
  activStep = 600000 #10mins
  
  #calculate the correct axis scale according to the larger  
  #xaxis is 50-interval activations 
  scopecpas = scopedsalivaserver_$serverprofile$ttime[scopedStatsRowRange]
  
  scopecpas2 = vector("list", length(scopecpas))
  for (i in seq_along(scopecpas)){
    if (is.na(scopecpas[i])) { scopecpas2[i] = scopecpas2[[i - 1]] + activStep }  else { 
      scopecpas2[i] = scopecpas[i] }
  }

  for (i in colsNo){ 
    local({
      currServCol = server.logcolumns[i]
      currServAxisName = server.yaxes[i]
      currXaxisName = graph.serverxaxis[1]
      
      outp[[paste('myServerTChart', i, sep='')]] <- renderChart2({
        
        p <- plotMultiBarChart(noscopedsalivaserver_$serverprofile, scopedsalivaserver_$serverprofile, 
                               scopecpas2, 
                               currServCol, scopedStatsRowRange, currXaxisName, currServAxisName, 'Server')#plot first x values
        
        
        p$set(width=600)
        
        p
      })
      
    })#local
    
  }
  
  noscopecpas = noscopedsalivaclient_$clientprofile$ttime[noscopedStatsRowRange]
  
  noscopecpas2 = vector("list", length(noscopecpas))
  for (i in seq_along(noscopecpas)){
    if (is.na(noscopecpas[i])) { noscopecpas2[i] = noscopecpas2[[i - 1]] + activStep }  else { 
      noscopecpas2[i] = noscopecpas[i] }
  }
  
  clientColsNo = seq_along(client.logcolumns)
  for (i in clientColsNo){ 
    local({
      
      currClientCol = client.logcolumns[i]
      currServAxisName = client.yaxes[i]
      currXaxisName = graph.clientxaxis[1]
      
      outp[[paste('myClientTChart', i, sep='')]] <- renderChart2({
        
        p <- plotMultiBarChart(noscopedsalivaclient_$clientprofile, scopedsalivaclient_$clientprofile, 
                               noscopecpas2, 
                               currClientCol, noscopedStatsRowRange, currXaxisName, currServAxisName, 'Clients')
        
        p$set(width=600)
        
        p
      })
    })#local
    
  }
}


runApp(list(
  ui = bootstrapPage(
    headerPanel("VUB Security Access Case Study - Benchmarks"),
    mainPanel(width = 12,
      div(class = "row", 
          div( class = "span3", style = 'display: inline-block; ',
                showOutput("myServerAChart1", "Highcharts")
          ),
          div(class = "span3", style = 'display: inline-block; ',
              showOutput("myServerAChart2", "Highcharts")
          )
      ),
      div(class = "row", 
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myServerAChart3", "Highcharts")
          ), 
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myServerAChart4", "Highcharts")
          )
      ),
      div(class = "row", 
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myServerAChart5", "Highcharts")
          ), 
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myServerAChart6", "Highcharts")
          )
      ),
      
      div(class = "row", 
          div( class = "span4", style = 'display: inline-block; ',
               showOutput("myClientAChart1", "Highcharts")
          ),
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myClientAChart2", "Highcharts")
          )
      ),
      div(class = "row", 
          div( class = "span4", style = 'display: inline-block; ',
               showOutput("myClientAChart3", "Highcharts")
          ),
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myClientAChart4", "Highcharts")
          )
      ),
      #div(class = "row", 
      #    div( class = "span4", style = 'display: inline-block; ',
      #         showOutput("myClientAChart5", "Highcharts")
      #    ),
      #    div(class = "span4", style = 'display: inline-block; ',
      #        showOutput("myClientAChart6", "Highcharts")
      #    )
      #),
      
      
      div(class = "row", 
          div( class = "span4", style = 'display: inline-block; ',
               showOutput("myServerTChart1", "Highcharts")
          ),
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myServerTChart2", "Highcharts")
          )
      ),
      div(class = "row", 
          div( class = "span4", style = 'display: inline-block; ',
               showOutput("myServerTChart3", "Highcharts")
          ),
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myServerTChart4", "Highcharts")
          )
      ),
      div(class = "row", 
          div( class = "span4", style = 'display: inline-block; ',
               showOutput("myClientTChart1", "Highcharts")
          ),
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myClientTChart2", "Highcharts")
          )
      ),
      div(class = "row", 
          div( class = "span4", style = 'display: inline-block; ',
               showOutput("myClientTChart3", "Highcharts")
          ),
          div(class = "span4", style = 'display: inline-block; ',
              showOutput("myClientTChart4", "Highcharts")
          )
      )
    )
  ),
  server = function(input, output){

    #50 activaiton interval graphs
   plotWithActivationNoSampling(input, output)
    
    #10min interval graphs
    plotWith10MinSampling(input, output)
    
  }
  
))
 