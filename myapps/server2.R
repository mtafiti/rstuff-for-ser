require(rCharts)
require(shiny)

library(jsonlite)

source('serenaCharting.R')
source('projscripts/multiLineChart.R')
source('projscripts/multiBarChart.R')


plotWith10MinSampling = function(inp, outp){
  scopedsalivaserverfile = 'serenalogs/same/10minsampling/valid/autoruns/run2fixed16hrs/vub_access.server.scope.3.3_210.log'
  scopedsalivaclientfile = 'serenalogs/same/10minsampling/valid/autoruns/run2fixed16hrs/vub_access.client.scoped.3.3_210.log' 
  
  noscopedsalivaserverfile = 'serenalogs/same/10minsampling/valid/autoruns/run2fixed16hrs/vub_access.server.NOscope.3.3_210.log'
  noscopedsalivaclientfile = 'serenalogs/same/10minsampling/valid/autoruns/run2fixed16hrs/vub_access.client.NOscope.3.3_210.log'
  
  scopedsalivaserver_ = loadAndCleanSerenaLogFile(scopedsalivaserverfile)
  scopedsalivaclient_ = loadAndCleanSerenaLogFile(scopedsalivaclientfile)
  
  noscopedsalivaserver_ = loadAndCleanSerenaLogFile(noscopedsalivaserverfile)
  #this has 60k sampling, scoped client has 600k, so filter every 10 rows
  noscopedsalivaclient_ = loadAndCleanSerenaLogFile(noscopedsalivaclientfile)
  
  server.logcolumns = c('factsadded', 'joins', 'activations', 'sumatime', 'memoryRSS', 'memoryHeapUsed')
  server.yaxes = c('No. of facts asserted', 'No. of joins', 'No. of activations', 'Activation Time (total)', 'Memory (RSS) (MBs)', 'Memory (Heap used) (MBs)')
  
  client.logcolumns = c('factsadded', 'activation', 'sumatime')
  client.yaxes = c('No. of facts asserted', 'No. of notifications', 'Activation Time (total)')
  
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
    
    #10min interval graphs
    plotWith10MinSampling(input, output)
    
  }
  
))
