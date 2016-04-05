#line charts

plotLineChart <- function(seriesvalues, seriesname, labels, xtitle, ytitle, title='Serena VUB Access Case'){
  
  p <- Highcharts$new()
  
 p$chart(zoomType = 'x', type = 'line'
        #style = list(
        #  fontFamily = "Dosis, sans-serif")
)

p$title(text = title)

p$series(name = seriesname,
         data = seriesvalues)

p$xAxis(categories = labels,
        title = list( text = toupper(xtitle)),
        tickInterval = 5,
        showLastLabel = TRUE)

p$yAxis( #type = 'logarithmic', 
         title = list( text = toupper(ytitle))
)

#p$tooltip(shared=true)

p$plotOptions(
  area = list(stacking = 'normal',
              lineWidth = 1,
              marker = list (enabled = FALSE, 
                            symbol = 'circle',
                            radius = 1,
                            states = list (hover =  list (enabled = TRUE))
              )),
  line = list(dataLabels = list( 
                  enabled = FALSE,
                  enableMouseTracking = FALSE))
)

#exporting
p$exporting(enabled=T,
            buttons=list(
              contextButton = list(
                align = 'right',
                verticalAlign = 'top'
              )
            ))

# save to standalone HTML page
p$save(destfile = 'newchart.html')

print(p)

p
}

plotBarChart <- function(seriesvalues, seriesnames, xtitle, ytitle, title='Serena VUB Access Case'){
  #multi=bar charts
  p <- Highcharts$new()
  
  p$chart(zoomType = 'x', type = 'bar'
          #style = list(
          #  fontFamily = "Dosis, sans-serif")
  )
  
  p$title(text = title)
  
  for (i in seq_along(seriesvalues)){
    p$series(name = colnames(seriesvalues)[i],#client server
             data = seriesvalues[i])
  }
  
  p$xAxis(categories = seriesnames, #noscope, scope
          title = list( text = NULL),
          tickInterval = 1,
          showLastLabel = TRUE)
  
  p$yAxis( #type = 'logarithmic', 
    title = list( text = toupper(ytitle), align = 'high'),
    min = 0,
    labels = list(overflow = 'justify')
  )
  
  #p$tooltip(shared=true)
  
  p$plotOptions(
    bar = list(dataLabels = list( 
      enabled = TRUE,
      enableMouseTracking = FALSE))
  )
  
  #exporting
  p$exporting(enabled=T,
              buttons=list(
                contextButton = list(
                  align = 'right',
                  verticalAlign = 'top'
                )
              ))
  
  p$legend(layout= 'vertical',
           align= 'right',
           verticalAlign='top',
           x= -40,
           y= 80,
           floating =TRUE,
           borderWidth = 1,
           shadow= TRUE
           )
  # save to standalone HTML page
  p$save(destfile = 'newbar.html')
  
  print(p)
  
}

#####################################
addSeriesToChart <- function(chart, seriesname, seriesdata){
  if (!is.null(chart)){
    chart$series(name = seriesname,
              data = seriesdata)
    print(chart) #show it
  } else {
    print ('Chart does not exist')
  }
}

addChartToPage <- function(chart, seriesname, seriesdata){
  #TODO
}