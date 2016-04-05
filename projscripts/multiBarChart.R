
# for events: https://github.com/ramnathv/rCharts/blob/master/inst/libraries/highcharts/examples.R

serenacolors = c('rgba( 0, 154, 253, 0.9 )', #bright blue
                 'rgba( 253, 99,  0,   0.9 )', #bright orange
                 'rgba( 154, 253, 0,   0.9 )', #bright green
                 'rgba( 145, 44,  138, 0.9 )', #mid purple
                 'rgba( 40,  40,  56,  0.9 )', #dark
                 'rgba( 253, 0,   154, 0.9 )', #bright pink
                 'rgba( 45,  47,  238, 0.9 )', #mid blue
                 'rgba( 177, 69,  0,   0.9 )', #dark orange
                 'rgba( 140, 140, 156, 0.9 )', #mid
                 'rgba( 238, 46,  47,  0.9 )', #mid red
                 'rgba( 44,  145, 51,  0.9 )', #mid green
                 'rgba( 103, 16,  192, 0.9 )'  #dark purple
)

seridx = 0

plotMultiBarChart <- function(noscopeprofile, scopeprofile, xcategories, seriesname, dataIndex, 
                              xaxistitle, yaxistitle, charttitle){
  seridx <<- seridx + 1
  
  #line charts
  p <- Highcharts$new()
  
  p$chart(zoomType = 'x', 
          type = 'column',
          #marginLeft= 40, # Keep all charts left aligned
          spacingTop= 20,
          spacingBottom= 20
  )
  
  p$title(text = charttitle)
  
  p$legend(enabled = T)
  
  
  p$xAxis(crosshair = T,
          categories = xcategories,
          title = list( text = toupper(xaxistitle)),
          #tickInterval = 14,
          showLastLabel = T)
  
  p$yAxis( type = 'logarithmic', 
      title = list( text = toupper(yaxistitle))
  )
  
  #p$tooltip(shared=true)
  p$plotOptions(
    column = list(pointPadding = 0.2, 
                  dataLabels = list(enabled= F
                                    #,format = '{point.y: .2f}'
                                    )
                  )
  )

  p$series(name =  'Unscoped rete',
           data = with(noscopeprofile, get(seriesname))[dataIndex],
           color = serenacolors[10]
  )
  
  p$series(name = 'Serena rete',
           data = with(scopeprofile, get(seriesname))[dataIndex],
           color = serenacolors[11]
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
  dst = paste('charts/multibarchart_', gsub(':','.', format(Sys.time(), '%a_%b_%d_%Y_%X')), '.html', sep='')
  
  p$save(destfile = dst)
  
  #add to filename list
  #fnames <- append(fnames, dst)
  
  #print(p)
  
  p
}

#####################################
addSeriesToMultiBar <- function(chart, profile, seriesname, suffix){

  #another color
  seridx = seridx + 1
  
  if (!is.null(chart)){
    seriesdata = with(profile, get(seriesname))
    
    chart$series(name = paste(seriesname, suffix, sep='-'),
                 data = (test = c(1,2,3,4,5,6,7,8,9)),
                 color = serenacolors[seridx %% length(serenacolors)])
    print(chart) #show it
  } else {
    print('KError: Chart does not exist')
  }
  
  chart
}