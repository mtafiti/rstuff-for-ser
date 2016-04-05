
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

plotMultiLineChart <- function(profile, xcategories, seriesname, title='Serena VUB Acc Case'){
  seridx <<- seridx + 1
  
    #line charts
    p <- Highcharts$new()
    
    p$chart(zoomType = 'x', 
          type = 'area',
          #marginLeft= 40, # Keep all charts left aligned
          spacingTop= 20,
          spacingBottom= 20
  )
  
  p$title(text = title,
          align= 'left',
          margin= 0,
          x= 30)
  
  p$legend(enabled = T)

  
  p$xAxis(crosshair = TRUE,
          categories = xcategories,
          title = list( text = toupper(paste('No. of ', 'activations', sep=''))),
          tickInterval = 14,
          showLastLabel = TRUE)
  
  p$yAxis( #type = 'logarithmic', 
    title = list( text = toupper(paste('no. of ', seriesname, sep='')))
  )
  
  #p$tooltip(shared=true)
  
  p$plotOptions(
    area = list(stacking = 'normal',
                pointStart = 0
                )
  )
  
  p$series(name = seriesname,
             data = with(profile, get(seriesname)),
             color = serenacolors[seridx %% length(serenacolors)],
            type = 'area'
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
  dst = paste('charts/multilinechart_', gsub(':','.', format(Sys.time(), '%a_%b_%d_%Y_%X')), '.html', sep='')
  
  p$save(destfile = dst)

  #add to filename list
  #fnames <- append(fnames, dst)
  
  print(p)
  
  p
}

#####################################
addSeriesToMultiChart <- function(chart, profile, seriesname, suffix){
  #suffix - in case previous series has same name
  seridx = seridx + 1
  if (!is.null(chart)){
    seriesdata = with(profile, get(seriesname))
    
    chart$series(name = paste(seriesname, suffix, sep='-'),
                 data = with(profile, get(seriesname)),
                 color = serenacolors[seridx %% length(serenacolors)])
    print(chart) #show it
  } else {
    print('KError: Chart does not exist')
  }
  
  chart
}