#sample bfast code from blog post
#http://timelyportfolio.blogspot.com/2012/04/structural-breaks-bull-or-bear.html
#analyze breakpoints with the R package bfast
#please read the paper 
#Verbesselt J, Hyndman R, Newnham G, Culvenor D (2010)
#Detecting Trend and Seasonal Changes in Satellite Image Time Series.
#Remote Sensing of Environment, 114(1), 106–115.
#http://dx.doi.org/10.1016/j.rse.2009.08.014

require(bfast)
require(quantmod)

GSPC <- getSymbols("^GSPC",from="1950-01-01", auto.assign=F)
#convert to log price
GSPC.monthly <- log(to.monthly(GSPC)[,4])
#get monthly returns for the close price
#not necessary, leave in price form
#GSPC.return <- monthlyReturn(GSPC[,4])
#need ts representation so do some brute force conversion
GSPC.ts <- ts(as.vector(GSPC.monthly["1951-01::"]),start=c(1951,1),frequency=12)
#look at the stl Seasonal-Trend decomposition procedure already in R
GSPC.stl <- stl(GSPC.ts,s.window="periodic")
plot(GSPC.stl,main="STL Decomposition of S&P 500")


#get the results from bfast
#adjusting h lower will result in more breakpoints
GSPC.bfast <- bfast(GSPC.ts,h=0.2,max.iter=1,season="none")

plot(GSPC.bfast,type="components",ylim=c(3,max(GSPC.monthly)+1),main="S&P 500 with bfast Breakpoints and Components")
plot(GSPC.bfast,type="trend",ylim=c(3,max(GSPC.monthly)+1),main="S&P 500 with bfast Trend Breakpoints")


#see everything with type="all" but in bfast calculation set seasonal to "none"
#play away with this
#plot(GSPC.bfast,type="all")

#do some additional plotting
#[[1]] is used since for speed I only did one iteration
#could plot each iteration if I did multiple
plot(GSPC.bfast$Yt/GSPC.bfast$output[[1]]$Tt-1,
     main="bfast Remainder as % of S&P 500 Price",
     xlab=NA, ylab="remainder (% of price)",bty="l")
#add vertical line for the breakpoints
abline(v=breakdates(GSPC.bfast$output[[1]]$bp.Vt),col="gray70")
#add horizontal line at 0
abline(h=0,col="black",lty=2)
text(x=breakdates(GSPC.bfast$output[[1]]$bp.Vt),y=par("usr")[3]+.01,
     labels=breakdates(GSPC.bfast$output[[1]]$bp.Vt,format.times=TRUE),
     srt=90,pos=4,cex=0.75)



require(rCharts)
options(viewer=NULL)

bfast.df <- data.frame(
  date = format(index(as.xts(GSPC.bfast$output[[1]]$Tt)),"%Y-%m-%d")#as.Date(index(as.xts(GSPC.bfast$output[[1]]$Tt)))
  ,bfast = as.numeric(GSPC.bfast$output[[1]]$Tt)
)
#just use dimple to start
dBfast <- dPlot(
  price ~ date,
  data = data.frame(
    date = format(as.Date(index(GSPC.monthly)),"%Y-%m-%d"),#as.numeric(as.Date(index(GSPC.monthly)))
    price = as.numeric(GSPC.monthly)
  ),
  type = "line"
)
#set up a time based x axis
dBfast$xAxis(
  #type = "addCategoryAxis",
  type = "addTimeAxis"
  ,inputFormat = "%Y-%m-%d"
  ,outputFormat = "%Y"
)
dBfast$yAxis(
  overrideMin = 0,
  overrideMax = 8
)
dBfast
#add the bfast layer
#with new dataset for layer
dBfast$templates$script = "../rCharts/inst/libraries/dimple/layouts/chart.html"
dBfast$layer(
  x = "date",
  y = "bfast",
  data = bfast.df,
  type = "line"
)
dBfast

# various cleanup experiments
# what should be in dimple/rCharts by default
# or made easier
dBfast$setTemplate(
  afterScript = '
    <script>
      myChart.series[1].shapes
        .transition().duration(10).delay(1000)
          .style("stroke","#AD5277")
          .style("stroke-dasharray",[10,5])
      //delete some of the ticks
      myChart.svg.select(".axis").selectAll(".tick")[0].forEach(function(d,i){
          if (!(+d3.time.format("%Y")(new Date(+d3.select(d).datum())) % 10 == 0)) {
            d.remove()
          }
        })
      myChart.svg.select(".axis").selectAll(".tick text")
        .attr("transform","none")
        .style("text-anchor","middle");
    </script>
  '
)
dBfast

#now try fancy code from @biovisualize svg -> canvas -> png
dBfast$setTemplate(
  afterScript = '
  <script>
  var svgString = new XMLSerializer().serializeToString(document.querySelector("svg"));
d3.select("body").append("div").attr("id","png-container")
d3.select("body").append("canvas")
  .attr("id","canvas")
  .attr("height",400)
  .attr("width",800);
var canvas = document.getElementById("canvas");
var ctx = canvas.getContext("2d");
var DOMURL = self.URL || self.webkitURL || self;
var img = new Image();
var svg = new Blob([svgString], {type: "image/svg+xml;charset=utf-8"});
var url = DOMURL.createObjectURL(svg);
img.onload = function() {
  ctx.drawImage(img, 0, 0);
  var png = canvas.toDataURL("image/png");
  document.querySelector("#png-container").innerHTML = \'<img src="\'+png+\'"/>\';
  DOMURL.revokeObjectURL(png);
};
img.src = url;

</script>
'
)
dBfast




#start working on an interactive view with a changing h for bfast
#this gives us each of the breakpoints and the jump to the successive breakpoint
#to get starting point just the point in time series
GSPC.bfast$Mags
#this might be an easier way to get
GSPC.bfast$output[[1]]$Tt[
  c(
    GSPC.bfast$output[[1]]$bp.Vt$breakpoints,
    GSPC.bfast$output[[1]]$bp.Vt$breakpoints+1)
]
as.xts(GSPC.bfast$output[[1]]$Tt)[
  c(
    GSPC.bfast$output[[1]]$bp.Vt$breakpoints,
    GSPC.bfast$output[[1]]$bp.Vt$breakpoints+1)
  ]
#also need to get begin and end points
head(as.xts(GSPC.bfast$output[[1]]$Tt),1)
tail(as.xts(GSPC.bfast$output[[1]]$Tt),1)
#so rbind the begin, breaks, and end
breaks <- rbind(
  head(as.xts(GSPC.bfast$output[[1]]$Tt),1)
  ,as.xts(GSPC.bfast$output[[1]]$Tt)[
    c(
      GSPC.bfast$output[[1]]$bp.Vt$breakpoints,
      GSPC.bfast$output[[1]]$bp.Vt$breakpoints+1)
    ]
  ,tail(as.xts(GSPC.bfast$output[[1]]$Tt),1)
)


#so let's try to make a function out of it to get json for linebreak
#convert bfast object to a list ready for plotting
#with rCharts and linebreak template
#should be
#key:{
#    line: timeseries data (x and y) in array of objects [{x:, y:}]
#    breaks: start and end points (x and y) in array of objects [{x:, y:}]
#}
bfast_list <- function ( bf, bfName ){
  breaks = rbind(
    head(as.xts(bf$output[[1]]$Tt),1)
    ,as.xts(bf$output[[1]]$Tt)[
      c(
        bf$output[[1]]$bp.Vt$breakpoints,
        bf$output[[1]]$bp.Vt$breakpoints+1)
      ]
    ,tail(as.xts(bf$output[[1]]$Tt),1)
  )
  breaksList <- apply(
    matrix(1:nrow(breaks),ncol=nrow(breaks)/2,byrow=F),
    MARGIN=2,
    function(x){
      return(data.frame(
        date = as.numeric(as.Date(index(breaks[x]))),
        x = breaks[x]
      ))
    }
  )

    
  dataSeries  <- list(
    list(
      line = data.frame(
        date = as.numeric(as.Date(index(as.xts(bf$Yt)))),
        x = as.xts(bf$Yt)
      ),
      breaks = unname(breaksList)
    )
  )
  names(dataSeries) <- bfName
  return ( dataSeries )
}

rCharts_linebreak <- setRefClass(
  "rCharts_linebreak",
  contains = "rCharts",
  methods = list(
    initialize = function(){
      callSuper(); 
    },
    getPayload = function(chartId){
      data = jsonlite::toJSON( params$data )
      opts = toJSON2(params[!(names(params) %in% c('data'))])
      list(chartParams = opts, data = data)
    }
  )
)

bP <- rCharts_linebreak$new()
bP$set(
  x = "date",
  y = "x",
  data = bfast_list(GSPC.bfast, "SP500")
)
bP$setLib( "." )
bP$set(
  bodyattrs = 'ng-app="myApp" ng-controller="MainCtrl"'
)
bP$setTemplate(
  chartDiv = 
    '
  <form class="well">
  <label><b>Select  series :</b></label>
  <select class="form-control" ng-model="selected" 
  ng-options="key for (key,value) in data">   
  </select><br>
  </form>
  <div id="graphic">
  </div>
  '
  ,afterScript = sprintf(
    '
  <script>
  var app = angular.module("myApp", []);
  app.controller("MainCtrl", function($scope, $window){
  $scope.data = data;
  
  $scope.drawChart = function(){
  drawChart( $scope.selected )
  }
  
  $scope.selected = $scope.data["%s"]
  
  if( $scope.selected ){
  $scope.drawChart()
  }
  
  $scope.updateChart = function(){
  updateChart( $scope.selected )
  }
  
  $scope.$watch("selected", function(){
  if( $scope.selected ){
  $scope.updateChart()
  }
  })
  });
  </script>
  ',
  names(bP$params$data)[1])
)
#manually set y in original version
#use template with y scaled by data
bP$templates$script = "./layouts/chart_flexy.html"
bP





#so now we could try lots of h in bfast like this
#and then interactively explore like this
bfastList <- sapply(
  seq(from=0.05,to=0.2,by=0.05),
  function(h){
    bfast_list(
      bf = bfast(GSPC.ts,h=h,max.iter=1,season="none"),#,hpc="foreach"), #hpc "none" default
      bfName = gsub(x=paste0("h",h),pattern="\\.",replacement="")
    )#maybe but don't think so [[1]]
  }
)
#bfastList <- lapply(1:length(bfastList),function(x){return(bfastList[[x]][1])})
#names(bfastList) <- paste0("h", seq(from=0.05,to=0.2,by=0.05)*100)

#use previous graph and just change data to our list
bP$params$data = bfastList
bP$setTemplate(
  afterScript = gsub(
    x= bP$templates$afterScript,
    pattern = "SP500",
    replacement = names(bfastList[1])
  )
)
bP
