#option add *Graph.bufferElements no
#option add *Graph.invertXY 	1
#option add *Graph.PlotPad 	0.25i
option add *Graph.title 	"Sine and Cosine Functions" 
option add *Graph.borderWidth 	1
option add *Graph.relief	sunken
option add *Graph.font 		-*-helvetica-bold-r-*-*-24-*-*-*-*-*-*-* 

option add *Axis.TickFont 	-*-helvetica-bold-r-*-*-12*-*-*-*-*-*-* 
option add *Axis.TitleFont 	-*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*
option add *Axis.limits		"%g"
option add *Axis.hide 		no
option add *x.stepSize		90 
option add *x.Command		formatXLabels 
option add *x.Subdivisions	0 
option add *x.Title		"X" 
option add *y.Title		"Y" 
option add *y.Loose		no
option add *y.Color		purple2
option add *y.rotate		90 
option add *y2.color		magenta3

option add *Legend.Position 	plotarea
option add *Legend.Anchor 	ne
option add *Legend.activeRelief raised
option add *Legend.activeBorderWidth 2
option add *Legend.font		-*-helvetica-medium-r-*-*-24-*-*-*-*-*-*-*

option add *Element.ScaleSymbols yes

option add *Pixels		 1.75m

graph $graph -leftvar changed

proc formatXLabels {graph x} {
     return "[expr int($x)]\260"
}

set tcl_precision 15
set pi1_2 [expr 3.14159265358979323846/180.0]

vector create x sinX cosX -variable ""
x seq -360 360 5
sinX expr { sin(x*$pi1_2) }
cosX expr { cos(x*$pi1_2) }

$graph element create line1 \
    -label "sin(x)" -fill orange -color black -xdata x -ydata sinX  
$graph element create line2 \
    -label "cos(x)" -color yellow4 -fill yellow -xdata x -ydata cosX 

Blt_ZoomStack $graph
Blt_Crosshairs $graph
Blt_ActiveLegend $graph
Blt_ClosestPoint $graph
#Blt_PrintKey $graph

$graph marker create bitmap -name bg -coords "-360 -1 360 1" \
	-bitmap @bitmaps/greenback \
	-bg seagreen2 -fg green4 -under yes

$graph postscript configure -maxpect yes -landscape yes

