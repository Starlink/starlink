
option add *HighlightThickness		0
option add *Graph.shadow  { navyblue 2 }
option add *Tile			bgTexture
option add *Button.Tile			""
option add *psPreview yes
image create photo bgTexture -file ./images/chalk.gif

option add *Graph.title			"Bitmap Symbols" 
option add *Graph.foreground		white
option add *Graph.borderWidth		2
option add *Graph.relief		raised
option add *Graph.font	 		-*-helvetica-bold-r-*-*-24-*-*-*-*-*-*-* 

option add *Axis.TickFont 	-*-helvetica-bold-r-*-*-12*-*-*-*-*-*-* 
option add *Axis.TitleFont 	-*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*
option add *y2.hide 		no
option add *degrees.stepSize		90 
option add *degrees.Command		formatXLabels 
option add *degrees.Subdivisions	0 
option add *degrees.Title		"Degrees" 
option add *degrees.Limits 	"T=%g"
option add *y.Limits		"%g"
option add *y.Title		"Y" 
option add *y.loose		no
option add *y.Color		purple2
option add *y.rotate		90 
option add *y2.rotate		270
option add *y2.color		magenta3
option add *y2.Title		"Y2" 
option add *temp.Limits 	"T=%g"
option add *temp.title		"Temperature"

option add *Legend.position 	plotarea
option add *Legend.anchor 	ne
option add *Legend.activeRelief raised
option add *Legend.activeBorderWidth 2
option add *Legend.font		-*-helvetica-medium-r-*-*-34-*-*-*-*-*-*-*
option add *Legend.relief	flat
option add *Legend.borderWidth  0
option add *Legend.foreground   orange

option add *Element.ScaleSymbols true
option add *Element*Pixels	2.75m

graph $graph

proc formatXLabels {graph x} {
     return "[expr int($x)]\260"
}

set max -1.0
set step 0.2

set letters { A B C D E F G H I J K L }
set count 0
for { set level 30 } { $level <= 100 } { incr level 10 } {
    set color [format "#dd0d%0.2x" [expr round($level*2.55)]]
    bitmap compose symbol$count [lindex $letters $count] \
    	-font -*-helvetica-medium-r-*-*-34-*-*-*-*-*-*-*
    $graph pen create style$level -color $color -symbol symbol$count \
	-fill "" -pixels 6m
    set min $max
    set max [expr $max + $step]
    lappend styles "style$level $min $max"
    incr count
}


$graph axis create temp -color lightgreen -title Temp  -min -0.5 -max 0.5
$graph axis create degrees
$graph xaxis use degrees

set tcl_precision 15
set pi1_2 [expr 3.14159265358979323846/180.0]

vector create w x sinX cosX radians
x seq -360.0 360.0 10.0
radians expr { x * $pi1_2 }
sinX expr sin(radians)
cosX expr cos(radians)
cosX dup w
vector destroy radians

$graph element create line1 \
    -color black \
    -dashes { 10 4 } \
    -fill orange \
    -fill yellow \
    -label "sin(x)" \
    -linewidth 1 \
    -mapx degrees \
    -pixels 6m \
    -symbol "@bitmaps/hobbes.xbm @bitmaps/hobbes_mask.xbm" \
    -xdata x \
    -ydata sinX

$graph element create line2 \
    -color green4 \
    -fill green \
    -label "cos(x)" \
    -mapx degrees \
    -styles $styles \
    -weights w \
    -xdata x \
    -ydata cosX 


Blt_ZoomStack $graph
Blt_Crosshairs $graph
#Blt_ActiveLegend $graph
Blt_ClosestPoint $graph
Blt_PrintKey $graph

