
option clear

proc FormatXTicks { w value } {

    # Determine the element name from the value

    set index [expr round($value)]
    if { $index != $value } {
	return $value 
    }
    incr index -1

    set name [lindex { A1 B1 A2 B2 C1 D1 C2 A3 E1 } $index]
    return $name
}

source patterns.tcl

image create photo bgTexture -file ./images/chalk.gif

option add *tile			bgTexture

option add *Barchart.font		 -*-helvetica-bold-r-*-*-14-*-*
option add *Barchart.title		"Comparison of Simulators"
option add *Barchart.barMode		stacked

option add *Axis.tickFont		-*-helvetica-medium-r-*-*-12-*-*
option add *Axis.titleFont		-*-helvetica-bold-r-*-*-12-*-*
option add *x.Command			FormatXTicks
option add *x.Title			"Simulator"
option add *y.Title			"Time (hrs)"

option add *activeBar.Foreground	black
option add *activeBar.stipple		pattern1
option add *Element.Background		white
option add *Element.Relief		raised

option add *Grid.dashes			{ 2 4 }
option add *Grid.hide			no
option add *Grid.mapX			""

option add *Legend.Font			"-*-helvetica*-bold-r-*-*-12-*-*"
option add *Legend.activeBorderWidth	1 
option add *Legend.activeRelief		raised 
option add *Legend.anchor		ne 
option add *Legend.borderWidth		0 
option add *Legend.position		right

option add *TextMarker.Font		*Helvetica-Bold-R*14*

barchart $graph

vector X Y0 Y1 Y2 Y3 Y4

X set { 1 2 3 4 5 6 7 8 9 }
Y0 set { 
    0.729111111  0.002250000  0.09108333  0.006416667  0.026509167 
    0.007027778  0.1628611    0.06405278  0.08786667  
}
Y1 set {
    0.003120278	 0.004638889  0.01113889  0.048888889  0.001814722
    0.291388889  0.0503500    0.13876389  0.04513333 
}

Y2 set {
    11.534444444 3.879722222  4.54444444  4.460277778  2.334055556 
    1.262194444  1.8009444    4.12194444  3.24527778  
}

Y3 set {
    1.015750000  0.462888889  0.49394444  0.429166667  1.053694444
    0.466111111  1.4152500    2.17538889  2.55294444 
}

Y4 set {
    0.022018611  0.516333333  0.54772222  0.177638889  0.021703889 
    0.134305556  0.5189278    0.07957222  0.41155556  
}


#
# Element attributes:  
#
#    Label	yData	Color		Stipple Pattern
set attributes { 
    "Load"	Y2	lightblue	pattern1
    "Solve"	Y3	cyan		pattern2 
    "Other"	Y4	lightpink	pattern1
    "Read In"	Y0	lightgoldenrod	pattern1
    "Setup"	Y1	lightyellow	pattern2
}
     
foreach {label yData color stipple} $attributes {
    $graph element create $yData -label $label -bd 1 \
	-ydata $yData -xdata X -fg ${color}1 -bg ${color}3 -stipple $stipple
}

Blt_ZoomStack $graph
Blt_Crosshairs $graph
Blt_ActiveLegend $graph
Blt_ClosestPoint $graph
