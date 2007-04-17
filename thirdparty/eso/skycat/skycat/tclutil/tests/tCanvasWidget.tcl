source test.tcl

set w [CanvasWidget .c]
pack $w -fill x -expand 1

[$w component canvas] create text 100 100 -text "CanvasWidget" 


