option add *highlightThickness 0 
if {$PowerUpdateTime} {
  canvas .c -height 26 -bd 2 -width 220 -relief raised -bg gray35
} else {
  canvas .c -height 21 -bd 2 -width 220 -relief raised -bg gray35
}
pack .c
set fg bisque
#set font -adobe-courier-medium-r-normal-*-8-80-75-75-m-50-iso8859-1
set font -adobe-helvetica-medium-r-normal-*-8-80-75-75-p-46-iso8859-1
set f2 -adobe-helvetica-bold-r-normal-*-12-120-75-75-p-70-iso8859-1
set f3 -adobe-helvetica-medium-r-normal-*-10-100-75-75-p-56-iso8859-1
.c create text 30 5 -text mem -anchor e -fill $fg -font $font
.c create text 30 11 -text swap -anchor e -fill $fg -font $font
.c create text 30 17 -text cpu -anchor e -fill $fg -font $font
if {$PowerUpdateTime} {
  .c create text 30 23 -text power -anchor e -fill $fg -font $font
}
.c create line 35 6 60 6 -width 4 -fill red -tags mU
.c create line 60 6 70 6 -width 4 -fill yellow -tags mB
.c create line 70 6 100 6 -width 4 -fill blue -tags mC
.c create line 100 6 135 6 -width 4 -fill green -tags mF
.c create line 35 12 70 12 -width 4 -fill red -tags sU
.c create line 70 12 135 12 -width 4 -fill green -tags sF
.c create line 35 18 55 18 -width 4 -fill red -tags cU
.c create line 55 18 70 18 -width 4 -fill yellow -tags cN
.c create line 70 18 100 18 -width 4 -fill blue -tags cS
.c create line 100 18 135 18 -width 4 -fill green -tags cI
if {$PowerUpdateTime} {
  .c create line 35 24 100 24 -width 4 -fill red -tags pU
  .c create line 100 24 135 24 -width 4 -fill green -tags pA
}
if {$PowerUpdateTime} {
  .c create text 180 16 -text 12:52am -fill $fg -font $f2 -anchor s -tags tT
  .c create text 180 15 -text {Wed Oct 16} -fill $fg -font $f3 -anchor n \
     -tags tD
} else {
  .c create text 180 15 -text 12:52am -fill $fg -font $f2 -anchor s -tags tT
  .c create text 180 12 -text {Wed Oct 16} -fill $fg -font $f3 -anchor n \
     -tags tD
}
bind .c <1> {DoDialog %X %Y}
bind .c <B1-Motion> {catch {grab .m}}
bind .c <ButtonRelease-1> {catch {grab .m}}

proc DoDialog {x y} {
  catch {source [glob ~/.apps]}
  if {![info exists application]} return;
  if {[winfo exists .m]} {destroy .m}
  menu .m -transient 1 -tearoff 0

  foreach n [lsort [array names application]] {
    .m add command -label $n -command "$application($n)"
  }
  .m post $x $y
  focus .m
  tkSaveGrabInfo .m
}

UpdateTime
UpdateMem
UpdateCPU
UpdatePower
