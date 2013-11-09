#
proc IsysInit {} {


  global AST_ETC

#  Flag if an image is currently open
  global ImageOpen
  set ImageOpen 0 


#  Initialise the short and long names of databases accessible on the
#  remote server (A C Davenhall, Edinburgh, 20/5/97).
   global remoteCatsShort
   set    remoteCatsShort  ""

   global remoteCatsLong
   set    remoteCatsLong   ""


#  set main background colour
  option add *background LightGrey
  option add *frame.background LightGrey



#  Initialise context-sensitive help
  global ContextHelp
  trace variable ContextHelp w HelpControl
#



# set filenames used for communication
  global InitFile
  set InitFile ~/.Isysrc
  global TipFile
  set TipFile $AST_ETC/Isys.tips
  global AboutFile
  set AboutFile $AST_ETC/Isys.about
  global SymbolFile 
  set SymbolFile $AST_ETC/symbols.list
  global File1D
  set File1D /tmp/[pid]_1D
  global FileARD
  set FileARD /tmp/[pid]_ARD
  global FilePos
  set FilePos /tmp/[pid]_Pos
  global CatListFile
  set CatListFile /tmp/[pid]_CatList

# set initial current directory to be startup directory
  global CurrDir
  set CurrDir " "

  global CreatedXanPgServer
  set CreatedXanPgServer 0

# set control variables
  global Control
  set Control [list {AutoRefresh 1} {Ext1DWnd 0} {ShowMsg 1} {ShowAttrib 1} \
             {ShowShape 1} {ImgFilter "*.sdf"} {DisplaySize 2} \
             {AutoSaveSettings 0} {ColTabDir "./"} {ColTabUseFav 0} {ShowTips 1} \
		   {TipNum 1} {ShowCache 0} {Browser " "} {CatServer "LEDAS"} \
		   {ColourWarn 1} ]
  foreach con $Control {
    set conkey [lindex $con 0]
    global $conkey
    set $conkey -1
  }


# look for init file and extract values
  if {[file exists $InitFile] == 1} {
    set f [open $InitFile r]
    while {[set n [gets $f line]] >=0} {
      set q [string first "=" $line]
      set key [string range $line 0 [expr $q-1]]
      set val [string range $line [expr $q+1] $n]
      foreach con $Control {
        set conkey [lindex $con 0]
        if {$key == $conkey} {
          set $conkey $val
        }
      }
    }
    close $f
  }

  foreach con $Control {
    set conkey [lindex $con 0]
    if {[expr \$$conkey] == -1} {
      set $conkey [lindex $con 1]
    }
  }


#  Set up graphics display window
  SetDisplay


}

#  Procedure for centering pop-up windows
proc Centre_Window {w} {

# Withdraw the window, then update all the geometry information
# so we know how big it wants to be, then center the window in
# parent and de-iconify it.

    wm withdraw $w
    update idletasks
    set parent [winfo parent $w]
    set x [expr [winfo width $parent]/2 - [winfo reqwidth $w]/2 \
        + [winfo x $parent]]
    set y [expr [winfo height $parent]/2 - [winfo reqheight $w]/2 \
        + [winfo y $parent]]
    wm geom $w +$x+$y
    wm deiconify $w
}
