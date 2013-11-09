#
proc SetDisplay {} {
  global DisplaySize Width Height

  switch $DisplaySize {
    1 {set Width 400;set Height 300}
    2 {set Width 500;set Height 375}
    3 {set Width 600;set Height 450}
    4 {set Width 700;set Height 525}
    5 {set Width 800;set Height 600}
    6 {set Width 900;set Height 675}
    7 {set Width 1000;set Height 750}
  }
}
#
#
#
proc ChangeDisplay {name element op} {
  global DisplaySize Width Height
  global canv nbid
  global AutoSaveSettings


  ImgExecWaitNoMsg gdevices "close=yes"

  SetDisplay

  pack forget $canv

  $canv configure -width $Width  -height $Height

  $canv delete gwm 
  $canv create gwm 0 0 -width $Width -height $Height -tags gwm \
               -name xwindows3 -background Black -foreground White


  update idletasks

  ImgExecWaitNoMsg gdevices "open=yes dev=x3win"

  pack $canv

  update idletasks

# make new graphic display size available to applications
  nbs put $nbid.xpmax $Width  
  nbs put $nbid.ypmax $Height

  if {$AutoSaveSettings == 1} {
    UpdateSetting DisplaySize $DisplaySize
  }


}
#
