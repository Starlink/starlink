#!/bin/csh -f
#
# Scuba wrapper script around linplot, mlinplot, and change_quality
# facilitating easy plotting and blanking of Scuba bolometers.
#

#
# Either source the setup files or set the paths directly
#
#source /star/bin/surf/surf.csh  >& /dev/null
#source /star/bin/kappa/kappa.csh >& /dev/null
#set kap = ""
#set sur = ""
#set kap = /star/bin/kappa/
#set sur = /star/bin/surf/

# Check that a STARLINK login has been completed
# Can do this simply by checking $SURF_DIR exists
# This will stop us hardwiring path to KAPPA and surf

if ($?SURF_DIR) then
   set sur = ${SURF_DIR}
   set kap = ${KAPPA_DIR}
else
  echo 'Error: Starlink system has not been initialised.'
  exit
endif

# Check for the new KAPPA
if (-e $KAPPA_DIR/style.def || -e $KAPPA_DIR/kappa_style.def) then
  set newkappa = 1
  # Store the original style before we mangle it
  set old_style = `${kap}/parget style linplot`
else
  set newkappa = 0
endif

# Check for even newer KAPPA 0.14
if (-e $KAPPA_DIR/kappa_style.def) then
  set newerkappa = 1
else
  set newerkappa = 0
endif

set prog = `echo $0 | awk -F\/ '{print $NF}' | tr '[A-Z]' '[a-z]'`
set mode = `echo $prog | cut -c1`

set lbol = ""
set sdf = ""
set ddf = ""
set mn = ""
set mx = ""
set phsec = ""
@ nbol = 0
@ nl = 13

# ----------------------------------------------------------------------
# Parse the arguments
#
while  ($#argv > 0)

  if ( "$1" =~ "-"[hv]* ) goto help
  if ("$1" == "-m") then
    shift
    if ($#argv == 0) goto help
    if ( "$1" =~ "-"* ) goto help
    set mode = $1
    set mode = `echo $mode | cut -c1 | tr '[A-Z]' '[a-z]'`
    shift
  else if ("$1" == "-f") then
    shift
    if ($#argv == 0) goto help
    if ( "$1" =~ "-"* ) goto help
    set sdf = $1
    set sdf = `echo $sdf | sed s/".sdf"/""/`
    shift
  else if ("$1" == "-d") then
    shift
    if ($#argv == 0) goto help
    if ( "$1" =~ "-"* ) goto help
    set ddf = $1
    set ddf = `echo $ddf | sed s/".sdf"/""/`
    shift
  else if ("$1" == "-s") then
    shift
    if ($#argv == 0) goto help
    if !( "$1" =~ "-"* ) goto help
    set mn = $1
    shift
    if ($#argv == 0) goto help
    if ( "$1" =~ "-"* ) goto help
    set mx = $1
    shift
  else if ("$1" == "-l") then
    shift
    if ($#argv == 0) goto help
    if ( "$1" =~ "-"* ) goto help
    @ nl = $1
    shift
  else
    set lbol = "$lbol $1"
    @ nbol ++
    shift
  endif

end

# ----------------------------------------------------------------------
# Check mode
#
while ( "$mode" != "d" && "$mode" != "D" && "$mode" != "P" && "$mode" != "R" && "$mode" != "p" && "$mode" != "r")
  echo " "
  echo "INVALID MODE: '$mode'. Allowed modes p(ltbol), d(spbol), r(linplot)."
  echo -n "Mode (p/d/r) > "
  set mode = "$<"
  set mode = `echo $mode | cut -c1 | tr '[A-Z]' '[a-z]'`
end

if ( "$mode" == "d" ) then
  @ nl = 1
  set ddf = ""
else if ( "$mode" == "p" ) then
  @ nl = 1
endif

#
# Get interactive input
#
if ( "$sdf" == "" ) then
  echo -n "Input file? > "
  set answer = "$<"
  set sdf = `echo $answer | sed s/".sdf"/""/`
endif

if ( "$mn" == "" || "$mx" == "" ) then
    if ( "$mode" == "r" ) then
      set mn = -0.01
      set mx =  0.2
    else
      set mn = -0.01
      set mx =  0.01
    endif
#    echo -n "Y-axis limits? ${mn} ${mx}> "
#    set answer = "$<"
#    if ( "$answer" != "" ) then
#       set limits = `echo "$answer" | sed s/","/" "/g | sed s/"  "/" "/g`
#       set mn = `echo $limits | awk -F" " '{print $1}'`
#       set mx = `echo $limits | awk -F" " '{print $NF}'`
#    endif
endif
set ymn = $mn
set ymx = $mx

if ( "$lbol" == "" )  then
    set lbol = ""
    echo "Note: additional bolometers can be set via menu below"
    echo -n "Which bolometer(s) [all]? ${lbol} > "
    set answer = "$<"
    if ( "$answer" != "" ) then
       set lbol = `echo " $answer" | sed s/","/" "/g | sed s/"  "/" "/g | sed s/"b"/""/g`
       set nbol = `echo "$answer" | awk -F" " '{print $NF}'`
    else
       set lbol = "all"
    endif
endif

# ----------------------------------------------------------------------
# Expand the bolometer lists, assume that if this is the shortwave
# array all implies alls = 91 bolomters.
#
if ( `fgrep -c sho ${sdf}.sdf` != 0 && "$lbol" == "all" ) then
  set lbol = "alls"
endif

set lbol = `echo $lbol | sed s/","/" "/g | sed s/"  "/" "/g | sed s/"b"/""/g`
if ( "$lbol" == "all" || "$lbol" == " all"  ) then
  set lbol = " 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20"
  set lbol = "$lbol 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37"
  @ nbol = 37;
else if ( "$lbol" == "alls" || "$lbol" == " alls"  ) then
  set lbol = " 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20"
  set lbol = "$lbol 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40"
  set lbol = "$lbol 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60"
  set lbol = "$lbol 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80"
  set lbol = "$lbol 81 82 83 84 85 86 87 88 89 90 91"
  @ nbol = 91;
endif

#
# Should have all essential parameters now
#
if ( "$lbol" == "" || "$sdf" == "" || "$mn" == "" || "$mx" == "" ) goto help

#
# Check the input files
#
if !(-e "${sdf}.sdf") then
  echo "Error: ${sdf}.sdf not found"
  exit
endif

# Now if we are using the new KAPPA we need to set the WCS of the
# input frame to PIXEL
if ($newkappa == 1) then
  $kap/wcsframe ${sdf} pixel
  if ( "$ddf" != "" ) then
    $kap/wcsframe ${ddf} pixel
  endif
  #$kap/wcsattrib ${sdf} set 'label(2)' 'sample'
  #$kap/wcsattrib ${sdf} set 'unit(2)' "' '"
endif

# ----------------------------------------------------------------------
# Photometry?
#
if (`${kap}/fitslist ${sdf} | fgrep MODE | fgrep -c PHOTOM` == 1) then
  set phsec = "(,,2)"
endif

if ( "$ddf" != "" ) then
  if !(-e "${sdf}.sdf") then
    echo "Error: ${sdf}.sdf not found"
    exit
  endif
endif


#
# Calculate the number of sets in mlinplot for mode = r. For the
# other modes this simply defaults to the number of bolometers.
#
set lbol = `echo $lbol | cut -c1-`
set nsets = `echo "${nbol}/${nl}+1" | bc`
set iset = 0

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# Enter main loop over bolometers
#

@ ibol = 0
while (${iset} < ${nsets})

#
# Now do some complicated stuff: assemble the bolometers in sets
# This really is only relevant for mode=r since mlinplot will plot a
# whole set at the time. For the other modes the sets consist of a
# single bolometer
#
  @ ii = 0
  set bolset = ""
  if ( "$mode" == "r" ) then
    while ($ii < ${nl} && $ibol < $nbol)
      set bolset = "${bolset},"`echo "$lbol" | cut -d' ' -f1`
      set lbol = `echo "$lbol" | cut -d' ' -f2-`
      @ ii ++
      @ ibol ++
    end
    set bolset = `echo "$bolset" | cut -c2-`
  else
    if ($ibol < $nbol) then
      set bolset = `echo "$lbol" | cut -d' ' -f1`
      set lbol = `echo "$lbol" | cut -d' ' -f2-`
      @ ii ++
      @ ibol ++
    endif
  endif
  set bol = "${bolset}"
  set bolset = "'${bolset}'"

  if ( $ibol >= $nbol ) @ iset = $nsets
  @ iset ++

  set menu = 1

  RESET_XLIM:
  set plim = ""

  # ----------------------------------------------------------------------
  REDRAW:

  if ( "$mode" == "r" ) then
    if ( $newerkappa == 0 ) then
	${kap}/mlinplot "${sdf}${phsec}" device=xwindows cosys=world \
		    absaxs=2 ylimit=\[${ymn},${ymx}\] "lnindx=${bolset}"
	if ( "$ddf" != "" ) then
	${kap}/mlinplot "${ddf}${phsec}" device=x2windows cosys=world \
			absaxs=2 ylimit=\[${ymn},${ymx}\] "lnindx=${bolset}"
	endif
    else
       ${kap}/mlinplot "${sdf}${phsec}" device=xwindows  \
		    absaxs=2 ybot=${ymn} ytop=${ymx} "lnindx=${bolset}"
	if ( "$ddf" != "" ) then
	${kap}/mlinplot "${ddf}${phsec}" device=x2windows  \
			absaxs=2 ybot{ymn} ytop=${ymx} "lnindx=${bolset}"
	endif

    endif
  else
    if ($newkappa == 0) then
        ${kap}/linplot mode=line lincol=red device=xwindows \
             $sdf'('${bol}','${plim}')' axlim 'ordlim=['$ymn','$ymx']' \
            'abslim=\!' cosys=world 'pltitl="'${sdf}: bolometer ${bol}'"' \
            >& /dev/null
      if ( "$ddf" != "" ) then
        ${kap}/linplot mode=line noclear lincol=green device=xwindows \
             $ddf'('${bol}',)' axlim 'ordlim=['$mn','$mx']' 'abslim=\!' \
             'pltitl="'${sdf}: bolometer ${bol}'"' cosys=world >& /dev/null
      endif
    else
        ${kap}/linplot mode=line device=xwindows $sdf'('${bol}','${plim}')' \
             ytop=$ymx ybot=$ymn \
             style="'colour(Lines)=red,title="${sdf}: bolometer ${bol}" '" \
             >& /dev/null

      if ( "$ddf" != "" ) then
        ${kap}/linplot mode=line device=xwindows $ddf'('${bol}',)' \
             noclear ytop=$mx ybot=$mn \
             style="'colour(Lines)=green,title="${sdf}: bolometer ${bol}"'" \
             >& /dev/null
      endif


    endif
  endif

  # ----------------------------------------------------------------------
  MENU:
  if ( $menu != 0 ) then
    echo    " "
    echo    " [M,H]                 (Re)display menu"
    echo    " [Q]                   Quit"
    echo    " [N]                   Next bolometer(s)"
    if ( "$mode" != "r" ) then
      echo    " [B#]                  Switch to bol #"
      echo    " [X min max], [X cen]  X-axis from min:max or cen+/-10"
      echo    "                       Just 'x' activates the cursor."
      echo    " [R]                   Reset X-axis"
    endif
    echo    " [Y min max], [Y lim]  Y-axis from min:max or -lim:+lim"
    echo    " [U]                   Reset Y-axis"
    if ( "$mode" == "d" ) then
      echo    " [#], [#:#], [#-#]     Despike point or range of points"
      echo    "                       Just 'p' activates the cursor."
    endif
    echo    " "
    echo "Plotted bolometer(s): ${bolset}."
  endif

  set menu = 0

  OPTION:
  echo -n "Option > "

  set answer = "$<"
  set answer = `echo "${answer}" | sed s/"\["/""/g | sed s/"\]"/""/g`

  # ----------------------------------------------------------------------
  # Options: Quit, Next, and Menu
  #
  if ( "$answer" =~ [Qq]* ) goto END_ALL

  if ( "$answer" =~ [Nn]* ) goto NEXT_BOL

  if ( "$answer" =~ [MmHh]* ) then
    set menu = 1
    goto MENU
  endif

  # ----------------------------------------------------------------------
  # Option: switch to alternative Bolometer
  #
  if ( "$mode" != "r" && "$answer" =~ [Bb]* ) then
    set bol = `echo "$answer" | cut -c2- | sed s/" "/""/g`
    echo "Switching to bolometer: $bol"
    goto RESET_XLIM
  endif

  # ----------------------------------------------------------------------
  # Option: scale X-axis
  #
  if ( "$mode" != "r" && "$answer" =~ [Xx]* ) then
    set limits = `echo "$answer" | sed s/"x"/"x "/ | sed s/"X"/"x "/`
    set limits = `echo "$limits" | sed s/","/" "/g | sed s/"  "/" "/g`
    set pmn = `echo $limits | awk -F" " '{print $2}'`
    set pmx = `echo $limits | awk -F" " '{print $NF}'`
    if ( "$pmn" == "" || "$pmx" == "x" ) then
       echo "Cursor: Left button: New plot center; Right button: Accept"
       echo "Hit return to activate cursor in xwindow"

       if ($newkappa == 0) then
         ${kap}/cursor >& /dev/null
         set pmn = `${kap}/parget xc cursor`
       else
         ${kap}/cursor >& /dev/null
         set pmn = `${kap}/parget lastpos cursor | awk '{print $1}'`
       endif
       set pmx = `echo "$pmn + 0.5" | bc | cut -d"." -f1`
       set pmn = $pmx
    endif
    if ( "${pmn}" == "${pmx}" ) then
      set pmx = `expr $pmn + 10`
      set pmn = `expr $pmn - 10`
    endif
    set plim = "${pmn}:${pmx}"
    echo "New X-axis limits: ${plim}"
    goto REDRAW
  endif

  # ----------------------------------------------------------------------
  # Option: Reset X-axis scale
  #
  if ( "$mode" != "r" && "$answer" =~ [Rr]* ) goto RESET_XLIM

  # ----------------------------------------------------------------------
  # Option: scale Y-axis
  #
  if ( "$answer" =~ [Yy]* ) then
    set limits = `echo "$answer" | sed s/"y"/"y "/ | sed s/"Y"/"y "/`
    set limits = `echo "$limits" | sed s/","/" "/g | sed s/"  "/" "/g`
    set ymn = `echo $limits | awk -F" " '{print $2}'`
    set ymx = `echo $limits | awk -F" " '{print $NF}'`
    if ( "$ymn" == "" || "$ymx" == "x" ) then
       echo "Invalid reply"
    else
      if ( "${ymn}" == "${ymx}" ) then
         set ymn = "-${ymx}"
      endif
      echo "New Y-axis limits: ${ymn}:${ymx}"
      goto REDRAW
    endif
  endif

  # ----------------------------------------------------------------------
  # Option: Reset Y-axis scale
  #
  if ( "$answer" =~ [Uu]* ) then
    set ymn = $mn
    set ymx = $mx
    goto REDRAW
  endif

  # ----------------------------------------------------------------------
  # Option: Despike
  #
  if ( $mode == "d" && "$answer" =~ [Pp0-9]* ) then
    set point = `echo "p${answer}" | sed s/"  "/" "/g | sed s/"pp"/"p"/g | sed s/"pP"/"p"/g`
    set point = `echo "$point" | sed s/"-"/":"/g | sed s/" "/":"/g `
    if ( "$point" == "p" || "$point" == "p " ) then
       echo "Cursor: Left button: Bad channel; Right button: Accept"
       echo "Hit return to activate cursor in xwindow"

       if ($newkappa == 0) then
         ${kap}/cursor >& /dev/null
         set point = `${kap}/parget xc cursor`
       else
         ${kap}/cursor frame=pixel >& /dev/null
         set point =  `${kap}/parget lastpos cursor | awk '{print $1}'`
       endif
       set point = `echo "$point + 0.5" | bc | cut -d"." -f1`
       set point = "p${point}"
    endif
    echo " Flagging quality $point BAD"
    ${sur}/change_quality "'${sdf}{b${bol};${point}}'" yes  > /dev/null

    if ($newkappa == 0) then
      ${kap}/linplot mode=line noclear lincol=green device=xwindows \
            $sdf'('${bol}','${plim}')' axlim 'ordlim=['$ymn','$ymx']' \
            'abslim=\!' cosys=world 'pltitl="'${sdf}: bolometer ${bol}'"' \
            >& /dev/null
    else
      ${kap}/linplot mode=line noclear device=xwindows \
            $sdf'('${bol}','${plim}')' ytop=$ymx ybot=$ymn \
	    style="'Colour(lines)=green,title="${sdf}: bolometer ${bol}" '" \
            >& /dev/null
    endif

    echo -n " Accept [y]/n > "
    set accept = "$<"
    if ( $accept =~ [Nn]* ) then
       echo " Resetting quality ${point} GOOD"
       ${sur}/change_quality "'${sdf}{b${bol};${point}}'" no  > /dev/null
    else
       echo " Accepted."
       set plim = "";
    endif
    goto REDRAW
  else
     echo "Error: '$answer' not a valid option. Try again."
     goto OPTION
  endif

  NEXT_BOL:

end
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

END_ALL:

# Reset the LINPLOT style
if ($newkappa == 1) then
  ${kap}/linplot device=! ndf=! style=$old_style > /dev/null
endif

echo " "
exit
help:
more << _EOT_

SCUPLOT (V1.1) -- Scuba utility to plot and/or despike bolometers

Use: scuplot  [-m mode ] [-f sdf_file] [-d sdf_file2] [-s min max] [-l #]
              [bol [bol [bol] ... ]]

     pltbol   [-f sdf_file] [-d sdf_file2] [-s min max] [bol [bol [bol] ... ]]

     dspbol   [-f sdf_file] [-s min max] [bol [bol [bol] ... ]]

     rlinplot [-f sdf_file] [-d sdf_file2] [-s min max] [-l #]
              [bol [bol [bol] ... ]]


OPTIONS:

     -h      help

     -m      mode:
                p: plot bolometers one by one, optionally overlayed with
                   data from the second input file.
                d: interactively despike the data for the bolometers in
                   specified file.
                r: same as 'p' except that a whole set of bolometers is
                   plotted in a window.
             If the program is invoked via a link (e.g. pltbol, dspbol, or
             rlinplot) the mode defaults to first character of the name.

     -f      name of SDF file (.sdf may be included in the name).

     -d      name of a second file: e.g. the despiked version of the SDF
             file. The same bolometers will be plotted in a second window or
             overlayed for comparison.

     -s      Y-axis scales for plot (can be changed via menu).

     -l      number of bolometers per window

     bol     list of bolometers to plot. Type 'all' for 1..37 and 'alls'
             for 1..91. Can be added via menu if mode = 'r'.


DETAILS:
   Scuplot is a wrapper script around a number of KAPPA utilities. Since it
   understands the Scuba NDF file format, it hides most of the complicated
   syntax from the user.  Mode = 'p' and 'r' are wrappers around plotting
   utilities and facilitate the inspection of the data of each bolometer.
   The utility allows change to the plot scales via the menu but will keep
   the scales the same for all bolometers which makes is easy to compare
   bolometers. Mode = 'd' allows for interactive despiking. Please read
   the note below the description of the menu on the use of the mouse.

   Mode = 'p' or pltbol (or any p* link to scuplot) is a wrapper around the
   KAPPA utility linplot. It allows plots  of a whole series of bolometers
   one by one, optionally overlaying them with the same bolometer from a
   second file. Obvious overlays are despiked on non-despiked data or
   data from different exposures to check the noise.

   Mode = 'r' or rlinplot (or any r* link to scuplot) is a wrapper around the
   Kappa Utility mlinplot. It provides  plots of sets of bolometers in a single
   window with optionally data from a second file in a second window.
   Obvious files are despiked and non-despiked data or data from different
   exposures to check the noise.

   Mode = 'd' or dspbol (or any d* link to scuplot) can be used to
   interactively despike bolometers. While it is not as fast as a completely
   integrated routine would be, it makes interactive despiking much
   easier by hiding the cycle between linplot and change_quality for the
   user. The most common use is to zoom in on the region with the spike
   via the 'X' menu option (either typing the input or using the cursor)
   and subsequently to flag the offending point (just type the coordinate
   of the point, a range, or use the cursor; IN GENERAL THE COORDINATE IS
   TO THE RIGHT OF THE PLOTTED POINT). The routine will overlay the
   despiked data, prompt the user to accept the new set and de-zoom to
   the original scale. To reset a previously flagged point, flag the point
   again but do NOT accept it: the point will be set to GOOD again.
   Please read the note below the derscription of the menu on the use of
   the mouse.

   For each mode the menu items are a subset of:

      [Q]                   Quit
      [N]                   Next bolometer
      [B#]                  Switch to bol #
      [X min max], [X cen]  X-axis from min:max or cen+/-10
                            Just 'x' activates the cursor.
      [R]                   Reset X-axis
      [Y min max], [Y lim]  Y-axis from min:max or -lim:+lim
      [U]                   Reset Y-axis
      [#], [#:#], [#-#]     Despike point or range of points;
                            Just 'p' activates the cursor.

      Option >

   Note that a X center defined with the cursor or [X cen] defaults to
   a 20 points window around cen, the position of the spike. Using
   the CURSOR, the Left Mouse button always defines the point, the Right
   Mouse button exits the cursor task while accepting the last point
   clicked.


EXAMPLES:
   scuplot  -m p -f s14_lon_ext 12 13 18 20 25 26 19
   pltbol   -f s14_lon_ext -d s14_lon_dsp -s -0.05 0.1 all
   dspbol   -f s14_lon_ext -s -0.05 0.1 all
   rlinplot -f s14_lon_ext -d s14_lon_dsp -s -0.01 0.2 -l 9 all


IMPORTANT NOTE:
   IF THE OVERLAY SOMEHOW COMES UP SCRAMBLED, delete the agi_xxx files
   in your home directory and if that does not work also files like
   linplot.sdf in the /home/you/adam subdirectory.

BUGS:
   Modes 'p' and 'd' hang on bolometers which have been flagged bad
   completely!

_EOT_

exit

# The starlink prologue. Needs to use * instead of #

*+
*  Name:
*    SCUPLOT
*
*  Purpose:
*    Interactive display and despiking
*
*  Type of Module:
*    C-shell script
*
*  Usage:
*    scuplot [-m mode ] [-f sdf_file] [-d sdf_file2] [-s min max] [-l #]
*              [bol [bol [bol] ... ]]
*
*  Description:
*    Scuplot is a wrapper script around a number of KAPPA
*    utilities. Since it understands the Scuba NDF file format, it
*    hides most of the complicated syntax from the user.  Mode = 'p'
*    and 'r' are wrappers around plotting utilities and facilitate the
*    inspection of the data of each bolometer.  The utility allows
*    change to the plot scales via the menu but will keep the scales
*    the same for all bolometers which makes is easy to compare
*    bolometers. Mode = 'd' allows for interactive despiking. Please
*    read the note below the description of the menu on the use of the
*    mouse.
*
*    Mode = 'p' or pltbol (or any p* link to scuplot) is a wrapper
*    around the kAPPA utility linplot. It allows plots of a whole series
*    of bolometers one by one, optionally overlaying them with the same
*    bolometer from a second file. Obvious overlays are despiked on
*    non-despiked data or data from different exposures to check the
*    noise.
*
*    Mode = 'r' or rlinplot (or any r* link to scuplot) is a wrapper
*    around the KAPPA utility mlinplot. It provides plots of sets of
*    bolometers in a single window with optionally data from a second
*    file in a second window.  Obvious files are despiked and
*    non-despiked data or data from different exposures to check the
*    noise.
*
*    Mode = 'd' or dspbol (or any d* link to scuplot) can be used to
*    interactively despike bolometers. While it is not as fast as a
*    completely integrated routine would be, it makes interactive
*    despiking much easier by hiding the cycle between linplot and
*    change_quality for the user. The most common use is to zoom in on
*    the region with the spike via the 'X' menu option (either typing
*    the input or using the cursor) and subsequently to flag the
*    offending point (just type the coordinate of the point, a range, or
*    use the cursor; IN GENERAL THE COORDINATE IS TO THE RIGHT OF THE
*    PLOTTED POINT). The routine will overlay the despiked data, prompt
*    the user to accept the new set and de-zoom to the original
*    scale. To reset a previously flagged point, flag the point again
*    but do NOT accept it: the point will be set to GOOD again.  Please
*    read the note below the derscription of the menu on the use of the
*    mouse.
*
*    For each mode the menu items are a subset of:
*
*      [M,H]                 Redisplay menu
*      [Q]                   Quit
*      [N]                   Next bolometer
*      [B#]                  Switch to bol #
*      [X min max], [X cen]  X-axis from min:max or cen+/-10
*                            Just 'x' activates the cursor.
*      [R]                   Reset X-axis
*      [Y min max], [Y lim]  Y-axis from min:max or -lim:+lim
*      [U]                   Reset Y-axis
*      [#], [#:#], [#-#]     Despike point or range of points;
*                            Just 'p' activates the cursor.
*
*      Option >
*
*    Note that a X center defined with the cursor or [X cen] defaults to
*    a 20 points window around cen, the position of the spike. Using the
*    CURSOR, the Left Mouse button always defines the point, the Right
*    Mouse button exits the cursor task while accepting the last point
*    clicked.

*  ADAM Parameters:
*     -h[elp]
*        Print the help information.
*     -m      mode:
*        Select usage mode:
*          p: plot bolometers one by one, optionally overlayed with
*             data from the second input file (equivalent to PLTBOL)
*          d: interactively despike the data for the bolometers in
*             specified file (equivalent to DSPBOL).
*          r: same as 'p' except that a whole set of bolometers is
*             plotted in a window (equivalent to RLINPLOT).
*     -f file
*        name of SDF file (.sdf may be included in the name).
*     -d file2
*        name of a second file: e.g. the despiked version of the SDF
*        file. The same bolometers will be plotted in a second window or
*        overlayed for comparison.
*     -s min max
*        Y-axis scales for plot (can be changed via menu).
*     -l #
*        number of bolometers per window
*     bol
*        list of bolometers to plot. Type 'all' for 1..37 and 'alls'
*        for 1..91. Can be added via menu if mode = 'r'.
*
*  Examples:
*     scuplot
*       The user will be asked for a mode and input file before proceeding.
*     scuplot -m d -f o39_lon_ext
*       Interactive despiking on o39_lon_ext.sdf (see also DSPBOL)
*     scuplot  -m p -f s14_lon_ext 12 13 18 20 25 26 19
*       Enter p mode and use file s14_lon_ext.sdf. Plot bolometers 12,13,
*       18, 20, 25, 26 and 19.

*  Notes:
*    - Can not handle blanked bolometers.
*    - If the overlay comes up scrambled, delete the agi_xxx files
*      in your home directory and if that does not work also files like
*      linplot.sdf in the /home/you/adam subdirectory.

*  Related Applications:
*    SURF: PLTBOL, DSPBOL, RLINPLOT, CHANGE_QUALITY
*    KAPPA: LINPLOT, MLINPLOT, CURSOR
*
*  Authors:
*    R.P.J. Tilanus (JACH)
*    {enter_new_authors_here}
*

*  Copyright:
*     Copyright (C) 1997,1998 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.9  2000/02/08 20:19:00  timj
*     Fix problems with $< in linux. Add "press return" message for CURSOR
*     use. Fix problem with $phsec not recognising PHOTOM observations when
*     the 'photom' string appears twice.
*
*     Revision 1.8  1999/11/04 04:36:16  timj
*     Update for KAPPA 0.14
*
*     Revision 1.7  1999/08/03 19:32:34  timj
*     Add copyright message to header.
*
*     Revision 1.6  1998/12/10 20:36:39  timj
*     Make sure that $ddf uses the PIXEL frame
*
*     Revision 1.5  1998/12/10 20:13:07  timj
*     Make V0.13 KAPPA compliant
*
*     Revision 1.4  1998/12/09 03:18:27  timj
*     Add '/' between task name and directory name.
*
*     Revision 1.3  1998/06/15 20:22:30  timj
*     Use $SURF_DIR and $KAPPA_DIR
*
*     Revision 1.2  1998/06/15 20:18:27  timj
*     Fix -h option (RPT)
*
*     Revision 1.1  1997/12/01 01:44:39  timj
*     Initial revision
*
*     {enter_further_changes_here}
*
*  Bugs:
*     Freezes when asked to plot a bad bolometer.
*     {note_any_bugs_here}
*
*-
