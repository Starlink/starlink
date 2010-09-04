#!/bin/csh -f
#
# !!begin
# !!title  WHT file separation script
# !!author T.R. Marsh
# !!created   01 January 2005
# !!revised   08 December 2005
# !!root   whtdiv
# !!index  whtdiv.csh
# !!descr  Shell script for dividing WHT data into aux, ISIS blue & red
# !!head1  Script for separating aux & ISIS blue/red data
#
# !!emph{whtdiv} divides AUX port and red and blue arm data from ISIS. It
# expects these to be in sdf format and to have had 'fixhead' run on them.
# It then uses hdstrace to list  It uses KAPPA's 'fitshead' to spot
# the type of data and then sends off the files to sub-directories aux,
# blue and red (!!emph{which should have been created beforehand!}).
#
# Arguments: just give a series of fits file names. The flag '-h' tries to cope
# with the loss of headers that occurred during the January 2005 ITP run. It must come
# first if at all, but it is specific top the detectors used in that run and
# may not work in general
#
# You must first have made directories 'red', 'blue' and 'aux' before running this script.
#
# !!end

source $STARLINK_DIR/etc/cshrc
source $STARLINK_DIR/etc/login

if($#argv < 1) then
  echo "usage: whtdiv (-h) file1 file2 ..."
  exit
endif

if($1 == "-h") then
    shift
    set head = "bad"
else
    set head = "good"
endif

# used sed to list just the headers

set sedsim = '/^SIMPLE  = /,/^END$/p'

foreach file ($argv)

  if(-e $file:r.sdf) then

    if($head == "good") then

      hdstrace $file:r.more.pamela.instrument | grep 'Cass. aux. port' > /dev/null
      if( !( $status ) ) then
        mv -vf $file aux/.
      else
        hdstrace $file:r.more.pamela.instrument | grep 'ISIS blue arm' > /dev/null
        if( !( $status ) ) then
           mv -vf $file blue/.
        else
          hdstrace $file:r.more.pamela.instrument | grep 'ISIS red arm' > /dev/null
          if( !( $status ) ) then
             mv -vf $file red/.
          else
             echo "Could not recognise instrument of $file"
          endif
        endif
      endif

    else

      hdstrace $file:r.more.pamela.detector | grep 'TEK2' > /dev/null
      if( !( $status ) ) then
        mv -vf $file aux/.
      else
        hdstrace $file:r.more.pamela.detector | grep 'EEV12' > /dev/null
        if( !( $status ) ) then
           mv -vf $file blue/.
        else
          hdstrace $file:r.more.pamela.detector | grep 'MARCONI2' > /dev/null
          if( !( $status ) ) then
             mv -vf $file red/.
          else
             echo "Could not recognise $file"
          endif
        endif
      endif
    endif
  endif

end

exit;




