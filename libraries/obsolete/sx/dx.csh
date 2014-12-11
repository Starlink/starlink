#+          
#  Name:
#     dx.csh

#  Purpose:
#     Start up the IBM Data Explorer (DX) incorporating the extensions in
#     the Starlink "SX" package.

#  Type of Module:
#     C shell script.

#  Invocation:
#     tcsh -c "source dx.csh" <options>

#  Description:
#     This procedure puts SX_DIR on the PATH if it is not already there.
#     It then runs dx in editor mode, with command line options to include 
#     the modules and macros in SX_DIR. Any dx command line options
#     supplied as arguments to this script are appended to the end of the
#     command. 
#
#     If the first option is "demo" then a DX/SX demo network is run. The
#     name of the demo should be given as the second option. If this is
#     not done, the user is prompted for the demo name.
#
#     By default, this script attempts to allocate three quarters of the 
#     currently un-used swap space to DX.

#  Authors:
#     DSB: D.S. Berry (STARLINK)
#     BLY: Martin Bly (STARLINK)
#     TIMJ: Tim Jenness (JAC, Hawaii)
#     {enter_new_authors_here}

#  History:
#     27-OCT-1995 (DSB):
#        Original Version derived from IRCAMPACK equivalent.
#     19-NOV-1996 (BLY):
#        Modified search for installed `dx' command.
#     10-MAR-1997 (BLY):
#        Modified invokation of swapon command for OSF1 to use full path.
#     29-AUG-2004 (TIMJ):
#        Add swap check for Linux
#     {enter_changes_here}

#-

#  Put the directory containing SX binaries (SX_DIR) on the path.

set foundit = "no"
if (`echo ${PATH} | grep -c ':'${SX_DIR}` != 0) set foundit = "yes"
if (`echo ${PATH} | grep -c '^'${SX_DIR}` != 0) set foundit = "yes"
if ( $foundit == "no" ) setenv PATH ${PATH}:${SX_DIR}
unset foundit

#  Find the command to execute dx as supplied by IBM (i.e. without the SX
#  extensions).

set ibm_dx = `sh -c 'type dx' | awk '{print $3}' -`

#  Find the amount of memory to use (0.75 of the available).
if ("`uname -s`" == "OSF1" ) then
   set t = `/usr/sbin/swapon -s | grep Available`
   @ use = $t[3] / 171

else if ("`uname -s`" == "Linux" ) then
   # run top once in batch mode
   set t = `top -b -n 1 | grep Swap`
   # Answer is in kB, we need it in megabytes (divide by 1024 / 3/4 = 1365 )
   @ use = `echo $t[6] | sed "s/k//"` / 1365

else
   # Solaris
   set t = `/usr/sbin/swap -s`
   @ use = `echo $t[11] | sed "s/k//"` / 1365

endif

#  Unless an sx demo is required, start up dx in editor mode. Tag any
#  user supplied arguments on to the end of the command.

if( "$1" != "demo" && "$1" != "-demo" ) then
   $ibm_dx -optimize memory -exec $SX_DIR/dxexec -macros $SX_DIR -mdf $SX_DIR/SX.mdf -memory $use $*


#  If a demo is to be run, ascertain which one.

else

   if( $#argv < 2 ) then
      echo " "
      echo " The following demonstrations are available:"
      echo " "
      echo " iso     - displays surfaces of constant value in 3D regular data grids"
      echo " slice   - displays 2-D slices through 3D regular data grids"
      echo " stream  - displays vector streamlines in 3D regular data grids"
      echo " scatter - displays irregular particle data in 3D"
      echo " "
      echo -n " Which demo do you want? "
      set demo = $<
   else
      set demo = $2
   endif


#  Identify the demo's network file.

   if( "$demo" == "iso" ) then
      set net = "iso_demo.net"
   
   else if( "$demo" == "slice" ) then
      set net = "slice_demo.net"
   
   else if( "$demo" == "stream" ) then
      set net = "stream_demo.net"
   
   else if( "$demo" == "scatter" ) then
      set net = "scatter_demo.net"
   
   else 
      echo " "
      echo " No such DX demo: '$demo'"
      echo " "
      exit 1
   endif


#  Run the demo. 

   $ibm_dx -exec $SX_DIR/dxexec -macros $SX_DIR -mdf \
            $SX_DIR/SX.mdf -image -program $SX_DIR/$net -noImageRWNetFile \
            -execute -memory $use

endif

# end
