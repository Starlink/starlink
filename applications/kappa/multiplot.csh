#!/bin/csh
#+
#  Name:
#     multiplot

#  Purpose:
#     Displays a series of NDF files in a series of PostScript files.

#  Language:
#     Unix C-shell

#  Invocation:
#     multiplot [-x xpic] [-y ypic] [-d device] [-P printer] ndf1 [ndf2...]

#  Description:
#     This displays a series of NDFs in a grid on a PostScript device.  Each
#     plot uses KAPPA's DISPLAY to draw with axes and a title equal to the
#     name of the NDF used to produce it.  The current scaling mode and
#     settings are used.
#
#     The plots are printed to a nominated device.

#  Arguments:
#     -x
#        The number of pictures along the x direction.  [4]
#     -y
#        The number of pictures along the y direction.  [2]
#     -d
#        The device name.  It must produce encapsulated PostScript. [epsf_l]
#     -P
#        The printer queue. [1]
#     ndf1 ...
#        A list of NDFs to be plotted.  The .sdf extension
#        should be supplied.

#  Output:
#     A series of Postscript files called plot_1.ps, plot_2.ps,...
#     one for each page.

#  Examples:
#     multiplot *

#  Prior Requirements:
#     For ease of use, it's recommended that you set up an alias for
#     this script, for example
#        alias multiplot 'source /home/bm/scripts/multiplot'
#     or put it in a directory in your path.

#  Notes:
#     -  

#  Authors:
#     MJC: Malcolm J. Currie (Starlink, RAL)
#     {enter_new_authors_here}

#  History:
#     1995 November 24 (MJC):
#        Original version.
#     1996 March 15 (MJC):
#        Allow for applications which produce multiple plot files
#        (though this doesn't apply to DISPLAY).
#     {enter_further_changes_here}

#-


# Conceal the startup messages.
    alias echo "echo > /dev/null"
    source $KAPPA_DIR/kappa.csh
    unalias echo

#  Initialize some shell variables.
    set xpic = 4
    set ypic = 2
    set printq = 2
    set devname = epsf_l
    set count = 0
    set args = ($argv[1-])
    set gfiles

#  Check that there are some arguments present.
    if ( $#args == 0 ) then
       echo "Usage: multiplot [-x xpic] [-y ypic] [-d device] [-P printer] ndf1 [ndf2...]"
       exit
    endif

#  Process each of the arguments to the script.
    while ( $#args > 0 )
       switch ($args[1])
       case -x:        #  Number of x pictures
          shift args
          set xpic = $args[1]
          shift args
          breaksw
       case -y:        #  Number of y pictures
          shift args
          set ypic = $args[1]
          shift args
          breaksw
       case -d:        #  Device name
          shift args
          set devname = $args[1]
          shift args
          breaksw
       case -P:        #  Printer name or number
          shift args
          set printq = $args[1]
          shift args
          breaksw
       case *:         # The NDFs
          set gfiles = ($gfiles[1-] $args[1])
          shift args
          breaksw
       endsw
     end

# Find the number of pictures per page.  Initialise the count of the number
# of plots.
     set npic = 8
     @ npic = $xpic * $ypic
     set nplot = 0

# Loop through the remaining arguments, assuming that these are NDFs.
     foreach file ($gfiles[1-])

# Obtain the NDF's name.
        set file1=$file:r

# Count the file number.
        @ count = $count + 1
        @ count = $count % $npic

# See if a new frame is to be started.
        if ( $count == 1 ) then

# Clear the database and remove the output Postscript file thus created.
           gdclear device=$devname";ZZjunkit"
           rm ZZjunkit*

           if ( $nplot > 0 ) then

# Merge the graphics.  Allow for multiple plots from the any task.
              psmerge plot1*.ps* > plot_$nplot.ps

# Print the plots to the chosen device.
              lpr -P$printq plot_$nplot.ps

# Remove the intermediate files.
              rm plot1*.ps*
           endif

# Increment the plot counter.
           @ nplot = $nplot + 1

# Set the counter to be high so that a wildcard list will preserve their
# order when they are merged.
           set pscount = 1000

# Create the grid of frames.
           picgrid $xpic $ypic device=$devname";plot"$pscount".ps"

# Increment the PostScript-file counter.  This will occur for each
# output file.
           @ pscount = $pscount + 1

# Set a greyscale colour table.
#           lutgrey device=$devname";plot"$pscount".ps"
#           @ pscount = $pscount + 1

        endif

# Select the next free FRAME picture.
        picempty device=$devname";plot"$pscount".ps"
        @ pscount = $pscount + 1

# Display in the picture using the current settings.
        display axes $file:r pltitl=$file:r device=$devname";plot"$pscount".ps" \\
        @ pscount = $pscount + 1

     end

# Deal with the last plot.
# Merge the graphics.
     psmerge plot1*.ps* > plot_$nplot.ps

# Print the plots to the chosen device.
     lpr -P$printq plot_$nplot.ps

# Remove the intermediate files.
     rm plot1*.ps*

     exit
