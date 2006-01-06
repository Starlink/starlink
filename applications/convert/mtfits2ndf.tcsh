#+
#  Name:
#     MTFITS2NDF

#  Purpose:
#     Converts FITS magnetic tape files into NDFs.

#  Type of Module:
#     C-Shell script - calls ADAM A-task

#  Description:
#     This application converts files from a FITS tape into NDFs by
#     using shell commands mt and dd to position the tape and convert the
#     selected tape files into FITS disk files and then using FITS2NDF to
#     produce the NDFs.  The intermediate FITS files may be saved.

#  Usage:
#     mtfits2ndf in out block=n [of=fits_file] [<fits2ndf_pars>]

#  ADAM Parameters:
#     BLOCK = _INTEGER (Read)
#        The the FITS blocking factor, i.e. the block size on the tape is
#        this value multiplied by the standard FITS block size.  The
#        suggested default is 10.
#     IN = DEVICE (Read)
#        The name of a tape device.  For correct tape positioning, a no-rewind
#        device must be used.  The device name may be have a file specifiers
#        appended, separated by commas and enclosed in [].  The file specifiers
#        indicate which files from the tape are to be processed.
#        For example:
#          [2] indicates the second file on the tape.
#          [4-6] indicates files 4 to 6.
#          [5-] indicates file 5 to the last file on the tape.
#          [1,3-5,7-] indicates files 1,3,4,5, and 7 to the end of the tape.
#          If no file specifiers are given, all files on the tape will be 
#          processed.
#     OF = LITERAL (Read)
         Name(prefix) of the intermediate FITS file(s) copied from the tape.
         Only set this if you want to save the intermediate FITS file(s).  If
#        a number of files are being produced, the name should contain a *,
#        which will be replaced by the corresponding FITS tape file number.
#        If OF is not specified, fitsmtin#.fits will be used and deleted.
#        (See also Note 1).
#     OUT = NDF (Write)
#        The name of the NDF(s) to be produced by FITS2NDF.  This is passed to
#        FITS2NDF but only a single element string can be specified.  It can
#        contain the matching patterns allowed for FITS2NDF, for example '*'.
#     <fits2ndf_pars>
#        Other parameters will be passed to FITS2NDF---see the description of
#        FITS2NDF.

#  Notes:
#     1. This application is a tcsh script which calls an ADAM A-task.
#        CONVERT startup sets alias mtfits2ndf to 'tcsh mtfits2ndf.tcsh',
#        and tcsh must be on the user's PATH.
#     2. The string specified for the intermediate FITS file name(s) will be
#        presented as the IN parameter for the FITS2NDF call.  All files
#        matching the string will be used, whether or not they were produced
#        in this run. (See the FITS2NDF description for details.)

#  Examples:
#     Note that []* etc must be protected from the shell
#
#     mtfits2ndf /dev/nst0[2] f256 block=10 fmtcnv=f
#        This converts the second file on the tape on device /dev/nst0 to
#        an NDF called f256. The FITS blocking factor of the tape is 10.
#        As a result of the parameters passed to FITS2NDF, the data type of
#        the NDF's data array matches that of the FITS primary data array,
#        a FITS extension is created in f256, and FITS sub-files are
#        propagated to NDF extensions.
#     mtfits2ndf /dev/nst0 * block=1 of=ral256_*.fit
#        Will convert each file on the tape on device /dev/nst0 (with a
#        blocking factor of 1) to FITS disk files named ral256_*.fit,
#        where * is replaced by each tape file number. The FITS files will
#        be converted to NDFs named ral256_*.sdf and retained.
#     mtfits2ndf
#       The user is prompted for the input device, the output NDF name and
#       the FITS blocking factor.  All other parameters are defaulted.

#  References:
#     NASA Office of Standards and Technology, 1997, "A User's Guide
#       for the Flexible Image Transport System (FITS)", version 4.0.
#     NASA Office of Standards and Technology, 1999, "Definition of
#       the Flexible Image Transport System (FITS)".

#  Related Applications:
#     CONVERT: FITS2NDF; KAPPA: FITSDIN, FITSIN.

#  Deficiencies:
#     1. Facilities for naming multiple output files are limited.
#
#     2. The command is not available from ICL, nor as an option in the 
#        automatic (on-the-fly) conversion system
#
#     3. Extensions within a FITS file may not be specified.  However, it
#        is possible to pass an EXTABLE parameter to the FITS2NDF operation
#        to select extensions.
#
#     4. Only tape devices for which the mt and dd commands will work may
#        be used.
#     [routine_deficiencies]...

#  Authors:
#     AJC: Alan J. Chipperfield (Starlink)
#     MJC: Malcolm J. Currie (Starlink)
#     {enter_new_authors_here}

#  History:
#     29-OCT-2001 (AJC):
#        Original version.
#     2005 November 10 (MJC):
#        Use standard parameter-type and access, and non-UNIX-shell
#        specific examples.  Removed the temporary awk script in two
#        other exit locations.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

# Obtain parameters from the command line.
# ========================================

# Set default variable values.
   set inf = ""
   set of = ""
   set ibs = ""
   set adampars = ""
   set of = "mtf2ndf*.fits"
   set ofdef = "true"
   @ nfiles = 0

# Now search the command line, constructing the parameters for the dd command
# and the fits2ndf command.
   while ( $#argv > 0 )
      set arg = `echo "$1" | tr inblockf INBLOCKF`
      switch ( "$arg" )
      case IN=*:
         set inf = `echo "$1" | awk -F= '{print $2}'`
         breaksw
      case OF=*:
         set of = `echo "$1" | awk -F= '{print $2}'`
         set ofdef = "false"
         breaksw
      case BLOCK=* :
         set ibs = `echo "$1" | awk -F= '{print $2*2880}'`
         breaksw
      default:
         set adampars = "$adampars $1"
      endsw

# Break if last argument just done
      if ( $#argv < 2 ) then
         break
      endif

# otherwise do next argument
      shift
   end

# Now ensure we have required arguments for dd.
# =============================================

# Ensure we have the input tape file.
# -----------------------------------
   if ( "$inf" == "" ) then

# It may have been given as a positional argument (position 1).
      if ( "$adampars" != "" ) then
         cat <<\**** >! awk000f
            { for (i=1; i<=NF; i++ ) {
                 if ( index($i, "=") == 0 ) {print $i;break}
              }
            }
\****

         set inf = `echo "$adampars" | awk -f awk000f`

# Or by keyword.
         if ( "$inf" != "" ) then
            cat <<\**** >! awk000f
               { done=0;
                 for (i=1; i<=NF; i++) {\
                    if (index($i, "=") != 0) {\
                       print $i\
                    } else {\
                       if ( done == 0 ) {
                          done = 1 \
                       } else { \
                          print $i \
                       } \
                    } \
                 } \
               }
\****

            set adampars=`echo "$adampars" | awk -f awk000f`
            \rm awk000f
         endif
      endif

# Tape drive
# ----------
# Ask for the tape drive if it was not supplied on the command line,
# mimicking a PAR prompt.
      if ( "$inf" == "" ) then
         printf "IN - Input tape unit > "
         set inf = "$<"
         if ( "$inf" == "" ) then
            while ( "$inf" == "" )
               printf "IN - Input tape unit or abort (!)> "
               set inf = "$<"
            end
         endif              

# Check for abort response; we need a fiddle here to prevent inf = ! from
# causing syntax error,
         if ( "$inf" == "\!" || "$inf" == "\!\!" ) then
            echo aborting
            \rm awk000f
            exit 1
         endif
      endif
   endif

# Find the required file position.  The awks are not as simple as might
# be because of problems on Solaris.
   set fc = `echo "$inf" | awk -F[ '/\[.*\]/{ split($2,a,"]"); print a[1]}'`
   set inf = `echo "$inf" | awk -F[ '{print $1}'`

# Find the input buffer size.
# ---------------------------

#  Mimic a PAR prompt, but accept is not avaailble.
   if ( "$ibs" == "" ) then
      printf "BLOCK - Input blocking factor /10/ > "
      set block = "$<"
      if ( "$block" == "" ) then
         set ibs = 28800

# Check for an abort request.
      else if ( "$block" == "\!" || "$block" == "\!\!" ) then
         echo aborting
         \rm awk000f
         exit 1

# The normal case.
      else
         @ ibs = 2880 * $block
      endif
   endif

# Get a list of the required file numbers using an awk script.
# ============================================================

# The N means the last file.
   cat <<\**** >! awk000f
   { for (i=1; i<=NF; i++) {
        nf = split($i, b, "-")
        if (nf > 1) {
           if (b[2] == "") {
              print b[1] " N"
              break
           } else {
              for (j=b[1]; j<=b[2]; j++) { print j }
           }
        } else { print b[1] }
     }
   }
\****

   if ( $fc != "" ) then
      set files = `echo "$fc" | awk -F, -f awk000f`

# Process all the FITS files.
   else
      set files = "N"
   endif

# Read the files from the tape.
# =============================

# Ensure the tape starts at the beginning .
#   echo "mt -f $inf rewind"
   mt -f $inf rewind || exit 1

   set filpos = 1

# Now convert each requested file on the tape.
   set i = 1
   while ( $i <= $#files )

      set file = $files[$i]
      if ( $file == "N" ) then
         @ file = $filpos
      else
         set file = $files[$i]            
         @ i = $i + 1
      endif      

# Move to required position.
# --------------------------
# Files are numbered from 1 relative to where we are on the tape, i.e.
# [1] means read from where we are; [2] means skip one tapemark before
# reading etc.  The special case [0] means rewind the tape before reading.
      @ nskip = $file - $filpos
      @ filpos = $file + 1

      if ( $nskip > 0 ) then
#         echo "mt -f $inf fsf $nskip"
         mt -f $inf fsf $nskip

# Mimic PCS error message.
         if ( $status ) then
            echo "\!\! Failed to find tape file $file"
            if ( $nfiles ) then
               echo "\!  Processing files already found"
            else
               exit 1
               \rm awk000f
            endif
            break
         endif
      endif
   
# Now do the dd command.
      set ofile = `echo "$of" | sed -e "s/*/$file/"`
      echo "Converting tape file $file to a FITS disk file $ofile"
#      echo "dd if=$inf of=$ofile ibs=$ibs obs=2880"
      dd if=$inf of=$ofile ibs=$ibs obs=2880 >& /dev/null

      if ( -z $ofile ) then
         echo "\!\! Tape file $file was empty---assumed end of tape."
         \rm $ofile
         break
      else
         @ nfiles = $nfiles + 1
      endif 
      
   end

# Convert the intermediate FITS file(s) to NDF.
# =============================================

# Use FITS2NDF to do the hard work, passing it the FITS files and 
# remaining parameters.
   echo "Converting $of to NDF"
#   echo fits2ndf "$of" "$adampars"
   $CONVERT_DIR/fits2ndf "$of" "$adampars"

# Now clean up any temporary FITS file.
   if ( "$ofdef" == "true" ) then
      rm -f mtf2ndf*.fits
   endif
   \rm awk000f

# End of file
