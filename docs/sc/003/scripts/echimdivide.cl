#

#+
#  Name:
#     echimdivide

#  Purpose:
#     Script to divide a series of extracted echelle spectra by an 
#     extracted flat-field and then rescale to the original means of 
#     the spectra.

#  Language:
#     IRAF cl script.

#  Description:
#     This script is for removing the blaze function of an extracted
#     echelle spectrum.  In this particular case an extracted flat-field
#     spectrum is used to divide-out the blaze function.
#     The script proceeds by dividing the echelle image by the flat field.
#     This will lead to some change in the mean flux level in the
#     orders of the spectrum.  To remove the level shift, the mean
#     value of each order in the input image and in the flat-field-divided
#     image is found.  A "correction" ratio for each order can then
#     be calculated and used to rescale the output image so that the
#     resulting blaze-corrected image retains the same per-order mean
#     level as the input.  By calculating a per-order mean, rather than
#     rescaling the image by a single factor, the order-to-order blaze
#     corrections are retained.

#  Usage:
#     The "flat" parameter is the name of an extracted flat field.
#
#     The "input" and "output" lists of spectra may be the same.
#
#     To install and use:
#
#        1. Place script in your IRAF home$ directory as echimdivide.cl.
#
#        2. Interactively type:
#
#              cl> task echimdivide=home$echimdivide.cl
#
#        3. Or, in your login.cl or loginuser.cl place the following 
#           before the keep:
#
#              task echimdivide = home$echimdivide.cl
#
#        4. Run the task by typing "echimdivide".  You will be prompted
#           to enter values for the parameters. Alternatively, set values
#           using the parameter editor and then run the task.

#  Authors:
#     FV: Francisco Valdez (IRAF)
#     MJC: Martin Clayton (Starlink)
#     {enter_new_authors_here}

#  History:
#     ??-???-???? (FV):
#       Original version.
#     30-JAN-1996 (MJC):
#       Cookbook Version, extra comments.
#     {enter_further_changes_here}

#-

procedure echimdivide ( input, output, flat )

#  Procedure parameters.
      string  input = ""   {prompt="List of input echelle spectra"}
      string  output = ""  {prompt="List of output echelle spectra"}
      file    flat = ""    {prompt="Flat-field echelle spectrum"}

      struct  *infd, *outfd

#  Start the procedure.
      begin
        
#     Local variables.
         int naxis1
         string inlist, outlist, in, out, flt
         string imtemp1, imtemp2, outtemp

#     Define temporary working files.
         inlist = mktemp ( "tmp$iraf" )
         outlist = mktemp ( "tmp$iraf" )
         imtemp1 = mktemp ( "tmp$iraf" )
         imtemp2 = mktemp ( "tmp$iraf" )

#     Expand the image lists and, for the input images,
#     include the width of the image (length of each line) in the
#     list.
         hselect ( input, "$I,naxis1", yes, > inlist )
         sections ( output, option="fullname", > outlist )
         flt = flat

#     Loop through input images.  
         infd = inlist
         outfd = outlist

#     Keep going until the input image list is exhausted.
         while ( fscan ( infd, in, naxis1 ) != EOF ) {

#        Display error message if the output image list is too short.
            if ( fscan ( outfd, out ) == EOF )
               error ( 1, "Output list ended prematurely" )

#        Process the case where the input and output images are
#        different.
            if ( out != in ) {
                
#           Display an informational message.                
               printf ( "ECHIMDIVIDE: %s -> %s\n", in, out )

#           Divide the input image by the flat-field image, 
#           replacing any divide-by-zero pixels by the number zero.
               imarith ( in, "/", flt, out, divzero=0., hparams="",
                         pixtype="", calctype="", verbose=no, noact=no )

#           Determine the mean (average) value in each order in both
#           the input and the output images.  This produces a list
#           of per-order mean fluxes for each of the two images.
               blkavg ( in, imtemp1, naxis1, 1 )
               blkavg ( out, imtemp2, naxis1, 1 )

#           Find the ratios of the per-order means in the input and output
#           images.
               imarith ( imtemp1, "/", imtemp2, imtemp1, divzero=0., 
                         hparams="", pixtype="", calctype="", verbose=no, 
                         noact=no )

#           Expand the list of per-order mean ratios into an image
#           the same size as the echelle image.  This allows a simple
#           image multiply to rescale the output.
               blkrep ( imtemp1, imtemp1, naxis1, 1 )

#           Perform the image rescale.  Each order in the output
#           has the same mean flux as in the input.
               imarith ( out, "*", imtemp1, out, divzero=0., hparams="",
                         pixtype="", calctype="", verbose=no, noact=no )
            
#        Process the case where the input and output images are
#        the same, so that the results overwrite the input.
            } else {
                
#           Display an informational message.                
               printf ( "ECHIMDIVIDE: %s\n", in )

#           Generate a temporary file for the output.
#           Once the output data have been correctly scaled this
#           file can be written to replace the input.
               outtemp = mktemp ( "tmp$iraf" )

#           Divide the input image by the flat-field image, 
#           replacing any divide-by-zero pixels by the number zero.
               imarith ( in, "/", flt, outtemp, divzero=0., hparams="",
                         pixtype="", calctype="", verbose=no, noact=no )

#           Determine the mean (average) value in each order in both
#           the input and the output images.  This produces a list
#           of per-order mean fluxes for each of the two images.
               blkavg ( in, imtemp1, naxis1, 1 )
               blkavg ( outtemp, imtemp2, naxis1, 1 )

#           Find the ratios of the per-order means in the input and output
#           images.
               imarith ( imtemp1, "/", imtemp2, imtemp1, divzero=0., 
                         hparams="", pixtype="", calctype="", verbose=no, 
                         noact=no )
#           Expand the list of per-order mean ratios into an image
#           the same size as the echelle image.  This allows a simple
#           image multiply to rescale the output.
               blkrep ( imtemp1, imtemp1, naxis1, 1 )

#           Perform the image rescale.  Each order in the output
#           has the same mean flux as in the input.
               imarith ( outtemp, "*", imtemp1, outtemp, divzero=0., 
                         hparams="", pixtype="", calctype="", verbose=no, 
                         noact=no )

#           Delete the existing input image.
               imdelete ( in, verify=no )

#           Rename the temporary output file replace the original.
               imrename ( outtemp, out, verbose=no )
            }

#        Delete temporary files for per-order means.
            imdelete ( imtemp1, verify=no )
            imdelete ( imtemp2, verify=no )
         }

#     Tidy up after the script by deleting temporary files and
#     clearing variables no longer used.
         delete ( inlist, verify=no )
         delete ( outlist, verify=no )
         infd = ""
         outfd = ""
      end

#  End-of-file.
