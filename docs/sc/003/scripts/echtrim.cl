#

#+
#  Name:
#     echtrim

#  Purpose:
#     Script to trim individual echelle orders.

#  Language:
#     IRAF cl script.

#  Description:
#     When the orders of a collapsed echellogram are merged it is
#     often the case that regions of overlap between two adjacent
#     orders have quite different signal levels.  This can be for
#     several reasons, most notably the blaze function.  When merging
#     the orders a weighted sum of some kind is often used, however,
#     there are almost always one or two discontinuities in the single
#     spectrum produced.  One way of ensuring that there is only one
#     discontinuity per-order, and this is at a known position, is
#     to trim the wavelength range of each of the orders of the 
#     echellogram so that there is precisely no overlap.  Instead, 
#     the orders each cover an adjacent region of the spectrum.

#     This script allows the specification of sections of the orders
#     of an echellogram to be used to produce a single spectrum.
#     A file listing the order numbers and required sections of the
#     orders is read and the selected regions of each order are then
#     trimmed into the output echelle image.  Several similar echelle
#     images can be processed at the same time by specifying a list
#     of input images.

#  Usage:
#     The "input" parameter is a list of IRAF echelle format images.
#
#     The "orders" parameter is a text file containing lines with order 
#     number and 1-D image section for trimming.  See the example below
#     for the format of the file.  Basically, you should use the standard
#     IRAF syntax for specifying a section of an image, one order per line
#     of the file.
#
#     Each specified order is trimmed and merged into the "output" image.
#
#     The "input" and "output" images may be the same.
#
#     To install and use:
#
#        1. Place script in your IRAF home$ directory as file echtrim.cl.
#
#        2. Interactively, load the echelle package and type:
#
#              ec> task echtrim=home$echtrim.cl
#
#        3. Or, in your login.cl or loginuser.cl place the following before 
#           the "keep":
#
#              task echtrim=home$echtrim.cl
#
#        4. Run the task by typing "echtrim".  You will be prompted
#           to enter values for the parameters. Alternatively, set values
#           using the parameter editor and then run the task.

#  Note:
#     1. This works with IRAF V2.10.2 through V2.10.4.
#
#     2. The script requires the ECHELLE package or other spectral
#        package containing SCOPY.

#  Example:
#     ec> imhead test.ec l-
#     test.ec[512,10][real]: Artificial Echelle Spectrum
#     ec> type sec.dat
#     [1:100,1]
#     [101:200,2]
#     [201:300,3]
#     [301:400,4]
#     [401:500,5]
#     [101:300,6]
#     [101:400,7]
#     ec> echtrim test.ec test1.ec sec.dat
#     test.ec -> test1.ec
#     test.ec[1:100,1](1)  -->  test1.ec[*,1](1)
#     test.ec[101:200,2](2)  -->  test1.ec[*,2](2)
#     test.ec[201:300,3](3)  -->  test1.ec[*,3](3)
#     test.ec[301:400,4](4)  -->  test1.ec[*,4](4)
#     test.ec[401:500,5](5)  -->  test1.ec[*,5](5)
#     test.ec[101:300,6](6)  -->  test1.ec[*,6](6)
#     test.ec[101:400,7](7)  -->  test1.ec[*,7](7)

#  Authors:
#     FV: Francisco Valdez (IRAF)
#     MJC: Martin Clayton (Starlink)
#     {enter_new_authors_here}

#  History:
#     ??-???-???? (FV):
#       Original version.
#     30-JAN-1996 (MJC):
#       Cookbook Version, extra comments and change in variable names.
#     {enter_further_changes_here}

#-

      procedure echtrim ( input, output, orders )

#  Parameters for the procedure.
      string  input    {prompt="List of input echelle images to trim"}
      string  output   {prompt="List of output trimmed echelle images"}
      file    orders   {prompt="File of orders and sections"}

      struct  *in, *out, *secs

#  Start the procedure.
      begin

#     Local variables.
         file inlist, outlist, seclist, inim, outim, trim

#     Generate names for temporary files.
         inlist = mktemp ( "tmp$iraf" )
         outlist = mktemp ( "tmp$iraf" )

#     Expand image lists to temporary files.
         sections ( input, option="fullname", > inlist )
         sections ( output, option="fullname", > outlist )

#     Get the list of orders and trim sections.
         seclist = orders

#     Open lists for processing and loop through them.
         in = inlist
         out = outlist

#     Keep going until either image list is exhausted.
         while ( fscan ( in, inim ) != EOF && fscan ( out, outim ) != EOF ) {
            
#        If the output image is different to the input, create a new
#        file by copying the input to the output.
            if ( inim != outim )
               imcopy ( inim, outim, verbose=yes )

#        Open the order section list for each image and loop through orders.
            secs = seclist
            while ( fscan ( secs, trim ) != EOF ) {
               scopy ( inim//trim, outim, w1=INDEF, w2=INDEF, 
                       apertures="", beams="", apmodulus=0, 
                       format="multispec", renumber=no, offset=0, 
                       clobber=yes, merge=yes, rebin=yes, verbose=yes )
            }

#        Clear the order section list.
            secs = ""
         }

#     Clear the input and output order lists.
         inlist = ""
         outlist = ""
      end

#  End-of-file.

