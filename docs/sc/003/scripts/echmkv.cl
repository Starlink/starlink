#

#+
#  Name:
#     echmkv

#  Purpose:
#     Script to extract orders from a dispersion-calibrated echelle
#     spectrum and convert to velocity at the specified wavelength.

#  Language:
#     IRAF cl script.

#  Description:
#     The problem:  One has echelle data with a series of lines, 
#     say Lyman series lines, which one wants to compare or combine 
#     in velocity space; that is the center of each line is zero velocity 
#     and the profile is in relative velocity.  The following script 
#     takes a list of echelle aperture (order) numbers and central 
#     wavelengths and extracts the orders from echelle wavelength-
#     calibrated images and modifies the header to place each order in 
#     relative velocity.  The individual 1-D orders can than be stacked 
#     with SPECPLOT or combined in velocity with SCOMBINE.
#
#     This script is useful for comparing sets of lines at different 
#     wavelengths, for example, Lyman lines.

#  Usage:
#     The "input" parameter is the name of a multiorder echelle spectrum.
#
#     The "lines" parameter is a two-column text file containing the 
#     aperture number for the desired feature and the wavelength for zero 
#     velocity.
#
#     The "output" parameter is a root filename.  A set of 1-D spectra 
#     with the root name and an appended integer will be produced, one 
#     for every line in the "lines" file.
#
#     To install and use:
#
#        1. Place script in your IRAF home$ directory as file echmkv.cl.
#
#        2. Interactively, load the onedspec package and type:
#
#              on> task echmkv=home$echmkv.cl
#
#        3. Or, in your login.cl or loginuser.cl place the following 
#           before the "keep":
#
#              task echmkv=home$echmkv.cl
#
#        4. Run the task by typing "echmkv".  You will be prompted
#           to enter values for the parameters. Alternatively, set values
#           using the parameter editor and then run the task.

#  Notes:
#     1. This works in all versions of IRAF V2.10.
#
#     2. The script requires the ONEDSPEC package or other spectral
#        package containing SCOPY.
#
#     3. Plots will have X-axes labeled as wavelength even though the 
#        value is velocity.

#  Authors:
#     ??: (IRAF)
#     MJC: Martin Clayton (Starlink)
#     {enter_new_authors_here}

#  History:
#     ??-???-???? (??):
#       Original version.
#     30-JAN-1996 (MJC):
#       Cookbook Version, extra comments and change in variable names.
#     {enter_further_changes_here}

#-

      procedure echmkv ( input, lines, output )

#  Parameters for the procedure.
      file input      {prompt="Input echelle image"}
      file lines      {prompt="File of apertures and wavelengths"}
      file output     {prompt="Output root name"}

#  Start the procedure.
      begin

#     Local variables.
        int i
        string in, out, ap, w, v0, dv

#     Query parameters.
        in = input
        out = output
        list = lines

#     Loop through the list of lines until exhausted.
        i = 0
        while ( fscan ( list, ap, w ) != EOF ) {
            i = i + 1
            scopy ( in, out//i, w1=INDEF, w2=INDEF, aperture=ap, beams="",
                    format="multispec", renumber=no, offset=0, clobber=no,
                    merge=no, rebin=yes, verbose=no )
            v0 = "((crval1/"//w//"-1)*2.998e5)"
            dv = "(cdelt1/"//w//"*2.998e5)"
            hedit ( out//i, "crval1", v0, update=yes, show=no, verify=no )
            hedit ( out//i, "cd1_1", dv, update=yes, show=no, verify=no )
            hedit ( out//i, "cdelt1", dv, update=yes, show=no, verify=no )
        }
        
#     Clear the lists variable.
        list = ""
     end

#  End-of-file.

