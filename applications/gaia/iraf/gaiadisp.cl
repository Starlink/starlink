#
#  Procedure to run GAIA from the cl> command-line.
#
#  Note this isn't an attempt to get GAIA to run from the TV package
#  via display. That is a much more difficult task (i.e. we'd need to 
#  intercept the iie protocols and translate these, then create a 
#  working file or shared memory segment to store the image etc.).
#

procedure gaia ( image, plane )
   string image       { prompt = "Name of image to display" }
   int plane          { 1, min=1, prompt = "Image plane" }

begin
   string realname

#  Get the fully expanded version of the file name.
   realname = osfn( image )

#  And run gaia externally to cl (note we get all image formats this
#  way unless CONVERT was initialised outside of cl. We do not get
#  any variables set up as part of the cl!).
   print ( "! $GAIA_DIR/gaiadisp.csh "// realname //" "// plane ) | cl
end
