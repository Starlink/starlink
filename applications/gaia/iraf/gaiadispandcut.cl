#
#  Procedure to apply intensity cuts to an image in GAIA.
#

procedure gaiadispandcut ( image, plane, lowcut, highcut )
   string image       { prompt = "Name of image to display" }
   int plane          { 1, min=1, prompt = "Image plane" }
   int lowcut         { prompt = "Lower intensity limit" }
   int highcut        { prompt = "Upper intensity limit" }

begin
   string realname

#  Get the fully expanded version of the file name.
   realname = osfn( image )

#  And run gaiadisp and gaiacut externally to cl.
   print ( "! $GAIA_DIR/gaiadisp.csh "// realname //" "// plane ) | cl
   print ( "! $GAIA_DIR/gaiacut "// plane //" "// lowcut //" "// highcut ) | cl
end
