#
#  Procedure to apply data cuts to GAIA from the cl> command-line.
#
procedure gaiacut ( plane, lowcut, highcut )
   int plane          { 1, min=1, prompt = "Image plane" }
   int lowcut         { prompt = "Lower intensity limit" }
   int highcut        { prompt = "Upper intensity limit" }

begin
#  And run gaiacut externally to cl.
   print ( "! $GAIA_DIR/gaiacut "// plane //" "// lowcut //" "// highcut ) | cl
end
