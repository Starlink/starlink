#
#  Procedure to apply a percentile data cut to GAIA from the cl> command-line.
#
procedure gaiapercent ( plane, percent )
   int plane          { 1, min=1, prompt = "Image plane" }
   int percent        { prompt = "Percentile cut" }

begin
#  And run gaiacut externally to cl.
   print ( "! $GAIA_DIR/gaiacut "// plane //" "// percent ) | cl
end
