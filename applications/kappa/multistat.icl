PROC MULTISTAT

{ Prompt for the number of NDFs to analyse.  Ensure that it is positive.
    INPUTI "Number of frames: " (NUM)
    NUM = MAX( 1, NUM )

{ Loop NUM times.
    LOOP FOR I=1 TO (NUM)

{ Find the number of characters required to format the number as
{ a string using a couple of ICL functions.
      NC = INT( LOG10( I ) ) + 1

{ Generate the name of the NDF to be analysed via the ICL function
{ SNAME.
      FILE = '@' & SNAME('BUM',I,NC)

{ Form the statistics of the image.
      STATS NDF=(FILE)
   END LOOP
END PROC
