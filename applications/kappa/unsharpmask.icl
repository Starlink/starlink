PROC UNSHARPMASK NDFIN CLIP NDFOUT

{ Insert ampersands to tell the command-line interpreter than these
{ strings are file names.
   IF SUBSTR( NDFIN, 1, 1 ) <> '@'
      NDFIN = '@' & (NDFIN)
   END IF
   IF SUBSTR( JUNK, 1, 1 ) <> '@'
      NDFOUT = '@' & (NDFOUT)
   END IF

{ Clip the image to remove the cores of stars and galaxies above
{ a nominated threshold.
    THRESH (NDFIN) tmp1 THRHI=(CLIP) NEWHI=(CLIP) \

{ Apply a couple of block smoothings with boxsizes of 5 and 13
{ pixels.  Delete the temporary files as we go along.
    BLOCK tmp1 tmp2 BOX=5
    ! rm tmp1.sdf
    BLOCK tmp2 tmp3 BOX=13
    ! rm tmp2.sdf

{ Multiply the smoothed image by a scalar.
    CMULT tmp3 0.8 tmp4
    ! rm tmp3.sdf

{ Subtract the smoothed and renormalised image from the input image.
{ The effect is highlight the fine detail, but still retain some of
{ low-frequency features.
    SUB (NDFIN) tmp4 (NDFOUT)
    ! rm tmp4.sdf
END PROC
