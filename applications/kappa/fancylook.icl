PROC FANCYLOOK NDF

{ Function:
{    Plots a image with axes and colour-table key.

{ Insert the prefix to tell the parser that this is a file.
   IF SUBSTR( NDF, 1, 1 ) <> '@'
      FILE = '@' & (NDF)
   ELSE
      FILE = (NDF)
   END IF

{ Clear the current window.
   GDCLEAR CURRENT

{ Set the background and annotation colours.
   PALENTRY 0 Sienna
   PALENTRY 1 Yellow

{ Find the extent of the current picture and its aspect ratio.
   GDSTATE NCX1=(FX1) NCX2=(FX2) NCY1=(FY1) NCY2=(FY2) NOREPORT
   ASP = ( FX2 - FX1 ) / ( FY2 - FY1 )

{ Inquire the label of the current picture.  If it does not have one
{ label the current picture
   PICIN CURRENT LABEL=(ORIGLABEL) NOREPORT NAME=!
   IF ORIGLABEL = " "
      PICLABEL ORIGIN
      ORIGLABEL = "ORIGIN"
   END IF

{ Display the image with axes using the most-ornate fount
   DISPLAY (FILE) MODE=PE AXES FONT=NCAR COSYS=D SCALOW=(LOW) SCAHIGH=(HIGH) \

{ Find the extent of the last frame picture, i.e. the one associated
{ with the last displayed image.
   PICFRAME
   GDSTATE NCX1=(DX1) NCX2=(DX2) NCY1=(DY1) NCY2=(DY2) NOREPORT

{ Determine the widths of the borders.
   XL = DX1 - FX1
   XR = FX2 - DX2
   YB = DY1 - FY1
   YT = FY2 - DY2

{ Restore the original current picture.
   PICSEL (ORIGLABEL)

{ Only plot a key if there is room.
   IF MAX( XL, XR, YB, YT ) > 0.055

{ Determine which side has most room for the key.  First, see if the
{ key is vertical.
      IF MAX( XL, XR ) >= MAX( YB, YT )

{ Determine the width and height of the vertical key.  Bias to select
{ the right-hand side.  Part of the 0.75 is to allow for the wider
{ border to the left (0.19:0.05 of frame).  Try to obtain the same
{ width, subject to the constraint that it must fit inside the current
{ picture.
         ASPOBL = MAX( ASP, 1.0 )
         WIDTH = MIN( 1.2 * MAX( 0.75 * XL, XR ), 0.14 / ASPOBL )
         HEIGHT = 0.7 * ( DY2 - DY1 )

{ Define the bounds of the colour-table key.   Offset the key so that
{ there is no large gap between it and the image's axes.  The factors are
{ empirical, and no doubt could be improved with a more-sophisticated algorithm.
         DELTA = 0.12 * MAX( 0.0, DX2 - DX1 - 0.6 / ASPOBL )
         IF XL > 1.333 * XR
            XK1 = MAX( 0.01, DX1 - WIDTH + DELTA )
         ELSE
            XK1 = DX2 - DELTA * 5.0 / 19.0
         END IF
         XK2 = XK1 + WIDTH
         YK1 = 0.5 * ( DY2 + DY1 - HEIGHT )
         YK2 = YK1 + HEIGHT
      ELSE

{ Determine the width and height of the horizontal key.  Try to obtain
{ the same width, subject to the constraint that it must fit inside the
{ current picture.
         ASPPRO = MIN( ASP, 1.0 )
         WIDTH = MIN( 1.2 * MAX( YB, YT ), 0.1 * ASPPRO )
         HEIGHT = 0.7 * ( DX2 - DX1 )

{ Define the bounds of the colour-table key.   Offset the key so that
{ there is no large gap between it and the image's axes.  The factors are
{ empirical, and no doubt could be improved with a more-sophisticated algorithm.
         DELTA = 0.11 * MAX( 0.0, DY2 - DY1 - 0.6 * ASPPRO )
         IF YB > YT
            YK1 = MAX( 0.01, DY1 - WIDTH + DELTA )
         ELSE
            YK1 = DY1 - DELTA * 9.0 / 15.0
         END IF
         YK2 = YK1 + WIDTH
         XK1 = 0.5 * ( DX2 + DX1 - HEIGHT )
         XK2 = XK1 + HEIGHT
      END IF

{ So far the units are in NDC.  LUTVIEW uses co-ordinates which go from
{ (0,0) to (1,1) for both axes.  So transform some of the co-ordinates.
      IF ASP >= 1
         YK1 = YK1 * ASP
         YK2 = YK2 * ASP
      ELSE
         XK1 = XK1 / ASP
         XK2 = XK2 / ASP
      END IF

{ Draw the key to fit within the current picture annotating with
{ the scaling used in DISPLAY.
      LUTVIEW LOW=(LOW) HIGH=(HIGH) LBOUND=[ (XK1&','&YK1)] UBOUND=[ (XK2&','&YK2)] MODE=XY
   END IF
END PROC
