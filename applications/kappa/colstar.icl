PROC COLSTAR FILE,X,Y,SIZE,OUTFILE

{+
{  Arguments:
{     FILE = FILENAME (Given)
{        Input NDF containing one or more star images.
{     X = REAL (Given)
{        The approximate x position of the star.
{     Y = REAL (Given)
{        The approximate y position of the star.
{     SIZE = REAL (Given)
{        The half-width of the region about the star's centroid to be
{        plotted and saved in the output file.
{     OUTFILE = FILENAME (Given)
{        Output primitive NDF of 2*%SIZE+1 pixels square (unless
{        constrained by the size of the data array or because the location
{        of the star is near an edge of the data array.
{-

{ Ensure that the filenames have the @ prefix.
   IF SUBSTR( FILE, 1, 1 ) <> '@'
      NDF = '@' & (FILE)
   ELSE
      NDF = (FILE)
   END IF
   IF SUBSTR( OUTFILE, 1, 1 ) <> '@'
      NDFOUT = '@' & (OUTFILE)
   ELSE
      NDFOUT = (OUTFILE)
   END IF

{ Search for the star in a 21x21 pixel box.  The centroid of the
{ star is stored in the ICL variables XC and YC.
   CENTROID NDF=(NDF) INIT=[ (X&','&Y)] XCEN=(XC) YCEN=(YC) ~
     MODE=INTERFACE SEARCH=21 MAXSHIFT=14

{ Convert the co-ordinates to pixel indices.
   IX = NINT( XC + 0.5 )
   IY = NINT( YC + 0.5 )

{ Find the upper and lower bounds of the data array to plot.  Note
{ this assumes no origin information in stored in the data file.
   XL = MAX( 1, IX - SIZE )
   YL = MAX( 1, IY - SIZE )
   XU = MAX( 1, IX + SIZE )
   YU = MAX( 1, IY + SIZE )

{ Create a new primitive NDF centred on the star.
   NDFCOPY IN=(NDF)((XL):(XU),(YL):(YU)) OUT=(NDFOUT)

{ Draw a perspective histogram around the star on the current
{ graphics device.
   COLUMNAR IN=(NDFOUT)

{ Exit if an error occurred, such as not being to find a star
{ near the supplied position, or being unable to make the plot.
{   EXCEPTION ADAMERR
{      PRINT Unable to find or plot the star.
{   END EXCEPTION
END PROC

