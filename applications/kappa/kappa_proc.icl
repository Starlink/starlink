HIDDEN PROC LUTBGYRW

{+
{  Name:
{     LUTBGYRW.ICL

{  Purpose:
{     Loads the BGYRW lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     This procedure loads the "BGYRW" lookup table with linear scaling
{     into the current image-display device.  It is a continuous LUT
{     starting with blue, followed by green, yellow, red and a splash of
{     white.

{  Usage:
{     lutbgyrw

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Apr 10 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     1994 June 23 (MJC):
{	 UNIX version.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=@$KAPPA_DIR/bgyrw_lut
END PROC

HIDDEN PROC LUTCOL

{+
{  Name:
{     LUTCOL.ICL

{  Purpose:
{     Loads the standard colour lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     Procedure for loading the standard colour lookup table into
{     the current image-display device with linear scaling.

{  Usage:
{     lutcol

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Mar 2 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=COLOUR
END PROC

HIDDEN PROC LUTCONT

{+
{  Name:
{     LUTCONT.ICL

{  Purpose:
{     Loads a lookup table to give the display the appearance of a
{     contour plot.

{  Type of module:
{     ICL procedure.

{  Description:
{     This procedure loads a lookup table that gives a contour-plot
{     appearance into the current image-display device.  The lookup table
{     is mainly black with a set of white stripes and it is loaded with
{     linear scaling.

{  Usage:
{     lutcont

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Apr 16 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     1994 June 23 (MJC):
{	 UNIX version.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=@$KAPPA_DIR/cont_lut NN
END PROC

HIDDEN PROC LUTFC

{+
{  Name:
{     LUTFC.ICL

{  Purpose:
{     Loads the standard false-colour lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     This procedure loads the standard false-colour lookup table with
{     linear scaling into the current image-display device.

{  Usage:
{     LUTFC

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the ADAM
{     parameter cannot be specified on the command line.  You will
{     only be prompted for the DEVICE parameter if the device current
{     image display is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1991 April 24 (MJC):
{	 Original version.
{     1994 June 23 (MJC):
{	 UNIX version.
{     {enter_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=@$KAPPA_DIR/fc_lut NN
END PROC

HIDDEN PROC LUTGREY

{+
{  Name:
{     LUTGREY.ICL

{  Purpose:
{     Loads the standard greyscale lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     Procedure for loading the standard greyscale lookup table into
{     the current image-display device with linear scaling.

{  Usage:
{     lutgrey

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Mar 2 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=GREY
END PROC

HIDDEN PROC LUTHEAT

{+
{  Name:
{     LUTHEAT.ICL

{  Purpose:
{     Loads the heat lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     This procedure loads the "heat" lookup table with linear scaling
{     into the current image-display device.

{  Usage:
{     lutheat

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Apr 10 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     1994 June 23 (MJC):
{	 UNIX version.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=@$KAPPA_DIR/heat_lut
END PROC

HIDDEN PROC LUTIKON

{+
{  Name:
{     LUTIKON.ICL

{  Purpose:
{     Loads the default Ikon lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     This procedure loads the default Ikon lookup table with linear
{     scaling into the current image-display device.

{  Usage:
{     lutikon

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Notes:
{     -  This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.
{     -  The device need not be an Ikon.

{  Prior requirements:
{     - The KAPPA definitions must be assigned.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 July 9 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     1994 June 23 (MJC):
{	 UNIX version.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=@$KAPPA_DIR/ikon_lut NN
END PROC
HIDDEN PROC LUTNEG

{+
{  Name:
{     LUTNEG.ICL

{  Purpose:
{     Loads the standard negative greyscale lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     Procedure for loading the standard greyscale lookup table into
{     the current image-display device with negative linear scaling.

{  Usage:
{     lutneg

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.

{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Mar 2 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=NEGATIVE
END PROC

HIDDEN PROC LUTRAMPS

{+
{  Name:
{     LUTRAMPS.ICL

{  Purpose:
{     Loads the coloured-ramps lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     This procedure loads the coloured-ramps lookup table with linear
{     scaling into the current image-display device.

{  Usage:
{     lutramps

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Apr 10 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     1994 June 23 (MJC):
{	 UNIX version.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=@$KAPPA_DIR/ramps_lut NN
END PROC
HIDDEN PROC LUTREAD [LUT]

{+
{  Name:
{     LUTREAD.ICL

{  Purpose:
{     Loads an image-display lookup table from an NDF.

{  Type of module:
{     ICL procedure.

{  Description:
{     This application reads a lookup table stored in an NDF with
{     the standard format, and loads it into the current image-display
{     device.

{  Usage:
{     lutread lut

{  Arguments:
{     LUT = LITERAL (Given)
{        The file containing the lookup table.  It is passed to the
{        parameter LUT but not validated.

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]
{     LUT = NDF (Read)
{        Name of the NDF containing the lookup table as its data
{        array.  The LUT must be 2-dimensional, the first dimension
{        being 3, and the second being arbitrary.  Linear interpolation
{        is used to compress or expand the colour table if the second
{        dimension is different from the number of unreserved colour
{        indices.  Also the LUT's values must lie in the range 0.0--1.0.

{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameters
{     cannot be specified on the command line.  You will only be
{     prompted for the parameters if the device is not suitable or not
{     available, and/or the lookup table file could not be accessed.

{  Prior requirements:
{     - The KAPPA definitions must be assigned.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Mar 2 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     1995 October 2 (MJC):
{	 Made LUT an optional argument from ICL.
{     {enter_further_changes_here}

{-
   IF UNDEFINED(LUT)

{   Allow for the case where the user has not given a LUT.  In this case
{   LUTABLE should prompt for the name of the LUT file.

      LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL

{   The file name needs to be prefixed by an @ sign. So check
{   whether or not it present.  If not, add one.

   ELSE IF SUBSTR( LUT, 1, 1 ) = '@'
      LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=(LUT)
   ELSE
      DUMMY = '@' & (LUT)
      LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=(DUMMY)
   END IF

END PROC

HIDDEN PROC LUTSPEC

{+
{  Name:
{     LUTSPEC.ICL

{  Purpose:
{     Loads a spectrum-like lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     This procedure loads a spectrum-like lookup table with linear scaling
{     into the current image-display device.

{  Usage:
{     lutspec

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Apr 16 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     1994 June 23 (MJC):
{	 UNIX version.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=@$KAPPA_DIR/spectrum_lut
END PROC

HIDDEN PROC LUTZEBRA

{+
{  Name:
{     LUTZEBRA.ICL

{  Purpose:
{     Loads a pseudo-contour lookup table.

{  Type of module:
{     ICL procedure.

{  Description:
{     This procedure loads a pseudo-contour lookup table with linear
{     scaling into the current image-display device. The lookup table
{     is mainly black with a set of white stripes.

{  Usage:
{     lutzebra

{  ADAM Parameters:
{     DEVICE = DEVICE (Read)
{        Name of the image display whose colour table is to be changed.
{        [Current image-display device]

{  Prior requirements:
{     - The KAPPA definitions must be assigned.
{
{  Notes:
{     This is a procedure that calls LUTABLE.  Therefore, the parameter
{     cannot be specified on the command line.  You will only be
{     prompted for the DEVICE parameter if the current image display
{     is not suitable or not available.

{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     {enter_new_authors_here}

{  History:
{     1990 Apr 10 (MJC):
{	 Original version.
{     1991 April 24 (MJC):
{	 NDF version of LUTABLE and extended the prologue.
{     1994 June 23 (MJC):
{	 UNIX version.
{     {enter_further_changes_here}

{-

   LUTABLE MAPPING=LINEAR COLTAB=EXTERNAL LUT=@$KAPPA_DIR/zebra_lut NN
END PROC
