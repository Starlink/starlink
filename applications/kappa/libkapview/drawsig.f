      SUBROUTINE DRAWSIG ( STATUS )
*+
*  Name:
*     DRAWSIG

*  Purpose:
*     Draws +/-n standard-deviation lines on a line plot.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DRAWSIG( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine draws straight lines on an existing plot stored in
*     the graphics database, such as produced by LINPLOT or HISTOGRAM.
*     The lines are located at arbitrary multiples of the standard
*     deviation (NSIGMA) either side of the mean of a given dataset.
*     The default dataset is the one used to draw the existing plot.
*     You can plot the lines horizontally or vertically as appropriate.
*     The lines extend the full width or height of the plot's data
*     area.  Up to five different multiples of the standard deviation
*     may be presented in this fashion.
*
*     The application also computes statistics for those array values
*     that lie between each pair of plotted lines.  In other words it
*     finds the statistics between clipping limits defined by each
*     2*NSIGMA range centred on the unclipped mean.
*
*     The task tabulates NSIGMA, the mean, the standard deviation, and
*     the error in the mean after the application of each pair of
*     clipping limits.  For comparison purposes the first line of the
*     table presents these values without clipping.  The table is
*     written at the normal reporting level.

*  Usage:
*     drawsig ndf nsigma [axis] [comp] [sigcol] [linestyle]

*  ADAM Parameters:
*     AXIS = LITERAL (Read)
*        The orientation of the lines, or put another way, the axis
*        which represents data value.  Thus the allowed values are
*        "Horizontal", "Vertical", "X", or "Y".  "Horizontal" is
*        equivalent to "Y" and "Vertical" is a synonym for "X".  On
*        LINPLOT output AXIS would be "Y", but on a plot from HISTOGRAM
*        it would be "X".  The suggested default is the current value.
*        ["Y"]
*     COMP = LITERAL (Read)
*        The name of the NDF array component from which to derive the
*        mean and standard deviation used to draw the lines: "Data",
*        "Error", "Quality" or "Variance" (where "Error" is the
*        alternative to "Variance" and causes the square root of the
*        variance values to be taken before computing the statistics).
*        If "Quality" is specified, then the quality values are treated
*        as numerical values (in the range 0 to 255).  ["Data"]
*     DEVICE = DEVICE (Read)
*        The graphics device to draw the sigma lines on.
*        [Current graphics device]
*     LINESTYLE = _INTEGER (Read)
*        Line style to be used.  The allowed values produce the
*        following styles.
*             1 = solid
*             2 = dashed
*             3 = dotted
*             4 = dot-dashed
*
*        LINESTYLE defaults to the current value, which is initially 3,
*        giving dotted lines. []
*     NDF = NDF (Read)
*        The NDF structure containing the data array whose error limits
*        are to be plotted.  Usually this parameter is not defined
*        thereby causing the statistics to be derived from the dataset
*        used to draw the plot.  If, however, you had plotted a section
*        of a dataset but wanted to plot the statistics from the whole
*        dataset, you would specify the full dataset with parameter NDF.
*        [The dataset used to create the existing plot.]
*     NSIGMA() = _REAL (Read)
*        Number of standard deviations about the mean at which the
*        lines should be drawn.  The null value or 0.0 causes a line to
*        be drawn at the mean value.
*     SIGCOL = _INTEGER (Read)
*        The colour in which to draw any graphics specified by
*        parameter LINESTYLE.  The options are described below.
*
*          "MAX"          - The maximum colour index used for the
*                           display of the image.
*          "MIN"          - The minimum colour index used for the
*                           display of the image.
*          An integer     - The actual colour index.  It is constrained
*                           between 0 and the maximum colour index
*                           available on the device. 
*          A named colour - Uses the named colour from the palette, and
*                           if it is not present, the nearest colour
*                           from the palette is selected.
*
*        If the colour is to remain unaltered as the lookup table is
*        manipulated choose an integer between 0 and 15, or a named
*        colour.  The suggested default is the current value.
*     THICK = _REAL (Read)
*        The thickness of the lines in the plot, where 1.0 is the
*        normal thickness.  It must take a value in the range
*        0.5--10.0.  [1.0]

*  Examples:
*     drawsig nsigma=3 linestyle=1
*        This draws solid horizontal lines on the last DATA picture on
*        the current graphics device located at plus and minus 3
*        standard deviations about the mean.  The statistics come from
*        the data array used to draw the DATA picture.
*     drawsig phot 2.5
*        This draws horizontal plus and minus 2.5 standard-deviation
*        lines about the mean for the data in the NDF called phot on
*        the default graphics device.
*     drawsig cluster [2,3] X Error
*        This draws vertical lines at plus and minus 2 and 3
*        standard deviations about the mean for the error data in the
*        NDF called cluster on the default graphics device.
*     drawsig device=xwindows phot(20:119) 3 linestyle=3 sigcol=red
*        This draws red dotted horizontal lines on the xwindows device
*        at +/- 3 standard deviations using the 100 pixels in NDF
*        phot(20:119).

*  Notes:
*     There must be an existing DATA picture stored within the graphics
*     database for the chosen device.  Lines will only be plotted
*     within this picture.

*  Algorithm:
*     - Obtain the plot information from the graphics database
*     - Calculate the mean and standard deviation
*     - Plot lines at requested standard-deviation multiples.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, and
*     QUALITY, components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  The
*     statistics are calculated using double-precision floating point.
*     -  Any number of NDF dimensions is supported.

*  Related Applications:
*     KAPPA: HISTOGRAM, LINPLOT, MLINPLOT, STATS.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 October 14 (TIMJ):
*        Original version
*     1996 October 16 (MJC):
*        Expanded the documentation.  Fixed a bug that caused the final
*        line always to be solid.  Added AXIS parameter.  Standardised
*        the code style.  Renamed parameter LINSTYLE to LINESTYLE.
*     6-MAY-1998 (DSB):
*        Update the GKS workstation after changing polyline
*        representations, and do not re-instate original representations
*        at end. This prevents the screen being cleared when the
*        workstation is closed.
*     23-JUN-1998 (DSB):
*        Used KPG1_MAP instead of NDF_MAP, so that NaN and Inf values
*        are converted to Starlink BAD values before being used.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT  NONE             ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'GKS_PAR'          ! GKS constants (e.g. GSET)
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Status:
      INTEGER STATUS             ! Global Status

*  Local Constants:
      INTEGER MXCLIP             ! Maximum number of clipping levels
      PARAMETER ( MXCLIP = 5 )

      INTEGER MPEN               ! SGS pen number used to draw graphics
      PARAMETER ( MPEN = 3 )

*  Local Variables:
      CHARACTER * ( 10 ) AXIS    ! Data-value axis/orientation in plot
      LOGICAL BAD                ! Bad pixels may be present?
      CHARACTER * ( 75 ) BUFFER  ! Buffer for the results
      INTEGER CI                 ! Colour index required for graphics
      REAL CLIP( MXCLIP )        ! Array of clipping limits
      INTEGER COLI               ! Original colour index of current pen
      CHARACTER * ( 28 ) COMLIS  ! List of available array components
      INTEGER COMLN              ! Length of component list
      CHARACTER * ( 8 ) COMP     ! Name of array component to analyse
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled?
      DOUBLE PRECISION DMAX      ! Maximum value of pixels in array
      DOUBLE PRECISION DMAXC     ! Maximum pixel value after clipping
      DOUBLE PRECISION DMIN      ! Minimum value of pixels in array
      DOUBLE PRECISION DMINC     ! Minimum pixel value after clipping
      INTEGER EL                 ! Number of array elements mapped
      LOGICAL GOTLOC             ! A locator to the NDF has been
                                 ! obtained?
      LOGICAL GOTNAM             ! A reference name of the NDF has been
                                 ! obtained?
      INTEGER I                  ! Counter
      INTEGER IERR               ! GKS error indicator
      INTEGER IMAX( 1 )          ! Vector index of maximum pixel
      INTEGER IMAXC( 1 )         ! Vector index of maximum clipped pixel
      INTEGER IMIN( 1 )          ! Vector index of minimum pixel
      INTEGER IMINC( 1 )         ! Vector index of minimum clipped pixel
      INTEGER IWKID              ! GKS workstation identifier
      INTEGER LNTYPE             ! Line type to be used
      INTEGER LNTYPI             ! Initial line type for current SGS pen
      CHARACTER * ( DAT__SZLOC ) LOCI ! Locator for input data structure
      REAL LWIDTH                ! The width of the current SGS pen
      DOUBLE PRECISION MEAN      ! Mean of pixels in array
      DOUBLE PRECISION MEANC     ! Mean of pixels after clipping
      CHARACTER * ( 8 ) MCOMP    ! Component name for mapping arrays
      INTEGER NCLIP              ! Number of clipping iterations
      INTEGER NDF                ! NDF identifier
      INTEGER NGOOD              ! Number of valid pixels in array
      INTEGER NGOODC             ! Number of valid pixels after clipping
      INTEGER NSIGMA             ! Number of sigma values
      INTEGER PEN                ! Current SGS pen
      INTEGER PICID              ! Input picture identifier
      INTEGER PICIDI             ! Data image picture identifier
      INTEGER PNTR( 1 )          ! Pointer to input DATA_ARRAY component
      CHARACTER * ( 256 ) REFNAM ! Reference name
      LOGICAL SETPLR             ! Polyline representation to be reset?
      REAL SIGLOW                ! Y position of lower line
      REAL SIGTOP                ! Y position of upper line
      DOUBLE PRECISION STDEV     ! Standard devn. of pixels in array
      DOUBLE PRECISION STDEVC    ! Std. devn. of pixels after clipping
      DOUBLE PRECISION SUM       ! Sum of pixels in array
      DOUBLE PRECISION SUMC      ! Sum of pixels after clipping
      LOGICAL THERE              ! Array component exists?
      REAL THICK                 ! Line thickness
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type for processing
      REAL WX1                   ! World co-ordinate of LHS of DATA zone
      REAL WX2                   ! World co-ordinate of RHS of DATA zone
      REAL WY1                   ! World co-ordinate of bottom DATA zone
      REAL WY2                   ! World co-ordinate of top of DATA zone
      INTEGER ZONEO              ! SGS zone of the displayed image
      INTEGER ZONEOV             ! SGS zone of the input picture

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DEVCAN = .FALSE.
      GOTLOC = .FALSE.
      GOTNAM = .FALSE.

*  Obtain an SGS zone for the last DATA picture.
*  =============================================

*  Associate graphics device and start database activity.  Update access
*  is used so that line can be drawn without destroying the existing
*  plot.
      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID, ZONEOV, STATUS )
      
*  Find the last DATA picture.
      CALL KPG1_AGFND( 'DATA', PICIDI, STATUS )
      
*  Obtain the SGS zone identifier for the current DATA picture.
      CALL AGS_NZONE( ZONEO, STATUS )
      
*  Report the name, comment, and label, if one exists, for the current
*  picture.
      CALL KPG1_AGATC( STATUS )

*  Obtain a reference to the NDF.
*  ==============================
      CALL KPG1_AGREF( PICIDI, 'READ', GOTNAM, REFNAM, STATUS )
      
*  See whether the reference is a name or locator.  The latter should
*  be phased out, but there may be some old databases and software in
*  circulation.
      CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
      IF ( GOTLOC ) LOCI = REFNAM
      
*  End immediately if there was an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*  Obtain the NDF.
*  ===============
      
*  Begin an NDF context.
      CALL NDF_BEGIN
      
*  Obtain the NDF.  If the name is given on the command line it will be
*  used.  If not, the database data reference is used, if there is one.
*  Otherwise, the user is prompted.
      CALL KPG1_ASREF( 'NDF', 'READ', GOTNAM, REFNAM, NDF, STATUS )

*  Find which array component to use.
*  ==================================
*  Inquire which arrays are available and form a comma-separated list
*  of them.
      CALL KPG1_ARCOL( NDF, 'Data,Quality,Error,Variance', COMLIS,
     :                 COMLN, STATUS )

*  Find which component to plot.  No need to inquire the value, if the
*  only array component is Data.  Note the mixed-case returned in the
*  list is for attractive error reports.  See below why there is a
*  MCOMP.
      IF ( COMLIS .EQ. 'Data' ) THEN
         COMP = 'DATA'
         MCOMP = COMP
      ELSE
         CALL PAR_CHOIC( 'COMP', 'Data', COMLIS( :COMLN ), .FALSE.,
     :                   COMP, STATUS )

*  Most NDF routines with a component argument don't recognise 'ERROR',
*  so we need two variables.  Thus convert 'ERROR' into 'VARIANCE' in
*  the variable needed for such routines.  The original value is held
*  in a variable with the prefix M for mapping, as one of the few
*  routines that does support 'ERROR' is NDF_MAP.
         MCOMP = COMP
         IF ( COMP .EQ. 'ERROR' ) COMP = 'VARIANCE'

      END IF

*  Map the array.
*  ==============

*  This application supports all the non-complex numeric types
*  directly.  Therefore for the given type of the image find in which
*  type it should be processed.
      CALL NDF_TYPE( NDF, COMP, TYPE, STATUS )

*  Map the input array.
      CALL KPG1_MAP( NDF, MCOMP, TYPE, 'READ', PNTR, EL, STATUS )

*  Find whether there may be bad pixels present.  There is no explicit
*  check.  It just relies on the current value.
      CALL NDF_BAD( NDF, COMP, .FALSE., BAD, STATUS )

*  Defer error reporting and obtain an array of clipping limits to be
*  applied. Constrain the values to be positive.
      NCLIP = 0
      CALL ERR_MARK
      CALL PAR_GDRVR( 'NSIGMA', MXCLIP, 0.0, VAL__MAXR, CLIP,
     :                NSIGMA, STATUS )

*  Interpret a null value as indicating that a line at the mean
*  be drawn.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NSIGMA = 1
         CLIP( 1 ) = 0.0
      END IF
      CALL ERR_RLSE

*  Obtain the data-value axis.
*  ===========================
      CALL PAR_CHOIC( 'AXIS', 'Y', 'X,Y,Horizontal,Vertical', .FALSE.,
     :                AXIS, STATUS )

*  Get information on line style and colour.
*  =========================================

*  Get the line thickness.
      CALL PAR_GDR0R( 'THICK', 1.0, 0.5, 10.0, .TRUE., THICK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 980

*  Inquire the workstation identifier for GKS inquiries.
      CALL SGS_ICURW( IWKID )
      SETPLR = .FALSE.

*  Obtain the colour index for the desired colour of the lines.
*  Do not restrict the colours to the palette.
      CALL KPG1_IVCI( 'DEVICE', 'SIGCOL', .FALSE., CI, STATUS )

*  Obtain the line style.
      CALL PAR_GDR0I( 'LINESTYLE', 1, 1, 4, .TRUE., LNTYPE, STATUS )

*  Set the linestyle and colour of the pen used to draw the lines.
*  ===============================================================

*  Inquire the current colour index of this pen (it will be restored
*  after all plotting is complete).
      CALL GQPLR( IWKID, MPEN, GSET, IERR, LNTYPI, LWIDTH, COLI )
 
*  Store the new colour index, line style and thickness for this pen.
      CALL GSPLR( IWKID, MPEN, LNTYPE, THICK, CI )
      SETPLR = .TRUE.

*  Ensure that the pen changes have been applied. This may cause GKS to 
*  redraw or clear the screen. It must be done now because otherwise, it
*  would be done when the workstation is closed, resulting in the newly
*  drawn graphics being erased.
      CALL GUWK( IWKID, 1 )

*  See if a GKS error has occurred.
      CALL GKS_GSTAT( STATUS )

*  Inquire the current SGS pen, and then select the pen used to draw
*  markers.
      CALL SGS_IPEN( PEN )
      CALL SGS_SPEN( MPEN )

*  Now get some information on the plot bounds.
      CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )

*  Find Some Statistics
*  ====================

*  If the clipped mean and standard deviaiton is to be reported
*  (verbose message reporting), print some headings.
      CALL MSG_OUTIF( MSG__NORM, 'BLANK', ' ', STATUS )
      CALL MSG_OUTIF( MSG__NORM, 'HEADING',
     :  '      Clip (+/-)         '/
     :  /'mean          std. deviation    Error in mean', STATUS )
      CALL MSG_OUTIF( MSG__NORM, 'HEADING2',
     :  '      ----------         '/
     :  /'----          --------------    -------------', STATUS )

*  Loop for every standard-deviation limit.
      DO I = 1, NSIGMA

*  Define the number of clipping iterations.
         IF ( CLIP( I ) .GT. 0.0 ) THEN
            NCLIP = 1
         ELSE
            NCLIP = 0
         END IF

*  Call the routine of the appropriate data type.
         IF ( TYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_STATB( BAD, EL, %VAL( PNTR( 1 ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )
 
         ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_STATUB( BAD, EL, %VAL( PNTR( 1 ) ), NCLIP,
     :                        CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                        IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                        IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC,
     :                        SUMC, MEANC, STDEVC, STATUS )
 
         ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_STATD( BAD, EL, %VAL( PNTR( 1 ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )
 
         ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_STATI( BAD, EL, %VAL( PNTR( 1 ) ), NCLIP, 
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )
 
         ELSE IF ( TYPE .EQ. '_REAL' ) THEN
            CALL KPG1_STATR( BAD, EL, %VAL( PNTR( 1 ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )
 
         ELSE IF ( TYPE .EQ. '_WORD' ) THEN
            CALL KPG1_STATW( BAD, EL, %VAL( PNTR( 1 ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )
 
         ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_STATUW( BAD, EL, %VAL( PNTR( 1 ) ), NCLIP,
     :                        CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                        IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                        IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC,
     :                        SUMC, MEANC, STDEVC, STATUS )
         END IF

*  Work out where the horizontal lines should be and draw them across
*  the full extent of the picture.  Do not plot two lines where the
*  clipping value is zero (NCLIP=0).
         IF ( AXIS .EQ. 'Y' .OR. AXIS( 1:1 ) .EQ. 'H' ) THEN
            SIGTOP = REAL( MEAN + ( STDEV * CLIP( I ) ) )
            SIGLOW = REAL( MEAN - ( STDEV * CLIP( I ) ) )

            IF ( SIGTOP .LT. WY2. AND. SIGTOP .GT. WY1 ) THEN
               CALL SGS_LINE( WX1, SIGTOP, WX2, SIGTOP )
            END IF
            IF ( SIGLOW .GT. WY1 .AND. SIGLOW .LT. WY2 .AND.
     :           NCLIP .EQ. 1 ) THEN
               CALL SGS_LINE( WX1, SIGLOW, WX2, SIGLOW )
            END IF

*  Work out where the vertical lines should be and draw them across the
*  full extent of the picture.  Do not plot two lines where the clipping
*  value is zero (NCLIP=0).
         ELSE IF ( AXIS .EQ. 'X' .OR. AXIS( 1:1 ) .EQ. 'V' ) THEN
            SIGTOP = REAL( MEAN + ( STDEV * CLIP( I ) ) )
            SIGLOW = REAL( MEAN - ( STDEV * CLIP( I ) ) )

            IF ( SIGTOP .LT. WX2. AND. SIGTOP .GT. WX1 ) THEN
               CALL SGS_LINE( SIGTOP, WY1, SIGTOP, WY2 )
            END IF
            IF ( SIGLOW .GT. WX1 .AND. SIGLOW .LT. WX2 .AND.
     :           NCLIP .EQ. 1 ) THEN
               CALL SGS_LINE( SIGLOW, WY1, SIGLOW, WY2 )
            END IF

         END IF

*  Report the clipped mean and standard deviation.
*  ===============================================

*  Report the unclipped values first.
         IF ( I .EQ. 1 .AND. NGOOD .GT. 0 ) THEN
            WRITE( BUFFER, '(16X,3(5X,G13.6))' ) MEAN, STDEV,
     :             STDEV / SQRT( DBLE( NGOOD ) )
            CALL MSG_OUTIF( MSG__NORM, 'RESULTS', BUFFER, STATUS )
         END IF

*  Now report the clipped values.
         IF ( NCLIP .EQ. 1 .AND. NGOODC .GT. 0 ) THEN
            WRITE( BUFFER, '(8X,F6.3,2X,3(5X,G13.6))' )
     :             CLIP( I ), MEANC, STDEVC,
     :             STDEVC / SQRT( DBLE( NGOODC ) )
            CALL MSG_OUTIF( MSG__NORM, 'RESULTS', BUFFER, STATUS )
         END IF
      END DO

*  Tidy up the input data structure.
      CALL NDF_ANNUL( NDF, STATUS )
      IF ( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
      CALL DAT_VALID( LOCI, GOTLOC, STATUS )
      IF ( GOTLOC ) CALL DAT_ANNUL( LOCI, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If necessary, re-instate the original colour index for the marker
*  pen.
      CALL SGS_SPEN( PEN )

*  AGI closedown
*  =============
 
  980 CONTINUE
 
*  Need to tidy up the graphics database before exiting.
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )
 
  999 CONTINUE

      END
