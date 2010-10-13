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
*     may be presented in this fashion. Each line can be drawn with a
*     different style (see Parameter STYLE).
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
*     drawsig ndf nsigma [axis] [comp]

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
*     NDF = NDF (Read)
*        The NDF structure containing the data array whose error limits
*        are to be plotted.  Usually this parameter is not defined
*        thereby causing the statistics to be derived from the dataset
*        used to draw the plot.  If, however, you had plotted a section
*        of a dataset but wanted to plot the statistics from the whole
*        dataset, you would specify the full dataset with Parameter NDF.
*        [The dataset used to create the existing plot.]
*     NSIGMA() = _REAL (Read)
*        Number of standard deviations about the mean at which the
*        lines should be drawn.  The null value or 0.0 causes a line to
*        be drawn at the mean value.
*     STYLE = GROUP (Read)
*        A group of attribute settings describing the plotting style to use
*        for the lines.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text file
*        preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and
*        interpreted in the same manner. Attribute settings are applied in
*        the order in which they occur within the list, with later settings
*        over-riding any earlier settings given for the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value> is
*        the value to assign to the attribute. Default values will be
*        used for any unspecified attributes. All attributes will be
*        defaulted if a null value (!) is supplied. See section "Plotting
*        Attributes" in SUN/95 for a description of the available
*        attributes. Any unrecognised attributes are ignored (no error is
*        reported).
*
*        The attributes Colour(Curves), Width(Curves), etc, can be used
*        to specify the style for the lines ("Lines" is recognised as a
*        synonym for "Curves"). These values apply to all
*        lines unless subsequent attributes over-ride them. Attributes for
*        individual clipping levels can be given by replacing "Curves" above
*        by a string of the form "Nsig<i>" where "<i>" is an integer
*        index into the list of clipping levels supplied for parameter
*        NSIGMA. Thus, "Colour(Nsig1)" will set the colour for the lines
*        associated with the first clipping level, etc. The attribute
*        settings can be restricted to one of the two lines by appending
*        either a "+" or a "-" to the "Nsig<i>" string. Thus,
*        "Width(Nsig2-)" sets the line width for the lower of the two
*        lines associated with the second clipping level, and "Width(Nsig2+)"
*        sets the width for the upper of the two lines. [current value]

*  Examples:
*     drawsig nsigma=3 style='style=1'
*        This draws solid horizontal lines on the last DATA picture on
*        the current graphics device located at plus and minus 3
*        standard deviations about the mean.  The statistics come from
*        the data array used to draw the DATA picture.
*     drawsig phot 2.5
*        This draws horizontal plus and minus 2.5 standard-deviation
*        lines about the mean for the data in the NDF called phot on
*        the default graphics device.
*     drawsig phot 2.5 style='"colour(nsig1-)=red,colour(nsig1+)=green"'
*        As above, but the lower line is drawn in red and the upper line
*        is drawn in green.
*     drawsig cluster [2,3] X Error
*        This draws vertical lines at plus and minus 2 and 3
*        standard deviations about the mean for the error data in the
*        NDF called cluster on the default graphics device.
*     drawsig device=xwindows phot(20:119) 3 style="'colour=red,style=4'"
*        This draws red dotted horizontal lines on the xwindows device
*        at +/- 3 standard deviations using the 100 pixels in NDF
*        phot(20:119).

*  Notes:
*     There must be an existing DATA picture stored within the graphics
*     database for the chosen device.  Lines will only be plotted
*     within this picture.

*  Related Applications:
*     KAPPA: HISTOGRAM, LINPLOT, MLINPLOT, STATS.

*  Implementation Status:
*     -  This routine correctly processes the DATA, VARIANCE, and
*     QUALITY, components of the NDF.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  The
*     statistics are calculated using double-precision floating point.
*     -  Any number of NDF dimensions is supported.

*  Copyright:
*     Copyright (C) 1996, 1998-1999, 2004 Central Laboratory of the
*     Research Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 October 14 (TIMJ):
*        Original version.
*     1996 October 16 (MJC):
*        Expanded the documentation.  Fixed a bug that caused the final
*        line always to be solid.  Added AXIS parameter.  Standardised
*        the code style.  Renamed Parameter LINSTYLE to LINESTYLE.
*     6-MAY-1998 (DSB):
*        Update the GKS workstation after changing polyline
*        representations, and do not re-instate original representations
*        at end. This prevents the screen being cleared when the
*        workstation is closed.
*     27-OCT-1999 (DSB):
*        Major changes to use AST/PGPLOT for the graphics.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PAR_ERR'          ! PAR error codes
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global Status

*  Local Constants:
      INTEGER MXCLIP             ! Maximum number of clipping levels
      PARAMETER ( MXCLIP = 5 )

*  Local Variables:
      CHARACTER AXIS*10          ! Data-value axis/orientation in plot
      CHARACTER BUFFER*75        ! Buffer for the results
      CHARACTER COMP*8           ! Name of array component to analyse
      CHARACTER LOCI*( DAT__SZLOC ) ! Locator for input data structure
      CHARACTER MCOMP*8          ! Component name for mapping arrays
      CHARACTER REFNAM*256       ! Reference name
      CHARACTER SYN*10           ! Synonym for an AST attribute
      CHARACTER TYPE*( NDF__SZTYP ) ! Numeric type for processing
      DOUBLE PRECISION DMAX      ! Maximum value of pixels in array
      DOUBLE PRECISION DMAXC     ! Maximum pixel value after clipping
      DOUBLE PRECISION DMIN      ! Minimum value of pixels in array
      DOUBLE PRECISION DMINC     ! Minimum pixel value after clipping
      DOUBLE PRECISION FINISH( 2 )! Current Frame coords at end of line
      DOUBLE PRECISION LBNDIN( 2 )! GRAPHICS lower bounds of viewport
      DOUBLE PRECISION MEAN      ! Mean of pixels in array
      DOUBLE PRECISION MEANC     ! Mean of pixels after clipping
      DOUBLE PRECISION START( 2 )! Current Frame coords at start of line
      DOUBLE PRECISION STDEV     ! Standard devn. of pixels in array
      DOUBLE PRECISION STDEVC    ! Std. devn. of pixels after clipping
      DOUBLE PRECISION SUM       ! Sum of pixels in array
      DOUBLE PRECISION SUMC      ! Sum of pixels after clipping
      DOUBLE PRECISION UBNDIN( 2 )! GRAPHICS upper bounds of viewport
      DOUBLE PRECISION XL( 2 )   ! GRAPHICS Frame coords
      DOUBLE PRECISION XU( 2 )   ! GRAPHICS Frame coords
      INTEGER DATAX              ! Index of data axis in Current Frame of Plot
      INTEGER EL                 ! Number of array elements mapped
      INTEGER I                  ! Counter
      INTEGER IAT                ! Used length of SYN
      INTEGER IMAX( 1 )          ! Vector index of maximum pixel
      INTEGER IMAXC( 1 )         ! Vector index of maximum clipped pixel
      INTEGER IMIN( 1 )          ! Vector index of minimum pixel
      INTEGER IMINC( 1 )         ! Vector index of minimum clipped pixel
      INTEGER INDF               ! NDF identifier
      INTEGER IPIC0              ! Input picture identifier
      INTEGER IPICD              ! Data image picture identifier
      INTEGER IPIN               ! Pointer to input array
      INTEGER IPLOT              ! Plot for drawing over DATA picture
      INTEGER NCLIP              ! Number of clipping iterations
      INTEGER NGOOD              ! Number of valid pixels in array
      INTEGER NGOODC             ! Number of valid pixels after clipping
      INTEGER NSIGMA             ! Number of sigma values
      INTEGER OTHAX              ! Index of other axis in Current Frame of Plot
      LOGICAL BAD                ! Bad pixels may be present?
      LOGICAL GOTLOC             ! A locator to the NDF has been obtained?
      LOGICAL GOTNAM             ! A reference name of the NDF has been obtained?
      REAL CLIP( MXCLIP )        ! Array of clipping limits
      REAL X1                    ! World co-ordinate of LHS of viewport
      REAL X2                    ! World co-ordinate of RHS of viewport
      REAL Y1                    ! World co-ordinate of bottom viewport
      REAL Y2                    ! World co-ordinate of top of viewport

*.

*  Check the global inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST Context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN( STATUS )

*  Open the graphics device and workstation. An error is reported if no
*  existing DATA picture can be found. An AST Plot is returned which
*  can be used to draw in the existing Plot.
      CALL KPG1_PLOTA( AST__NULL, 'OLD', ' ', IPIC0, IPICD, IPLOT,
     :                 STATUS )

*  Report the name, comment, and label, if one exists, for the current
*  picture.
      CALL KPG1_AGATC( STATUS )

*  Obtain a reference to the NDF.
      CALL KPG1_AGREF( IPICD, 'READ', GOTNAM, REFNAM, STATUS )

*  See whether the reference is a name or locator.  The latter should be
*  phased out, but there may be some old databases and software
*  in circulation.
      CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
      IF( GOTLOC ) LOCI = REFNAM

*  End immediately if there was an error.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF.  If the name is given on the command line it will be
*  used.  If not, the database data reference is used, if there is one.
*  Otherwise, the user is prompted.
      CALL KPG1_ASREF( 'NDF', 'READ', GOTNAM, REFNAM, INDF, STATUS )

*  Find which array component to use.
      CALL KPG1_ARCOG( 'COMP', INDF, MCOMP, COMP, STATUS )

*  This application supports all the non-complex numeric types
*  directly.  Therefore for the given type of the image find in which
*  type it should be processed.
      CALL NDF_TYPE( INDF, COMP, TYPE, STATUS )

*  Map the input array.
      CALL KPG1_MAP( INDF, MCOMP, TYPE, 'READ', IPIN, EL, STATUS )

*  Find whether there may be bad pixels present.  There is no explicit
*  check.  It just relies on the current value.
      CALL NDF_BAD( INDF, COMP, .FALSE., BAD, STATUS )

*  End immediately if there was an error.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain an array of clipping limits to be applied. Constrain the values to
*  be positive.
      NCLIP = 0
      CALL PAR_GDRVR( 'NSIGMA', MXCLIP, 0.0, VAL__MAXR, CLIP, NSIGMA,
     :                STATUS )

*  Interpret a null value as indicating that a line at the mean be drawn.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NSIGMA = 1
         CLIP( 1 ) = 0.0
      END IF

*  Obtain the data-value axis.
      CALL PAR_CHOIC( 'AXIS', 'Y', 'X,Y,Horizontal,Vertical', .FALSE.,
     :                AXIS, STATUS )

*  Get the index of the data axis (IAXIS), and the other axis (JAXIS).
      IF( AXIS .EQ. 'Y' .OR. AXIS( 1:1 ) .EQ. 'H' ) THEN
         DATAX = 2
         OTHAX = 1
      ELSE
         DATAX = 1
         OTHAX = 2
      END IF

*  Get the extent of the DATA picture in GRAPHICS co-ordinates.
      CALL PGQWIN( X1, X2, Y1, Y2 )

*  Get the range of values on the other axis covered by the DATA picture.
      LBNDIN( 1 ) = X1
      LBNDIN( 2 ) = Y1
      UBNDIN( 1 ) = X2
      UBNDIN( 2 ) = Y2

      CALL AST_MAPBOX( IPLOT, LBNDIN, UBNDIN, .TRUE., OTHAX,
     :                 START( OTHAX ), FINISH( OTHAX ), XL, XU, STATUS )

*  Find Some Statistics. If the clipped mean and standard deviaiton is to be
*  reported (verbose message reporting), print some headings.
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_OUT( 'HEADING',
     :  '      Clip (+/-)         '/
     :  /'mean          std. deviation    Error in mean', STATUS )
      CALL MSG_OUT( 'HEADING2',
     :  '      ----------         '/
     :  /'----          --------------    -------------', STATUS )

*  Loop for every standard-deviation limit.
      DO I = 1, NSIGMA

*  Define the number of clipping iterations.
         IF( CLIP( I ) .GT. 0.0 ) THEN
            NCLIP = 1
         ELSE
            NCLIP = 0
         END IF

*  Call the routine of the appropriate data type.
         IF( TYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_STATB( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )

         ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
            CALL KPG1_STATUB( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), NCLIP,
     :                        CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                        IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                        IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC,
     :                        SUMC, MEANC, STDEVC, STATUS )

         ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_STATD( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )

         ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_STATI( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )

         ELSE IF( TYPE .EQ. '_REAL' ) THEN
            CALL KPG1_STATR( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )

         ELSE IF( TYPE .EQ. '_WORD' ) THEN
            CALL KPG1_STATW( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), NCLIP,
     :                       CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                       IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                       IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC, SUMC,
     :                       MEANC, STDEVC, STATUS )

         ELSE IF( TYPE .EQ. '_UWORD' ) THEN
            CALL KPG1_STATUW( BAD, EL, %VAL( CNF_PVAL( IPIN ) ), NCLIP,
     :                        CLIP( I ), NGOOD, IMIN( 1 ), DMIN,
     :                        IMAX( 1 ), DMAX, SUM, MEAN, STDEV, NGOODC,
     :                        IMINC( 1 ), DMINC, IMAXC( 1 ), DMAXC,
     :                        SUMC, MEANC, STDEVC, STATUS )
         END IF

*  Ensure that any previous synonyms for AST attributes are cleared.
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Use Lines and Line+ as synonyms for Curve.
         CALL KPG1_ASPSY( '(LINES)', '(CURVE)', STATUS )
         CALL KPG1_ASPSY( '(LINE+)', '(CURVE)', STATUS )

*  Make the synonymns "(NSIG<i>)" and "(NSIG<i>+)" equate to "(Curve)"
         SYN = '(NSIG'
         IAT = 5
         CALL CHR_PUTI( I, SYN, IAT )
         CALL CHR_APPND( ')', SYN, IAT )
         CALL KPG1_ASPSY( SYN( : IAT ), '(CURVE)', STATUS )

         IAT = IAT - 1
         CALL CHR_APPND( '+)', SYN, IAT )
         CALL KPG1_ASPSY( SYN( : IAT ), '(CURVE)', STATUS )

*  Set the plotting style for the upper (positive) line.
         CALL KPG1_ASSET( 'KAPPA_DRAWSIG', 'STYLE', IPLOT, STATUS )

*  Draw the first line.
         START( DATAX ) = MEAN + STDEV*CLIP( I )
         FINISH( DATAX ) = START( DATAX )
         CALL AST_CURVE( IPLOT, START, FINISH, STATUS )

*  Draw the second line if the clipping limit is not zero.
         IF( NCLIP .EQ. 1 ) THEN

*  Ensure that any previous synonyms for AST attributes are cleared.
            CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Use Lines and Line- as synonyms for Curve.
            CALL KPG1_ASPSY( '(LINES)', '(CURVE)', STATUS )
            CALL KPG1_ASPSY( '(LINE-)', '(CURVE)', STATUS )

*  Make the synonymns "(NSIG<i>)" and "(NSIG<i>-)" equate to "(Curve)"
            SYN = '(NSIG'
            IAT = 5
            CALL CHR_PUTI( I, SYN, IAT )
            CALL CHR_APPND( ')', SYN, IAT )
            CALL KPG1_ASPSY( SYN( : IAT ), '(CURVE)', STATUS )

            IAT = IAT - 1
            CALL CHR_APPND( '-)', SYN, IAT )
            CALL KPG1_ASPSY( SYN( : IAT ), '(CURVE)', STATUS )

*  Set the plotting style for the upper (positive) line.
            CALL KPG1_ASSET( 'KAPPA_DRAWSIG', 'STYLE', IPLOT, STATUS )

*  Draw the line.
            START( DATAX ) = MEAN - STDEV*CLIP( I )
            FINISH( DATAX ) = START( DATAX )
            CALL AST_CURVE( IPLOT, START, FINISH, STATUS )

         END IF

*  Report the clipped mean and standard deviation. Report the unclipped
*  values first.
         IF( I .EQ. 1 .AND. NGOOD .GT. 0 ) THEN
            WRITE( BUFFER, '(16X,3(5X,G13.6))' ) MEAN, STDEV,
     :             STDEV / SQRT( DBLE( NGOOD ) )
            CALL MSG_OUT( 'RESULTS', BUFFER, STATUS )
         END IF

*  Now report the clipped values.
         IF( NCLIP .EQ. 1 .AND. NGOODC .GT. 0 ) THEN
            WRITE( BUFFER, '(8X,F6.3,2X,3(5X,G13.6))' )
     :             CLIP( I ), MEANC, STDEVC,
     :             STDEVC / SQRT( DBLE( NGOODC ) )
            CALL MSG_OUT( 'RESULTS', BUFFER, STATUS )
         END IF

*  Free resources
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

      END DO

*  Tidy up.
 999  CONTINUE

*  Tidy up the locators.
      IF( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
      CALL DAT_VALID( LOCI, GOTLOC, STATUS )
      IF( GOTLOC ) CALL DAT_ANNUL( LOCI, STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST Context.
      CALL AST_BEGIN( STATUS )

*  Add a context report if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DRAWSIG_ERR', 'DRAWSIG: Failed to draw N-'//
     :                 'sigma lines over an existing plot.', STATUS )
      END IF

      END
