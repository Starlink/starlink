      SUBROUTINE KPS1_MLPLB( NDISP, USE, N, X, Y, OFFSET, IPLOT, PARAM1,
     :                       PARAM2, PARAM3, PARAM4, APP, LINDX, ZMARK,
     :                       IGRP3, STATUS )
*+
*  Name:
*     KPS1_MLPLB

*  Purpose:
*     Adds curve labels and zero-point markers to the plot produced by
*     MLINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPLB( NDISP, USE, N, X, Y, OFFSET, IPLOT, PARAM1,
*                      PARAM2, PARAM3, PARAM4, APP, LINDX, ZMARK, IGRP3,
*                      STATUS )

*  Description:
*     This routine adds curve labels and zero0point markers to the plot
*     produced by MLINPLOT.  The following graphical attributes may be
*     used to control the apperance of the labels and zero-point
*     markers.
*
*        Size(Zeromark)  : Relative scale factor for the length of the
*                          zero-point markers.
*        Colour(Zeromark): Colour for the zero-point markers.
*        Width(Zeromark) : Line width for the zero-point markers.
*        Style(Zeromark) : Line style for the zero-point markers.
*
*        Size(Labels)    : Relative text size for the curve labels.
*        Colour(Labels)  : Colour for the curve labels.
*        Width(Labels)   : Line width for the curve labels.
*        Style(Labels)   : Line style for the curve labels.
*        Font(Labels)    : Line style for the curve labels.
*
*        LabPos(Left)    : Horizontal position of the left-curve label.
*        LabPos(Right)   : Horizontal position of the right-curve
*                          label.
*
*     Minimum abbreviation for "ZeroMark" Is "Zer".
*     Minimum abbreviation for "Labels" Is "Lab".
*     Minimum abbreviation for "Left" Is "L".
*     Minimum abbreviation for "Right" Is "R".
*
*     "LabPos" without any qualifier refers to the Left label.  LabPos
*     values are floating point, with 0.0 meaning the left edge of the
*     plotting area, and 1.0 the right edge.

*  Arguments:
*     NDISP = INTEGER (Given)
*        Number of curves plotted.
*     USE( NDISP ) = LOGICAL (Given)
*        Set .FALSE. if the corresponding curve was not drawn.
*     N = INTEGER (Given)
*        Number of points to be plotted.
*     X( N, NDISP ) = DOUBLE PRECISION (Given)
*        The X value at each point in each curve, in PGPLOT world
*        co-ordinate (i.e. millimetres from the bottom=left corner
*        of the view surface).
*     Y( N, NDISP ) = DOUBLE PRECISION (Given)
*        The Y value at each point, in PGPLOT world co-ordinate (i.e.
*        millimetres from the bottom-left corner of the view surface).
*     OFFSET( NDISP ) = DOUBLE PRECISION (Given)
*        The zero-point for each curve, in PGPLOT world co-ordinate
*        (i.e. millimetres from the bottom of the view surface).
*     IPLOT = INTEGER (Given)
*        An AST Plot which can be used to do the drawing.  The Base
*        Frame should be GRAPHICS co-ordinates (millimetres from the
*        bottom-left corner of the PGPLOT view surface).
*     PARAM1 = CHARACTER * ( * ) (Given)
*        The name of the style parameter to be used when obtaining the
*        plotting style.
*     PARAM2 = CHARACTER * ( * ) (Given)
*        The name of the parameter to be used when obtaining the
*        individual pen defintions for each curve.
*     PARAM3 = CHARACTER * ( * ) (Given)
*        The name of the parameter to be used when obtaining a group of
*        curve labels.
*     PARAM4 = CHARACTER * ( * ) (Given)
*        The name of the parameter to be used when determining whether
*        or not to display curve labels at the left hand end of each
*        curve.
*     APP = CHARACTER * ( * ) (Given)
*        The name of the calling application in the form
*        <package>_<application> (e.g. "KAPPA_DISPLAY").
*     LINDX( NDISP ) = INTEGER (Given)
*        The NDF pixel index of each line of plotted data.
*     ZMARK = LOGICAL (Given)
*        Should zero-point markers and a right hand label be drawn?
*     IGRP3 = INTEGER (Returned)
*        A group holding the curve labels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-AUG-1999 (DSB):
*        Original version.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
*     2006 June 23 (MJC):
*        Delete labels' group identifier during tidying.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function
                                 ! declarations
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      INTEGER NDISP
      LOGICAL USE( NDISP )
      INTEGER N
      DOUBLE PRECISION X( N, NDISP )
      DOUBLE PRECISION Y( N, NDISP )
      DOUBLE PRECISION OFFSET( NDISP )
      INTEGER IPLOT
      CHARACTER PARAM1*(*)
      CHARACTER PARAM2*(*)
      CHARACTER PARAM3*(*)
      CHARACTER PARAM4*(*)
      CHARACTER APP*(*)
      INTEGER LINDX( NDISP )
      LOGICAL ZMARK

*  Arguments Returned:
      INTEGER IGRP3

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NAT                ! No. of AST attributes used
      PARAMETER ( NAT = 11 )

*  Local Variables:
      CHARACTER ATTNAM( NAT )*20 ! Names of attributes used by this
                                 ! routine
      CHARACTER LAB*( GRP__SZNAM ) ! A curve label
      CHARACTER PENDEF*( GRP__SZNAM )! AST attribute settings for
                                 ! current pen
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      DOUBLE PRECISION DIST      ! X distance between data sample and
                                 ! label
      DOUBLE PRECISION GAP       ! Gap between left edge and label
                                 ! centre
      DOUBLE PRECISION MNDIST    ! Minimum X distance found so far
      DOUBLE PRECISION POS( 2 )  ! Label position
      DOUBLE PRECISION YMAX      ! Maximum Y value in plot
      DOUBLE PRECISION YMIN      ! Minimum Y value in plot
      INTEGER I                  ! Position index
      INTEGER IAT                ! Number of characters in string
      INTEGER ICURR              ! Index of original Plot Currnt Frame
      INTEGER IGRP1              ! GRP identifier for pen definitions
      INTEGER IGRP2              ! GRP identifier for label group
      INTEGER IPEN               ! Index of current pen definition
      INTEGER J1                 ! Index at start of attribute setting
      INTEGER J2                 ! Index of comma at end of attribute
                                 ! setting
      INTEGER K                  ! Line index
      INTEGER NLAB               ! Number of labels supplied in IGRP2
      INTEGER NPEN               ! Number of pen defintions supplied in
                                 ! IGRP1
      LOGICAL BADAT              ! Was attribute setting string invalid?
      LOGICAL LINLAB             ! Include in-line labels?
      REAL ATT0( NAT )           ! Original Plot attributes on entry
      REAL ATT1( NAT )           ! Plot attributes supplied by PARAM1
      REAL NL                    ! Nominal length of zero-point markers
      REAL OFF                   ! Curve offset
      REAL UP( 2 )               ! Up vector for text
      REAL XL                    ! Left X limit of PGPLOT window
      REAL XR                    ! Right X limit of PGPLOT window
      REAL YB                    ! Bottom Y limit of PGPLOT window
      REAL YT                    ! Top Y limit of PGPLOT window
      REAL ZEROSZ                ! Length of zero-point marker

      DATA ATTNAM /'COLOUR(AXES)',  'WIDTH(AXES)',     'STYLE(AXES)',
     :             'MAJTICKLEN',    'COLOUR(TEXTLAB)', 'WIDTH(TEXTLAB)',
     :             'FONT(TEXTLAB)', 'STYLE(TEXTLAB)',  'SIZE(TEXTLAB)',
     :             'TEXTLABGAP(1)', 'TEXTLABGAP(2)' /

      DATA UP / 0.0, 1.0 /
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Save the index of the Plot's Current Frame, and then set it to the
*  Base Frame.  This is done so that we can use AST graphics routines to
*  draw in GRAPHICS (Base Frame) co-ordinates.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )
      CALL AST_SETI( IPLOT, 'CURRENT', AST__BASE, STATUS )

*  Get the bounds of the current PGPLOT window.
      CALL PGQWIN( XL, XR, YB, YT )
      YMAX = MAX( YB, YT )
      YMIN = MIN( YB, YT )

*  Find the nominal length of a zero-point marker.
      NL = 0.02*( XR - XL )

*  See if in-line labels are to be produced.
      CALL PAR_GET0L( PARAM4, LINLAB, STATUS )

*  Create a GRP group to hold supplied curve labels.
      CALL GRP_NEW( 'Curve labels', IGRP2, STATUS )

*  Create a GRP group to hold returned curve labels.
      CALL GRP_NEW( 'Curve labels', IGRP3, STATUS )

*  Get a group of labels from the environment, storing them in the group
*  created above.  Annul the error if no labels are supplied.
      CALL KPG1_GTGRP( PARAM3, IGRP2, NLAB, STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Obtain the plotting attributes (colour, width, font, size, style)
*  to be used when drawing the offset lines and curve labels.
*  =================================================================

*  Save the plotting attributes for the AST graphical elements which may
*  be changed by this routine, so that we can reinstate before we
*  return.
      DO I = 1, NAT
         ATT0( I ) = AST_GETR( IPLOT, ATTNAM( I ), STATUS )
         CALL AST_CLEAR( IPLOT, ATTNAM( I ), STATUS )
      END DO

*  Establish synonyms for AST graphical element names to be recognised
*  during the following call to KPG1_ASSET.  The length of the
*  zero-point markers is specified by "Size(zeromark)" which is a
*  synonym for "MajTickLen".  MajTickLen is used because it can take
*  both negative and positive values.  We first translate any explicit
*  MajTickLen settings into settings for the illegal attribute name
*  DUMMY.  This prevents MajTickLen settings from changing the length
*  of the zero-point markers.
      CALL KPG1_ASPSY( 'MAJTICKLEN', 'DUMMY', STATUS )

*  Now translate any Size(zeromark) settings into settings for
*  MajTickLen.  Note, since this is done after the previous translation,
*  these MajTickLen settings will not get translated into settings for
*  DUMMY.
      CALL KPG1_ASPSY( 'SIZE(ZER*OMARK)', 'MAJTICKLEN', STATUS )

*  The other attributes for the zero-point markers are set equal to
*  those of AST "axes".  We first translate any explicitly supplied
*  Axes values into Dummy values so that they do not effect the
*  appearance of the zero-point markers.
      CALL KPG1_ASPSY( '(AXES)', '(DUMMY)', STATUS )
      CALL KPG1_ASPSY( '(ZER*OMARK)', '(AXES)', STATUS )

*  The attributes for the curve labels are set equal to those of AST
*  "strings".
      CALL KPG1_ASPSY( '(LAB*ELS)', '(STRINGS)', STATUS )

*  The horizontal position of each label is controlled by the TextLabGap
*  attributes (left by the axis-1 value, right by the axis-2 value).
*  First ignore any explicit settings for these attributes.
      CALL KPG1_ASPSY( 'TEXTLABGAP', 'DUMMY', STATUS )

*  Now translate "LabPos(L*eft)" into "TextLabGap(1), and
*  "LabPos(R*ight)" into "TextLabGap(2).  "LabPos" is made equivalent to
*  "LabPos(Left)".
      CALL KPG1_ASPSY( 'LABPOS', 'LABPOS(LEFT)', STATUS )
      CALL KPG1_ASPSY( 'LABPOS(L*EFT)', 'TEXTLABGAP(1)', STATUS )
      CALL KPG1_ASPSY( 'LABPOS(R*IGHT)', 'TEXTLABGAP(2)', STATUS )

*  Set the attributes of the supplied Plot using the supplied parameter
*  to access a plotting style.  The above synonyms are recognised and
*  translated into the corresponding AST attribute names.  Colour names
*  are also translated into colour indices.
      CALL KPG1_ASSET( APP, PARAM1, IPLOT, STATUS )

*  Save the attributes to be used if no pen definitions have been
*  supplied in IGRP1.
      DO I = 1, NAT
         ATT1( I ) = AST_GETR( IPLOT, ATTNAM( I ), STATUS )
      END DO

*  Get the drawing attributes to use for each curve.
      CALL KPS1_MLPPN( PARAM2, IGRP1, STATUS )

*  Store the number of pen definitions supplied in the GRP group.  These
*  definitions will override the attributes set by the previous call to
*  KPG1_ASSET.
      IF( IGRP1 .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP1, NPEN, STATUS )
      ELSE
         NPEN = 0
      END IF

*  Initialise the pen number.
      IPEN = 0

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each used curve.
*  ===========================
      DO K = 1, NDISP
         LAB = ' '

*  If any individual pen definitions were supplied...
         IF( NPEN .GT. 0 ) THEN

*  Increment the index of the next pen to use, cycling back to the start
*  when the end is reached.
            IPEN = IPEN + 1
            IF( IPEN .GT. NPEN ) IPEN = 1

         END IF

*  Continue if this curve is not to be drawn.
         IF( .NOT. USE( K ) ) GO TO 20

*  Store a single precision copy of the offset for this curve.
         OFF = REAL( OFFSET( K ) )

*  If different pens are being used, set the required attributes in the
*  Plot to produce the pen style for this curve.
         IF( NPEN .GT. 0 ) THEN

*  Get the next list of AST Attribute settings from the group.
            CALL GRP_GET( IGRP1, IPEN, 1, PENDEF, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each comma-delimited attribute in the definitions,
*  translating colour names and any defined synonyms, and storing it in
*  the Plot.
            IF( PENDEF .NE. ' ' ) THEN
               J1 = 1
               DO WHILE( J1 .LE. GRP__SZNAM )
                  J2 = J1
                  CALL CHR_FIND( PENDEF, ',', .TRUE., J2 )
                  CALL KPG1_ASSTS( PENDEF( J1 : J2 - 1 ), .TRUE.,
     :                             .TRUE., IPLOT, BADAT, STATUS )
                  J1 = J2 + 1

*  Annul any error which occurred while setting the pen.
                  IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

               END DO

            END IF

         END IF

*  Start a PGPLOT buffering context.
         CALL PGBBUF

*  First draw the required zero-point markers.
*  ===========================================
         IF( ZMARK ) THEN

*  Set up the correct PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'AXES', .TRUE., ATTR, STATUS )

*  Get the size of the markers.  This is scaled by the "size(zero)"
*  attribute which is a synonym for "size(curves)".
            ZEROSZ = AST_GETR( IPLOT, 'MAJTICKLEN', STATUS )*NL

*  Draw the left marker if required.
            CALL PGMOVE( XL, OFF )
            CALL PGDRAW( XL + ZEROSZ, OFF )

*  Draw the right marker if required.
            CALL PGMOVE( XR, OFF )
            CALL PGDRAW( XR - ZEROSZ, OFF )

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'AXES', .FALSE., ATTR, STATUS )

         END IF

*  Now draw the required labels.
*  =============================
*  Get the label.  If a group has been supplied, use the supplied label.
         IF( K .LE. NLAB ) THEN
            CALL GRP_GET( IGRP2, K, 1, LAB, STATUS )

*  Otherwise, format the line pixel index, preceeding it by a "#".
*  Store the label in the group.
         ELSE
            LAB = '#'
            IAT = 1
            CALL CHR_PUTI( LINDX( K ), LAB, IAT )
         END IF

*  Only proceed if the label is not blank, and labels are required.
         IF( LAB .NE. ' ' .AND. LINLAB .OR. ZMARK ) THEN

*  Set up the correct PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'STRINGS', .TRUE., ATTR, STATUS )

*  First do the left label.
            IF( LINLAB ) THEN

*  Get the horizontal position of the label (0.0 -> left edge, 1.0 ->
*  right edge).
               GAP = AST_GETD( IPLOT, 'TEXTLABGAP(1)', STATUS )
               POS( 1 ) = XL * ( 1.0 - GAP ) + XR * GAP
               POS( 2 ) = AST__BAD

*  Find the X data value which is closest to this X position, and has a
*  good Y data value, and note the Y data value.
               MNDIST = VAL__MAXD
               DO I = 1, N
                  DIST = ABS( X( I, K ) - POS( 1 ) )
                  IF( DIST .LT. MNDIST ) THEN
                     IF( Y( I, K ) .NE. AST__BAD ) THEN
                        MNDIST = DIST
                        POS( 2 ) = Y( I, K )
                     END IF
                  END IF
               END DO

*  Draw the label.
               CALL AST_TEXT( IPLOT, LAB, POS, UP, 'CL', STATUS )

            END IF

*  Now do the right label, so long as it is not above or below the plot.
            IF( ZMARK .AND. OFFSET( K ) .GE. YMIN .AND.
     :                      OFFSET( K ) .LE. YMAX ) THEN

*  Get the horizontal position of the label (0.0 -> left edge, 1.0 ->
*  right edge).
               GAP = AST_GETD( IPLOT, 'TEXTLABGAP(2)', STATUS )
               POS( 1 ) = XL * ( 1.0 - GAP ) + XR * GAP
               POS( 2 ) = OFFSET( K )

*  Draw the label, left justified at the above position.
               CALL AST_TEXT( IPLOT, LAB, POS, UP, 'CL', STATUS )

            END IF

*  Re-instate the original PGPLOT attributes.
            CALL KPG1_PGSTY( IPLOT, 'STRINGS', .FALSE., ATTR, STATUS )
         END IF

*  End the PGPLOT buffering context.
         CALL PGEBUF

*  If separate pen definitions are being used, re-instate the plotting
*  attributes specified by the parameter PARAM1.
         IF( NPEN .GT. 0 ) THEN
            DO I = 1, NAT
               CALL AST_SETR( IPLOT, ATTNAM( I ), ATT1( I ), STATUS )
            END DO
         END IF

*  Jump here to continue with the next curve.
 20      CONTINUE

*  Return the used label in IGRP3.
         CALL GRP_PUT( IGRP3, 1, LAB, 0, STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  Tidy up.
*  ========

 999  CONTINUE

*  Delete any group holding pen definitions or labels.
      IF( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )
      IF( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )

*  Release resources used to store the synonyms.
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Re-instate the original plotting attributes for the AST graphical elements
*  which may have been changed by this routine.
      DO I = 1, NAT
         CALL AST_SETR( IPLOT, ATTNAM( I ), ATT0( I ), STATUS )
      END DO

*  Re-instate the original Plot Current Frame.
      CALL AST_SETI( IPLOT, 'CURRENT', ICURR, STATUS )

      END
