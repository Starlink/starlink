      SUBROUTINE KPS1_CNTPN( PARAM1, PARAM2, PARAM3, IPLOT, NCONT, 
     :                       CNTLEV, IGRP, STATUS )
*+
*  Name:
*     KPS1_CNTPN

*  Purpose:
*     Create pen definitons with which to draw each contour.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNTPN( PARAM1, PARAM2, PARAM3, IPLOT, NCONT, CNTLEV, 
*                      IGRP, STATUS )

*  Description:
*     This routine returns a group holding AST attribute settings which
*     define the appearance required for each contour drawn by CONTOUR.
*
*     Priority is given to pens specified explicitly using parameter
*     PARAM1. If no such pens are given then, then a set of pens is
*     created automatically if pen rotation is selected using PARAM2.
*     Line colour rotates if there are at least 3 available colours,
*     otherwise line style rotates. If contours below the threshold given
*     by PARAM3 are to be drawn differently, then line style is use to
*     distinguish the contours unless line style has already been used for
*     rotating pens, in which case line width is used instead.

*  Arguments:
*     PARAM1 = CHARACTER * ( * ) (Given)
*        The parameter to use to get the user-specified pens for each
*        contour (should be 'PENS').
*     PARAM2 = CHARACTER * ( * ) (Given)
*        The parameter to use to see if the user wants to use
*        automatically rotating pens (should be 'PENROT').
*     PARAM3 = CHARACTER * ( * ) (Given)
*        The parameter to use to see if contours below a threshold value
*        are to be drawn differently (should be 'DASHED').
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot which is to be used to draw the
*        contours.
*     NCONT = INTEGER (Given)
*        The number of contours.
*     CNTLEV( NCONT ) = REAL (Given)
*        The contour levels.
*     IGRP = INTEGER (Returned)
*        A GRP identifier for a group holding the AST attribute settings
*        defining each pen. The group contains one element for each
*        contour level. Each element holds a comma separated list of 
*        attribute settings for an AST Plot.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'CTM_PAR'          ! KAPPA Colour Table Management constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM1*(*)
      CHARACTER PARAM2*(*)
      CHARACTER PARAM3*(*)
      INTEGER IPLOT
      INTEGER NCONT
      REAL CNTLEV( NCONT )

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS              ! Global status

*  External References:
      INTEGER CHR_LEN             ! Significant length of a string
      CHARACTER CHR_NTH*2         ! Two character ordinal abbreviation

*  Local Constants:
      INTEGER NROT                ! No. of pens in one rotation cycle
      PARAMETER( NROT = 3 )

      INTEGER NPGSTY              ! No. of line styles available in PGPLOT
      PARAMETER( NPGSTY = 5 )

*  Local Variables:
      CHARACTER ATTR*80           ! AST attribute setting
      CHARACTER ENTITY*255        ! AST attribute settings for current pen
      CHARACTER PENDEF*(GRP__SZNAM)! AST attribute settings for current pen
      INTEGER DOWN                ! Lowest colourt index
      INTEGER I                   ! Current contour index
      INTEGER IAT                 ! No. of characters ina string
      INTEGER IPEN                ! Current pen number
      INTEGER IPLOTT              ! AST pointer to Plot with current pen set
      INTEGER J1                  ! Index at start of attribute setting
      INTEGER J2                  ! Index of comma at end of attribute setting
      INTEGER NC                  ! No. of characters in formatted value
      INTEGER NPEN                ! No. of explicit pen definitions given
      INTEGER PENHI               ! Highest pen number
      INTEGER STY1                ! PGPLOT style for contours above threshold
      INTEGER STY2                ! PGPLOT style for contours below threshold
      INTEGER UP                  ! Highest colour index
      LOGICAL BADAT               ! Was pen definition invalid?
      LOGICAL PENROT              ! Have pens been rotated?
      LOGICAL STYROT              ! Has line style been rotated?
      REAL THRESH                 ! Threshold contour value
      REAL WID1                   ! PGPLOT line width for contours above threshold
      REAL WID2                   ! PGPLOT line width for contours below threshold
*.

*  Initialise.
      IGRP = GRP__NOID

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new group to contain the pen definitions.
      CALL GRP_NEW( 'Pen definitions', IGRP, STATUS )

*  Use a semicolon as the separator character because AST required
*  the individual attribute settings within the pen definition to be
*  separated by commas.
      CALL GRP_SETCC( IGRP, 'DELIMITER', ';', STATUS ) 

*  Get a group of strings holding pen definitions from the environment.
      CALL KPG1_GTGRP( PARAM1, IGRP, NPEN, STATUS )

*  Annul the error if a null value was given.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NPEN = 0
      END IF

*  If some pen definitions were supplied, check them...
      IF( NPEN .GT. 0 ) THEN

*  Take a deep copy of the supplied Plot. This Plot will be modify using the
*  supplied pen definitions. A copy is used so that the original plotting
*  attributes are not changed.
         IPLOTT = AST_COPY( IPLOT, STATUS )

*  Scan through each pen 
         DO IPEN = 1, NPEN 

*  Get the next list of AST Attribute settings from the group. 
            CALL GRP_GET( IGRP, IPEN, 1, PENDEF, STATUS )

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each comma-delimited attribute in the definitions, translating 
*  colour names and any defined synonyms, and storing it in the Plot.
            IF( PENDEF .NE. ' ' ) THEN
               J1 = 1
               DO WHILE( J1 .LE. GRP__SZNAM )
                  J2 = J1
                  CALL CHR_FIND( PENDEF, ',', .TRUE., J2 )
                  CALL KPG1_ASSTS( PENDEF( J1 : J2 - 1 ), .TRUE.,
     :                             .TRUE., IPLOTT, BADAT, STATUS )
                  J1 = J2 + 1
               END DO

*  Issue a context message if anything went wrong setting the pen.
               IF( STATUS .NE. SAI__OK .AND. BADAT ) THEN

                  CALL MSG_SETI( 'I', IPEN )
                  CALL MSG_SETC( 'I', CHR_NTH( IPEN ) )               
                  CALL ERR_REP( 'KPS1_CNTPN_1', 'Invalid pen '//
     :                         'definition supplied for the ^I '//
     :                         'contour level.', STATUS )

                  GO TO 999

               END IF

            END IF

         END DO

*  If no pen definitions were supplied...
      ELSE

*  Indicate that pen rotation is not based on changing line styles.
         STYROT = .FALSE.

*  See if pen rotation is required.
         CALL PAR_GTD0L( PARAM2, .FALSE., .TRUE., PENROT, STATUS )

*  Abort if an error occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  If so...
         IF( PENROT ) THEN

*  Get the number of colour indices available on the choosen device.
            CALL PGQCOL( DOWN, UP )

*  If this is more than NROT (ignoring the background colour), create pen 
*  definitions which rotate the pen colour indices starting with the 
*  colour index in the Plot.
            IF( UP .GE. NROT ) THEN
               NPEN = NCONT
               IPEN = AST_GETI( IPLOT, 'COLOUR(CURVES)', STATUS )
               PENHI = MIN( IPEN + NROT - 1, MIN( CTM__RSVPN, UP ) )

               DO I = 1, NCONT
                  IF( IPEN .GT. PENHI ) IPEN = IPEN - NROT
                  ATTR = 'COLOUR='
                  CALL CHR_ITOC( IPEN, ATTR( 8 : ), NC )
                  CALL GRP_PUT( IGRP, 1, ATTR, 0, STATUS )
                  IPEN = IPEN + 1
               END DO

*  If there are fewer than NROT colours available, create pen definitions 
*  which rotate the pen styles (so long as there are enough styles).
            ELSE IF( NROT .LE. NPGSTY ) THEN
               NPEN = NCONT
               STYROT = .TRUE.
               IPEN = AST_GETI( IPLOT, 'STYLE(CURVES)', STATUS )
               PENHI = MIN( IPEN + NROT - 1, NPGSTY )

               DO I = 1, NCONT
                  IF( IPEN .GT. PENHI ) IPEN = IPEN - NROT
                  ATTR = 'STYLE='
                  CALL CHR_ITOC( IPEN, ATTR( 7 : ), NC )
                  CALL GRP_PUT( IGRP, 1, ATTR, 0, STATUS )
                  IPEN = IPEN + 1
               END DO

            END IF

         END IF

*  See if contours below a threshold value are to be drawn differently.
*  If not, annul the error.
         CALL PAR_GET0R( PARAM3, THRESH, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

*  If so...
         ELSE

*  If pen rotation results in line style varying from contour to contour,
*  then we use line width instead of style to distinguish the contours below
*  THRESH.
            IF( STYROT ) THEN

*  Store the two line widths to use.
               WID1 = AST_GETR( IPLOT, 'WIDTH(CURVES)', STATUS )
               WID2 = 1.5*WID1

*  Do each contour.
               DO I = 1, NCONT

*  If this contour already has a pen definition in the group (produced by
*  pen rotation), append the new line width to it.
                  IF( I .LE. NPEN ) THEN
                     CALL GRP_GET( IGRP, I, 1, ATTR, STATUS )
                     IAT = CHR_LEN( ATTR )
                     CALL CHR_APPND( ',WIDTH=', ATTR, IAT )
                     IAT = IAT + 1
                  ELSE
                     ATTR = 'WIDTH='
                     IAT = 7
                  END IF

*  Append the line width to the attribute string.
                  IF( CNTLEV( I ) .GE. THRESH ) THEN
                     CALL CHR_RTOC( WID1, ATTR( IAT : ), NC )
                  ELSE
                     CALL CHR_RTOC( WID2, ATTR( IAT : ), NC )
                  ENDIF

*  Store in the group at the correct index.
                  CALL GRP_PUT( IGRP, 1, ATTR, I, STATUS )

               END DO

*  The group now contains NCONT pen definitions.
               NPEN = NCONT

*  If line style has not been used for rotated pens, use it to distinguish
*  contours below THRESH.
            ELSE
               STY1 = AST_GETI( IPLOT, 'STYLE(CURVES)', STATUS )
               IF( STY1 .EQ. 1 ) THEN
                  STY2 = 2
               ELSE
                  STY2 = 1
               END IF

               DO I = 1, NCONT

                  IF( I .LE. NPEN ) THEN
                     CALL GRP_GET( IGRP, I, 1, ATTR, STATUS )
                     IAT = CHR_LEN( ATTR )
                     CALL CHR_APPND( ',STYLE=', ATTR, IAT )
                     IAT = IAT + 1
                  ELSE
                     ATTR = 'STYLE='
                     IAT = 7
                  END IF

                  IF( CNTLEV( I ) .GE. THRESH ) THEN
                     CALL CHR_ITOC( STY1, ATTR( IAT : ), NC )
                  ELSE
                     CALL CHR_ITOC( STY2, ATTR( IAT : ), NC )
                  ENDIF

                  CALL GRP_PUT( IGRP, 1, ATTR, I, STATUS )

               END DO

*  The group now contains NCONT pen definitions.
               NPEN = NCONT

            END IF

         END IF

*  Delete the returned group if it contains no pen definitions.
         IF( NPEN .EQ. 0 ) CALL GRP_DELET( IGRP, STATUS )

      END IF

 999  CONTINUE

*  Delete the group holding pen definitions if an error has occurred.
      IF( STATUS .NE. SAI__OK ) CALL GRP_DELET( IGRP, STATUS )

      END
