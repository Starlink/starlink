      SUBROUTINE KPS1_CNTPN( PARAM1, PARAM2, PARAM3, IPLOT, NCONT,
     :                       CNTLEV, IGRP, STATUS )
*+
*  Name:
*     KPS1_CNTPN

*  Purpose:
*     Creates pen definitons with which to draw each contour.

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
*     PARAM1.  If no such pens are given then, then a set of pens is
*     created automatically if pen rotation is selected using PARAM2.
*     Line colour rotates if there are at least three available colours,
*     otherwise line style rotates.  If contours below the threshold
*     given by PARAM3 are to be drawn differently, then line style is
*     used to distinguish the contours unless line style has already
*     been used for rotating pens, in which case line width is used
*     instead.

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

*  Copyright:
*     Copyright (C) 1998, 2001 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1998 (DSB):
*        Original version.
*     25-JAN-2001 (DSB):
*        Add parameter PEN1 which is used to remember the last pen
*        colour used when rotating pens.  This enables rotation to
*        continue between invocations of the application.  Changed
*        NROT from 3 to 5.
*     9-FEB-2001 (DSB):
*        Modified to make the COLOUR and STYLE attribues included in the
*        returned group refer to the CURVES and STRINGS elements only.
*     2006 April 12 (MJC):
*        Remove unused variable and wrapped long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PRM_PAR'          ! VAL constants
      INCLUDE 'CTM_PAR'          ! Colour-Table Management constants
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
      CHARACTER CHR_NTH*2         ! Two-character ordinal abbreviation

*  Local Constants:
      INTEGER NROT                ! No. of pens in one rotation cycle
      PARAMETER( NROT = 5 )

      INTEGER NPGSTY              ! Number of line styles available in
      PARAMETER( NPGSTY = 5 )     ! PGPLOT

*  Local Variables:
      CHARACTER ATTR*( 80 )       ! AST attribute setting
      CHARACTER PENDEF*( GRP__SZNAM ) ! AST attribute settings for
                                  ! current pen
      INTEGER DOWN                ! Lowest colour index
      INTEGER I                   ! Current contour index
      INTEGER IAT                 ! No. of characters ina string
      INTEGER IPEN                ! Current pen number
      INTEGER IPLOTT              ! AST pointer to Plot with current pen
                                  ! set
      INTEGER J1                  ! Index at start of attribute setting
      INTEGER J2                  ! Index of comma at end of attribute
                                  ! setting
      INTEGER NC                  ! No. of characters in formatted value
      INTEGER NPEN                ! Number of explicit pen definitions
                                  ! given
      INTEGER PENHI               ! Highest pen number
      INTEGER STY1                ! PGPLOT style for contours above
                                  ! threshold
      INTEGER STY2                ! PGPLOT style for contours below
                                  ! threshold
      INTEGER UP                  ! Highest colour index
      LOGICAL BADAT               ! Was pen definition invalid?
      LOGICAL PENROT              ! Have pens been rotated?
      LOGICAL STYROT              ! Has line style been rotated?
      REAL THRESH                 ! Threshold contour value
      REAL WID1                   ! PGPLOT line width for contours above
                                  ! threshold
      REAL WID2                   ! PGPLOT line width for contours below
                                  ! threshold
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

*  Take a deep copy of the supplied Plot.  This Plot will be modify
*  using the supplied pen definitions.  A copy is used so that the
*  original plotting attributes are not changed.
         IPLOTT = AST_COPY( IPLOT, STATUS )

*  Scan through each pen
         DO IPEN = 1, NPEN

*  Get the next list of AST Attribute settings from the group.
            CALL GRP_GET( IGRP, IPEN, 1, PENDEF, STATUS )

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
         IF( PENROT .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the number of colour indices available on the choosen device.
            CALL PGQCOL( DOWN, UP )

*  If this is more than NROT (ignoring the background colour), create
*  pen definitions which rotate the pen colour indices starting with the
*  next colour index after the one used previously.
            IF( UP .GE. NROT ) THEN

*  Parameter PEN1 should have a vpath of INTERNAL in order to hide it
*  form the user.  It is used to hold the index of the last pen used, so
*  that subsequent invocations can use a different pen.  This allows pen
*  rotation to continue between invocations of CONTOUR.
               CALL PAR_GET0I( 'PEN1', IPEN, STATUS )
               IF( STATUS .EQ. PAR__NULL ) THEN
                  CALL ERR_ANNUL( STATUS )
                  IPEN = AST_GETI( IPLOT, 'COLOUR(CURVES)', STATUS )
               END IF
               IPEN = MOD( IPEN, NROT ) + 1

*  Create the pen definitions.
               NPEN = NCONT
               PENHI = MIN( NROT, MIN( CTM__RSVPN, UP ) )
               DO I = 1, NCONT
                  ATTR = 'COLOUR(CURVES)='
                  NC = 15
                  CALL CHR_PUTI( IPEN, ATTR, NC )
                  CALL CHR_APPND( ',COLOUR(STRINGS)=', ATTR, NC )
                  CALL CHR_PUTI( IPEN, ATTR, NC )
                  CALL GRP_PUT( IGRP, 1, ATTR, 0, STATUS )
                  IPEN = MOD( IPEN, NROT ) + 1
               END DO

*  Set the first pen to use next time.
               CALL PAR_PUT0I( 'PEN1', IPEN, STATUS )

*  If there are fewer than NROT colours available, create pen
*  definitions that rotate the pen styles (so long as there are enough
*  styles).
            ELSE IF( NROT .LE. NPGSTY ) THEN
               NPEN = NCONT
               STYROT = .TRUE.
               IPEN = AST_GETI( IPLOT, 'STYLE(CURVES)', STATUS )
               PENHI = MIN( IPEN + NROT - 1, NPGSTY )

               DO I = 1, NCONT
                  IF( IPEN .GT. PENHI ) IPEN = IPEN - NROT
                  ATTR = 'STYLE(CURVES)='
                  NC = 14
                  CALL CHR_PUTI( IPEN, ATTR, NC )
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

*  If pen rotation results in line style varying from contour to
*  contour, then we use line width instead of style to distinguish the
*  contours below THRESH.
            IF( STYROT ) THEN

*  Store the two line widths to use.
               WID1 = AST_GETR( IPLOT, 'WIDTH(CURVES)', STATUS )
               WID2 = 1.5*WID1

*  Do each contour.
               DO I = 1, NCONT

*  If this contour already has a pen definition in the group (produced
*  by pen rotation), append the new line width to it.
                  IF( I .LE. NPEN ) THEN
                     CALL GRP_GET( IGRP, I, 1, ATTR, STATUS )
                     IAT = CHR_LEN( ATTR )
                     CALL CHR_APPND( ',WIDTH(CURVES)=', ATTR, IAT )
                  ELSE
                     ATTR = 'WIDTH(CURVES)='
                     IAT = 14
                  END IF

*  Append the line width to the attribute string.
                  IF( CNTLEV( I ) .GE. THRESH ) THEN
                     CALL CHR_RTOC( WID1, ATTR, IAT )
                  ELSE
                     CALL CHR_RTOC( WID2, ATTR, IAT )
                  ENDIF

*  Store in the group at the correct index.
                  CALL GRP_PUT( IGRP, 1, ATTR, I, STATUS )

               END DO

*  The group now contains NCONT pen definitions.
               NPEN = NCONT

*  If line style has not been used for rotated pens, use it to
*  distinguish contours below THRESH.
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
                     CALL CHR_APPND( ',STYLE(CURVES)=', ATTR, IAT )
                  ELSE
                     ATTR = 'STYLE(CURVES)='
                     IAT = 14
                  END IF

                  IF( CNTLEV( I ) .GE. THRESH ) THEN
                     CALL CHR_PUTI( STY1, ATTR, IAT )
                  ELSE
                     CALL CHR_PUTI( STY2, ATTR, IAT )
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
