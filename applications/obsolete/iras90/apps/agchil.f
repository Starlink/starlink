      SUBROUTINE AGCHIL( IFLG, LBNM, LNNO )
*+
*  Name:
*     AGCHAX

*  Purpose:
*     Set pen number for axis labels of an NCAR (AUTOGRAPH) display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AGCHIL( IFLG, LBNM, LNNO )

*  Description:
*     This is a user version of AGCHIL to replace the one in NCAR
*     library. It is used, together with the routine IRM_STPEN, to set
*     the pen number for the title and the axis labels of an NCAR
*     (AUTOGRPH) display. It is not called directly by the user
*     program, but by AUTOGRAPH  itself, just before and just after
*     each of the labels or title. This routine will set the pen number
*     for the title and the axis labels according to the values of the
*     NCAR pen setting variables in the common block IRM_NCAR which are
*     set by IRM_STPEN.
*
*     The title refered to is the title witten by NCAR (AUTOGRAPH)
*     routines EZY, EZMY etc. which have the name of 'T' and line
*     number of 100. The axis labels have the name either 'T' or
*     'B' or 'R' or 'L'.
*
*     To use this routine, it must be linked explicitly with the user
*     program to override the default version of AGCHAX in NCAR library.

*  Arguments:
*     See section 3.25.2 of AUTOGRAPH document.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Argument Given:
      INTEGER IFLG
      INTEGER LNNO
      CHARACTER*(*) LBNM

*  Local Variables:
      INTEGER IVAL               ! Integer attribute value.
      INTEGER STATUS             ! Inherited status.

      LOGICAL LVAL               ! Logical attribute value.

*.

*  Initialise the inherited status to a good value.
      STATUS = SAI__OK

*  If the variables in the common block have been set values, use them
*  to set pen number. Otherwise do nothing.
      CALL IRM_IATTR( 'STDAT', 0, IVAL, LVAL, STATUS )
      IF ( LVAL ) THEN

*  If a informational label is to be drawn, ...
         IF ( IFLG .EQ. 0 ) THEN

*  If the label to be drawn is the title of the display,
            IF ( LBNM( : 1 ) .EQ. 'T' .AND. LNNO .EQ. 100 ) THEN
               CALL IRM_IATTR( 'TITCL', 0, IVAL, LVAL, STATUS )
               CALL GSTXCI( IVAL )

*  If the label to be draw is other axis label,
            ELSE IF ( ( LBNM( : 1 ) .EQ. 'T' .AND. LNNO .NE. 100 )
     :               .OR. LBNM( : 1 ) .EQ. 'B'
     :               .OR. LBNM( : 1 ) .EQ. 'R'
     :               .OR. LBNM( : 1 ) .EQ. 'L' ) THEN
               CALL IRM_IATTR( 'ALBCL', 0, IVAL, LVAL, STATUS )
               CALL GSTXCI( IVAL )
            END IF

*  If a informational label has just been drawn, set the GKS colour
*  index to the original setting.
         ELSE
           CALL IRM_IATTR( 'OLDTX', 0, IVAL, LVAL, STATUS )
           CALL GSTXCI( IVAL )
         END IF

      END IF

*  If an error has occurred, flush it.
      IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

      END
