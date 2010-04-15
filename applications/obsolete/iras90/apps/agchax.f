      SUBROUTINE AGCHAX( IFLG, IAXS, IPRT, VILS )
*+
*  Name:
*     AGCHAX

*  Purpose:
*     Set pen number for axes of an NCAR (AUTOGRAPH) display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AGCHAX( IFLG, IAXS, IPRT, VILS )

*  Description:
*     This is a user version of AGCHAX to replace the one in NCAR
*     library. It is used, together with the routine IRM_STPEN, to set
*     the pen number for the axis lines, tick marks and numeric labels
*     of an NCAR (AUTOGRPH) display. It is not called directly by the
*     user program, but by AUTOGRAPH itself, just before and just after
*     each of the objects making up an axis is drawn. This routine will
*     set the pen number for each axis portion according to the values
*     of the NCAR pen setting variables in the common block IRM_COM
*     which are set by IRM_STPEN.

*     To use this routine, it must be linked explicitly with the user
*     program to override the default version of AGCHAX in NCAR library.

*  Arguments:
*     See section 3.23.2 of AUTOGRAPH document.

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
      INTEGER IAXS
      INTEGER IPRT
      REAL VILS

*  Local Variables:
      INTEGER IVAL               ! Integer attribute value.
      INTEGER STATUS             ! Inherited status.

      LOGICAL LVAL               ! Logical attribute value.

*.

*  Initialise the inherited status to a good value.
      STATUS = SAI__OK

*  If the variable have been set values, use them to set pen number.
*  Otherwise do nothing.
      CALL IRM_IATTR( 'STDAT', 0, IVAL, LVAL, STATUS )
      IF ( LVAL ) THEN

*  If an object is to be drawn, set the pen.
         IF ( IFLG .EQ. 0 ) THEN

*  Flush out the previous drawing.
            CALL SGS_FLUSH

*  If it is an axis line, set pen for the axis lines.
            IF ( IPRT .EQ. 1 ) THEN
               CALL IRM_IATTR( 'AXPN', 0, IVAL, LVAL, STATUS )
               CALL GSPLI( IVAL )

*  Else if the object to be drawn is a tick mark,
*  set pen for the tick marks.
            ELSE IF ( IPRT .EQ. 2 .OR. IPRT .EQ. 3 ) THEN
               CALL IRM_IATTR( 'TKPN', 0, IVAL, LVAL, STATUS )
               CALL GSPLI( IVAL )

*  Else if the object to be drawn is a numeric label,
*  set colour for the numeric labels.
            ELSE IF ( IPRT .EQ. 4 .OR. IPRT .EQ. 5 ) THEN
               CALL IRM_IATTR( 'NLBCL', 0, IVAL, LVAL, STATUS )
               CALL GSTXCI( IVAL )
            END IF

*  If an object has just been drawn, reset the pen number to the
*  original setting.
         ELSE

*  Flush out the previous drawing.
            CALL SGS_FLUSH

            IF ( IPRT .LE. 3 .AND. IPRT .GE. 1 ) THEN
               CALL IRM_IATTR( 'OLDPX', 0, IVAL, LVAL, STATUS )
               CALL GSPLI( IVAL )

            ELSE IF ( IPRT .LE. 5 .AND. IPRT .GE. 4 ) THEN
               CALL IRM_IATTR( 'OLDTX', 0, IVAL, LVAL, STATUS )
               CALL GSTXCI( IVAL )

            END IF

         END IF

      END IF

*  If an error has occurred, flush it.
      IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

      END
