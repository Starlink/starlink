      SUBROUTINE AGCHCU( IFLG, KDSH )
*+
*  Name:
*     AGCHCU

*  Purpose:
*     Set pen number for curves of an NCAR (AUTOGRAPH) display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AGCHCU( IFLG, KDSH )

*  Description:
*     This is a user version of AGCHCU to replace the one in NCAR
*     library. It is used, together with the routine IRM_STPEN, to set
*     the pen number for each curve in an NCAR display. It is not called
*     directly by a user program, byt by NCAR (AUTOGRAPH) itself, just
*     before and after each curve is draw. This routine will set the
*     pen number for each curve according to the value of NCAR pen setting
*     variables in the common block IRM_COM which are set by IRM_STPEN.
*
*     To use this routine, it must be linked explicitly with the user
*     program to override the default version in NCAR library.

*  Arguments:
*     See section 3.24.2 of AUTOGRAPH document.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER IFLG
      INTEGER KDSH

*  Local Variables:
      INTEGER COL                ! Colour index of a in-line label
      INTEGER CUVNUM             ! Curve number
      INTEGER IVAL               ! Integer attribute value.
      INTEGER PEN                ! Pen number of a curve
      INTEGER STATUS             ! Inherited status.

      LOGICAL LVAL               ! Logical attribute value.

*.

*  Initialise the inherited status to a good value.
      STATUS = SAI__OK

*  If the variables in common block has been assigned values,
*  use the data. Otherwise do nothing.
      CALL IRM_IATTR( 'STDAT', 0, IVAL, LVAL, STATUS )
      IF ( LVAL ) THEN

*  Set global status as OK
      STATUS = SAI__OK

*  If a curve is to be drawn, set the pen number for it.
         IF ( IFLG .EQ. 0 ) THEN

*  Flush out previous drawing.
            CALL SGS_FLUSH

*  Set the line type as solid for all pens if requested.
            CALL IRM_IATTR( 'SOCUR', 0, IVAL, LVAL, STATUS )
            IF ( LVAL ) CALL IRM_SOLIN( STATUS )

*  Set the pen number in a circular fashion.
            CALL IRM_IATTR( 'NCURV', 0, IVAL, LVAL, STATUS )
            CUVNUM = MOD( ABS( KDSH ) - 1, IVAL ) + 1
            CALL IRM_IATTR( 'CRPN', CUVNUM, PEN, LVAL, STATUS )
            CALL GSPLI( PEN )

*  Set the colour index for the in-line label in a curcular fashion.
            CALL IRM_IATTR( 'INCL', CUVNUM, COL, LVAL, STATUS )
            CALL GSTXCI( COL )

*  If a curve has been finished, set the line type back to original.
*  And the GKS polyline index and text colour index to the original
*  setting.
         ELSE

*  Flush out the previous drawing and reset.
            CALL SGS_FLUSH
            CALL IRM_IATTR( 'SOCUR', 0, IVAL, LVAL, STATUS )
            IF ( LVAL ) CALL IRM_ANTSO( STATUS )

            CALL IRM_IATTR( 'OLDPX', 0, IVAL, LVAL, STATUS )
            CALL GSPLI( IVAL )

            CALL IRM_IATTR( 'OLDTX', 0, IVAL, LVAL, STATUS )
            CALL GSTXCI( IVAL )

         END IF

      END IF

*  If an error has occurred, flush it.
      IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

      END
