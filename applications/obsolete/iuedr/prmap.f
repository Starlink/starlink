      SUBROUTINE PRMAP( STATUS )
*+
*  Name:
*     SUBROUTINE PRMAP

*  Purpose:
*     Print mapped spectrum information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS        ! Global status.

*  Global Variables:
      INCLUDE 'CMCOMB'
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Avoid saying nothing.
      IF ( NOCOMB .OR. NCOMB.LE.0 ) THEN
         GO TO 999
      END IF

*  Banner.
      CALL LINE_WCONT( '%p%2w Mean Spectrum:\\' )
      CALL PRTBUF( STATUS )

*  Wavelength grid
      CALL LINE_WRITF( '%p%4w Wavelengths from %.3f\\', XCOMB( 1 ) )
      CALL LINE_WRITF( ' to %.3f\\', XCOMB( NCOMB ) )
      CALL LINE_WRITF( ' in steps of %.3f (A).\\', DXCOMB )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITI('%p%4w Contains %i points.\\', NCOMB)
      CALL PRTBUF( STATUS )

*  End
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

 999  CONTINUE

      END
