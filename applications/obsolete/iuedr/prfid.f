      SUBROUTINE PRFID( STATUS )

*+
*  Name:
*     SUBROUTINE PRFID

*  Purpose:
*     Print fiducial position data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRFID( STATUS )

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
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFIDS'
      INCLUDE 'CMFIDT'

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Avoid saying nothing
      IF ( NOFIDS ) THEN
         GO TO 999
      END IF

*   Banner.
      CALL LINE_WCONT( '%p%2w Fiducials:\\' )
      CALL PRTBUF( STATUS )

*   Type.
      CALL LINE_WRITS( '%p%4w Type is ''%s''.\\', FIDSTP )
      CALL PRTBUF( STATUS )

*   Source.
      CALL LINE_WRITS( '%p%4w Source is ''%s''.\\', FIDSID )
      CALL PRTBUF( STATUS )

*   Grid.
      CALL LINE_WRITI( '%p%4w (X,Y) grid is size (%i,\\', NFIDX )
      CALL LINE_WRITI( '%i)\\', NFIDY )
      CALL PRTBUF( STATUS )

*   Size.
      CALL LINE_WRITF(
     :     '%p%4w Fiducials are square with side %g pixels.\\',
     :     2.0 * FIDHW )
      CALL PRTBUF( STATUS )

*   THDA and whether derivatives.
      CALL LINE_WRITF( '%p%4w Standard THDA=%g\\', FIDT0 )
      IF ( NOFIDT ) THEN
         CALL LINE_WCONT( ', with no THDA derivatives.\\' )

      ELSE
         CALL LINE_WCONT( ', with THDA derivatives.\\' )
      END IF
      CALL PRTBUF( STATUS )

*   End.
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

 999  CONTINUE
      END
