      SUBROUTINE PRHAL( STATUS )

*+
*  Name:
*     SUBROUTINE PRHAL

*  Purpose:
*     Print order overlap correction data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRHAL( STATUS )

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

*  External References:
      LOGICAL STR_SIMLR     ! caseless string equality

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMHAL'

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Avoid non-HIRES and when undefined.
      IF ( .NOT. STR_SIMLR( 'HIRES\\', RESOL ) ) THEN
         GO TO 999

      ELSE IF ( NOHAL ) THEN
         GO TO 999
      END IF

*   Banner.
      CALL LINE_WCONT( '%p%2w Order Overlap:\\' )
      CALL PRTBUF( STATUS )

*   Type.
      CALL LINE_WRITS( '%p%4w Type is ''%s''.\\', HALTP )
      CALL PRTBUF( STATUS )

*   Details.
      CALL LINE_WCONT( '%p%4w Correction is semi-empirical.\\' )
      CALL PRTBUF( STATUS )

*   C(W).
      CALL LINE_WCONT(
     :      '%p%4w Correction factor, C(W), is linear function:\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITF( '%p%6w C(%6.1f)=\\', HALWC )
      CALL LINE_WRITF( '%6.3f\\', HALC )
      CALL PRTBUF( STATUS )
      CALL LINE_WRITF( '%p%6w C(%6.1f)=\\', HALW0 )
      CALL LINE_WRITF( '%6.3f\\', 0.0 )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT( '%p%4w end.\\' )
      CALL PRTBUF( STATUS )
      CALL LINE_WCONT(
     :          '%p%4w Corrections folded with triangle function,\\' )
      CALL LINE_WRITF( ' FWHM %g (pixels).\\', HALAV )
      CALL PRTBUF( STATUS )

*   End.
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

 999  CONTINUE
      END
