      SUBROUTINE PRPIX( STATUS )

*+
*  Name:
*     SUBROUTINE PRPIX

*  Purpose:
*     Print extraction parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRPIX( STATUS )

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
*     Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*     Martin Clayton     16-DEC-94     IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Status:
      INTEGER STATUS        ! status return

*  Global Variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMSIST'

*  Local Variables:
      INTEGER IBKG          ! loop index

*.

*   Check.
      IF ( NOEXTP ) THEN
         CALL ERROUT( 'Error: no extraction details\\', STATUS )
         RETURN
      END IF

*   Pre-title line.
      CALL LINE_WRITS( '%p %40s\\', 'Background\\' )
      CALL LINE_WRITS( '%24s\\', 'Object\\' )
      CALL PRTBUF( STATUS )

*   Title line.
      CALL LINE_WCONT( '%p %20w\\' )
      CALL LINE_WRITS( '%12s\\', 'Left\\' )
      CALL LINE_WRITS( '%12s\\', 'Right\\' )
      CALL LINE_WRITS( '%12s\\', 'Net\\' )
      CALL LINE_WRITS( '%12s\\', 'Shift\\' )
      CALL PRTBUF( STATUS )

*   Good pixels used.
      CALL LINE_WRITS( '%p %-20s\\', 'Good pixels used\\' )
      DO IBKG = 1, NBKG
         CALL LINE_WRITI( '%12i\\', NBUSE( IBKG ) )
      END DO

      CALL LINE_WRITI( '%12i\\', NGUSE )
      CALL LINE_WRITI( '%12i\\', NCUSE )
      CALL PRTBUF( STATUS )

*   Defective pixels used.
      CALL LINE_WRITS( '%p %-20s\\', 'Bad pixels used\\' )
      DO IBKG = 1, NBKG
         CALL LINE_WRITI( '%12i\\', NBBAD( IBKG ) )
      END DO

      CALL LINE_WRITI( '%12i\\', NGBAD )
      CALL LINE_WRITI( '%12i\\', NCBAD )
      CALL PRTBUF( STATUS )

*   Unused pixels.
      CALL LINE_WRITS( '%p %-20s\\', 'Pixels not used\\' )
      DO IBKG = 1, NBKG
         CALL LINE_WRITI( '%12i\\', NBUND( IBKG ) )
      END DO

      CALL LINE_WRITI( '%12i\\', NGUND )
      CALL LINE_WRITI( '%12i\\', NCUND )
      CALL PRTBUF( STATUS )

*   Rejected pixels.
      CALL LINE_WRITS( '%p %-20s\\', 'Rejected pixels\\' )
      DO IBKG = 1, NBKG
         CALL LINE_WRITI( '%12i\\', NBREJ( IBKG ) )
      END DO

      CALL LINE_WRITI( '%12i\\', 0 )
      CALL LINE_WRITI( '%12i\\', 0 )
      CALL PRTBUF( STATUS )

*   Mean value.
      CALL LINE_WRITS( '%p %-20s\\', 'Mean value\\' )
      DO IBKG = 1, NBKG
         CALL LINE_WRITF( '%12.1f\\', ASBKG( IBKG ) )
      END DO

      CALL LINE_WRITF( '%12.1f\\', ASNET )
      CALL LINE_WRITF( '%12.3f\\', ASCEN )
      CALL PRTBUF( STATUS )

*   Error on mean.
      CALL LINE_WRITS( '%p %-20s\\', 'RMS variation\\' )
      DO IBKG = 1, NBKG
         CALL LINE_WRITF( '%12.1f\\', ADBKG( IBKG ) )
      END DO

      CALL LINE_WRITF( '%12.1f\\', ADNET )
      CALL LINE_WRITF( '%12.3f\\', ADCEN )
      CALL PRTBUF( STATUS )

*   Number of evaluations.
      CALL LINE_WRITS( '%p %-20s\\', 'Evaluations\\' )
      DO IBKG = 1, NBKG
         CALL LINE_WRITI( '%12i\\', (BKGIT + 1) * (CENIT + 1) )
      END DO

      CALL LINE_WRITI( '%12i\\', CENIT + 1 )
      CALL LINE_WRITI( '%12i\\', CENIT + 1 )
      CALL PRTBUF( STATUS )

      END
