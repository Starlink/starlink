      SUBROUTINE PRIMG( STATUS )

*+
*  Name:
*     SUBROUTINE PRIMG

*  Purpose:
*     The available information used by STAK is printed.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRIMG( STATUS )

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
*     Paul Rees          05-OCT-88     IUEDR Vn. 2.0
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
      INCLUDE 'CMFACE'
      INCLUDE 'CMDATA'

*  Local Variables:
      INTEGER IL            ! loop index
      INTEGER NPIXEL        ! pixel count

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check against doing nothing.
      IF ( NODATA .AND. NOROT .AND. NOFACE ) THEN
         GO TO 999
      END IF

*   Banner.
      CALL LINE_WCONT( '%p%2w Image and Faceplate:\\' )
      CALL PRTBUF( STATUS )

*   Faceplate circle.
      IF ( .NOT. NOFACE ) THEN
         CALL LINE_WRITI( '%p%4w Faceplate radius is %i pixels.\\',
     :                    RADIUS )
         CALL LINE_WRITI( ' centred at (%i\\', CENTRE( 1 ) )
         CALL LINE_WRITI( ',%i).\\', CENTRE( 2 ) )
         CALL PRTBUF( STATUS )
      END IF

*   Rotation specification.
      IF ( .NOT. NOROT ) THEN
         CALL LINE_WRITF( '%p%4w Mean dispersion at %.2f (deg) to L,\\',
     :                    ANGLE )
         CALL LINE_WRITI( ' with radial limits (%i,\\', RLIM( 1 ) )
         CALL LINE_WRITI( '%i)\\', RLIM( 2 ) )
         CALL PRTBUF( STATUS )
      END IF

*   IMAGE details.
      IF ( .NOT. NODATA ) THEN
         CALL LINE_WRITI( '%p%4w Image size (%i,\\', NS )
         CALL LINE_WRITI( '%i),\\', NL )
         CALL LINE_WRITI( ' including lines (%i,\\', LMIN )
         CALL LINE_WRITI( '%i),\\', LMAX )
         NPIXEL = 0
         DO IL = LMIN, LMAX
            IF ( SMIN( IL ) .LE. SMAX( IL ) ) THEN
               NPIXEL = NPIXEL + SMAX( IL ) - SMIN( IL ) + 1
            END IF
         END DO
         CALL LINE_WRITI( ' and %i pixels.\\', NPIXEL )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITF( '%p%4w FN = data number * %g,\\', DSCALE )
         CALL LINE_WRITI( '  with data number in range (%i,\\',
     :                    DLIM( 1 ) )
         CALL LINE_WRITI( '%i).\\', DLIM( 2 ) )
         CALL PRTBUF( STATUS )
      END IF

*   End.
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

 999  CONTINUE
      END
