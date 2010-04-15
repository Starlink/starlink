      SUBROUTINE PRSPEC( STATUS )

*+
*  Name:
*     SUBROUTINE PRSPEC

*  Purpose:
*     Print spectrum information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRSPEC( STATUS )

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
      INCLUDE 'CMSAVE'

*  Local Variables:
      REAL*8 W1S(100)       ! calibrated start wavelengths
      REAL*8 W2S(100)       ! calibrated end wavelengths

      INTEGER I             ! loop index
      INTEGER I1            ! loop index
      INTEGER I2            ! loop limit
      INTEGER IAPER         ! aperture index

*.

*   Check inherited global status.
      IF (STATUS .NE. SAI__OK) RETURN

*   Avoid saying nothing
      IF (NOSPEC .OR. NORDER.LE.0) RETURN

*   Banner
      CALL LINE_WCONT('%p%2w Uncalibrated Spectrum:\\')
      CALL PRTBUF( STATUS )

*   Create wavelength ranges
      DO I = 1, NORDER
         W1S(I) = WAV1S(I)
         W2S(I) = WAV2S(I)
      END DO

*   Calibrate wavelength ranges
      IF (STR_SIMLR('HIRES\\', RESOL)) THEN
         DO I = 1, NORDER
            CALL CAWAV(1, ORDERS(I), 1, W1S(I))
            CALL CAWAV(1, ORDERS(I), 1, W2S(I))
         END DO
      ELSE
         DO I = 1, NORDER
            CALL CAWAV(ORDERS(I), 0, 1, W1S(I))
            CALL CAWAV(ORDERS(I), 0, 1, W2S(I))
         END DO
      END IF

*   Print list
      IF (STR_SIMLR('HIRES\\', RESOL)) THEN
         CALL LINE_WCONT('%p%4w Orders and Wavelengths:\\')
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT('%p%2w \\')

         DO I = 1, 3
            CALL LINE_WCONT('%6wM%4wMIN%4wMAX\\')
         END DO
         CALL PRTBUF( STATUS )

         DO I1 = 1, NORDER, 3
            I2 = MIN(NORDER, I1 + 2)
            CALL LINE_WCONT('%p%2w \\')
            DO I = I1, I2
               CALL LINE_WRITI('%4w%3i\\', ORDERS(I))
               CALL LINE_WRITF('%1w%6.1f\\', W1S(I))
               CALL LINE_WRITF('%1w%6.1f\\', W2S(I))
            END DO
            CALL PRTBUF( STATUS )
         END DO

      ELSE IF (STR_SIMLR('LORES\\', RESOL)) THEN
         CALL LINE_WCONT('%p%4w Apertures and Wavelengths:\\')
         CALL PRTBUF( STATUS )

         DO I = 1, NORDER
            IAPER = ORDERS(I)
            CALL LINE_WRITS('%p%6w %s: \\', APERS(1, IAPER))
            CALL LINE_WRITF('(%.2f,\\', W1S(I))
            CALL LINE_WRITF('%.2f)\\', W2S(I))
            CALL PRTBUF( STATUS )
         END DO
      END IF

      CALL LINE_WCONT('%p%4w end.\\')
      CALL PRTBUF( STATUS )

*   End
      CALL LINE_WCONT('%p%2w end.\\')
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

      END
