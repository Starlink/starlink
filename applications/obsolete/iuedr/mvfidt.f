      SUBROUTINE MVFIDT(NPRINT, STATUS)
*+
*
*   Name:
*      SUBROUTINE MVFIDT
*
*   Description:
*      The fiducial positions are moved to account for any temperature
*      sensitivity.  The standard THDA for the fiducials is set to that
*      associated with the current image.   This means that if the latter
*      is changed, the new shifts can be applied in a consistent way
*      (assuming linear shifts!).
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      The rotation transform is taken from CMROTS.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NPRINT     ! print level

*   Export:
      INTEGER STATUS     ! status return

*   CMHEAD:
      INCLUDE 'CMHEAD'

*   CMFIDS:
      INCLUDE 'CMFIDS'

*   CMFIDT:
      INCLUDE 'CMFIDT'

*   Local variables:
      INTEGER IX         ! loop index
      INTEGER IY         ! loop index

      REAL*8 DT          ! relative temperature change
*.

      IF (.NOT.NOFIDS .AND. .NOT.NOFIDT .AND. THDA.NE.0.0) THEN
         IF (NPRINT.GT.0) THEN
            CALL LINE_WCONT(
     :           '%p Fiducial positions have THDA corrections.\\'
     :                   )
            CALL PRTBUF( STATUS )
            CALL LINE_WRITF('%p Image THDA is %.2f (C),\\', THDA)
            CALL LINE_WRITF(' Mean Fiducial THDA is %.2f (C).\\', FIDT0)
            CALL PRTBUF( STATUS )
         END IF

         DT = THDA - FIDT0

         DO IY = 1, NFIDY
            DO IX = 1, NFIDX
               FIDS(IX, IY) = FIDS(IX, IY) + DT*FIDST(IX, IY)
               FIDL(IX, IY) = FIDL(IX, IY) + DT*FIDLT(IX, IY)
            END DO
         END DO

         FIDT0 = THDA

      ELSE IF (NPRINT.GT.0) THEN
         CALL LINE_WCONT(
     :        '%p Fiducial positions not corrected for THDA.\\'
     :                  )
         CALL PRTBUF( STATUS )

      END IF

      STATUS = 0

      END
