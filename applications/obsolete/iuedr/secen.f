      SUBROUTINE SECEN

*+
*
*   Name:
*      SUBROUTINE SECEN
*
*   Description:
*      Set centroid template prior to extraction. The contents of CMCEN
*      are filled either with zeros or from an entry in CMTEM.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The Centm is used to indicate whether a pre-existing template
*      is mapped onto the extraction wavelength grid.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Global variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMDISH'
      INCLUDE 'CMCEN'
      INCLUDE 'CMWAV'
      INCLUDE 'CMTEM'

*   Local variables:
      REAL*8 DC
      REAL*8 DCDW
      REAL*8 W1
      REAL*8 W2

      INTEGER I     ! loop index
      INTEGER K     ! loop index
      INTEGER M     ! order

*   Zero it out (always)
      DO I = 1, NWAV
         SCEN(I) = 0.0
         DCEN(I) = 0.0
         QCEN(I) = 1
      END DO

*   Optionally use a pre-existing template
      IF (CENTM .AND. .NOT.NOTEM) THEN

         CALL FNTEM(CORD, M)

*      Use CMTEM data
         IF (M.GT.0) THEN

            I = 1

            DO 120 K = 2, NTEMS(M)

               W2 = DBLE(K - 1) * TEMDW(M) + TEMW0(M)
               W1 = W2 - TEMDW(M)
               DCDW = (TEMCEN(K, M) - TEMCEN(K - 1, M))/(W2 - W1)
               DC = TEMCEN(K - 1, M) - W1*DCDW

 110           CONTINUE

               IF (.NOT.(I.LE.NWAV)) GO TO 120

               IF (WAV(I).LT.W1) THEN

                  I = I + 1

               ELSE IF (WAV(I).GT.W2) THEN

                  GO TO 120

               ELSE

                  SCEN(I) = DCDW*WAV(I) + DC
                  I = I + 1

               END IF

               GO TO 110

 120        CONTINUE

         END IF

      END IF

      END
