C+
      SUBROUTINE AHOPEN(NVERT,STATUS)
C
C     A H O P E N
C
C     Opens a plot on the current hardcopy device.
C
C     Parameters    ("<" output)
C
C     (>) NVERT     (Integer) Number of vertical plots to a page.
C     (<) STATUS    (Integer) Set to zero if plot opened OK,
C                   to 1 otherwise.
C
C     Common variables used - None
C                                             KS / AAO 5th Sept 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NVERT, STATUS
C
C     Functions
C
      INTEGER PGBEGIN
C
C     Local variables
C
      CHARACTER*32 DEVICE
C
C     Get user variable HARD and open plot.
C
      CALL VAR_GETCHR('HARD',0,0,DEVICE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('No hard copy graphics device defined',
     :                                                    STATUS)
         STATUS=1
      ELSE
         STATUS=PGBEGIN(0,DEVICE,1,NVERT)
         IF (STATUS.NE.1) THEN
            CALL PAR_WRUSER('Unable to open hard copy device',STATUS)
            STATUS=1
         ELSE
            STATUS=0
         END IF
      END IF
C
      END
