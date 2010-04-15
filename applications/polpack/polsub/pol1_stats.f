      SUBROUTINE POL1_STATS( TITLE, NEL, DATA, STATUS )
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

      INTEGER NEL, STATUS, N, I
      REAL DATA( NEL ), SUM, SUM2, MX, MN, VAL, MEAN
      CHARACTER TITLE*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      SUM = 0.0
      SUM2 = 0.0
      N = 0
      MX = VAL__MINR
      MN = VAL__MAXR

      DO I = 1, NEL
         VAL = DATA( I )
         IF( VAL .NE. VAL__BADR ) THEN
            SUM = SUM + VAL
            SUM2 = SUM2 + VAL*VAL
            N = N + 1
            MX = MAX( MX, VAL )
            MN = MIN( MN, VAL )
         END IF
      END DO

      WRITE(*,*) TITLE
      IF( N .GT. 0 ) THEN
         MEAN = SUM/N
         WRITE(*,*) '   Mean  : ',MEAN
         WRITE(*,*) '   Sigma : ',SQRT( MAX( 0.0, SUM2/N - MEAN*MEAN ) )
         WRITE(*,*) '   Max   : ',MX
         WRITE(*,*) '   Min   : ',MN
         WRITE(*,*) '   NGood : ',N
      ELSE
         WRITE(*,*) '   NGood : 0'
      END IF

      END
