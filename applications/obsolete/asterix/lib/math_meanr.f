*+  MATH_MEANR - Calculate REAL mean
      SUBROUTINE MATH_MEANR( NPTS, DATA, QUAL, QUALOK, MEAN, ERROR,
     :                                                          STATUS )
*    Description :
*     Calculates arithmetic mean
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phil Andrews (pla@uk.ac.bham.sr.star)
*    History :
*     25/8/88: original (pla)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER                NPTS                                       ! Number of data points

      REAL                   DATA(*)                                    ! Data values

      LOGICAL                QUAL(*)                                    ! Quality values
      LOGICAL                QUALOK                                     ! Use quality?
*    Export :
      REAL                   MEAN                                       ! Mean value
      REAL                   ERROR                                      ! Standard error on mean
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER                I                                          ! Loop counter
      INTEGER                NUM                                        ! Number of valid points

      REAL                   SUM                                        ! Sum of data values
      REAL                   SDEV                                       ! Used in calc of standard deviation

      LOGICAL                CONTINUE                                   ! Used to skip over bad quality
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CONTINUE = .TRUE.
      SUM      = 0.0
      NUM      = 0

      DO I = 1, NPTS
         IF ( QUALOK ) THEN
            CONTINUE = QUAL(I)

         END IF

         IF ( CONTINUE ) THEN
            SUM = SUM + DBLE(DATA(I))
            NUM = NUM + 1

         END IF
      END DO

      IF ( NUM .GT. 0 ) THEN
        MEAN = REAL( SUM / DBLE(NUM) )

      ELSE
         CALL MSG_OUT( ' ', 'FATAL ERROR: unable to calculate mean',
     :                                                          STATUS )
         STATUS = SAI__ERROR

      END IF

*    Check STATUS
      IF ( STATUS .NE. SAI__OK ) GOTO 999

      SDEV = 0.0

      DO I = 1, NPTS
         IF ( QUALOK ) THEN
            CONTINUE = QUAL(I)

         END IF

         IF ( CONTINUE ) THEN
            SDEV = ( DBLE(DATA(I)) - (SUM / DBLE(NUM)) )**2

         END IF
      END DO
      SDEV  = SQRT( SDEV / DBLE(NUM) )
      ERROR = REAL( SDEV / SQRT(DBLE(NUM)) )

999   IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', '...from MATH_MEANR', STATUS )

      END IF
      END
