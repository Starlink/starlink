*+  MATH_WTMEANR - Calculate REAL weighted mean
      SUBROUTINE MATH_WTMEANR( NPTS, DATA, VAR, QUAL, QUALOK, MEAN,
     :                                                   ERROR, STATUS )
*    Description :
*     Calculates weigthed mean
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Phillip Andrews (pla@uk.ac.bham/sr/star)
*    History :
*     25/8/88:  original (pla@uk.ac.bham/sr/star)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER                NPTS         ! Number of data points

      REAL                   DATA(*)      ! Data values
      REAL                   VAR(*)       ! Variance values

      LOGICAL                QUAL(*)      ! Quality
      LOGICAL                QUALOK       ! Use quality?
*    Export :
      REAL                   MEAN         ! Weighted mean of data
      REAL                   ERROR        ! Error on weighted mean
*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER                I            ! Loop counter

      DOUBLE PRECISION       WEIGHT       ! Weighting factor
      DOUBLE PRECISION       WTSUM        ! Sum of weighting factors
      DOUBLE PRECISION       SUM          ! Sum of (data * weight)

      LOGICAL                CONTINUE     ! Used to skip over bad data
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CONTINUE = .TRUE.

      DO I = 1, NPTS
         IF ( QUALOK ) THEN
            CONTINUE = QUAL(I)

         END IF

         IF ( VAR(I) .LE. 0.0 ) THEN
            CONTINUE = .FALSE.

         END IF

         IF ( CONTINUE ) THEN
            WEIGHT = 1.0D0 / DBLE( VAR(I) )
            WTSUM  = WTSUM + WEIGHT
            SUM    = SUM + DBLE(DATA(I)) * WEIGHT

         END IF
      END DO

      IF ( WTSUM .GT. 0.0D0 ) THEN
         MEAN  = REAL( SUM / WTSUM )
         ERROR = REAL( 1.0D0 / SQRT(WTSUM) )

      ELSE
         CALL MSG_OUT( ' ',
     :        'FATAL ERROR: Unable to calculate weighted mean', STATUS )
         STATUS = SAI__ERROR

      END IF

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', '... from MATH_WTMEANR', STATUS )

      END IF
      END
