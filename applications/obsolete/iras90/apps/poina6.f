	SUBROUTINE POINA6( EXTEND, INSMPL, INSMPU, MINSMP, OUTSMP,
     :                     SINGLR, SMPLBD, SMPUBD, THSD, DATA,
     :                     ACTSMP, DETER2, MEAN, NOBAD,
     :                     NOLARG, NOSAMP, STDDEV, STATUS )

*+
*  Name:
*     POINA6

*  Purpose:
*     To calculate the mean and variance of a set of samples, the
*     calculation takes place iteratively rejecting any samples
*     which are outside THSD * previous iteration standard deviation
*     from the mean.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA6( EXTEND, INSMPL, INSMPU, MINSMP, OUTSMP,
*                  SINGLR, SMPLBD, SMPUBD, THSD, DATA,
*                  ACTSMP, DETER2, MEAN, NOBAD,
*                  NOLARG, NOSAMP, STDDEV, STATUS )


*  Description:
*     To calculate the mean and variance of a set of samples, the
*     calculation takes place iteratively rejecting any samples
*     which are outside THSD * previous iteration standard deviation
*     from the mean.
*     The calculation can be done in two modes. If SINGLR is true
*     then the noise is calculated over a single range from OUTSMP( 1 )
*     to OUTSMP( 1 ). If SINGLR is false then the calculation takes place
*     over two ranges OUTSMP( 1 ) to INSMPL and INSMPU to OUTSMP( 2 ).
*     If the argument EXTEND is true then if the number of samples
*     in the current iteration is less than MINSMP, the length of
*     samples taken is extended at either side and the range used
*     is entered in ACTSMP. In two range mode this implies that samples
*     outside the OUTSMP range are used but the gap in the middle (which
*     might be excluding a source) is not altered.
*     If the argument EXTEND is false then if the number of samples
*     is less than MINSMP an error is reported and the variable
*     DETER2 is set true.
*
*  Arguments:
*     EXTEND = LOGICAL (Given)
*        TRUE if the range of samples included in the calculation should
*	 be extended if the number of non bad, not over threshold
*	 samples is not greater than the minimum number
*     INSMPL = INTEGER (Given)
*        If a double range calculation is required this is the lower bound
*	 of the inner region which is not to be included in the noise
*	 calculation
*     INSMPU = INTEGER (Given)
*        If a double range calculation is required this is the upper bound
*	 of the inner region which is not to be included in the noise
*	 calculation
*     MINSMP = INTEGER (Given)
*        Minimum number of of non bad, not over threshold samples to be
*	 included in noise calculation
*     OUTSMP( 2 )  = INTEGER (Given)
*        The lower and upper sample numbers that should be considered in
*        subsequent analysis.
*     SINGLR = LOGICAL (Given)
*        TRUE if a single range of samples is to be included in the noise
*	 calculation, FALSE if a split range (eg to omit a point source)
*	 is being used
*     SMPLBD = INTEGER (Given)
*        Lower limit of the sample index in the current NDF
*     SMPUBD = INTEGER (Given)
*        Upper limit of the sample index in the current NDF
*     THSD = REAL (Given)
*        Threshold of standard deviations above which sample is to be excluded
*	 from noise calculations as an assumed source
*     DATA( SMPLBD : SMPUBD ) = REAL
*        The single dimension input data array
*     ACTSMP( 2 ) = INTEGER (Returned)
*        The first and last sample numbers of the actual sample range used
*     DETER2 = LOGICAL (Returned)
*        TRUE if there are so many bad values in the detector trace in the
*        region from which noise is to be calculated that a minimum number
*        of samples is not reached
*     MEAN = REAL (Returned)
*        Mean value of the samples over which the noise is calculated
*     NOBAD = INTEGER (Returned)
*        Number of bad samples in the range ( or two ranges if SINGLR is FALSE )
*     NOLARG = INTEGER (Returned)
*        Number of samples with value over the threshold in range (or two
*	 ranges)
*     NOSAMP = INTEGER (Returned)
*        Number of samples used in calculating the noise
*     STDDEV = REAL (Returned)
*        Standard deviation of the sample values ie noise
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEPT-1994 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG system constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants
      INCLUDE 'IRA_PAR'          ! IRA system constants
      INCLUDE 'IRA_ERR'          ! IRA system errors
      INCLUDE 'PAR_PAR'          ! PAR system constants
      INCLUDE 'PAR_ERR'          ! PAR system errors
      INCLUDE 'PRM_PAR'          ! Primitve constants inc. VAL__BAD

*  Arguments Given:
      LOGICAL EXTEND
      INTEGER INSMPL
      INTEGER INSMPU
      INTEGER MINSMP
      INTEGER OUTSMP( 2 )
      LOGICAL SINGLR
      INTEGER SMPLBD
      INTEGER SMPUBD
      REAL THSD
      REAL DATA( SMPLBD : SMPUBD )

*  Arguments Returned:
      INTEGER ACTSMP( 2 )
      LOGICAL DETER2
      REAL MEAN
      INTEGER NOBAD
      INTEGER NOLARG
      INTEGER NOSAMP
      REAL STDDEV

*  Status:
      INTEGER STATUS             ! Global status

*  Local constants:
      INTEGER NITER              ! Maximum number of iterations in niose
				 ! calculation
      PARAMETER ( NITER = 7 )

*  Local Variables:
      INTEGER IITER              ! Iteration count
      INTEGER ISMP               ! Sample index for loops
      REAL PREVSD                ! Value of the standard deviation in previous
				 ! iteration
      INTEGER REMAIN             ! Number of samples in scan not used in noise
				 ! calculation
      REAL SUM   		 ! Sum of sample values
      REAL SUMSQU		 ! Sum of sample values squared
      REAL THRESH		 ! Value of threshold at which samples are
				 ! rejected
      INTEGER TOFIND		 ! Number of samples by which range is to be
				 ! extended
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      PREVSD = 0.0

*  Set a dummy standard deviation to a high value so that no values will
*  be rejected because they are over the threshold in the first iteration
      STDDEV = 1.0E10

*  Set the bounds over which the noise is to be calculated to OUTSMP
      ACTSMP( 1 ) = OUTSMP( 1 )
      ACTSMP( 2 ) = OUTSMP( 2 )

*  Set thew number of iterations carried out to zero
      IITER = 0

*  Repeat until either the iteration count is exceeded or the standard deviation
*  does not change significantly between iterations
      DO WHILE( ( IITER .LT. NITER ) .AND.
     :          ( ABS( STDDEV - PREVSD ) .GT. VAL__SMLR ) )

*  Zeroise the counts of number of valid samples, number of samples
*  breaking the threshold, and number of bad samples
         NOSAMP = 0
         NOLARG = 0
         NOBAD = 0

*  Zeroise the sum and sumsqu for this iteration
         SUM = 0.0
         SUMSQU = 0.0

*  Put standard deviation calculated in previous iteration into PREVSD
         PREVSD  = STDDEV

*  Calculate the threshold value
         THRESH = PREVSD * THSD + MEAN

* ************************************************************************
* Calculation of noise for this iteration
* ************************************************************************
         DO ISMP = ACTSMP( 1 ), ACTSMP( 2 )

* Process samples if a single range is true or
* if the sample number is outside the inner range limits if the single range
* is false
            IF ( ( SINGLR ) .OR.
     :           ( ( ISMP .LE. INSMPL ) .OR.
     :             ( ISMP .GE. INSMPU ) ) ) THEN

* Determine whether the value is bad
               IF ( DATA( ISMP ) .EQ. VAL__BADR ) THEN
                  NOBAD = NOBAD + 1

* Or above the rejection threshold
               ELSE IF ( DATA( ISMP ) .GE. THRESH ) THEN
                  NOLARG = NOLARG + 1

* Or a sample from which the noise of the next iteration should be calculated
               ELSE
                  SUM = SUM + DATA( ISMP )
                  SUMSQU = SUMSQU +
     :                     ( DATA( ISMP ) * DATA( ISMP ) )
                  NOSAMP = NOSAMP + 1
               END IF

* End if for inner range reject test
            END IF

* End do for sample loop
         END DO

* Calculate mean and standard deviation
         MEAN = SUM / REAL( NOSAMP )
         STDDEV = SUMSQU / REAL( NOSAMP ) - ( MEAN * MEAN )
         STDDEV = SQRT( STDDEV )

* Check whether sufficient samples were present
         IF ( NOSAMP .GE. MINSMP ) THEN

* Sufficient samples were present set the error flag false
            DETER2 = .FALSE.

*  Else check whether  EXTEND is true
         ELSE IF ( EXTEND ) THEN

*  Check whether there are sufficient samples to enable extension to take place
            REMAIN = ( ACTSMP( 1 ) - OUTSMP( 1 ) ) +
     :                    ( OUTSMP( 2 ) - ACTSMP( 2 ) )
            TOFIND = MINSMP - NOSAMP
            IF ( REMAIN .GE. TOFIND ) THEN

* There are sufficient samples to extend set flag to noerror
               DETER2 = .FALSE.

* While there are still extension samples to find alternately add extension
* samples at either end until either it bumps into one end, or there are no
* more to add. If it bumps into one end of the scan and there are still samples
* to add then they are all added at the other end.
               DO WHILE ( TOFIND .GT. 0 )
                  IF ( ACTSMP( 1 ) .GT. OUTSMP( 1 ) ) THEN
                      ACTSMP( 1 ) =  ACTSMP( 1 )  - 1
                      TOFIND = TOFIND - 1
                  END IF
                  IF ( ( ACTSMP( 2 ) .LT. OUTSMP( 2 ) )
     :                      .AND. ( TOFIND .GT. 0 ) ) THEN
                      ACTSMP( 2 ) =  ACTSMP( 2 ) + 1
                      TOFIND = TOFIND - 1
                  END IF
               END DO

*  If there is insuficient data in rest of scan to extend
            ELSE
               DETER2 = .TRUE.
               RETURN
            ENDIF

*  If no extend then set flag indicating lack of data and exit
         ELSE
            DETER2 = .TRUE.
            RETURN

*  End if for check on validity of iteration results
         END IF

*  End do for iteration loop
      END DO

      END
