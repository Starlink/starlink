      SUBROUTINE POINA9( MAXSRC, FMEAN, FSTDEV, OUTSMP, SMPLBD, SMPUBD,
     :                   OUTWAV, THFILT, CANDSM, CANDN, STATUS )
*+
*  Name:
*     POINA9

*  Purpose:
*     Scan the Square wave filtered data for candidate point sources

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA9( MAXSRC, FMEAN, FSTDEV, OUTSMP, SMPLBD, SMPUBD,
*                  OUTWAV, THFILT, CANDSM, CANDN, STATUS )

*  Description:
*     Scan the Square wave filtered data for candidate point sources
*
*  Arguments:
*     MAXSRC = INTEGER (Given)
*        Maximum number of sources
*     FMEAN = REAL (Given)
*        Mean value of the square wave filtered data over the length
*        OUTSMP( 1 ) to OUTSMP( 2 ) - 7
*     FSTDEV = REAL (Given)
*        Standard deviation of the square wave filtered data over the length
*        OUTSMP( 1 ) to OUTSMP( 2 ) - 7
*     OUTSMP( 2 )  = INTEGER (Given)
*        The lower and upper sample numbers that should be considered in
*        subsequent analysis.
*     SMPLBD = INTEGER (Given)
*        Lower limit of the sample index in the current NDF
*     SMPUBD = INTEGER (Given)
*        Upper limit of the sample index in the current NDF
*     OUTWAV( SMPLBD : SMPUBD ) = REAL (Returned)
*        The output square wave filtered output, the output data is put in the
*	 position corresponding to the first sample used in its calculation.
*     THFILT = REAL (Given)
*        Threshold of the square wave filter signal to noise.
*     CANDSM( MAXSRC ) = INTEGER (Returned)
*        An array containing the sample numbers of each point source
*        candidate - note this true position of the point source will be
*        centered 4 samples away from the position at which the filter
*        value was calculated.
*     CANDN = INTEGER (Returned)
*        Number of candidate point sources
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEPT-1994 (DCP):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitve constants inc. VAL__BAD

*  Arguments Given:
      INTEGER MAXSRC
      REAL FMEAN
      REAL FSTDEV
      INTEGER OUTSMP( 2 )
      INTEGER SMPLBD
      INTEGER SMPUBD
      REAL OUTWAV( SMPLBD : SMPUBD )
      REAL THFILT

*  Arguments Returned:
      INTEGER CANDSM( MAXSRC)
      INTEGER CANDN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISAMP              ! Do loop index
      REAL THRESH                ! Threshold value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculated the threshold value
      THRESH = ( THFILT * FSTDEV ) + FMEAN

*  Set the number of candidate samples to zero
      CANDN = 0

*  Go through each of the square wave filtered values and check whether they
*  are larger than the threshold value
      DO ISAMP = OUTSMP( 1 ), OUTSMP( 2 ) - 7

         IF ( OUTWAV( ISAMP ) .GE. THRESH ) THEN

*  Check whether it is a local maximum
            IF ( ( OUTWAV( ISAMP) .GE. OUTWAV( ISAMP - 1 ) ) .AND.
     :           ( OUTWAV( ISAMP) .GE. OUTWAV( ISAMP + 1 ) ) ) THEN

*  Candidate sample found, increment the count of candidates found, and enter
*  the position in the list of candidates ( NB the peak of the candidate
*  point source is 4.5 samples from the start of its associated square wave
*  filter value, here we round it to 4)
               CANDN = CANDN + 1
               CANDSM( CANDN ) = ISAMP + 4

            END IF
         END IF

* End of do loop for each sample
      END DO

      END
