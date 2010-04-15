      SUBROUTINE POINB0( MAXSRC, BAND, CANDN, CANDSM, INTLBD, INTUBD,
     :                   INTDAT, NOISMP, PRFWID, PROF, SMPLBD, SMPUBD,
     :                   SMPDAT, SPSQ, SIP, SP, SISQ, SI, S1, V,
     :                   S2NREQ, THCORR, THS2N, THSD, MINSMP,
     :                   SRCAMP, SRCBAS, SRCCOR, SRCNON,
     :                   SRCNOS, SRCSLP, SRCSMP, SRCN, STATUS )
*+
*  Name:
*     POINB0

*  Purpose:
*     Detects point sources.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINB0( MAXSRC, BAND, CANDN, CANDSM, INTLBD, INTUBD, INTDAT,
*                  NOISMP, PRFWID, PROF, SMPLBD, SMPUBD, SMPDAT,
*                  SPSQ, SIP, SP, SISQ, SI, S1, V,
*                  S2NREQ, THCORR, THS2N, MXSRCN,
*                  SRCAMP, SRCBAS, SRCCOR, SRCNON, SRCNOS,
*                  SRCSLP, SRCSMP, SRCN, STATUS )

*  Description:
*     This subroutine is used by POINTCRDD to select point sources from
*     the given candidates. The selections are determined by the
*     correlations between the candidates with a linearly biased ideal
*     point source profile. Only those candidates whose coorelations
*     exceed the given threshold, THCORR, are selected as the detected
*     point sources. The linearly biase parameters, amplitude, slop and
*     baseline height, are determined by the least squares fitting. To
*     reduce the error cause by sampling, the least square fitting are
*     performed at CANDSM - 1, CANDSM and CANDSM + 1. The parabola
*     interpolation are used to find the position of the max.
*     correlation coefficient. A candidate will only be regarded as a
*     point source if this max. correlation coefficient is greater than
*     the given threshold.

*  Arguments:
*     MAXSRC = INTEGER (Given)
*        Maximum number of sources
*     BAND = INTEGER (Given)
*        Waveband number
*     CANDN = INTEGER (Given)
*        The number of candidate point sources
*     CANDSM( MAXSRC) = INTEGER (Given)
*        The sample indices of the candidate point sources
*     INTLBD = INTEGER (Given)
*        The begining sample index of the interpolated data
*     INTUBD = INTEGER (Given)
*        The end sample index of the interpolated data
*     INTDAT( INTLBD: INTUBD ) = INTEGER (Given)
*        The interpolated data
*     NOISMP = INTEGER (Returned)
*        Number of samples to be included for each side of expected source
*        in calculation of local noise.
*     MINSMP = INTEGER (Given)
*        Minimum number of of non bad, not over threshold samples to be
*	 included in noise calculation
*     PRFWID = INTEGER (Given)
*        Number of samples in the ideal point source profile.
*     PROF( PRFWID ) = REAL (Given)
*        The point source profile.
*     SMPLBD = INTEGER (Given)
*        The begining sample index of the original CRDD data for the current
*	 detector
*     SMPUBD = INTEGER (Given)
*        The end sample index of the original CRDD data for the current
*	 detector
*     SMPDAT( SMPLBD: SMPUBD ) = REAL (Given)
*        The copy of a single detector trace from the original CRDD data
*     SPSQ = DOUBLE PRECISION (Given)
*        The sum of squared point source profile samples
*     SIP = DOUBLE PRECISION (Given)
*        Sum of sample indices times  profile samples.
*     SP = DOUBLE PRECISION (Given)
*        Sum of profile samples.
*     SISQ = DOUBLE PRECISION (Given)
*        Sum of squared sample indices.
*     SI = DOUBLE PRECISION (Given)
*        Sum of sample indices.
*     S1 = DOUBLE PRECISION (Given)
*        Sum of constant 1.
*     V = DOUBLE PRECISION (Given)
*        Determinant of the symmatric matrix formed by SPSQ, SIP, SP,
*        SISQ, SI, S1.
*     S2NREQ = LOGICAL (Given)
*        TRUE if the signal to noise test is required else all sources
*        which pass the correlation test are reported
*     THCORR = REAL (Given)
*        The correlation threshold.
*     THS2N = REAL (Given)
*        The signal to noise ratio threshold.
*     THSD = REAL (Given)
*        Threshold of standard deviations above which sample is to be excluded
*	 from noise calculations as an assumed source
*     SRCAMP( MAXSRC ) = REAL (Returned)
*        The amplitude of the detected point sources.
*     SRCBAS( MAXSRC ) = REAL (Returned)
*        Baseline height of the detected point sources.
*     SRCCOR( MAXSRC ) = REAL (Returned)
*        Correlation coefficient for the detected point sources.
*     SRCNON( MAXSRC ) = INTEGER (Returned)
*        Number of samples used in calculating the local noise
*     SRCNOS( MAXSRC ) = REAL (Returned)
*        Estimated local noise.
*     SRCSLP( MAXSRC ) = REAL (Returned)
*        The slope of the data in the region surrounding the source.
*     SRCSMP( MAXSRC ) = REAL (Returned)
*        The sample position the the detected point sources.
*     SRCN = INTEGER (Returned)
*        The number of the point sources detected in this detector scan.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     4-MAR-1993 (WG):
*        Original version.
*     12-OCT-1994 (DCP):
*        Rewritten
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 package constants

*  Arguments Given:
      INTEGER MAXSRC
      INTEGER BAND
      INTEGER CANDN
      INTEGER CANDSM( MAXSRC)
      INTEGER INTLBD, INTUBD
      REAL INTDAT( INTLBD: INTUBD )
      INTEGER NOISMP
      INTEGER MINSMP
      INTEGER PRFWID
      REAL PROF( PRFWID )
      INTEGER SMPLBD, SMPUBD
      REAL SMPDAT( SMPLBD: SMPUBD )
      DOUBLE PRECISION SPSQ
      DOUBLE PRECISION SIP
      DOUBLE PRECISION SP
      DOUBLE PRECISION SISQ
      DOUBLE PRECISION SI
      DOUBLE PRECISION S1
      DOUBLE PRECISION V
      LOGICAL S2NREQ
      REAL THCORR
      REAL THS2N
      REAL THSD

*  Arguments Returned:
      REAL SRCAMP( MAXSRC )
      REAL SRCBAS( MAXSRC )
      REAL SRCCOR( MAXSRC )
      INTEGER SRCNON( MAXSRC )
      REAL SRCNOS( MAXSRC )
      REAL SRCSLP( MAXSRC )
      REAL SRCSMP( MAXSRC )
      INTEGER SRCN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ACTSMP( 2 )        ! The first and last sample numbers of the
				 ! actual sample range used in calculating noise
      REAL AMP                   ! Amplitude at max. corellation coefficient
      REAL AMPI, AMI, AMLI       ! Amplitude at samples I-1, I, I+1 where I is
				 ! the candidate sample number
      REAL BSPI, BSI, BSLI       ! Baseline height at I-1, I, I+1
      REAL CC                    ! Max. CC
      REAL CCPI, CCI, CCLI       ! Correlations at I-1, I, I+1
      REAL CMEAN                 ! Mean value of the samples over which
				 ! the noise is calculated
      INTEGER CNOBAD             ! Number of bad values in noise range
      INTEGER CNOLRG             ! Number of over thrshold samples in noise
				 ! range
      INTEGER CNOSMP             ! Number of valid samples in noise range
      REAL CSTDEV                ! Standard deviation of samples ie Noise
      LOGICAL DETER2             ! TRUE if there less than MINSMP samples
				 ! available for noise calcuation
      INTEGER HAFNOS             ! Half number of samples in noise calculation
      INTEGER ICAN               ! Point source candidate index
      INTEGER INSMPL             ! Lower bound of region around candidate point
				 ! source ommitted from noise calulations as
				 ! being affected by source
      INTEGER INSMPU             ! Upper bound of region around candidate point
				 ! source ommitted from noise calulations as
				 ! being affected by source
      INTEGER LSRCSM             ! Sample number associated with the last source
				 ! found valid
      REAL POSCOR                ! Position of the max. CC
      INTEGER OUTSMP( 2 )        ! Begin and end of range for noise etimating
      REAL SLPI, SLI, SLLI       ! Slop at I-1, I, I+1
      INTEGER SMP                ! Candidate sample position
      REAL THRESH

* Programmers notes:
* Two sets of crdd data are used in this routine:-
* The original data which includes bad_values is used for calcualting the
* local noise around the candidate sample position.
* The interpolated data has any original bad values replaced by values
* interpolated from the surrounding samples, but valid data samples are as
* in the original. This data is used in the local maximum check, the calculation
* of the correlation, and the calculation of amplitude, slope and constant
* values.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the number of fully checked out point sources to zero
      SRCN = 0

*  Set the sample number associated with the last source found to zero
      LSRCSM = 0

*  Consider the given candidates one by one.
      DO ICAN = 1, CANDN

*  First of all the input data should have a peak around the candidate
*  position.
         SMP = CANDSM( ICAN )
         IF ( ( INTDAT( SMP - 1 ) .GT. INTDAT( SMP - 2 ) .AND.
     :          INTDAT( SMP - 1 ) .GE. INTDAT( SMP ) ) .OR.
     :        ( INTDAT( SMP - 1 ) .GE. INTDAT( SMP - 2 ) .AND.
     :          INTDAT( SMP - 1 ) .GT. INTDAT( SMP ) ) .OR.
     :        ( INTDAT( SMP ) .GT. INTDAT( SMP - 1 ) .AND.
     :          INTDAT( SMP ) .GE. INTDAT( SMP + 1 ) ) .OR.
     :        ( INTDAT( SMP ) .GE. INTDAT( SMP - 1 ) .AND.
     :          INTDAT( SMP ) .GT. INTDAT( SMP + 1 ) ) .OR.
     :        ( INTDAT( SMP + 1 ) .GT. INTDAT( SMP ) .AND.
     :          INTDAT( SMP + 1 ) .GE. INTDAT( SMP + 2 ) ) .OR.
     :        ( INTDAT( SMP + 1 ) .GE. INTDAT( SMP ) .AND.
     :          INTDAT( SMP + 1 ) .GT. INTDAT( SMP + 2 ) ) ) THEN

*  Do least square fit of the point source profile at the candidate
*  sample position, and calculate the correlation coefficient and
*  mean, slope and point source amplitude for the data surrounding
*  that position
            CALL POINC2( INTLBD, INTUBD, INTDAT, PRFWID, PROF, SMP,
     :                   SPSQ, SIP, SP, SISQ, SI, S1, V, AMI,
     :                   SLI, BSI, CCI, STATUS )

*  Do the similar calculation at the candidate sample - 1 position.
            CALL POINC2( INTLBD, INTUBD, INTDAT, PRFWID, PROF, SMP - 1,
     :                   SPSQ, SIP, SP, SISQ, SI, S1, V, AMPI,
     :                   SLPI, BSPI, CCPI, STATUS )

*  Do the similar calculation at the candidate sample + 1 position.
            CALL POINC2( INTLBD, INTUBD, INTDAT, PRFWID, PROF, SMP + 1,
     :                   SPSQ, SIP, SP, SISQ, SI, S1, V, AMLI,
     :                   SLLI, BSLI, CCLI, STATUS )

*  Find the position correction needed to obtain the maximum correlation
*  coefficient from the three points above by parabola interpolation.
            CALL POINC3( CCPI, CCI, CCLI, CC, POSCOR, STATUS )

*  Continue with those source candidates which have correlation coefficients
*  above the correlation coefficient threshold.
            IF ( CC .GE. THCORR ) THEN

*  Get the amplitude of the point source by parabola interpolation.
               CALL POINC4( AMPI, AMI, AMLI, POSCOR, AMP, STATUS )

*  Calculate the range over which the local noise at the candidate position
*  is to be extimated.
*  Two ranges are used on either side of the candidate point source
*  each from .5 times the profile width from the source to that plus
*  half NOISMP samples unless it bumps into the edge of the scan.
               HAFNOS      = INT( REAL( NOISMP ) / 2 )
               INSMPL      = CANDSM( ICAN ) -
     :                       INT( ( 0.5 * REAL( PRFWID)) + 1 )
               INSMPU      = CANDSM( ICAN ) +
     :                       INT( ( 0.5 * REAL( PRFWID)) + 1 )
               OUTSMP( 1 ) = MAX( SMPLBD, INSMPL - HAFNOS )
               OUTSMP( 2 ) = MIN( SMPUBD, INSMPU + HAFNOS )

*  Estimate the local noise at the candidate position.
               CALL POINA6( .TRUE., INSMPL, INSMPU, MINSMP, OUTSMP,
     :                      .FALSE., SMPLBD, SMPUBD, THSD, SMPDAT,
     :                      ACTSMP, DETER2, CMEAN, CNOBAD,
     :                      CNOLRG, CNOSMP, CSTDEV, STATUS )

*  Accept candiate as a source if the signal to noise ratio is above the
*  threshold or signal to noise test is not required.
               THRESH = THS2N * CSTDEV
               IF ( .NOT. S2NREQ .OR. ( AMP .GT. THRESH) ) THEN

*  First check whether the last source recorded was associated with the
*  sample immediately preceeding this current sample
                  IF ( CANDSM( ICAN ) - ( LSRCSM + 1 ) .NE. 0 ) THEN

*  The source and the prievous source are not consecutive samples and
*  threfore record this source
                     SRCN = SRCN + 1

*  Update the sample number of the last source written to the current sample
*  number
                     LSRCSM = CANDSM( ICAN )

* Enter data required for current source
                     SRCSMP( SRCN ) = REAL( CANDSM( ICAN ) ) + POSCOR
                     SRCAMP( SRCN ) = AMP
                     SRCNOS( SRCN ) = CSTDEV
                     SRCNON( SRCN ) = CNOSMP
                     SRCCOR( SRCN ) = CC

*  Find the data slope value at the position of maximum correlation
*  coefficient by using parabolic interpolation between the slope at
*  the candidate and at the two adjacent points
                     CALL POINC4( SLPI, SLI, SLLI, POSCOR,
     :                            SRCSLP( SRCN ),STATUS )

*  Find the mean value at the position of maximum correlation coefficient by
*  using parabolic interpolation between the mean at the candidate and at the
*  two adjacent points
                     CALL POINC4( BSPI, BSI, BSLI, POSCOR,
     :                            SRCBAS( SRCN ),STATUS )

*  Calibrate the constant so that it stands for the constant at the centre of
*  the point source rather than that at one end of the point source profile
                     SRCBAS( SRCN ) = SRCBAS( SRCN ) +
     :                   SRCSLP( SRCN ) * ( REAL( PRFWID) / 2.0 + 1.0 )

*  Convert the slope from per sample to per arcmin
                     SRCSLP( SRCN ) =
     :                      SRCSLP( SRCN ) * REAL( I90__SRATE( BAND ) )
     :                      / I90__SPEED

* If the preceeding source was the immediately preceeding sample ( or
* part of a series of consecutive samples )
                  ELSE

* Set the last source "written" to the current sample number, this will enable
* a string of samples at consecutive samples to record only one source
                     LSRCSM = CANDSM( ICAN )

* Check which of the two consecutive sources has the higher correlation
                     IF ( CC .GT. SRCCOR( SRCN ) ) THEN

* Only if this source has a higher correlation than the previous one do we
* OVERWRITE the previous source data with that for the current source
* Enter data required for current source
                        SRCSMP( SRCN ) = REAL( CANDSM( ICAN ) ) + POSCOR
                        SRCAMP( SRCN ) = AMP
                        SRCNOS( SRCN ) = CSTDEV
                        SRCNON( SRCN ) = CNOSMP
                        SRCCOR( SRCN ) = CC

*  Find the data slope value at the position of maximum correlation
*  coefficient by using parabolic interpolation between the slope at
*  the candidate and at the two adjacent points
                        CALL POINC4( SLPI, SLI, SLLI, POSCOR,
     :                            SRCSLP( SRCN ),STATUS )

*  Find the mean value at the position of maximum correlation coefficient by
*  using parabolic interpolation between the mean at the candidate and at the
*  two adjacent points
                        CALL POINC4( BSPI, BSI, BSLI, POSCOR,
     :                            SRCBAS( SRCN ),STATUS )

*  Calibrate the constant so that it stands for the constant at the centre of
*  the point source rather than that at one end of the point source profile
                        SRCBAS( SRCN ) = SRCBAS( SRCN ) +
     :                   SRCSLP( SRCN ) * ( REAL( PRFWID) / 2.0 + 1.0 )

*  Convert the slope from per sample to per arcmin
                        SRCSLP( SRCN ) =
     :                      SRCSLP( SRCN ) * REAL( I90__SRATE( BAND ) )
     :                      / I90__SPEED

* End if for check on which of two consecutive sources has the greater
* correlation ( if the current source has a lower correlation then it is not
* entered in the list of sources )
                     END IF

* End if for check whether sources are consecutive
                  END IF
               END IF
            END IF
         END IF
      END DO

      END
