      SUBROUTINE CTEMZ0( BAND1, BAND2, BETA, LERR, RERR, LCK, IPLKR,
     :                   IPCKR, IPLKF1, IPCKF1, RATLO, RATHI, STATUS )
*+
*  Name:
*     CTEMZ0

*  Purpose:
*     Obtain cubic spline fits to temperature and BAND1 flux.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTEMZ0( BAND1, BAND2, BETA, LERR, RERR, LCK, IPLKR, IPCKR,
*                  IPLKF1, IPCKF1, RATLO, RATHI, STATUS )

*  Description:
*     The observed model flux in both wavebands is evaluated at 45
*     temperatures logarithmically spaced between 30 K and 10000 K.  A
*     cubic spline is fitted to the observed model flux for BAND1 using
*     temperature as the independant variable. The knots and
*     coefficients are returned. The ratio of observed model flux in
*     BAND1 to that in BAND2 is found. Another cubic spline is found
*     using these ratio values as the independant variable and
*     temperature as the dependant variable. The knots and coefficients
*     are returned. The maximum and minimum ratio values for which this
*     cubic spline is defined are also returned.

*  Arguments:
*     BAND1 = INTEGER (Given)
*        The lower waveband index.
*     BAND2 = INTEGER (Given)
*        The higher waveband index.
*     BETA = DOUBLE PRECISION (Given)
*        The emisivity spectral index.
*     LERR = REAL (Given)
*        Wavelength shift to apply to the spectral response curves
*        before use, in microns.
*     RERR = REAL (Given)
*        Gain factor to apply to the spectral response curves before
*        use.
*     LCK = INTEGER (Returned)
*        The number of knots in the returned cubic splines.
*     IPLKR = INTEGER (Returned)
*        A pointer to an array holding the positions of the knots for
*        the cubic spline which gives temperature as a function of flux
*        ratio.
*     IPLKR = INTEGER (Returned)
*        A pointer to an array holding the coefficients for the cubic
*        spline which gives temperature as a function of flux ratio.
*     IPLKF1 = INTEGER (Returned)
*        A pointer to an array holding the positions of the knots for
*        the cubic spline which gives observed model flux in BAND1
*        as a function of temperature.
*     IPLKF1 = INTEGER (Returned)
*        A pointer to an array holding the coefficients for the cubic
*        spline which gives observed model flux in BAND1 as a function
*        of temperature.
*     RATLO = DOUBLE PRECISION (Returned)
*        The lowest usable flux ratio value.
*     RATHI = DOUBLE PRECISION (Returned)
*        The highest usable flux ratio value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants.
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_ERR'          ! NAG error

*  Arguments Given:
      INTEGER BAND1
      INTEGER BAND2
      DOUBLE PRECISION BETA
      REAL LERR
      REAL RERR

*  Arguments Returned:
      INTEGER LCK
      INTEGER IPLKR
      INTEGER IPCKR
      INTEGER IPLKF1
      INTEGER IPCKF1
      DOUBLE PRECISION RATLO
      DOUBLE PRECISION RATHI

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION THI       ! Upper temperature limit.
      PARAMETER ( THI = 10000.0D0 )

      DOUBLE PRECISION TLO       ! Lower temperature limit.
      PARAMETER ( TLO = 30.0D0 )

      INTEGER NTEMP              ! No. of temperature increments.
      PARAMETER ( NTEMP = 45 )

*  Local Variables:
      DOUBLE PRECISION
     :       RATIO( NTEMP ),     ! Surface brightness ratio at each
                                 ! temperature.
     :       SB1( NTEMP ),       ! Surface brightness in BAND1 at each
                                 ! temperature.
     :       SB2( NTEMP ),       ! Surface brightness in BAND2 at each
                                 ! temperature.
     :       T,                  ! Current temperature
     :       TEMP( NTEMP ),      ! Temperatures.
     :       TFAC,               ! Factor between adjacent temperatures.
     :       WORK( 6*NTEMP + 16 )! NAG workspace.

      INTEGER
     :       I,                  ! Loop count.
     :       IFAIL               ! NAG error status.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Tell the user what is happening.
      CALL MSG_OUTIF( MSG__VERB, 'CTEMZ0_MSG1',
     :                '  Calculating flux ratio table...', STATUS )

*  Calculate the factor between adjacent temperatures. This factor is
*  chosen so that the given temperature range is covered in the given
*  number of steps.
      TFAC = ( THI/TLO )**( 1.0D0/DBLE( NTEMP - 1 ) )

*  Initialise the current temperature (in Kelvin) to be the lower bound.
      T = TLO

*  Initialise the maximum and minimum flux ratio values.
      RATLO = VAL__MAXD
      RATHI = VAL__MIND

*  Loop round each of the temperature values.
      DO I = 1, NTEMP

*  Evaluate the observed flux density in the first waveband at this
*  temperature, and store it.
         CALL CTEMZ1( T, BAND1, BETA, LERR, RERR, SB1( I ), STATUS )

*  Evaluate the observed flux density in the second waveband at this
*  temperature, and store it.
         CALL CTEMZ1( T, BAND2, BETA, LERR, RERR, SB2( I ), STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Store the flux ratio.
         RATIO( I ) = SB1( I )/SB2( I )

*  Update the maximum and minimum flux ratio values.
         RATLO = MIN( RATLO, RATIO( I ) )
         RATHI = MAX( RATHI, RATIO( I ) )

*  Store temperature.
         TEMP( I ) = T

*  Calculate the next temperature.
         T = T*TFAC

      END DO

*  Obtain workspace to hold the data defining the cubic splines.
      CALL PSX_CALLOC( NTEMP + 4, '_DOUBLE', IPLKR, STATUS )
      CALL PSX_CALLOC( NTEMP + 4, '_DOUBLE', IPCKR, STATUS )
      CALL PSX_CALLOC( NTEMP + 4, '_DOUBLE', IPLKF1, STATUS )
      CALL PSX_CALLOC( NTEMP + 4, '_DOUBLE', IPCKF1, STATUS )

*  Find a cubic spline which gives the temperature values as a function
*  of ratio values.
      IFAIL = -1
*      CALL E01BAF( NTEMP, RATIO, TEMP, %VAL( IPLKR ), %VAL( IPCKR ),
*     :             NTEMP + 4, WORK, 6*NTEMP + 16, IFAIL )

         STATUS = IRC__NAGER
         CALL ERR_REP('CTEMZ0_ERR0',
     :        'NAG not compiled into this version of IRAS90.',
     :        STATUS)
         GO TO 999

*  Report an error if something went wrong in the NAG routine.
      IF( IFAIL .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CTEMZ1_ERR1',
     :      'CTEMZ0: NAG error while fitting cubic spline to '//
     :      'temperature values.', STATUS )
         GO TO 999
      END IF

*  Find a cubic spline which gives BAND1 flux values as a function of
*  temperature.
      IFAIL = -1
*      CALL E01BAF( NTEMP, TEMP, SB1, %VAL( IPLKF1 ), %VAL( IPCKF1 ),
*     :             NTEMP + 4, WORK, 6*NTEMP + 16, IFAIL )

      STATUS = IRC__NAGER
      CALL ERR_REP('CTEMZ0_ERR0',
     :     'NAG not compiled into this version of IRAS90.',
     :     STATUS)
      GO TO 999

*  Report an error if something went wrong in the NAG routine.
      IF( IFAIL .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'L', I90__WAVEL( BAND1 ) )
         CALL ERR_REP( 'CTEMZ1_ERR1',
     :      'CTEMZ0: NAG error while fitting cubic spline to '//
     :      '^L um flux values.', STATUS )
         GO TO 999
      END IF

*  Return the no. of knots in each spline.
      LCK = NTEMP + 4

 999  CONTINUE

      END
