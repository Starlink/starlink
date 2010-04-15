      SUBROUTINE CTEMZ1( T, BAND, BETA, LERR, RERR, FLUX, STATUS )
*+
*  Name:
*     CTEMZ1

*  Purpose:
*     Calculate the observed model flux per steradian at a given
*     temperature.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTEMZ1( T, BAND, BETA, LERR, RERR, FLUX, STATUS )

*  Description:
*     The blackbody function is multiplied by the spectral response and
*     the optical depth factor, and the product is integrated over all
*     frequencies. The critical frequency is assumed to be 1.0E12 Hz.

*  Arguments:
*     T = DOUBLE PRECISION (Given)
*        The temperature at which the intregral is required, in Kelvin.
*     BAND = INTEGER (Given)
*        Waveband index (1-4) for which spectral response data is
*        required.
*     BETA = DOUBLE PRECISION (Given)
*        The emmisivity spectral index.
*     LERR = REAL (Given)
*        The shift in wavelength to apply to the standard detector
*        response curves, in microns.
*     RERR = REAL (Given)
*        The multiplicative factor to apply to the standard detector
*        response curves.
*     FLUX = DOUBLE PRECISION (Returned)
*        The observed flux (in 1.0E16 pW/m**2) per steradian.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-APR-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_ERR'          ! NAG error

*  Arguments Given:
      DOUBLE PRECISION T
      INTEGER BAND
      DOUBLE PRECISION BETA
      REAL LERR
      REAL RERR

*  Arguments Returned:
      DOUBLE PRECISION FLUX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION CTEMP0    ! Blackbody function.

*  Local Constants:
      INTEGER MXSIZ              ! Max. no. of points per curve.
      PARAMETER ( MXSIZ = 36 )

*  Local Variables:
      DOUBLE PRECISION
     :      CK( MXSIZ + 4 ),     ! Cubic spline co-efficients.
     :      DWAVEL( I90__BANDS ),! Wavelength increment between response
                                 ! values, in microns
     :      INTGND( MXSIZ ),     ! Response data in increasing order.
     :      LK( MXSIZ + 4 ),     ! Knot positions of cubic spline.
     :      NRESP( MXSIZ, I90__BANDS ),! Response normalised to a
                                 ! peak in each band of unity. Taken
                                 ! from Exp. Supp. Table II.C.5
                                 ! "Relative system resp." (padded with
                                 ! extra zeros).
     :      NU( MXSIZ ),         ! Frequency in units of 1.0E12 Hz.
     :      WAVEL0( I90__BANDS ),! Wavelength of first response value
     :      WORK( 6*MXSIZ + 16 ) ! NAG real workspace.

      INTEGER
     :      I,                   ! Do loop index
     :      IFAIL,               ! NAG error status
     :      J,                   ! Do loop index
     :      SIZE( I90__BANDS )   ! No. of response values for each band

*  Local Data:
      DATA DWAVEL / 0.5, 0.5, 3.0, 5.0 /

      DATA NRESP   / 0.000, 0.000, 0.000, 0.008, 0.535, 0.689, 0.735,
     : 0.815, 0.900, 0.904, 0.834, 0.816, 0.793, 0.854, 0.938, 0.991,
     : 1.000, 0.934, 0.388, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     : 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     : 0.000, 0.000,

     :               0.000, 0.000, 0.007, 0.101, 0.288, 0.388, 0.452,
     : 0.521, 0.562, 0.626, 0.683, 0.729, 0.778, 0.832, 0.912, 0.914,
     : 0.938, 0.933, 0.875, 0.910, 1.000, 0.911, 0.840, 0.763, 0.749,
     : 0.829, 0.914, 0.790, 0.877, 0.558, 0.274, 0.069, 0.012, 0.000,
     : 0.000, 0.000,

     :               0.000, 0.000, 0.000, 0.010, 0.036, 0.068, 0.174,
     : 0.315, 0.483, 0.585, 0.658, 0.716, 0.824, 0.915, 0.987, 0.990,
     : 1.000, 0.946, 0.713, 0.531, 0.174, 0.047, 0.000, 0.000, 0.000,
     : 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     : 0.000, 0.000,

     :               0.000, 0.000, 0.000, 0.010, 0.113, 0.306, 0.505,
     : 0.695, 0.824, 0.947, 0.939, 1.000, 0.631, 0.319, 0.195, 0.106,
     : 0.053, 0.010, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     : 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
     : 0.000, 0.000 /

      DATA SIZE / 22, 36, 25, 20 /

      DATA WAVEL0 / 6.0, 15.0, 21.0, 55.0 /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up an array containing the integrand value at each frequency.
*  Values must be stored in order of increasing frequency (i.e.
*  decreasing wavelength). Set up another array containing the
*  frequency (in units of 1.0D12 Hz) at each point.
      DO J = 1, SIZE( BAND )
         I = SIZE( BAND ) - J
         NU( J ) = 299.79/( WAVEL0( BAND ) + I*DWAVEL( BAND ) + LERR )
         INTGND( J ) = RERR*NRESP( I + 1, BAND )* (NU( J )**BETA) *
     :                 CTEMP0( NU( J ), T )
      END DO

*  Find a cubic spline which interpolates the integrand values.
      IFAIL = -1
*      CALL E01BAF( SIZE( BAND ), NU, INTGND, LK, CK, MXSIZ + 4, WORK,
*     :             6*MXSIZ + 16, IFAIL )

      STATUS = IRC__NAGER
      CALL ERR_REP('CTEMZ1_ERR0',
     :     'NAG not compiled into this version of IRAS90.',
     :     STATUS)
      GO TO 999

*  Report an error if something went wrong in the NAG routine.
      IF( IFAIL .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'L', I90__WAVEL( BAND ) )
         CALL MSG_SETR( 'T', REAL( T ) )
         CALL ERR_REP( 'CTEMZ1_ERR1',
     :      'CTEMZ1: NAG error while fitting cubic spline to flux '//
     :      'density observed from a ^T K source (^L um band)', STATUS )
         GO TO 999
      END IF

*  Now evaluate the definite integral under the cubic spline.
      IFAIL = -1
*      CALL E02BDF( SIZE( BAND ) + 4, LK, CK, FLUX, IFAIL )

      STATUS = IRC__NAGER
      CALL ERR_REP('CTEMZ1_ERR0',
     :     'NAG not compiled into this version of IRAS90.',
     :     STATUS)
      GO TO 999

*  Report an error if something went wrong in the NAG routine.
      IF( IFAIL .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'L', I90__WAVEL( BAND ) )
         CALL MSG_SETR( 'T', REAL( T ) )
         CALL ERR_REP( 'CTEMZ1_ERR2',
     :      'CTEMZ1: NAG error while integrating the flux '//
     :      'density observed from a ^T K source (^L um band)', STATUS )
      END IF

 999  CONTINUE

      END
