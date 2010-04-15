      SUBROUTINE MRESA0( SIZE, NRESP, PRESP, WAVEL0, DWAVEL, VERR, LERR,
     :                   DATA, VAR, AXCEN, AXVAR, STATUS )
*+
*  Name:
*     MRESA0

*  Purpose:
*     Store spectral response data in the output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MRESA0( SIZE, NRESP, PRESP, WAVEL0, DWAVEL, VERR, LERR,
*                  DATA, VAR, AXCEN, AXVAR, STATUS )

*  Description:
*     The supplied responses are normalised to a peak of unity in each
*     band. These values are multiplied by the peak response in each
*     band to get values which can be compared between wavebands. The
*     elements are equally spaced in wavelength, with a constant error
*     on the wavelength at each element, given by LERR. The fractional
*     error on the response values are (given by VERR) is converted into
*     a variance for each element.

*  Arguments:
*     SIZE = INTEGER (Given)
*        The no. of elements in the response curve.
*     NRESP( SIZE ) = REAL (Given)
*        The response data, normalised to a peak of unity in each band.
*     PRESP = REAL (Given)
*        The peak response in each band, normalised to a peak of unity
*        throughout all 4 bands.
*     WAVEL0 = REAL (Given)
*        The wavelength of the first element, in microns.
*     DWAVEL = REAL (Given)
*        The wavelength increment between elements, in microns.
*     VERR = REAL (Given)
*        The fractional uncertainty in each response value.
*     LERR = REAL (Given)
*        The uncertainty in each wavelength value, in microns.
*     DATA( SIZE ) = REAL (Returned)
*        The response curve normalised to a peak of unity throughout all
*        4 bands.
*     VAR( SIZE ) = REAL (Returned)
*        The variance in the DATA values.
*     AXCEN( SIZE ) = REAL (Returned)
*        The wavelength at the centre of each element, in microns.
*     AXVAR( SIZE ) = REAL (Returned)
*        The variance in each wavelength value, in square microns.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER SIZE
      REAL NRESP( SIZE )
      REAL PRESP
      REAL WAVEL0
      REAL DWAVEL
      REAL VERR
      REAL LERR

*  Arguments Returned:
      REAL DATA( SIZE )
      REAL VAR( SIZE )
      REAL AXCEN( SIZE )
      REAL AXVAR( SIZE )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each element in the spectral response curve.
      DO I = 1, SIZE

*  The response value stored in the NDF is normalised so that the peak
*  value throughtout all 4 bands is unity.
         DATA( I ) = PRESP*NRESP( I )

*  Assign a variance for the reponse value, based on the fractional
*  uncertainty.
         VAR( I ) = ( VERR*DATA( I ) )**2

*  The elements of the response curve are equally spaced in wavelength.
         AXCEN( I ) = WAVEL0 + DWAVEL*( I - 1 )

*  The error on the wavelength is independant of wavelength.
         AXVAR( I ) = LERR**2

      END DO

      END
