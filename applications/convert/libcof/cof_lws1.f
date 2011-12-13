      SUBROUTINE COF_LWS1( NOBS, PREC, OWAVEL, OERROR, NWAVEL, WAVEL,
     :                     ERROR, IP, STATUS )
*+
*  Name:
*     COF_LWS1

*  Purpose:
*     Finds the distinct wavelength values in array and orders them.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_LWS1( NOBS, PREC, OWAVEL, OERROR, NWAVEL, WAVEL, ERROR,
*                    IP, STATUS )

*  Description:
*     This routine creates arrays sorted in ascending order of the
*     distinct wavelengths and the corresponding errors, for a set of
*     observations from an LWS AN data product.  The measure of
*     distinctiveness is given by a maximum-allowed difference.

*  Arguments:
*     NOBS = INTEGER (Given)
*        Number of observations.
*     PREC = REAL (Given)
*        The maximum-allowed difference in wavelength for two values to
*        be regarded as identical.  Non-positive values are treated as
*        the machine precision.
*     OWAVEL( NOBS ) = REAL (Given)
*        The wavelengths for each of the observations.
*     OERROR( NOBS ) = REAL (Given)
*        The uncertainty in the wavelength for each of the observations.
*     NWAVEL = INTEGER (Returned)
*        Number of distinct wavelengths found in the OWAVEL array.
*     WAVEL( NOBS ) = REAL (Returned)
*        The sorted distinct list wavelengths.
*     ERROR( NOBS ) = REAL (Returned)
*        The list of wavelength errors corresponding to the each of the
*        elements of WAVEL .
*     IP( NOBS ) = REAL (Returned)
*        The indices to the sorted wavelengths and errors.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 May 25 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER NOBS
      REAL PREC
      REAL OWAVEL( NOBS )
      REAL OERROR( NOBS )

*  Arguments Returned:
      INTEGER NWAVEL
      REAL WAVEL( NOBS )
      REAL ERROR( NOBS )
      INTEGER IP( NOBS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop for the observations
      REAL LPREC                 ! Internal value of the distinctiveness
                                 ! precsiuon

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the count of distinct wavelengths.
      NWAVEL = 0

*  Find the distinctiveness criterion.
      LPREC = PREC
      IF ( PREC .LE. 0.0 ) LPREC = VAL__EPSR

*  Sort the array of observed wavelengths into ascending order.  There
*  are likely to be more-efficient strategies, but unless that is
*  significant problem, we'll use this approach for simplicity.  Use
*  the index array to make the interchanges to the observed wavelength
*  errors as well as the wavelengths themselves.
      CALL PDA_QSIAR( NOBS, OWAVEL, IP )

      IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop through each observation.
*  ==============================

*  By definition the first distinct wavelength and error are known.
         WAVEL( 1 ) = OWAVEL( IP( 1 ) )
         ERROR( 1 ) = OERROR( IP( 1 ) )

*  Initialise the the counter of distinct wavelengths.
         NWAVEL = 1

*  The job is done if there is only one wavelength.  (As far as we know
*  this cannot happen, so this is just some defensive programming.)
         IF ( NOBS .GT. 1 ) THEN

*  Loop through the array of sorted wavelengths.
            DO I = 2, NOBS

*  See if the next observation has a distinct wavelength.
               IF ( ( OWAVEL( IP( I ) ) - WAVEL( NWAVEL ) ) .GT.
     :              LPREC * OWAVEL( IP( I ) ) ) THEN

*  It does, so increment the counter and assign the wavelength and its
*  assoicated error to the arrays of distinct values.
                  NWAVEL = NWAVEL + 1
                  WAVEL( NWAVEL ) = OWAVEL( IP( I ) )
                  ERROR( NWAVEL ) = OERROR( IP( I ) )
               END IF
            END DO
         END IF
      END IF

      END
