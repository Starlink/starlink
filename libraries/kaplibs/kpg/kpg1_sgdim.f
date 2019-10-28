      SUBROUTINE KPG1_SGDIM( NDF, NDIM, DIMV, STATUS )
*+
*  Name:
*     KPG1_SGDIM

*  Purpose:
*     Determines the number of significant dimensions in an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SGDIM( NDF, NDIM, DIMV, STATUS )

*  Description:
*     This routine finds the number of significant dimensions, i.e.
*     those with greater than one element, in an NDF.  If the number
*     found is not equal to a specified number a bad status, SAI_ERROR,
*     is returned.  Likewise should there be no significant dimensions.
*     The significant dimensions are recorded and returned.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     NDIM = INTEGER (Given)
*        The desired number of dimensions.
*     DIMV( NDIM ) = INTEGER (Returned)
*        The significant dimensions i.e. the ones that are greater than
*        one.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     -  The NDF must exist.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 February 11 (MJC):
*        Original version.
*     1992 April 16 (MJC):
*        Added NDF token to the error reports.
*     15-JUN-1998 (DSB):
*        Modified to avoid addressing DIMV outside the range [1-NDIM].
*        Converted to modern style layout.
*     4-OCT-2019 (DSB):
*        Changed to use 8-byte NDF interface.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER NDF
      INTEGER NDIM

*  Arguments Returned:
      INTEGER DIMV( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ACTDIM             ! Actual number of dimensions in NDF
      INTEGER*8 DIM( NDF__MXDIM )! The NDF dimensions
      INTEGER I                  ! Loop counter
      INTEGER SIGDIM             ! Number of signifcant dimensions

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the number of dimensions.
      CALL NDF_DIM8( NDF, NDF__MXDIM, DIM, ACTDIM, STATUS )

*  Initialise counter.
      SIGDIM = 0

*  Initialise the significant dimensions.
      DO I = 1, NDIM
         DIMV( I ) = 0
      END DO

*  Loop for each dimension.
      DO I = 1, ACTDIM

*  Is the dimension significant?
         IF ( DIM( I ) .GT. 1 ) THEN

*  Yes, so add it to the total.
            SIGDIM = SIGDIM + 1

*  Record the dimension if it is within range.
           IF( SIGDIM .LE. NDIM ) DIMV( SIGDIM ) = I

         END IF

      END DO

*  Look for error conditions.
*  ==========================

*  Must have at least one significant dimension.
      IF ( SIGDIM .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'KPG1_SGDIM_NOSIG',
     :     'All dimensions are one in the NDF ^NDF.', STATUS )

*  The effective dimensionality of the NDF must equal the prescribed
*  number.
      ELSE IF ( SIGDIM .NE. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL ERR_REP( 'KPG1_SGDIM_NODIM',
     :     'The NDF ^NDF is not ^NDIM-dimensional.', STATUS )
      END IF

      END
