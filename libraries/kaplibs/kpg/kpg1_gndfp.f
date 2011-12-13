      SUBROUTINE KPG1_GNDFP( PARNDF, PARPAX, NDIM, MODE, INDF, SDIM,
     :                       LBND, UBND, PERPAX, STATUS )
*+
*  Name:
*     KPG1_GNDFP

*  Purpose:
*     Gets an NDF or NDF section with a specified number of significant
*     dimensions, but also may select one further significant iteration
*     dimension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GNDFP( PARNDF, PARPAX, NDIM, MODE, INDF, SDIM, LBND,
*                      UBND, PERPAX, STATUS )

*  Description:
*     The supplied parameter name PARNDF is associated with an NDF
*     through the environment, and an identifier is obtained for the NDF
*     using the specified access mode.  Each axis of the NDF is checked
*     to see if is significant (i.e. has a size greater than 1).  The
*     index of each significant axis is returned in SDIM; and the bounds
*     of the axes are returned in LBND and UBND.  If the number of
*     significant axes is exactly one more than NDIM, one axis may be
*     chosen for later iteration over `planes' of NDIM dimensions.
*     If there are fewer than NDIM significant dimensions then the
*     insignificant dimensions are used (starting from the lowest) to
*     ensure that the required number of dimensions are returned.

*  Arguments:
*     PARNDF = CHARACTER * ( * ) (Given)
*        The parameter name to obtain the NDF.
*     PARPAX = CHARACTER * ( * ) (Given)
*        The parameter name to obtain the axis to iterate over the
*        `planes'.
*     NDIM = INTEGER (Given)
*        The number of dimensions required.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode required for the NDF.
*     INDF = INTEGER (Returned)
*        An identifier for the NDF.
*     SDIM( NDIM ) = INTEGER (Returned)
*        The indices of the significant dimensions.
*     LBND( NDIM ) = INTEGER (Returned)
*        The lower bounds of the NDF.
*     UBND( NDIM ) = INTEGER (Returned)
*        The upper bounds of the NDF.
*     PERPAX = INTEGER (Returned)
*        The index of the axis perpendicular to the `plane' if there is
*        exactly one more significant axis than NDIM, otherwise set to
*        zero.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 June 21 (MJC):
*        Original version based upon BLOCK code by DSB.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      CHARACTER * ( * ) PARNDF
      CHARACTER * ( * ) PARPAX
      INTEGER NDIM
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      INTEGER INDF
      INTEGER SDIM( NDIM )
      INTEGER LBND( NDF__MXDIM )
      INTEGER UBND( NDF__MXDIM )
      INTEGER PERPAX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER AXSUM              ! Sum of all significant pixel axes
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER I                  ! Loop counter
      INTEGER NAX                ! Number of significant pixel axes
      INTEGER NDIMS              ! Number of dimensions in NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the NDF containing the input data.
      CALL LPG_ASSOC( PARNDF, MODE, INDF, STATUS )

*  Obtain the bounds of the NDF.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIMS, STATUS )

*  Count the number of significant pixel axes, storing the indices of
*  the first two in SDIM.  Also note the index of the first
*  insignificant pixel axis.
      AXSUM = 0
      NAX = 0
      PERPAX = 0
      DO I = 1, NDIMS
         DIM( I ) = UBND( I ) - LBND( I ) + 1
         IF ( DIM( I ) .GT. 1 ) THEN
            NAX = NAX + 1
            IF ( NAX .LE. NDIM ) SDIM( NAX ) = I
            AXSUM = AXSUM + I
         ELSE IF ( PERPAX .EQ. 0 ) THEN
            PERPAX = I
         END IF
      END DO

*  If there is no insignificant axis, use an additional trailing axis.
      IF ( PERPAX .EQ. 0 ) PERPAX = NDIMS + 1

*  If there are exactly one more than the required number of significant
*  pixel axes, see which ones span the `plane' to be smoothed.
      IF ( NAX .EQ. NDIM + 1 ) THEN
         CALL PAR_GDR1I( PARPAX, NDIM, SDIM, 1, NDIMS, .TRUE., SDIM,
     :                   STATUS )

*  Find the index of the pixel axis which is perpendicular to the
*  smoothing `plane'.
         PERPAX = AXSUM - SDIM( 1 ) - SDIM( 2 )

*  If the NDF does not have exactly one more significant axis, find
*  whether or not there are no more than the required number of
*  significant dimensions and which ones they are.
      ELSE
         CALL KPG1_SDIMP( INDF, NDIM, SDIM, STATUS )
      END IF

      END
