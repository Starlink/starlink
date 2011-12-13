      SUBROUTINE CCD1_OVLAP( NDF1, NDF2, NPIX, STATUS )
*+
*  Name:
*     CCD1_OVLAP

*  Purpose:
*     Find the number of pixels overlap between two NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_OVLAP( NDF1, NDF2, NPIX, STATUS )

*  Description:
*     The routine returns a count of the number of pixels overlap
*     between two NDFs whose identifiers are supplied. If the NDFs do
*     not overlap, a value of zero is returned.

*  Arguments:
*     NDF1 = INTEGER (Given)
*        First NDF identifier.
*     NDF2 = INTEGER (Given)
*        Second NDF identifier.
*     NPIX = INTEGER (Returned)
*        Number of pixels overlap.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     A value of zero will be returned for the NPIX argument if this
*     routine returns unsuccessfully for any reason.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ global constants

*  Arguments Given:
      INTEGER NDF1
      INTEGER NDF2

*  Arguments Returned:
      INTEGER NPIX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIMPIX             ! Pixels in common for a dimension
      INTEGER I                  ! Loop counter
      INTEGER LBND1( NDF__MXDIM ) ! Lower bounds of first NDF
      INTEGER LBND2( NDF__MXDIM ) ! Lower bounds of second NDF
      INTEGER NDIM1              ! Number of dimensions (first NDF)
      INTEGER NDIM2              ! Number of dimensions (second NDF)
      INTEGER UBND1( NDF__MXDIM ) ! Upper bounds of first NDF
      INTEGER UBND2( NDF__MXDIM ) ! Upper bounds of second NDF

*.

*  Set an initial value of zero for the NPIX argument.
      NPIX = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the bounds of each NDF.
      CALL NDF_BOUND( NDF1, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )
      CALL NDF_BOUND( NDF2, NDF__MXDIM, LBND2, UBND2, NDIM2, STATUS )

*  Loop to compare the extent of each relevant dimension.
      NPIX = 1
      DO 1 I = 1, MAX( NDIM1, NDIM2 )

*  Find the number of pixels in common in each dimension.
         DIMPIX = MIN( UBND1( I ), UBND2( I ) ) -
     :            MAX( LBND1( I ), LBND2( I ) ) + 1

*  If there are no pixels in common, then return NPIX = 0.
         IF ( DIMPIX .LT. 1 ) THEN
            NPIX = 0
            GO TO 2

*  Otherwise, accumulate the count of the total number of pixels in
*  common.
         ELSE
            NPIX = NPIX * DIMPIX
         END IF
 1    CONTINUE
 2    CONTINUE

*  If an error occurred, return an NPIX value of zero.
      IF ( STATUS .NE. SAI__OK ) THEN
         NPIX = 0
      END IF

      END
* $Id$
