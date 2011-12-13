      SUBROUTINE KPS1_INMAT( IGRP, NNDF, STRIM, INDFS, NDIM, STATUS )
*+
*  Name:
*     KPS1_INMAT

*  Purpose:
*     Matches a group of NDFs for INTERLEAVE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_INMAT( IGRP, NNDF, STRIM, INDFS, NDIM, STATUS )

*  Description:
*     This routine matches NDF bounds and data types for a group of
*     NDFs.  The NDF identifiers and the commom dimensionality of the
*     NDFs are returned.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier of a group of NDFs to be examined.
*     NNDF = INTEGER (Given)
*        The number of NDFs in the group (size of IGRP).
*     STRIM = CHARACTER * ( * ) (Given)
*        Whether the common region should be the union ('PAD') or
*        intersection ('TRIM') of the input NDFs.  As presented to
*        NDF_MBNDN.
*     INDFS( NNDF ) = INTEGER (Returned)
*        NDF identifiers of matched NDF sections for the input NDFs.
*        The global status.
*     NDIM = INTEGER (Returned)
*        Common dimensionality of all the NDFs.

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council. All Rights Reserved.

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
*     MJC: Malcolm J. Currie (Starlink)
*     MBT: Mark Taylor (Starlink)
*     {enter_new_authors_here}

*  History:
*     2005 August 11 (MJC):
*        Original version based upon MBT's KPS1_MSA.
*     2006 April 12 (MJC):
*        Remove unused variable.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF system constants
      INCLUDE 'GRP_PAR'          ! GRP system constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER NNDF
      CHARACTER STRIM * ( * )

*  Arguments Returned:
      INTEGER INDFS( NNDF )
      INTEGER NDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( NDF__MXDIM )  ! Dimensions of NDF
      INTEGER I                  ! Loop variable
      CHARACTER NAME * ( GRP__SZNAM ) ! NDF name
      INTEGER NDIMC              ! Dimensionality of current NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      NDIM = 1

*  Try to open all the NDFs.
      DO I = 1, NNDF

*  Open the NDF.
         CALL NDG_NDFAS( IGRP, I, 'READ', INDFS( I ), STATUS )

*  Find its dimensionality.
         CALL NDF_DIM( INDFS( I ), NDF__MXDIM, DIM, NDIMC, STATUS )

*  Check that this has the same dimensionality as the first one in the
*  list, and bail out if not.
         IF ( I .EQ. 1 ) THEN
            NDIM = NDIMC
         ELSE
            IF ( NDIMC .NE. NDIM ) THEN
               CALL GRP_GET( IGRP, 1, 1, NAME, STATUS )
               CALL MSG_SETC( 'NDF0', NAME )
               CALL GRP_GET( IGRP, 1, I, NAME, STATUS )
               CALL MSG_SETC( 'NDF', NAME )
               CALL MSG_SETI( 'NDIM', NDIM )
               CALL MSG_SETI( 'NDIMC', NDIMC )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPS1_INMAT_ERR1', 'NDF ^NDF0 ' /
     :                       /'has ^NDIM dimensions and ^NDF ' /
     :                       /'has ^NDIMC---they must match.', STATUS )
               GO TO 999
            END IF
         END IF
      END DO

*  Match the bounds of the NDFs in the list.
      CALL NDF_MBNDN( STRIM, NNDF, INDFS, STATUS )

*  Error exit label.
  999 CONTINUE

      END
