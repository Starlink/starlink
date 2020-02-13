      SUBROUTINE KPS1_MSBS( IGRP, NNDF, COMP, STRIM, OPARAM, ITYPE,
     :                      INDFS, ONDF, STATUS )
*+
*  Name:
*     KPS1_MSBS

*  Purpose:
*     Obtains identifiers to matched NDF sections for MSTATS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MSBS( IGRP, NNDF, COMP, STRIM, OPARAM, ITYPE, INDFS,
*                     ONDF, STATUS )

*  Description:
*     Matches NDF bounds and data types for a given component of a
*     group of NDFs.  The NDF identifiers and the pointers to the
*     mapped data, as well as an identifier for a newly created output
*     NDF propagated from one of the input ones, are returned.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier of a group of NDFs to be examined.
*     NNDF = INTEGER (Given)
*        The number of NDFs in the group (size of IGRP).
*     COMP = CHARACTER * ( * ) (Given)
*        The component of the NDFs which is to be interrogated.  May
*        be Data, Variance, Error or Quality.
*     STRIM = CHARACTER * ( * ) (Given)
*        Whether the common region should be the union ('PAD') or
*        intersection ('TRIM') of the input NDFs.  As presented to
*        NDF_MBNDN.
*     OPARAM = CHARACTER * ( * ) (Given)
*        The name of an ADAM parameter via which to retrieve the name
*        of the new output NDF returned in ONDF.
*     ITYPE = CHARACTER * ( * ) (Returned)
*        The common data type of the components.  Will be '_REAL'
*        or '_DOUBLE'.
*     INDFS( NNDF ) = INTEGER (Returned)
*        NDF identifiers of matched NDF sections for the input NDFs.
*     ONDF = INTEGER (Returned)
*        The NDF identifier of a new NDF for output.  This is propagated
*        from the first in the list of input NDFs, with bounds adjusted
*        by trimming or padding, if necessary.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008, 2009 Science & Technology Facilities Council.
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
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MBT: Mark Taylor (Starlink)
*     MJC: Malcolm J. Currie (Starlink)
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     2008 April 13 (MJC):
*        Original version based upon MBT's KPS1_MSA.
*     2009 November 24 (MJC):
*        Initialise returned identifiers.  Use NDF_MSG rather than
*        GRP_GET and MSG_SETC to define tokens for the NDF names in
*        the error message.
*     13-FEB-2020 (DSB):
*        Support huge NDFs.
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
      CHARACTER*( * ) COMP
      CHARACTER*( * ) STRIM
      CHARACTER*( * ) OPARAM

*  Arguments Returned:
      CHARACTER ITYPE * ( * )
      INTEGER INDFS( NNDF )
      INTEGER ONDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER*8 DIM( NDF__MXDIM )! Dimensions of NDF
      CHARACTER*( NDF__SZFTP ) DTYPE ! Full data type name
      INTEGER I                  ! Loop variable
      CHARACTER*( GRP__SZNAM ) NAME ! NDF name
      INTEGER NDIM               ! Dimensionality of current NDF
      INTEGER NDIM0              ! Common dimensionality of input NDFs

*.

*  Set the returned identifiers to null.
      ONDF = NDF__NOID
      DO I = 1, NNDF
         INDFS( I ) = NDF__NOID
      END DO

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to open all the NDFs.
      DO I = 1, NNDF

*  Open an NDF.
         CALL NDG_NDFAS( IGRP, I, 'READ', INDFS( I ), STATUS )

*  Find its dimensionality.
         CALL NDF_DIM8( INDFS( I ), NDF__MXDIM, DIM, NDIM, STATUS )

*  Check that this has the same dimensionality as the first one in the
*  list, and bail out if not.
         IF ( I .EQ. 1 ) THEN
            NDIM0 = NDIM
         ELSE
            IF ( NDIM .NE. NDIM0 ) THEN
               CALL NDF_MSG( 'NDF0', INDFS( 1 ) )
               CALL NDF_MSG( 'NDF', INDFS( I ) )
               CALL MSG_SETI( 'NDIM0', NDIM0 )
               CALL MSG_SETI( 'NDIM', NDIM )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'KPS1_MSBS_ERR1', 'NDF ^NDF0 ' //
     :                       'has ^NDIM0 dimensions and ^NDF ' //
     :                       'has ^NDIM; they must match.', STATUS )
               GO TO 99
            END IF
         END IF
      END DO

*  Match the bounds of the NDFs in the list.
      CALL NDF_MBNDN( STRIM, NNDF, INDFS, STATUS )

*  Match the data types of the selected component arrays of all the NDFs
*  in the list.
      CALL NDF_MTYPN( '_REAL,_DOUBLE', NNDF, INDFS, COMP, ITYPE, DTYPE,
     :                STATUS )

*  Propagate the first NDF in the list to a new output NDF.
      CALL NDF_PROP( INDFS( 1 ), 'WCS,AXIS', OPARAM, ONDF, STATUS )

*  Set new output NDF array component data types to the correct output
*  type.
      CALL NDF_STYPE( ITYPE, ONDF, 'DATA', STATUS )
      CALL NDF_STYPE( ITYPE, ONDF, 'VARIANCE', STATUS )

*  Error exit label.
   99 CONTINUE

      END
