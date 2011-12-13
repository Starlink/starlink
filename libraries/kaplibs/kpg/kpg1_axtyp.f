      SUBROUTINE KPG1_AXTYP( NDF, COMP, ATYPE, STATUS )
*+
*  Name:
*     KPG1_AXTYP

*  Purpose:
*     Determines the implementation type for NDF axis arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_AXTYP( NDF, COMP, ATYPE, STATUS )

*  Description:
*     The routine returns the highest precision required to process
*     the NDF axis component in all dimensions.  '_DOUBLE' is returned
*     if any of the arrays have type '_INTEGER' or 'DOUBLE'.

*  Arguments:
*     NDF = INTEGER (Given)
*        The NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        The name of the axis array component whose implementation type
*        is required: 'CENTRE', 'VARIANCE', or 'WIDTH'.
*     ATYPE = CHARACTER * ( * ) (Returned)
*        The implementation type of the axis arrays.  It is in uppercase
*        and is either '_REAL' or '_DOUBLE'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The NDF identifier must be valid and there must be an axis
*     structure.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     1991 June 17 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      CHARACTER * ( * ) ATYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :  DIM( NDF__MXDIM ),       ! Dimensions of the NDF
     :  I,                       ! Loop counter
     :  NDIM                     ! Dimensionality of the NDF

      CHARACTER
     :  XTYPE * ( NDF__SZTYP )   ! Processing type of the axis centres

*.

*    Check the inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise to the default.

      ATYPE = '_REAL'

*    Find the dimensionality of the NDF.

      CALL NDF_DIM( NDF, NDF__MXDIM, DIM, NDIM, STATUS )

*    Loop to find the highest precision required.

      DO  I = 1, NDIM
         CALL NDF_ATYPE( NDF, COMP, I, XTYPE, STATUS )
         IF ( XTYPE .EQ. '_INTEGER' .OR. XTYPE .EQ. '_DOUBLE' ) THEN
            IF ( ATYPE .NE. 'DOUBLE' ) ATYPE = '_DOUBLE'
         END IF
      END DO

      END
