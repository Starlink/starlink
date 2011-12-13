      SUBROUTINE COF_ATYPC( NDF, COMP, IAXIS, TYPE, DATCOD, ITYPE,
     :                      STATUS )
*+
*  Name:
*     COF_ATYPC

*  Purpose:
*     Sets NDF axis-array types from the FITS headers and preferred data
*     type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_ATYPC( NDF, COMP, IAXIS, TYPE, DATCOD, ITYPE, STATUS )

*  Description:
*     This sets the data type for an NDF axis-array component.  A
*     preferred type value may be used.   The type may also be
*     undefined, in which case the supplied FITS binary-table datacode
*     defines the data type.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF to be converted from the FITS file.
*     COMP = CHARACTER * ( * ) (Given)
*        The axis array component.  It must be 'Centre', 'Variance',
*        or 'Width'.  Case is ignored.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis whose array type is to be set.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the component.  If this is null, the BITPIX
*        defines the data type.  Case is ignored.
*     DATCOD = INTEGER (Given)
*        The FITSIO data-type code from a FITS binary-table header to
*        be used to define the output HDS data type when TYPE = ' '.
*        Thus for example, 42 would generate _REAL, and 21 makes _WORD.
*        See the FITSIO manual for a full list.  Generally, the meaning
*        need not be known, as one merely passes the value returned by
*        FTGTCL into this routine.
*     ITYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The data type selected for the component.
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
*     1996 January 19 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS
      CHARACTER * ( * ) TYPE
      INTEGER DATCOD

*  Arguments Returned:
      CHARACTER * ( * ) ITYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 8 ) UCOMP    ! Uppercase form of the supplied
                                 ! component name

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert supplied component name to uppercase, using a local variable.
      UCOMP = COMP
      CALL CHR_UCASE( UCOMP )

*  Determine the data type of the output array component.  A null
*  supplied type means use the data type of the FITS file.  Convert the
*  FITSIO datacode into an HDS type.
      IF ( TYPE .EQ. ' ' ) THEN
         CALL COF_FD2HT( DATCOD, ITYPE, STATUS )

*  Just use the supplied data type.
      ELSE
         ITYPE = TYPE
      END IF

*  Change the type of the component.
      CALL NDF_ASTYP( ITYPE, NDF, COMP, IAXIS, STATUS )

      END
