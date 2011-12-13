      SUBROUTINE COF_STYPE( NDF, COMP, TYPE, BITPIX, FMTCNV, ITYPE,
     :                      STATUS )
*+
*  Name:
*     COF_STYPE

*  Purpose:
*     Sets NDF array types from the FITS headers and preferred data
*     type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_STYPE( NDF, COMP, TYPE, BITPIX, FMTCNV, ITYPE, STATUS )

*  Description:
*     This sets the data type for an NDF array component.  A preferred
*     type value is used, unless the component is the QUALITY (in which
*     case the routine returns without action). The type may also be
*     undefined, in which case the supplied FITS BITPIX defines the data
*     type.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF to be converted from the FITS file.
*     COMP = CHARACTER * ( * ) (Given)
*        The array component.  It must be 'Data', 'Quality' or
*        'Variance'.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the component.  If this is null, the BITPIX
*        defines the data type.
*     BITPIX = INTEGER (Given)
*        The FITS BITPIX value to define the data type when TYPE = ' '.
*        Thus for example -32 would generate _REAL and 16 makes _WORD.
*     FMTCNV = LOGICAL (Given)
*        Always return a floating point data type if TYPE is null?
*     ITYPE = CHARACTER * ( DAT__SZTYP ) (Returned)
*        The data type selected for the component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  ITYPE is always returned as '_UBYTE' if COMP is 'Quality' since
*     an NDF QUALITY component can only have this data type.

*  Copyright:
*     Copyright (C) 1996, 1998-1999 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 January 19 (MJC):
*        Original version.
*     10-DEC-1998 (DSB):
*        Return without action if COMP is 'Quality', since calling
*        NDF_STYPE would cause an error to be reported by the NDF
*        library.
*     8-JAN-1999 (DSB):
*        Added FMTCNV argument.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDF
      CHARACTER * ( * ) COMP
      CHARACTER * ( * ) TYPE
      INTEGER BITPIX
      LOGICAL FMTCNV

*  Arguments Returned:
      CHARACTER * ( * ) ITYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  QUALITY arrays must be unsigned byte, and cannot be changed.
      IF ( COMP .EQ. 'QUALITY' ) THEN
         ITYPE = '_UBYTE'

*  For DATA or VARIANCE components...
      ELSE

*  Determine the data type of the output array component.  A null
*  supplied type means use the data type of the FITS file (converted to
*  floating point if format conversion is being performed).
         IF ( TYPE .EQ. ' ' ) THEN
            CALL COF_BP2HT( BITPIX, FMTCNV, ITYPE, STATUS )

*  Just use the supplied data type.
         ELSE
            ITYPE = TYPE
         END IF

*  Change the type of the component.
         CALL NDF_STYPE( ITYPE, NDF, COMP, STATUS )

      END IF

      END
