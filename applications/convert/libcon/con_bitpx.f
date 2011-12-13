      SUBROUTINE CON_BITPX( HDSTYP, BITPIX, STATUS )
*+
*  Name:
*     CON_BITPX

*  Purpose:
*     Derives the FITS BITPIX for HDS numeric data types.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_BITPX( HDSTYP, BITPIX, STATUS )

*  Description:
*     This routine uses a lookup table to return the FITS BITPIX value
*     corresponding to an HDS numeric data type.

*  Arguments:
*     HDSTYP = CHARACTER * ( * ) (Given)
*        The HDS numeric data type.
*     BITPIX = INTEGER (Returned)
*        The number of bits per data value.  A negative value indicates
*        floating-point data.  In other words the FITS BITPIX.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     1992 September 16 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-sytem constants

*  Arguments Given:
      CHARACTER * ( * ) HDSTYP

*  Arguments Returned:
      INTEGER BITPIX

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXTYP             ! Number of numeric data types
      PARAMETER( MAXTYP=7 )

*  Local Variables:
      INTEGER FBITS( MAXTYP )    ! BITPIX values
      CHARACTER * ( DAT__SZTYP ) HTYPES( MAXTYP ) ! HDS numeric types
      INTEGER I                  ! Loop counter

*  Local Data:
      DATA FBITS /8, 16, 32, -32, -64, 8, 16/
      DATA HTYPES/'_BYTE','_WORD','_INTEGER','_REAL','_DOUBLE',
     :            '_UBYTE','_UWORD'/

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until a match is found.  Copy the type string to the output
*  argument. If none is found an error report will be made, otherwise
*  the routine exits.
      DO I = 1, MAXTYP
         IF ( HDSTYP .EQ. HTYPES( I ) ) THEN
            BITPIX = FBITS( I )
            GOTO 999
         END IF
      END DO

*  If an error occurred, then report a contextual message.
      STATUS = SAI__ERROR
      CALL MSG_SETC( 'HDSTYP', HDSTYP )
      CALL ERR_REP( 'CON_BITPX_ERR',
     :  'Invalid HDS data type ^HDSTYP.  Unable to convert it to a '/
     :  /'FITS BITPIX value.', STATUS )

  999 CONTINUE

      END
