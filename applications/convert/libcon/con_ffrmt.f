      SUBROUTINE CON_FFRMT( BITPIX, UNSIGN, BPV, FMTIN, STATUS )
*+
*  Name:
*     CON_FFRMT

*  Purpose:
*     Obtains the input HDS data format for a sequential FITS-like file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_FFRMT( BITPIX, UNSIGN, BPV, FMTIN, STATUS )

*  Description:
*     This is a server routine for ASCII2NDF and UNF2NDF.  It packages
*     up the operations required to define the input data format
*     (HDS types) from information given in a FITS-like header.

*  Arguments:
*     BITPIX = INTEGER (Given)
*        The value of the BITPIX keyword in the FITS header, i.e. the
*        number of bits per data value.  If it is negative this
*        indicates floating-point data following the FITS-like header.
*     UNSIGN = LOGICAL (Given)
*        If true the data are unsigned as given by the UNSIGNED being
*        true in the FITS header.
*     BPV = INTEGER (Returned)
*        The number of bytes per data value.
*     FMTIN = CHARACTER * ( * ) (Returned)
*        The HDS format of the data in the sequential file with the
*        FITS-like header.
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
*     1992 September 18 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER BITPIX
      LOGICAL UNSIGN

*  Arguments Returned:
      INTEGER BPV
      CHARACTER * ( * ) FMTIN

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Use BITPIX to determine the data format.
*  ========================================

*  Find the number of bytes per data value, note the absolute because
*  BITPIX can be negative.
      BPV = ABS( BITPIX / 8 )

*  Find the input format type.
      IF ( BITPIX .LT. 0 ) THEN

*  Firstly, assign the floating-point values.
         IF ( BPV .EQ. 4 ) THEN
            FMTIN = '_REAL'
         ELSE IF ( BPV .EQ. 8 ) THEN
            FMTIN = '_DOUBLE'
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'INVTYP',
     :        'The FITS object has an invalid value for BITPIX.',
     :        STATUS )
         END IF
      ELSE

*  Secondly, the integer types.
         IF ( BPV .EQ. 1 .AND. UNSIGN ) THEN
            FMTIN = '_UBYTE'
         ELSE IF ( BPV .EQ. 2 .AND. UNSIGN ) THEN
            FMTIN = '_UWORD'
         ELSE IF ( BPV .EQ. 1 ) THEN
            FMTIN = '_BYTE'
         ELSE IF ( BPV .EQ. 2 ) THEN
            FMTIN = '_WORD'
         ELSE IF ( BPV .EQ. 4 ) THEN
            FMTIN = '_INTEGER'
          ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'INVTYP',
     :        'The FITS object has an invalid value for BITPIX.',
     :        STATUS )
         END IF
      END IF

      END

