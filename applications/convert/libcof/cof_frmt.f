      SUBROUTINE FTS1_FRMT( BITPIX, IEEE, FMTCNV, BPV, FMTIN, FMTOUT,
     :                      STATUS )
*+
*  Name:
*     FTS1_FRMT

*  Purpose:
*     Obtain the input and output data format for a FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_FRMT( BITPIX, IEEE, FMTCNV, BPV, FMTIN, FMTOUT,
*    :                STATUS )

*  Description:
*     This is a server routine for FITSIN.  It packages up the
*     operations required to define the input and output data formats
*     (HDS types).

*  Arguments:
*     BITPIX = INTEGER (Given)
*        The value of the BITPIX keyword in the FITS header, i.e. the
*        number of bits per data value.  If it is negative this
*        indicates an IEEE-format file.
*     IEEE = LOGICAL (Given)
*        If true the FITS data are in IEEE floating-point format.
*     FMTCNV = LOGICAL (Given)
*        If true, format conversion from the integer FITS data to
*        the real output data array is required.
*     BPV = INTEGER (Returned)
*        The number of bytes per data value.
*     FMTIN = CHARACTER * ( * ) (Returned)
*        The HDS format of the data in the FITS file.
*     FMTOUT = CHARACTER * ( * ) (Returned)
*        The destination HDS format of the data array in the output
*        file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     1990 November 18 (MJC):
*        Original version.
*     2012 April 30 (MJC):
*        Add 64-bit integer.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER BITPIX

      LOGICAL IEEE
      LOGICAL FMTCNV

*  Arguments Returned:
      INTEGER BPV
      CHARACTER * ( * ) FMTIN
      CHARACTER * ( * ) FMTOUT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Use BITPIX to determine the data format.
*  ========================================

*  Find the number of bytes per data value, note the absolute because
*  BITPIX can be negative.
       BPV = ABS( BITPIX / 8 )

*  Find the input format type.
       IF ( IEEE ) THEN

*  First, the floating-point values.
          IF ( BPV .EQ. 4 ) THEN
             FMTIN = '_REAL'
          ELSE IF ( BPV .EQ. 8 ) THEN
             FMTIN = '_DOUBLE'
          ELSE
             STATUS = SAI__ERROR
             CALL ERR_REP( 'INVTYP',
     :         'The FITS object has an invalid value for BITPIX.',
     :         STATUS )
          END IF
       ELSE

*  Second, the integer types.
          IF ( BPV .EQ. 1 ) THEN
             FMTIN = '_UBYTE'
          ELSE IF ( BPV .EQ. 2 ) THEN
             FMTIN = '_WORD'
          ELSE IF ( BPV .EQ. 4 ) THEN
             FMTIN = '_INTEGER'
          ELSE IF ( BPV .EQ. 8 ) THEN
             FMTIN = '_INT64'
          ELSE
             STATUS = SAI__ERROR
             CALL ERR_REP( 'INVTYP',
     :         'The FITS object has an invalid value for BITPIX.',
     :         STATUS )
          END IF
       END IF

*  Find the output format.  Format conversion does not apply to
*  floating point.
       IF ( FMTCNV .AND. .NOT. IEEE ) THEN
          FMTOUT = '_REAL'
       ELSE
          FMTOUT = FMTIN
       END IF

       END
