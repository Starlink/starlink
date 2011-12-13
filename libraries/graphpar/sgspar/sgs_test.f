      SUBROUTINE SGS_TEST(STATUS)
*+
*  Name:
*     sgs_test

*  Purpose:
*     Test SGS environment level.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Authors:
*     {enter_new_authors_here}

*-
      INCLUDE 'SAE_PAR'
      INCLUDE 'SGS_ERR'

      INTEGER STATUS
      INTEGER IZONID,IZ
*.

      CALL SGS_ASSOC( 'DEVICE', 'WRITE', IZONID, STATUS)
      IF (STATUS .NE. SAI__OK) RETURN

*  Declare a shaped zone on the first device
      CALL SGS_ZSHAP(1.0, 'O', IZ, STATUS)
      IF (STATUS .NE. SAI__OK) RETURN

*  BOX
      CALL SGS_BOX( 0.1, 0.9, 0.4, 0.6)

*  MESSAGE
      CALL SGS_SHTX( 0.1 )
      CALL SGS_STXJ( 'CC' )
      CALL SGS_BTEXT( 0.5, 0.5)
      CALL SGS_ATEXT('STARLINK')

      CALL SGS_CANCL( 'DEVICE', STATUS )
      CALL SGS_DEACT(  STATUS )

      END
