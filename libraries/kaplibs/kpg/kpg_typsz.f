      SUBROUTINE KPG_TYPSZ( TYPE, NBYTES, STATUS )
*+
*  Name:
*     KPG_TYPSZ

*  Purpose:
*     Returns the number of bytes used to store an item of any of the
*     HDS primitive data types.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG_TYPSZ( TYPE, NBYTES, STATUS )

*  Description:
*     If the input TYPE is one of the HDS primitive numeric data types,
*     i.e. one of _REAL, _DOUBLE, _INTEGER, _WORD, _UWORD, _BYTE or
*      _UBYTE, then the number of bytes used by that data type is
*     returned as NBYTES.  The values are those stored as the symbolic
*     constants VAL__NBx in the PRM_PAR include file.  (See SUN/39.)
*     If the TYPE is not one of the above, an error results and
*     STATUS is set.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The HDS data type.
*     NBYTES = INTEGER (Returned)
*        The number of bytes used for the supplied data type.
*     STATUS = INTEGER (Given and Returned)
*        Global status.  Bad status is returned should TYPE not be
*        a valid HDS data type.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 May 31 (MJC):
*        Original version based on JM's CONVERT routine, but using a
*        standard prologue and style.
*     2011 January 11 (MJC):
*        Transferred from CONVERT (COF_TYPSZ) to KAPLIBS as it is
*        generically useful.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No implicit typing

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! SSE global definitions
      INCLUDE  'PRM_PAR'         ! PRIMDAT symbolic constants

*  Arguments Given:
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER NBYTES

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Compare character strings

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Assign the number of bytes appropriate for the HDS data type using
*  the PRIMDAT constants.
      IF ( CHR_SIMLR( TYPE, '_REAL' ) ) THEN
         NBYTES = VAL__NBR

      ELSE IF ( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN
         NBYTES = VAL__NBD

      ELSE IF ( CHR_SIMLR( TYPE, '_INTEGER' ) ) THEN
         NBYTES = VAL__NBI

      ELSE IF ( CHR_SIMLR( TYPE, '_WORD' ) ) THEN
         NBYTES = VAL__NBW

      ELSE IF ( CHR_SIMLR( TYPE, '_UWORD' ) ) THEN
         NBYTES = VAL__NBUW

      ELSE IF ( CHR_SIMLR( TYPE, '_BYTE' ) ) THEN
         NBYTES = VAL__NBB

      ELSE IF ( CHR_SIMLR( TYPE, '_UBYTE' ) ) THEN
         NBYTES = VAL__NBUB

      ELSE

*  If the type is not recognised, report the error.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP ( 'KPG_TYPSZ_INVTYP',
     :     '^TYPE is not a primitive numeric type.', STATUS )
      END IF

      END
