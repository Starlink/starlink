      SUBROUTINE NDF1_DMSG( TOKEN, IDCB )
*+
*  Name:
*     NDF1_DMSG

*  Purpose:
*     Assign the name of an NDF stored in the DCB to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DMSG( TOKEN, IDCB )

*  Description:
*     The routine assigns the full name (including the file name) of an
*     NDF data object to a message token for use with the ERR_ and MSG_
*     routines (SUN/104). The NDF is identified by the index of its
*     entry in the DCB.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     IDCB = INTEGER * ( * ) (Given)
*        Index of the NDF data object entry in the DCB.

*  Notes:
*     This routine has no STATUS argument and does not perform normal
*     error checking. If it should fail, then no value will be assigned
*     to the message token and this will be apparent in the final
*     message.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1990 (RFWS):
*        Original version.
*     19-OCT-1993 (RFWS):
*        Use the name of an associated foreign file, if it exists.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_FORFL( NDF__MXDCB ) = CHARACTER * ( NDF__SZFIL ) (Read)
*           Name of associated foreign format file (if it exists).
*        DCB_IFMT( NDF__MXDCB ) = INTEGER (Read)
*           FCB code identifying the format of an associated foreign
*           file (zero if no such file exists).
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      INTEGER IDCB

*.

*  If there is no foreign format file associated with the data object,
*  then assign the name of the NDF to a message token, using the data
*  object locator stored in the DCB.
      IF ( DCB_IFMT( IDCB ) .EQ. 0 ) THEN
         CALL DAT_MSG( TOKEN, DCB_LOC( IDCB ) )

*  Otherwise, assign the name of the associated foreign file, as stored
*  in the DCB.
      ELSE
         CALL MSG_SETC( TOKEN, DCB_FORFL( IDCB ) )
      END IF

      END
