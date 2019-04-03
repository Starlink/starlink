      SUBROUTINE NDF1_EVMSG( TOKEN, IDCB )
*+
*  Name:
*     NDF1_EVMSG

*  Purpose:
*     Assign text describing a standard NDF event to a specified
*     message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_EVMSG( TOKEN, IDCB )

*  Description:
*     The routine creates a string describing the supplied NDF that can
*     be used as the descriptive text associated with an NDF event (see
*     NDF1_EVENT), and then assigns it to a specified message token.
*
*     The string contains the following fields, separated by double
*     colons ("::"):
*
*     - The NDF path
*     - The foreign format code (if any)
*
*     Any trailing blank fields are omitted, together with the associated
*     "::" delimiters.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The name of the message token to use.
*     IDCB = INTEGER (Given)
*        Index to the DCB entry to be used.

*  Notes:
*     - This routine has no STATUS argument and does not perform normal
*     error checking. If it should fail, then no value will be assigned
*     to the message token and this will be apparent in the final
*     message.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-APR-2010 (DSB):
*        Original version.
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

      INCLUDE 'NDF_FCB'          ! NDF_ Format Conversion Block
*        FCB_FMT = CHARACTER * ( 2 * NDF__SZFMT ) (Read)
*           Foreign format list string.
*        FCB_FMT1( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of start of each foreign format name.
*        FCB_FMT2( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of end of each foreign format name.

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      INTEGER IDCB

*  Local Variables:
      INTEGER IFMT              ! FCB index for the foreign format code
*.

*  If there is no foreign format file associated with the data object,
*  then assign the name of the NDF to a message token, using the data
*  object locator stored in the DCB.
      IFMT = DCB_IFMT( IDCB )
      IF ( IFMT .EQ. 0 ) THEN
         CALL DAT_MSG( TOKEN, DCB_LOC( IDCB ) )

*  Otherwise, assign the name of the associated foreign file, as stored
*  in the DCB, and append the foreign format code.
      ELSE
         CALL MSG_SETC( TOKEN, DCB_FORFL( IDCB ) )
         CALL MSG_SETC( TOKEN, '::' )
         CALL MSG_SETC( TOKEN, FCB_FMT( FCB_FMT1( IFMT ) :
     :                                  FCB_FMT2( IFMT ) ) )
      END IF

      END
