      SUBROUTINE NDF_CMSG( TOKEN, INDF, COMP, STATUS )
*+
*  Name:
*     NDF_CMSG

*  Purpose:
*     Assign the value of an NDF character component to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CMSG( TOKEN, INDF, COMP, STATUS )

*  Description:
*     The routine assigns the value of the specified character
*     component of an NDF to a message token, for use in constructing
*     messages using the MSG_ or ERR_ routines (see SUN/104).

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        Name of the message token.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the character component whose value is to be used:
*        'LABEL', 'TITLE' or 'UNITS'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the specified NDF component does not have a defined value,
*     then the string '<undefined>' is assigned to the token instead.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Validate the component name.
*     -  Obtain an index to the data object entry in the DCB and ensure
*     that information about the required character component is
*     available in the DCB.
*     -  If the component is not present in the NDF, then assign an
*     appropriate value to the message token.
*     -  If it is present, then map it, assign its value to the message
*     token and then unmap it.

*  Copyright:
*     Copyright (C) 1989, 1990, 1992 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1989 (RFWS):
*        Original version.
*     16-JAN-1990 (RFWS):
*        Changed the default string to '<undefined>'.
*     17-JAN-1992 (RFWS):
*        Added handling of mapped character string length for UNIX
*        compatibility.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_CLOC( NDF__MXCCN, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read)
*           Locators to NDF character components.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DUMMY( 1 )         ! Dummy dimension array
      INTEGER IACB               ! Index to NDF entry in the DCB
      INTEGER ICCOMP             ! Indentifier for character component
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER LENC               ! Length of mapped character value
      INTEGER PNTR               ! Pointer to mapped data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the component name.
      CALL NDF1_VCCN( COMP, ICCOMP, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB (IACB )

*  Ensure that information about the required character component is
*  available in the DCB.
         CALL NDF1_DC( IDCB, ICCOMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the component is not present in the NDF, then assign an
*  appropriate value to the message token.
            IF ( DCB_CLOC( ICCOMP, IDCB ) .EQ. DAT__NOLOC ) THEN
               CALL MSG_SETC( TOKEN, '<undefined>' )

*  If it is present, then map it and determine its length.
            ELSE
               CALL DAT_MAPC( DCB_CLOC( ICCOMP, IDCB ), 'READ', 0,
     :                        DUMMY, PNTR, STATUS )
               CALL DAT_CLEN( DCB_CLOC( ICCOMP, IDCB ), LENC, STATUS )

*  Assign the component's value to the message token and then unmap it.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NDF1_SETC( %VAL( CNF_PVAL( PNTR ) ), TOKEN,
     :                            %VAL( CNF_CVAL( LENC ) ) )
               END IF
               CALL NDF1_HUNMP( DCB_CLOC( ICCOMP, IDCB ), STATUS )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF_CMSG', STATUS )

      END
