************************************************************************

      SUBROUTINE AGI_CANCL ( PARAM, STATUS )

*+
*  Name:
*     AGI_CANCL
*
*  Purpose:
*     Cancel the ADAM device parameter
*
*  Invocation:
*     CALL AGI_CANCL( PARAM, STATUS )
*
*  Description:
*     Cancel the association of the ADAM device parameter with AGI. Any
*     picture identifiers associated with this parameter are annulled.
*     This routine is executed regardless of the given value of status.
*
*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        Name of the parameter used for accessing the device
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Do not check the inherited status.
*     Annul all identifiers that use this parameter
*     Cancel the parameter.
*     Decrement the reference counter.
*
*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1991, 1992 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     Aug 1988 (NE):
*        Original version
*     Dec 1989 (NE):
*        Added CIDIID
*     Apr 1990 (NE):
*        Added calls to GNS_STOP
*     Nov 1990 (NE):
*        Tidied up picture release
*     Nov 1991 (NE):
*        Set global status at end
*     Jul 1992 (NE):
*        Remove leading blanks from the parameter name
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_pfree'
      INCLUDE 'agi_cref'
      INCLUDE 'agi_locs'

*  Arguments Given :
      CHARACTER * ( * ) PARAM

*  Status :
      INTEGER STATUS

*  External references :
      EXTERNAL AGI_BLOCK

*  Local variables :
      INTEGER I, LSTAT

      CHARACTER * ( PAR__SZNAM ) LPARAM
*.

*   Do not check status on entry, and use a local status flag
      LSTAT = SAI__OK

*   Copy parameter name to local variable and convert to upper case
      LPARAM = PARAM
      CALL CHR_LDBLK( LPARAM )
      CALL CHR_UCASE( LPARAM )

*   Annul any identifiers that use this parameter
      DO I = 1, FRELEN
         IF ( LPARAM .EQ. PTNAME( I ) ) THEN
            CALL AGI_ANNUL( I, LSTAT )
         ENDIF
      ENDDO

*   Cancel the parameter
      CALL PAR_CANCL( PARAM, LSTAT )

*   Decrement the reference counter
      IF ( CREF .GT. 0 ) THEN
         CREF = CREF - 1
      ENDIF

*   Return the local error status if the global status is zero
      IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = LSTAT
      ENDIF

*      print*, '+++++ AGI_CANCL +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

