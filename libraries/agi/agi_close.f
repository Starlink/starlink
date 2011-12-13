************************************************************************

      SUBROUTINE AGI_CLOSE ( STATUS )

*+
*  Name:
*     AGI_CLOSE
*
*  Purpose:
*     Close AGI in non-ADAM environments
*
*  Invocation:
*     CALL AGI_CLOSE( STATUS )
*
*  Description:
*     Close AGI in non-ADAM environments. The database file is closed.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Release any deferred pictures.
*     If the database locator is valid then
*        Close the HDS database file.
*        Indicate the database locator is no longer valid.
*     Endif
*     Close down GNS for all packages.
*
*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*
*  History:
*     Sep 1988 (NE):
*        Original version
*     Jul 1989 (NE):
*        Close database from locator in common block
*     Apr 1990 (NE):
*        Added calls to GNS_STOP
*     Oct 1990 (NE):
*        Added call to TRN_CLOSE and picture deferral
*     Apr 1991 (NE):
*        Clear transformation flags in cache
*     Nov 1991 (NE):
*        Set global status at end
*     Jul 2004 (TIMJ):
*        Replace GNS_STOP call with AGI1_ENDGNS
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_cache'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL YESNO

      INTEGER I, J, LSTAT
*.

*   Release any deferred pictures
      IF ( CISDEP ) THEN
         CISDEP = .FALSE.

*   Check each picture in turn
         DO J = 1, FRELEN
            IF ( CDEPS( J ) .NE. 0 ) THEN
               LSTAT = SAI__OK
               CALL AGI_1FRETN( FRELEN, J, FRELIS, NEXFRE, LSTAT )
               IF ( LSTAT .EQ. SAI__OK ) THEN
                  CGRAWK( J ) = ' '
                  CAGIWK( J ) = ' '
                  CPICNM( J ) = 0
                  CIDIID( J ) = 0
                  CLEVEL( J ) = 0
                  PTNAME( J ) = ' '
                  PICACS( J ) = ' '
                  CDEPS( J ) = 0
                  CNUMID = CNUMID - 1
               ENDIF
            ENDIF
         ENDDO
      ENDIF

*   Errors from AGI_1FRETN are non-fatal so reset status
      LSTAT = SAI__OK

*   See if the database locator is valid
      CALL DAT_VALID( DABLOC, YESNO, LSTAT )

*   If it is then close down the database file
      IF ( YESNO ) THEN
         CALL HDS_CLOSE( DABLOC, LSTAT )
         DABLOC = ' '

*   Set the database locator valid flag and the flush flag
         LOCVAL = .FALSE.
         FLUSH = .FALSE.
      ENDIF

*   Ensure that the TRANSFORM facility is closed down
      CALL TRN_CLOSE( LSTAT )

*   Clear out cache transformation flags
      DO J = 0, NFIFO - 1
         DO I = 0, FIFLEN - 1
            CTRFOR( I, J ) = 0
            CTRINV( I, J ) = 0
         ENDDO
      ENDDO

*   Make sure GNS is shut down for all packages (if required)
      CALL AGI1_ENDGNS( LSTAT )

*   Return the local error status if the global status is zero
      IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = LSTAT
      ENDIF

*      print*, '+++++ AGI_CLOSE +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

