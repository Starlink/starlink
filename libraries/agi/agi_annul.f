************************************************************************

      SUBROUTINE AGI_ANNUL ( PICID, STATUS )

*+
*  Name:
*     AGI_ANNUL
*
*  Purpose:
*     Annul a picture identifier
*
*  Invocation:
*     CALL AGI_ANNUL( PICID, STATUS )
*
*  Description:
*     Annul the picture identifier. If this is the last active
*     identifier then the database is closed. This routine is
*     executed regardless of the given value of status.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Do not check the inherited status.
*     Verify the given picture identifier is valid.
*     If the picture is the current one then defer the release.
*     Delete all references to this picture from the common blocks.
*     If the number of allocated id's is zero then
*        Release any deferred pictures.
*        Close the database.
*     Endif
*
*  Copyright:
*     Copyright (C) 1989, 1990, 1991 Science & Engineering Research Council.
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
*     Jun 1989 (NE):
*        Original version
*     Dec 1989 (NE):
*        Added CIDIID
*     Apr 1990 (NE):
*        Added calls to GNS_STOP
*     Oct 1990 (NE):
*        Added picture deferral
*     Nov 1990 (NE):
*        Add nesting level and remove reference counter
*     Apr 1991 (NE):
*        Clear transformations flags in cache
*     Nov 1991 (NE):
*        Correct use of local status
*     Jul 2004 (TIMJ):
*        Replace GNS_STOP call with AGI1_ENDGNS
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_ERR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_cache'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL YESNO

      INTEGER I, J, LSTAT, NUMDEP
*.

*   Do not check status on entry, and use a local status flag
      LSTAT = SAI__OK

*   Check that this identifier is valid
      IF ( ( PICID .GE. 1 ) .AND. ( PICID .LE. FRELEN ) ) THEN

*   If it is the current picture then defer the release
         IF ( PICID .EQ. CURPID ) THEN
            CDEPS( PICID ) = 1
            CISDEP = .TRUE.

*   Release the storage associated with this entry in the common blocks
         ELSE
            CALL AGI_1FRETN( FRELEN, PICID, FRELIS, NEXFRE, LSTAT )
            IF ( LSTAT .EQ. SAI__OK ) THEN
               CGRAWK( PICID ) = ' '
               CAGIWK( PICID ) = ' '
               CPICNM( PICID ) = 0
               PTNAME( PICID ) = ' '
               PICACS( PICID ) = ' '
               CIDIID( PICID ) = 0
               CLEVEL( PICID ) = 0
               CNUMID = CNUMID - 1
            ENDIF
         ENDIF

*   Error status values from AGI_1FRETN are non-fatal so reset status
         LSTAT = SAI__OK

*   Otherwise indicate that the picture id is invalid,
*   but do not report an error if the input status is bad
      ELSE
         IF ( STATUS .EQ. SAI__OK ) THEN
            LSTAT = AGI__IMPID
            CALL ERR_REP( 'AGI_ANNUL_IMPID',
     :                    'Invalid picture identifier', LSTAT )
         ENDIF
         GOTO 98
      ENDIF

*   Count how many deferred pictures there are
      NUMDEP = 0
      IF ( CISDEP ) THEN
         DO J = 1, FRELEN
            IF ( CDEPS( J ) .EQ. 1 ) THEN
               NUMDEP = NUMDEP + 1
            ENDIF
         ENDDO
      ENDIF

*   If the last identifier has been released then close the database
      IF ( ( CNUMID .EQ. 0 ) .OR.
     :     ( ( CNUMID .EQ. NUMDEP ) .AND. CISDEP ) ) THEN

*   Release the deferred pictures
         IF ( CISDEP ) THEN
            DO J = 1, FRELEN
               IF ( CDEPS( J ) .EQ. 1 ) THEN
                  LSTAT = SAI__OK
                  CALL AGI_1FRETN( FRELEN, J, FRELIS, NEXFRE, LSTAT )
                  IF ( LSTAT .EQ. SAI__OK ) THEN
                     CGRAWK( J ) = ' '
                     CAGIWK( J ) = ' '
                     CPICNM( J ) = 0
                     PTNAME( J ) = ' '
                     PICACS( J ) = ' '
                     CIDIID( J ) = 0
                     CLEVEL( J ) = 0
                     CDEPS( J ) = 0
                     CNUMID = CNUMID - 1
                  ENDIF
               ENDIF
            ENDDO
            CISDEP = .FALSE.
         ENDIF

*   Error status values from AGI_1FRETN are non-fatal so reset status
         LSTAT = SAI__OK

*   Indicate there is no current picture
         CURPID = 0

*   Close the database if the nesting level is 1
*   First check if the locator is valid
         IF ( CNEST .EQ. 1 ) THEN
            CALL DAT_VALID( DABLOC, YESNO, LSTAT )

*   If it is then close the database file
            IF ( YESNO ) THEN
               CALL HDS_CLOSE( DABLOC, LSTAT )
               DABLOC = ' '

*   Set database locator valid flag and the database flush flag
               LOCVAL = .FALSE.
               FLUSH = .FALSE.

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
            ENDIF

*   Otherwise indicate that AGI_END should close the database
         ELSE
            CLOSEF = .TRUE.
         ENDIF

*  Ensure any required merging of PGPLOT postscript files has been done.
         CALL AGP1_EPSMG( STATUS )

      ENDIF

*   Return the local error status if the global status is zero
  98  CONTINUE
      IF ( STATUS .EQ. SAI__OK ) THEN
         STATUS = LSTAT
      ENDIF

*      print*, '+++++ AGI_ANNUL +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

