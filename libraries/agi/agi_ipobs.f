************************************************************************

      SUBROUTINE AGI_IPOBS ( PICID, LOBS, STATUS )

*+
*  Name:
*     AGI_IPOBS
*
*  Purpose:
*     Is current picture obscured by another?
*
*  Invocation:
*     CALL AGI_IPOBS( PICID, LOBS, STATUS )
*
*  Description:
*     Inquire if the current picture is obscured, either totally or
*     partially by another picture. Obscured means that a picture
*     intersects the current picture and was created more recently.
*     If the input value for the picture identifier is negative the
*     current picture is tested against all other overlying pictures;
*     i.e. those created more recently than the current picture. If the
*     picture identifier corresponds to a valid picture then the current
*     one is only tested against the given one.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier.
*     LOBS = LOGICAL (Returned)
*        True if picture is obscured, otherwise false.
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get details of the current picture.
*     Get a locator to the top level database structure.
*     Get a locator to the workstation.
*     Inquire the number of pictures on the workstation.
*     Get a locator to the given picture.
*     Read the device coordinates of this picture.
*     If the test is against all pictures then
*        Set the obscuration flag to false.
*        Set the picture counter to the total number of pictures.
*        Do while the picture is not obscured, and the picture counter
*        is less than the picture number.
*           Get a locator to the picture indicated by the picture counter.
*           Read the device coodrdinates of this picture.
*           If the pictures overlap indicate that the picture is obsured.
*           Decrement the picture counter.
*        Enddo
*     Else
*        Read the device coordinates of the given picture.
*        See if this overlaps the current picture.
*     Endif
*     Tidy up.
*
*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1993 Science & Engineering Research Council.
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
*     Jul 1989 (NE):
*         Read database locator from common block
*     Aug 1990 (NE):
*         Added number of pictures
*     Jan 1993 (NE):
*         Initialise header block if necessary
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_cache'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER PICID

*  Arguments Returned :
      LOGICAL LOBS

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND, YESNO

      INTEGER I, PICNUM, TPICNM, TOTNUM

      REAL DEV1( 4 ), DEV2( 4 )

      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) TWKNAM, WKNAME
*.

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get the details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )
            PICNUM = CPICNM( CURPID )
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_IPOBS_NOCUP', 'No current picture',
     :                    STATUS )
            GOTO 99
         ENDIF

*   Find the picture structure locator and the total number of pictures
         CALL AGI_1FDB( FOUND, STATUS )
         IF ( FOUND ) THEN
            WKSLOC = ' '
            CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
            IF ( FOUND ) THEN
               PSTLOC = ' '
               CALL AGI_1IPIC( WKSLOC, PSTLOC, TOTNUM, FOUND, STATUS )
               CALL DAT_ANNUL( WKSLOC, STATUS )
               WKSLOC = ' '
            ENDIF

*   Get the device coordinates of the current picture
            IF ( FOUND ) THEN
               PICLOC = ' '
               CALL AGI_1FPIC( PSTLOC, PICNUM, PICLOC, FOUND, STATUS )
               IF ( FOUND ) THEN
                  CALL AGI_1RARPR( PICLOC, 'DEVICE', FOUND, DEV1,
     :                             STATUS )
                  CALL DAT_ANNUL( PICLOC, STATUS )
                  PICLOC = ' '
               ENDIF

*   Remember the number of pictures on this device
               IF ( CNUMPW .NE. WKNAME ) CHEAD = -1
               CNUMPW = WKNAME
               CNUMPS = TOTNUM
            ENDIF

*   If PICID is negative then test all pictures above the current one
            IF ( PICID .LT. 0 ) THEN

*   Check if this picture is in any way obscured by another.
*   Note it can only be obscured by a picture with a higher picture number
               LOBS = .FALSE.
               I = TOTNUM
               IF ( FOUND ) THEN
                  DO WHILE ( ( .NOT. LOBS ) .AND. ( I .GT. PICNUM ) )
                     PICLOC = ' '
                     CALL AGI_1FPIC( PSTLOC, I, PICLOC, FOUND, STATUS )
                     IF ( FOUND ) THEN
                        CALL AGI_1RARPR( PICLOC, 'DEVICE', FOUND, DEV2,
     :                                   STATUS )
                        CALL DAT_ANNUL( PICLOC, STATUS )
                        PICLOC = ' '
                        IF ( FOUND ) THEN

*   Test if picture is outside current picture. If so do nothing.
                           IF ( ( DEV2( 1 ) .GE. DEV1( 2 ) ) .OR.
     :                          ( DEV2( 2 ) .LE. DEV1( 1 ) ) .OR.
     :                          ( DEV2( 3 ) .GE. DEV1( 4 ) ) .OR.
     :                          ( DEV2( 4 ) .LE. DEV1( 3 ) ) ) THEN

*   Else pictures must intersect
                           ELSE
                              LOBS = .TRUE.
                           ENDIF

                        ENDIF
                     ENDIF

*   Decrement counter
                     I = I - 1

                  ENDDO
               ENDIF

*   Otherwise just check the given picture against the current one
            ELSE
               LOBS = .FALSE.

*   Get the details of the given picture
               TWKNAM = CAGIWK( PICID )
               TPICNM = CPICNM( PICID )

*   Check that they are on the same workstation and that the given
*   picture has a higher picture number than the current one.
               IF ( ( TPICNM .GT. PICNUM ) .AND.
     :              ( TWKNAM .EQ. WKNAME ) ) THEN

*   Find the device coordinates of the given picture
                  PICLOC = ' '
                  CALL AGI_1FPIC( PSTLOC, TPICNM, PICLOC, FOUND, STATUS)
                  IF ( FOUND ) THEN
                     CALL AGI_1RARPR( PICLOC, 'DEVICE', FOUND, DEV2,
     :                                STATUS )
                     CALL DAT_ANNUL( PICLOC, STATUS )
                     PICLOC = ' '
                     IF ( FOUND ) THEN

*   Test if picture is outside current picture. If so do nothing.
                        IF ( ( DEV2( 1 ) .GE. DEV1( 2 ) ) .OR.
     :                       ( DEV2( 2 ) .LE. DEV1( 1 ) ) .OR.
     :                       ( DEV2( 3 ) .GE. DEV1( 4 ) ) .OR.
     :                       ( DEV2( 4 ) .LE. DEV1( 3 ) ) ) THEN

*   Else pictures must intersect
                        ELSE
                           LOBS = .TRUE.
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF

*   Delete pstloc if still active
            CALL DAT_VALID( PSTLOC, YESNO, STATUS )
            IF ( YESNO ) THEN
               CALL DAT_ANNUL( PSTLOC, STATUS )
               PSTLOC = ' '
            ENDIF

         ENDIF
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_IPOBS +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

