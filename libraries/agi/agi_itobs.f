************************************************************************

      SUBROUTINE AGI_ITOBS ( NXY, X, Y, LTOBS, STATUS )

*+
*  Name:
*     AGI_ITOBS
*
*  Purpose:
*     Inquire if test points are obscured
*
*  Invocation:
*     CALL AGI_ITOBS( NXY, X, Y, LTOBS, STATUS )
*
*  Description:
*     Inquire if the members of the array of test points are obscured
*     by any picture overlying (create more recently than) the current
*     picture. The points are defined in the world coordinate system of
*     the current picture. An array of logical values is returned
*     containing true if the corresponding point is obscured, otherwise
*     false.
*
*  Arguments:
*     NXY = INTEGER (Given)
*        Number of test points
*     X = REAL(NXY) (Given)
*        Array of x coordinates of test points
*     Y = REAL(NXY) (Given)
*        Array of y coordinates of test points
*     LTOBS = LOGICAL(NXY) (Returned)
*        Array of results. True if point is obscured, otherwise false.
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get details of current picture.
*     Get a locator to the top level database structure.
*     Get a locator to the workstation.
*     Inquire how many pictures are on the workstation.
*     Get a locator to the given picture and read its device coordinates.
*     Initialise the visibility of all points to false.
*     Set the picture counter to the total number of pictures.
*     Do while the picture counter is greater than the picture number.
*        Get a locator to the picture defined by the picture counter.
*        Read the device coordinates of this picture.
*        Test all the points to see of they are obscured.
*        Decrement the picture counter.
*     Enddo
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
*     July 1988 (NE):
*        Original version
*     June 1989 (NE):
*        Changed how the device coords are calculated.
*     July 1989 (NE):
*        Read database locator from common block
*     August 1990 (NE):
*        Added number of pictures
*     September 1990 (NE):
*        Removed test for overlapping pictures.
*     January 1993 (NE):
*        Initialise header block if necessary
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
      INTEGER NXY
      REAL X( NXY )
      REAL Y( NXY )

*  Arguments Returned :
      LOGICAL LTOBS( NXY )

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND, YESNO

      INTEGER I, J, PICNUM, TOTNUM

      REAL DEV1( 4 ), DEV2( 4 ), DX, DY, FACTOR, WOR1( 4 )

      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )
            PICNUM = CPICNM( CURPID )
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_ITOBS_NOCUP', 'No current picture',
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

*   Get the device coordinates of the given picture
            IF ( FOUND ) THEN
               PICLOC = ' '
               CALL AGI_1FPIC( PSTLOC, PICNUM, PICLOC, FOUND, STATUS )
               IF ( FOUND ) THEN
                  CALL AGI_1RARPR( PICLOC, 'DEVICE', FOUND, DEV1,
     :                             STATUS )
                  CALL AGI_1RARPR( PICLOC, 'WORLD', FOUND, WOR1,
     :                             STATUS )
                  CALL DAT_ANNUL( PICLOC, STATUS )
                  PICLOC = ' '
               ENDIF

*   Remember the number of pictures on this device
               IF ( CNUMPW .NE. WKNAME ) CHEAD = -1
               CNUMPW = WKNAME
               CNUMPS = TOTNUM
            ENDIF

*   If any of the things have not been found then flag an error
            IF ( .NOT. FOUND ) THEN
               STATUS = AGI__NOPIC
               CALL ERR_REP( 'AGI_ITOBS_NOPIC', 'No pictures on device',
     :                       STATUS )
               GOTO 99
            ENDIF

*   Check if this picture is in any way obscured by another.
*   Note it can only be obscured by a picture with a higher picture number
            DO J = 1, NXY
               LTOBS( J ) = .FALSE.
            ENDDO
            I = TOTNUM
            IF ( FOUND ) THEN
               DO WHILE ( I .GT. PICNUM )
                  PICLOC = ' '
                  CALL AGI_1FPIC( PSTLOC, I, PICLOC, FOUND, STATUS )
                  IF ( FOUND ) THEN
                     CALL AGI_1RARPR( PICLOC, 'DEVICE', FOUND, DEV2,
     :                                STATUS )
                     CALL DAT_ANNUL( PICLOC, STATUS )
                     PICLOC = ' '
                     IF ( FOUND ) THEN

*   Do not overwrite a point that is already obscured.
                        DO J = 1, NXY

*   Convert the world coordinates into device coordinates
                           FACTOR = ( X( J ) - WOR1( 1 ) ) /
     :                              ( WOR1( 2 ) - WOR1( 1 ) )
                           DX = FACTOR * DEV1( 2 ) +
     :                          ( 1.0 - FACTOR ) * DEV1( 1 )
                           FACTOR = ( Y( J ) - WOR1( 3 ) ) /
     :                              ( WOR1( 4 ) - WOR1( 3 ) )
                           DY = FACTOR * DEV1( 4 ) +
     :                          ( 1.0 - FACTOR ) * DEV1( 3 )
                           IF ( .NOT. LTOBS( J ) ) THEN
                              IF ( ( DX .GE. DEV2( 1 ) ) .AND.
     :                             ( DX .LE. DEV2( 2 ) ) .AND.
     :                             ( DY .GE. DEV2( 3 ) ) .AND.
     :                             ( DY .LE. DEV2( 4 ) ) ) THEN
                                 LTOBS( J ) = .TRUE.
                              ENDIF
                           ENDIF
                        ENDDO

                     ENDIF
                  ENDIF

*   Decrement counter
                  I = I - 1

               ENDDO
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

*      print*, '+++++ AGI_ITOBS +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

