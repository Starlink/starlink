************************************************************************

      SUBROUTINE AGD_SWIND ( DISPID, MEMID, XSIZE, YSIZE, XOFF, YOFF,
     :                       PNAME, COMENT, WX1, WX2, WY1, WY2,
     :                       PICID, STATUS )

*+
*  Name:
*     AGD_SWIND
*
*  Purpose:
*     Save an IDI window in the database
*
*  Invocation:
*     CALL AGD_SWIND( DISPID, MEMID, XSIZE, YSIZE, XOFF, YOFF, PNAME,
*    :                COMENT, WX1, WX2, WY1, WY2, PICID, STATUS )
*
*  Description:
*     Save an IDI window as a picture in the database. The new picture
*     must be equal in size or smaller than the current picture in the
*     database. The window coordinates define the size of the
*     rectangular area in pixels and the offset of its bottom left
*     corner from the memory origin. The name of the picture and a
*     comment are used to identify the picture in the database. The name
*     string has leading blanks removed and is converted to upper case.
*     The world coordinates define the user coordinate system and are
*     saved in the database as given. They should be linear and increasing
*     left to right and bottom to top. If the picture was successfully
*     created then a valid picture identifier is returned and the new
*     picture becomes the current picture.
*
*  Arguments:
*     DISPID = INTEGER (Given)
*        Display identifier
*     MEMID = INTEGER (Given)
*        Memory identifier
*     XSIZE = INTEGER (Given)
*        X size of window (pixels)
*     YSIZE = INTEGER (Given)
*        Y size of window (pixels)
*     XOFF = INTEGER (Given)
*        X offset of window from origin (pixels)
*     YOFF = INTEGER (Given)
*        Y offset of window from origin (pixels)
*     PNAME = CHARACTER*(*) (Given)
*        Name of picture
*     COMENT = CHARACTER*(*) (Given)
*        Description of picture
*     WX1 = REAL (Given)
*        World coordinate of left edge of new picture
*     WX2 = REAL (Given)
*        World coordinate of right edge of new picture
*     WY1 = REAL (Given)
*        World coordinate of bottom edge of new picture
*     WY2 = REAL (Given)
*        World coordinate of top edge of new picture
*     PICID = INTEGER (Returned)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Check IDI has been activated.
*     Update the zoom and scroll factors for the current device.
*     Get the details of the current picture.
*     Check that the given display identifier matches that of the
*     current picture.
*     Verify that the given memory exists on this device.
*     Check that the given window will fit in this memory.
*     Get the current zoom and scroll factors for this memory.
*     Calculate the device, ndc and world coordinates of the window.
*     If there is a current picture in the database then
*        Read the contents of the current picture.
*        Check that the window lies within the current picture.
*     Endif
*     Save the window as a picture in the database.
*     Get a picture identifier and save the details in the common blocks.
*
*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     Jun 1990 (NE):
*        Original version
*     Mar 1991 (NE):
*        Added world coordinates to argument list
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER DISPID
      INTEGER MEMID
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER XOFF
      INTEGER YOFF
      CHARACTER * ( * ) PNAME
      CHARACTER * ( * ) COMENT
      REAL WX1
      REAL WX2
      REAL WY1
      REAL WY2

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND

      INTEGER MEMITD( MXMEMS ), MEMS( MXMEMS ), MEMSD( MXMEMS ),
     :        MEMSX( MXMEMS ), MEMSY( MXMEMS ), MODE, NMEMS
      INTEGER CURMID, ISTAT, J, JMEM, JMEMTY, LENSTR, MEMTY, NCONF,
     :        NVAL, PICNUM
      INTEGER PXSCRL, PYSCRL, PZOOMF, XSCRL, YSCRL, ZOOMF

      CHARACTER * ( AGI__CMAX ) CURCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM
      CHARACTER * ( DAT__SZNAM ) WKNAME
      CHARACTER STRING * 64

      REAL CDEV( 4 ), CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ),
     :     DEVICE( 4 ), NDC( 4 ), NORMAL, SFA, WORLD( 4 ),
     :     XFACT, YFACT, ZRATIO
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Check that IDI has started
      IF ( .NOT. CIDION ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGD_SWIND_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Update the zoom and scroll factors
      CALL AGD_UPDAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get the details of the current picture
      IF ( CURPID .GT. 0 ) THEN
         WKNAME = CAGIWK( CURPID )
         PICNUM = CPICNM( CURPID )
      ELSE
         STATUS = AGI__NOCUP
         CALL ERR_REP( 'AGD_SWIND_NOCUP', 'No current picture', STATUS )
         GOTO 99
      ENDIF

*   Check that the given display matches that of the current picture
      IF ( DISPID .NE. CIDIID( CURPID ) ) THEN
         STATUS = AGI__DIFDV
         CALL ERR_REP( 'AGD_SWIND_DIFDV', 'Different devices', STATUS )
         GOTO 99
      ENDIF

*   Get the memory size from the configuration number.
*   Have to check each memory type ( 1, 2, 4 ) for a match.
      CALL IIDQCI( DISPID, 11, 1, NCONF, NVAL, ISTAT )
      DO 20 JMEMTY = 0, 2
         MEMTY = 2 ** JMEMTY
         CALL IIDQDC( DISPID, NCONF, MEMTY, MXMEMS, MODE, MEMS, MEMSX,
     :                MEMSY, MEMSD, MEMITD, NMEMS, ISTAT )
         IF ( ISTAT .NE. 0 ) THEN
            CALL IIDERR( ISTAT, STRING, LENSTR )
            STATUS = AGI__IDERR
            CALL ERR_REP( 'AGD_SWIND_IDQDC', STRING( :LENSTR ), STATUS )
            GOTO 99
         ENDIF

*   Examine the memories of this type for a match
         DO 10 J = 1, NMEMS
            IF ( MEMS( J ) .EQ. MEMID ) THEN
               JMEM = J
               GOTO 30
            ENDIF
  10     CONTINUE
  20  CONTINUE

*   If this point is reached then there is no match
      STATUS = AGI__MEMIN
      CALL MSG_SETI( 'IVAL', MEMID )
      CALL ERR_REP( 'AGD_SWIND_MEMIN', 'IDI memory ^IVAL invalid',
     :              STATUS )
      GOTO 99
  30  CONTINUE

*   Verify the window size against the memory size
      IF ( ( XOFF .LT. 0 ) .OR. ( YOFF .LT. 0 ) .OR.
     :     ( XOFF + XSIZE .GT. MEMSX( JMEM ) ) .OR.
     :     ( YOFF + YSIZE .GT. MEMSY( JMEM ) ) ) THEN
         STATUS = AGI__MEMTS
         CALL ERR_REP( 'AGD_SWIND_MEMTS', 'IDI memory too small',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Get the zoom and scrolls for this memory
      CALL IIZRSZ( DISPID, MEMID, XSCRL, YSCRL, ZOOMF, ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
         CALL IIDERR( ISTAT, STRING, LENSTR )
         STATUS = AGI__IDERR
         CALL ERR_REP( 'AGD_SWIND_IZRSZ', STRING( :LENSTR ), STATUS )
         GOTO 99
      ENDIF

*   Calculate the coordinates of the given window
      DEVICE( 1 ) = REAL( XOFF )
      DEVICE( 2 ) = REAL( XOFF + XSIZE )
      DEVICE( 3 ) = REAL( YOFF )
      DEVICE( 4 ) = REAL( YOFF + YSIZE )

*   The NDC's are calculated so that the longest memory dimension
*   has limits of 0 and 1
      NORMAL = REAL( MAX( MEMSX( JMEM ), MEMSY( JMEM ) ) )
      NDC( 1 ) = DEVICE( 1 ) / NORMAL
      NDC( 2 ) = DEVICE( 2 ) / NORMAL
      NDC( 3 ) = DEVICE( 3 ) / NORMAL
      NDC( 4 ) = DEVICE( 4 ) / NORMAL

*   Use the world coordinates passed as arguments
      WORLD( 1 ) = WX1
      WORLD( 2 ) = WX2
      WORLD( 3 ) = WY1
      WORLD( 4 ) = WY2

*   If the current picture number is negative then there must be no
*   pictures on this device so save the window as the first one.
*   If the current picture number is positive then make sure the given
*   window lies within it
      IF ( PICNUM .GT. 0 ) THEN

*   Read the contents of the current picture
         CALL AGI_1RPIC( WKNAME, PICNUM, CURNAM, CURCOM, CURDEV, CURNDC,
     :                   CURWOR, CURMID, FOUND, STATUS )

*   If the picture is not there then indicate an error
         IF ( .NOT. FOUND ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGD_SWIND_PICNF', 'Picture not found',
     :                    STATUS )
            GOTO 99
         ENDIF

*   Get the IDI parameters for the current picture
         CALL AGD_1IIDIP( CURMID, PXSCRL, PYSCRL, PZOOMF, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Calculate the device coordinates of the current picture in terms
*   of the present zoom and scroll
         XFACT = ( PXSCRL - XSCRL ) / REAL( ZOOMF + 1 )
         YFACT = ( PYSCRL - YSCRL ) / REAL( ZOOMF + 1 )
         ZRATIO = REAL( PZOOMF + 1 ) / REAL( ZOOMF + 1 )
         CDEV( 1 ) = XFACT + ZRATIO * CURDEV( 1 )
         CDEV( 2 ) = XFACT + ZRATIO * CURDEV( 2 )
         CDEV( 3 ) = YFACT + ZRATIO * CURDEV( 3 )
         CDEV( 4 ) = YFACT + ZRATIO * CURDEV( 4 )

*   Check that the given window lies within the current picture
*   Have to allow for fuzzy edges because IDI only works with whole pixels
*   However the device may be a metafile so if all the device coordinates
*   are less than one then assume it is a metafile.
         IF ( ( DEVICE( 1 ) .LT. 1.0 ) .AND.
     :        ( DEVICE( 2 ) .LT. 1.0 ) .AND.
     :        ( DEVICE( 3 ) .LT. 1.0 ) .AND.
     :        ( DEVICE( 4 ) .LT. 1.0 ) ) THEN
            SFA = 0.001
         ELSE
            SFA = 0.999
         ENDIF
         IF ( ( DEVICE( 1 ) .LT. CDEV( 1 ) - SFA ) .OR.
     :        ( DEVICE( 2 ) .GT. CDEV( 2 ) + SFA ) .OR.
     :        ( DEVICE( 3 ) .LT. CDEV( 3 ) - SFA ) .OR.
     :        ( DEVICE( 4 ) .GT. CDEV( 4 ) + SFA ) ) THEN
            STATUS = AGI__PICOB
            CALL ERR_REP( 'AGD_SWIND_PICOB', 'Picture out of bounds',
     :                    STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Save the window in the database
      CALL AGI_1PNEW( WKNAME, PNAME, COMENT, DEVICE, NDC, WORLD, MEMID,
     :                PICNUM, STATUS )

*   Get a picture identifier
*   Use a local status for the free list access
      ISTAT = SAI__OK

*   Get a picture identifier from the free list
      CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, PICID, ISTAT )

*   If the exit status is not OK then the free list is full
      IF ( ISTAT .NE. SAI__OK ) THEN
         STATUS = AGI__NOPID
         CALL ERR_REP( 'AGD_SWIND_NOPID', 'No more picture identifiers',
     :                 STATUS )
         GOTO 99
      ELSE
         CNUMID = CNUMID + 1
      ENDIF

*   Store the details in the common block
      CAGIWK( PICID ) = WKNAME
      CPICNM( PICID ) = PICNUM
      CGRAWK( PICID ) = CGRAWK( CURPID )
      PICACS( PICID ) = PICACS( CURPID )
      CIDIID( PICID ) = DISPID
      CLEVEL( PICID ) = CNEST

*   Make this the current picture
      CURPID = PICID

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGD_SWIND +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

