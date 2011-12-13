************************************************************************

      SUBROUTINE AGD_NWIND ( MEMID, DISPID, XSIZE, YSIZE, XOFF, YOFF,
     :                       STATUS )

*+
*  Name:
*     AGD_NWIND
*
*  Purpose:
*     Define an IDI window from the current picture
*
*  Invocation:
*     CALL AGD_NWIND( MEMID, DISPID, XSIZE, YSIZE, XOFF, YOFF, STATUS )
*
*  Description:
*     Define an IDI window in the given memory from the current picture.
*     The window coordinates define the size of the rectangular area in
*     pixels and the offset of its bottom left corner from the memory
*     origin. The window is defined as the smallest possible area, made
*     up of whole pixels, that completely contains the picture. If the
*     device associated with the current picture is not already open
*     then this routine will open the device and return the display
*     identifier. Furthermore if the device was opened with 'WRITE'
*     access (in AGI_ASSOC or AGI_OPEN) then the window will be cleared.
*     If the device is already open then the display identifier will be
*     the same as previously and the device will not be cleared.
*
*  Arguments:
*     MEMID = INTEGER (Given)
*        Memory identifier
*     DISPID = INTEGER (Returned)
*        Display identifier
*     XSIZE = INTEGER (Returned)
*        X size of window
*     YSIZE = INTEGER (Returned)
*        Y size of window
*     XOFF = INTEGER (Returned)
*        X offset of window from origin
*     YOFF = INTEGER (Returned)
*        Y offset of window from origin
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Check IDI has been activated.
*     Update the zoom and scroll factors for the current device.
*     Get the details of the current picture.
*     If the common block contains a valid display identifier then
*        Use this
*     Else
*        Open the device specified by the current picture.
*     Endif
*     Verify that the given memory exists on this device.
*     Obtain the current zoom and scroll factors for this memory.
*     Read the contents of the current picture.
*     Calculate the device coordinates of the current picture.
*     Check that this will fit in the given memory.
*     Round down the device coordinates to integer pixel coordinates.
*     If the current picture has 'WRITE' access then
*        If the current picture is the base picture then
*           Clear the whole memory.
*        Else
*           Use a blockfill to clear the relevant area.
*        Endif
*     Endif
*
*  Copyright:
*     Copyright (C) 1990, 1992 Science & Engineering Research Council.
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
*     Nov 1990 (NE):
*        Round partial pixels up not down, and added clear flag
*     Jul 1992 (NE):
*        Test status from all IDI routines
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
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER MEMID

*  Arguments Returned :
      INTEGER DISPID
      INTEGER XSIZE
      INTEGER YSIZE
      INTEGER XOFF
      INTEGER YOFF

*  Status :
      INTEGER STATUS

*  Local Constants :
*     Size of internal array for clearing area
      INTEGER NBLANK
      PARAMETER ( NBLANK = 1024 * 16 )

*  Local variables :
      LOGICAL FOUND, GOTIT

      INTEGER BLANK( NBLANK ), CLRNUM, CURMID, DEPTH, ISTAT, I, J, JMEM,
     :        JMEMTY, LENSTR, MEMTY, MODE, NCONF, NMEMS, NPIX, NROWS,
     :        NSENT, NVAL, PACK, PICNUM, PXSCRL, PYSCRL, PZOOMF, SROWS,
     :        XSCRL, YSCRL, ZOOMF
      INTEGER MEMS( MXMEMS ), MEMSX( MXMEMS ), MEMSY( MXMEMS ),
     :        MEMSD( MXMEMS ), MEMITD( MXMEMS ), TWOFX( MXMEMS ),
     :        TWOFY( MXMEMS ), TWSIX( MXMEMS ), TWSIY( MXMEMS )

      CHARACTER * ( AGI__CMAX ) CURCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM
      CHARACTER * ( DAT__SZNAM ) WKNAME
      CHARACTER DEVNAM * 64, STRING * 64

      REAL CDEV( 4 ), CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ),
     :     XFACT, YFACT, ZRATIO

*  Local data :
      DATA BLANK / NBLANK * 0 /
      SAVE BLANK
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Check that IDI has started
      IF ( .NOT. CIDION ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGD_NWIND_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Update the zoom and scrolls
      CALL AGD_UPDAT( STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get the details of the current picture
      IF ( CURPID .GT. 0 ) THEN
         WKNAME = CAGIWK( CURPID )
         PICNUM = CPICNM( CURPID )
      ELSE
         STATUS = AGI__NOCUP
         CALL ERR_REP( 'AGD_NWIND_NOCUP', 'No current picture', STATUS )
         GOTO 99
      ENDIF

*   Return the display identifier from the common block if the device
*   is already open
      IF ( CIDIID( CURPID ) .GT. 0 ) THEN
         DISPID = CIDIID( CURPID )

*   Otherwise open the device using IDI_CLRFG to prevent IDI recalling
*   its context since the last package was probably not IDI
      ELSE
         CALL IDI_CLRFG( 1 )

*   Try opening IDI for this device
         ISTAT = 0
         CALL IIDOPN( CGRAWK( CURPID ), DISPID, ISTAT )

*   If the graphics name has failed to open the device it might because
*   it is a GKS name so try using the AGI name to get a device name
         IF ( ISTAT .NE. 0 ) THEN
            ISTAT = 0
            CALL GNS_IIAI( CAGIWK( CURPID ), DEVNAM, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99
            CALL IIDOPN( DEVNAM, DISPID, ISTAT )

*   If this has failed then give up
            IF ( ISTAT .NE. 0 ) THEN
               CALL IIDERR( ISTAT, STRING, LENSTR )
               STATUS = AGI__WKNOP
               CALL ERR_REP( 'AGD_NWIND_IDOPN', STRING( :LENSTR ),
     :                       STATUS )
               GOTO 99
            ENDIF
         ENDIF

*   Copy the display identifier into the common block
         CIDIID( CURPID ) = DISPID
      ENDIF

*   Get the memory size from the configuration number.
*   Have to check each memory type ( 1, 2, 4 ) for a match.
      CALL IIDQCI( DISPID, 11, 1, NCONF, NVAL, ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
         CALL IIDERR( ISTAT, STRING, LENSTR )
         STATUS = AGI__IDERR
         CALL ERR_REP( 'AGD_NWIND_IDQCI', STRING( :LENSTR ), STATUS )
         GOTO 99
      ENDIF
      DO 20 JMEMTY = 0, 2
         MEMTY = 2 ** JMEMTY
         CALL IIDQDC( DISPID, NCONF, MEMTY, MXMEMS, MODE, MEMS, MEMSX,
     :                MEMSY, MEMSD, MEMITD, NMEMS, ISTAT )
         IF ( ISTAT .NE. 0 ) THEN
            CALL IIDERR( ISTAT, STRING, LENSTR )
            STATUS = AGI__IDERR
            CALL ERR_REP( 'AGD_NWIND_IDQDC', STRING( :LENSTR ), STATUS )
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
      CALL ERR_REP( 'AGD_NWIND_MEMIN', 'IDI memory ^IVAL invalid',
     :              STATUS )
      GOTO 99
  30  CONTINUE

*   Get the zoom and scrolls for this memory
      CALL IIZRSZ( DISPID, MEMID, XSCRL, YSCRL, ZOOMF, ISTAT )
      IF ( ISTAT .NE. 0 ) THEN
         CALL IIDERR( ISTAT, STRING, LENSTR )
         STATUS = AGI__IDERR
         CALL ERR_REP( 'AGD_NWIND_IZRSZ', STRING( :LENSTR ), STATUS )
         GOTO 99
      ENDIF

*   If PICNUM is greater than 0 then get the coordinates for this picture
*   and create the corresponding window. Otherwise report an error.
      IF ( PICNUM .GT. 0 ) THEN

*   Read the contents of the picture
         CALL AGI_1RPIC( WKNAME, PICNUM, CURNAM, CURCOM, CURDEV, CURNDC,
     :                   CURWOR, CURMID, FOUND, STATUS )

*   If picture is not there then indicate an error
         IF ( .NOT. FOUND ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGD_NWIND_PICNF', 'Picture not found',
     :                    STATUS )
            GOTO 99
         ENDIF

*   Get the IDI parameters for the current picture
         CALL AGD_1IIDIP( CURMID, PXSCRL, PYSCRL, PZOOMF, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Calculate the device coordinates of the current picture
         XFACT = ( PXSCRL - XSCRL ) / REAL( ZOOMF + 1 )
         YFACT = ( PYSCRL - YSCRL ) / REAL( ZOOMF + 1 )
         ZRATIO = REAL( PZOOMF + 1 ) / REAL( ZOOMF + 1 )
         CDEV( 1 ) = XFACT + ZRATIO * CURDEV( 1 )
         CDEV( 2 ) = XFACT + ZRATIO * CURDEV( 2 )
         CDEV( 3 ) = YFACT + ZRATIO * CURDEV( 3 )
         CDEV( 4 ) = YFACT + ZRATIO * CURDEV( 4 )

*   If the device coordinates of the database are larger than the
*   given memory then cannot create a new window
         IF ( ( CDEV( 1 ) .LT. 0 ) .OR.
     :        ( CDEV( 2 ) .GT. MEMSX( JMEM ) ) .OR.
     :        ( CDEV( 3 ) .LT. 0 ) .OR.
     :        ( CDEV( 4 ) .GT. MEMSY( JMEM ) ) ) THEN
            STATUS = AGI__MEMTS
            CALL ERR_REP( 'AGD_NWIND_MEMTS', 'IDI memory too small',
     :                    STATUS )
            GOTO 99
         ELSE

*   Return the smallest area comprising of whole pixels that
*   completely contains the picture
            XOFF = INT( CDEV( 1 ) )
            YOFF = INT( CDEV( 3 ) )
            XSIZE = INT( CDEV( 2 ) + 0.999 ) - XOFF
            YSIZE = INT( CDEV( 4 ) + 0.999 ) - YOFF
         ENDIF

      ELSE
         STATUS = AGI__IMPIC
         CALL ERR_REP( 'AGD_NWIND_IMPIC', 'Picture number is improper',
     :                 STATUS )
         GOTO 99
      ENDIF

*   If the current picture has write access and the workstation has
*   the clear flag set then clear the zone
      IF ( PICACS( CURPID ) .EQ. 'WRITE' ) THEN
         GOTIT = .FALSE.
         DO I = 1, CLRLEN
            IF ( ( CLEARF( I ) .NE. 0 ) .AND.
     :           ( CLEARW( I ) .EQ. WKNAME ) ) THEN
               GOTIT = .TRUE.
               CLRNUM = I
            ENDIF
         ENDDO

*   If the clear flag is set then clear the zone and reset the flag
         IF ( GOTIT ) THEN
*   If the current picture is the base zone then clear the whole memory
            IF ( CURNAM .EQ. 'BASE' ) THEN
               CALL IIMCMY( DISPID, MEMID, 1, 0, ISTAT )
               IF ( ISTAT .NE. 0 ) THEN
                  CALL IIDERR( ISTAT, STRING, LENSTR )
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IMCMY', STRING( :LENSTR ),
     :                          STATUS )
                  GOTO 99
               ENDIF

*   Otherwise have to use a block fill with the background colour
*   First inquire the current transfer window
            ELSE
               CALL IIDQCI( DISPID, 32, MXMEMS, TWSIX, NVAL, ISTAT )
               IF ( ISTAT .NE. 0 ) THEN
                  CALL IIDERR( ISTAT, STRING, LENSTR )
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IDQCI', STRING( :LENSTR ),
     :                          STATUS )
                  GOTO 99
               ENDIF
               CALL IIDQCI( DISPID, 33, MXMEMS, TWSIY, NVAL, ISTAT )
               IF ( ISTAT .NE. 0 ) THEN
                  CALL IIDERR( ISTAT, STRING, LENSTR )
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IDQCI', STRING( :LENSTR ),
     :                          STATUS )
                  GOTO 99
               ENDIF
               CALL IIDQCI( DISPID, 34, MXMEMS, TWOFX, NVAL, ISTAT )
               IF ( ISTAT .NE. 0 ) THEN
                  CALL IIDERR( ISTAT, STRING, LENSTR )
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IDQCI', STRING( :LENSTR ),
     :                          STATUS )
                  GOTO 99
               ENDIF
               CALL IIDQCI( DISPID, 35, MXMEMS, TWOFY, NVAL, ISTAT )
               IF ( ISTAT .NE. 0 ) THEN
                  CALL IIDERR( ISTAT, STRING, LENSTR )
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IDQCI', STRING( :LENSTR ),
     :                          STATUS )
                  GOTO 99
               ENDIF

*   Set up a new transfer window
               DEPTH = MEMSD( JMEM )
               CALL IIMSTW( DISPID, MEMID, 0, XSIZE, YSIZE, DEPTH, XOFF,
     :                      YOFF, ISTAT )
               IF ( ISTAT .NE. 0 ) THEN
                  CALL IIDERR( ISTAT, STRING, LENSTR )
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IMSTW', STRING( :LENSTR ),
     :                          STATUS )
                  GOTO 99
               ENDIF

*   Fill this transfer window with zeroes by sending the BLANK array as
*   many times as necessary, but to make things easier make sure the
*   filled area comprises of complete lines.
               PACK = 1
               IF ( ( XSIZE .GE. 1 ) .AND. ( XSIZE .LE. NBLANK ) ) THEN
                  SROWS = NBLANK / XSIZE
               ELSE
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IDERR', 'IDI error', STATUS )
                  GOTO 99
               ENDIF
               NSENT = 0
               DO WHILE ( NSENT .LT. YSIZE )
                  NROWS = YSIZE - NSENT
                  IF ( SROWS .LT. NROWS ) THEN
                     NPIX = SROWS * XSIZE
                  ELSE
                     NPIX = NROWS * XSIZE
                  ENDIF
                  CALL IIMWMY( DISPID, MEMID, BLANK, NPIX, DEPTH, PACK,
     :                         0, NSENT, ISTAT )
                  IF ( ISTAT .NE. 0 ) THEN
                     CALL IIDERR( ISTAT, STRING, LENSTR )
                     STATUS = AGI__IDERR
                     CALL ERR_REP( 'AGD_NWIND_IMWMY', STRING( :LENSTR ),
     :                             STATUS )
                     GOTO 99
                  ENDIF
                  NSENT = NSENT + SROWS
               ENDDO

*   Flush the IDI output
               CALL IIDUPD( DISPID, ISTAT )
               IF ( ISTAT .NE. 0 ) THEN
                  CALL IIDERR( ISTAT, STRING, LENSTR )
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IDUPD', STRING( :LENSTR ),
     :                          STATUS )
                  GOTO 99
               ENDIF

*   Restore the old transfer window
               CALL IIMSTW( DISPID, MEMID, 0, TWSIX( JMEM ),
     :                      TWSIY( JMEM ), DEPTH, TWOFX( JMEM ),
     :                      TWOFY( JMEM ), ISTAT )
               IF ( ISTAT .NE. 0 ) THEN
                  CALL IIDERR( ISTAT, STRING, LENSTR )
                  STATUS = AGI__IDERR
                  CALL ERR_REP( 'AGD_NWIND_IMSTW', STRING( :LENSTR ),
     :                          STATUS )
                  GOTO 99
               ENDIF
            ENDIF
            CLEARF( CLRNUM ) = 0
         ENDIF
      ENDIF

*   FLush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGD_NWIND +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

