      PROGRAM IDITEST
*+
*  Name:
*     IDITEST
*  Purpose:
*     TEST IDI
*  Language:
*     FORTRAN
*  Copyright:
*     Copyright (C) 1024, 1989, 1991, 1994 Science & Engineering Research Council.
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
*     NE: Nick Eaton  (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*  History:
*     May 1989 (NE):
*        Original version
*     March 1991 (NE):
*        Update to use NDFs and work with X-windows interface
*     March 1994 (DLT):
*        Converted to non-ADAM program and extended to test more IDI functions.
*-
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'IDI_ERR'          ! IDI error codes
      INCLUDE 'IDI_PAR'          ! IDI global constants
*  Status:
      INTEGER STATUS             ! Global status
*  Local variables:
      CHARACTER * 64 TEXT
      INTEGER TRIGS( IDI__MAXTR ), UPDATE
      INTEGER DEPTH, DSIZE(2), EXTRN, ID, INDF, INTID, INTOP,
     :        INTTY, IPIN, LBND(2), LTEXT, NARR, NCHAR, NDATA,
     :        NDIM, NEL, NUMCUR, NVAL, OBJID, OBJTY, OUTMID,
     :        PACK, UBND(2), XC, XOFF, XSTART, YC, YOFF, YSTART,
     :        MXI, MXJ, MAXCOL, I, J, DDEPTH
      DATA MXI, MXJ /256, 256/
      REAL F(256*256), FMIN, FMAX, DITHER
      INTEGER IDATA(256*256), JDATA(1024*1024), IX(5), IY(5)
      REAL VLUT(3,256),  VLUT2(3,256)

      INTEGER UP, DOWN
      PARAMETER (UP=0, DOWN=1)
      INTEGER MEM0
      PARAMETER (MEM0=0)
      INTEGER LUT0, LUT1
      PARAMETER (LUT0=0, LUT1=1)

*.

      FMIN = F(1)
      FMAX = F(1)
      DO 20 I=0,MXI-1
          DO 10 J=1,MXJ
              F(I*MXI+J) = COS(0.6*SQRT(I*80./MXI)-16.0*J/(3.*MXJ))*
     :                 COS(16.0*I/(3.*MXI))+(I/FLOAT(MXI)-J/FLOAT(MXJ))
              FMIN = MIN(F(I*MXI+J),FMIN)
              FMAX = MAX(F(I*MXI+J),FMAX)
   10     CONTINUE
   20 CONTINUE

*   Open IDI
      CALL IIDOPN( 'xwindows', ID, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Inquire the physical size of the display using Query Capabilities
*   Integer code 12 ( ISIZED ) returns the screen size in pixels
      NARR = 2
      CALL IIDQCI( ID, ISIZED, NARR, DSIZE, NVAL, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Check the image size against the display size
      IF ( MXI.GT.DSIZE(1) ) THEN
         MXI = DSIZE(1)
      ENDIF
      IF ( MXJ.GT.DSIZE(2) ) THEN
         MXJ = DSIZE(2)
      ENDIF

*   Reset the device
      CALL IIDRST( ID, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Inquire the depth of the first (default) memory using Query Capabilities
*   Integer code 22 ( IMEMDE ) returns the memory depth
      NARR = 1
      CALL IIDQCI( ID, IMEMDE, NARR, DEPTH, NVAL, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Scale the data to fit the memory depth
      MAXCOL = (2**DEPTH) -1
      DO 40 I=0,MXI-1
          DO 30 J=1,MXJ
              IDATA(I*MXI+J) = (F(I*MXI+J)-FMIN) / (FMAX - FMIN)
     :                         * MAXCOL
   30     CONTINUE
   40 CONTINUE

*   Set up the transfer window to be the same size as the input image
*   and to be centered in the middle of the screen; XOFF, YOFF define
*   the position of the lower left corner of the transfer window. Plot
*   the image in the base memory ( 0 ) and loading from bottom to top ( 0 )
      XOFF = MAX( 0, ( DSIZE( 1 ) - MXI ) / 2 )
      YOFF = MAX( 0, ( DSIZE( 2 ) - MXJ ) / 2 )
      CALL IIMSTW( ID, MEM0, UP, MXI, MXJ, DEPTH, XOFF, YOFF,
     :             STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Write the image to the screen starting at position 0, 0 in the
*   transfer window. The data is taken from the lowest byte of each
*   integer word in the data array ( defined by PACK = 1, DEPTH = 8 ).
      PACK = 1
      XSTART = 0
      YSTART = 0
      NDATA = MXI * MXJ
      CALL IIMWMY( ID, MEM0, IDATA, NDATA, DEPTH, PACK,
     :             XSTART, YSTART, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Display the memory by setting its visibility to .TRUE.
      CALL IIMSMV( ID, MEM0, 1, 1, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Read back the image
      CALL IIMRMY( ID, MEM0, NDATA, XSTART, YSTART, DEPTH, PACK,
     :             .TRUE., JDATA, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Re-display the read back data
      PRINT *, 'Re-displaying data read back from memory...'
      CALL IIMWMY( ID, MEM0, JDATA, NDATA, DEPTH, PACK,
     :             XSTART, YSTART, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Create a funky lut
      NARR = 1
      CALL IIDQCI( ID, IDISDE, NARR, DDEPTH, NVAL, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

      DO 60 I = 1, 2**DDEPTH
         VLUT(1,I) = REAL(I-1)/(REAL(2**DDEPTH)-1)
         VLUT(2,I) = 1.0 - VLUT(1,I)
         VLUT(3,I) = (VLUT(1,I) * VLUT(2,I)) * 4.0
  60  CONTINUE

*   Load it
      PRINT *, 'Loading funky LUT...'
      CALL IILWLT( ID, LUT0, 0, 2**DDEPTH, VLUT, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Read back lut and check it against the original
      CALL IILRLT( ID, LUT0, 0, 2**DDEPTH, VLUT2, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      DITHER = 1.0/(2**DDEPTH)
      DO 80 J = 1, 2**DDEPTH
         DO 70 I = 1, 3
           IF (ABS(VLUT(I,J)-VLUT2(I,J)).GT.DITHER)
     :          PRINT *, 'VLUT mismatch'
  70     CONTINUE
  80  CONTINUE

*   Create a grey lut
      PRINT *, 'Loading grey LUT...'
      DO 90 I = 1, 2**DDEPTH
         VLUT(1,I) = REAL(I-1)/(REAL(2**DDEPTH)-1)
         VLUT(2,I) = VLUT(1,I)
         VLUT(3,I) = VLUT(1,I)
         CALL IILWLT( ID, LUT1, I-1, 1, VLUT(1,I), STATUS)
         IF ( STATUS .NE. IDI__OK ) GOTO 98
  90  CONTINUE

*   Select it
      CALL IIMSLT( ID, MEM0, LUT1, -1, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Take a snap shot
      CALL IIDSNP( ID, MEM0, NDATA, 0, 0, DEPTH, PACK, JDATA, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Subtract initial image and display that
      DO 100 I = 1, NDATA
         JDATA(I) = ABS(JDATA(I) - IDATA(I))
  100 CONTINUE
      PRINT *,
     :   'Displaying difference between the snapshot and the data...'
      CALL IIMWMY( ID, MEM0, JDATA, NDATA, DEPTH, PACK,
     :             XSTART, YSTART, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      VLUT(1,1) = 1.0
      VLUT(2,1) = 0.0
      VLUT(3,1) = 0.0
      CALL IILWLT( ID, LUT1, 1, 1, VLUT(1,1), STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      VLUT(1,1) = 0.0
      VLUT(2,1) = 1.0
      VLUT(3,1) = 0.0
      CALL IILWLT( ID, LUT1, 2, 1, VLUT(1,1), STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      VLUT(1,1) = 0.0
      VLUT(2,1) = 0.0
      VLUT(3,1) = 1.0
      CALL IILWLT( ID, LUT1, 3, 1, VLUT(1,1), STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      VLUT(1,1) = 1.0
      VLUT(2,1) = 0.0
      VLUT(3,1) = 1.0
      CALL IILWLT( ID, LUT1, 4, 1, VLUT(1,1), STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Clear the memory
      CALL IIMCMY( ID, MEM0, 1, 0, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Set the background to white and check that snapshot gives us 255
*   for all pixels
      VLUT(1,1) = 1.0
      VLUT(2,1) = 1.0
      VLUT(3,1) = 1.0
      CALL IILWLT( ID, LUT1, 0, 1, VLUT(1,1), STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIDSNP( ID, MEM0, NDATA, 0, 0, DEPTH, PACK, JDATA, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

      IF (JDATA(1).NE.255) PRINT *,
     :                  'Snapshot of white does not give 255', JDATA(1)

*   Repeat for black
      VLUT(1,1) = 0.0
      VLUT(2,1) = 0.0
      VLUT(3,1) = 0.0
      CALL IILWLT( ID, LUT1, 0, 1, VLUT(1,1), STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIDSNP( ID, 0, NDATA, 0, 0, DEPTH, PACK, JDATA, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

      IF (JDATA(1).NE.0) PRINT *,
     :                  'Snapshot of black does not give 0'

*   Change load direction and plot the image again
      YSTART = MXJ - 1
      CALL IIMSTW( ID, MEM0, DOWN, MXI, MXJ, DEPTH, XOFF, YOFF,
     :             STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      PRINT *, 'Re-displaying with load direction reversed...'
      CALL IIMWMY( ID, MEM0, IDATA, NDATA, DEPTH, PACK,
     :             XSTART, YSTART, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98


*   Plot a text string
      CALL IIGTXT( ID, MEM0, 'IDI test program', 100,
     :             100, 0, 0, (2**DDEPTH)-1, 0, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIGTXT( ID, MEM0, 'wibble...', 100,
     :             300, 0, 0, (2**DDEPTH)-1, 0, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Outline the image
      IX(1) = XOFF
      IY(1) = YOFF
      IX(2) = XOFF + MXI -1
      IY(2) = YOFF
      IX(3) = IX(2)
      IY(3) = YOFF + MXJ -1
      IX(4) = XOFF
      IY(4) = IY(3)
      IX(5) = XOFF
      IY(5) = YOFF
      CALL IIGPLY( ID, MEM0, IX, IY, 5, (2**DDEPTH)-1, 1, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Snapshot the whole window with the coloured lut
      VLUT(1,1) = 1.0
      VLUT(2,1) = 1.0
      VLUT(3,1) = 1.0
      CALL IILWLT( ID, LUT0, 0, 1, VLUT(1,1), STATUS)
      CALL IIMSLT( ID, MEM0, 0, -1, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIMSTW( ID, MEM0, UP, DSIZE(1), DSIZE(2), DEPTH, 0, 0,
     :             STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIDSNP( ID, MEM0, DSIZE(1)*DSIZE(2), 0, 0, DEPTH, PACK,
     :             JDATA, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Display the snapshot with the grey lut
      CALL IIMCMY( ID, MEM0, 1, 0, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      YSTART = 0
      CALL IIMWMY( ID, MEM0, JDATA, DSIZE(1)*DSIZE(2), DEPTH, PACK,
     :             XSTART, YSTART, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIMSLT( ID, MEM0, 1, -1, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Set up interactions to move the cursor and zoom the memory
*   Set up the mouse ( interactor type = 0, interactor id = 0 ) to
*   control the cursor ( object type = 1, object id = 0 ) by moving
*   it ( interactive operation = 1 ). End the interaction by pressing
*   the right hand button ( exit trigger number = 2 ).
      INTTY = 0
      INTID = 0
      OBJTY = 1
      OBJID = 0
      INTOP = 1
      EXTRN = 2
      IX(1) = IX(1) + 20
      IX(2) = IX(1)
      CALL IIGPLY( ID, MEM0, IX, IY, 2, (2**DDEPTH)-1, 3, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Snapshot the whole window with the coloured lut
      VLUT(1,1) = 1.0
      VLUT(2,1) = 1.0
      VLUT(3,1) = 1.0
      CALL IILWLT( ID, LUT0, 0, 1, VLUT(1,1), STATUS)
      CALL IIMSLT( ID, MEM0, 0, -1, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIMSTW( ID, MEM0, UP, DSIZE(1), DSIZE(2), DEPTH, 0, 0,
     :             STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIDSNP( ID, MEM0, DSIZE(1)*DSIZE(2), 0, 0, DEPTH, PACK,
     :             JDATA, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Display the snapshot with the grey lut
      CALL IIMCMY( ID, MEM0, 1, 0, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      YSTART = 0
      CALL IIMWMY( ID, MEM0, JDATA, DSIZE(1)*DSIZE(2), DEPTH, PACK,
     :             XSTART, YSTART, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      CALL IIMSLT( ID, MEM0, 1, -1, STATUS)
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      STATUS = UPDATE( ID )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Set up interactions to move the cursor and zoom the memory
*   Set up the mouse ( interactor type = 0, interactor id = 0 ) to
*   control the cursor ( object type = 1, object id = 0 ) by moving
*   it ( interactive operation = 1 ). End the interaction by pressing
*   the right hand button ( exit trigger number = 2 ).
      INTTY = 0
      INTID = 0
      OBJTY = 1
      OBJID = 0
      INTOP = 1
      EXTRN = 2
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Inquire the interactive operation and instruct the user
      CALL IIIQID( ID, INTTY, INTID, TEXT, LTEXT, STATUS )
      TEXT( LTEXT + 1 : ) = ' to control cursor'
      PRINT *, TEXT

*   Set up the left hand button ( interactor type = 5, interactor id = 0 )
*   to control the memory ( object type = 5, object id = 0 ) by increasing
*   the zoom ( interactive operation = 3 ). End the interaction by pressing
*   the right hand button ( exit trigger number = 2 ).
      INTTY = 5
      INTID = 0
      OBJTY = 5
      OBJID = MEM0
      INTOP = 3
      EXTRN = 2
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Inquire the interactive operation and instruct the user
      CALL IIIQID( ID, INTTY, INTID, TEXT, LTEXT, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      TEXT( LTEXT + 1 : ) = ' to increase zoom'
      PRINT *, TEXT

*   Set up the centre button ( interactor type = 5, interactor id = 1 )
*   to control the memory ( object type = 5, object id = 0 ) by decreasing
*   the zoom ( interactive operation = 4 ). End the interaction by pressing
*   the right hand button ( exit trigger number = 2 ).
      INTTY = 5
      INTID = 1
      OBJTY = 5
      OBJID = MEM0
      INTOP = 4
      EXTRN = 2
      CALL IIIENI( ID, INTTY, INTID, OBJTY, OBJID, INTOP, EXTRN,
     :             STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Inquire the interactive operation and instruct the user
      CALL IIIQID( ID, INTTY, INTID, TEXT, LTEXT, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      TEXT( LTEXT + 1 : ) = ' to decrease zoom'
      PRINT *, TEXT

*   Inquire the exit trigger operation and instruct the user
      CALL IIIQID( ID, INTTY, EXTRN, TEXT, LTEXT, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98
      TEXT( LTEXT + 1 : ) = ' to exit'
      PRINT *, TEXT

*   Move the cursor to the middle of the screen. A memory id = -1 sets
*   the cursor position relative to the screen origin.
      XC = DSIZE( 1 ) / 2
      YC = DSIZE( 2 ) / 2
      NUMCUR = 0
      CALL IICINC( ID, -1, NUMCUR, 1, 2, XC, YC, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Display the cursor by setting its visibility to .TRUE.
      CALL IICSCV( ID, NUMCUR, 1, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Execute the interactions.
      CALL IIIEIW( ID, TRIGS, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Read the cursor position relative to the memory origin.
      CALL IICRCP( ID, MEM0, NUMCUR, XC, YC, OUTMID, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

*   Output the cursor position.
      PRINT*, 'Cursor position is ', XC, YC

*   Undisplay the cursor by setting its visibility to .FALSE.
      CALL IICSCV( ID, NUMCUR, 0, STATUS )
      IF ( STATUS .NE. IDI__OK ) GOTO 98

      GO TO 99
  98  CONTINUE

*   Obtain a meaningful IDI error message
      CALL IIDERR( STATUS, TEXT, NCHAR )

*   Output this message
      PRINT *, TEXT(:NCHAR)

  99  CONTINUE

*   Close down IDI
      CALL IIDCLO( ID, STATUS )

      END

      INTEGER FUNCTION UPDATE( ID )
      INTEGER ID, STATUS
      CALL IIDUPD( ID, STATUS )
      CALL SLEEP(2)
      UPDATE = STATUS
      RETURN
      END
