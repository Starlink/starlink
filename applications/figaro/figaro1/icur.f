      SUBROUTINE ICUR
*+
*  Name:
*     ICUR

*  Purpose:
*     Inspect image with cursor.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Figaro application

*  Invocation:
*     CALL ICUR

*  Description:
*     This routine displays the position and data value according to the
*     position of the cursor on the image display. Up to 50 pixel
*     positions can be recorded for later use by other applications.
*
*     Use the cursor to select a position in the image previously
*     displayed with the application IMAGE. Press one of the following:
*
*        D     to display coordinates and pixel value,
*     <space>  to record pixel position,
*        Q     to quit the application.
*
*     Any other key is treated like 'D', alphabetic keys are
*     case-insensitive.

*  ADAM Parameters:
*     IDEV = _CHAR (Read)
*        The name of the imaging device, normally got from a global
*        parameter which was set with the IDEV command.
*     IMARRAY( 12 ) = _REAL (Read)
*        Information about the displayed part of the image and the part
*        of the display used - set by IMAGE or similar.
*     IMFILE = _CHAR (Read)
*        File name of the image displayed - set by IMAGE or similar.
*     XPIXELS( 50 ) = _REAL (Write)
*        The pixel numbers in X for the points indicated by the cursor.
*     YPIXELS( 50 ) = _REAL (Write)
*        The pixel numbers in Y for the points indicated by the cursor.
*     NPIXELS = _REAL (Write)
*        The number of points selected by the cursor. Note: if no points
*        are selected, the values of NPIXELS, XPIXELS, YPIXELS are left
*        unchanged.

*  Implementation Status:
*     This routine does not provide a continuous display of coordinates
*     and image value.

*  Authors:
*     KS: Keith Shortridge (CIT, AAO)
*     DJA: {authors_name} (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJCL: Martin Clayton (Starlink, UCL)
*     ACD: Clive Davenhall (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20 Mar 1984 (KS):
*        Original version.
*     Summer 1987 (DJA):
*        Converted to use DSA_ routines.
*     18 Aug 1988 (KS):
*        References to `Grinnell' removed from comments. Now uses TVSIZE
*        and TVCSIZE to get image display characteristics (and so will
*        needs a version of TVPCKG that supports these), so no longer
*        assumes display is 512 by 512.
*     05 Feb 1991 (KS):
*        Now handles the display parameters as set in IMARRAY by the
*        latest version of IMAGE, which is not as limited in the
*        stretching it can apply to an image to fit it onto the display.
*     05 Mar 1993 (HME):
*        Conversion to IDI. Support bad values.
*     12 Mar 1993 (HME):
*        Conversion to PGPLOT. Use IDEV device.
*     14 Jan 1994 (HME):
*        Change routine prologue to provide the proper description of
*        the application.
*     20 Mar 1996 (HME):
*        Fixed broken format string (the line just broke in
*        the middle of the string, now two strings are concatenated.)
*     16 Feb 1997 (MJCL):
*        Changed the size of the output parameter arrays to be
*        the same as the number of points to be written, rather than
*        the maximum number of points possible.
*     30 May 2002 (ACD):
*        Changed the size of variable IMAGE from 80 to 132.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Global PRIMDAT constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants:
      INTEGER MAXPTS             ! Maximum recorded pixels
      PARAMETER ( MAXPTS = 50 )

*  Local Variables:
      REAL ARRAY( 12 )
      CHARACTER * ( 132 ) IMAGE
      LOGICAL DONE               ! Switch controlling while loop
      INTEGER STATUS             ! Figaro status
      INTEGER IGNORE             ! (F)PAR status
      INTEGER STAT1              ! Figaro status
      INTEGER STAT2              ! Figaro status
      INTEGER IDISTA             ! IDI status
      INTEGER IXST               ! Image subset start in X
      INTEGER IXEN               ! Image subset end in X
      INTEGER IYST               ! Image subset start in Y
      INTEGER IYEN               ! Image subset end in Y
      INTEGER IXORIG             ! Displaylet origin in X
      INTEGER IYORIG             ! Displaylet origin in Y
      INTEGER IXWID              ! Displaylet width in X
      INTEGER IYWID              ! Displaylet width in Y
      INTEGER NDIM               ! Image dimensionality
      INTEGER DIMS( 2 )          ! Image dimensions
      INTEGER NDELM              ! Image size
      INTEGER DPTR, DSLOT        ! Data pointer and slot
      INTEGER XPTR, XSLOT        ! X axis data pointer and slot
      INTEGER YPTR, YSLOT        ! Y axis data pointer and slot
      INTEGER DSIZE( 2 )         ! IDI display size
      INTEGER IPOS( 2 )          ! Image pixel
      INTEGER NS                 ! Number of pixels recorded
      REAL WINDOW( 4 )           ! PGPLOT window
      REAL CPOS( 2 )             ! Cursor postion
      REAL RDATA( 3 )            ! Image coordinates and value
      REAL XS( MAXPTS )          ! Recorded X pixel numbers
      REAL YS( MAXPTS )          ! Recorded Y pixel numbers
      CHARACTER * ( 32 ) DEVNAM  ! IDI device name
      CHARACTER * ( 64 ) IDIMES  ! String for internal write
      CHARACTER * ( 1 ) KEY      ! The key pressed

*  Internal References:
      INTEGER PGBEGIN            ! Open image device
      INTEGER PGCURSE            ! Get key and cursor position
      INTEGER ICH_FOLD           ! Turn string to upper case
      INTEGER ICH_LEN            ! Used length of a string

*.

*  Startup.
      STATUS = 0
      STAT1  = 0
      STAT2  = 0
      IGNORE = 0
      IDISTA = 0
      CALL DSA_OPEN( STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Reset the pixel records.
      DO 1 NS = 1, MAXPTS
         XS(NS) = 0.
         YS(NS) = 0.
 1    CONTINUE
      NS = 0

*  Get information about the display and displayed image.
      CALL VAR_GETARY( 'IMARRAY', 12, ARRAY, STAT1 )
      CALL VAR_GETCHR( 'IMFILE', 0, 0, IMAGE, STAT2 )
      IF ( ( STAT1 .NE. 0 ) .OR. ( STAT2 .NE. 0 ) ) THEN
         CALL PAR_WRUSER( 'ICUR: Error obtaining display ' //
     :      'information. Probably no image currently displayed.',
     :      IGNORE )
         GO TO 500
      END IF
      IYST = ARRAY(1)
      IYEN = ARRAY(2)
      IXST = ARRAY(3)
      IXEN = ARRAY(4)
      IXWID = ARRAY(7)
      IYWID = ARRAY(8)
      IXORIG = ARRAY(9)
      IYORIG = ARRAY(10)

*  Map the displayed data.
      CALL DSA_NAMED_INPUT( 'IMAGE', IMAGE, STATUS )
      CALL DSA_USE_FLAGGED_VALUES( 'IMAGE', STATUS )
      CALL DSA_DATA_SIZE( 'IMAGE', 2, NDIM, DIMS, NDELM, STATUS )
      CALL DSA_MAP_DATA( 'IMAGE', 'READ', 'FLOAT',
     :   DPTR, DSLOT, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Map the axis data.
      CALL DSA_MAP_AXIS_DATA( 'IMAGE', 1, 'READ', 'FLOAT',
     :   XPTR, XSLOT, STATUS )
      CALL DSA_MAP_AXIS_DATA( 'IMAGE', 2, 'READ', 'FLOAT',
     :   YPTR, YSLOT, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500

*  Open the image display.
      CALL VAR_GETCHR( 'IDEV', 0, 0, DEVNAM, STATUS )
      IF ( STATUS .NE. 0 ) THEN
         CALL PAR_WRUSER( 'ICUR: Error: No display specified. ' //
     :      'Use IDEV to select an image display.', IGNORE )
         GO TO 500
      END IF
      IDISTA = PGBEGIN( 0, DEVNAM(:ICH_LEN(DEVNAM)) // '/append', 1, 1 )
      IF ( IDISTA .NE. 1 ) THEN
         CALL PAR_WRUSER( 'ICUR: Error opening image display.',
     :      IGNORE )
         GO TO 500
      END IF
      CALL PGASK( .FALSE. )

*  Find out the display size in pixels.
      CALL PGVPORT( 0., 1., 0., 1. )
      CALL PGQVP( 3, WINDOW(1), WINDOW(2), WINDOW(3), WINDOW(4) )
      CALL PGWINDOW( WINDOW(1), WINDOW(2), WINDOW(3), WINDOW(4) )
      DSIZE(1) = INT( WINDOW(2) - WINDOW(1) ) + 1
      DSIZE(2) = INT( WINDOW(4) - WINDOW(3) ) + 1

*  Instruct the user.
      CALL PAR_WRUSER( ' ', IGNORE )
      CALL PAR_WRUSER( 'Use mouse to control cursor.', IGNORE )
      CALL PAR_WRUSER( 'Press "D" to display pixel data.', IGNORE )
      CALL PAR_WRUSER( 'Press space bar to record pixel.', IGNORE )
      CALL PAR_WRUSER( 'Press "Q" to exit.', IGNORE )
      CALL PAR_WRUSER( ' ', IGNORE )

*  Start position for cursor in middle of displaylet.
      CPOS(1) = FLOAT( IXORIG + IXWID / 2 )
      CPOS(2) = FLOAT( IYORIG + IYWID / 2 )

*  Execute the interaction.
*  This is a while loop exited after pressing the Q key.
      DONE = .FALSE.
 2    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .NOT. DONE ) THEN

*     Let user move cursor and wait for key press.
         IDISTA = PGCURSE( CPOS(1), CPOS(2), KEY )
         IF ( IDISTA .NE. 1 ) KEY = 'Q'
         IGNORE = ICH_FOLD( KEY )

*     If exit.
         IF ( KEY .EQ. 'Q' ) THEN
            DONE = .TRUE.

*     Else (don't exit).
         ELSE

*        Then subtract the displaylet origin.
            IPOS(1) = INT(CPOS(1)) - IXORIG
            IPOS(2) = INT(CPOS(2)) - IYORIG

*        If inside displaylet.
            IF ( IPOS(1) .GE. 0 .AND. IPOS(1) .LT. IXWID .AND.
     :           IPOS(2) .GE. 0 .AND. IPOS(2) .LT. IYWID ) THEN

*           Convert cursor position to image pixel.
*           The displaylet IPOS = 0 ... IXWID corresponds to image
*           pixels IPOS = IXST-0.5 ... IXEN+0.5, or rather the NINT
*           thereof.
               IPOS(1) = INT( FLOAT(IXST)
     :            + FLOAT(IPOS(1))/FLOAT(IXWID) * FLOAT(IXEN-IXST+1) )
               IPOS(2) = INT( FLOAT(IYST)
     :            + FLOAT(IPOS(2))/FLOAT(IYWID) * FLOAT(IYEN-IYST+1) )

*           We use a subroutine to work out the axis coordinates and
*           image data value, since here we have only pointers to the
*           arrays. RDATA has three elements, X position, Y position,
*           image value.
               CALL FIG_ICUR_1( IPOS, DIMS(1), DIMS(2),
     :                          %VAL(CNF_PVAL(XPTR)),
     :                          %VAL(CNF_PVAL(YPTR)),
     :                          %VAL(CNF_PVAL(DPTR)), RDATA )

*           If it was the space bar.
               IF ( KEY .EQ. ' ' ) THEN
                  IF ( NS .LT. MAXPTS ) THEN
                     NS = NS + 1
                     XS(NS) = FLOAT(IPOS(1))
                     YS(NS) = FLOAT(IPOS(2))
                     WRITE( IDIMES,
     :                  '(''Pixel record #'',I2.2,'' at'',2I6,2G15.7)' )
     :                  NS, IPOS, RDATA(1), RDATA(2)
                     CALL PAR_WRUSER( IDIMES, IGNORE )
                  ELSE
                     CALL PAR_WRUSER( 'Warning: Cannot record more ' //
     :                  'pixels.', IGNORE )
                  END IF

*           Else (it was the D key - or any key without other meaning).
               ELSE

*              Write the results into a string and write that to the
*              screen.
                  IF ( RDATA(3) .EQ. VAL__BADR ) THEN
                     WRITE( IDIMES, '(2G15.7,'' bad value'')' )
     :                  RDATA(1), RDATA(2)
                  ELSE
                     WRITE( IDIMES, '(3G15.7)' ) RDATA
                  END IF
                  CALL PAR_WRUSER( IDIMES, IGNORE )
               END IF
            END IF
         END IF
         GO TO 2
      END IF

*  Write the recorded pixels.
      IF ( NS .GT. 0 ) THEN
         CALL VAR_SETARY( 'XPIXELS', NS, XS, IGNORE )
         CALL VAR_SETARY( 'YPIXELS', NS, YS, IGNORE )
         CALL VAR_SETNUM( 'NPIXELS', 0, 0, FLOAT(NS), IGNORE )
      END IF

*  Tidy up.
 500  CONTINUE

*  Close down PGPLOT and DSA.
      CALL PGEND
      CALL DSA_CLOSE( STATUS )

*  Return.
      END
      SUBROUTINE FIG_ICUR_1( PIXEL, DIM1, DIM2, XDATA, YDATA, DATA,
     :   PIXDAT )
*+
*  Name:
*     FIG_ICUR_1

*  Purpose:
*     Return coordinates and data value for a pixel.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_ICUR_1( PIXEL, DIM1, DIM2, XDATA, YDATA, DATA, PIXDAT )

*  Description:
*     This routine returns the X and Y coordinates and the data value of
*     a specified pixel in a given image (2-D array). This routine is
*     not actually doing anything. Only the calling routine will often
*     have only pointers to XDATA, YDATA, and DATA.

*  Arguments:
*     PIXEL( 2 ) = INTEGER (Given)
*        The pixel number in X and Y.
*     DIM1 = INTEGER (Given)
*        The X dimension (size) of the image.
*     DIM2 = INTEGER (Given)
*        The Y dimension (size) of the image.
*     XDATA( DIM1 ) = REAL (Given)
*        The array of X coordinates for each pixel.
*     YDATA( DIM2 ) = REAL (Given)
*        The array of Y coordinates for each pixel.
*     DATA( DIM1, DIM2 ) = REAL (Given)
*        The image array.
*     PIXDAT( 3 ) = REAL (Returned)
*        The three elements of this vector are the X coordinate for
*        PIXEL(1), the Y coordinate for PIXEL(2), and the image value
*        for (PIXEL(1),PIXEL(2)).

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 Mar 1993 (HME):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER PIXEL( 2 )
      INTEGER DIM1
      INTEGER DIM2
      REAL XDATA( DIM1 )
      REAL YDATA( DIM2 )
      REAL  DATA( DIM1, DIM2 )

*  Arguments Returned:
      REAL PIXDAT( 3 )

*.

      PIXDAT(1) = XDATA( PIXEL(1) )
      PIXDAT(2) = YDATA( PIXEL(2) )
      PIXDAT(3) =  DATA( PIXEL(1), PIXEL(2) )

      END
