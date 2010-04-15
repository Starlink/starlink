*+  INXY - creates a list of xy positions defined by image-display
*          cursor

      SUBROUTINE INXY( ZONEOV, ZONEO, XCEN, YCEN, LBND, DIM1, DIM2,
     :                 ARRAY, XLOW, XHIGH, YLOW, YHIGH, PNCONT, PNFILE,
     :                 PNTITL, STATUS )
*
*    Description :
*
*     This subroutine writes to an ASCII file a user-defined label,
*     followed by a series of x-y locations and values of pixels within
*     a 2-d array. A loop is performed wherein each pixel is selected
*     via the cursor and pressing choice number one of an image
*     display. A chosen pixel is marked by a green cross on the overlay
*     plane. Repeated positions are obtained until the escape choice is
*     selected. The previous pixel may be excluded from the ASCII file
*     by selecting a graphics choice greater than number one. The cursor
*     need not be placed over the pixel to be withdrawn.
*
*     If a previous run of this application was curtailed, there is a
*     mechanism provided to continue from the point of interruption. The
*     ASCII file created at that time may be read, the pixels selected
*     are marked on the image-display overlay, and subsequent data
*     appended to that file.
*
*     The SGS zones defining the image display and overlay plane must
*     have already been set up, and the picture displayed in the former.
*
*    Invocation :
*
*     CALL INXY( ZONEOV, ZONEO, XCEN, YCEN, LBND, DIM1, DIM2, ARRAY,
*    :           XLOW, XHIGH, YLOW, YHIGH, PNCONT, PNFILE, PNTITL,
*    :           STATUS )
*
*    Arguments :
*
*     ZONEOV = INTEGER ( READ )
*         Frame zone identifier of image-display overlay plane
*     ZONEO = INTEGER ( READ )
*         Data zone identifier of image-display overlay plane
*     XCEN = REAL ( READ )
*         Column centre of the image display
*     YCEN = REAL ( READ )
*         Line centre of the image display
*     LBND( 2 ) = INTEGER ( READ )
*         Lower bounds of the array, thus the first element of the
*         array (1,1) is actually at (LBND(1),LBND(2)).
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     ARRAY( DIM1, DIM2 ) = REAL ( READ )
*         2-d array whose pixel locations and values are to be written
*           to an ASCII file
*     XLOW = REAL ( READ )
*         Minimum x-value that the cursor can take before being outside
*           the 2-d array displayed
*     XHIGH = REAL ( READ )
*         Maximum x-value that the cursor can take before being outside
*           the 2-d array displayed
*     YLOW = REAL ( READ )
*         Minimum y-value that the cursor can take before being outside
*           the 2-d array displayed
*     YHIGH = REAL ( READ )
*         Maximum y-value that the cursor can take before being outside
*           the 2-d array displayed
*     PNCONT = CHARACTER ( READ )
*         Parameter name for whether or not to append to old ASCII file
*     PNFILE = CHARACTER ( READ )
*         Parameter name of new ASCII file
*     PNTITL = CHARACTER ( READ )
*         Parameter name of the label
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Store current zone
*     Select overlay plane
*     Determine whether or not old file to be appended
*     Set appropriate file access
*     Open the file
*     If an error occurred then abort
*     If old file is to be appended to then
*        Read header from file if present
*        If successful then
*           Output to user
*           Show functions of the choice device's buttons on frame zone
*           Do while more pixel positions in file and there is no error
*              Read next line of the data file except when first
*                record was not a label
*              If no error then
*                 Get next pixel position from file as free-format
*                   reals, if the record is not a comment line
*                 If no error then
*                    Increment number of pixels tried by one
*                    If pixel is within the permitted bounds then
*                       Draw a green cross on the overlay plane
*                       Increment valid pixel counter by one
*                    Endif
*                 Else
*                    Report error context, close file and abort
*                 Endif
*              Else
*                 Report error context if not the end of file
*              Endif
*           Enddo
*           If end of file then
*              Annul error
*              Output values of pixel counters
*           Endif
*        Else
*           Report error context, close file and abort
*        Endif
*        Close the file
*     Else
*        Get label (with default if there is an error)
*        Write label to file
*        If status not ok then
*           Report error context, close file and abort
*        Endif
*        Show functions of the choice device's buttons on frame zone
*     Endif
*     Repeat until a valid position has been chosen or exit button
*       has been depressed or there has been an error
*        If button is exit then
*           If there is a record of pixel data to be written then
*              Write line of data to the file
*              If an error has occurred, report context
*           Switch repeat button off
*        Else if first button is pressed then
*           If there is a record of pixel data to be written then
*              Write line of data to the file
*              If an error has occurred, report context and exit
*           If cursor position lies within the bounds of the array then
*              Reset last cursor position to its current situation
*              Convert cursor position to pixel co-ordinates
*              Obtain pixel's value
*              Store x,y position
*           Else
*              Tell user what has happened
*           Endif
*        Else
*           Cancel previous pixel, so that its position and value are not
*             written to the file subsequently
*        Endif
*     End repeat
*     Close the file
*     Restore entry zone
*     End
*
*    Authors :
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     1988 Sep  9 : Original (RL.STAR::CUR)
*     1989 Apr 19 : Converted to graphics database (new argument list -
*                   ZONEOV for ZONEI) (RL.STAR::CUR).
*     1989 Jul 27 : Used packaged FIO_ASSOC to open the x,y file, and
*                   passed the array dimensions as two variables
*                   (RL.STAR::CUR).
*     1990 Jan 8  : Corrected SGS status (RL.STAR::CUR).
*     1990 Feb 20 : AIF_OPFIO renamed AIF_ASFIO (RAL::CUR).
*     1990 Nov 29 : Tested for comment strings, made the label a
*                   standard comment, and stored floating-point
*                   co-ordinates in the x-y list files (RAL::CUR).
*     1991 Jun 10 : Added LBND argument as temporary patch for INSPECT
*                   to use NDF (RAL::CUR).
*     1992 Mar 3  : Replaced AIF parameter-system calls by the extended
*                   PAR library (RAL::CUR).
*     1993 Feb 9  : Used the improved FIO_ASSOC and the new FIO_ANNUL.
*                   (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE            ! no default typing

*    Global Constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PRM_PAR'       ! Magic-value definitions
      INCLUDE  'PAR_ERR'       ! parameter-system errors
      INCLUDE  'FIO_PAR'       ! Fortran-I/O-system constants
      INCLUDE  'FIO_ERR'       ! Fortran-I/O-system errors

*    Import :

      INTEGER
     :  LBND( 2 ),
     :  DIM1, DIM2,
     :  ZONEOV,
     :  ZONEO

      REAL
     :  ARRAY( DIM1, DIM2 ),
     :  XCEN, YCEN,
     :  XLOW, YLOW,
     :  XHIGH, YHIGH

      CHARACTER*(*)
     :  PNCONT,
     :  PNFILE,
     :  PNTITL

*    Status :

      INTEGER STATUS

*    External references :

      INTEGER
     :  CHR_LEN

*    Local Constants :

      INTEGER
     :  NCHLIN                 ! maximum number of characters in a
                               ! an output record
      PARAMETER ( NCHLIN = 80 )

      REAL
     :  CROSIZ,                ! width/height of marking cross
     :  CROARM                 ! length of an arm of marking cross
      PARAMETER ( CROSIZ = 4. )
      PARAMETER ( CROARM = 0.5 * CROSIZ )

*    Local Variables :

      INTEGER
     :  BUTTON,                ! Number of button pressed on the
                               ! trackerball device to get x,y pair
     :  DONE,                  ! number of valid positions deglitched
     :  FD,                    ! file description
     :  HASH,                  ! Column where a hash is found in the
                               ! input buffer
     :  INZONE,                ! SGS zone on entry and exit
     :  NCHAR,                 ! number of characters in a record
     :  NCO,                   ! Character column counter
     :  SHRIEK,                ! Column where a shriek is found in the
                               ! input buffer
     :  TRIED,                 ! number of pixel positions read in
     :  UBND( 2 ),             ! Upper bounds of the array
     :  XV, YV,                ! X,y co-ordinate of selected pixel
     :  XC,                    ! X co-ordinate of last pixel
     :  YC                     ! Y co-ordinate of last pixel

      REAL
     :  PIXVAL,                ! Pixel value
     :  XL, YL,                ! Last valid cursor position
     :  CUR( 2 )               ! X-y co-ordinates of point from cursor

      CHARACTER*(NCHLIN)
     :  LABEL,                 ! Label for a new ASCII file
     :  LINE                   ! Line buffer which takes the image data
                               ! and transfers it to the file

      CHARACTER
     :  ACCESS*6               ! file access

      LOGICAL                  ! true if:
     :  APPEND,                ! append to old ASCII file
     :  LINEIN,                ! The first line of data has been read
     :  REPEAT                 ! when a simulated REPEAT..UNTIL loop
                               ! is to continue

*-
*    Check te inherited status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find the upper bounds.

      UBND( 1 ) = LBND( 1 ) + DIM1 - 1
      UBND( 2 ) = LBND( 2 ) + DIM2 - 1

*    Store supplied graphics zone

      CALL SGS_ICURZ( INZONE )

*    Is file to be appended?

      CALL PAR_GTD0L( PNCONT, .FALSE., .TRUE., APPEND, STATUS )
      CALL PAR_CANCL( PNCONT, STATUS )

      IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*    Set access for opening file

      IF ( APPEND ) THEN
         ACCESS = 'UPDATE'
      ELSE
         ACCESS = 'WRITE'
      END IF

*    attempt to obtain and open a free-format data file

      CALL FIO_ASSOC( PNFILE, ACCESS, 'LIST', NCHLIN, FD, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
         GOTO 999
      END IF
      CALL MSG_SETC( 'FILNAM', PNFILE )
      CALL MSG_OUT( 'LOG', 'Logging to $^FILNAM.', STATUS )

      IF ( APPEND ) THEN

*       Read the old file.  First get the label.

         CALL FIO_READ( FD, LABEL, NCHAR, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*          Look for a comment to confirm it is the title.

            SHRIEK = INDEX( LABEL, '!' )
            HASH = INDEX( LABEL, '#' )

*          Watch for blank lines or comment lines. A hash or shriek
*          in the first column indicates a comment line.  In such
*          cases the line can be ignored.

            IF ( SHRIEK .EQ. 1 .OR. HASH .EQ. 1 .OR.
     :           LABEL .EQ. ' ' ) THEN

*             Write out the label to the user.

               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL MSG_OUT( 'LINE1', 'Title of XY list file :',
     :                       STATUS )
               CALL MSG_SETC( 'HEADER', LABEL( 2:NCHAR ) )
               CALL MSG_OUT( 'LINE2', '^HEADER', STATUS )

*             First record does not contain x-y data.

               LINEIN = .FALSE.
            ELSE
               LINEIN = .TRUE.
            END IF

*          Switch to image-display overlay frame

            CALL SGS_SELZ( ZONEOV, STATUS )

*          give function of trackerball buttons - BTTN3 must be called
*          before the pixels are marked on the overlay plane because it
*          clears the overlay plane

            CALL BTTN3( 'SELECT', 'CANCEL', 'EXIT', '*', '*', .TRUE.,
     :                  STATUS )

            CALL SGS_SELZ( ZONEO, STATUS )

*          initialise the counters

            TRIED  =  0
            DONE   =  0

*          scan round the file until the end is reached

            DO WHILE ( STATUS .EQ. SAI__OK )

               IF ( .NOT. LINEIN ) THEN

*                read in the current pixel position as a free-format
*                integer pair - jump out of loop if the end of file is
*                found

                  CALL FIO_READ( FD, LINE, NCHAR, STATUS )
               ELSE

*                Copy what could have been a label into the data buffer.

                  LINE = LABEL

*                Past the first line so switch of the flag to read
*                subsequent data records.

                  LINEIN = .FALSE.
               END IF

               IF ( STATUS .EQ. SAI__OK ) THEN

*                Look for a comment to confirm it is the title.

                  SHRIEK = INDEX( LINE, '!' )
                  HASH = INDEX( LINE, '#' )

*                Watch for blank lines or comment lines. A hash or
*                shriek in the first column indicates a comment line.
*                In such cases the line can be ignored.

                  IF ( SHRIEK .NE. 1 .AND. HASH .NE. 1 .AND.
     :                 LINE .NE. ' ' ) THEN

*                   extract real positions from the string

                     NCO = 1
                     CALL KPG1_FFRR( LINE, 2, NCO, CUR, STATUS )

                     IF ( STATUS .EQ. SAI__OK ) THEN

*                      increment the counter keeping track of the number
*                      of requested pixels

                       TRIED  =  TRIED + 1

*                      check that given pixel position lies within the
*                      displayed input array

                        IF ( CUR( 1 ) .GE. XLOW .AND.
     :                       CUR( 1 ) .LE. XHIGH .AND.
     :                       CUR( 2 ) .GE. YLOW .AND.
     :                       CUR( 2 ) .LE. YHIGH ) THEN

*                         mark a green cross at the point

                           CALL SGS_SPEN( 3 )
                           CALL KPG1_CROSS( CUR( 1 ), CUR( 2 ), CROSIZ,
     :                                      STATUS )

*                         increment valid position counter by one

                           DONE  =  DONE + 1

*                      end of if-pixel-position-is-within-array check

                        END IF

                     ELSE

                        CALL MSG_SETC( 'BUFFER', LINE )
                        CALL ERR_REP( 'ERR_INXY_INTPDA',
     :                    'INXY: Error reading x-y positions in line '/
     :                    /'^BUFFER.', STATUS )
                        CALL ERR_FLUSH( STATUS )
                        GOTO 980

*                   end of no-error-reading-internal-file check

                     END IF

*                End of comment-line check

                  END IF

               ELSE

*                report error context unless the end-of-file has been
*                reached

                  IF ( STATUS .NE. FIO__EOF ) THEN
                     CALL ERR_REP( 'ERR_INXY_RDDATA',
     :                 'INXY: Error reading data record from the '/
     :                 /'file.', STATUS )
                     CALL ERR_FLUSH( STATUS )
                  END IF

*             end of no-error-reading-record-from-file check

               END IF

*          end of loop round x-y-list file

            END DO

*          the x-y file has no more entries or the file is empty

            IF ( STATUS .EQ. FIO__EOF ) THEN

               CALL ERR_ANNUL( STATUS )

*             output messages to give the number of pixel positions read
*             in and actually deglitched

               CALL MSG_OUT( 'BLANK', ' ', STATUS )
               CALL MSG_SETI( 'TRIED', TRIED )
               CALL MSG_OUT( 'NTRIED',
     :           ' Number of pixel positions read in from file      ='/
     :           /'  ^TRIED.', STATUS )
               CALL MSG_SETI( 'DONE', DONE )
               CALL MSG_OUT( 'NDONE',
     :           ' Number of valid positions within picture bounds  ='/
     :           /'  ^DONE.', STATUS )
               CALL MSG_OUT( 'BLANK', ' ', STATUS )

*          end of file-end-status check

            END IF

         ELSE

*         report error context

            CALL ERR_REP( 'ERR_INXY_RDHEAD',
     :        'INXY: Error reading label record from the file.',
     :        STATUS )
            CALL ERR_FLUSH( STATUS )
            GOTO 980

*       end of read-header-string test

         END IF

      ELSE

*       the file is new so get the label

         CALL PAR_GET0C( PNTITL, LABEL, STATUS )
         CALL PAR_CANCL( PNTITL, STATUS )

         IF ( STATUS .EQ. PAR__ABORT ) THEN
            GOTO 980

*       put in dummy label if there has been an error

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            LABEL = '# KAPPA - Inspect - XYcur'
            NCHAR = 25

         ELSE

*          Add the comment character.

            LABEL = '# '//LABEL( :NCHLIN-2 )

*          Find the length of the label.

            NCHAR = CHR_LEN( LABEL )
         END IF

*       Write buffer to file

         CALL FIO_WRITE( FD, LABEL( :NCHAR ), STATUS )

*       Report and error, close file and return

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'LINE', LINE )
            CALL ERR_REP( 'ERR_INXY_WRHEAD',
     :        'INXY: Error writing label to file. '/
     :        /'Buffer was: ^LINE', STATUS )
            CALL ERR_FLUSH( STATUS )
            GOTO 980
         END IF

*       Give function of trackerball buttons in the frame

         CALL SGS_SELZ( ZONEOV, STATUS )
         CALL BTTN3( 'SELECT', 'CANCEL', 'EXIT', '*', '*', .TRUE.,
     :               STATUS )

         CALL SGS_SELZ( ZONEO, STATUS )
      END IF

*    Use the cursor to define each point whose location and value
*    are to be written to an ASCII file.

      IF ( STATUS .EQ. SAI__OK ) THEN
         REPEAT = .TRUE.

         CALL MSG_OUT( 'BLANK', ' ', STATUS )
         CALL MSG_OUT( 'FUNCTIONS', 'Choices are:', STATUS )
         CALL MSG_OUT( 'FUNCTIONS', 'Select - stores the current '/
     :     /'pixel co-ordinates and value, and writes previous',
     :     STATUS )
         CALL MSG_OUT( 'FUNCTIONS', '         pixel co-ordinates '/
     :     /'and value to the file', STATUS )

         CALL MSG_OUT( 'FUNCTIONS', 'Cancel - cancels the previous '/
     :     /'pixel so that its co-ordinates and value are not', STATUS )
         CALL MSG_OUT( 'FUNCTIONS', '         written to the file',
     :     STATUS )

         CALL MSG_OUT( 'FUNCTIONS', 'Exit   - writes the previous '/
     :     /'pixel co-ordinates and value to the file; closes', STATUS )
         CALL MSG_OUT( 'FUNCTIONS', '         the file and exits',
     :     STATUS )
      END IF

      XC = VAL__BADI
      XL = XCEN
      YL = YCEN

      DO WHILE ( REPEAT .AND. STATUS .EQ. SAI__OK )

         CALL SGS_SETCU( XL, YL )

*       Read choice device

         CALL SGS_REQCU( CUR( 1 ), CUR( 2 ), BUTTON )

*       Convert the `break' on a mouse (the middle button) for image
*       displays to the GKS 7.2 behaviour, so that the interaction can
*       be fully under mouse control.
         IF ( BUTTON .EQ. 0 ) BUTTON = 2

*       Choice may be negative under GKS 7.4.

         IF ( BUTTON .LE. 0 .OR. BUTTON .GT. 3 ) THEN

*          Write previous value if not undefined

            IF ( XC .NE. VAL__BADI ) THEN

               WRITE( LINE, * ) XL, YL, PIXVAL
               NCHAR = CHR_LEN( LINE )

*             Write buffer to file

               CALL FIO_WRITE( FD, LINE( :NCHAR ), STATUS )

*             Report and error, close file and return

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'LINE', LINE )
                  CALL ERR_REP( 'ERR_INXY_WLDAT',
     :              'INXY: Error writing last x, y and value to file. '/
     :              /'Buffer was: ^LINE', STATUS )
                  CALL ERR_FLUSH( STATUS )

               END IF

*             Mark previous point in green

               CALL SGS_SPEN( 3 )
               CALL KPG1_CROSS( XL, YL, CROSIZ, STATUS )

            END IF

*          Exit from loop

            REPEAT = .FALSE.

*       Allowing choice 3 is a temporary fudge to allow the first
*       button of an X-windows mouse to be used.  It will mean that the
*       right-hand white button on an ARGS will behave like the red one
*       (but is anyone still using an ARGS?)

         ELSE IF ( BUTTON .EQ. 1 .OR. BUTTON .EQ. 3 ) THEN

*          Write previous value if not undefined

            IF ( XC .NE. VAL__BADI ) THEN

               WRITE( LINE, * ) XL, YL, PIXVAL
               NCHAR = CHR_LEN( LINE )

*             Write buffer to file

               CALL FIO_WRITE( FD, LINE( :NCHAR ), STATUS )

*             report and error, close file and return

               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'LINE', LINE )
                  CALL ERR_REP( 'ERR_INXY_WDAT',
     :              'INXY: Error writing x, y and value to file. '/
     :              /'Buffer was: ^LINE', STATUS )
                  CALL ERR_FLUSH( STATUS )
                  GOTO 980

               END IF

*             Mark previous point in green

               CALL SGS_SPEN( 3 )
               CALL KPG1_CROSS( XL, YL, CROSIZ, STATUS )

*             There is now no previous pixel to be written so make x
*             co-ordinate undefined

               XC = VAL__BADI
            END IF

*          Check whether the co-ordinates of the position
*          defined by the cursor are actually within the array
*          displayed.

            IF ( ( CUR( 1 ) .GE. XLOW .AND. CUR( 1 ) .LE. XHIGH ) .AND.
     :           ( CUR( 2 ) .GE. YLOW .AND. CUR( 2 ) .LE. YHIGH ) ) THEN

*             Reset last cursor position

               XL = CUR( 1 )
               YL = CUR( 2 )

*             Convert the cursor position to find the selected
*             data pixel

               XV = MIN( UBND( 1 ), MAX( LBND( 1 ), NINT( CUR( 1 ) ) ) )
               YV = MIN( UBND( 2 ), MAX( LBND( 2 ), NINT( CUR( 2 ) ) ) )

*             Get the value of the pixel

               CALL GETV2( ARRAY, DIM1, DIM2, XV - LBND( 1 ) + 1,
     :                     YV - LBND( 2 ) + 1, PIXVAL, STATUS )

*             Mark point in white

               CALL SGS_SPEN( 1 )
               CALL KPG1_CROSS( CUR( 1 ), CUR( 2 ), CROSIZ, STATUS )

*             Store position in case it is to be cancelled

               XC = XV
               YC = YV

            ELSE

               CALL MSG_OUT( 'INXY_POI', 'Cursor value not'/
     :                       /' within array.', STATUS )

            END IF

         ELSE IF ( BUTTON .GT. 1 ) THEN

*          Remove last cross (arms are 2 pixels).  Note it might remove
*          parts of any adjacent crosses

            IF ( XC .NE. VAL__BADI )
     :        CALL SGS_CLRBL( XL - CROARM, XL + CROARM,
     :                        YL - CROARM, YL + CROARM )
            XC = VAL__BADI

         END IF
      END DO

*    Close the output file.

  980 CONTINUE
      CALL FIO_ANNUL( FD, STATUS )

  999 CONTINUE

*    Restore entry zone

      IF ( STATUS .EQ. SAI__OK ) CALL SGS_SELZ( INZONE, STATUS )

      END
