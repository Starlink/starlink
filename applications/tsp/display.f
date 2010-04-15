C+
      SUBROUTINE DISPLAY(STATUS)
C
C            D I S P L A Y
C
C     Command name:
C        DISPLAY
C
C     Function:
C        Display a 3D TSP dataset on an image display device
C
C     Description:
C        Display a 3D time series image dataset on an image display device.
C        There are a range of options selected with the COMMAND parameter.
C        The image may be displayed as a single frame, as a movie of a
C        sequence of frames etc.
C
C        The COMMANDs available are as follows (only the first letter is
C        recognized:
C
C        Frame - Select a frame number to display
C
C        Movie - Display a movie for a range of frames
C
C        Cursor - Put up the cursor to read coordinates and data values
C
C        Quit - Exit from DISPLAY
C
C     Parameters:
C    (1) INPUT     (TSP, 3D)   The time series dataset to be displayed.
C    (2) DEVICE    (Device)    The GKS image display device
C    (3) HIGH      (Real)      Upper scaling level
C    (4) LOW       (Real)      Lower scaling level
C    (5) FIRST     (Integer)   First frame for movie
C    (6) LAST      (Integer)   Last frame for movie
C        COMMAND   (Char)      Command to control further options
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         18/11/1991
C
C-
C
C  History:
C    19/10/1989   Original Version (MOVIE).   JAB/JAC
C    21/10/1989   Change name to DISPLAY, add command mode.  JAB/JAC
C    18/11/1991   Handle bad values     JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,LOC2,ILOC,TLOC

*  Data pointers
      INTEGER PTR,IPTR,TPTR

*  SGSOP Zone
      INTEGER ZONE

*  Data dimensions
      INTEGER NDIMS,DIMS(3)

*  Number of display colours
      REAL COLS

*  Minimum colour index
      INTEGER CMIN

*  Range of frames
      INTEGER F1,F2

*  Data Maximum and minimum
      REAL XMIN,XMAX

*  Command string
      CHARACTER*64 COMMAND

      LOGICAL FINISHED,OK
      REAL X,Y,XX
      INTEGER NX,NY,N

*  Get the input file
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Map the data
      CALL TSP_MAP_DATA(LOC,'READ',PTR,LOC2,STATUS)

*  Get the size of the data
      CALL TSP_SIZE(LOC,3,DIMS,NDIMS,STATUS)

*  Map the time axis
      CALL TSP_MAP_TIME(LOC,'READ',TPTR,TLOC,STATUS)

*  Make temporary array for display
      CALL TSP_TEMP(DIMS(1)*DIMS(2),'_INTEGER',IPTR,ILOC,STATUS)

*  Open display device
      CALL SGS_ASSOC('DEVICE','WRITE',ZONE,STATUS)

*  Set colour table
      CALL TSP_DISPLAY_SETCOL(COLS,CMIN,STATUS)

*  Draw Border
      CALL SGS_SPEN(1)
      CALL SGS_BOX(0.01,0.99,0.01,0.99)
      CALL SGS_BOX(0.008,0.992,0.008,0.992)
      CALL SGS_BOX(0.02,0.98,0.86,0.98)
      CALL SGS_LINE(0.69,0.98,0.69,0.86)
      CALL SGS_BOX(0.02,0.98,0.02,0.85)

*  Draw Title
      CALL SGS_SHTX(0.03)
      CALL SGS_SPEN(2)
      CALL SGS_TX(0.05,0.93,'TSP DISPLAY')
      CALL SGS_TX(0.051,0.93,'TSP DISPLAY')
      CALL SGS_TX(0.052,0.93,'TSP DISPLAY')
      CALL SGS_TX(0.053,0.93,'TSP DISPLAY')
      CALL SGS_FLUSH

*  Get maximum and minimum of data for first frame
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_DISPLAY_SCALE(DIMS(1),DIMS(2),DIMS(3),%VAL(PTR),
     :        XMIN,XMAX,STATUS)
      ENDIF

*  Get scaling levels using data maximum and minimum as defaults
      CALL PAR_DEF0R('HIGH',XMAX,STATUS)
      CALL PAR_GET0R('HIGH',XMAX,STATUS)
      CALL PAR_DEF0R('LOW',XMIN,STATUS)
      CALL PAR_GET0R('LOW',XMIN,STATUS)

*  Display the first frame
      F1=1
      F2=1
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_DISPLAY(DIMS(1),DIMS(2),DIMS(3),%VAL(PTR),%VAL(IPTR),
     :           %VAL(TPTR),XMIN,XMAX,F1,F2,COLS,CMIN,STATUS)
      ENDIF

*  Enter command mode - get a command and process it
      FINISHED = .FALSE.
      DO WHILE (STATUS .EQ. SAI__OK .AND. (.NOT. FINISHED))
          CALL PAR_GET0C('COMMAND',COMMAND,STATUS)
          CALL PAR_CANCL('COMMAND',STATUS)

*  Quit command
          IF (COMMAND(1:1) .EQ. 'Q' .OR. COMMAND(1:1) .EQ. 'q') THEN
              FINISHED = .TRUE.
          ELSE IF (COMMAND(1:1) .EQ. 'M' .OR.
     :             COMMAND(1:1) .EQ. 'm') THEN

*  Display MOVIE of a range of frames
*  Get range of frames
              CALL MSG_SETI('FRAMES',DIMS(3))
              CALL MSG_OUT(' ',
     :            'Time Series Contains ^FRAMES Frames',STATUS)

*  Prompt user for first frame
              CALL PAR_DEF0I('FIRST',1,STATUS)
              CALL PAR_GET0I('FIRST',F1,STATUS)
              IF (F1 .LT. 1) F1=1
              IF (F1 .GT. DIMS(3)) F1=DIMS(3)

*  Prompt user for last frame
              CALL PAR_DEF0I('LAST',DIMS(3),STATUS)
              CALL PAR_GET0I('LAST',F2,STATUS)
              IF (F2 .LT. 1) F2=1
              IF (F2 .GT. DIMS(3)) F2=DIMS(3)
              CALL PAR_CANCL('FIRST',STATUS)
              CALL PAR_CANCL('LAST',STATUS)

*  Display the movie
              IF (STATUS .EQ. SAI__OK) THEN
                  CALL TSP_DISPLAY(DIMS(1),DIMS(2),DIMS(3),%VAL(PTR),
     :              %VAL(IPTR),%VAL(TPTR),XMIN,XMAX,F1,F2,COLS,CMIN,
     :              STATUS)
              ENDIF
          ELSE IF (COMMAND(1:1) .EQ. 'F' .OR.
     :             COMMAND(1:1) .EQ. 'f') THEN

*  Display a single frame
              F2=F2+1
              IF (F2 .GT. DIMS(3)) F2=DIMS(3)

*  Prompt user for frame number
              CALL PAR_DEF0I('FRAME',F2,STATUS)
              CALL PAR_GET0I('FRAME',F2,STATUS)
              CALL PAR_CANCL('FRAME',STATUS)
              F1=F2

*  Display the frame
              IF (STATUS .EQ. SAI__OK) THEN
                  CALL TSP_DISPLAY(DIMS(1),DIMS(2),DIMS(3),%VAL(PTR),
     :              %VAL(IPTR),%VAL(TPTR),XMIN,XMAX,F1,F2,COLS,CMIN,
     :              STATUS)
              ENDIF

          ELSE IF (COMMAND(1:1) .EQ. 'C' .OR.
     :             COMMAND(1:1) .EQ. 'c') THEN

*  Read cursor position
              OK = .FALSE.
              DO WHILE (.NOT. OK)
                  CALL SGS_REQCU(X,Y,N)
                  IF (X .GE. 0.022 .AND. X .LE. 0.978 .AND.
     :                Y .GE. 0.022 .AND. Y .LE. 0.848) THEN
                      OK = .TRUE.
                  ENDIF
              ENDDO

*  Calculate pixel coordinates
              NX = INT((X-0.022)/(0.978-0.022)*REAL(DIMS(1)))+1
              NY = INT((Y-0.022)/(0.848-0.022)*REAL(DIMS(2)))+1
              IF (NX .LT. 1) NX=1
              IF (NX .GT. DIMS(1)) NX = DIMS(1)
              IF (NY .LT. 1) NY=1
              IF (NY .GT. DIMS(2)) NY = DIMS(2)

*  Get the data value for pixel
              IF (STATUS .EQ. SAI__OK) THEN
                  CALL TSP_DISPLAY_GET(DIMS(1),DIMS(2),DIMS(3),
     :                NX,NY,F2,%VAL(PTR),XX)
              ENDIF

*  Output results
              CALL MSG_SETI('NX',NX)
              CALL MSG_SETI('NY',NY)
              CALL MSG_SETR('XX',XX)
              CALL MSG_OUT(' ','X = ^NX Y = ^NY  ^XX',STATUS)
          ENDIF
      ENDDO
      IF (STATUS .EQ. 2) STATUS = SAI__OK

*  Tidy up
      CALL TSP_UNMAP(LOC2,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL SGS_ANNUL(ZONE,STATUS)
      CALL TSP_UNMAP(ILOC,STATUS)
      END


      SUBROUTINE TSP_DISPLAY(N1,N2,N3,X,IX,TD,XMIN,XMAX,F1,F2,COLS,CMIN,
     :    STATUS)
*+
*
*   TSP_DISPLAY
*
*   Display one or more frames sequentially on the image display device
*
*   The plotting is done using the GKS cell array (GCA) routine. The plot
*   device must already be open, and the appropriate coordinate system
*   and colour table set up.
*
*   As well as plotting the image this routine outputs the frame number
*   and time values in the text area above the image display area.
*
*   (>)  N1      (Integer)  First dimension of the data cube
*   (>)  N2      (Integer)  Second dimension of the data cube
*   (>)  N3      (Integer)  Third dimension of the data cube
*   (>)  X       (Real array (N1,N2,N3))  The time series image data
*   (>)  IX      (Integer array (N1,N2))  Workspace array to use for display
*   (>)  TD      (Double array (N3))  Time axis of data (Modified Julian Date)
*   (>)  XMIN    (Real)     Minimum data value for display
*   (>)  XMAX    (Real)     Maximum data value for display
*   (>)  F1      (Integer)  First frame to display
*   (>)  F2      (Integer)  Second frame to display
*   (>)  COLS    (Real)     Number of colours
*   (>)  CMIN    (Integer)  Minimum colour index
*   (>)  STATUS  (Integer)  Status value
*
*   Jeremy Bailey   19/10/1989
*
*   Modified:
*     18/11/1991   Handle bad values
*
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER N1,N2,N3
      REAL X(N1,N2,N3)
      INTEGER IX(N1,N2)
      DOUBLE PRECISION TD(N3)
      REAL XMIN,XMAX
      INTEGER F1,F2
      REAL COLS
      INTEGER CMIN
      INTEGER STATUS

*  Local variables
      INTEGER I1,I2,FRAME,CMAX
      CHARACTER*12 UTDATE,UTC,UTDOLD
      DOUBLE PRECISION D,MJD
      INTEGER FINT
      INTEGER LFRAME
      DATA LFRAME /0/
      SAVE LFRAME

*  Initialize old UT value
      UTDOLD = ' '

*  Output the scaling levels
      CALL SGS_SPEN(2)
      CALL SGS_SHTX(0.02)
      CALL SGS_TX(0.70,0.91,'High:')
      CALL SGS_TXR(0.77,0.91,XMAX,-8,1)
      CALL SGS_TX(0.70,0.87,'Low:')
      CALL SGS_TXR(0.77,0.87,XMIN,-8,1)
      CALL SGS_OTEXT

*  Set maximum colour index
      CMAX = CMIN+NINT(COLS)-1

*  Determine the movie direction
      IF (F2 .LT. F1) THEN
          FINT = -1
      ELSE
          FINT = 1
      ENDIF

*  Loop over frame numbers
      DO FRAME = F1,F2,FINT

*  Loop over pixels
          DO I2=1,N2
              DO I1=1,N1

*  If the data value is bad set to minimum colour index
                IF (X(I1,I2,FRAME) .EQ. VAL__BADR) THEN
                  IX(I1,I2) = CMIN
                ELSE

*  Otherwise set a colour index in proportion to the value
                  IX(I1,I2) = COLS*(X(I1,I2,FRAME)-XMIN)/(XMAX-XMIN)
                  IX(I1,I2) = IX(I1,I2)+CMIN

*  And clip to the maximum and minimum indices
                  IF (IX(I1,I2) .GT. CMAX) IX(I1,I2) = CMAX
                  IF (IX(I1,I2) .LT. CMIN) IX(I1,I2) = CMIN
                ENDIF
              ENDDO
          ENDDO

*  Output frame information in the text area at the top of the display

*  Output the frame number
          IF (LFRAME .NE. 0) THEN
              CALL SGS_SPEN(3)
              CALL SGS_BTEXT(0.05,0.87)
              CALL SGS_ATEXT('Frame: ')
              CALL SGS_ATXI(LFRAME,-4)

*  Output UT title
              CALL SGS_ATEXT('  UT: '//UTC)
              CALL SGS_OTEXT
          ENDIF

*  Calculate the date and time from the MJD
          D = MOD(TD(FRAME),1D0)
          MJD = TD(FRAME)

*  Output the UT date
          CALL TCV_MJD2S(MJD,UTDATE,STATUS)
          IF (UTDATE .NE. UTDOLD) THEN
              CALL SGS_SPEN(3)
              CALL SGS_TX(0.3,0.90,UTDOLD)
              CALL SGS_SPEN(2)
              CALL SGS_TX(0.3,0.90,UTDATE)
              UTDOLD = UTDATE
          ENDIF

*  Output the UT time
          CALL TCV_TIME2S(D,2,':',UTC,STATUS)
          CALL SGS_SPEN(2)
          CALL SGS_SHTX(0.02)
          CALL SGS_BTEXT(0.05,0.87)
          CALL SGS_ATEXT('Frame: ')
          CALL SGS_ATXI(FRAME,-4)
          CALL SGS_ATEXT('  UT: '//UTC)
          CALL SGS_OTEXT

*  Output message on terminal
          CALL MSG_SETI('FRAME',FRAME)
          CALL MSG_OUT(' ','Displaying frame ^FRAME',STATUS)

*  Display the image
          CALL GCA(0.022,0.022,0.978,0.848,N1,N2,1,1,N1,N2,IX)

*  Flush the graphics output
          CALL SGS_FLUSH
          LFRAME = FRAME
      ENDDO

      END

      SUBROUTINE TSP_DISPLAY_SCALE(N1,N2,N3,X,XMIN,XMAX,STATUS)
*+
*
*   TSP_DISPLAY_SCALE
*
*   Find the maximum and minimum values for the first frame in a
*   time series image
*
*   (>)   N1   (Integer)   First dimension of cube
*   (>)   N2   (Integer)   Second dimension of cube
*   (>)   N3   (Integer)   Third dimension of cube
*   (>)   X    (Real array(N1,N2,N3)  Time series image data
*   (<)   XMIN (Real)      Minimum value in the array
*   (<)   XMAX (Real)      Maximum value in the array
*   (!)   STATUS (Integer) Status value
*
*   Jeremy Bailey    19/10/1989
*
*   Modified:
*      18/11/1991   Handle bad values
*
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER N1,N2,N3
      REAL X(N1,N2,N3),XMIN,XMAX
      INTEGER STATUS

*  Local variables
      INTEGER I1,I2

*  Find max and min of first frame
      XMIN = VAL__MAXR
      XMAX = VAL__MINR
      DO I2=1,N2
         DO I1=1,N1
           IF (X(I1,I2,1) .NE. VAL__BADR) THEN
             IF (X(I1,I2,1) .LT. XMIN) XMIN = X(I1,I2,1)
             IF (X(I1,I2,1) .GT. XMAX) XMAX = X(I1,I2,1)
           ENDIF
         ENDDO
      ENDDO

      END



      SUBROUTINE TSP_DISPLAY_SETCOL(COLS,CMIN,STATUS)
*+
*
*  TSP_DISPLAY_SETCOL
*
*  Set up a grey scale colout table for the image display device and
*  return the number of colour indices and the minimum colour index
*
*  The first three colour table entires are set to values which can
*  be used for line graphics
*
*   (<)   COLS    (Integer)   Number of colours used for colour table
*   (<)   CMIN    (Integer)   Minimum colour index
*   (!)   STATUS  (Integer)   Status value
*
*  Jeremy Bailey  19/10/1989
*
*  Modified:
*
*+

      IMPLICIT NONE

*  Parameters
      REAL COLS
      INTEGER CMIN
      INTEGER STATUS

*  Local variables
      INTEGER IWKID,KERROR,CONID,WTYPE,NCOL,JCOL,NPCI
      INTEGER I
      REAL X

*  Find out how many colours the workstation supports

      CALL SGS_ICURW(IWKID)
      CALL GQWKC(IWKID,KERROR,CONID,WTYPE)
      CALL GQCF(WTYPE,KERROR,NCOL,JCOL,NPCI)

      IF (JCOL .EQ. 0 .OR. NPCI .LT. 8) THEN
         CALL MSG_OUT(' ','Workstation does not support enough colours'
     :      //' for image display',STATUS)
         STATUS = 2
         RETURN
      ENDIF

*  Set the first three entries

      CALL GSCR(1,1,0.78,0.54,0.15)
      CALL GSCR(1,2,0.61,0.61,1.00)
      CALL GSCR(1,3,0.0,0.0,0.0)

*  Set remainder of colours to a greyscale colour table

      CMIN = 4
      COLS = NPCI-4
      DO I=4,NPCI-1
          X = REAL(I-4)/REAL(NPCI-5)
          CALL GSCR(1,I,X,X,X)
      ENDDO

      END


      SUBROUTINE TSP_DISPLAY_GET(N1,N2,N3,I1,I2,I3,X,XX)
*+
*
*   TSP_DISPLAY_GET
*
*   Get a single value out of a time series image
*
*   (>)   N1   (Integer)   First dimension of cube
*   (>)   N2   (Integer)   Second dimension of cube
*   (>)   N3   (Integer)   Third dimension of cube
*   (>)   I1   (Integer)   First index
*   (>)   I2   (Integer)   Second index
*   (>)   I3   (Integer)   Third index
*   (>)   X    (Real array(N1,N2,N3)  Time series image data
*   (>)   XX   (Real)      Pixel value
*
*  Jeremy Bailey    19/10/1989
*
*  Modified:
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER N1,N2,N3,I1,I2,I3
      REAL X(N1,N2,N3),XX

*  Copy value
      XX = X(I1,I2,I3)
      END

