      SUBROUTINE TSP_PHSLOT(SIZE,FSIZE,X,XD,Y,YE,XR,XR2,
     :  XLABEL,YLABEL,TOP,BOT,FIRST,PHSTART,PHEND,
     :  LAST,ITEM,IMIN,IMAX,BINSIZE,PLABEL,FILE,STATUS)
*+
*
*   T S P _ P H S L O T
*
*   PHASEPLOT command
*
*   Subroutine to do the plot
*
*  (>)  SIZE    (Integer) -  The number of data points to plot
*  (>)  FSIZE   (Integer) -  The number of points before binning
*  (>)  X       (Double)  -  The X data array before binning
*  (>)  XD      (Double)  -  The X data array after binning
*  (>)  Y       (Real)    -  The Y data array
*  (>)  YE      (Real)    -  The Y error array
*  (W)  XR      (Real)    -  Workspace real array
*  (W)  XR2     (Real)    -  Workspace real array
*  (>)  YLABEL  (Char)    -  Y axis label
*  (>)  TOP     (Real)    -  Position of top of plot
*  (>)  BOT     (Real)    -  Position of bottom of plot
*  (>)  FIRST   (Logical) -  TRUE if first plot of series
*  (>)  PHSTART (Double)  -  Starting phase for plot
*  (>)  PHEND   (Double)  -  End phase for plot
*  (>)  LAST    (Logical) -  TRUE if last plot of series
*  (>)  ITEM    (Char)    -  Item to be plotted
*  (>)  IMIN    (Real)    -  Minimum Y value for plot scaling
*  (>)  IMAX    (Real)    -  Maximum Y value for plot scaling
*  (>)  BINSIZE (Double)  -  Bin size
*  (>)  PLABEL  (Char)    -  Label for plot
*  (>)  FILE    (Logical) -  True to create file
*  (!)  STATUS  (Integer) -  Status value
*
*   Jeremy Bailey  28/2/1988
*
*   Modified:
*       11/12/1991
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER SIZE,FSIZE
      REAL Y(SIZE),YE(SIZE)
      DOUBLE PRECISION X(FSIZE)
      DOUBLE PRECISION XD(SIZE)
      REAL XR(SIZE),XR2(SIZE)
      REAL TOP,BOT
      LOGICAL ERRORS
      LOGICAL FIRST,LAST
      DOUBLE PRECISION PHSTART,PHEND
      CHARACTER*(*) ITEM
      DOUBLE PRECISION BINSIZE
      REAL IMIN,IMAX
      INTEGER STATUS
      CHARACTER*(*) YLABEL
      CHARACTER*(*) PLABEL
      CHARACTER*40 XLABEL
      LOGICAL FILE

*  Local variables
      INTEGER I
      CHARACTER*80 TSTRING

*  JD zero point
      INTEGER JDZERO

*  Data in autograph coordinates
      REAL GX,GY,GY1,GY2

*  Coordinate conversion functions
      REAL SNX_AGUGX,SNX_AGUGY
      REAL ZEROPT,ZEROT
      CHARACTER*40 LABEL

*  True for line plot
      LOGICAL LINE

*  Pen number
      INTEGER PEN
      REAL CSIZE
      INTEGER IZ,J,IZA
      REAL AX(2),AY(2)

*  File descriptor
      INTEGER FD

*  File buffer
      CHARACTER*80 BUF

      INTEGER L
      INTEGER CHR_LEN
      CHARACTER*20 BUFF

*  SAVE variables re-used in multiple calls

      SAVE ERRORS,LINE,PEN,CSIZE

      IF (STATUS .EQ. SAI__OK) THEN

         IF (FIRST) THEN

*  Get the plot label

             CALL PAR_GET0C('LABEL',LABEL,STATUS)

*  Get LINE parameter

             CALL PAR_GET0L('LINE',LINE,STATUS)

*  Get PEN parameter

             CALL PAR_GET0I('PEN',PEN,STATUS)

*  Get Size parameter

             CALL PAR_GET0R('SIZE',CSIZE,STATUS)

*  Get ERRORS parameter

             CALL PAR_GET0L('ERRORS',ERRORS,STATUS)

*  Copy original phase array

         ENDIF

*  Set X axis label

         IF (XLABEL .NE. 'Phase') THEN

*  Special handling if X label is not phase. This is used to identify
*  the case where we are plotting against time rather than phase, since
*  this routine is used by TSPLOT as well as by PHASEPLOT

*  Determine zero point (integral part of MJD)

             JDZERO = INT(X(1))
             DO I=1,FSIZE
                XR2(I) = REAL(X(I) - DBLE(JDZERO))
             ENDDO

*  Copy binned time array

             DO I=1,SIZE
                 XR(I) = REAL(XD(I) - DBLE(JDZERO))
             ENDDO

*  Set up X label

             WRITE(BUFF,'('' - '',I5,''$'')') JDZERO
             L = CHR_LEN(XLABEL)
             XLABEL(L+1:L+10) = BUFF
             ZEROT = XR(1)
             PHSTART = XR2(1)
             PHEND = XR2(FSIZE)
         ELSE
             DO I=1,FSIZE
                XR2(I) = REAL(X(I))
             ENDDO

*   Copy binned phase array

             DO I=1,SIZE
                 XR(I) = REAL(XD(I))
             ENDDO

         ENDIF


*  If output is to a file - open the file

         IF (FILE) THEN
             CALL FIO_ASSOC('FILENAME','WRITE','LIST',0,FD,STATUS)
         ENDIF

*  Position plot

         CALL AGSETF('GRID/TOP.',TOP)
         CALL AGSETF('GRID/BOTTOM.',BOT)

         IF (LAST) THEN

*  output the X-axis label

             CALL AGSETC('LABEL/NAME.','B')
             CALL AGSETI('LINE/NUMBER.',-100)
             CALL AGSETC('LINE/TEXT.',XLABEL)

*  Set character size depending on number of plots

             IF (FIRST .AND. LAST) THEN
                 CALL AGSETF('LINE/CHAR.',0.030*CSIZE)
                 CALL AGSETF('B/WI.',0.030*CSIZE)
             ELSE
                 CALL AGSETF('LINE/CHAR.',0.040*CSIZE)
                 CALL AGSETF('B/WI.',0.040*CSIZE)
             ENDIF
             CALL AGSETF('B/MA/CO.',2.0)
             CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',3.0)

         ELSE

*  Blank X-axis label

             CALL AGSETC('LABEL/NAME.','B')
             CALL AGSETI('LINE/NUMBER.',-100)
             CALL AGSETC('LINE/TEXT.',' $')
             CALL AGSETF('AXIS/BOTTOM/NUMERIC/TYPE.',0.0)
         ENDIF
         IF (FIRST) THEN

*  Top label - Only output it if it is the top plot

             CALL AGSETC('LABEL/NAME.','T')
             CALL AGSETF('LABEL/BASEPOINT/X.',0.05)
             CALL AGSETF('LABEL/CENTERING.',-1.0)
             CALL AGSETI('LINE/NUMBER.',100)
             CALL AGSETC('LINE/TEXT.',LABEL//'$')
             CALL AGSETF('LINE/CHAR.',0.040*CSIZE)
             CALL AGSETF('TOP/MAJOR/CO.',2.0)
             CALL AGSETF('AXIS/TOP/NUMERIC/TYPE.',0.0)
             CALL AGSETC('LABEL/NAME.','M')
             CALL AGSETF('LABEL/BASEPOINT/X.',0.05)
             CALL AGSETF('LABEL/BASEPOINT/Y.',1.0)
             CALL AGSETF('LABEL/OFFSET/Y.',-0.05)
             CALL AGSETF('LABEL/CENTERING.',-1.0)

*  Blank label

         ELSE
             CALL AGSETC('LABEL/NAME.','T')
             CALL AGSETI('LINE/NUMBER.',100)
             CALL AGSETC('LINE/TEXT.',' $')
             CALL AGSETF('AXIS/TOP/NUMERIC/TYPE.',0.0)
         ENDIF

*  Output the plot label - one for each plot

         CALL AGSETC('LABEL/NAME.','M')
         CALL AGSETI('LINE/NUMBER.',-100)
         TSTRING = PLABEL//'$'
         CALL AGSETC('LINE/TEXT.',TSTRING)
         IF (FIRST .AND. LAST) THEN
             CALL AGSETF('LINE/CHAR.',0.030*CSIZE)
         ELSE
             CALL AGSETF('LINE/CHAR.',0.040*CSIZE)
         ENDIF
         CALL AGSETF('LINE/CHAR.',0.040*CSIZE)

*  Y-axis label

         CALL AGSETC('LABEL/NAME.','L')
         CALL AGSETI('LINE/NUMBER.',100)
         IF (FIRST .AND. LAST) THEN
             CALL AGSETF('LINE/CHAR.',0.030*CSIZE)
         ELSE
             CALL AGSETF('LINE/CHAR.',0.040*CSIZE)
         ENDIF
         TSTRING = YLABEL//'$'
         CALL AGSETC('LINE/TEXT.',TSTRING)

*  Character size and spacing

         CALL AGSETF('L/WI.',0.040*CSIZE)
         CALL AGSETF('L/MA/CO.',2.0)
         CALL AGSETF('RIGHT/MA/CO.',2.0)
         CALL AGSETF('X/NICE.',0.0)
         CALL AGSETF('Y/NICE.',0.0)

*  Set scaling levels

         CALL AGSETF('Y/MIN.',IMIN)
         CALL AGSETF('Y/MAX.',IMAX)

*  Invert the direction if we are plotting magnitudes

         IF (ITEM .EQ. 'MAG') THEN
            CALL AGSETF('Y/ORDER.',1.0)
         ELSE
            CALL AGSETF('Y/ORDER.',0.0)
         ENDIF

*  Set up a dashed line pattern (used for zero polarization lines)

         CALL AGSETI('DASH/PATTERNS/2.',61680)

*  Set up phase range

         AX(1)=PHSTART
         AX(2)=PHEND

*  Draw the axes

         CALL AGSTUP(AX,1,1,2,1,Y,1,1,SIZE,1)
         CALL AGBACK

*  Draw the zero line

         CALL SGS_ICURZ(IZA)
         AX(1)=-1.0
         AX(2)=2.0
         AY(1)=0.0
         AY(2)=0.0
         CALL AGCURV(AX,1,AY,1,2,2)

*  Set SGS Zone to correspond to Autograph User Coordinates

         CALL SNX_AGCS

*  Select Window

         J=SAI__OK
         CALL SGS_ZONE(0.0,1.0,0.0,1.0,IZ,J)
         CALL SGS_SW(0.0,1.0,0.0,1.0,J)

*  Set the pen

         CALL SGS_SPEN(PEN)

*  Loop over points

         DO I=1,SIZE
          IF (Y(I) .NE. VAL__BADR) THEN
            IF (FILE) THEN

*  If FILE option set write the data to the file

                IF (ERRORS) THEN
                    WRITE(BUF,'(3G20.8)') XR(I),Y(I),YE(I)
                ELSE
                    WRITE(BUF,'(2G20.8)') XR(I),Y(I)
                ENDIF
                CALL FIO_WRITE(FD,BUF,STATUS)
            ENDIF

*  Calculate X and Y coordinates of data point...

            GX=SNX_AGUGX(XR(I))
            GY=SNX_AGUGY(Y(I))
            IF (ERRORS) THEN

*  ... and of ends of error bars if required

                GY1=SNX_AGUGY(Y(I)+YE(I))
                GY2=SNX_AGUGY(Y(I)-YE(I))

*  Draw the error bar

                CALL SGS_LINE(GX-.0030,GY,GX+0.0030,GY)
                CALL SGS_LINE(GX,GY1,GX,GY2)
            ENDIF

*  Draw the point (as a small polygon)

            CALL SGS_BPOLY(GX+.002,GY)
            CALL SGS_APOLY(GX+.0014,GY+.0020)
            CALL SGS_APOLY(GX,GY+.003)
            CALL SGS_APOLY(GX-.0014,GY+.0020)
            CALL SGS_APOLY(GX-.002,GY)
            CALL SGS_APOLY(GX-.0014,GY-.0020)
            CALL SGS_APOLY(GX,GY-.003)
            CALL SGS_APOLY(GX+.0014,GY-.0020)
            CALL SGS_APOLY(GX+.002,GY)

*  Output the polygon
            CALL SGS_OPOLY
          ENDIF
         ENDDO

*  draw a connecting line if required

         IF (LINE) THEN

*  Position at start of line

            IF (Y(1) .EQ. VAL__BADR) THEN
                GX=SNX_AGUGX(XR2(1))
            ELSE
                GX=SNX_AGUGX(XR(1))
            ENDIF
            GY=SNX_AGUGY(Y(1))
            CALL SGS_BPOLY(GX,GY)

*  Loop over remaining points

            DO I=2,SIZE
              IF (Y(I) .NE. VAL__BADR) THEN

*  Draw line to next point

                GX=SNX_AGUGX(XR(I))
                GY=SNX_AGUGY(Y(I))
                CALL SGS_APOLY(GX,GY)
              ENDIF
            ENDDO

*  Output the line
            CALL SGS_OPOLY
         ENDIF

*  Restore pen
         CALL SGS_SPEN(1)

*  Flush buffers
         CALL SGS_FLUSH
         CALL SGS_SELZ(IZA,J)

      ENDIF

*  Close the FILE if the FILE option was set

      IF (FILE) THEN
          CALL FIO_CANCL('FILENAME',STATUS)
      ENDIF
      END
