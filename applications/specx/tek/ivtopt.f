*  History:
*     19 Nov 1993 (hme):
*        Disuse the OUTERR_HANDLER error handler.
*        Replace STR$UPCASE with CHR_UCASE.
*     25 Nov 1993 (hme):
*        Attempt to disuse IERASE_LINE, IERASE_PAGE, IPUT_SCREEN,
*        ISET_CURSOR.
*     29 Nov 1993 (hme):
*        The WRITE(CHAR,'(A1)') seems to upset things, try CHR.
*        That looks better, i.e. dbx is able to look at CHR, but the
*        internal WRITE still doesn't work. Try the CHAR function.
*     06 Jan 1994 (rp):
*        Put back calls to nnn_SCREEN etc (which are now dummies)
*     15 Jan 1994 (rp):
*        Change CHR_UCASE to UUCASE
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused CHKBOX
C-----------------------------------------------------------------------

      INTEGER FUNCTION IVTOPT(IDEV, VALOPT, P, Q, REPEAT)

C   General routine to perform interactive graphics options on
C   NCAR type plot

      PARAMETER       (NOPTS=15)

      LOGICAL         REPEAT,FIRST_BOX
      INTEGER*4       VCHAR
      REAL*4          P(2),Q(2),Q2(2)
      CHARACTER       TXTLIN*32,CHR*1,VALOPT*(*),ICH2*2

      INCLUDE 'PLOTPAR1'

      CHARACTER       LEFT,RIGHT,CR,TOP,BOTTOM,HELP,END,CLEAR,
     &                NEWLIM,ACCEPT,DSPLBOX,QUIT,PLUS,QUERY,LIMITS
      COMMON /CURSOR/ LEFT,RIGHT,CR,TOP,BOTTOM,HELP,END,CLEAR,
     &                NEWLIM,ACCEPT,DSPLBOX,QUIT,PLUS,QUERY,LIMITS

      BYTE            ZCHAR(NOPTS)
      EQUIVALENCE     (LEFT,ZCHAR(1))

      IVTOPT    = 0
      REPEAT    = .FALSE.
      FIRST_BOX = .TRUE.
      P(1)= XST1
      P(2)= XEND1
      Q(1)= YST1
      Q(2)= YEND1

C  Initialize characters (DATA statement doesn't work since in common /CURSOR/

      ZCHAR(1) = 076  ! LEFT
      ZCHAR(2) = 082  ! RIGHT
      ZCHAR(3) = 013  ! <CR>
      ZCHAR(4) = 084  ! TOP
      ZCHAR(5) = 066  ! BOTTOM
      ZCHAR(6) = 072  ! HELP
      ZCHAR(7) = 069  ! END
      ZCHAR(8) = 067  ! CLEAR
      ZCHAR(9) = 078  ! NEWLIM
      ZCHAR(10)= 065  ! ACCEPT
      ZCHAR(11)= 068  ! DSPLBOX
      ZCHAR(12)= 081  ! QUIT
      ZCHAR(13)= 043  ! PLUS
      ZCHAR(14)= 063  ! QUERY
      ZCHAR(15)= 083  ! LIMITS

      CALL SXGVWINDOW   (25., 25.+XLEN1, 50., 50.+YLEN1)
      CALL SXGDEVINFO   (XSIZED, YSIZED)
      CALL SXGVRELOCATE (0.0,    YSIZED)

      DO WHILE(IVTOPT.EQ.0)

C  Read cursor position and write character to screen
       CALL SXGCURSOR (XPOS,YPOS,VCHAR)
       CHR = CHAR(VCHAR)
       CALL CONFIRM(CHR,ICH2)
       CALL IERASE_LINE(1,1)
       CALL IPUT_SCREEN('GIN character '//ICH2,1,61,2)
*      PRINT *,'GIN character '//ICH2

C  Decode X-position and Y-position to plot co-ordinates and confirm

*      CALL SXGTIDLE
*      Print *,'X and Y values (mm) ',XPOS,YPOS
*      Print *,'XST1, XEND1         ',XST1,XEND1
*      Print *,'YST1, YEND1         ',YST1,YEND1
*      Print *,'XLEN1,YLEN1         ',XLEN1,YLEN1
*      CALL SXGTTGRAPH

       X=XST1+(XPOS-25.)*(XEND1-XST1)/XLEN1
       Y=YST1+(YPOS-50.)*(YEND1-YST1)/YLEN1
       WRITE (TXTLIN,'(''X=''F10.4,1X,''Y=''F10.3)', IOSTAT=IERR) X, Y
       CALL IPUT_SCREEN(TXTLIN(:25),2,55,0)
       CALL ISET_CURSOR(23,1)
*      PRINT *,TXTLIN(:25)

C  Convert lower case letters to upper case
       CALL UUCASE(CHR)
       CALL UUCASE(ICH2)

C  Then decode options
       IF (INDEX(VALOPT,ICH2).EQ.0) THEN
         CALL IPUT_SCREEN('Invalid character '//ICH2,1,1,2)
*        PRINT *,'Invalid character '//ICH2

       ELSE IF (CHR.EQ.END) THEN
         IVTOPT=2
         CALL SXGCLEAR
         CALL SXGTIDLE

       ELSE IF (CHR.EQ.QUIT) THEN
         IVTOPT=2
         CALL SXGTIDLE

       ELSE IF (CHR.EQ.CR) THEN
         IVTOPT=1

       ELSE IF (CHR.EQ.ACCEPT) THEN
*        Make up some useful limits for the boxes
         IF (Q(1)*Q(2) .LE. 0.0) THEN
           Q2(1) = + 0.125*(Q(1)-Q(2))
           Q2(2) = - 0.125*(Q(1)-Q(2))
         ELSE
           Q2(1) = Q(1) + 0.2*(Q(2)-Q(1))
           Q2(2) = Q(1) + 0.8*(Q(2)-Q(1))
         END IF
*        And then mark the box
         CALL DRAW_BOX (FIRST_BOX, P, Q2)
         FIRST_BOX = .TRUE.
         IVTOPT    = 3

       ELSE IF(CHR.EQ.LEFT) THEN
         P(1)=X
       ELSE IF(CHR.EQ.RIGHT) THEN
         P(2)=X
       ELSE IF (CHR.EQ.BOTTOM) THEN
         Q(1)=Y
       ELSE IF (CHR.EQ.TOP) THEN
         Q(2)=Y

       ELSE IF (CHR.EQ.CLEAR) THEN
         CALL IERASE_PAGE(1,1)

       ELSE IF (CHR.EQ.HELP) THEN
         CALL LIST_VTHELP(VALOPT)

       ELSE IF (CHR.EQ.NEWLIM) THEN
         CALL NEW_XY (P, Q, .TRUE.)
         CALL SXGCLEAR
         REPEAT=.TRUE.
         IVTOPT=4

       ELSE IF (CHR.EQ.LIMITS) THEN
         CALL SXGTIDLE
         CALL GEN_GETR4A('X-axis scale: Beginning and end?', P, 2,
     &                   '2(F7.2,1X)', P, ISTAT)
         CALL GEN_GETR4A('Y-axis scale: Beginning and end?', Q, 2,
     &                   '2(F7.2,1X)', Q, ISTAT)
         CALL NEW_XY (P, Q, .FALSE.)
         CALL SXGTTGRAPH
         REPEAT = .TRUE.
         IVTOPT = 4

       ELSE IF (CHR.EQ.DSPLBOX)  THEN
         CALL DRAW_BOX (FIRST_BOX, P, Q)
         FIRST_BOX = .FALSE.

       ELSE IF (CHR.EQ.PLUS)   THEN
         P(2)=X
         Q(2)=Y
         IVTOPT=5
       END IF

      END DO

      RETURN
      END
