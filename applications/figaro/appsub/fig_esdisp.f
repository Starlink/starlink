C+
C                           F I G _ E S D I S P
C
C  Routine name:
C     FIG_ESDISP
C
C  Function:
C     Main display routine for ECHSELECT
C
C  Description:
C     This routine is the interactive display routine that displays
C     a subset of the spectrum obtained by collapsing the corrected
C     echellogram in the spectral direction.  It then allows the user
C     to indicate which cross-sections should be used for the sky
C     and object for the various orders.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL FIG_ESDISP (SPECT,XDATA,NY,PREV,NELM,MSTART,MDELTA,
C                                            MINORD,MAXORD,SKY,ORDERS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) SPECT        (Real array,ref) The summed cross-sections through
C                      the echellogram.
C     (>) XDATA        (Real array,ref) The axis array associated with
C                      the summed cross-section data.
C     (>) NELM         (Integer,ref) The number of elements in SPECT and
C                      XDATA.
C     (>) PREV         (Logical, ref) True if ORDERS already contains
C                      previous selections that are to be modified.
C     (>) MSTART       (Integer,ref) The first order number - should be
C                      the order number for the bottom order in the image,
C                      but it isn't critical since the user can change it.
C     (>) MDELTA       (Integer,ref) The increment value in order number
C                      as you move from left to right in the spectrum -
C                      should be 1 or -1 (if it's wrong, the '<' and '>'
C                      commands work backwards, but that's all.
C     (<) MINORD       (Integer,ref) The lowest order number selected.
C     (<) MAXORD       (Integer,ref) The highest order number selected.
C     (<) SKY          (Logical,ref) True if there were sky orders
C                      selected.
C     (!) ORDERS       (Integer array,ref) Array used to record selections.
C                      Each element corresponds to a cross-section of the
C                      image, and 0 => unused, m => used as object for order
C                      m, and -m => used as sky for order m.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     GKD_WRITE_LINE, GEN_BSEARCH,
C     FIG_HELP, FIG_ECS_TABLE, ICH_FOLD, ICH_LEN, ICH_CI, PGASK,
C     PGADVANCE, PGENV, PGBIN, PGSCI, PGSLS,PGMOVE, PGDRAW, PGCURSE
C
C  Prior requirements:
C     Called as part of the Figaro application ECHSELECT
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 15th Feb 1989
C-
C  Subroutine / function details:
C
C  History:
C     15th Feb 1989  Original version.  KS / AAO.
C     26th Jul 1993  HME / UoE, Starlink.  Disuse GKD_* except
C                    GKD_WRITE_LINE. Disuse PAR_Q*, use PAR_ABORT.
C+
      SUBROUTINE FIG_ESDISP (SPECT,XDATA,NELM,PREV,MSTART,MDELTA,
     :                                        MINORD,MAXORD,SKY,ORDERS)
C
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL PREV,SKY
      INTEGER NELM,MSTART,MDELTA,MINORD,MAXORD,ORDERS(NELM)
      REAL SPECT(NELM),XDATA(NELM)
C
C     Functions used
C
      LOGICAL   PAR_ABORT
      INTEGER   GEN_BSEARCH, ICH_FOLD, ICH_LEN
      CHARACTER ICH_CI*8
C
C     Local variables
C
      LOGICAL   AUTO            ! Indicates data is to be autoscaled
      LOGICAL   CARRYON         ! Controls main loop through spectrum
      INTEGER   CEN             ! Last cross-section in selected range
      CHARACTER CHR             ! Character hit by user
      INTEGER   CODE            ! Status value from FIG_COUNT_ORD
      INTEGER   CST             ! First cross-section in selected range
      LOGICAL   GOTCHR          ! True if char entered instead of 'return'
      REAL      HIGH            ! High value for display scale
      REAL      HWAS            ! Previous value of HIGH
      INTEGER   I               ! Loop index
      INTEGER   ICHEN           ! Last cross-section in object or sky range
      INTEGER   ICHST           ! First cross-section in object or sky range
      INTEGER   IEND            ! Last element to display
      INTEGER   IGNORE          ! Used for don't care returned status
      INTEGER   INVOKE          ! Dummy function value
      INTEGER   IST             ! Fisrst element to display
      INTEGER   IX              ! Element number selected by user
      INTEGER   LENGTH          ! Number of elements to display at once
      LOGICAL   LINES           ! True if lines used to delimit selections
      INTEGER   LOBJ            ! Number of characters in SKYOBJ
      LOGICAL   LOOP            ! Controls cursor loop with these display limits
      REAL      LOW             ! Low value for display scale
      REAL      LWAS            ! Previous value of LOW
      INTEGER   MORDER          ! Current order number
      INTEGER   NEXT            ! Next character to use in STRING
      LOGICAL   OBJSEL          ! Indicates object already selected for order
      INTEGER   ORDVAL          ! Value to put into ORDERS (MORDER or -MORDER)
      LOGICAL   OK              ! Indicates range is OK to use
      LOGICAL   QUIT            ! Indicates user wants to quit
      LOGICAL   REPLY           ! Reply from would-be PAR_QUEST
      CHARACTER SKYOBJ*8        ! Either 'sky ' or 'object '
      LOGICAL   SKYSEL          ! Indicates sky already selected for order
      CHARACTER STRING*64       ! Used for formatting messages
      REAL      VALUE           ! Real value entered by user
      REAL      VMAX            ! Max data value in displayed range
      REAL      VMIN            ! Min data value in displayed range
      REAL      X               ! Plotted X-value indicated by cursor
      REAL      XEND            ! Last axis value displayed
      REAL      XSTART          ! First axis value displayed
      REAL      XVAL            ! Delimiting value for selection
      REAL      Y               ! Plotted Y-value indicated by cursor
C
C     PGPLOT colour index values and line style codes
C
      INTEGER  ERASE,RED,BLUE,WHITE,DASHED,FULL
      PARAMETER (ERASE=0,WHITE=1,RED=2,BLUE=4,DASHED=2,FULL=1)
C
C     Real number limits for scales
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Clear the orders array (unless it contains previous selections)
C     and set the limits to the left end of the spectrum.  Set all
C     other initial values.
C
      IF (.NOT.PREV) THEN
         DO I=1,NELM
            ORDERS(I)=0
         END DO
      END IF
      LINES=.FALSE.
      MORDER=MSTART
      OBJSEL=.FALSE.
      SKYSEL=.FALSE.
      DO I=1,NELM
         IF (ORDERS(I).EQ.-MORDER) SKYSEL=.TRUE.
         IF (ORDERS(I).EQ.MORDER) OBJSEL=.TRUE.
      END DO
      IF (OBJSEL) CALL GKD_WRITE_LINE('Object already selected')
      IF (SKYSEL) CALL GKD_WRITE_LINE('Some sky already selected')
      LENGTH=MIN(100,NELM)
      IST=1
      IEND=LENGTH
      AUTO=.TRUE.
      GOTCHR=.FALSE.
      CALL PGASK(.FALSE.)
C
C     This loop continues until the user selects the Q(uit) option.
C
      CARRYON=.TRUE.
      DO WHILE (CARRYON)
C
C        Display data between elements IST and IEND
C
         XSTART=XDATA(IST)
         XEND=XDATA(IEND)
         VMAX=SPECT(IST)
         VMIN=SPECT(IST)
         DO I=IST+1,IEND
            VMAX=MAX(VMAX,SPECT(I))
            VMIN=MIN(VMIN,SPECT(I))
         END DO
         IF (VMAX.EQ.VMIN) THEN
            IF (VMAX.EQ.0.0) THEN
               VMAX=1.0
            ELSE
               VMAX=VMAX*1.1
            END IF
         END IF
         IF (AUTO) THEN
            HIGH=VMAX+(VMAX-VMIN)*.1
            LOW=VMIN
         END IF
         CALL PGADVANCE
         CALL PGENV (XSTART,XEND,LOW,HIGH,.FALSE.,1)
         CALL PGBIN (IEND-IST+1,XDATA(IST),SPECT(IST),.TRUE.)
         I=IST
         DO WHILE (I.LE.IEND)
            IF (ORDERS(I).NE.0) THEN
               CST=I
               ORDVAL=ORDERS(I)
               DO WHILE ((I.LE.IEND).AND.(ORDERS(I).EQ.ORDVAL))
                  I=I+1
               END DO
               CEN=I-1
               CALL PGSCI(ERASE)
               CALL PGBIN (CEN-CST+1,XDATA(CST),SPECT(CST),.TRUE.)
               SKY=ORDVAL.LT.0
               IF (SKY) THEN
                  CALL PGSCI(BLUE)
               ELSE
                  CALL PGSCI(RED)
               END IF
               CALL PGBIN (CEN-CST+1,XDATA(CST),SPECT(CST),.TRUE.)
               CALL PGSCI(WHITE)
               IF (LINES) THEN
                  CALL PGSLS(DASHED)
                  IF (CST.GT.1) THEN
                     XVAL=(XDATA(CST)+XDATA(CST-1))*0.5
                  ELSE
                     XVAL=XDATA(CST)-(XDATA(CST+1)-XDATA(CST))*0.5
                  END IF
                  CALL PGMOVE(XVAL,LOW)
                  CALL PGDRAW(XVAL,HIGH)
                  IF (CEN.LT.NELM) THEN
                     XVAL=(XDATA(CEN)+XDATA(CEN+1))*0.5
                  ELSE
                     XVAL=XDATA(CEN)+(XDATA(CEN)-XDATA(CEN-1))*0.5
                  END IF
                  CALL PGMOVE(XVAL,LOW)
                  CALL PGDRAW(XVAL,HIGH)
                  CALL PGSLS(FULL)
               END IF
            ELSE
               I=I+1
            END IF
         END DO
         CALL GKD_WRITE_LINE (
     :            'O(bject),S(ky), > or < to change order,H(elp)')
         STRING='Current order is '//ICH_CI(MORDER)
         CALL GKD_WRITE_LINE(STRING(:ICH_LEN(STRING)))
C
C        Loop geting cursor position etc continues until user selects
C        a different data range.
C
         LOOP=.TRUE.
         DO WHILE (LOOP)
            IF (.NOT.GOTCHR) CALL PGCURSE (X,Y,CHR)
            GOTCHR=.FALSE.
            IX=GEN_BSEARCH(XDATA(IST),IEND-IST+1,X)+IST-1
            INVOKE=ICH_FOLD(CHR)
            IF (CHR.EQ.'N') THEN
C
C              'N' - go on to next screen's worth of data
C
               IF (IEND.LT.NELM) THEN
                  IST=IEND-LENGTH*0.25
                  IEND=IST+LENGTH-1
                  IF (IEND.GT.NELM) THEN
                     IEND=NELM
                     IST=IEND-LENGTH+1
                  END IF
                  LOOP=.FALSE.
               END IF
            ELSE IF (CHR.EQ.'B') THEN
C
C              'B' - go back to previous screen's worth of data
C
               IF (IST.GT.1) THEN
                  IEND=IST+LENGTH*0.25
                  IST=IEND-LENGTH+1
                  IF (IST.LT.1) THEN
                     IST=1
                     IEND=LENGTH
                  END IF
                  LOOP=.FALSE.
               END IF
            ELSE IF ((CHR.EQ.'O').OR.(CHR.EQ.'S')) THEN
C
C              'O' - indicate Object range
C              'S' - indicate Sky range
C
               SKY=CHR.EQ.'S'
               ICHST=IX
               OK=.TRUE.
               IF (ORDERS(ICHST).NE.0) THEN
                   CALL GKD_WRITE_LINE (
     :                              'Cross-section already selected')
                   OK=.FALSE.
               ELSE
                  IF (OBJSEL.AND.(.NOT.SKY)) THEN
                     CALL PAR_CNPAR('ADD')
                     CALL PAR_RDKEY('ADD',.FALSE.,OK)
                     IF (PAR_ABORT()) RETURN
                  END IF
               END IF
               IF (OK) THEN
                  IF (SKY) THEN
                     SKYOBJ='sky '
                     LOBJ=4
                     ORDVAL=-MORDER
                  ELSE
                     SKYOBJ='object '
                     LOBJ=7
                     ORDVAL=MORDER
                  END IF
                  CALL GKD_WRITE_LINE ('Indicate other end of '//
     :               SKYOBJ(:LOBJ)//'range with cursor and hit any key')
                  CALL PGCURSE (X,Y,CHR)
                  ICHEN=GEN_BSEARCH(XDATA(IST),IEND-IST+1,X)+IST-1
                  OK=.TRUE.
                  CST=MIN(ICHST,ICHEN)
                  CEN=MAX(ICHST,ICHEN)
                  DO I=CST,CEN
                     IF (ORDERS(I).NE.0) THEN
                        STRING='Cross-section '//ICH_CI(I)
                        NEXT=ICH_LEN(STRING)+1
                        STRING(NEXT:)=' already selected'
                        CALL GKD_WRITE_LINE (STRING(:NEXT+16))
                        OK=.FALSE.
                     END IF
                  END DO
                  IF (OK) THEN
                     DO I=CST,CEN
                        ORDERS(I)=ORDVAL
                     END DO
                     STRING='Cross-sections '//ICH_CI(CST)
                     NEXT=ICH_LEN(STRING)+1
                     STRING(NEXT:)=' to '//ICH_CI(CEN)
                     NEXT=ICH_LEN(STRING)+1
                     STRING(NEXT:)=' used as '//SKYOBJ(:LOBJ)//
     :                                  ' for order '//ICH_CI(MORDER)
                     CALL GKD_WRITE_LINE (STRING(:ICH_LEN(STRING)))
                     CALL PGSCI(ERASE)
                     CALL PGBIN (CEN-CST+1,XDATA(CST),SPECT(CST),
     :                                                          .TRUE.)
                     IF (SKY) THEN
                        SKYSEL=.TRUE.
                        CALL PGSCI(BLUE)
                     ELSE
                        OBJSEL=.TRUE.
                        CALL PGSCI(RED)
                     END IF
                     CALL PGBIN (CEN-CST+1,XDATA(CST),SPECT(CST),
     :                                                          .TRUE.)
                     CALL PGSCI(WHITE)
                     IF (LINES) THEN
                        CALL PGSLS(DASHED)
                        IF (CST.GT.1) THEN
                           XVAL=(XDATA(CST)+XDATA(CST-1))*0.5
                        ELSE
                           XVAL=XDATA(CST)-(XDATA(CST+1)-XDATA(CST))*0.5
                        END IF
                        CALL PGMOVE(XVAL,LOW)
                        CALL PGDRAW(XVAL,HIGH)
                        IF (CEN.LT.NELM) THEN
                           XVAL=(XDATA(CEN)+XDATA(CEN+1))*0.5
                        ELSE
                           XVAL=XDATA(CEN)+(XDATA(CEN)-XDATA(CEN-1))*0.5
                        END IF
                        CALL PGMOVE(XVAL,LOW)
                        CALL PGDRAW(XVAL,HIGH)
                        CALL PGSLS(FULL)
                     END IF
                  END IF
               END IF
            ELSE IF ((CHR.EQ.'L').OR.(CHR.EQ.'C').OR.(CHR.EQ.'M')) THEN
C
C              'C' - redisplay centered on current cursor X-position
C              'L' - like 'C', but with option to change display length
C              'M' - move to sepcified X-axis position
C
               IF (CHR.EQ.'L') THEN
                  CALL PAR_CNPAR('DISNCHAN')
                  CALL PAR_RDVAL('DISNCHAN',10.,FLOAT(NELM),
     :               FLOAT(LENGTH),'Pixels',VALUE)
                  IF (PAR_ABORT()) RETURN
                  LENGTH=VALUE
               END IF
               IF (CHR.EQ.'M') THEN
                  CALL PAR_CNPAR('MOVETOX')
                  CALL PAR_RDVAL('MOVETOX',XDATA(1),XDATA(NELM),
     :               XDATA(IX),' ',VALUE)
                  IF (PAR_ABORT()) RETURN
                  IX=GEN_BSEARCH(XDATA(1),NELM,VALUE)
               END IF
C
C              Note - 'L' and 'C' get IX from cursor, 'L' may have
C              changed LENGTH, 'M' has got IX from user.  Given IX and
C              LENGTH, calculate new limits for plots.
C
               IST=MAX(IX-LENGTH/2,1)
               IF (IST+LENGTH-1.GT.NELM) THEN
                  IEND=NELM
                  IST=MAX(1,IEND-LENGTH+1)
               ELSE
                  IEND=IST+LENGTH-1
               END IF
               LOOP=.FALSE.
            ELSE IF ((CHR.EQ.'>').OR.(CHR.EQ.'<')) THEN
C
C              '>','<' move to new m-value, ie to new order.
C              '>' implies move to right, ie add MDELTA to current
C              order number, '<' implies to left.
C
               IF (CHR.EQ.'>') THEN
                  MORDER=MORDER+MDELTA
               ELSE
                  MORDER=MORDER-MDELTA
               END IF
               CALL PAR_CNPAR('ORDER')
               CALL PAR_RDVAL('ORDER',0.,1000.,FLOAT(MORDER),' ',VALUE)
               IF (PAR_ABORT()) RETURN
               MORDER=VALUE
               STRING='Order number now '//ICH_CI(MORDER)
               CALL GKD_WRITE_LINE(STRING(:ICH_LEN(STRING)))
               OBJSEL=.FALSE.
               SKYSEL=.FALSE.
               DO I=1,NELM
                  IF (ORDERS(I).EQ.-MORDER) SKYSEL=.TRUE.
                  IF (ORDERS(I).EQ.MORDER) OBJSEL=.TRUE.
               END DO
               IF (OBJSEL) THEN
                  CALL GKD_WRITE_LINE('Object already selected')
               END IF
               IF (SKYSEL) THEN
                  CALL GKD_WRITE_LINE('Some sky already selected')
               END IF
            ELSE IF (CHR.EQ.'U') THEN
C
C              'U' clear all settings for this order
C
               CALL PAR_CNPAR('CLEAR')
               CALL PAR_RDKEY('CLEAR',.TRUE.,REPLY)
               IF (PAR_ABORT()) RETURN
               IF (REPLY) THEN
                  DO I=1,NELM
                     IF ((ORDERS(I).EQ.MORDER).OR.
     :                              (ORDERS(I).EQ.-MORDER)) THEN
                        ORDERS(I)=0
                     END IF
                  END DO
                  CALL GKD_WRITE_LINE('Selections cleared')
                  OBJSEL=.FALSE.
                  SKYSEL=.FALSE.
                  LOOP=.FALSE.
               END IF
            ELSE IF (CHR.EQ.'R') THEN
C
C              'R' - Redraw
C
               LOOP=.FALSE.
            ELSE IF (CHR.EQ.'P') THEN
C
C              'P' - indicate Position
C
               STRING='Cross-section '//ICH_CI(IX)
               NEXT=ICH_LEN(STRING)+1
               IF (ORDERS(IX).EQ.0) THEN
                  STRING(NEXT:)=' not yet selected'
               ELSE
                  IF (ORDERS(IX).GT.0) THEN
                     STRING(NEXT:)=' used as object'
                     NEXT=NEXT+15
                  ELSE
                     STRING(NEXT:)=' used as sky'
                     NEXT=NEXT+12
                  END IF
                  STRING(NEXT:)=' for order '//ICH_CI(ABS(ORDERS(IX)))
               END IF
               CALL GKD_WRITE_LINE(STRING(:ICH_LEN(STRING)))
            ELSE IF (CHR.EQ.'D') THEN
C
C              'D' - delimit selections using vertical lines (useful
C              when device doesn't have colour.
C
               LINES=.TRUE.
               LOOP=.FALSE.
            ELSE IF ((CHR.EQ.'H').OR.(CHR.EQ.'?')) THEN
C
C              '?' or 'H' - Output help information
C
               CALL FIG_HELP('echselect',IGNORE)
               GOTCHR=.FALSE.
               CHR=STRING(1:1)
               LOOP=.FALSE.
            ELSE IF (CHR.EQ.'T') THEN
C
C              'T' Table results so far
C
               CALL FIG_ECS_TABLE (ORDERS,NELM)
               GOTCHR=.FALSE.
               CHR=STRING(1:1)
               LOOP=.FALSE.
            ELSE IF (CHR.EQ.'A') THEN
C
C              'A' revert to autoscaling
C
               AUTO=.TRUE.
               LOOP=.FALSE.
            ELSE IF (CHR.EQ.'Y') THEN
C
C              'Y' - set Y-scale limits
C
               AUTO=.FALSE.
               LWAS=LOW
               HWAS=HIGH
               CALL PAR_CNPAR('LOW')
               CALL PAR_RDVAL('LOW',FMIN*0.5,FMAX*0.5,LOW,' ',VALUE)
               IF (PAR_ABORT()) RETURN
               LOW=VALUE
               CALL PAR_CNPAR('HIGH')
               CALL PAR_RDVAL('HIGH',LOW,FMAX*0.5,HIGH,' ',VALUE)
               IF (PAR_ABORT()) RETURN
               HIGH=VALUE
               LOOP=(HIGH.EQ.HWAS).AND.(LOW.EQ.LWAS)
               IF (LOW.EQ.HIGH) HIGH=HIGH+(VMAX-VMIN)*.1
            ELSE IF (CHR.EQ.'Q') THEN
C
C              'Q' - Quit selection
C
               CALL FIG_COUNT_ORD (ORDERS,NELM,MDELTA,MINORD,
     :                                               MAXORD,SKY,CODE)
               CALL PAR_CNPAR('QUITSEL')
               CALL PAR_RDKEY('QUITSEL',(CODE.EQ.0),QUIT)
               IF (PAR_ABORT()) RETURN
               IF (QUIT) THEN
                  CARRYON=.FALSE.
                  LOOP=.FALSE.
               END IF
            END IF
         END DO
      END DO
C
      END
