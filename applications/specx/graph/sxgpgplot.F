* History:
*    25-JUL-2000 (AJC)
*       Handle DEVICES structure in C
*       Replace TYPE * with PRINT *
*       Re-write illegal concatenation
*       Remove CARRIAGECONTROL from OPEN
*       Use format I3 to read type size
*       Use format L1 to read logical elements
*       Unused in SXGDEVICE: ISTAT, TT_ERROR
*              in SXGCURSOR: ISTAT, IOSTAT, XSIZE, YSIZE
*              in SXGXLABEL: GEN_ILEN
*              in SXGXLABEL2: GEN_ILEN
*              in SXGYLABEL: GEN_ILEN
*              in SXGCONTOUR: ITEMP
*              in SXGCONB: ITEMP
*              in SXGPLOTID: NTICKS
*       Don't write "hardcopy file closed" if it isn't
*    05 Mar 2002 (TIMJ)
*       SAVE cursor position in SXGCURSOR
*-----------------------------------------------------------------------
#if HAVE_CONFIG_H
#  include "config.h"
#endif

      SUBROUTINE SXGINIT

*  Routine to read list of graphics devices in GRAPHCAP.TXT and
*  load device array. Also initializes other variables

      IMPLICIT  NONE

*     Includes

      INCLUDE  'SXG_GRAPHCAP.INC'
      INCLUDE  'SXGPGPLOT.INC'

*     Local variables

      INTEGER   IST, IFIN
      INTEGER   ICHAR
      INTEGER   ID
      INTEGER   IERR
      INTEGER   LL1, LL2, LL
      INTEGER   LUN
      INTEGER   IOSTAT
      INTEGER   LENSG
      INTEGER   STATUS

      CHARACTER LINE*80
      CHARACTER GRAPHCAP_FILE*80
      CHARACTER FORMAT*8

*  Values read from file
      INTEGER   FDEV_NO
      LOGICAL   FTERM
      LOGICAL   FDUAL
      LOGICAL   FCOLOUR
      LOGICAL   FHARD
      CHARACTER FPROMPT*16
      CHARACTER FDEVNAME*16
      CHARACTER FFILE*80

*     Functions

      INTEGER   GEN_ILEN

*  Ok, go...

      PRINT *, 'Graphics initialized to use PGPLOT'

*     Translate graphcap file name

      CALL UTRNLOG ('SXG_GRAPHCAP', GRAPHCAP_FILE, STATUS)
      IF (STATUS.ne.0) THEN
        PRINT *, '--- sxginit ---'
        PRINT *, '    No translation for logical name "SXG_GRAPHCAP"!'
        RETURN
      ELSE
        LENSG = GEN_ILEN (GRAPHCAP_FILE)
      END IF

*     Open the graphcap file

      CALL UGETLUN (LUN, IOSTAT)
      OPEN (LUN, FILE=GRAPHCAP_FILE(:LENSG), STATUS='OLD',
     &      FORM = 'FORMATTED',  IOSTAT=IOSTAT)
      IF (IOSTAT.NE.0) THEN
        PRINT *, 'Error reading GRAPHCAP'
        GO TO 99
      END IF

*     Read the entries

      ID = 0

      DO WHILE (.TRUE.)

        READ (LUN, '(A)', END=10, IOSTAT=IOSTAT) LINE
        if ( iostat .ne. 0 ) goto 10
*       Check not a comment line

        IF (LINE(1:1).NE.'#' .and. LINE(1:1).NE.'!'
     &                       .and. LINE(1:1).NE.'*') THEN

*         Check for in-line comments

          LL  = GEN_ILEN (LINE)
          LL1 = INDEX (LINE, '!')
          LL2 = INDEX (LINE, '#')
          IF (LL1.NE.0) LL = LL1
          IF (LL2.NE.0) LL = MIN (LL, LL2)

          ID    = ID + 1
          ICHAR = 1

          CALL GEN_GETIT4 (LINE, 1, IST, IFIN, ICHAR, IERR)
* Construct FORMAT for reading symbol device number
          WRITE( FORMAT, '(''(I'', I3, '')'')' ) IFIN-IST+1
* and read it
          READ (LINE(IST:IFIN),FORMAT,IOSTAT=IOSTAT) FDEV_NO
CD        PRINT *, 'Device # ', FDEV_NO, IOSTAT

          CALL GEN_GETIT4 (LINE, 1, IST, IFIN, ICHAR, IERR)
          FPROMPT = LINE(IST:IFIN)
CD        PRINT *, 'Specx name', FPROMPT, IOSTAT

          CALL GEN_GETIT4 (LINE, 1, IST, IFIN, ICHAR, IERR)
          FDEVNAME = LINE(IST:IFIN)
CD        PRINT *, 'Device name', FDEVNAME, IOSTAT

          CALL GEN_GETIT4 (LINE, 1, IST, IFIN, ICHAR, IERR)
          READ (LINE(IST:IFIN),'(L1)',IOSTAT=IOSTAT) FTERM
CD        PRINT *, 'Terminal? ', FTERM, IOSTAT

          CALL GEN_GETIT4 (LINE, 1, IST, IFIN, ICHAR, IERR)
          READ (LINE(IST:IFIN),'(L1)',IOSTAT=IOSTAT) FDUAL
CD        PRINT *, 'Dual screen? ', FDUAL, IOSTAT

          CALL GEN_GETIT4 (LINE, 1, IST, IFIN, ICHAR, IERR)
          READ (LINE(IST:IFIN),'(L1)',IOSTAT=IOSTAT) FCOLOUR
CD        PRINT *, 'Colour?      ', FCOLOUR, IOSTAT

          CALL GEN_GETIT4 (LINE, 1, IST, IFIN, ICHAR, IERR)
          READ (LINE(IST:IFIN),'(L1)',IOSTAT=IOSTAT) FHARD
CD        PRINT *, 'Hardcopy? ', FHARD, IOSTAT

          CALL GEN_GETIT4 (LINE, 1, IST, IFIN, ICHAR, IERR)
          FFILE = ' '
          IF (IERR.EQ.0) THEN
            FFILE = LINE(IST:IFIN)
CD          PRINT *, 'Output file = ', FFILE(:IFIN-IST+1)
          END IF

          CALL SXG_WRDEV( ID, FDEV_NO, FDEVNAME, FPROMPT, FTERM, FDUAL,
     :       FCOLOUR, FHARD, FFILE, IERR )

        END IF

      END DO
   10 CONTINUE

      NDEVS = ID

*     Close the file

      CLOSE (LUN)
      CALL UFREELUN (LUN, IOSTAT)

*     Report status

      PRINT *, 'Successfully read GRAPHCAP: ', NDEVS, ' devices present'

*     Initialize other variables

      DEVINIT = .FALSE.

      GREYOK  = .TRUE.
      COLOROK = .TRUE.

      RETURN

*     Error return

   99 CONTINUE
      RETURN

      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGTERMDEV (TERMDEV, TTNN, IERR)

*  Routine to set up SPECX for new graphics terminal

      IMPLICIT   NONE

*     Formal parameters:

      CHARACTER  TERMDEV*(*)    ! (character, returned) terminal device name
      CHARACTER  TTNN*(*)       ! (character, returned) VMS terminal name
      INTEGER    IERR           ! (integer,   returned) error return:
                                !          0  = success
                                !         21  = unavailable device

*     Include files

      INCLUDE   'SXG_GRAPHCAP.INC'

*     Functions

      INTEGER    GEN_ILEN
      LOGICAL    SXG_INQTERM

*     Local variables

      INTEGER    COMMA
      INTEGER    DEVKNT
      INTEGER    IP
      INTEGER    IOK
      INTEGER    IDEV
      INTEGER    JDEV
      INTEGER    ISTAT
      INTEGER    LP
      INTEGER    LT
      CHARACTER*16 DPROMPT
      CHARACTER  PROMPT*512

*  Ok, go...
      IERR  = 0
      LT    = GEN_ILEN (TERMDEV)

*     Create a prompt from graphcap table

      IDEV   = 0
      IOK    = 0
      PROMPT = ''' '
      IP     = 3
      DEVKNT = 0

      DO IDEV = 1, NDEVS
        IF (SXG_INQTERM(IDEV)) THEN
          DEVKNT = DEVKNT + 1
          IOK    = IOK  + 1
          CALL SXG_GTPR( IDEV, DPROMPT, IERR )
          LP     = GEN_ILEN (DPROMPT)
          PROMPT(IP:IP+17) = DPROMPT(:LP) // ', '
          IP = IP + 18
          IF (IOK.EQ.4) THEN
            PROMPT (IP:IP+3) = '''/'' '
            IP  = IP + 4
            IOK = 0
          END IF
        END IF
      END DO

      IF (DEVKNT.GT.0) THEN
        IF (IOK.EQ.0) IP = IP - 4
        COMMA = IP - 2 - (16-LP)
        PROMPT(COMMA:COMMA) = ' '
      ELSE
        PRINT *, 'No terminals available!'
        GO TO 99
      END IF

      PROMPT =   '"''Terminals available: ''/'
     &         // PROMPT(:IP-1)  // ''''
     &         // '//'' Terminal type? ['  // TERMDEV(:LT) // '] ''"'

*     Request input

      CALL GEN_GETSTR (PROMPT, TERMDEV, ' ', TERMDEV, ISTAT)

*     Minimum match with graphcap

      IOK = 0
      LT  = GEN_ILEN (TERMDEV)
      DO IDEV = 1, NDEVS
        IF (SXG_INQTERM(IDEV)) THEN
          CALL SXG_GTPR( IDEV, DPROMPT, IERR )
          IF (LT .LE. GEN_ILEN(DPROMPT)) THEN
            IF (TERMDEV(:LT) .EQ. DPROMPT(:LT)) THEN
              IOK  = IOK + 1
              JDEV = IDEV
            END IF
          END IF
        END IF
      END DO

      IF (IOK.EQ.1) THEN
        CALL SXG_GTPR( JDEV, TERMDEV, IERR )
      ELSE IF (IOK.GT.1) THEN
        PRINT *, 'Ambiguous device!'
        GO TO 99
      ELSE
        PRINT *, 'No such device!'
        GO TO 99
      END IF

*     Also request device name (for if not default)

      LT = GEN_ILEN(TERMDEV)
      PROMPT = TERMDEV(:LT)
      PROMPT(LT+1:) =
     & ' device? (e.g TXA0) if not plotting on this terminal'
      CALL GEN_GETSTR  ( PROMPT, ' ', ' ', TTNN, ISTAT)

      RETURN

*     Error return

   99 CONTINUE
      RETURN

      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGPRINTDEV (PRINTDEV, PORTRAIT, IERR)

*  Routine to set up SPECX for new graphics hardcopy device

      IMPLICIT   NONE

*     Formal parameters:

      CHARACTER  PRINTDEV*(*)   ! (character, returned) hardcopy device name
      LOGICAL    PORTRAIT       ! (logical,   returned) true for portrait plots
      INTEGER    IERR           ! (integer,   returned) error return:
                                !          0  = success
                                !         21  = unavailable device

*     Include files

      INCLUDE   'SXG_GRAPHCAP.INC'

*     Functions

      INTEGER    GEN_ILEN
      LOGICAL    SXG_INQHARD

*     Local variables

      INTEGER    COMMA
      INTEGER    DEVKNT
      INTEGER    IP
      INTEGER    IOK
      INTEGER    IDEV
      INTEGER    JDEV
      INTEGER    ISTAT
      INTEGER    LP
      INTEGER    LT
      CHARACTER*16 DPROMPT
      CHARACTER  PROMPT*512

*  Ok, go...

      IERR  = 0
      LT    = GEN_ILEN (PRINTDEV)

*     Create a prompt from graphcap table

      IDEV   = 0
      IOK    = 0
      PROMPT = ''' '
      IP     = 3
      DEVKNT = 0

      DO IDEV = 1, NDEVS
        IF (SXG_INQHARD(IDEV)) THEN
          DEVKNT = DEVKNT + 1
          IOK    = IOK  + 1
          CALL SXG_GTPR( IDEV, DPROMPT, IERR )
          LP = GEN_ILEN (DPROMPT)
          PROMPT(IP:IP+17) = DPROMPT(:LP) // ', '
          IP = IP + 18
          IF (IOK.EQ.4) THEN
            PROMPT (IP:IP+3) = '''/'' '
            IP  = IP + 4
            IOK = 0
          END IF
        END IF
      END DO

      IF (DEVKNT.GT.0) THEN
        IF (IOK.EQ.0) IP = IP - 4
        COMMA = IP - 2 - (16-LP)
        PROMPT(COMMA:COMMA) = ' '
      ELSE
        PRINT *, 'No printers available!'
        GO TO 99
      END IF

      PROMPT =   '"''Printers available: ''/'
     &         // PROMPT(:IP-1)  // ''''
     &         // '//'' Printer type? ['  // PRINTDEV(:LT) // '] ''"'

*     Request input

      CALL GEN_GETSTR (PROMPT, PRINTDEV, ' ', PRINTDEV, ISTAT)

*     Minimum match with graphcap

      IOK = 0
      LT  = GEN_ILEN (PRINTDEV)
      DO IDEV = 1, NDEVS
        IF (SXG_INQHARD(IDEV)) THEN
          CALL SXG_GTPR(IDEV, DPROMPT, IERR )
          IF (LT .LE. GEN_ILEN(DPROMPT)) THEN
            IF (PRINTDEV(:LT) .EQ. DPROMPT(:LT)) THEN
              IOK  = IOK + 1
              JDEV = IDEV
            END IF
          END IF
        END IF
      END DO

      IF (IOK.EQ.1) THEN
        CALL SXG_GTPR( JDEV, PRINTDEV, IERR )
      ELSE IF (IOK.GT.1) THEN
        PRINT *, 'Ambiguous device!'
        GO TO 99
      ELSE
        PRINT *, 'No such device!'
        GO TO 99
      END IF

      RETURN

*     Error return

   99 CONTINUE
      RETURN

      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGDEVICE (DNAME, FNAME, ORIENT, IERR)

*  Routine to set requested device for graphics output.

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER DNAME*(*)
      CHARACTER FNAME*(*)
      LOGICAL   ORIENT
      INTEGER   IERR

*     Include files

      INCLUDE 'SXG_GRAPHCAP.INC'
      INCLUDE 'SXGPGPLOT.INC'

*     Local variables:

      CHARACTER*16 DEVNAME
      CHARACTER*16 DPROMPT
      INTEGER   I
      INTEGER   ID
      INTEGER   IP
      INTEGER   IDOUT
      INTEGER   LOUT
      INTEGER   IPG
      REAL      X1, X2, Y1, Y2
      CHARACTER*256 FSTRING

*     Functions:

      INTEGER   GEN_ILEN
      INTEGER   PGBEGIN
      INTEGER   SXG_INQDEVNO

*  Ok, go...

*     Find the device ID for the requested device and compare with
*     the current value (IDEV)

      DO I = 1, NDEVS
        CALL SXG_GTPR( I, DPROMPT, IERR )
        IP = GEN_ILEN(DPROMPT)
        IF (DPROMPT(:IP) .EQ. DNAME(:IP)) THEN
          ID = I
        END IF
      END DO

      IF (DEVINIT .AND. SXG_INQDEVNO(ID).NE.IDEV) CALL SXGDEVEND

      CALL SXG_GTDEV( ID, IDEV, DEVNAME, DPROMPT, ITERM, DUAL, COLOUR,
     :  HARDCOPY, OUTFILE, IERR )

!      IF (OUTFILE .EQ. ' ') OUTFILE = 'SPECXDIR:SPECX_PGPLOT.PS'

*     Device already open?

      IF (DEVINIT) THEN

*       Set PGPLOT to clear screen without asking, clear the screen, move
*       Terminal to graphics mode

        CALL PGASK (.FALSE.)
        CALL PGADVANCE
        CALL TTGRAPH

      ELSE

        IF (ITERM) THEN

*         If VT240/330 set to Tektronix emulation mode before opening as a
*               Tektronix device

          CALL TTGOTEK

*         Open the device in PGPLOT

          IDOUT = GEN_ILEN(DEVNAME)
          IF (FNAME.NE.' ') THEN
            LOUT = GEN_ILEN(FNAME)
            FSTRING = FNAME(:LOUT)
            FSTRING(LOUT+1:) = DEVNAME(:IDOUT)
            IPG  = PGBEGIN (0, FSTRING(:LOUT+IDOUT),1,1)
            WRITE (6,*) 'Graphics output assigned to ',FNAME
          ELSE
*           CALL GET_JPITERM (FNAME)
            IPG  = PGBEGIN (0, DEVNAME(:IDOUT), 1, 1)
          END IF

*       Otherwise for hardcopy devices, initialize for specified output file

        ELSE IF (HARDCOPY) THEN
          IDOUT = GEN_ILEN(DEVNAME)
          IF (OUTFILE .NE. ' ') THEN
             LOUT = GEN_ILEN (OUTFILE)
             IPG = PGBEGIN (0, OUTFILE(:LOUT) // DEVNAME(:IDOUT),
     &                      1, 1)
          ELSE
             IPG = PGBEGIN (0, DEVNAME(:IDOUT), 1, 1)
          END IF
        END IF

*       Error in PGBEGIN?

        IF (IPG .EQ. 1) THEN
          DEVINIT = .TRUE.

*         Set flag to clear terminal without asking permission first

          IF (ITERM) THEN
            CALL PGASK (.FALSE.)
          END IF

*         Find size of screen in mm.
*         PGQVP returns the size of the default viewport which is the
*         display surface less a margin 4 default character heights wide.

          CALL PGQVP (2, X1, X2, Y1, Y2)
          XSIZED = X2 + X1
          YSIZED = Y2 + Y1

        ELSE

*         error in PGBEG

          IERR = 73
          RETURN

        END IF
      END IF

C     Set full screen to plotter and virtual window

      CALL PGVSIZE  (0.0, XSIZED/25.4, 0.0, YSIZED/25.4)

C     Initialize virtual co-ordinates to mm

      CALL PGWINDOW (0.0, XSIZED, 0.0, YSIZED)

      print *, '-- sxgdevice -- ', devname,' ',
     :         outfile(:gen_ilen(outfile))
CD     print *, '   idev     = ', idev
CD     print *, '   iterm    = ', iterm
CD     print *, '   dual     = ', dual
CD     print *, '   colour   = ', colour
CD     print *, '   hardcopy = ', hardcopy
CD     print *, '   devname  = ', devname
CD     print *, '   outfile  = ', outfile(:gen_ilen(outfile))

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGDEVEND

*  Routine to close off the current plot device, flush buffer etc,
*  and send hardcopy file to the appropriate device.

      IMPLICIT  NONE

*     Include files

      INCLUDE 'SXGPGPLOT.INC'

*     Local vars

      INTEGER   LF

*     Functions

      INTEGER   GEN_ILEN

*  Ok, go...

      IF (DEVINIT) THEN
        CALL PGEND

*  run command files to print out any intermediate files, or inform user
*  where output file is


*  close down the terminal, tell VT240/330 to go back to VT mode

        IF (ITERM) THEN
          CALL TTGO220
          CALL TTCLOSE
        ELSE
          LF = GEN_ILEN (OUTFILE)
          PRINT *,
     &     'Hardcopy file closed - output in file:  ', OUTFILE(:LF)
        END IF
      END IF

      DEVINIT = .FALSE.

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGCLEAR

*  Routine to clear the screen or erase what is already written to
*  hardcopy output file.

      IMPLICIT  NONE

*  Ok, go...

      CALL PGADVANCE

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGGOTEK

*  Routine to switch Tek-emulator terminals to Tek4010 emulator
*  from "native" VT220 mode or similar

      IMPLICIT  NONE

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      CALL TTGOTEK

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGVT220

*  Routine to switch Tek-emulator terminals back to "native" VT220
*  mode or similar

      IMPLICIT  NONE

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      CALL TTGO220

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGTIDLE

*  Routine to flush graphics buffer and return dual screen alpha/graphics
*  device to alpha mode if in graphics mode.

      IMPLICIT  NONE

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      IF (IDEV .LT. 20) THEN
        CALL TIDLE
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGTTGRAPH

*  Routine to return dual screen alpha/graphics terminal to graphics
*  mode if now in alpha

      IMPLICIT  NONE

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      IF (IDEV.LT.20) THEN
        CALL TTGRAPH
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGCURSOR (XPOS, YPOS, KEY)

C  Routine to read back the current graphics cursor position using
C  Tektronix 4010/4014 GIN emulation, and convert position to millimetres.

      IMPLICIT  NONE

C     Formal parameters

      INTEGER*4 KEY                        ! Decimal equivalent of key hit
      REAL*4    XPOS,  YPOS                ! Cursor position in mm

C     Include files

      INCLUDE  'SXGPGPLOT.INC'

C     Local variables

      CHARACTER CKEY*1
      REAL      U, V

      INTEGER   SXGCTRLC
#if HAVE_BOZ
      DATA SXGCTRLC /Z'08008003'/
#elif HAVE_TYPELESS_BOZ
      DATA SXGCTRLC /X'08008003'/
#elif HAVE_OLD_TYPELESS_BOZ
      DATA SXGCTRLC /'08008003'X/
#else
      BOZ constants not supported
#endif
C     Functions

      INTEGER   PGCURSE

C  Make sure we retain the position from the time this was called
C  previously
      SAVE U,V

C  OK, go...

      CALL PGUPDT ()

      IF (PGCURSE (U, V, CKEY) .NE. 1) THEN
        XPOS = 0.0
        YPOS = 0.0
        KEY  = ICHAR('Q')
        RETURN
      END IF

C     Transform key to integer

      KEY = ICHAR (CKEY)

C     Convert cursor positions to millimetres

      XPOS = XLL + (U-ULL) * (XHH-XLL)/(UHH-ULL)
      YPOS = YLL + (V-VLL) * (YHH-YLL)/(VHH-VLL)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGVWINDOW (XLO, XHI, YLO, YHI)

*  Routine to set current plot box location on screen/output device.
*  Dimensions in millimetres.

      IMPLICIT  NONE

*     Formal parameters:

      REAL XLO
      REAL XHI
      REAL YLO
      REAL YHI

*     Include files

      INCLUDE 'SXGPGPLOT.INC'

*  Ok, go...

      CALL PGVSIZE (XLO/25.4, XHI/25.4, YLO/25.4, YHI/25.4)

*     ... and remember corresponding virtual (mm) co-ordinates

      XLL = XLO
      XHH = XHI
      YLL = YLO
      YHH = YHI

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGLIMITS (XLO, XHI, YLO, YHI)

*  Routine to set limits in user-coordinates of the current plot box.

      IMPLICIT  NONE

*     Formal parameters:

      REAL XLO
      REAL XHI
      REAL YLO
      REAL YHI

*     Include files

      INCLUDE 'SXGPGPLOT.INC'

*  Ok, go...

      CALL PGWINDOW (XLO, XHI, YLO, YHI)

      ULL = XLO
      UHH = XHI
      VLL = YLO
      VHH = YHI

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGRELOCATE (X, Y)

*  Routine to reposition the cursor (without writing) to a new
*  position specified in user coordinates

      IMPLICIT  NONE

*     Formal parameters:

      REAL X
      REAL Y

*  Ok, go...

      CALL PGMOVE (X, Y)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGVRELOCATE (X, Y)

*  Routine to reposition the cursor (without writing) to a new
*  position specified in millimetres

      IMPLICIT  NONE

*     Formal parameters:

      REAL X
      REAL Y

*     Include files

      INCLUDE 'SXGPGPLOT.INC'

*     Local variables

      REAL XW
      REAL YW

*  Ok, go...

      XW = ULL + (X-XLL)*(UHH-ULL)/(XHH-XLL)
      YW = VLL + (Y-YLL)*(VHH-VLL)/(YHH-YLL)

      CALL PGMOVE (XW, YW)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGDRAW (X, Y)

*  Routine to draw a line of current style from current location to
*  a position specified in user coordinates.

      IMPLICIT  NONE

*     Formal parameters:

      REAL X
      REAL Y

*  Ok, go...

      CALL PGDRAW (X, Y)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGVDRAW (X, Y)

*  Routine to draw a line of current style from current location to
*  a position specified in millimetres

      IMPLICIT  NONE

*     Formal parameters:

      REAL X
      REAL Y

*     Include files

      INCLUDE 'SXGPGPLOT.INC'

*     Local variables

      REAL XW
      REAL YW

*  Ok, go...

      XW = ULL + (X-XLL)*(UHH-ULL)/(XHH-XLL)
      YW = VLL + (Y-YLL)*(VHH-VLL)/(YHH-YLL)

      CALL PGDRAW (XW, YW)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGFONT (IFONT)

*  Routine to select MONGO font for labelling.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER IFONT

*     Include files

      INCLUDE  'SXGPGPLOT.INC'

*     Local variables:

      INTEGER  JFONT

*  Ok, go...

      FONT = IFONT

      IF (IFONT.EQ.1) JFONT = 1
      IF (IFONT.EQ.2) JFONT = 1
      IF (IFONT.EQ.3) JFONT = 1
      IF (IFONT.EQ.4) JFONT = 1

      CALL PGSCF (JFONT)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGEXPAND (E)

*  Routine to set default expansion size for characters, tickmarks.

      IMPLICIT  NONE

*     Formal parameters:

      REAL E

*     Include files:

      INCLUDE 'SXGPGPLOT.INC'

*  Ok, go...

      EXPAND = E
      CALL PGSCH (E)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGLABEL (X, Y, STRING)

*  Routine to write a label STRING of NCHARS at the current cursor
*  location.

      IMPLICIT  NONE

*     Formal parameters:

      REAL      X
      REAL      Y
      CHARACTER STRING*(*)

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*     Functions:

      INTEGER   GEN_ILEN

*     Local variables:

      INTEGER   NCHAR

*  Ok, go...

      NCHAR = GEN_ILEN (STRING)
      CALL PGTEXT (X, Y, STRING(:NCHAR))

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGVLABEL (X, Y, STRING)

*  Routine to write a label STRING of NCHARS at the position (X,Y)
*  (specified in millimetres)

      IMPLICIT  NONE

*     Formal parameters:

      REAL      X
      REAL      Y
      CHARACTER STRING*(*)

*     Include files

      INCLUDE   'SXGPGPLOT.INC'

*     Local variables

      INTEGER   NCHAR
      REAL      XW
      REAL      YW

*     Functions

      INTEGER   GEN_ILEN

*  Ok, go...

      XW = ULL + (X-XLL)*(UHH-ULL)/(XHH-XLL)
      YW = VLL + (Y-YLL)*(VHH-VLL)/(YHH-YLL)

      NCHAR = GEN_ILEN (STRING)
      CALL PGTEXT (XW, YW, STRING(:NCHAR))

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGXLABEL (LABEL)

*  Routine to write a label to the x-axis of a plot

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER LABEL*(*)

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      CALL PGLABEL (LABEL, ' ', ' ')

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGXLABEL2 (LABEL)

*  Routine to write a label to the upper x-axis of a plot

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER LABEL*(*)

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

C     CALL PGLABEL (LABEL, ' ', ' ')

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGYLABEL (LABEL)

*  Routine to write a label to the y-axis of a plot

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER LABEL*(*)

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      CALL PGLABEL (' ', LABEL, ' ')

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGSETDIGIT (IDIGIT)

*  Routine to set default number of digits in axis labels etc.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER IDIGIT

*  Ok, go...

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGBOX (LABELX, LABELY)

*  Routine to write a box within the current plot window and then to
*  add scales.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER LABELX
      INTEGER LABELY

*     Include file:

      INCLUDE  'SXGPGPLOT.INC'

*     Local variables

      CHARACTER XOPT*8
      CHARACTER YOPT*8
      INTEGER   NXSUB
      INTEGER   NYSUB

*  Ok, go...

      IF (LABELX .EQ. 0) THEN
        XOPT = 'bcts'
      ELSE IF (LABELX .EQ. 1) THEN
        XOPT = 'bcnts'
      ELSE IF (LABELX .EQ. 2) THEN
        XOPT = 'bcnts'
      END IF

      IF (LABELY .EQ. 0) THEN
        YOPT = 'bcts'
      ELSE IF (LABELY .EQ. 1) THEN
        YOPT = 'bcnts'
      ELSE IF (LABELY .EQ. 2) THEN
        YOPT = 'bcntsv'
      END IF

      IF (XSTICK.NE.0.0) THEN
        NXSUB = NINT (ABS (XTICK/XSTICK))
      ELSE
        NXSUB = 0
      END IF

      IF (YSTICK.NE.0.0) THEN
        NYSUB = NINT (ABS (YTICK/YSTICK))
      ELSE
        NYSUB = 0
      END IF

      CALL PGBOX (XOPT, XTICK, NXSUB, YOPT, YTICK, NYSUB)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGBOX2 (LABELX, LABELY, TOPSCAL, A0, A1)

*  Routine to write a box within the current plot window and then to
*  add scales. If TOPSCAL is set then makes separate top axis scaled
*  x(top) = a0 + a1*x(bottom)

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER LABELX
      INTEGER LABELY
      LOGICAL TOPSCAL
      REAL    A0
      REAL    A1

*     Include file:

      INCLUDE  'SXGPGPLOT.INC'

*     Local variables

      CHARACTER XOPT*8
      CHARACTER YOPT*8
      INTEGER   NXSUB
      INTEGER   NYSUB

*  Ok, go...

      IF (LABELX .EQ. 0) THEN
        XOPT = 'bts'
      ELSE IF (LABELX .EQ. 1) THEN
        XOPT = 'bnts'
      ELSE IF (LABELX .EQ. 2) THEN
        XOPT = 'bnts'
      END IF

      IF (.NOT. TOPSCAL) THEN
        XOPT = 'c'//XOPT
      END IF

      IF (LABELY .EQ. 0) THEN
        YOPT = 'bcts'
      ELSE IF (LABELY .EQ. 1) THEN
        YOPT = 'bcnts'
      ELSE IF (LABELY .EQ. 2) THEN
        YOPT = 'bcntsv'
      END IF

      IF (XSTICK.NE.0.0) THEN
        NXSUB = NINT (ABS (XTICK/XSTICK))
      ELSE
        NXSUB = 0
      END IF

      IF (YSTICK.NE.0.0) THEN
        NYSUB = NINT (ABS (YTICK/YSTICK))
      ELSE
        NYSUB = 0
      END IF

      CALL PGBOX (XOPT, XTICK, NXSUB, YOPT, YTICK, NYSUB)

      IF (TOPSCAL) THEN
        IF (LABELX.EQ.0) THEN
          XOPT = 'cts'
        ELSE IF (LABELX.EQ.1) THEN
          XOPT = 'cmts'
        ELSE IF (LABELX.EQ.2) THEN
          XOPT = 'cmts'
        END IF
        YOPT = ' '
        CALL PGWINDOW (A0+A1*ULL,   A0+A1*UHH,   VLL, VHH)
        CALL PGBOX    (XOPT, XTICK, NXSUB, YOPT, YTICK, NYSUB)
        CALL PGWINDOW (ULL,         UHH,         VLL, VHH)
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGRDBOX (LABELX, LABELY)

*  Routine to write a box within the current plot window and then to
*  add scales. Labels axes if required in time coordinates;

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER          LABELX
      INTEGER          LABELY

*     Include file:

      INCLUDE  'SXGPGPLOT.INC'

*     Local variables

      CHARACTER XOPT*16
      CHARACTER YOPT*16
      INTEGER   NXSUB
      INTEGER   NYSUB
      REAL      XTICKD
      REAL      YTICKD

*  Ok, go...

      IF (LABELX .EQ. 0) THEN
        XOPT = 'bcts'
      ELSE IF (LABELX .EQ. 1) THEN
        XOPT = 'bcnts'
      ELSE IF (LABELX .EQ. 2) THEN
        XOPT = 'bcnts'
      END IF

      IF (XSTICK.NE.0.0) THEN
        NXSUB = NINT (ABS (XTICK/XSTICK))
      ELSE
        NXSUB = 0
      END IF

      XTICKD = XTICK
      IF (XSEX) THEN
        XOPT   = 'ZYH'//XOPT
        XTICKD = 0.0
        NXSUB  = 0
      END IF

      IF (LABELY .EQ. 0) THEN
        YOPT = 'bcts'
      ELSE IF (LABELY .EQ. 1) THEN
        YOPT = 'bcnts'
      ELSE IF (LABELY .EQ. 2) THEN
        YOPT = 'bcntsv'
      END IF

      IF (YSTICK.NE.0.0) THEN
        NYSUB = NINT (ABS (YTICK/YSTICK))
      ELSE
        NYSUB = 0
      END IF

      YTICKD = YTICK
      IF (YSEX) THEN
        YOPT   = 'ZYD'//YOPT
        YTICKD = 0.0
        NYSUB  = 0
      END IF

      CALL PGTBOX   (XOPT, XTICKD, NXSUB, YOPT, YTICKD, NYSUB)

      XSEX = .FALSE.
      YSEX = .FALSE.

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGTICKSIZE (SX, BX, SY, BY)

*  Routine to set interval between ticks for default axes such
*  as are drawn by BOX etc.

      IMPLICIT  NONE

*     Formal parameters:

      REAL SX
      REAL BX
      REAL SY
      REAL BY

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      XTICK  = BX
      YTICK  = BY
      XSTICK = SX
      YSTICK = SY

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGCONNECT (XARR, YARR, NPTS)

*  Routine to draw straight lines linking the points in an array.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER NPTS
      REAL    XARR(NPTS)
      REAL    YARR(NPTS)

*  Ok, go...

      CALL PGLINE (NPTS, XARR, YARR)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGPOINTS (NXY, X, Y, STYLE)

*  Routine to write points of nominated style at locations specified
*  by x and y arrays...

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER NXY
      REAL    X(NXY)
      REAL    Y(NXY)
      INTEGER STYLE

*  Ok, go...

      CALL PGPOINT (NXY, X, Y, STYLE)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGCONTOUR (Z, NX, NY, UL, UH, VL, VH, LEVELS, NL)

*  Routine to contour image with NL contours set in array LEVELS.
*  Image has dimension NX * NY and has x-extent (XL,XH), y-extent (YL,YH)

*  26-MAY-1992: Modified to use only line type of 1 (black) if device is
*               CANON laser printer

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER  NX, NY
      REAL     Z(NX,NY)
      REAL     UL, UH
      REAL     VL, VH
      INTEGER  NL
      REAL     LEVELS(NL)

*     Include file

      INCLUDE  'SXGPGPLOT.INC'

*     Local variables

      REAL     TR(6)
      REAL     DX, DY

*  Ok, go...

      DX    = (UH-UL)/FLOAT(NX-1)
      DY    = (VH-VL)/FLOAT(NY-1)

      TR(1) = UL - DX
      TR(2) = DX
      TR(3) = 0.0

      TR(4) = VL - DY
      TR(5) = 0.0
      TR(6) = DY

*     Call contouring routine

      CALL PGCONT (Z, NX, NY, 1, NX, 1, NY, LEVELS, -NL, TR)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGCONB (Z, NX, NY, UL, UH, VL, VH, LEVELS, NL, BAD)

*  Routine to contour image with NL contours set in array LEVELS.
*  Image has dimension NX * NY and has x-extent (XL,XH), y-extent (YL,YH)
*  Blanking value BAD masks off contours in unset pixels.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER  NX, NY
      REAL     Z(NX,NY)
      REAL     UL, UH
      REAL     VL, VH
      INTEGER  NL
      REAL     LEVELS(NL)
      REAL     BAD

*     Include file

      INCLUDE  'SXGPGPLOT.INC'

*     Local variables

      REAL     TR(6)
      REAL     DX, DY

*  Ok, go...

      DX    = (UH-UL)/FLOAT(NX-1)
      DY    = (VH-VL)/FLOAT(NY-1)

      TR(1) = UL - DX
      TR(2) = DX
      TR(3) = 0.0

      TR(4) = VL - DY
      TR(5) = 0.0
      TR(6) = DY

*     Call contouring routine

      CALL PGCONB (Z, NX, NY, 1, NX, 1, NY, LEVELS, -NL, TR, BAD)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGGREY (Z, NX, NY, UL, UH, VL, VH, GREYLIM)

*  Routine to greyscale image with greyscale limits set in array GREYLIM.
*  Image has dimension NX * NY and has x-extent (XL,XH), y-extent (YL,YH)

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER  NX, NY
      REAL     Z(NX,NY)
      REAL     UL, UH
      REAL     VL, VH
      REAL     GREYLIM(2)

*     Include file

      INCLUDE  'SXGPGPLOT.INC'

*     Functions

      LOGICAL  SXGCOLOROK

*     Local variables

      REAL     TR(6)
      REAL     DX, DY

*  Ok, go...

      DX    = (UH-UL)/FLOAT(NX-1)
      DY    = (VH-VL)/FLOAT(NY-1)

      TR(1) = UL - DX
      TR(2) = DX
      TR(3) = 0.0

      TR(4) = VL - DY
      TR(5) = 0.0
      TR(6) = DY

*     Call grey-scaling routine or colouring routine, as appropriate

      IF (SXGCOLOROK()) THEN
        CALL PGIMAG  (Z, NX, NY, 1, NX, 1, NY, GREYLIM(1), GREYLIM(2),
     &                TR)
      ELSE
        CALL PGGRAY  (Z, NX, NY, 1, NX, 1, NY, GREYLIM(2), GREYLIM(1),
     &                TR)
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGPLOTID (STRING1, STRING2)

*  Routine to put a label to a plot.

      IMPLICIT  NONE

*     Formal parameters:

      CHARACTER STRING1*(*)
      CHARACTER STRING2*(*)

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*     Local variables:

*      INTEGER NTICKS
      INTEGER STATUS
      INTEGER L1, L2
      CHARACTER CTIME*24
      CHARACTER*256 TSTRING

*  Ok, go...

      STATUS = 0
*     CALL PSX_TIME   (NTICKS, STATUS)
*     CALL PSX_CTIME  (NTICKS, CTIME, STATUS)
      CALL UGETDATTIM (CTIME,  STATUS)

      CALL PGSCH   (EXPAND*0.6)
      L1 = LEN( STRING1 )
      L2 = LEN( STRING2 )
      TSTRING = STRING1
      TSTRING(L1+2:) = STRING2(:L2)
      TSTRING(L1+L2+3:) = CTIME

      CALL PGMTEXT ('t', +0.2, 1.0, 1.0, TSTRING(:L1+L2+2+24) )
      CALL PGSCH   (EXPAND)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGLTYPE (ILINE)

*  Routine to set the type of line to be used for CONNECT etc.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER ILINE

*     Local variables

      INTEGER LINE

*  Ok, go...

      LINE = ILINE+1
      IF (LINE.GT.5)  LINE = 1
      CALL PGSLS (LINE)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGLWEIGHT (IWEIGHT)

*  Routine to set the lineweight to be used for CONNECT etc.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER IWEIGHT

*  Ok, go...

      CALL PGSLW (IWEIGHT)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGSCI (IC)

*  Routine to set colour index to be used for subsequent plotting
*  (colour table is defined elsewhere: CI=0 is background/erase, CI=1
*   is foreground).

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER IC

*  Ok, go...

      CALL PGSCI (IC)

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGSCR (IC, CR, CG, CB)

*  Routine to set the (RGB) colour representation for colour IC

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER IC
      REAL    CR, CG, CB

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

*     PRINT *, '-- sxgscr -- set col ',IC, ' to ', cr, cg, cb
      CALL PGSCR (IC, CR, CG, CB)

      RED(IC)   = CR
      GREEN(IC) = CG
      BLUE(IC)  = CB

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGXSIX (NDEC)

*  Routine to plot x-axis value as sexagisimal with NDEC decimal places
*  on lsb.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER NDEC

*     Include files:

      INCLUDE 'SXGPGPLOT.INC'

*  Ok, go...

      NDECX =  NDEC
      XSEX  = .TRUE.

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGYSIX (NDEC)

*  Routine to plot y-axis value as sexagisimal with NDEC decimal places
*  on lsb.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER NDEC

*     Include files:

      INCLUDE 'SXGPGPLOT.INC'

*  Ok, go...

      NDECY =  NDEC
      YSEX  = .TRUE.

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGDEVINFO (XSIZEQ, YSIZEQ)

*  Routine to return X and Y sizes of current output device in mm

      IMPLICIT  NONE

*     Formal parameters:

      REAL    XSIZEQ
      REAL    YSIZEQ

*     Include files

      INCLUDE 'SXGPGPLOT.INC'

*  Ok, go...

      XSIZEQ = XSIZED
      YSIZEQ = YSIZED

      RETURN
      END

*-----------------------------------------------------------------------

      LOGICAL FUNCTION SXGGREYOK ()

*  Routine to return logical indicating whether or not graphics can do
*  greyscaling.

      IMPLICIT  NONE

*     Include file

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      SXGGREYOK = GREYOK

      RETURN
      END

*-----------------------------------------------------------------------

      LOGICAL FUNCTION SXGCOLOROK ()

*  Routine to return logical indicating whether or not graphics can do
*  colour.

      IMPLICIT  NONE

*     Include file

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

      SXGCOLOROK = COLOROK .and. COLOUR

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE SXGQCOL (COLMIN, COLMAX)

*  Routine to return minimum and maximum colour indices available on plot
*  device

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER COLMIN, COLMAX

*     Include file

      INCLUDE  'SXGPGPLOT.INC'

*  ok, go...

      CALL PGQCOL (COLMIN, COLMAX)

      IF (COLMAX .GT. 1) THEN
         COLMIN = 16                  ! lowest 16 indices are set by PGPLOT
      END IF                          ! itself

      RETURN
      END

*-----------------------------------------------------------------------
