C*GROPEN -- open device for graphics
C+
      INTEGER FUNCTION GROPEN (TYPE,DUMMY,FILE,IDENT)
      INTEGER   TYPE, DUMMY, IDENT
      CHARACTER*(*) FILE
C
C GRPCKG: assign a device and prepare for plotting.  GROPEN must be
C called before all other calls to GRPCKG routines.
C
C Returns:
C
C GROPEN (output, integer): 1 => success, any other value
C       indicates a failure (usually the value returned will
C       be a VMS error code). In the event of an error, a
C       message will be sent to the standard error unit.
C
C Arguments:
C
C TYPE (input, integer): default device type (integer code).
C DUMMY (input, integer): not used at present.
C FILE (input, character): plot specifier, of form 'device/type'.
C IDENT (output, integer): plot identifier to be used in later
C       calls to GRPCKG.
C
C  1-Jun-1984 - [TJP].
C  2-Jul-1984 - change to call GRSLCT [TJP].
C 13-Jul-1984 - add device initialization [TJP].
C 23-Jul-1984 - add /APPEND qualifier.
C 19-Oct-1984 - add VV device [TJP].
C 26-Dec-1984 - obtain default file name from common [TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 12-Oct-1986 - fix bug causing GREXEC to erase screen [AFT].
C  3-Jun-1987 - remove declaration of exit handler [TJP].
C 15-Dec-1988 - standardize [TJP].
C 25-Jun-1989 - remove code that removes spaces from the device name 
C               [TJP].
C 26-Nov-1990 - [TJP].
C  5-Jan-1993 - [TJP].
C  1-Sep-1994 - store device capabilities in common for later use [TJP].
C 17-Apr-1995 - zero-length string fix [TJP].
C  6-Jun-1995 - explicitly initialize GRSTAT [TJP].
C 29-Apr-1996 - moved initialization into GRINIT [TJP].
C 12-Jul-1999 - fix bug [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER   IER, FTYPE, NBUF, LCHR
      INTEGER   GRPARS, GRTRIM
      REAL      RBUF(6)
      LOGICAL   APPEND
      CHARACTER*128 FFILE,CHR
C
C Initialize GRPCKG; read font file (if necessary).
C
      CALL GRINIT
C
C Allocate an identifier.
C
      IDENT = 1
   10 IF (GRSTAT(IDENT).NE.0) THEN
          IDENT = IDENT+1
          IF (IDENT.GT.GRIMAX) THEN
              CALL GRWARN('Too many active plots.')
              GROPEN = -1
              IDENT = 0
              RETURN
          END IF
      GOTO 10
      END IF
C
C Validate the device specification.
C
      IER = GRPARS(FILE,FFILE,FTYPE,APPEND)
      IF (IER.NE.1) THEN
          CHR = 'Invalid device specification: '
          CHR(31:) = FILE
          CALL GRWARN(CHR)
          GROPEN = -1
          RETURN
      END IF
      IF (FTYPE.EQ.0) FTYPE = TYPE
      IF (1.LE.FTYPE) THEN
          GRTYPE(IDENT) = FTYPE
      ELSE
          CHR = 'Device type omitted or invalid: '
          CHR(33:) = FILE
          CALL GRWARN(CHR)
          GROPEN = -1
          RETURN
      END IF
C
C Install the file name, or assign default.
C
      IF (FFILE.EQ.' ') THEN
          CALL GREXEC(GRTYPE(IDENT), 5,RBUF,NBUF,FFILE,LCHR)
      END IF
      GRFILE(IDENT) = FFILE
      GRFNLN(IDENT) = MAX(1,GRTRIM(GRFILE(IDENT)))
C
C Open workstation.
C
      RBUF(3)=0
      IF (APPEND) RBUF(3)=1
      NBUF=3
      CALL GREXEC(GRGTYP, 9,RBUF,NBUF, GRFILE(IDENT),GRFNLN(IDENT))
      GROPEN=RBUF(2)
      IF (GROPEN.NE.1) THEN
         IDENT = 0
         RETURN
      END IF
      GRGTYP = GRTYPE(IDENT)
      GRUNIT(IDENT)=RBUF(1)
      GRPLTD(IDENT) = .FALSE.
      GRSTAT(IDENT) = 1
      CALL GRSLCT(IDENT)
C
C Install the default plot parameters
C
C--- Inquire color-index range.
      CALL GREXEC(GRGTYP, 2,RBUF,NBUF,CHR,LCHR)
      GRMNCI(IDENT)=RBUF(5)
      GRMXCI(IDENT)=RBUF(6)
C--- Inquire resolution.
      CALL GREXEC(GRGTYP, 3,RBUF,NBUF,CHR,LCHR)
      GRPXPI(IDENT)=RBUF(1)
      GRPYPI(IDENT)=RBUF(2)
C--- Inquire default character size.
      CALL GREXEC(GRGTYP, 7,RBUF,NBUF,CHR,LCHR)
      GRCSCL(IDENT) = RBUF(1)
      GRCFAC(IDENT) = RBUF(1)
C--- Inquire default plot size.
      CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
      GRXMXA(IDENT) = RBUF(2)
      GRYMXA(IDENT) = RBUF(4)
      GRXMIN(IDENT) = RBUF(1)
      GRXMAX(IDENT) = RBUF(2)
      GRYMIN(IDENT) = RBUF(3)
      GRYMAX(IDENT) = RBUF(4)
C--- Inquire device capabilities.
      GRGCAP(IDENT) = 'NNNNNNNNNNN'
      CALL GREXEC(GRGTYP, 4,RBUF,NBUF,CHR,LCHR)
      IF (LCHR.GT.LEN(GRGCAP(IDENT))) LCHR = LEN(GRGCAP(IDENT))
      GRGCAP(IDENT)(1:LCHR) = CHR(:LCHR)
C--- Current pen position.
      GRXPRE(IDENT) = 0.0
      GRYPRE(IDENT) = 0.0
C--- GRSETS has not been called.
      GRADJU(IDENT) = .FALSE.
C---Default scaling.
      CALL GRTRN0(0.0, 0.0, 1.0, 1.0)
C
C Default attributes.
C  text font (normal)
C  color (white)
C  line-style (full)
C  line-width (minimum)
C  marker number (dot)
C
      GRCFNT(IDENT) = 1
      GRCCOL(IDENT) = 1
      GRSTYL(IDENT) = 1
      GRWIDT(IDENT) = 1
      GRCMRK(IDENT) = 1
      GRDASH(IDENT) = .FALSE.
C
      GROPEN = 1
C
      END


