C*PGOPEN -- open a graphics device
C%int cpgopen(const char *device);
C+
      INTEGER FUNCTION PGOPEN (DEVICE)
      CHARACTER*(*) DEVICE
C
C Open a graphics device for PGPLOT output. If the device is
C opened successfully, it becomes the selected device to which
C graphics output is directed until another device is selected
C with PGSLCT or the device is closed with PGCLOS.
C
C The value returned by PGOPEN should be tested to ensure that
C the device was opened successfully, e.g.,
C
C       ISTAT = PGOPEN('plot.ps/PS')
C       IF (ISTAT .LE. 0 ) STOP
C
C Note that PGOPEN must be declared INTEGER in the calling program.
C
C The DEVICE argument is a character constant or variable; its value
C should be one of the following:
C
C (1) A complete device specification of the form 'device/type' or
C     'file/type', where 'type' is one of the allowed PGPLOT device
C     types (installation-dependent) and 'device' or 'file' is the 
C     name of a graphics device or disk file appropriate for this type.
C     The 'device' or 'file' may contain '/' characters; the final
C     '/' delimits the 'type'. If necessary to avoid ambiguity,
C     the 'device' part of the string may be enclosed in double
C     quotation marks.
C (2) A device specification of the form '/type', where 'type' is one
C     of the allowed PGPLOT device types. PGPLOT supplies a default
C     file or device name appropriate for this device type.
C (3) A device specification with '/type' omitted; in this case
C     the type is taken from the environment variable PGPLOT_TYPE,
C     if defined (e.g., setenv PGPLOT_TYPE PS). Because of possible
C     confusion with '/' in file-names, omitting the device type
C     in this way is not recommended.
C (4) A blank string (' '); in this case, PGOPEN will use the value
C     of environment variable PGPLOT_DEV as the device specification,
C     or '/NULL' if the environment variable is undefined.
C (5) A single question mark, with optional trailing spaces ('?'); in
C     this case, PGPLOT will prompt the user to supply the device
C     specification, with a prompt string of the form
C         'Graphics device/type (? to see list, default XXX):'
C     where 'XXX' is the default (value of environment variable
C     PGPLOT_DEV).
C (6) A non-blank string in which the first character is a question
C     mark (e.g., '?Device: '); in this case, PGPLOT will prompt the
C     user to supply the device specification, using the supplied
C     string as the prompt (without the leading question mark but
C     including any trailing spaces).
C
C In cases (5) and (6), the device specification is read from the
C standard input. The user should respond to the prompt with a device
C specification of the form (1), (2), or (3). If the user types a 
C question-mark in response to the prompt, a list of available device
C types is displayed and the prompt is re-issued. If the user supplies
C an invalid device specification, the prompt is re-issued. If the user
C responds with an end-of-file character, e.g., ctrl-D in UNIX, program
C execution is aborted; this  avoids the possibility of an infinite
C prompting loop.  A programmer should avoid use of PGPLOT-prompting
C if this behavior is not desirable.
C
C The device type is case-insensitive (e.g., '/ps' and '/PS' are 
C equivalent). The device or file name may be case-sensitive in some
C operating systems.
C
C Examples of valid DEVICE arguments:
C
C (1)  'plot.ps/ps', 'dir/plot.ps/ps', '"dir/plot.ps"/ps', 
C      'user:[tjp.plots]plot.ps/PS'
C (2)  '/ps'      (PGPLOT interprets this as 'pgplot.ps/ps')
C (3)  'plot.ps'  (if PGPLOT_TYPE is defined as 'ps', PGPLOT
C                  interprets this as 'plot.ps/ps')
C (4)  '   '      (if PGPLOT_DEV is defined)
C (5)  '?  '
C (6)  '?Device specification for PGPLOT: '
C
C [This routine was added to PGPLOT in Version 5.1.0. Older programs
C use PGBEG instead.]
C
C Returns:
C  PGOPEN          : returns either a positive value, the
C                    identifier of the graphics device for use with
C                    PGSLCT, or a 0 or negative value indicating an
C                    error. In the event of error a message is
C                    written on the standard error unit.
C Arguments:
C  DEVICE  (input) : the 'device specification' for the plot device
C                    (see above).
C--
C 22-Dec-1995 - new routine [TJP].
C 14-May-1996 - device '? ' should not give a blank prompt [TJP].
C-----------------------------------------------------------------------
      INCLUDE       'pgplot.inc'
      INTEGER       DEFTYP,GRDTYP,GROPEN,L,LR,IC1, LPROMP
      INTEGER       GRGCOM, IER, LDEFDE, UNIT, ISTAT
      REAL          DUMMY,DUMMY2,XCSZ, XSZ, YSZ
      CHARACTER*128 DEFDEV, PROMPT
      CHARACTER*20  DEFSTR
      CHARACTER*256 REQ
      LOGICAL JUNK
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
C Get the default device/type (environment variable PGPLOT_DEV).
C
      CALL GRGENV('DEV', DEFDEV, LDEFDE)
      IF (LDEFDE.EQ.0) THEN
         DEFDEV = '/NULL'
         LDEFDE = 5
      END IF
C
C Open the plot file; default type is given by environment variable
C PGPLOT_TYPE.
C
      CALL GRGENV('TYPE', DEFSTR, L)
      IF (L.EQ.0) THEN
          DEFTYP = 0
      ELSE
          CALL GRTOUP(DEFSTR, DEFSTR)
          DEFTYP = GRDTYP(DEFSTR(1:L))
      END IF
      IF (DEVICE.EQ.' ') THEN
C        -- Blank device string: use default device and type.
         ISTAT = GROPEN(DEFTYP,UNIT,DEFDEV(1:LDEFDE),PGID)
      ELSE IF (DEVICE(1:1).EQ.'?') THEN
         IF (DEVICE.EQ.'?') THEN
C           -- Device string is a ingle question mark: prompt user
C           -- for device/type
            PROMPT = 'Graphics device/type (? to see list, default '
     :           //DEFDEV(1:LDEFDE)//'): '
            LPROMP = LDEFDE + 48
         ELSE
C           -- Device string starts with a question mark: use it
C           -- as a prompt
            PROMPT = DEVICE(2:)
            LPROMP = LEN(DEVICE)-1
         END IF
   10    IER = GRGCOM(REQ, PROMPT(1:LPROMP), LR)
         IF (IER.NE.1) THEN
            CALL GRWARN('Error reading device specification')
            PGOPEN = -1
            RETURN
         END IF
         IF (LR.LT.1 .OR. REQ.EQ.' ') THEN
            REQ = DEFDEV(1:LDEFDE)
         ELSE IF (REQ(1:1).EQ.'?') THEN
            CALL PGLDEV
            GOTO 10
         END IF
         ISTAT = GROPEN(DEFTYP,UNIT,REQ,PGID)
         IF (ISTAT.NE.1) GOTO 10
      ELSE
          ISTAT = GROPEN(DEFTYP,UNIT,DEVICE,PGID)
      END IF
C
C Failed to open plot file?
C
      IF (ISTAT.NE.1) THEN
         PGOPEN = - 1
         RETURN
      END IF
C
C Success: determine device characteristics.
C
      IF (PGID.LT.0 .OR. PGID.GT.PGMAXD) CALL
     1       GRWARN('Something terribly wrong in PGOPEN')
      PGDEVS(PGID) = 1
      PGADVS(PGID) = 0
      PGPFIX(PGID) = .FALSE.
      CALL GRSIZE(PGID,XSZ,YSZ,DUMMY,DUMMY2,
     1            PGXPIN(PGID),PGYPIN(PGID))
      CALL GRCHSZ(PGID,XCSZ,DUMMY,PGXSP(PGID),PGYSP(PGID))
      PGROWS(PGID)= .TRUE.
      PGNX(PGID)  = 1
      PGNY(PGID)  = 1
      PGXSZ(PGID) = XSZ
      PGYSZ(PGID) = YSZ
      PGNXC(PGID) = 1
      PGNYC(PGID) = 1
      CALL GRQTYP(DEFSTR,JUNK)
C
C Set the prompt state to ON, so that terminal devices pause between
C pages; this can be changed with PGASK.
C
      CALL PGASK(.TRUE.)
C
C If environment variable PGPLOT_BUFFER is defined (any value),
C start buffering output.
C
      PGBLEV(PGID) = 0
      CALL GRGENV('BUFFER', DEFSTR, L)
      IF (L.GT.0) CALL PGBBUF
C
C Set background and foreground colors if requested.
C
      CALL GRGENV('BACKGROUND', DEFSTR, L)
      IF (L.GT.0) CALL PGSCRN(0, DEFSTR(1:L), IER)
      CALL GRGENV('FOREGROUND', DEFSTR, L)
      IF (L.GT.0) CALL PGSCRN(1, DEFSTR(1:L), IER)
C
C Set default attributes.
C
      CALL PGSCI(1)
      CALL PGSLS(1)
      CALL PGSLW(1)
      CALL PGSCH(1.0)
      CALL PGSCF(1)
      CALL PGSFS(1)
      CALL PGSAH(1, 45.0, 0.3)
      CALL PGSTBG(-1)
      CALL PGSHS(45.0, 1.0, 0.0)
      CALL PGSCLP(1)
C
C Set the default range of color indices available for images (16 to
C device maximum, if device maximum >= 16; otherwise not possible).
C Select linear transfer function.
C
      CALL GRQCOL(IC1, PGMXCI(PGID))
      PGMNCI(PGID) = 16
      IF (PGMXCI(PGID).LT.16) PGMXCI(PGID) = 0
      PGITF(PGID) = 0
C
C Set the default window (unit square).
C
      PGXBLC(PGID) = 0.0
      PGXTRC(PGID) = 1.0
      PGYBLC(PGID) = 0.0
      PGYTRC(PGID) = 1.0
C
C Set the default viewport.
C
      CALL PGVSTD
C
      PGOPEN = PGID
      RETURN
      END
