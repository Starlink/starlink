C+
      SUBROUTINE SOFT
C
C     S O F T  /  H A R D / I D E V
C
C     SOFT is used to set the user variable (SOFT) that controls
C     the soft copy graphics output of Figaro programs.  HARD is
C     used to set the user variable (HARD) that controls the
C     hardcopy graphics output. IDEV is used to set the user variable
C     (IDEV) that controls the image display.
C
C     Command parameters -
C
C     HARDDEV  (Character string) A device name recognised by
C       or     the GKS version of PGPLOT.  Normally, what should
C     SOFTDEV  be used is one of the device names recognised by
C       or     GNS.  HARDDEV and SOFTDEV are used by HARD and SOFT
C     IMAGDEV  respectively.
C
C     Command keywords -
C
C     OPTIONS  Causes a list of the various acceptable SGS device
C              names to be output.
C
C     FORCE    Forces the device specification to be accepted even
C              if it does not match one of the acceptable SGS names.
C
C     DRAW     Draws a test diagram on the screen.  (DRAW is the
C              default for SOFT and IDEV, NODRAW is the default for HARD)
C
C                                     KS / AAO 16th March 1988
C     History -
C
C     16th March 1988.  KS/AAO.  This is a complete re-write and
C                       combination of the two previously separate
C                       routines HARD and SOFT, intended for use with
C                       the GKS version of PGPLOT.
C     6th Sept 1989.    KS / AAO.  Now outputs a better error description
C                       if the user variable cannot be set.  Also tests
C                       for a parameter system abort request.
C     28th Oct 1991.    HME/UoE.  Increase maximum number of devices in
C                       list to 96 (from 32).
C     19th Aug 1992.    HME/UoE.  Do not fold the device name to upper
C                       case.
C     22nd Jan 1993.    HME/UoE, Starlink.  Use GNS rather than SGS
C                       so as to make abbreviated device names acceptable.
C     10th Mar 1993.    HME/UoE, Starlink.  Add the IDEV command.
C     19th Mar 1993.    HME/UoE, Starlink.  Remove call to GEN_ERRMSG,
C                       the status would be from VAR and is not a VMS
C                       (or Unix) status.
C     2nd  Apr 1996.    BKM/RAL, Starlink.  Initialise STATUS.
C+
      IMPLICIT NONE
C
C     Functions
C
      LOGICAL PAR_ABORT, PAR_BATCH
      INTEGER PGBEGIN, ICH_LEN
C
C     Parameters
C
      INTEGER   MAXPEN             ! Protect so many PGPLOT pens
      PARAMETER (MAXPEN=16)
C
C     Local variables
C
      LOGICAL   BATCH              ! True if in batch mode
      INTEGER   COL1, COL2         ! First and last PGPLOT pen
      CHARACTER COMMAND*4          ! 'SOFT' or 'HARD'
      INTEGER   CONTEX             ! GNS search context
      CHARACTER DEVICE*7           ! Device parameter name
      LOGICAL   DRAW               ! Setting of DRAW keyword
      LOGICAL   DRAWDEF            ! Default setting for DRAW keyword
      LOGICAL   FAULT              ! Indicates an error
      LOGICAL   FORCE              ! Setting of FORCE keyword
      CHARACTER GNSDEV*64          ! Device name from GNS list
      CHARACTER GNSDSC*64          ! Device desription
      REAL      GREY               ! Grey intensity
      INTEGER   I                  ! Loop index
      INTEGER   ICONID             ! Returned by GNS translator
      INTEGER   IWKTYP             ! Returned by GNS translator
      INTEGER   LD                 ! Length of device description or name
      REAL      MAXMIN             ! Scaling between pen and grey value
      LOGICAL   OPTIONS            ! Setting of OPTIONS keyword
      CHARACTER PLTDEV*64          ! Plot device specification
      LOGICAL   REPEAT             ! Controls repeat of device spec loop
      LOGICAL   REPEA2             ! Controls repeat of device list loop
      INTEGER   STATUS             ! Used for all status codes
      CHARACTER STRING*80          ! String to display device list
C
C     Routine used to handle SGS names
C
      EXTERNAL GNS_FILTG
C
C     Initial values
C
      STATUS=0
      FAULT=.FALSE.
C
C     See which command we are servicing, and see if we are in
C     batch mode or not.
C
      CALL PAR_COMMAND(COMMAND)
      BATCH = PAR_BATCH()
      IF (COMMAND.EQ.'SOFT') THEN
         DEVICE='SOFTDEV'
         DRAWDEF = .TRUE.
      ELSE IF (COMMAND.EQ.'IDEV') THEN
         DEVICE='IMAGDEV'
         DRAWDEF = .TRUE.
      ELSE
         DEVICE='HARDDEV'
         DRAWDEF = .FALSE.
      END IF
C
C     Get the keywords.
C
      CALL PAR_RDKEY('OPTIONS',.FALSE.,OPTIONS)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDKEY('FORCE',.FALSE.,FORCE)
      IF (PAR_ABORT()) GO TO 500
      CALL PAR_RDKEY('DRAW',DRAWDEF,DRAW)
      IF (PAR_ABORT()) GO TO 500
C
C     See if OPTIONS has been specified.  If so, list devices.
C     This loops through the list of GNS devices. GNS_GWNG gets
C     the next list entry, or returns CONTEX=0 behind the end of
C     the list. As long as another entry was found, the name and
C     desription are written with PAR_WRUSER.
C
      IF (OPTIONS) THEN
         CALL PAR_WRUSER(' ',STATUS)
         CALL PAR_WRUSER('Possible devices are:',STATUS)
         CALL PAR_WRUSER(' ',STATUS)
         REPEA2 = .TRUE.
         CONTEX = 0
         DO WHILE (REPEA2)
            CALL GNS_GWNG(GNS_FILTG,CONTEX,GNSDEV,GNSDSC,LD,STATUS)
            IF (STATUS.NE.0) GO TO 500
            IF (CONTEX.GT.0) THEN
               LD = MAX(12,ICH_LEN(GNSDEV)+2)
               STRING = GNSDEV
               STRING (LD:) = GNSDSC
               CALL PAR_WRUSER (STRING(:ICH_LEN(STRING)),STATUS)
            ELSE
               REPEA2 = .FALSE.
            END IF
         END DO
         CALL PAR_WRUSER(' ',STATUS)
      END IF
C
C     Get device specification.  If the spec is unacceptable,
C     this loop will be repeated, unless FORCE is specified or
C     we are in batch mode.
C
      REPEAT = .TRUE.
      DO WHILE (REPEAT)
         CALL PAR_RDCHAR (DEVICE,' ',PLTDEV)
         IF (PAR_ABORT()) GO TO 500
C
C        If FORCE or BATCH we accept the device name anyway.
C
         IF (FORCE.OR.BATCH) THEN
            REPEAT = .FALSE.
         ELSE
C
C           Let GNS translate the name just to see if it makes sense.
C           If it doesn't, repeat the loop.
C
            CALL GNS_TNG(PLTDEV,IWKTYP,ICONID,STATUS)
            IF (STATUS.EQ.0) THEN
               REPEAT = .FALSE.
            ELSE
               CALL EMS_ANNUL(STATUS)
               CALL PAR_CNPAR(DEVICE)
               CALL PAR_WRUSER(' ',STATUS)
               CALL PAR_WRUSER ('Device specification "'//
     :            PLTDEV(:ICH_LEN(PLTDEV))//
     :            '" is not a valid GNS name',STATUS)
               CALL PAR_WRUSER(' ',STATUS)
               CALL PAR_WRUSER('Possible devices are:',STATUS)
               CALL PAR_WRUSER(' ',STATUS)
               REPEA2 = .TRUE.
               CONTEX = 0
               DO WHILE (REPEA2)
                  CALL GNS_GWNG(GNS_FILTG,CONTEX,
     :               GNSDEV,GNSDSC,LD,STATUS)
                  IF (STATUS.NE.0) GO TO 500
                  IF (CONTEX.GT.0) THEN
                     LD = MAX(12,ICH_LEN(GNSDEV)+2)
                     STRING = GNSDEV
                     STRING (LD:) = GNSDSC
                     CALL PAR_WRUSER (STRING(:ICH_LEN(STRING)),STATUS)
                  ELSE
                     REPEA2 = .FALSE.
                  END IF
               END DO
               CALL PAR_WRUSER(' ',STATUS)
            END IF
         END IF
      END DO
C
C     Are we to draw a trial plot?
C
      IF (DRAW) THEN
         STATUS = PGBEGIN (0,PLTDEV,1,1)
         IF (STATUS.NE.1) THEN
            CALL PAR_WRUSER (
     :        'Unable to open specified device for plotting',STATUS)
         ELSE
            CALL PGADVANCE
            CALL PGVSTAND
            CALL PGWINDOW (0.0,1.0,0.0,1.0)
            CALL PGBOX ('BC',0.0,0,'BC',0.0,0)
            IF (COMMAND.EQ.'IDEV') THEN
               CALL PGQCOL(COL1,COL2)
               IF (COL2.LE.MAXPEN) THEN
                  CALL PAR_WRUSER('Unable to set up grey scale '//
     :               'lookup table. Too few colours available '//
     :               'for image display.',STATUS)
               ELSE
                  MAXMIN=FLOAT(COL2-MAXPEN)
                  DO 1 I=MAXPEN,COL2
                     GREY=FLOAT(I-MAXPEN)/MAXMIN
                     CALL PGSCR(I,GREY,GREY,GREY)
 1                CONTINUE
               END IF
               CALL PGPTEXT (0.5,0.5,0.0,0.5,'PGPLOT imaging')
            ELSE
               CALL PGPTEXT (0.5,0.5,0.0,0.5,'PGPLOT')
            END IF
            CALL PGEND
         END IF
      END IF
C
C     Set appropriate user variable - note this matches command name
C
      CALL VAR_SETCHR(COMMAND,0,0,PLTDEV,STATUS)
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Unable to set '//COMMAND//' variable',STATUS)
         FAULT=.TRUE.
      END IF
C
C     Exit
C
  500 CONTINUE
      IF (FAULT) CALL FIG_SETERR
C
      END
