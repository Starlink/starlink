      SUBROUTINE COCOML (LUOUT,LUECH,LUREP,LUINP2,LUINP1,LUPRO,LUERR)
*+
*  - - - - - - -
*   C O C O M L
*  - - - - - - -
*
*  Conversion of celestial coordinates - main routine.
*
*  Given:
*     LUOUT    i    I/O unit; output; raw output for redirection
*     LUECH    i    I/O unit; output; echo of input
*     LUREP    i    I/O unit; output; report (including error warnings)
*     LUINP2   i    I/O unit; input; commands and data (secondary)
*     LUINP1   i    I/O unit; input; commands and data (primary)
*     LUPRO    i    I/O unit; output; prompts
*     LUERR    i    I/O unit; output; error warnings
*
*  Called:  sla_EPCO, sla_EPJ2D, sla_DRANRM, sla_DRANGE, sla_DTP2S,
*           PROMPT, TRAN, REPSYS, REPRA, REPRES, PAR1, DQP, DP, DR2TN,
*           OPR, KTEST, R2DR
*
*-----------------------------------------------------------------------
*
*  COCO commands:
*
*   I p       specify input coordinate system (p defined below)
*   O p       specify output coordinate system (       "       )
*   F x       specify RA mode:  x = H or D (hours,degrees)
*   R y       select report resolution:  y = H, M or L (high,medium,low)
*   S         display current settings
*   <coords>  perform conversion  (or = to repeat last coordinates)
*   ?         show format of <coords>
*   /<file>   switch to secondary input file <file>
*   H         list the commands
*   E         exit
*
*  Parameters p for I and O commands:
*
*   4 [eq] [ep]   equatorial, FK4 (barycentric)
*   B [eq] [ep]   like FK4 but without E-terms (barycentric)
*   5 [eq] [ep]   equatorial, FK5 (barycentric)
*   A  ep         equatorial, geocentric apparent
*   E  ep         ecliptic (barycentric)
*   G [ep]        galactic (barycentric)
*
*   eq            equinox, e.g. 1950 (optional B or J prefix)
*   ep            epoch, e.g. 1984.53 or 1983 2 26.4
*
*-----------------------------------------------------------------------
*
*  Notes:
*
*    1)  COCO is for use only with sources well outside the solar
*        system.
*
*    2)  Three report resolutions are provided:  L (low), M (medium),
*        and H (high).  The computations are the same irrespective
*        of the report resolution selected.  The accuracy is more than
*        adequate for report resolution 'M', and is good enough for
*        all practical purposes at present.  Resolution 'H' is provided
*        mainly to allow comparison with other predictions and to
*        decrease rounding errors where differences are taken.
*
*    3)  The raw output file consists only of positions, expressed
*        to a fixed but high resolution.  It is free from extraneous
*        prompting, logging and error messages in order to facilitate
*        subsequent processing by other programs.
*
*    4)  COCO is not intended for the conversion of catalogue data,
*        and reports positions only - updated proper motions etc
*        are not reported.  Full conversion of catalogue data is
*        possible using some of COCO's component subprograms, which
*        can be found in the SLALIB library.
*
*  P T Wallace   Starlink   5 November 1996
*-

      IMPLICIT NONE

      INTEGER LUOUT,LUECH,LUREP,LUINP2,LUINP1,LUPRO,LUERR

*  External functions
      DOUBLE PRECISION sla_EPCO,sla_EPJ2D,sla_DRANRM,sla_DRANGE
      INTEGER KTEST
      DOUBLE PRECISION R2DR

*  Degrees, seconds, arcseconds to radians, pi/2
      DOUBLE PRECISION D2R,S2R,AS2R,PIBY2
      PARAMETER (D2R=0.1745329251994329577D-1,
     :           S2R=0.7272205216643039849D-4,
     :          AS2R=0.4848136811095359949D-5,
     :         PIBY2=3.1415926535897932385D0/2D0)

* 'Ending' flag
      LOGICAL FINISH

*  I/O unit for input
      INTEGER LUINP

*  Input buffer, processed version, and pointers
      INTEGER LENBUF
      PARAMETER (LENBUF=72)
      CHARACTER INBUF*(LENBUF),INREC*(LENBUF)
      INTEGER IPTR,IPTR0

*  Last coordinates processed
      CHARACTER COORDS*(LENBUF)

*  Comment and length
      CHARACTER COMM*(LENBUF)
      INTEGER NCOMM

*
*  Current control parameters
*
*  (each pair is input,output; index is INOUT)
      INTEGER INOUT
*  Coordinate system (names correspond to commands)
      CHARACTER KSYS(2)
*  B or J for equinox
      CHARACTER JBEQ(2)
*  Equinox
      DOUBLE PRECISION EQ(2)
*  B or J for epoch
      CHARACTER JBEP(2)
*  Epoch
      DOUBLE PRECISION EP(2)
*  RA format
      CHARACTER KRA
*  Report resolution
      CHARACTER KRES
*

*  Decode arrays
      DOUBLE PRECISION F(10)
      INTEGER JF(10)

*  Spherical coordinates, annual derivatives, parallax, rv
      DOUBLE PRECISION A,B,AW,BW,DA,DB,PX,RV

*  'Proper motions supplied' flag:  1 = not supplied
      INTEGER JZ

*  Direction cosines
      DOUBLE PRECISION V(3)

*  Arrays for mixed radix formatting
      INTEGER IFA(4),IFB(4),IFD(4)

*  Miscellaneous
      CHARACTER KSYST,FROMTO*5,KSIGN

      INTEGER J,I,N

      DOUBLE PRECISION W





*  Announcement
      WRITE (LUREP,'(/1X,''*  Celestial Coordinate Conversions''//)')

*  Set defaults:
*  input FK4, output FK5, RA hours, medium resolution, primary input
      KSYS(1)='4'
      JBEQ(1)='B'
      EQ(1)=1950D0
      JBEP(1)='B'
      EP(1)=1950D0
      KSYS(2)='5'
      JBEQ(2)='J'
      EQ(2)=2000D0
      JBEP(2)='J'
      EP(2)=2000D0
      KRA='H'
      KRES='M'
      LUINP=LUINP1
      COORDS=' '

*
*  Main loop
*  ---------

      FINISH=.FALSE.
      DO WHILE (.NOT.FINISH)

*     Prompt, read and echo the next command
         IF (LUINP.EQ.LUINP1) CALL PROMPT(LUPRO,'<- ',.TRUE.)
         READ (LUINP,'(A)',END=100) INBUF
         IF (LUINP.EQ.LUINP2) CALL PROMPT(LUPRO,'<- '//INBUF,.FALSE.)
         WRITE (LUECH,'(1X,A)') INBUF
         GO TO 200

*     End of input file detected
 100     CONTINUE
         INBUF='E'

*     Remove TABs and fold to upper case
 200     CONTINUE
         CALL TRAN(INBUF,INREC)

*     Strip comments
         IPTR=INDEX(INREC,'*')
         IF (IPTR.GT.0) THEN
            INREC(IPTR:)=' '
            COMM=INBUF(IPTR:)
            NCOMM=LENBUF
            DO WHILE (NCOMM.GT.1.AND.COMM(NCOMM:NCOMM).EQ.' ')
               NCOMM=NCOMM-1
            END DO
         ELSE
            COMM=' '
            NCOMM=1
         END IF

*     Find command
         IPTR=1
         DO WHILE (IPTR.LT.LEN(INREC).AND.
     :             INREC(IPTR:IPTR).EQ.' ')
            IPTR=IPTR+1
         END DO

*     Special case - allow HELP command
         IF (INREC(MIN(IPTR,LENBUF):
     :             MIN(IPTR+3,LENBUF)).EQ.'HELP') THEN
            INREC='H'
            IPTR=1
         END IF

*     Identify the command
         IF (INREC(IPTR:IPTR).EQ.'I'.OR.
     :       INREC(IPTR:IPTR).EQ.'O') THEN

*
*        I (input) or O (output) command
*        -------------------------------

*        Set array index for storage of parameters
            IF (INREC(IPTR:IPTR).EQ.'I') THEN
               INOUT=1
            ELSE
               INOUT=2
            END IF

*     Ensure single character command
         IF (KTEST(INREC,IPTR+1).LT.0) GO TO 9000

*        Look for a 1-character parameter
            CALL PAR1(INREC,IPTR,J)
            IF (J.LT.0) GO TO 9000
            KSYST=INREC(IPTR:IPTR)
            IF (KTEST(INREC,IPTR+1).LT.0) GO TO 9000

*        Validate
            IF (KSYST.NE.'4'.AND.
     :          KSYST.NE.'B'.AND.
     :          KSYST.NE.'5'.AND.
     :          KSYST.NE.'A'.AND.
     :          KSYST.NE.'E'.AND.
     :          KSYST.NE.'G') GO TO 9000

*        Point past the parameter
            IPTR=IPTR+1

*        Decode parameters according to coordinate system
            IF (KSYST.EQ.'4') THEN
               CALL DQP(INREC,IPTR,'B',1950D0,
     :                  JBEQ(INOUT),EQ(INOUT),
     :                  JBEP(INOUT),EP(INOUT),J)
            ELSE IF (KSYST.EQ.'B') THEN
               CALL DQP(INREC,IPTR,'B',1950D0,
     :                  JBEQ(INOUT),EQ(INOUT),
     :                  JBEP(INOUT),EP(INOUT),J)
            ELSE IF (KSYST.EQ.'5') THEN
               CALL DQP(INREC,IPTR,'J',2000D0,
     :                  JBEQ(INOUT),EQ(INOUT),
     :                  JBEP(INOUT),EP(INOUT),J)
            ELSE IF (KSYST.EQ.'A') THEN
               CALL DP(INREC,IPTR,
     :                 '!',0D0,JBEP(INOUT),EP(INOUT),J)
            ELSE IF (KSYST.EQ.'E') THEN
               CALL DP(INREC,IPTR,
     :                 '!',0D0,JBEP(INOUT),EP(INOUT),J)
            ELSE
               CALL DP(INREC,IPTR,
     :                 'B',1950D0,JBEP(INOUT),EP(INOUT),J)
            END IF
            IF (J.EQ.2) GO TO 9000

*        Store the coordinate system parameter
            KSYS(INOUT)=KSYST

*        Report
            IF (INOUT.EQ.1) THEN
               FROMTO='from:'
            ELSE
               FROMTO='to:'
            END IF
            WRITE (LUREP,'(/4X,''Conversion is '',A)') FROMTO
            CALL REPSYS(LUREP,KRES,KSYS(INOUT),JBEQ(INOUT),EQ(INOUT),
     :                                    JBEP(INOUT),EP(INOUT))
            WRITE (LUREP,'(1X)')

         ELSE IF (INREC(IPTR:IPTR).EQ.'F') THEN

*
*        F (RA format) command
*        ---------------------

*     Ensure single character command
         IF (KTEST(INREC,IPTR+1).LT.0) GO TO 9000

*        Get a single 1-character parameter
            CALL PAR1(INREC,IPTR,J)
            IF (J.NE.0) GO TO 9000

*        Validate it
            IF (INREC(IPTR:IPTR).NE.'H'.AND.
     :          INREC(IPTR:IPTR).NE.'D') GO TO 9000

*        Save and report it
            KRA=INREC(IPTR:IPTR)
            CALL REPRA(LUREP,KRA)
            WRITE (LUREP,'(1X)')

         ELSE IF (INREC(IPTR:IPTR).EQ.'R') THEN

*
*        R (resolution) command
*        ----------------------

*     Ensure single character command
         IF (KTEST(INREC,IPTR+1).LT.0) GO TO 9000

*        Get a single 1-character parameter
            CALL PAR1(INREC,IPTR,J)
            IF (J.NE.0) GO TO 9000

*        Validate it
            IF (INREC(IPTR:IPTR).NE.'H'.AND.
     :          INREC(IPTR:IPTR).NE.'M'.AND.
     :          INREC(IPTR:IPTR).NE.'L') GO TO 9000

*        Save and report it
            KRES=INREC(IPTR:IPTR)
            CALL REPRES(LUREP,KRES)

         ELSE IF (INREC(IPTR:IPTR).EQ.'?') THEN

*
*        ? (show format) command
*        -----------------------

*        Ensure rest of record is blank
            IF (KTEST(INREC,IPTR+1).NE.0) GO TO 9000

*        Output the appropriate report
            WRITE (LUREP,'(/4X,''Input format:''/)')
            IF (KSYS(1).EQ.'4'.OR.
     :          KSYS(1).EQ.'B') THEN
               IF (KRA.EQ.'H') THEN
                  WRITE (LUREP,
     :  '(4X,''    RA     Dec       PM       Px    RV'')')
                  WRITE (LUREP,
     :  '(4X,''   h m s  d '''' "  [s/y  "/y    ["  [km/s]]]'')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h m s  d '''''')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h m    d '''''')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h      d''/)')
                  WRITE (LUREP,
     :  '(4X,'' (proper motions default to zero in FK5)''/)')
               ELSE
                  WRITE (LUREP,
     :  '(4X,''    RA     Dec'')')
                  WRITE (LUREP,
     :  '(4X,''     d      d''/)')
               END IF
            ELSE IF (KSYS(1).EQ.'5') THEN
               IF (KRA.EQ.'H') THEN
                  WRITE (LUREP,
     :  '(4X,''    RA     Dec       PM       Px    RV'')')
                  WRITE (LUREP,
     :  '(4X,''   h m s  d '''' "  [s/y  "/y    ["  [km/s]]]'')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h m s  d '''''')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h m    d '''''')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h      d''/)')
               ELSE
                  WRITE (LUREP,
     :  '(4X,''    RA     Dec'')')
                  WRITE (LUREP,
     :  '(4X,''     d      d''/)')
               END IF
            ELSE IF (KSYS(1).EQ.'A') THEN
               IF (KRA.EQ.'H') THEN
                  WRITE (LUREP,
     :  '(4X,''    RA     Dec'')')
                  WRITE (LUREP,
     :  '(4X,''   h m s  d '''' "'')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h m s  d '''''')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h m    d '''''')')
                  WRITE (LUREP,
     :  '(4X,''or'')')
                  WRITE (LUREP,
     :  '(4X,''   h      d''/)')
               ELSE
                  WRITE (LUREP,
     :  '(4X,''    RA     Dec'')')
                  WRITE (LUREP,
     :  '(4X,''     d      d''/)')
               END IF
            ELSE IF (KSYS(1).EQ.'E') THEN
               WRITE (LUREP,
     :  '(4X,''   lambda  beta'')')
               WRITE (LUREP,
     :  '(4X,''      d      d''/)')
            ELSE
               WRITE (LUREP,
     :  '(4X,''   L2   B2'')')
                  WRITE (LUREP,
     :  '(4X,''    d    d''/)')
            END IF

         ELSE IF (INREC(IPTR:IPTR).EQ.'S') THEN

*
*        S (settings) command
*        --------------------

*        Ensure rest of record is blank
            IF (KTEST(INREC,IPTR+1).NE.0) GO TO 9000

*        Report settings
            WRITE (LUREP,'(/4X,''Conversion is from:'')')
            CALL REPSYS(LUREP,KRES,KSYS(1),JBEQ(1),EQ(1),JBEP(1),EP(1))
            WRITE (LUREP,'(4X,''to:'')')
            CALL REPSYS(LUREP,KRES,KSYS(2),JBEQ(2),EQ(2),JBEP(2),EP(2))
            CALL REPRA(LUREP,KRA)
            CALL REPRES(LUREP,KRES)

         ELSE IF (INREC(IPTR:IPTR).EQ.'/') THEN

*
*        / (switch to secondary input file) command
*        ------------------------------------------

*        Command invalid from secondary input file
            IF (LUINP.EQ.LUINP2) GO TO 9000

*        Push pointer past command
            IPTR=IPTR+1

*        Error if no argument
            IF (KTEST(INREC,IPTR).EQ.0) GO TO 9000

*        Find start of filename
            DO WHILE (IPTR.LT.LEN(INREC).AND.
     :                INREC(IPTR:IPTR).EQ.' ')
               IPTR=IPTR+1
            END DO
            IPTR0=IPTR

*        Find end of filename
            DO WHILE (IPTR.LT.LENBUF.AND.
     :                INREC(IPTR:IPTR).NE.' ')
               IPTR=IPTR+1
            END DO
            IF (INREC(IPTR:IPTR).NE.' ') IPTR=IPTR-1

*        Ensure rest of record is blank
            IF (KTEST(INREC,IPTR+1).NE.0) GO TO 9000

*        Switch to secondary input file (INBUF not INREC for lowercase)
            CALL OPR(LUINP2,INBUF(IPTR0:IPTR),J)
            IF (J.EQ.0) THEN
               LUINP=LUINP2
            ELSE
               WRITE (LUREP,'(1X,''   <'',A,''> ?'')') INBUF(IPTR0:IPTR)
               WRITE (LUERR,
     :  '(1X,A/''**** bad filename <'',A,''> ****'')') INBUF(IPTR0:IPTR)
            END IF

         ELSE IF (INREC(IPTR:IPTR).EQ.'H') THEN

*
*        H (help) command
*        ----------------

*        Ensure rest of record is blank
            IF (KTEST(INREC,IPTR+1).NE.0) GO TO 9000

*        Output the help information
            WRITE (LUREP,'(//1X,''Commands:'')')
            WRITE (LUREP,
     :  '(1X,'' I p       specify input coordinate'//
     :                                ' system (p defined below)'')')
            WRITE (LUREP,
     :  '(1X,'' O p       specify output coordinate'//
     :                                ' system (       "       )'')')
            WRITE (LUREP,
     :  '(1X,'' F x       specify RA mode:'//
     :                             '  x = H or D (hours,degrees)'')')
            WRITE (LUREP,
     :  '(1X,'' R x       select report resolution:'//
     :                        '  x = H, M or L (high,medium,low)'')')
            WRITE (LUREP,
     :  '(1X,'' S         display current settings'')')
            WRITE (LUREP,
     :  '(1X,'' <coords>  perform conversion'//
     :                      '  (or = to repeat last coordinates)'')')
            WRITE (LUREP,
     :  '(1X,'' ?         show format of <coords>'')')
            WRITE (LUREP,
     :  '(1X,'' /<file>   switch to secondary input file <file>'')')
            WRITE (LUREP,
     :  '(1X,'' H         list the commands'')')
            WRITE (LUREP,
     :  '(1X,'' E         exit'')')
            WRITE (LUREP,
     :  '(/1X,''Parameters p for I and O commands:'')')
            WRITE (LUREP,
     :  '(1X,'' 4 [eq] [ep]   equatorial, FK4 (barycentric)'')')
            WRITE (LUREP,
     :  '(1X,'' B [eq] [ep]   like FK4 but without'//
     :                                    ' E-terms (barycentric)'')')
            WRITE (LUREP,
     :  '(1X,'' 5 [eq] [ep]   equatorial, FK5 (barycentric)'')')
            WRITE (LUREP,
     :  '(1X,'' A  ep         equatorial, geocentric apparent'')')
            WRITE (LUREP,
     :  '(1X,'' E  ep         ecliptic (barycentric)'')')
            WRITE (LUREP,
     :  '(1X,'' G [ep]        galactic (barycentric)'')')
            WRITE (LUREP,
     :  '(/1X,'' eq            equinox, e.g. 1950'//
     :                                ' (optional B or J prefix)'')')
            WRITE (LUREP,
     :  '(1X,'' ep            epoch, e.g. 1984.53 or 1983 2 26.4''/)')

         ELSE IF (INREC(IPTR:IPTR).EQ.'E') THEN

*
*        E (end) command
*        ---------------

*        Ensure rest of record is blank
            IF (KTEST(INREC,IPTR+1).NE.0) GO TO 9000

*        If primary input set exit flag
            IF (LUINP.NE.LUINP2) THEN
               FINISH=.TRUE.

*        IF secondary input switch back to primary
            ELSE
               CLOSE (LUINP)
               LUINP=LUINP1
            END IF

         ELSE IF (INREC.NE.' ') THEN

*
*        Possibly coordinate record
*        --------------------------

*        If '=', copy last coordinates
            IF (INREC(IPTR:IPTR).EQ.'=') THEN
               IF (KTEST(INREC,IPTR+1).NE.0) GO TO 9000
               INREC=COORDS
            END IF

*        Attempt appropriate decode
            IF (KSYS(1).EQ.'4'.OR.
     :          KSYS(1).EQ.'B'.OR.
     :          KSYS(1).EQ.'5'.OR.
     :          KSYS(1).EQ.'A') THEN

*           Decode RA,Dec

*           Initialise count of fields
               N=0

*           Decode up to 10 fields
               DO I=1,10

*              Decode next field, defaulting to zero
                  W=0D0
                  CALL sla_DFLTIN(INREC,IPTR,W,J)
                  F(I)=W

*              Allow special "arcsec" notation for proper motions
                  IF ((I.EQ.7.OR.I.EQ.8).AND.
     :                INREC(IPTR:IPTR).EQ.'"') THEN
                     J=-2
                     IPTR=IPTR+1
                  END IF
                  JF(I)=J

*              Examine status
                  IF (J.GT.1) THEN

*                 If bad, reject record
                     GO TO 9000

                  ELSE IF (J.NE.1) THEN

*                 If present, update count
                     N=I

                  END IF

*              Next field
               END DO

*           Check nothing left in record
               IF (KTEST(INREC,IPTR+1).NE.0) GO TO 9000

*           Examine number of fields
               IF (N.LT.2) THEN

*              Error if fewer than 2 fields
                  GO TO 9000

               ELSE IF (N.EQ.2) THEN

*              If 2 fields interpret as h d or d d
                  JF(6)=0
                  JF(5)=0
                  F(4)=F(2)
                  JF(4)=JF(2)
                  JF(3)=0
                  F(2)=0D0
                  JF(2)=0
                  N=6

               ELSE IF (KRA.NE.'H') THEN

*              Error if RA in degrees and not 2 fields
                  GO TO 9000

               ELSE IF (N.EQ.3) THEN

*              Error if 3 fields
                  GO TO 9000

               ELSE IF (N.EQ.4) THEN

*              If 4 fields interpret as h m d '
                  JF(6)=0
                  F(5)=F(4)
                  JF(5)=JF(4)
                  F(4)=F(3)
                  JF(4)=JF(3)
                  F(3)=0D0
                  JF(3)=0
                  N=6

               ELSE IF (N.EQ.5) THEN

*              Otherwise interpret as h m s d '
                  JF(6)=0
                  N=6

               END IF

*           Elementary validation
               IF (JF(1).NE.0.OR.
     :             JF(2).NE.0.OR.
     :             JF(3).NE.0.OR.
     :             JF(4).GT.0.OR.
     :             JF(5).NE.0.OR.
     :             JF(6).NE.0.OR.
     :             N.EQ.7.OR.
     :             ABS(F(7)).GE.15D0.OR.
     :             ABS(F(8)).GE.15D0.OR.
     :             JF(9).LT.0.OR.
     :             F(9).GE.1D0.OR.
     :             ABS(F(10)).GE.200D0.OR.
     :             (KSYS(1).EQ.'A'.AND.N.GT.6)) GO TO 9000

*           Convert fields to internal forms
               A=(60D0*(60D0*F(1)+F(2))+F(3))*S2R
               B=(60D0*(60D0*ABS(F(4))+F(5))+F(6))*AS2R
               IF (JF(4).LT.0) B=-B
               JZ=JF(7)
               DA=F(7)*S2R
               DB=F(8)*AS2R
               PX=F(9)
               RV=F(10)

*           Adjust if RA in degrees
               IF (KRA.NE.'H') A=A/15D0

*           Special handling for proper motion in RA specified in arcsec
               IF (JZ.EQ.-2) THEN
                  CALL sla_DTP2S(DA/15D0,DB,0D0,B,DA,DB)
                  DA=sla_DRANGE(DA)
                  DB=DB-B
               ELSE
                  IF (COS(A)*ABS(DA)/AS2R.GE.15D0) GO TO 9000
               END IF

            ELSE

*           Decode ecliptic or galactic coordinates
               CALL sla_DFLTIN(INREC,IPTR,W,J)
               IF (J.GT.0) GO TO 9000
               A=W*D2R
               CALL sla_DFLTIN(INREC,IPTR,W,J)
               IF (J.GT.0) GO TO 9000
               B=W*D2R
               IF (ABS(W).GT.90D0) GO TO 9000

            END IF

*        Normalise
            CALL sla_DCS2C(A,B,V)
            CALL sla_DCC2S(V,A,B)
            A=sla_DRANRM(A)

*        Copy
            AW=A
            BW=B

*        Check nothing left in record
            IF (KTEST(INREC,IPTR+1).NE.0) GO TO 9000

*        Coordinates OK:  copy
            COORDS=INREC

*        Report & convert to J2000 FK5 at specified epoch
            IF (KSYS(1).EQ.'4') THEN

*           FK4

*           If RA hours, report RA,Dec,Equ,Ep,Px,RV

               IF (KRA.EQ.'H') THEN

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL DR2TN(1,A,KSIGN,IFA)
                     CALL sla_DR2AF(0,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I1,1X,A,I2.2,2I3.2,'//
     :  '2(2X,A,F6.1),''  FK4'',F6.2,F7.1)')
     :                                       IFA,KSIGN,
     :                                       (IFB(N),N=1,3),
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL DR2TN(3,A,KSIGN,IFA)
                     CALL sla_DR2AF(2,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I3.3,1X,A,I2.2,2I3.2,''.'',I2.2,'//
     :  '2(2X,A,F7.2),''  FK4'',F7.3,F7.1)')
     :                                       IFA,KSIGN,IFB,
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV

                  ELSE

*                 High resolution
                     CALL DR2TN(5,A,KSIGN,IFA)
                     CALL sla_DR2AF(4,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I5.5,1X,A,I2.2,2I3.2,''.'',I4.4,'//
     :  '2(2X,A,F8.3),''  FK4'',F9.5,F9.3)')
     :                                       IFA,KSIGN,IFB,
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV
                  END IF

                  IF (JZ.NE.1) THEN

*                 Report proper motions
                     IF (KRES.EQ.'L') THEN

*                    Low resolution
                        WRITE (LUREP,'(1X,SP,F15.3,F11.2)')
     :                                                    DA/S2R,DB/AS2R

                     ELSE IF (KRES.EQ.'M') THEN

*                    Medium resolution
                        WRITE (LUREP,'(1X,SP,F17.5,F13.4)')
     :                                                    DA/S2R,DB/AS2R

                     ELSE

*                    High resolution
                        WRITE (LUREP,'(1X,SP,F17.5,F15.4)')
     :                                                    DA/S2R,DB/AS2R

                     END IF

                  END IF

               ELSE

*              If RA degrees, report RA,Dec,Equ,Ep

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     WRITE (LUREP,
     :              '(/1X,'' ='',F7.2,F8.2,1X,2(2X,A,F6.1),''   FK4'')')
     :                                  R2DR(2,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     WRITE (LUREP,
     :  '(/1X,'' ='',F10.5,F11.5,1X,2(2X,A,F7.2),''   FK4'')')
     :                                  R2DR(5,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)

                  ELSE

*                 High resolution
                     WRITE (LUREP,
     :  '(/1X,'' ='',F13.8,F14.8,1X,2(2X,A,F8.3),''   FK4'')')
     :                                  R2DR(8,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)

                  END IF

               END IF

*           Convert to J2000 FK5 at final epoch

               IF (JZ.NE.1) THEN

*              Proper motions supplied

*              Proper motion initial to final epoch
                  CALL sla_PM(A,B,DA,DB,PX,RV,
     :                        sla_EPCO('B',JBEP(1),EP(1)),
     :                        sla_EPCO('B',JBEP(2),EP(2)),AW,BW)

*              Remove E-terms
                  CALL sla_SUBET(AW,BW,
     :                           sla_EPCO('B',JBEQ(1),EQ(1)),A,B)

*              Precess to B1950
                  CALL sla_PRECES('FK4',sla_EPCO('B',JBEQ(1),EQ(1)),
     :                        1950D0,A,B)

*              Add E-terms to make B1950 FK4 position
                  CALL sla_ADDET(A,B,1950D0,AW,BW)

*              Conversion to J2000 FK5 without proper motion
                  CALL sla_FK45Z(AW,BW,
     :                           sla_EPCO('B',JBEP(2),EP(2)),
     :                           A,B)

               ELSE

*              Proper motions not supplied

*              Remove E-terms
                  CALL sla_SUBET(AW,BW,
     :                           sla_EPCO('B',JBEQ(1),EQ(1)),A,B)

*              Precess to B1950
                  CALL sla_PRECES('FK4',sla_EPCO('B',JBEQ(1),EQ(1)),
     :                           1950D0,A,B)

*              Add E-terms to make FK4 position
                  CALL sla_ADDET(A,B,1950D0,AW,BW)

*              Convert to J2000 FK5 without proper motion
                  CALL sla_FK45Z(AW,BW,
     :                           sla_EPCO('B',JBEP(1),EP(1)),
     :                           A,B)

               END IF

            ELSE IF (KSYS(1).EQ.'B') THEN

*           BN

            IF (KRA.EQ.'H') THEN

*              If RA hours, report RA,Dec,Equ,Ep,Px,RV

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL DR2TN(1,A,KSIGN,IFA)
                     CALL sla_DR2AF(0,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I1,1X,A,I2.2,2I3.2,'//
     :  '2(2X,A,F6.1),''  (no E-terms)'',F6.2,F7.1)')
     :                                       IFA,KSIGN,
     :                                       (IFB(N),N=1,3),
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL DR2TN(3,A,KSIGN,IFA)
                     CALL sla_DR2AF(2,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I3.3,1X,A,I2.2,2I3.2,''.'',I2.2,'//
     :  '2(2X,A,F7.2),''  (no E-terms)'',F7.3,F7.1)')
     :                                       IFA,KSIGN,IFB,
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV

                  ELSE

*                 High resolution
                     CALL DR2TN(5,A,KSIGN,IFA)
                     CALL sla_DR2AF(4,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I5.5,1X,A,I2.2,2I3.2,''.'',I4.4,'//
     :  '2(2X,A,F8.3),''  (no E-terms)'',F9.5,F9.3)')
     :                                       IFA,KSIGN,IFB,
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV

                  END IF

                  IF (JZ.NE.1) THEN

*                 Report proper motions
                     IF (KRES.EQ.'L') THEN

*                    Low resolution
                        WRITE (LUREP,'(1X,SP,F15.3,F11.2)')
     :                                                    DA/S2R,DB/AS2R

                     ELSE IF (KRES.EQ.'M') THEN

*                    Medium resolution
                        WRITE (LUREP,'(1X,SP,F17.5,F13.4)')
     :                                                    DA/S2R,DB/AS2R

                     ELSE

*                    High resolution
                        WRITE (LUREP,'(1X,SP,F17.5,F15.4)')
     :                                                    DA/S2R,DB/AS2R

                     END IF

                  END IF

               ELSE

*              If RA degrees, report RA,Dec,Equ,Ep

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     WRITE (LUREP,
     :  '(/1X,''  ='',F7.2,F8.2,1X,'//
     :  '2(2X,A,F6.1),''   (no E-terms)'')')
     :                                  R2DR(2,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     WRITE (LUREP,
     :  '(/1X,'' ='',F10.5,F11.5,1X,'//
     :  '2(2X,A,F7.2),''   (no E-terms)'')')
     :                                  R2DR(5,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)

                  ELSE

*                 High resolution
                     WRITE (LUREP,
     :  '(/1X,'' ='',F13.8,F14.8,1X,'//
     :  '2(2X,A,F8.3),''   (no E-terms)'')')
     :                                  R2DR(8,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)

                  END IF

               END IF

*           Convert to J2000 FK5 at final epoch

               IF (JZ.NE.1) THEN

*              Proper motions supplied

*              Proper motion initial to final epoch
                  CALL sla_PM(AW,BW,DA,DB,PX,RV,
     :                        sla_EPCO('B',JBEP(1),EP(1)),
     :                        sla_EPCO('B',JBEP(2),EP(2)),A,B)

*              Precess to B1950
                  CALL sla_PRECES('FK4',sla_EPCO('B',JBEQ(1),EQ(1)),
     :                        1950D0,A,B)

*              Add E-terms to make FK4 position
                  CALL sla_ADDET(A,B,1950D0,AW,BW)

*              Conversion to J2000 FK5 without proper motion
                  CALL sla_FK45Z(AW,BW,
     :                           sla_EPCO('B',JBEP(2),EP(2)),
     :                           A,B)

               ELSE

*              Proper motions not supplied

*              Precess to B1950
                  CALL sla_PRECES('FK4',sla_EPCO('B',JBEQ(1),EQ(1)),
     :                           1950D0,A,B)

*              Add E-terms to make FK4 position
                  CALL sla_ADDET(A,B,1950D0,AW,BW)

*              Convert to J2000 FK5 without proper motion
                  CALL sla_FK45Z(AW,BW,
     :                           sla_EPCO('B',JBEP(1),EP(1)),
     :                           A,B)

               END IF

            ELSE IF (KSYS(1).EQ.'5') THEN

*           FK5

               IF (KRA.EQ.'H') THEN

*              If RA hours, report RA,Dec,Equ,Ep,Px,RV and PM

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL DR2TN(1,A,KSIGN,IFA)
                     CALL sla_DR2AF(0,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I1,1X,A,I2.2,2I3.2,'//
     :  '2(2X,A,F6.1),''  FK5'',F6.2,F7.1)')
     :                                       IFA,KSIGN,
     :                                       (IFB(N),N=1,3),
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL DR2TN(3,A,KSIGN,IFA)
                     CALL sla_DR2AF(2,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I3.3,1X,A,I2.2,2I3.2,''.'',I2.2,'//
     :  '2(2X,A,F7.2),''  FK5'',F7.3,F7.1)')
     :                                       IFA,KSIGN,IFB,
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV

                  ELSE

*                 High resolution
                     CALL DR2TN(5,A,KSIGN,IFA)
                     CALL sla_DR2AF(4,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I5.5,1X,A,I2.2,2I3.2,''.'',I4.4,'//
     :  '2(2X,A,F8.3),''  FK5'',F9.5,F9.3)')
     :                                       IFA,KSIGN,IFB,
     :                                       JBEQ(1),EQ(1),
     :                                       JBEP(1),EP(1),
     :                                       PX,RV

                  END IF

                  IF (JZ.NE.1) THEN

*                 Report proper motions
                     IF (KRES.EQ.'L') THEN

*                    Low resolution
                        WRITE (LUREP,'(1X,SP,F15.3,F11.2)')
     :                                                    DA/S2R,DB/AS2R

                     ELSE IF (KRES.EQ.'M') THEN

*                    Medium resolution
                        WRITE (LUREP,'(1X,SP,F17.5,F13.4)')
     :                                                    DA/S2R,DB/AS2R

                     ELSE

*                    High resolution
                        WRITE (LUREP,'(1X,SP,F17.5,F15.4)')
     :                                                    DA/S2R,DB/AS2R

                     END IF

                  END IF

               ELSE

*              If RA degrees, report RA,Dec,Equ,Ep

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     WRITE (LUREP,
     :  '(/1X,'' ='',F7.2,F8.2,1X,2(2X,A,F6.1),''   FK5'')')
     :                                  R2DR(2,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     WRITE (LUREP,
     :  '(/1X,'' ='',F10.5,F11.5,1X,2(2X,A,F7.2),''   FK5'')')
     :                                  R2DR(5,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)


                  ELSE

*                 High resolution
                     WRITE (LUREP,
     :  '(/1X,'' ='',F13.8,F14.8,1X,2(2X,A,F8.3),''   FK5'')')
     :                                  R2DR(8,A),B/D2R,
     :                                  JBEQ(1),EQ(1),
     :                                  JBEP(1),EP(1)


                  END IF

               END IF

*           Convert to J2000 FK5 at final epoch

*           Proper motion initial to final epoch
               CALL sla_PM(AW,BW,DA,DB,PX,RV,
     :                     sla_EPCO('J',JBEP(1),EP(1)),
     :                     sla_EPCO('J',JBEP(2),EP(2)),
     :                     A,B)

*           Precession to J2000
               CALL sla_PRECES('FK5',sla_EPCO('J',JBEQ(1),EQ(1)),
     :                     2000D0,A,B)

            ELSE IF (KSYS(1).EQ.'A') THEN

*           Apparent

*           Report RA,Dec,Ep

               IF (KRA.EQ.'H') THEN

*              RA is in hours

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL sla_DJCAL(0,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(1),EP(1))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     CALL DR2TN(1,A,KSIGN,IFA)
                     CALL sla_DR2AF(0,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I1,1X,A,I2.2,2I3.2,'//
     :  '1X,I5,2I3.2,''  geocentric apparent'')')
     :                              IFA,KSIGN,
     :                              (IFB(N),N=1,3),
     :                              (IFD(N),N=1,3)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL sla_DJCAL(1,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(1),EP(1))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     CALL DR2TN(3,A,KSIGN,IFA)
                     CALL sla_DR2AF(2,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I3.3,1X,A,I2.2,2I3.2,''.'',I2.2,'//
     :  '1X,I5,2I3.2,''.'',I1,''  geocentric apparent'')')
     :                              IFA,KSIGN,IFB,IFD

                  ELSE

*                 High resolution
                     CALL sla_DJCAL(3,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(1),EP(1))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     CALL DR2TN(5,A,KSIGN,IFA)
                     CALL sla_DR2AF(4,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(/1X,'' ='',3I3.2,''.'',I5.5,1X,A,I2.2,2I3.2,''.'',I4.4,'//
     :  '1X,I5,2I3.2,''.'',I3.3,''  geocentric apparent'')')
     :                              IFA,KSIGN,IFB,IFD

                  END IF

               ELSE

*           RA is in degrees

               IF (KRES.EQ.'L') THEN

*                 Low resolution

                     CALL sla_DJCAL(0,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(1),EP(1))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     WRITE (LUREP,
     :  '(/1X,'' ='',F7.2,F8.2,2X,I5,2I3.2,'//
     :  '''   geocentric apparent'')')
     :                                  R2DR(2,A),B/D2R,(IFD(N),N=1,3)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL sla_DJCAL(1,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(1),EP(1))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     WRITE (LUREP,
     :  '(/1X,'' ='',F10.5,F11.5,2X,I5,2I3.2,''.'',I1,'//
     :  '''  geocentric apparent'')')
     :                                  R2DR(2,A),B/D2R,IFD

                  ELSE

*                 High resolution
                     CALL sla_DJCAL(3,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(1),EP(1))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     WRITE (LUREP,
     :  '(/1X,'' ='',F13.8,F14.8,2X,I5,2I3.2,''.'',I3.3,'//
     :  '''   geocentric apparent'')')
     :                                  R2DR(2,A),B/D2R,IFD

                  END IF

               END IF

*           Convert to J2000 FK5
               CALL sla_AMP(AW,BW,
     :                      sla_EPJ2D(sla_EPCO('J',JBEP(1),EP(1))),
     :                      2000D0,A,B)

            ELSE IF (KSYS(1).EQ.'E') THEN

*           Ecliptic

*           Report Lambda,Beta,Ep

               IF (KRES.EQ.'L') THEN

*              Low resolution
                  WRITE (LUREP,
     :  '(/1X,'' ='',F7.2,F8.2,2X,A,F6.1,''  ecliptic'')')
     :                           R2DR(2,A),B/D2R,JBEP(1),EP(1)
               ELSE IF (KRES.EQ.'M') THEN
                  WRITE (LUREP,
     :  '(/1X,'' ='',F10.5,F11.5,2X,A,F9.4,''  ecliptic'')')
     :                           R2DR(5,A),B/D2R,JBEP(1),EP(1)
               ELSE
                  WRITE (LUREP,
     :  '(/1X,'' ='',F13.8,F14.8,2X,A,F12.7,''  ecliptic'')')
     :                           R2DR(8,A),B/D2R,JBEP(1),EP(1)
               END IF

*           Convert to J2000 FK5

               CALL sla_ECLEQ(AW,BW,
     :                  sla_EPJ2D(sla_EPCO('J',JBEP(1),EP(1))),
     :                        A,B)

            ELSE

*           Galactic

*           Report L2,B2,Ep

               IF (KRES.EQ.'L') THEN

*              Low resolution
                  WRITE (LUREP,
     :  '(/1X,'' ='',F7.2,F8.2,2X,A,F6.1,''  galactic (II)'')')
     :                           R2DR(2,A),B/D2R,JBEP(1),EP(1)

               ELSE IF (KRES.EQ.'M') THEN

*              Medium resolution
                  WRITE (LUREP,
     :  '(/1X,'' ='',F10.5,F11.5,2X,A,F7.2,''  galactic (II)'')')
     :                           R2DR(5,A),B/D2R,JBEP(1),EP(1)

               ELSE

*              High resolution
                  WRITE (LUREP,
     :  '(/1X,'' ='',F13.8,F14.8,2X,A,F8.3,''  galactic (II)'')')
     :                           R2DR(8,A),B/D2R,JBEP(1),EP(1)

               END IF

*           Convert to J2000 FK5
               CALL sla_GALEQ(AW,BW,A,B)

            END IF

*        Copy
            AW=A
            BW=B

*        Convert from J2000 FK5 to output system & report
            IF (KSYS(2).EQ.'4') THEN

*           FK4

*           Convert to B1950 FK4 without proper motion
               CALL sla_FK54Z(AW,BW,
     :                        sla_EPCO('B',JBEP(2),EP(2)),
     :                        A,B,DA,DB)

*           Remove E-terms
               CALL sla_SUBET(A,B,1950D0,AW,BW)

*           Precess to final equinox
               CALL sla_PRECES('FK4',1950D0,
     :                         sla_EPCO('B',JBEQ(2),EQ(2)),
     :                         AW,BW)

*           Add E-terms
               CALL sla_ADDET(AW,BW,
     :                        sla_EPCO('B',JBEQ(2),EQ(2)),
     :                        A,B)

               IF (KRA.EQ.'H') THEN

*              Report (RA hours)

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL DR2TN(1,A,KSIGN,IFA)
                     CALL sla_DR2AF(0,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I1,1X,A,I2.2,2I3.2,'//
     :  '2(2X,A,F6.1),''  FK4''/)')
     :                                        IFA,KSIGN,
     :                                        (IFB(N),N=1,3),
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL DR2TN(3,A,KSIGN,IFA)
                     CALL sla_DR2AF(2,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I3.3,1X,A,I2.2,2I3.2,''.'',I2.2,'//
     :  '2(2X,A,F7.2),''  FK4''/)')
     :                                        IFA,KSIGN,IFB,
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2)

                  ELSE

*                 High resolution
                     CALL DR2TN(5,A,KSIGN,IFA)
                     CALL sla_DR2AF(4,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I5.5,1X,A,I2.2,2I3.2,''.'',I4.4,'//
     :  '2(2X,A,F8.3),''  FK4''/)')
     :                                        IFA,KSIGN,IFB,
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2)

                  END IF

*              Output
                  CALL DR2TN(4,A,KSIGN,IFA)
                  CALL sla_DR2AF(3,B,KSIGN,IFB)
                  WRITE (LUOUT,
     :  '(1X,I2.2,2I3.2,''.'',I4.4,1X,A,I2.2,2I3.2,''.'',I3.3,'//
     :  '2(2X,A,F7.2),''  FK4  '',A)')
     :                                        IFA,KSIGN,IFB,
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2),
     :                                        COMM(:NCOMM)

               ELSE

*              Report (RA degrees)

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F6.2,F8.2,1X,2(2X,A,F6.1),''   FK4''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F9.5,F11.5,1X,2(2X,A,F7.2),''   FK4''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  ELSE

*                 High resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F12.8,F14.8,1X,2(2X,A,F8.3),''   FK4''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  END IF

*              Output
                  WRITE (LUOUT,
     :  '(1X,F12.7,F13.7,1X,2(2X,A,F7.2),''   FK4  '',A)')
     :                                        A/D2R,B/D2R,
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2),
     :                                        COMM(:NCOMM)


               END IF

            ELSE IF (KSYS(2).EQ.'B') THEN

*           BN

*           Convert to B1950 FK4 without proper motion
               CALL sla_FK54Z(A,B,
     :                        sla_EPCO('B',JBEP(2),EP(2)),
     :                        AW,BW,DA,DB)

*           Remove E-terms
               CALL sla_SUBET(AW,BW,1950D0,A,B)

*           Precess to final equinox
               CALL sla_PRECES('FK4',1950D0,
     :                         sla_EPCO('B',JBEQ(2),EQ(2)),
     :                         A,B)

               IF (KRA.EQ.'H') THEN

*              Report (RA hours)

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL DR2TN(1,A,KSIGN,IFA)
                     CALL sla_DR2AF(0,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I1,1X,A,I2.2,2I3.2,'//
     :  '2(2X,A,F6.1),''  (no E-terms)''/)')
     :                              IFA,KSIGN,
     :                              (IFB(N),N=1,3),
     :                              JBEQ(2),EQ(2),
     :                              JBEP(2),EP(2)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL DR2TN(3,A,KSIGN,IFA)
                     CALL sla_DR2AF(2,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I3.3,1X,A,I2.2,2I3.2,''.'',I2.2,'//
     :  '2(2X,A,F7.2),''  (no E-terms)''/)')
     :                              IFA,KSIGN,IFB,
     :                              JBEQ(2),EQ(2),
     :                              JBEP(2),EP(2)

                  ELSE

*                 High resolution
                     CALL DR2TN(5,A,KSIGN,IFA)
                     CALL sla_DR2AF(4,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I5.5,1X,A,I2.2,2I3.2,''.'',I4.4,'//
     :  '2(2X,A,F8.3),''  (no E-terms)''/)')
     :                              IFA,KSIGN,IFB,
     :                              JBEQ(2),EQ(2),
     :                              JBEP(2),EP(2)

                  END IF

*              Output
                  CALL DR2TN(4,A,KSIGN,IFA)
                  CALL sla_DR2AF(3,B,KSIGN,IFB)
                  WRITE (LUOUT,
     :  '(1X,I2.2,2I3.2,''.'',I4.4,1X,A,I2.2,2I3.2,''.'',I3.3,'//
     :  '2(2X,A,F7.2),''  (no E-terms)  '',A)')
     :                           IFA,KSIGN,IFB,
     :                           JBEQ(2),EQ(2),
     :                           JBEP(2),EP(2),
     :                           COMM(:NCOMM)

               ELSE

*              Report (RA degrees)

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F6.2,F8.2,1X,2(2X,A,F6.1),'//
     :  '''   (no E-terms)''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F9.5,F11.5,1X,2(2X,A,F7.2),'//
     :  '''   (no E-terms)''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  ELSE

*                 High resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F12.8,F14.8,1X,2(2X,A,F8.3),'//
     :  '''   (no E-terms)''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  END IF

*              Output
                  WRITE (LUOUT,
     :  '(1X,F12.7,F13.7,1X,2(2X,A,F7.2),''   (no E-terms)  '',A)')
     :                                        A/D2R,B/D2R,
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2),
     :                                        COMM(:NCOMM)


               END IF

            ELSE IF (KSYS(2).EQ.'5') THEN

*           FK5

*           Precess to final equinox
               CALL sla_PRECES('FK5',2000D0,
     :                         sla_EPCO('J',JBEQ(2),EQ(2)),
     :                         A,B)

               IF (KRA.EQ.'H') THEN

*                 Report (RA hours)

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL DR2TN(1,A,KSIGN,IFA)
                     CALL sla_DR2AF(0,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I1,1X,A,I2.2,2I3.2,'//
     :  '2(2X,A,F6.1),''  FK5''/)')
     :                                       IFA,KSIGN,
     :                                       (IFB(N),N=1,3),
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL DR2TN(3,A,KSIGN,IFA)
                     CALL sla_DR2AF(2,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I3.3,1X,A,I2.2,2I3.2,''.'',I2.2,'//
     :  '2(2X,A,F7.2),''  FK5''/)')
     :                                       IFA,KSIGN,IFB,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  ELSE

*                 High resolution
                     CALL DR2TN(5,A,KSIGN,IFA)
                     CALL sla_DR2AF(4,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I5.5,1X,A,I2.2,2I3.2,''.'',I4.4,'//
     :  '2(2X,A,F8.3),''  FK5''/)')
     :                                        IFA,KSIGN,IFB,
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2)

                  END IF

*              Output
                  CALL DR2TN(4,A,KSIGN,IFA)
                  CALL sla_DR2AF(3,B,KSIGN,IFB)
                  WRITE (LUOUT,
     :  '(1X,I2.2,2I3.2,''.'',I4.4,1X,A,I2.2,2I3.2,''.'',I3.3,'//
     :  '2(2X,A,F7.2),''  FK5  '',A)')
     :                                        IFA,KSIGN,IFB,
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2),
     :                                        COMM(:NCOMM)

               ELSE

*              Report (RA degrees)

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F6.2,F8.2,1X,2(2X,A,F6.1),''   FK5''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F9.5,F11.5,1X,2(2X,A,F7.2),''   FK5''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  ELSE

*                 High resolution
                     WRITE (LUREP,
     :  '(1X,''-> '',F12.8,F14.8,1X,2(2X,A,F8.3),''   FK5''/)')
     :                                       A/D2R,B/D2R,
     :                                       JBEQ(2),EQ(2),
     :                                       JBEP(2),EP(2)

                  END IF

*              Output
                  WRITE (LUOUT,
     :  '(1X,F12.7,F13.7,1X,2(2X,A,F7.2),''   FK5  '',A)')
     :                                        A/D2R,B/D2R,
     :                                        JBEQ(2),EQ(2),
     :                                        JBEP(2),EP(2),
     :                                        COMM(:NCOMM)


               END IF

            ELSE IF (KSYS(2).EQ.'A') THEN

*           Apparent

*           Convert to geocentric apparent
               CALL sla_MAP(AW,BW,0D0,0D0,PX,0D0,2000D0,
     :                      sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                      A,B)

               IF (KRA.EQ.'H') THEN

*                 Report (RA hours)
                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL sla_DJCAL(0,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     CALL DR2TN(1,A,KSIGN,IFA)
                     CALL sla_DR2AF(0,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I1,1X,A,I2.2,2I3.2,'//
     :  '1X,I5,2I3.2,''  geocentric apparent''/)')
     :                              IFA,KSIGN,
     :                              (IFB(N),N=1,3),
     :                              (IFD(N),N=1,3)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL sla_DJCAL(1,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     CALL DR2TN(3,A,KSIGN,IFA)
                     CALL sla_DR2AF(2,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I3.3,1X,A,I2.2,2I3.2,''.'',I2.2,'//
     :  '1X,I5,2I3.2,''.'',I1,''  geocentric apparent''/)')
     :                              IFA,KSIGN,IFB,IFD

                  ELSE

*                 High resolution
                     CALL sla_DJCAL(3,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     CALL DR2TN(5,A,KSIGN,IFA)
                     CALL sla_DR2AF(4,B,KSIGN,IFB)
                     WRITE (LUREP,
     :  '(1X,''->'',3I3.2,''.'',I5.5,1X,A,I2.2,2I3.2,''.'',I4.4,'//
     :  '1X,I5,2I3.2,''.'',I3.3,''  geocentric apparent''/)')
     :                              IFA,KSIGN,IFB,IFD

                  END IF

*              Output
                  CALL sla_DJCAL(2,
     :                       sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                       IFD,J)
                  IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                  CALL DR2TN(4,A,KSIGN,IFA)
                  CALL sla_DR2AF(3,B,KSIGN,IFB)
                  WRITE (LUOUT,
     :  '(1X,I2.2,2I3.2,''.'',I4.4,1X,A,I2.2,2I3.2,''.'',I3.3,'//
     :  '1X,I5,2I3.2,''.'',I2.2,''  geocentric apparent  '',A)')
     :                           IFA,KSIGN,IFB,IFD,COMM(:NCOMM)

               ELSE

*              Report (RA degrees)

                  IF (KRES.EQ.'L') THEN

*                 Low resolution
                     CALL sla_DJCAL(0,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     WRITE (LUREP,
     :  '(1X,''-> '',F6.2,F8.2,2X,I5,2I3.2,'//
     :  '''   geocentric apparent''/)')
     :                                       A/D2R,B/D2R,
     :                                       (IFD(N),N=1,3)

                  ELSE IF (KRES.EQ.'M') THEN

*                 Medium resolution
                     CALL sla_DJCAL(1,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     WRITE (LUREP,
     :  '(1X,''-> '',F9.5,F11.5,2X,I5,2I3.2,''.'',I1,'//
     :  '''   geocentric apparent''/)')
     :                                       A/D2R,B/D2R,IFD

                  ELSE

*                 High resolution
                     CALL sla_DJCAL(3,
     :                          sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                          IFD,J)
                     IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                     WRITE (LUREP,
     :  '(1X,''-> '',F12.8,F14.8,2X,I5,2I3.2,''.'',I3.3,'//
     :  '''   geocentric apparent''/)')
     :                              A/D2R,B/D2R,IFD

                  END IF

*              Output
                  CALL sla_DJCAL(2,
     :                       sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                       IFD,J)
                  IF (J.NE.0) WRITE (LUREP,'(1X,''COCO bug!'')')
                  WRITE (LUOUT,
     :  '(1X,F12.7,F13.7,2X,I5,2I3.2,''.'',I2.2,'//
     :  '''   geocentric apparent  '',A)')
     :                                A/D2R,B/D2R,IFD,COMM(:NCOMM)

               END IF

            ELSE IF (KSYS(2).EQ.'E') THEN

*           Ecliptic

               CALL sla_EQECL(AW,BW,
     :                 sla_EPJ2D(sla_EPCO('J',JBEP(2),EP(2))),
     :                        A,B)

*           Report

               IF (KRES.EQ.'L') THEN

*              Low resolution
                  WRITE (LUREP,
     :  '(1X,''-> '',F6.2,F8.2,2X,A,F6.1,''  ecliptic''/)')
     :                           A/D2R,B/D2R,JBEP(2),EP(2)

               ELSE IF (KRES.EQ.'M') THEN

*              Medium resolution
                  WRITE (LUREP,
     :  '(1X,''-> '',F9.5,F11.5,2X,A,F9.4,''  ecliptic''/)')
     :                           A/D2R,B/D2R,JBEP(2),EP(2)

               ELSE

*              High resolution
                  WRITE (LUREP,
     :  '(1X,''-> '',F12.8,F14.8,2X,A,F12.7,''  ecliptic''/)')
     :                           A/D2R,B/D2R,JBEP(2),EP(2)

               END IF

*           Output
               WRITE (LUOUT,
     :  '(1X,F12.7,F13.7,2X,A,F12.6,''  ecliptic  '',A)')
     :                        A/D2R,B/D2R,JBEP(2),EP(2),COMM(:NCOMM)

            ELSE

*           Galactic

*           Convert to galactic coordinates
               CALL sla_EQGAL(AW,BW,A,B)

*           Report

               IF (KRES.EQ.'L') THEN

*              Low resolution
                  WRITE (LUREP,
     :  '(1X,''-> '',F6.2,F8.2,2X,A,F6.1,''  galactic (II)''/)')
     :                           A/D2R,B/D2R,JBEP(2),EP(2)

               ELSE IF (KRES.EQ.'M') THEN

*              Medium resolution
                  WRITE (LUREP,
     :  '(1X,''-> '',F9.5,F11.5,2X,A,F7.2,''  galactic (II)''/)')
     :                           A/D2R,B/D2R,JBEP(2),EP(2)

               ELSE

*              High resolution
                  WRITE (LUREP,
     :  '(1X,''-> '',F12.8,F14.8,2X,A,F8.3,''  galactic (II)''/)')
     :                           A/D2R,B/D2R,JBEP(2),EP(2)

               END IF

*           Output
               WRITE (LUOUT,
     :  '(1X,F11.7,F13.7,2X,A,F12.6,''  galactic (II)  '',A)')
     :                        A/D2R,B/D2R,JBEP(2),EP(2),COMM(:NCOMM)

            END IF

         END IF

*     Next command
         GO TO 9900

*     Errors
 9000    CONTINUE
         WRITE (LUREP,'(1X,''   ?'')')
         WRITE (LUERR,'(1X,A/1X,''**** data error ****'')') INBUF

*     Next command
 9900    CONTINUE
      END DO

*  Wrap up
      WRITE (LUPRO,'(/)')

      END
