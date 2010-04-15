
      SUBROUTINE CONSTR(NG,GINFP,GINFH,GINFW,CONP,CONH,CONW,
     :                  ICHAINP,ICHAINH,ICHAINW,CHAINP,CHAINH,
     :                  CHAINW)
C
C     C O N S T R
C
C     Handles the interactive set up of the single constraints on
C     the fit parameters for position, height and width and the
C     multiple constraints in the form of chained values
C     for position ( CHAINP ), height ( CHAINH ) and width ( CHAINW ).
C
C     Parameters - (">" input, "<" output )
C
C     (>) NG       (Integer) Number of elements in the GINF. arrays
C     (>) GINFP    (Real array) The array of positions of the fitted
C                  Gaussians
C     (>) GINFH    (Real array) The array of peak heights of the fitted
C                  Gaussians
C     (>) GINFW    (Real array) The array of widths ( sigma ) of the fitted
C                  Gaussians
C     (<) CONP     (Integer array) The array of the index number of the
C                  Gaussians whose peak position is to be fixed
C     (<) CONH     (Integer array) The array of the index number of the
C                  Gaussians whose peak height is to be fixed
C     (<) CONW     (Integer array) The array of the index number of the
C                  Gaussians whose sigma is to be fixed
C     (<) ICHAINP  (Integer array) The array of the chaining index
C                  number of the Gaussians to be chained in position
C     (<) CHAINP   (Real array) The array of values of the line seperations
C                  of lines to be locked
C     (<) ICHAINH  (Integer array) The array of the chaining index number
C                  of the Gaussians to be chained in height
C     (<) CHAINH   (Real array) The array of values of the line height
C                  ratios of the lines to be locked
C     (<) ICHAINW  (Integer array) The array of the chaining index number
C                  of the Gaussians to be chained in width
C     (<) CHAINW   (Real array) The array of values of the line width
C                  ratios of lines to be locked
C
C                                                JRW/ AAO Sept 1987
C
C     Subroutines called:
C        TERMWRIT - writes the parameters of the fitted Gaussians to
C                   the terminal
C
C      1992 Aug 31 Calls to WRUSER changed to PAR_WRUSER.  HME / UoE, Starlink.
C      1994 Dec 20 Fix bug whereby ICHAIN was not cancelled.
C                  HME / UoE, Starlink.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NG,CONP(NG),CONH(NG),CONW(NG),ICHAINP(NG),ICHAINH(NG),
     :ICHAINW(NG)
      REAL GINFP(NG),GINFH(NG),GINFW(NG),CHAINP(NG),CHAINH(NG),
     :CHAINW(NG)
C
C     Functions
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      INTEGER I,J,STAT,IGNORE,INVOKE,I1,NJ,NEW,NCH,ICH,LOW
      REAL VALUE,RUB(10),ORIGP,ORIGH,ORIGW,FMIN,FMAX
      CHARACTER*1 C1,C2
      CHARACTER*16 RUBB,CONSTRING
      CHARACTER*80 OUT
      LOGICAL LALL,LSING,MSING

      PARAMETER ( FMIN=-1.7E38,
     :            FMAX=1.7E38 )
      LALL=.FALSE.  ! Don't print out to terminal EW, rms etc for fit info
C
C     Initialize all integer arrays
C
      DO I=1,NG,1
        CONP(I)=0
        CONH(I)=0
        CONW(I)=0
        ICHAINP(I)=0
        ICHAINH(I)=0
      ENDDO
C
C     Set up any single constraints
C
      CALL PAR_CNPAR('SINCON')
      CALL PAR_RDKEY('SINCON',.FALSE.,LSING)
      IF (LSING) THEN
C
C       Write values of fitted parameters to terminal
C
        CALL TERMWRIT(NG,GINFP,GINFH,GINFW,RUB,RUB,RUB,.FALSE.,
     :                RUBB,RUBB,LALL)
        DO I=1,3*NG,1
10        CALL PAR_WRUSER('Enter number of Gaussian to constrain,'//
     :                    ' ( -1 to end )',STAT)
          CALL PAR_WRUSER(' followed by P for position; H for height'//
     :                    ' or W for width',STAT)
          CALL PAR_CNPAR('NPCON')
          CALL PAR_RDCHAR('NPCON',' ',CONSTRING)
          READ(CONSTRING,11,IOSTAT=IGNORE) I1
11        FORMAT(I2)
C
C         Check integer value is within range 1 to NG and a legitimate
C         parameter
C
          IF (I1.EQ.-1) THEN
C
C           Input of values complete
C
            GO TO 20
          ENDIF
          READ(CONSTRING,12,IOSTAT=IGNORE) I1,C1
12        FORMAT(I1,A1)
          IF (I1.GT.NG) THEN
            CALL PAR_WRUSER(' No such Gaussian',STAT)
            GO TO 10
          ENDIF
          IF (C1.EQ.'P'.OR.C1.EQ.'p') THEN
            CONP(I1)=1
            GO TO 18
          ENDIF
          IF (C1.EQ.'H'.OR.C1.EQ.'h') THEN
            CONH(I1)=1
            GO TO 18
          ENDIF
          IF (C1.EQ.'W'.OR.C1.EQ.'w') THEN
            CONW(I1)=1
            GO TO 18
          ENDIF
          GO TO 10 ! C1 not P, H or W
18        CONTINUE
        END DO
C
C       Report any single constraints at this stage
C
20      DO I=1,NG,1
          IF (CONP(I).EQ.1) THEN
            WRITE(OUT,21) I,GINFP(I)
            CALL PAR_WRUSER(OUT,STAT)
          ENDIF
21        FORMAT(' Gaussian number ',I2,' constrained in position
     : at ',F9.2)
          IF (CONH(I).EQ.1) THEN
            WRITE(OUT,22) I,GINFH(I)
            CALL PAR_WRUSER(OUT,STAT)
          ENDIF
22        FORMAT(' Gaussian number ',I2,' constrained in height
     : at   ',E10.3)
          IF (CONW(I).EQ.1) THEN
            WRITE(OUT,23) I,GINFW(I)
            CALL PAR_WRUSER(OUT,STAT)
          ENDIF
23        FORMAT(' Gaussian number ',I2,' constrained in width
     : at   ',F9.2)
        END DO
      ENDIF
C
C     Now set up any multiple constraints on position or height or
C     width
C
50    CALL PAR_CNPAR('MULTCON')
      CALL PAR_RDKEY('MULTCON',.FALSE.,MSING)
      IF (MSING) THEN
        J=1
53      CALL TERMWRIT(NG,GINFP,GINFH,GINFW,RUB,RUB,RUB,.FALSE.,
     :                RUBB,RUBB,LALL)
C
C       Prompt for number of Gaussians to chain
C
55      CALL PAR_CNPAR('NCHAIN')
        CALL PAR_RDVAL('NCHAIN',-10.,REAL(NG),REAL(NG),' ',VALUE)
        NCH=INT(VALUE)
        IF (NCH.EQ.-1) THEN
C
C         Input of chained values complete
C
          GO TO 90
        ENDIF
        IF (NCH.EQ.1) THEN
          CALL PAR_WRUSER('Cannot chain only one line',STAT)
          GO TO 55
        ENDIF
        IF (NCH.GT.NG) THEN
          CALL PAR_WRUSER('More lines to chain than lines available',
     :                    STAT)
          GO TO 55
        ENDIF
C
C       Find out which parameter to chain
C
60      CALL PAR_CNPAR('CHAIN')
        CALL PAR_RDCHAR('CHAIN',' ',C2)
        INVOKE=ICH_FOLD(C2)
        IF (C2.NE.'P'.AND.C2.NE.'H'.AND.C2.NE.'W') THEN
          CALL PAR_WRUSER('No such parameter',STAT)
          GO TO 60
        ENDIF
        NEW=0
        DO I=1,NCH,1
66        CALL PAR_CNPAR('ICHAIN')
          CALL PAR_RDVAL('ICHAIN',1.,REAL(NG),1.0,' ',VALUE)
          ICH=INT(VALUE)
C
C         Check that this value is not singly constrained
C
          IF (C2.EQ.'P'.AND.CONP(ICH).EQ.1) THEN
            CALL PAR_WRUSER('Position of this Gaussian already'//
     :                      ' constrained',STAT)
            GO TO 66
          ENDIF
          IF (C2.EQ.'H'.AND.CONH(ICH).EQ.1) THEN
            CALL PAR_WRUSER('Height of this Gaussian already'//
     :                      ' constrained',STAT)
            GO TO 66
          ENDIF
          IF (C2.EQ.'W'.AND.CONW(ICH).EQ.1) THEN
            CALL PAR_WRUSER('Width of this Gaussian already'//
     :                      ' constrained',STAT)
            GO TO 66
          ENDIF
C
C         Set the constraints on the positions as increments on the
C         first constrained line peak in the chain
C
          IF (C2.EQ.'P') THEN
            ICHAINP(ICH)=J
            IF (NEW.EQ.0) THEN
              CHAINP(ICH)=0.0
              NEW=1
              ORIGP=GINFP(I)
              GO TO 70
            ENDIF
            IF (NEW.NE.0) THEN
              WRITE(OUT,67) ORIGP
67            FORMAT('Enter increment on base position of ',F9.2)
              CALL PAR_WRUSER(OUT,STAT)
              CALL PAR_CNPAR('RCHAIN')
              CALL PAR_RDVAL('RCHAIN',FMIN,FMAX,0.0,' ',VALUE)
              CHAINP(ICH)=VALUE
            ENDIF
70          CONTINUE
          ENDIF
C
C         Set the constraints on the heights as ratios to the
C         first constrained line height in the chain
C
          IF (C2.EQ.'H') THEN
            ICHAINH(ICH)=J
            IF (NEW.EQ.0) THEN
              CHAINH(ICH)=1.00
              NEW=1
              ORIGH=GINFH(I)
              GO TO 73
            ENDIF
            IF (NEW.NE.0) THEN
              WRITE(OUT,68) ORIGH
68            FORMAT('Enter height relative to line of height ',F9.2)
              CALL PAR_WRUSER(OUT,STAT)
              CALL PAR_CNPAR('RCHAIN')
              CALL PAR_RDVAL('RCHAIN',FMIN,FMAX,0.0,' ',VALUE)
              CHAINH(ICH)=VALUE
            ENDIF
73          CONTINUE
          ENDIF
C
C         Set the constraints on the widths as ratios to the
C         first constrained line width in the chain
C
          IF (C2.EQ.'W') THEN
            ICHAINW(ICH)=J
            IF (NEW.EQ.0) THEN
              CHAINW(ICH)=1.00
              NEW=1
              ORIGW=GINFW(I)
              GO TO 76
            ENDIF
            IF (NEW.NE.0) THEN
              WRITE(OUT,69) ORIGW
69            FORMAT('Enter width relative to line of width ',F9.2)
              CALL PAR_WRUSER(OUT,STAT)
              CALL PAR_CNPAR('RCHAIN')
              CALL PAR_RDVAL('RCHAIN',FMIN,FMAX,0.0,' ',VALUE)
              CHAINW(ICH)=VALUE
            ENDIF
76          CONTINUE
          ENDIF
        ENDDO
        J=J+1
        GO TO 53
C
C       Report the values of the chained Gaussian parameters; position
C       chains first
C
90      NJ=J-1
        DO J=1,NJ,1
          LOW=0
          DO I=1,NG,1
            IF (ICHAINP(I).EQ.J.AND.LOW.EQ.0) THEN
              WRITE(OUT,81) I,J,GINFP(I)
81            FORMAT(' Gaussian no. ',I2,' chain no. ',I2,
     :' position = ',F9.2)
              CALL PAR_WRUSER(OUT,STAT)
              ORIGP=GINFP(I)
              LOW=1
              GO TO 83
            ENDIF
            IF (ICHAINP(I).EQ.J.AND.LOW.EQ.1) THEN
              WRITE(OUT,82) I,J,ORIGP,CHAINP(I)
82            FORMAT(' Gaussian no. ',I2,' chain no. ',I2,
     :' position = ',F9.2,' + ',F8.2)
              CALL PAR_WRUSER(OUT,STAT)
              GO TO 83
            ENDIF
83          CONTINUE
          END DO
        END DO
C
C       Now report values of the height chains
C
        DO J=1,NJ,1
          LOW=0
          DO I=1,NG,1
            IF (ICHAINH(I).EQ.J.AND.LOW.EQ.0) THEN
              WRITE(OUT,84) I,J
84            FORMAT(' Gaussian no. ',I2,' chain no. ',I2,
     :' relative height = 1.00 ')
              CALL PAR_WRUSER(OUT,STAT)
              LOW=1
              GO TO 86
            ENDIF
            IF (ICHAINH(I).EQ.J.AND.LOW.EQ.1) THEN
              WRITE(OUT,85) I,J,CHAINH(I)
85            FORMAT(' Gaussian no. ',I2,' chain no. ',I2,
     :' relative height = ',F9.2)
              CALL PAR_WRUSER(OUT,STAT)
              GO TO 86
            ENDIF
86          CONTINUE
          END DO
        END DO
C
C       Now report values of the width chains
C
        DO J=1,NJ,1
          LOW=0
          DO I=1,NG,1
            IF (ICHAINW(I).EQ.J.AND.LOW.EQ.0) THEN
              WRITE(OUT,87) I,J
87            FORMAT(' Gaussian no. ',I2,' chain no. ',I2,
     :' relative width = 1.00 ')
              CALL PAR_WRUSER(OUT,STAT)
              LOW=1
              GO TO 89
            ENDIF
            IF (ICHAINW(I).EQ.J.AND.LOW.EQ.1) THEN
              WRITE(OUT,88) I,J,CHAINW(I)
88            FORMAT(' Gaussian no. ',I2,' chain no. ',I2,
     :' relative width = ',F9.2)
              CALL PAR_WRUSER(OUT,STAT)
              GO TO 89
            ENDIF
89          CONTINUE
          END DO
        END DO
      ENDIF

99    END
