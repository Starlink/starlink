      SUBROUTINE GROPWK(FILE,IDEFTY,IWKID,IWKTYP,ISTAT)
*+
*
*     - - - - - - - -
*       G R O P W K    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Opens a device for plotting: Open GKS if necessary and open
*   the workstation. The catagory of the workstation is checked to see
*   that it is not a metafile input or input only workstation. If the
*   workstation is a metafile output workstation, the type of the target
*   device rather than the metafile is returned.
*
*   Given
*      FILE     c     Device name
*      IDEFTY   i     Default device type
*
*   Returned
*      IWKID    i     Workstation identifier
*      IWKTYP   i     Workstation type
*      ISTAT    i     Status (1=>success)
*
*   Constants from GKS_PAR
*      GGKCL    i     GKS closed
*      GMO      i     Catagory - metafile output
*      GMI      i        "           "    input
*      GINPUT   i        "       input
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INCLUDE 'GKS_PAR'

      INCLUDE 'GKS_ERR'

      INCLUDE 'PGP_ERR'

      INCLUDE 'SAE_PAR'


      CHARACTER*(*) FILE
      INTEGER IDEFTY, IWKID, IWKTYP, ISTAT

      INTEGER CHR_LEN

      INTEGER ISTATE, ICON, IERR, ICAT, IQUAL
      INTEGER ICODE, I, J, K, LFILE, ITMP
      CHARACTER*40 PART(3), QUAL
      CHARACTER*10 DEFTYP
      LOGICAL INQUO

      CHARACTER*6 APPEND,TARGET

*  Declarations for "Starlink" escape function
      INTEGER IA(2), IER, LIESC, LOESC, LASTER
      CHARACTER*80 STR(1), IESCDR(1), OESCDR(1)
      REAL RA(1)
      DATA STR/' '/
*...
      DATA APPEND /'APPEND'/, TARGET /'TARGET'/

*  Suppress the reporting of errors so that alternative workstation name
*  syntaxes can be tried with out generating error message
      CALL ERR_MARK

      IERR = SAI__OK
*  Split the workstation name into its component part (separated by /
*  characters).
      J = 1
      K = 1
      DO 1 I = 1,3
         PART(I) = ' '
   1  CONTINUE

      LFILE = CHR_LEN(FILE)
      INQUO = .FALSE.
      DO 2 I = 1 , LFILE
         IF (FILE(I:I).EQ.'"') THEN
            INQUO = .NOT.INQUO
            GO TO 2
         ELSE
            IF (FILE(I:I).EQ.'/'.AND..NOT.INQUO) THEN
               J = J + 1
               K = 1
               IF (J.GT.3) GO TO 99
            ELSE
               IF (K.GE.40) GO TO 2
               PART(J)(K:K) = FILE(I:I)
               K = K + 1
            END IF
         END IF
   2  CONTINUE

*  Construct and integer between 0 and 7 the bits of which indicate the
*  presence of each of the three possible components.
      ICODE = 0
      IF (PART(1).NE.' ') ICODE = ICODE + 1
      IF (PART(2).NE.' ') ICODE = ICODE + 2
      IF (PART(3).NE.' ') ICODE = ICODE + 4

*  The following forms of name are valid:
*        GNS-name                   icode = 1
*        GNS-name/qualifier               = 3
*        device/type                      = 3
*        device/type/qualifier            = 7
*        /type                            = 2
*        /type/qualifier                  = 6
*        device                           = 1
*        logical-name                     = 1

      GO TO (99,10,20,30,99,99,60,70) ICODE + 1
      GO TO 99

   10 CONTINUE
*             GNS-name
         CALL gns_TNG(PART(1),IWKTYP,ICON,IERR)
         IF (IERR.EQ.SAI__OK) THEN
            IQUAL = 0
         ELSE
*             device (with type defaulted to the value of IDEFTY)
            IF (IDEFTY.EQ.0) GO TO 99
            IERR = 0
            WRITE (DEFTYP,'(I10)') IDEFTY
            CALL gns_TNDG(DEFTYP,PART(1),IWKTYP,ICON,IERR)
            IF (IERR.NE.SAI__OK) GO TO 99
         END IF
         GO TO 1000

   20 CONTINUE
*             /type
         CALL gns_TNG(PART(2),IWKTYP,ICON,IERR)
         IF (IERR.NE.SAI__OK) GO TO 99
         IQUAL = 0
         GO TO 1000

   30 CONTINUE
*             device/type
         CALL gns_TNDG(PART(2),PART(1),IWKTYP,ICON,IERR)
         IF (IERR.EQ.SAI__OK) THEN
            IQUAL = 0
         ELSE
*             GNS-name/qualifier
            IERR = 0
            CALL gns_TNG(PART(1),IWKTYP,ICON,IERR)
            IF (IERR.NE.SAI__OK) GO TO 99
            IQUAL = 2
         END IF
         GO TO 1000

   60 CONTINUE
*             /type/qualifier
         CALL gns_TNG(PART(2),IWKTYP,ICON,IERR)
         IF (IERR.NE.SAI__OK) GO TO 99
         IQUAL = 3
         GO TO 1000

   70 CONTINUE
*             device/type/qualifier
         CALL gns_TNDG(PART(2),PART(1),IWKTYP,ICON,IERR)
         IF (IERR.NE.SAI__OK) GO TO 99
         IQUAL = 3
         GO TO 1000

   99 CONTINUE
*             Not a valid name
         CALL ERR_ANNUL(IERR)
         CALL ERR_RLSE
         CALL MSG_SETC('NAME',FILE(:LFILE))
         CALL ERR_REP('GRIWKN',
     :        'GROPWK - ^NAME is not a valid workstation name', GRIWKN)
         ISTAT = -1
         GO TO 9999
 1000 CONTINUE

*    Flush out any existing error messages and re-enable error
*    reporting
         CALL ERR_ANNUL(IERR)
         CALL ERR_RLSE

*    Test state of GKS
         CALL GQOPS(ISTATE)

*    Open if necessary
         IF (ISTATE.EQ.GGKCL) CALL GOPKS(6,-1)

*    Check the catagory of the workstation
         CALL GQWKCA(IWKTYP,IERR,ICAT)
         IF (IERR.NE.0) THEN
            CALL MSG_SETI('TYPE', IWKTYP)
            CALL ERR_REP('GRIWKT',
     :      'GROPWK - ^TYPE is not a valid GKS workstation type',
     :      GRIWKT)
            ISTAT = -1
            GO TO 9999
         ENDIF
         IF (ICAT.EQ.GMI .OR. ICAT.EQ.GINPUT) THEN
            CALL MSG_SETI('TYPE', IWKTYP)
            CALL ERR_REP('GRICAT',
     :      'GROPWK - GKS Workstation ^TYPE is not capable of output',
     :      GRICAT)
            ISTAT = -1
            GO TO 9999
         ENDIF

*    Locate a free workstation id
         IWKID = 0
 2000    CONTINUE
         IWKID = IWKID + 1
         CALL GQWKS(IWKID,IERR,ISTATE)
         IF (IERR.EQ.0) GO TO 2000

*    Check for presence of /APPEND
         IF (IQUAL.GT.0) THEN
            CALL GRTOUP(QUAL,PART(IQUAL))
            IF (APPEND(:INDEX(QUAL,' ')-1).EQ.QUAL) THEN
               IQUAL = 0

*       Suppress screen clear

*---Uncomment these line if your GKS doesn't support the escape function to
*   suppress the clearing of the display surface when a workstation is
*   opened
*               CALL ERR_REP('GRNOAP',
*     :            'GROPWK - append qualifier not supported', GRNOAP)
*---

*   Call "Starlink" escape function
                CALL ERR_MARK
                IA(1) = IWKTYP
                IA(2) = GYES
                CALL GPREC(2, IA, 1, RA, 1, 1, STR, 1, IER, LIESC,
     :                     IESCDR)
                CALL GESC(-3, LIESC, IESCDR, 1, LOESC, OESCDR)

*   Check for GKS error
                CALL ERR_STAT(LASTER)
C                IF (LASTER.EQ.GKS__ERROR) CALL ERR_REP('GRNOAP',
C     :            'GROPWK - append qualifier not supported', GRNOAP)
                IF (LASTER.EQ.GKS__ERROR) CALL ERR_ANNUL(LASTER)
                CALL ERR_RLSE
            END IF
         END IF

*    Open the workstation
         CALL GOPWK(IWKID,ICON,IWKTYP)

*    Check that we succeded
         CALL GQWKS(IWKID,IERR,ISTAT)
         IF (IERR.NE.0) THEN
            CALL ERR_REP('GRUOWK',
     :      'GROPWK - unable to open workstation', GRUOWK)
            ISTAT = -1
         ELSE

*       If the workstation is a metafile, return the type of the target
*       instead of the type of the metafile
            IF (ICAT.EQ.GMO) THEN

*          Default target is A4 landscape postscript
               IWKTYP = 2701

*          Test for presence of /target
               IF (IQUAL.GT.0) THEN
                  I = INDEX(PART(IQUAL),'=')
                  CALL GRTOUP(QUAL,PART(IQUAL))
                  IF (TARGET(:I-1).EQ.QUAL(:I-1)) THEN

*               Convert target name to workstation type
                     CALL gns_TNG(PART(IQUAL)(I+1:),IWKTYP,ITMP,IERR)
                     IF (IERR.NE.SAI__OK) THEN
                        CALL MSG_SETC('NAME',PART(IQUAL)(I+1:))
                        CALL ERR_REP('GRIWKN',
     :        'GROPWK - ^NAME is not a valid workstation name', GRIWKN)
                     END IF
                     IQUAL = 0
                  END IF
               END IF
            END IF

*       Report unknown qualifier
            IF (IQUAL.NE.0) THEN
               CALL MSG_SETC('NAME',PART(IQUAL))
               CALL ERR_REP('GRUNKQ',
     :        'GROPWK - Workstation qualifer ^NAME ignored', GRUNKQ)
            END IF

*       success
            ISTAT = 1
         END IF

 9999 CONTINUE
      END
