*+  CONV_RADEC - converts RA/DEC in string form to DP degrees
	SUBROUTINE CONV_RADEC(RA,DEC,RAD,DECD,STATUS)
*
*    Description :
*
*         RA/DEC is input in one of the following string forms and
*         output as DOUBLE PREC degrees
*
*             235.789        decimal degrees
*               2.56879rad   radian  (r is sufficient)
*             12:34:24.5     hms/dms
*             12:34.465
*             12 34 24.5     hms/dms
*             12h34m24.5s    hms
*             -12d45m20.5s   dms
*              -12d45.34
*
*    History :
*
*     ?? ??? ?? : Original (RJV)
*     16 Aug 93 : Fixed bugs handling HH MM.M type input (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
       INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*) RA,DEC
*
*    Export :
*
      DOUBLE PRECISION RAD,DECD
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*20 RAS,DECS
      INTEGER LRA,LDEC
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  make local copy and convert to lower case
        RAS=RA
        CALL CHR_LCASE(RAS)
        DECS=DEC
        CALL CHR_LCASE(DECS)

*  remove any leading spaces
        CALL CHR_LDBLK( RAS )
        CALL CHR_LDBLK( DECS )

* get used lengths of strings
        LRA=CHR_LEN(RAS)
        LDEC=CHR_LEN(DECS)

*  RA

*  radian
        IF (INDEX(RAS,'r').NE.0) THEN
          CALL CONV_RADEC_RADIAN(RAS,RAD,STATUS)

*  h m s
        ELSEIF (INDEX(RAS,'h').NE.0) THEN
          CALL CONV_RADEC_HMS1(RAS,RAD,STATUS)

*  : : :
        ELSEIF (INDEX(RAS,':').NE.0) THEN
          CALL CONV_RADEC_HMS2(RAS,':',RAD,STATUS)

*  spaces
        ELSEIF (INDEX(RAS(:LRA),' ').NE.0) THEN
          CALL CONV_RADEC_HMS2(RAS,' ',RAD,STATUS)

*  decimal
        ELSE
          CALL CONV_RADEC_DECIMAL(RAS,RAD,STATUS)

        ENDIF


*  DEC

*  radian
        IF (INDEX(DECS,'r').NE.0) THEN
          CALL CONV_RADEC_RADIAN(DECS,DECD,STATUS)


*  d m s
        ELSEIF (INDEX(DECS,'d').NE.0) THEN
          CALL CONV_RADEC_DMS1(DECS,DECD,STATUS)

*  : : :
        ELSEIF (INDEX(DECS,':').NE.0) THEN
          CALL CONV_RADEC_DMS2(DECS,':',DECD,STATUS)

*  spaces
        ELSEIF (INDEX(DECS(:LDEC),' ').NE.0) THEN
          CALL CONV_RADEC_DMS2(DECS,' ',DECD,STATUS)

*  decimal
        ELSE
          CALL CONV_RADEC_DECIMAL(DECS,DECD,STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_RADEC',STATUS)
        ENDIF

      ENDIF

      END



	SUBROUTINE CONV_RADEC_RADIAN(STR,ANG,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
       INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) STR
*    Export :
      DOUBLE PRECISION ANG
*    Status :
      INTEGER STATUS
*    Local constants :
      DOUBLE PRECISION PI,RTOD
      PARAMETER (PI=3.14159265358979D0,RTOD=180.0D0/PI)
*    Local variables :
      DOUBLE PRECISION RAD
      INTEGER R
*
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  read value in radian from string
        R=INDEX(STR,'r')
        IF (R.GT.1) THEN
          CALL CHR_CTOD(STR(:R-1),RAD,STATUS)
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid format')
          STATUS=SAI__ERROR
        ENDIF

*  convert to degrees
        IF (STATUS.EQ.SAI__OK) THEN
          ANG=RAD*RTOD
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_RADEC_RAD',STATUS)
        ENDIF

      ENDIF

      END

      SUBROUTINE CONV_RADEC_DECIMAL(STR,ANG,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
       INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) STR
*    Export :
      DOUBLE PRECISION ANG
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
*
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL CHR_CTOD(STR,ANG,STATUS)


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_RADEC_DECIMAL',STATUS)
        ENDIF

      ENDIF

      END

      SUBROUTINE CONV_RADEC_HMS1(STR,ANG,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
       INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) STR
*    Export :
      DOUBLE PRECISION ANG
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER H,M,S,L
      INTEGER IHOUR,IMIN
      REAL SEC
*
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IHOUR=0
        IMIN=0
        SEC=0.0

        L=CHR_LEN(STR)

*  get hours
        H=INDEX(STR,'h')
        IF (H.GT.1) THEN
          CALL CHR_CTOI(STR(:H-1),IHOUR,STATUS)
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid format')
          STATUS=SAI__ERROR
        ENDIF

*  get minutes
        IF (STATUS.EQ.SAI__OK) THEN

          M=INDEX(STR,'m')
          IF (M.EQ.0.AND.L.GT.H) THEN
            CALL CHR_CTOI(STR(H+1:L),IMIN,STATUS)
          ELSEIF (M.GE.H+2) THEN
            CALL CHR_CTOI(STR(H+1:M-1),IMIN,STATUS)
          ELSE
            CALL MSG_PRNT('AST_ERR: invalid format')
            STATUS=SAI__ERROR
          ENDIF

        ENDIF

*  get seconds
        IF (STATUS.EQ.SAI__OK.AND.M.GT.0) THEN

          S=INDEX(STR,'s')
          IF (S.EQ.0.AND.M.LT.L) THEN
            CALL CHR_CTOR(STR(M+1:L),SEC,STATUS)
          ELSEIF (S.GT.0.AND.S.GE.M+2) THEN
            CALL CHR_CTOR(STR(M+1:S-1),SEC,STATUS)
          ELSEIF (S.GT.0) THEN
            CALL MSG_PRNT('AST_ERR: invalid format')
            STATUS=SAI__ERROR
          ENDIF

        ENDIF

*  convert to degrees
        IF (STATUS.EQ.SAI__OK) THEN

          ANG=(DBLE(IHOUR)+DBLE(IMIN)/60.0D0+DBLE(SEC)/3600.0D0)*15.0D0

        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_RADEC_HMS1',STATUS)
        ENDIF

      ENDIF

      END

      SUBROUTINE CONV_RADEC_HMS2(STR,DELIM,ANG,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
       INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) STR
      CHARACTER*1 DELIM
*    Export :
      DOUBLE PRECISION ANG
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      INTEGER H,M,L
      INTEGER IHOUR,IMIN
      REAL SEC,MIN
*
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IHOUR=0
        IMIN=0
        SEC=0.0
        MIN=0.0
        L=CHR_LEN(STR)

*  get hours
        H=INDEX(STR,DELIM)
        IF (H.GT.1) THEN
          CALL CHR_CTOI(STR(:H-1),IHOUR,STATUS)
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid format')
          STATUS=SAI__ERROR
        ENDIF

*  get minutes
        IF (STATUS.EQ.SAI__OK) THEN

          M=INDEX(STR(H+1:),DELIM)+H
          IF (M.GE.H+2) THEN
            CALL CHR_CTOR(STR(H+1:M-1),MIN,STATUS)
          ELSEIF (M.EQ.H.AND.L.GT.H) THEN
            CALL CHR_CTOR(STR(H+1:L),MIN,STATUS)
          ELSE
            CALL MSG_PRNT('AST_ERR: invalid format')
            STATUS=SAI__ERROR
          ENDIF

        ENDIF

*  get seconds
        IF (STATUS.EQ.SAI__OK) THEN

          IF (M.GT.H.AND.L.GT.M) THEN
            CALL CHR_CTOR(STR(M+1:L),SEC,STATUS)
          ENDIF

        ENDIF

*  convert to degrees
        IF (STATUS.EQ.SAI__OK) THEN

          ANG=(DBLE(IHOUR)+DBLE(MIN)/60.0D0+DBLE(SEC)/3600.0D0)*15.0D0

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_RADEC_HMS2',STATUS)
        ENDIF

      ENDIF

      END


      SUBROUTINE CONV_RADEC_DMS1(STR,ANG,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
       INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) STR
*    Export :
      DOUBLE PRECISION ANG
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local variables :
      INTEGER D,M,S,L
      INTEGER IDEG
      REAL MIN
      REAL SEC
      DOUBLE PRECISION SIGN
*
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IDEG=0
        MIN=0.0
        SEC=0.0

*  check for sign
        IF (STR(1:1).EQ.'+') THEN
          SIGN=1.0D0
          STR=STR(2:)
        ELSEIF (STR(1:1).EQ.'-') THEN
          SIGN=-1.0D0
          STR=STR(2:)
        ELSE
          SIGN=1.0D0
        ENDIF

*  get overall length of string
        L=CHR_LEN(STR)

*  get degrees
        D=INDEX(STR,'d')
        IF (D.GT.1) THEN
          CALL CHR_CTOI(STR(:D-1),IDEG,STATUS)
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid format')
          STATUS=SAI__ERROR
        ENDIF

*  get minutes
        IF (STATUS.EQ.SAI__OK) THEN

          M=INDEX(STR,'m')
          IF (M.EQ.0.AND.L.GT.D) THEN
            CALL CHR_CTOR(STR(D+1:L),MIN,STATUS)
          ELSEIF (M.GE.D+2) THEN
            CALL CHR_CTOR(STR(D+1:M-1),MIN,STATUS)
          ELSE
            CALL MSG_PRNT('AST_ERR: invalid format')
            STATUS=SAI__ERROR
          ENDIF

        ENDIF

*  get seconds
        IF (STATUS.EQ.SAI__OK.AND.M.GT.0) THEN

          S=INDEX(STR,'s')
          IF (S.EQ.0.AND.M.LT.L) THEN
            CALL CHR_CTOR(STR(M+1:L),SEC,STATUS)
          ELSEIF (S.GT.0.AND.S.GE.M+2) THEN
            CALL CHR_CTOR(STR(M+1:S-1),SEC,STATUS)
          ELSEIF (S.GT.0) THEN
            CALL MSG_PRNT('AST_ERR: invalid format')
            STATUS=SAI__ERROR
          ENDIF

        ENDIF

*  convert to degrees
        IF (STATUS.EQ.SAI__OK) THEN

          ANG=(DBLE(IDEG)+DBLE(MIN)/60.0D0+DBLE(SEC)/3600.0D0)*SIGN

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_RADEC_DMS1',STATUS)
        ENDIF

      ENDIF

      END

      SUBROUTINE CONV_RADEC_DMS2(STR,DELIM,ANG,STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
       INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) STR
      CHARACTER*1 DELIM
*    Export :
      DOUBLE PRECISION ANG
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      INTEGER D,M,L
      INTEGER IDEG
      REAL MIN,SEC
      DOUBLE PRECISION SIGN
*
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IDEG=0
        MIN=0.0
        SEC=0.0


*  check for sign
        IF (STR(1:1).EQ.'+') THEN
          SIGN=1.0D0
          STR=STR(2:)
        ELSEIF (STR(1:1).EQ.'-') THEN
          SIGN=-1.0D0
          STR=STR(2:)
        ELSE
          SIGN=1.0D0
        ENDIF

        L=CHR_LEN(STR)

*  get degrees
        D=INDEX(STR,DELIM)
        IF (D.GT.1) THEN
          CALL CHR_CTOI(STR(:D-1),IDEG,STATUS)
        ELSE
          CALL MSG_PRNT('AST_ERR: invalid format')
          STATUS=SAI__ERROR
        ENDIF

*  get minutes
        IF (STATUS.EQ.SAI__OK) THEN

          M=INDEX(STR(D+1:),DELIM)+D
          IF (M.GE.D+2) THEN
            CALL CHR_CTOR(STR(D+1:M-1),MIN,STATUS)
          ELSEIF (M.EQ.D.AND.L.GT.D) THEN
            CALL CHR_CTOR(STR(D+1:L),MIN,STATUS)
          ELSE
            CALL MSG_PRNT('AST_ERR: invalid format')
            STATUS=SAI__ERROR
          ENDIF

        ENDIF

*  get seconds
        IF (STATUS.EQ.SAI__OK) THEN

          IF (M.GT.D.AND.L.GT.M) THEN
            CALL CHR_CTOR(STR(M+1:L),SEC,STATUS)
          ENDIF

        ENDIF

*  convert to degrees
        IF (STATUS.EQ.SAI__OK) THEN

          ANG=(DBLE(IDEG)+DBLE(MIN)/60.0D0+DBLE(SEC)/3600.0D0)*SIGN

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_RADEC_DMS2',STATUS)
        ENDIF

      ENDIF

      END
