      SUBROUTINE PARSE (INP,CMD,PARAMS,NPARAMS,CPARAM,FSTR,OK,OUT_LU)
C+
C
C Subroutine: 
C
C   P A R S E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C INP (<), CMD (>), PARAMS (>), NPARAMS (>), CPARAM (>),
C FSTR (>), OK (>), OUT_LU (>)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C This subroutine hacks up an input string into the command verb
C followed by several numerical parameters. The cparam is the stuff 
C following the command or the stuff enclosed in quotes.
C
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
C
      REAL NUM
      INTEGER INCR
      INTEGER SRANGE
      INTEGER ERANGE
      INTEGER NPARAMS        ! The number of parameters
      REAL PARAMS(*)         ! Parameter values
      INTEGER T_NPARAMS
      REAL T_PARAMS(100)
      CHARACTER*80 INP       ! The input string as entered
      CHARACTER*10 CMD       ! The parsed command (lower case)
      CHARACTER*80 CPARAM    ! The character parameter
      CHARACTER*80 DUMMY
      INTEGER I,J            ! Loop integers
      LOGICAL OK             ! Status flags
      LOGICAL MORE_PARAMS    !       "
      LOGICAL GOTCMD,FSTR
C
      OK = .TRUE.
      GOTCMD=.FALSE.
      FSTR=.FALSE.
C
C If the input string is empty then return with a bad status
C
      IF (INP.EQ.' ') THEN
          OK = .FALSE.
          GOTO 666
      ENDIF
C
C Set the parameters to a flag value
C
      J = 0
      DO I = 1,100
       PARAMS(I) = -1.E30
      ENDDO
      CPARAM = ' '
C
C Strip of the preceding spaces (if any)
C
      CALL SSTRIP(INP)
C
C Now check whether there are quotes. In this case it is ASSUMED that
C the string is like 'command "char string"'.
C
      I  =  INDEX(INP,'"') 
      IF (I .NE. 0) THEN
       J  =  INDEX(INP,' ')
       IF (J .NE. 0) THEN
        CMD  =  INP(:(J-1))
        CALL TOLOWERC(CMD)
        GOTCMD=.TRUE.
        DUMMY  =  INP((I+1):)
        INP = DUMMY
        J  =  INDEX(INP,'"')
        IF (J.NE.0) THEN
          CPARAM = INP(:(J-1))
          DUMMY=INP((J+1):)
          CALL SSTRIP(DUMMY)
          INP=DUMMY
          FSTR=.TRUE.
         ELSE
          CALL WR_ERROR('Unmatched quotes',OUT_LU)
          CPARAM = ' '
          OK = .FALSE.
         GOTO 666
        ENDIF
       ELSE
        CMD = INP(1:10)
        GOTO 666
       ENDIF
      ENDIF
C
C Set the input string to lower case. Unless any character string 
C parameters have been enclosed in quotes these too will be recased.
C
      IF (.NOT.GOTCMD) THEN
       CALL TOLOWERC(INP)
       I = INDEX(INP,' ')
       IF (I.EQ.0) THEN
        CMD = INP(1:10)
        GOTO 666
        ELSE
        CMD = INP(1:(I-1))
       ENDIF
       INP = INP((I+1):)
       CALL SSTRIP(INP)
      ENDIF
C
C Check that there are parameters.
C

      IF (INP.EQ.' ') THEN
        NPARAMS = 0
        GOTO 666
      ENDIF
      IF (.NOT.FSTR) CPARAM = INP
      T_NPARAMS = 0
      MORE_PARAMS = .TRUE.
C
C Loop through the string finding the parameters and incrementing the
C number of parameters. Do this until you reach the end of the string.
C
      DO WHILE(MORE_PARAMS)
C
        IF (INP(1:1).EQ.'>') THEN
         IF (T_NPARAMS.GT.0) THEN
          T_NPARAMS = T_NPARAMS+1
          T_PARAMS(T_NPARAMS) = -1.E30
          INP=INP(2:)
          CALL SSTRIP(INP)
          ELSE
          CALL WR_ERROR('No lower range value',OUT_LU)
          OK = .FALSE.
          GOTO 666
         ENDIF
        ENDIF
C
        READ(INP,*,ERR = 20) NUM
        T_NPARAMS=T_NPARAMS+1
        T_PARAMS(T_NPARAMS)=NUM
        I = INDEX(INP,' ')
        INP = INP((I+1):)
        CALL SSTRIP(INP)
        IF (INP.EQ.' ') THEN 
          MORE_PARAMS = .FALSE.
          IF (T_PARAMS(T_NPARAMS).EQ.-1.30) THEN
           CALL WR_ERROR('No upper range limit',OUT_LU)
           OK=.FALSE.
           GOTO 666
          ENDIF
        ENDIF
C
      ENDDO

      NPARAMS=0
      DO I=1,T_NPARAMS
       IF (T_PARAMS(I).EQ.-1.E30) THEN
        SRANGE=INT(T_PARAMS(I-1))
        ERANGE=INT(T_PARAMS(I+1))
        IF (SRANGE.LT.ERANGE) THEN
         INCR=1
         ELSE IF (SRANGE.GT.ERANGE) THEN
         INCR=-1
         ELSE
         INCR=0
        ENDIF
C
        IF (INCR.EQ.0) THEN
         PARAMS(NPARAMS)=REAL(SRANGE)
        ELSE
         NPARAMS=NPARAMS-1
         DO J=SRANGE,ERANGE,INCR
          NPARAMS=NPARAMS+1
          PARAMS(NPARAMS)=REAL(J)
         ENDDO
         NPARAMS=NPARAMS-1
        ENDIF
       ELSE
        NPARAMS=NPARAMS+1
        PARAMS(NPARAMS)=T_PARAMS(I)
       ENDIF
      ENDDO
C
666   GOTO 999
C
C The crash out routine
C
20    CONTINUE
      IF (T_NPARAMS.NE.0) THEN
       CALL WR_ERROR('Cannot read parameters',OUT_LU)
       NPARAMS = 0
       OK=.FALSE.
      ENDIF
999   CONTINUE
      END
