*+  PRS_GETLIST - Obtains 1-D list from user.
      SUBROUTINE PRS_GETLIST(PAR,NMAX,LIST,NITEM,STATUS)
*    Description :
*     Obtains a character string from the user specifying the list.
*     This is then converted into the INTEGER array LIST.
*
*    Method :
*    Parameters :
*     PAR    Input ranges string (CHAR)
*    Deficiencies :
*    Bugs :
*    Authors :
*      (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*    Import :
      CHARACTER*(*)    PAR                 ! CHARACTER parameter to call
      INTEGER 	       NMAX		   ! maximum number of items
*    Export :
      INTEGER          LIST(NMAX)	   ! output list
      INTEGER          NITEM		   ! number of items in list
*    Status :
      INTEGER          STATUS

*    Local variables :
      CHARACTER*(132)  INPUT                ! One line of input
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Get input from user
        CALL PAR_GET0C( PAR, INPUT, STATUS )

*      Parse it
        CALL PRS_GETLIST_INT( INPUT, NMAX, LIST, NITEM, STATUS )

        IF (STATUS.NE.SAI__OK.AND.STATUS.NE.PAR__NULL) THEN
          CALL ERR_REP(' ', 'from PRS_GETLIST', STATUS)
        END IF

      END IF

      END



*+  PRS_GETLIST_INT - Parse a list string
      SUBROUTINE PRS_GETLIST_INT(INPUT,NMAX,LIST,NITEM,STATUS)
*    Description :
*     Obtains a character string from the user specifying the list.
*     This is then converted into the INTEGER array LIST.
*
*    Method :
*    Parameters :
*     PAR    Input ranges string (CHAR)
*    Deficiencies :
*    Bugs :
*    Authors :
*      (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*    Import :
      CHARACTER*(*)    INPUT               ! String to parse
      INTEGER 	       NMAX		   ! maximum number of items
*    Export :
      INTEGER          LIST(NMAX)	   ! output list
      INTEGER          NITEM		   ! number of items in list
*    Status :
      INTEGER          STATUS

*    Function declarations :
      INTEGER          CHR_LEN
*    Local constants :
      CHARACTER*1 SPACE,STAR,COLON,SEMICOLON,COMMA,ZERO,NINE
      PARAMETER (SPACE=' ',STAR='*',COLON=':',SEMICOLON=';',
     :           COMMA=',',ZERO='0',NINE='9')
*    Local variables :
      CHARACTER*1      C
      INTEGER          I,J,K                ! Loop counter
      INTEGER          LEN                  ! No of characters in INPUT
      INTEGER          IVAL                 ! integer value
      INTEGER	       NTERM			! Number of list elements

      LOGICAL          CONT		    ! continue parsing
      LOGICAL          RANGE		    ! look for range
*-
*    Check status - return if bad.
      IF (STATUS .NE. SAI__OK) RETURN

      LEN = CHR_LEN(INPUT)
      IF ( LEN .LT. 1 ) THEN
        CALL MSG_PRNT('! No list entered')
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Number of parsed list bits
      NTERM = 0

*  remove any leading spaces
      I=1
      DO WHILE (INPUT(I:I).EQ.' ')
        I=I+1
      END DO
      INPUT=INPUT(I:)
      LEN=LEN-(I-1)

*  check first character is numeric or '*' or ':'
      IF ((INPUT(1:1).LT.ZERO.OR.INPUT(1:1).GT.NINE)
     :            .AND.INPUT(1:1).NE.COLON.AND.INPUT(1:1).NE.STAR) THEN
        CALL MSG_PRNT('AST_ERR: syntax error in list specification')
        STATUS=SAI__ERROR
      END IF

*  treat case of list= '*' '*:*'  ':*' '*: or  ':'
      IF (INPUT.EQ.STAR.OR.INPUT.EQ.STAR//COLON//STAR
     :  .OR.INPUT.EQ.COLON//STAR.OR.INPUT.EQ.STAR//COLON
     :                                 .OR.INPUT.EQ.COLON) THEN
        NTERM = NTERM + 1
        DO J=1,NMAX
          LIST(J)=J
        ENDDO
        NITEM=NMAX
        CONT=.FALSE.
*  list starts with ':'
      ELSEIF (INPUT(1:1).EQ.COLON) THEN
        LIST(1)=1
        NITEM=1
        CONT=.TRUE.
        RANGE=.TRUE.
        I=2
*  list starts with '*:'
      ELSEIF (INPUT(1:2).EQ.STAR//COLON) THEN
        LIST(1)=1
        NITEM=1
        CONT=.TRUE.
        RANGE=.TRUE.
        I=3
*  starts with number
      ELSE
        NITEM=0
        CONT=.TRUE.
        RANGE=.FALSE.
        I=1
      END IF

*  scan for each list entry
      DO WHILE (CONT.AND.STATUS.EQ.SAI__OK)

        J=I+1
        C=INPUT(J:J)
*  scan to next delimiter
        DO WHILE (C.NE.SPACE.AND.C.NE.COMMA.AND.C.NE.STAR
     :       .AND.C.NE.SEMICOLON.AND.C.NE.COLON.AND.J.LE.LEN)
          J=J+1
          C=INPUT(J:J)
        ENDDO

*
*  decode number
        CALL CHR_CTOI(INPUT(I:J-1),IVAL,STATUS)

*  if looking for upper end of a range fill in inbetween values
        IF (RANGE) THEN
          DO K=LIST(NITEM)+1,IVAL
            NITEM=NITEM+1
            LIST(NITEM)=K
          ENDDO
          RANGE=.FALSE.
*  otherwise just add to list
        ELSE
          NITEM=NITEM+1
          LIST(NITEM)=IVAL
          NTERM = NTERM + 1
        END IF

*  deal with case where delimeter is ':' or '*'

        IF (C.EQ.COLON) THEN
*  last character is ':'
          IF (J.EQ.LEN) THEN
            DO K=LIST(NITEM)+1,NMAX
              NITEM=NITEM+1
              LIST(NITEM)=K
            ENDDO
            CONT=.FALSE.
*  string ends with ':*'
          ELSEIF (INPUT(J:J+1).EQ.COLON//STAR) THEN
            DO K=LIST(NITEM)+1,NMAX
              NITEM=NITEM+1
              LIST(NITEM)=K
            ENDDO
            CONT=.FALSE.
*  start new range
          ELSEIF (J.LT.LEN) THEN
            RANGE=.TRUE.
          END IF

*  last character is '*' and looking for upper end of range
        ELSEIF (C.EQ.STAR.AND.RANGE.AND.J.EQ.LEN) THEN
          DO K=LIST(NITEM)+1,NMAX
            NITEM=NITEM+1
            LIST(NITEM)=K
          ENDDO
          CONT=.FALSE.

        END IF

        CONT=(CONT.AND.J.LT.LEN.AND.STATUS.EQ.SAI__OK)

        I=J+1
        C=INPUT(I:I)
*  scan to next numeric or ':' or '*'
        DO WHILE (CONT.AND.(C.LT.ZERO.OR.C.GT.NINE)
     :            .AND.C.NE.COLON.AND.C.NE.STAR.AND.I.LT.LEN)
          I=I+1
          C=INPUT(I:I)
        ENDDO

      ENDDO

*  check content of list
      I=1
      DO WHILE (I.LE.NITEM.AND.STATUS.EQ.SAI__OK)
*  check values fall within 1...NMAX
        IF (LIST(I).LT.1.OR.LIST(I).GT.NMAX) THEN
          CALL MSG_PRNT('AST_ERR: list contains illegal values')
          STATUS=SAI__ERROR
        END IF

*  check for repeated values
        IF (NTERM.GT.1 ) THEN
          J=I+1
          DO WHILE (J.LE.NITEM.AND.STATUS.EQ.SAI__OK)
            IF (LIST(I).EQ.LIST(J)) THEN
              STATUS = SAI__ERROR
              CALL ERR_REP( ' ', 'AST_ERR: list contains repeated '/
     :                      /'values', STATUS )
            END IF
            J=J+1
          ENDDO
        END IF
        I=I+1
      ENDDO

 99   CONTINUE
      IF (STATUS.NE.SAI__OK.AND.STATUS.NE.PAR__NULL) THEN
        CALL ERR_REP(' ', 'from PRS_GETLIST_INT', STATUS)
      END IF

      END
