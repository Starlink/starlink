*+  CONV_SPLIT - splits string containing RA/DEC into separate strings for each
	SUBROUTINE CONV_SPLIT(RADEC,RA,DEC,STATUS)
*
*    Description :
*    History :
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
       INCLUDE 'SAE_PAR'
       INCLUDE 'DAT_PAR'
*
*    Import :
      CHARACTER*(*) RADEC
*    Export :
      CHARACTER*(*) RA,DEC
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local constants :
      CHARACTER*1 BLANK,TAB
      PARAMETER (TAB=CHAR(9),BLANK=' ')
*    Local variables :
      INTEGER CF,CL
      INTEGER IC
      INTEGER NB
      INTEGER POS(10),SP
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  ignore any leading spaces
        CF=1
        DO WHILE (RADEC(CF:CF).EQ.' ')
          CF=CF+1
        ENDDO

* get used lengths of string
        CL=CHR_LEN(RADEC)

* get number of blanks or tabs
        NB=0
        DO IC=CF,CL
          IF (RADEC(IC:IC).EQ.TAB.OR.RADEC(IC:IC).EQ.BLANK) THEN
            NB=NB+1
            POS(NB)=IC
          ENDIF
        ENDDO

        IF (NB.EQ.1) THEN
* just one space or tab - assume this to be delimiter
          SP=POS(1)
        ELSEIF (NB.EQ.5) THEN
* everything delimited by spaces
          SP=POS(3)
        ELSEIF (NB.EQ.4) THEN
* assume only RA has seconds
          SP=POS(3)
        ELSEIF (NB.EQ.3) THEN
* assume no seconds
          SP=POS(2)
        ELSEIF (NB.EQ.0) THEN
          SP=INDEX(RADEC,',')
          IF (SP.EQ.0) THEN
            STATUS=SAI__ERROR
          ENDIF
        ELSE
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN
          RA=RADEC(:SP-1)
          DEC=RADEC(SP+1:)
        ELSE
          CALL MSG_PRNT('AST_ERR: cannot decipher RA/DEC format')
        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_SPLIT',STATUS)
        ENDIF

      ENDIF

      END
