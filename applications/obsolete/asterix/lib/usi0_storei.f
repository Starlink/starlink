*+  USI0_STOREI - Stores ADI identifier in internal common
      SUBROUTINE USI0_STOREI( PAR, ID, IO, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USI_CMN'
*    Import :
      CHARACTER*(*)          PAR       ! Parameter name
      INTEGER			ID			! Parameter identifier
      CHARACTER*1            IO	       ! input or output
*    Export :

*    Status :
      INTEGER                STATUS
*    Local variables :
      INTEGER I
      LOGICAL SPARE
*-

      IF (STATUS.EQ.SAI__OK) THEN
        I=1
        SPARE=.FALSE.
*  scan list to find first empty slot
        DO WHILE (.NOT.SPARE.AND.I.LE.USI__NMAX)
          IF (.NOT.(DS(I).USED)) THEN
            SPARE=.TRUE.
          ELSE
            I=I+1
          ENDIF
        ENDDO

        IF (SPARE) THEN
          DS(I).LOC=DAT__NOLOC
          DS(I).ADIFPN =.TRUE.
          DS(I).PAR=PAR
          DS(I).USED=.TRUE.
          DS(I).IO=IO
          CALL ADI1_PUTLOC( LOC, DS(I).ADI_ID, STATUS )
        ELSE
          STATUS=SAI__ERROR
          CALL ERR_REP(' ', 'Maximum number of datasets exceeded',
     ;                  STATUS )
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from USI0_STOREI',STATUS)
        ENDIF
      ENDIF
      END
