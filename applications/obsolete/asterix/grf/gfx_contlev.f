*+  GFX_CONTLEV - get contour levels
      SUBROUTINE GFX_CONTLEV(ZMIN,ZMAX,NL,LEVEL,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GCB_PAR'
*    Import :
      REAL ZMIN,ZMAX
*    Import-export :
*    Export :
      INTEGER NL
      REAL LEVEL(GCB__MXCONTLEV)
*    Status :
      INTEGER STATUS
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER K
      LOGICAL OK
*    External references :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETI('CONT_N',OK,NL,STATUS)
        IF (.NOT.OK) THEN
          NL=GCB__DEFCONTLEV
          DO K=1,NL
            LEVEL(K)=ZMIN + REAL(K-1)* (ZMAX-ZMIN)/REAL(NL-1)
          ENDDO
        ELSE
          CALL GCB_GET1R('CONT_LEVS',1,NL,OK,LEVEL,STATUS)
          IF (.NOT.OK) THEN
            DO K=1,NL
              LEVEL(K)=ZMIN + REAL(K-1)* (ZMAX-ZMIN)/REAL(NL-1)
            ENDDO
          ENDIF

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_CONTLEV',STATUS)
        ENDIF

      ENDIF
      END
