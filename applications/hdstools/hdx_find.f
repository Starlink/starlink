*+  HDX_FIND - Find locator for structured name
      SUBROUTINE HDX_FIND(LOCIN,NAME,LOCOUT,STATUS)
* Description :
* Deficiencies :
* Bugs :
* Authors :
*     Dick Willingale 1986-Sep-18
*     A Chipperfield 2001-Nov-15
* History :
*     15-NOV-2001 (AJC):
*        Do CELL or SLICE depending upon specifier,
*           not on if it's a structure
*-
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
* Import :
      CHARACTER*(DAT__SZLOC) LOCIN	  !start locator
      CHARACTER*(*) NAME	          !structured name
* Import-Export :
* Export :
      CHARACTER*(DAT__SZLOC) LOCOUT	  !locator found
* Status :
      INTEGER STATUS
* Function declarations :
* Local constants :
      INTEGER MAXL
      PARAMETER (MAXL=10)
* Local variables :
      CHARACTER*(DAT__SZNAM) OBJECT(MAXL)
      CHARACTER*(DAT__SZLOC) LOC,LOCA
      INTEGER I
      INTEGER KDIM(MAXL),KELS(DAT__MXDIM,MAXL),KUPS(DAT__MXDIM,MAXL)
      INTEGER K,NOB
      LOGICAL CELL
* Global variables :
* Local data :
*-
      IF(STATUS .NE. SAI__OK) RETURN
*
* Parse name into objects and dimensions
      CALL STR_PARSE(NAME,MAXL,DAT__MXDIM,OBJECT,KDIM,KELS,KUPS,NOB)
      IF(NOB.EQ.0) THEN
         STATUS=SAI__ERROR
      ENDIF

C Now find locator
      CALL DAT_CLONE(LOCIN,LOCOUT,STATUS)
*
      K=1
      DO WHILE ( K.LE.NOB .AND. STATUS.EQ.SAI__OK )

*  locate whole object
         IF( OBJECT(K) .EQ. ' ' ) THEN
            CALL DAT_CLONE( LOCOUT, LOC, STATUS )

         ELSE
            CALL DAT_FIND(LOCOUT,OBJECT(K),LOC,STATUS)

         ENDIF

         IF( KDIM(K).NE.0 .AND. STATUS.EQ.SAI__OK ) THEN
*  locate slice or cell
            CELL=.TRUE.
            DO I=1,KDIM(K)
               IF( KELS(I,K) .NE. KUPS(I,K) ) THEN
                  CELL=.FALSE.
               ENDIF
            ENDDO

            IF ( CELL ) THEN
               CALL DAT_CELL(LOC,KDIM(K),KELS(1,K),LOCA,STATUS)

            ELSE
               CALL DAT_SLICE(LOC,KDIM(K),KELS(1,K),KUPS(1,K),LOCA,
     :                                                      STATUS)
            ENDIF

            CALL DAT_ANNUL(LOC,STATUS)
            CALL DAT_CLONE(LOCA,LOC,STATUS)

         ENDIF

         CALL DAT_ANNUL(LOCOUT,STATUS)
         CALL DAT_CLONE(LOC,LOCOUT,STATUS )
         K=K+1

      ENDDO
*
      IF( STATUS.NE.SAI__OK ) THEN
         CALL ERR_REP(' ','from HDX_FIND',STATUS)
      ENDIF
*
      END
