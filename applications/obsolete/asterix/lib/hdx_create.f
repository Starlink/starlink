*+  HDX_CREATE - Create locator for structured name
      SUBROUTINE HDX_CREATE(ILOC,NAME,NDIM,IELS,TYPE,OLOC,STATUS)
*
* Description :
* Method :
*     <description of how the subroutine works - for programmer info>
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
* Author Dick Willingale 1986-Sep-18
* History :
*     18 Sep 86 : Original (LTVAD::RW)
*     10 May 88 : Converted to Asterix88 (LTVAD::RDS)
*     11 Jun 91 : Error reporting done properly (BHVAD::DJA)
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
*
      CHARACTER*(DAT__SZLOC)   ILOC                  ! Start locator
      CHARACTER*(*)            NAME                  ! Structured name
      INTEGER                  NDIM	             ! # of dimensions
      INTEGER                  IELS(DAT__MXDIM)      ! Size of each dimension
      CHARACTER*(*)            TYPE                  ! Data type e.g.'_DOUBLE'
*
*    Export :
*
      CHARACTER*(DAT__SZLOC)   OLOC                  ! Output locator for name
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER MAXL
      PARAMETER (MAXL=10)
*
*    Local variables :
*
      CHARACTER*(DAT__SZNAM)   OBJECT(MAXL)
      CHARACTER*(DAT__SZTYP)   STYPE
      CHARACTER*(DAT__SZLOC)   LOC,LOCA

      INTEGER KDIM(MAXL),KELS(DAT__MXDIM,MAXL),KUPS(DAT__MXDIM,MAXL)
      INTEGER J,K,I                       !Loop variables
      INTEGER NTOT,NELS,NOB

      LOGICAL THERE
*
*    Local data :
*
      DATA STYPE/'STRUCTURE'/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Parse name into objects and dimensions
      CALL STR_PARSE(NAME,MAXL,DAT__MXDIM,OBJECT,KDIM,KELS,KUPS,NOB)
      IF ( NOB .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Check not trying to create a slice
      IF ( KDIM(NOB) .GT. 0 ) THEN
        CALL MSG_SETC( 'NAME', NAME )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'HDX_CREATE failed, attempt to create a '/
     :                                      /'slice, ^NAME', STATUS )
        GOTO 99
      END IF

*    Transfer dimensions from call
      KDIM(NOB)=NDIM
      DO J=1,NDIM
        KELS(J,NOB)=IELS(J)
        KUPS(J,NOB)=IELS(J)
      END DO

*    Now find/create locator
      CALL DAT_CLONE( ILOC, OLOC, STATUS )
*
      DO K=1,NOB
*
        CALL DAT_THERE(OLOC,OBJECT(K),THERE,STATUS)
*
        IF ( .NOT. THERE ) THEN
*
          IF ( K .EQ. NOB ) THEN
            CALL DAT_NEW(OLOC,OBJECT(K),TYPE,KDIM(K),KELS(1,K),STATUS)
          ELSE
            CALL DAT_NEW(OLOC,OBJECT(K),STYPE, KDIM(K),KELS(1,K),STATUS)
          END IF
*
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( ' ', 'HDX_CREATE failed, '//OBJECT(K), STATUS)
            CALL DAT_ANNUL( OLOC, STATUS )
            GOTO 99
          END IF
*
        END IF
*
        CALL DAT_FIND(OLOC,OBJECT(K),LOC,STATUS)
        IF(KDIM(K).NE.0.AND.K.NE.NOB) THEN
*
          CALL DAT_SLICE(LOC,KDIM(K),KELS(1,K),KUPS(1,K),LOCA,STATUS)
*
          IF ( STATUS .NE. SAI__OK ) THEN
            WRITE(*,*) 'HDX_CREATE failed: dimension/slice ',
     :		   OBJECT(K),(KELS(I,K),I=1,KDIM(K))
            CALL DAT_ANNUL(OLOC,STATUS)
            CALL DAT_ANNUL(LOC,STATUS)
            GOTO 99
          END IF

          CALL DAT_ANNUL(LOC,STATUS)
          LOC=LOCA
*
        ENDIF
*
        IF ( K .LT. NOB ) THEN
          CALL DAT_ANNUL(OLOC,STATUS)
          OLOC = LOC
        ELSE
          LOCA = OLOC
          OLOC = LOC
          LOC = LOCA
        END IF

      END DO

*    Now check type and dimensions
      CALL DAT_TYPE( OLOC, STYPE, STATUS )
      CALL DAT_SIZE( OLOC, NTOT, STATUS )
      NELS=1
*
      DO J=1,NDIM
        NELS=NELS*IELS(J)
      END DO
      IF ( STYPE .NE. TYPE ) GOTO 20
      IF ( NELS .NE. NTOT ) GOTO 20
      CALL DAT_ANNUL( LOC, STATUS )
      GOTO 99

*    Here to modify type and dimensions
 20   CALL DAT_ERASE( LOC, OBJECT(NOB), STATUS )
      CALL DAT_NEW( LOC, OBJECT(NOB), TYPE, NDIM, IELS, STATUS )
      CALL DAT_FIND( LOC, OBJECT(NOB), OLOC, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from HDX_CREATE', STATUS )
      END IF

      END
