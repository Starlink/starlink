*+  STR_ROOT - Extracts 'root' of a filename, i.e. strips off any directory name
      SUBROUTINE STR_ROOT(INFIL, ROOT)
* Description :
*     <description of what the subroutine does>
* Restrictions :
*       Intended for VAX/VMS filename formats only
* History :
*         Author	Clive Page	1986 Jan 26.
*         Asterix88 version      May 1988      (LTVAD::RDS)
* Type Definitions :
        IMPLICIT NONE
* Import :
	CHARACTER*(*) INFIL           ! filename with or without extension
* Import-Export :
* Export :
	CHARACTER*(*) ROOT            ! filename with any node,dev,dir,
*                                     !      version specifications removed.
* Local constants :
* Local variables :
        INTEGER IEND,L
*-
C search for ]
	IEND = INDEX(INFIL,']')
C if not found then search for last colon or last /
	IF(IEND.EQ.0) THEN
C start at end and work backwards
		DO L=LEN(INFIL),1,-1
		IF(INFIL(L:L).EQ.':' .OR. INFIL(L:L).EQ.'/') THEN
			IEND = L
			GOTO 10
		END IF
		END DO
	ENDIF
*
10	CONTINUE
*
	ROOT = INFIL(IEND+1:)
	L = INDEX(ROOT, ';')
	IF(L .GT. 1) ROOT = ROOT(1:L-1)
*
	END
