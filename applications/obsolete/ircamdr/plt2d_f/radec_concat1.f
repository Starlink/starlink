	SUBROUTINE RADEC_CONCAT1( ST1, ST2, ST3, ST4, POS, STRING)

*       History
*       22-Jul-1994 Changed STR$ calls to CHR_ (SKL@JACH)
*
	IMPLICIT NONE

        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'

	INTEGER L1, L2, L3
	INTEGER POS( 5)

	CHARACTER*( *) ST1, ST2, ST3, ST4
	CHARACTER*80 STRING

        CALL CHR_CLEAN( ST1 )
        L1 = 0
	CALL CHR_APPND( ST1, ST1, L1)
        CALL CHR_CLEAN( ST2 )
        L2 = 0
	CALL CHR_APPND( ST2, ST2, L2)
        CALL CHR_CLEAN( ST3 )
        L3 = 0
	CALL CHR_APPND( ST3, ST3, L3)

	POS( 1) = L1 + 1
	POS( 2) = L1 + 1 + L2 + 1
	POS( 3) = L1 + 1 + L2 + 1 + L3 + 1
	POS( 4) = 0
	POS( 5) = 0

	STRING = ST1 // ' ' // ST2 // ' ' // ST3 // '.' // ST4

	END
