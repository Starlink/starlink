************************************************************************

	BLOCK DATA FIXED_PAR

*	Initializes the free_list common block so that no parameter
*	is fixed.

	IMPLICIT NONE

	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )

	INTEGER INDEX( M_PAR )
	INTEGER TABLE_FREE( M_PAR )
	COMMON / FREE_LIST / INDEX, TABLE_FREE

	DATA INDEX / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
     1			13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
     2			23, 24, 25, 26, 27, 28, 29, 30, 31, 32 /
	DATA TABLE_FREE / M_PAR * 0 /

	END

************************************************************************

	BLOCK DATA LINKED_POS

*	Initializes the link_status common block so that no parameter
*	is linked.

	IMPLICIT NONE

	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )

	INTEGER L_INDEX( M_PAR )
	REAL L_OFFSET( M_PAR )
	COMMON / LINK_STATUS / L_INDEX, L_OFFSET

	DATA L_INDEX / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
     1			13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
     2			23, 24, 25, 26, 27, 28, 29, 30, 31, 32 /
	DATA L_OFFSET / M_PAR * 0.0 /

	END

************************************************************************

	BLOCK DATA BOUNDARY

*	Set up the default boundary within which parameters are confined.

	IMPLICIT NONE

	INTEGER M_PAR
	REAL N_LARGE, P_LARGE
	PARAMETER( M_PAR = 32 )
	PARAMETER( N_LARGE = -1.0E+36, P_LARGE = 1.0E+36 )

	REAL LO_A( M_PAR )
	REAL HI_A( M_PAR )
	COMMON / BOUNDARY_PAR / LO_A, HI_A

	DATA LO_A / M_PAR * N_LARGE /
	DATA HI_A / M_PAR * P_LARGE /

	END

************************************************************************
	BLOCK DATA CYCLIC_PAR

	IMPLICIT NONE

	INTEGER M_PAR
	PARAMETER( M_PAR = 32 )

	INTEGER LIST_CYCLIC( M_PAR )
	REAL TAB_PERIOD( M_PAR )
	REAL TAB_BOTTOM( M_PAR )
	COMMON / CYCLIC_LIST / LIST_CYCLIC, TAB_PERIOD, TAB_BOTTOM

	DATA LIST_CYCLIC / M_PAR * 0 /
	DATA TAB_PERIOD / M_PAR * 0.0 /
	DATA TAB_BOTTOM / M_PAR * 0.0 /

	END

************************************************************************
	BLOCK DATA CYCLIC_FUNCTION

	IMPLICIT NONE

	LOGICAL CYCLIC_DATA
	REAL CYCLE_DATA
	COMMON / DATA_CYCLIC / CYCLIC_DATA, CYCLE_DATA

	DATA CYCLIC_DATA / .FALSE. /
	DATA CYCLE_DATA / 0.0 /

	END

************************************************************************
	BLOCK DATA SCALE_CHANGE

	IMPLICIT NONE

	REAL TAME_FACTOR
	COMMON / SCALE_DOWN / TAME_FACTOR

	DATA TAME_FACTOR / 1.0 /

	END





