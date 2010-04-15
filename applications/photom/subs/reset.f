************************************************************************

	SUBROUTINE RESET( )

*+
*  Name :
*     RESET
*
*  Purpose :
*     Erm, take a guess?
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL RESET()
*
*  Description :
*     Resets the MARQUART COMMON blocks
*
*  Arguments :
*     None
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     KM: Koji Mukai (Oxford University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     08-FEB-1999
*        Cut and hack for Starlink
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-


	IMPLICIT NONE

	INTEGER M_PAR
	REAL N_LARGE, P_LARGE
	PARAMETER( M_PAR = 32 )
	PARAMETER( N_LARGE = -1.0E+36, P_LARGE = 1.0E+36 )

	INTEGER K

	INTEGER INDEX( M_PAR )
	INTEGER TABLE_FREE( M_PAR )
	INTEGER L_INDEX( M_PAR )
	REAL L_OFFSET( M_PAR )
	REAL LO_A( M_PAR )
	REAL HI_A( M_PAR )
	INTEGER LIST_CYCLIC( M_PAR )
	REAL TAB_PERIOD( M_PAR )
	REAL TAB_BOTTOM( M_PAR )
	LOGICAL CYCLIC_DATA
	REAL CYCLE_DATA
	REAL TAME_FACTOR
	COMMON / FREE_LIST / INDEX, TABLE_FREE
	COMMON / LINK_STATUS / L_INDEX, L_OFFSET
	COMMON / BOUNDARY_PAR / LO_A, HI_A
	COMMON / CYCLIC_LIST / LIST_CYCLIC, TAB_PERIOD, TAB_BOTTOM
	COMMON / DATA_CYCLIC / CYCLIC_DATA, CYCLE_DATA
	COMMON / SCALE_DOWN / TAME_FACTOR

	DO K = 1, M_PAR
	  INDEX( K ) = K
	  TABLE_FREE( K ) = 0
	  L_INDEX( K ) = K
	  L_OFFSET( K ) = 0.0
	  LO_A( K ) = N_LARGE
	  HI_A( K ) = P_LARGE
	  LIST_CYCLIC( K ) = 0
	  TAB_PERIOD( K ) = 0.0
	  TAB_BOTTOM( K ) = 0.0
	END DO
	CYCLIC_DATA = .FALSE.
	CYCLE_DATA = 0.0
	TAME_FACTOR = 1.0

	END
