*+EXPOS_L_CSRCE Write the corrected source list
	SUBROUTINE EXPOS_L_CSRCE (LOC, CFLUX, NS, STATUS)
	IMPLICIT NONE
	INCLUDE 'DAT_PAR'
* Input
	CHARACTER*(DAT__SZLOC)  LOC		! Top loactor
	REAL	  	     CFLUX(*)	  	! Corrected flux list
	INTEGER		     NS		        ! Length of list
* Output
	INTEGER		STATUS		! HDS status
* M. Denby Feb-90
* P .McGale Sept 94 - UNIX changes
*-
* local
        INTEGER         CFPTR           ! Mapped CFLUX field

*      Create new field
        CALL SSO_CREFLD( LOC, 'CFLUX', '_REAL', STATUS )

*      Write units
        CALL SSO_PUTFITEM0C( LOC, 'CFLUX', 'UNITS', 7, 'count/s',
     :                                                   STATUS )

*      Map field and write data
        CALL SSO_MAPFLD( LOC, 'CFLUX', '_REAL', 'WRITE', CFPTR,
     :                                                STATUS )
        CALL ARR_COP1R( NS, CFLUX, %VAL(CFPTR), STATUS )
        CALL SSO_UNMAPFLD( LOC, 'CFLUX', STATUS )

	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in EXPOS_L_CSRCE'
	ENDIF

	END
