*+EXPOS_L_SRCE Read the uncorrected source list
	SUBROUTINE EXPOS_L_SRCE (LOC, SRA, SDEC, SCOUNT, NS, STATUS)
	IMPLICIT NONE

	include 'CONSTANTS.INC'
	INCLUDE 'DAT_PAR'
* Input
	CHARACTER*(DAT__SZLOC)	LOC		! Top loactor
* Output
	DOUBLE PRECISION SRA(*)         ! List of RAs
	DOUBLE PRECISION SDEC(*)        ! List of DECs
	REAL		SCOUNT(*)	! List of raw counts
	INTEGER		NS		! Length of lists
	INTEGER		STATUS		! HDS status
* M. Denby Feb-90
* P. McGale Jan 93. convert RAs and DECs from degrees to rads.
* P. McGale Sept 94.  UNIX changes
*-
* local
	integer		i
	CHARACTER*(DAT__SZLOC)	POS		! HDS locators
	CHARACTER*(DAT__SZLOC)	CEL
	CHARACTER*(DAT__SZLOC)	RA
	CHARACTER*(DAT__SZLOC)	DEC
	CHARACTER*(DAT__SZLOC)	FLU

* Find the CEL_COORDS structure
	CALL DAT_FIND (LOC, 'POSIT', POS, STATUS)
	CALL CMP_GET0I(POS, 'NSRC', NS, STATUS)

	IF (NS.GT.0 .AND. NS.LE.40000) THEN
* Find the coordinate and count arrays
	  CALL DAT_FIND (POS, 'CEL_COORDS', CEL, STATUS)

* Read the RA and DEC lists, convert to rads.
	  CALL DAT_FIND  (CEL, 'RA', RA, STATUS)
	  CALL CMP_GET1D (RA, 'DATA_ARRAY', 40000, SRA, NS, STATUS)
	  CALL DAT_FIND  (CEL, 'DEC', DEC, STATUS)
	  CALL CMP_GET1D (DEC, 'DATA_ARRAY', 40000, SDEC, NS, STATUS)
	  do i=1,ns
	    sra(i)  = sra(i)  * dtor
	    sdec(i) = sdec(i) * dtor
	  enddo

* Find the FLuX structure
	  CALL DAT_FIND  (POS, 'FLUX', FLU, STATUS)
	  CALL CMP_GET1R (FLU, 'DATA_ARRAY', 40000, SCOUNT, NS, STATUS)
	ELSE
	  WRITE(*,*) '   Warning in EXPOS_L_SRCE - NSRCE is 0 or >40000'
	ENDIF

	IF (STATUS .NE. 0) THEN
	  WRITE(*,*) '   Error in EXPOS_L_SRCE'
	ENDIF

	END

