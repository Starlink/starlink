*+BSORTD In situ bubble sort
	SUBROUTINE BSORTD (DLIST, NLIST, IND)
	IMPLICIT NONE
* Input
	INTEGER		NLIST			! Length of list
	INTEGER		IND (NLIST)		! Sorted pointer index
	DOUBLE PRECISION DLIST (NLIST)		! Unsorted/Sorted List
* M Denby Feb-88
*-
* Local
	INTEGER		NL, J, I		! Loop counters
	INTEGER		ITEMP			! Temp storage
	DOUBLE PRECISION DTEMP			! Temp storage

* Prime the sorted index list
	DO NL = 1, NLIST
	  IND(NL) = NL
	ENDDO

* Bubble sort the list and index list
	DO J = 2, NLIST
	  DO I = J, 2, -1
	    IF (DLIST(I) .GE. DLIST(I-1)) GOTO 100
	    DTEMP = DLIST(I)
	    DLIST(I) = DLIST(I-1)
	    DLIST(I-1) = DTEMP

	    ITEMP = IND(I)
	    IND(I) = IND(I-1)
	    IND(I-1) = ITEMP
	  ENDDO
100	ENDDO

	END
