*+  STR_REALIST - Decodes string containing list of reals separated by commas
	SUBROUTINE STR_REALIST(STRING, MAXVAL, NVALS, ARRAY)
*    Description :
*    History :
*     29-Sep 1988 original (LTVAD::RDS)   based on MX_REALIST by Mike Watson
*    Type Definitions :
        IMPLICIT NONE
*    Import :
	CHARACTER*(*) STRING	! String.   e.g. 4.0,-99.99,34.567
	INTEGER MAXVAL		! Max length of output array ARRAY.
*    Import-Export :
*    Export :
	INTEGER NVALS		! No of elements returned in ARRAY.
	REAL    ARRAY(MAXVAL)	! List of One item per array element
*    Functions :
        INTEGER CHR_LEN
*    Local constants :
*    Local variables :
	CHARACTER*1 COMMA
        INTEGER I,K,LENMAX,L
        INTEGER KOMPOS          ! Comma position
        INTEGER LENSUB
*    Local data :
	DATA COMMA/','/
*-
* Initialisations :
	I = 1
	NVALS = 0
	LENMAX = CHR_LEN(STRING)
*
* Loop over string characters.
	DO K=1,MAXVAL
*
	   IF (I.GT.LENMAX) THEN
	      NVALS = K - 1
	      GOTO 999
	   ENDIF
*
	   L = INDEX(STRING(I:),COMMA)
*
* Found a comma, get string and convert to real
	   IF (L.GT.0) THEN
	      KOMPOS = I + L - 1
	      LENSUB = KOMPOS - I
	      READ(STRING(I:KOMPOS-1),1000) ARRAY(K)
1000	      FORMAT(BN,F<LENSUB>.0)
              I = KOMPOS + 1
*
* No more commas
	   ELSE
	      LENSUB = LENMAX - I + 1
	      READ(STRING(I:LENMAX),1000) ARRAY(K)
	      NVALS = K
	      GO TO 999
	   ENDIF

	ENDDO
*
999	CONTINUE
*
	END
