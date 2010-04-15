      SUBROUTINE NDP_DISPLAY_PROGRESS(AXIS,COUNT)
C+
C
C   ---------------------------------------
C   N D P _ D I S P L A Y _ P R O G R E S S
C   ---------------------------------------
C
C   Description
C   -----------
C   Displays the progress message `Done output axis x element n' when
C   the element count on a given axis is a multiple of 10.
C
C
C   Parameters
C   ----------
C   AXIS   (> integer). Axis number.
C   COUNT  (> integer). Element count.
C
C
C   External functions & subroutines called
C   ---------------------------------------
C   DSA library:
C   DSA_WRUSER
C
C   INCLUDE statements
C   ------------------
C   None.
C
C
C   Extensions to FORTRAN77
C   -----------------------
C   IMPLICIT NONE / Names > 6 characters
C
C   Author/s
C   --------
C   Nick Fuller  RGO  (RGVAD::NMJF or NMJF@UK.AC.RGO.STAR)
C   Julian Gold  RGO  (CAVAD::GOLDJIL or GOLDJIL@UK.AC.CAM.AST-STAR)
C
C   History
C   -------
C   01-FEB-1989   - Original program
C   27-NOV-1992   - Unix version (GOLDJIL)
C   06-OCT-1994   - Removed unused variables. (GJP)
C
C
C+-----------------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER   AXIS
      INTEGER   COUNT
      CHARACTER STRING*64
C
      IF((REAL(COUNT)/10.0).EQ.REAL(COUNT/10))THEN
        WRITE(STRING,'(A,I1,A,I4)')
     &    '  Done output axis ',AXIS,' element ',COUNT
	CALL DSA_WRUSER(STRING//'\\N')
      END IF
C
      END
