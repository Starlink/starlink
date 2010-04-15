C+
C                       D S A _ E R R _ T O _ V A R I A N C E
C
C  Routine name:
C     DSA_ERR_TO_VARIANCE
C
C  Function:
C     Converts a standard deviation error array to a variance array
C
C  Description:
C     This routine is passed the address of an array of specified
C     type and size that contains standard deviation error values.
C     It converts these into variance values in a second array of
C     the same type and size whose address it is also passed.  The
C     two arrays may actually be the same arrays in the calling
C     program.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_ERR_TO_VARIANCE (NELM,TYPE,ERR_ADDR,VAR_ADDR,ERRORS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NELM        (Integer,ref) The number of elements to be converted.
C     (>) TYPE        (Fixed string,descr) The type of the array elements
C                     to be converted.  This can be any of the recognised
C                     DTA types ('INT', 'FLOAT', etc), and is not case-
C                     sensitive.
C     (>) ERR_ADDR    (Integer,ref) The address of the start of the array
C                     containing the SD errors to be converted.
C     (>) VAR_ADDR    (Integer,ref) The address of the start of the array
C                     containing the resulting variance values.
C     (<) ERRORS      (Integer,ref) The number of elements that gave rise
C                     to conversion errors.
C
C  External variables used:  None.
C
C  External subroutines / functions used:
C     DSA_SQUARE_{x}, CNF_PVAL
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Subroutine / function details:
C     DSA_SQUARE_{x}    Square the elements of an array.
C
C  History:
C     27th June 1989.   Original version.  KS / AAO.
C     21st Aug 1992     Automatic portability modifications
C                       ("INCLUDE" syntax etc) made. KS/AAO
C     29th Aug 1992     "INCLUDE" filenames now upper case. KS/AAO
C     2005 June 3       Replace DYNAMIC_MEMORY with
C                       %VAL(CNF_PVAL(ADDRESS)) contruct for 64-bit
C                       addressing.  MJC / Starlink
C+
      SUBROUTINE DSA_ERR_TO_VARIANCE (NELM,TYPE,ERR_ADDR,VAR_ADDR,
     :                                                         ERRORS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER NELM, ERR_ADDR, VAR_ADDR, ERRORS
      CHARACTER*(*) TYPE
C
C     Functions used
C
      INTEGER ICH_FOLD
C
C     Local variables
C
      CHARACTER CHR*1        ! First character of TYPE
      INTEGER   INVOKE       ! Dummy function value
C
      CHR=TYPE(1:1)
      INVOKE=ICH_FOLD(CHR)
      ERRORS=0
      IF (CHR.EQ.'B') THEN
         CALL DSA_SQUARE_B (NELM,%VAL(CNF_PVAL(ERR_ADDR)),
     :                      %VAL(CNF_PVAL(VAR_ADDR)),ERRORS)
      ELSE IF (CHR.EQ.'F') THEN
         CALL DSA_SQUARE_F (NELM,%VAL(CNF_PVAL(ERR_ADDR)),
     :                      %VAL(CNF_PVAL(VAR_ADDR)),ERRORS)
      ELSE IF (CHR.EQ.'D') THEN
         CALL DSA_SQUARE_D (NELM,%VAL(CNF_PVAL(ERR_ADDR)),
     :                      %VAL(CNF_PVAL(VAR_ADDR)),ERRORS)
      ELSE IF (CHR.EQ.'U') THEN
         CALL DSA_SQUARE_U (NELM,%VAL(CNF_PVAL(ERR_ADDR)),
     :                      %VAL(CNF_PVAL(VAR_ADDR)),ERRORS)
      ELSE IF (CHR.EQ.'I') THEN
         CALL DSA_SQUARE_I (NELM,%VAL(CNF_PVAL(ERR_ADDR)),
     :                      %VAL(CNF_PVAL(VAR_ADDR)),ERRORS)
      ELSE IF (CHR.EQ.'S') THEN
         CALL DSA_SQUARE_S (NELM,%VAL(CNF_PVAL(ERR_ADDR)),
     :                      %VAL(CNF_PVAL(VAR_ADDR)),ERRORS)
      END IF
C
      END
