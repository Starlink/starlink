*+  TASK_ENC1R - encode a vector as a character string
      SUBROUTINE TASK_ENC1R ( NVALS, RVALS, STRING, STATUS )
*    Description :
*     Convert the given 1-D array into characters and concatenate the
*     result into a string with the ADAM syntax, that is the elements of
*     the array are separated and the whole is surrounded by [].
*     There is a routine for each type C, D, I, L, R.
*    Invocation :
*     CALL TASK_ENC1R ( NVALS, RVALS, STRING, STATUS )
*    Parameters :
*     NVALS=INTEGER (given)
*           number of values in the 1-D array
*     RVALS(NVALS)=REAL (given)
*           the array to be converted
*     STRING=CHARACTER*(*) (returned)
*           the returned character string
*     STATUS=INTEGER
*    Method :
*     Call TASK_ENCNR
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     06.11.1987:  original (REVAD::BDK)
*     29.04.1989:  make it generic (AAOEPP::WFL)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
 
*    Import :
      INTEGER NVALS         ! number of values in the 1-D array
 
      REAL RVALS(NVALS) ! the array to be encoded
 
*    Export :
      CHARACTER*(*) STRING  ! the returned character string
 
*    Status :
      INTEGER STATUS
 
*    Local variables :
      INTEGER NDIMS         ! no of dimensions to be passed
*-
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      NDIMS = 1
      CALL TASK_ENCNR ( NDIMS, NVALS, RVALS, STRING, STATUS )
 
      END
