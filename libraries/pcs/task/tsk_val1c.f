*+  TASK_VAL1C - encode a vector as a character string
      SUBROUTINE TASK_VAL1C ( NVALS, CVALS, STRING, STATUS )
*    Description :
*     Convert the given 1-D array into characters and concatenate the
*     result into a string with the ADAM syntax, that is the elements of
*     the array are separated and the whole is surrounded by [].
*     There is a routine for each type C, D, I, L, R.
*    Invocation :
*     CALL TASK_VAL1C ( NVALS, CVALS, STRING, STATUS )
*    Parameters :
*     NVALS=INTEGER (given)
*           number of values in the 1-D array
*     CVALS(NVALS)=CHARACTER*(*) (given)
*           the array to be converted
*     STRING=CHARACTER*(*) (returned)
*           the returned character string
*     STATUS=INTEGER
*    Method :
*     Call TASK_ENC1C
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     06.11.1987:  original (REVAD::BDK)
*     29.04.1989:  make it generic (same as TASK_ENC1C) (AAOEPP::WFL)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
 
*    Import :
      INTEGER NVALS         ! number of values in the 1-D array
 
      CHARACTER*(*) CVALS(NVALS) ! the array to be encoded
 
*    Export :
      CHARACTER*(*) STRING  ! the returned character string
 
*    Status :
      INTEGER STATUS
*-
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      CALL TASK_ENC1C ( NVALS, CVALS, STRING, STATUS )
 
      END
