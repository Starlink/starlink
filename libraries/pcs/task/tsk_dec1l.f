*+  TASK_DEC1L - decode a character string as a vector
      SUBROUTINE TASK_DEC1L ( STRING, MAXVALS, NVALS, LVALS,
     :                          STATUS )
*    Description :
*     Convert the given character string, which is assumed to have
*     the ADAM syntax for an array, that is the whole is surrounded by
*     [] and the elements of the array are separated, into a 1-D array.
*     There is a routine for each type C, D, I, L, R.
*    Invocation :
*     CALL TASK_DEC1L ( STRING, MAXVALS, NVALS, LVALS, STATUS )
*    Parameters :
*     STRING=CHARACTER*(*) (given)
*           the given character string
*     MAXVALS=CHARACTER*(*) (given)
*           the maximum number of values that can be returned
*     NVALS=INTEGER (returned)
*           number of values in the 1-D array
*     LVALS(NVALS)=LOGICAL (returned)
*           the returned 1-D array
*     STATUS=INTEGER
*    Method :
*     Call TASK_DECNL
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
 
*    Import :
      CHARACTER*(*) STRING  ! the given character string
 
      INTEGER MAXVALS       ! the maximum number of values in 1-D array
 
*    Export :
      INTEGER NVALS         ! the number of values in the 1-D array
 
      LOGICAL LVALS(1:*)   ! the returned 1-D array
 
*    Status :
      INTEGER STATUS
 
*    Local variables :
      INTEGER NMAXDIMS      ! max no of dimensions to return
      INTEGER NDIMS         ! no of dimensions in encoded string
*-
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      NMAXDIMS = 1
      CALL TASK_DECNL ( STRING, NMAXDIMS, MAXVALS, NDIMS, NVALS,
     :                    LVALS, STATUS )
 
      END
 
!*+  TASK_DEC1 - decode a character string as a value
!      SUBROUTINE TASK_DEC1
!*    Description :
!*     Dummy routine to allow MMS to maintain the object library properly
!
!      END
