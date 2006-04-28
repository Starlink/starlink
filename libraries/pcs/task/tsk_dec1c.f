      SUBROUTINE TASK_DEC1C ( STRING, MAXVALS, NVALS, CVALS,
     :                          STATUS )
*+
*  Name:
*     TASK_DEC1C

*  Purpose:
*     Decode a character string as a vector

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_DEC1C ( STRING, MAXVALS, NVALS, CVALS, STATUS )

*  Description:
*     Convert the given character string, which is assumed to have
*     the ADAM syntax for an array, that is the whole is surrounded by
*     [] and the elements of the array are separated, into a 1-D array.
*     There is a routine for each type C, D, I, L, R.

*  Arguments:
*     STRING=CHARACTER*(*) (given)
*           the given character string
*     MAXVALS=CHARACTER*(*) (given)
*           the maximum number of values that can be returned
*     NVALS=INTEGER (returned)
*           number of values in the 1-D array
*     CVALS(NVALS)=CHARACTER*(*) (returned)
*           the returned 1-D array
*     STATUS=INTEGER

*  Algorithm:
*     Call TASK_DECNC

*  Authors:
*     W.F.Lupton (AAOEPP::WFL)
*     {enter_new_authors_here}

*  History:
*     29-APR-1989 (AAOEPP::WFL):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
 
*  Arguments Given:
      CHARACTER*(*) STRING  ! the given character string
 
      INTEGER MAXVALS       ! the maximum number of values in 1-D array
 
*  Arguments Returned:
      INTEGER NVALS         ! the number of values in the 1-D array
 
      CHARACTER*(*) CVALS(1:*)   ! the returned 1-D array
 
*  Status:
      INTEGER STATUS
 
*  Local Variables:
      INTEGER NMAXDIMS      ! max no of dimensions to return
      INTEGER NDIMS         ! no of dimensions in encoded string
*.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      NMAXDIMS = 1
      CALL TASK_DECNC ( STRING, NMAXDIMS, MAXVALS, NDIMS, NVALS,
     :                    CVALS, STATUS )
 
      END
 
!*+  TASK_DEC1 - decode a character string as a value
!      SUBROUTINE TASK_DEC1
!*    Description :
!*     Dummy routine to allow MMS to maintain the object library properly
!
!      END
