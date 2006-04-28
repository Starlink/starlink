      SUBROUTINE TASK_VALNI ( NDIMS, DIMS, IVALS, STRING, STATUS )
*+
*  Name:
*     TASK_VALNI

*  Purpose:
*     Encode an array as a character string

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_VALNI ( NDIMS, DIMS, IVALS, STRING, STATUS )

*  Description:
*     Convert the given multidimensional array into characters and
*     concatenate the values into a string with separators. The
*     dimensions of the array are delimited by [] following the ADAM
*     syntax.
*     There is a routine for each type C, D, I, L, R.

*  Arguments:
*     NDIMS=INTEGER (given)
*           number of dimensions of the given array
*     DIMS(NDIMS)=INTEGER (given)
*           the dimensions of the given array
*     IVALS(1:*)=INTEGER (given)
*           the given array, treated as a vector
*     STRING=CHARACTER*(*) (returned)
*           the returned string
*     STATUS=INTEGER

*  Algorithm:
*     Call TASK_ENCNI

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-NOV-1987 (REVAD::BDK):
*        Original
*     29-APR-1989 (AAOEPP::WFL):
*        Make it generic (same as TASK_ENCNI)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
 
*  Arguments Given:
      INTEGER NDIMS        ! number of dimensions of the given array
 
      INTEGER DIMS(NDIMS)  ! the dimensions of the given array
 
      INTEGER IVALS(1:*) ! the given array, treated as a vector
 
*  Arguments Returned:
      CHARACTER*(*) STRING ! the returned string
 
*  Status:
      INTEGER STATUS
*.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      CALL TASK_ENCNI ( NDIMS, DIMS, IVALS, STRING, STATUS )
 
      END
