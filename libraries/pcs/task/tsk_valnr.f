*+  TASK_VALNR - encode an array as a character string
      SUBROUTINE TASK_VALNR ( NDIMS, DIMS, RVALS, STRING, STATUS )
*    Description :
*     Convert the given multidimensional array into characters and
*     concatenate the values into a string with separators. The
*     dimensions of the array are delimited by [] following the ADAM
*     syntax.
*     There is a routine for each type C, D, I, L, R.
*    Invocation :
*     CALL TASK_VALNR ( NDIMS, DIMS, RVALS, STRING, STATUS )
*    Parameters :
*     NDIMS=INTEGER (given)
*           number of dimensions of the given array
*     DIMS(NDIMS)=INTEGER (given)
*           the dimensions of the given array
*     RVALS(1:*)=REAL (given)
*           the given array, treated as a vector
*     STRING=CHARACTER*(*) (returned)
*           the returned string
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
*     29.04.1989:  make it generic (same as TASK_ENCNR) (AAOEPP::WFL)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
 
*    Import :
      INTEGER NDIMS        ! number of dimensions of the given array
 
      INTEGER DIMS(NDIMS)  ! the dimensions of the given array
 
      REAL RVALS(1:*) ! the given array, treated as a vector
 
*    Export :
      CHARACTER*(*) STRING ! the returned string
 
*    Status :
      INTEGER STATUS
*-
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      CALL TASK_ENCNR ( NDIMS, DIMS, RVALS, STRING, STATUS )
 
      END
