*+  TASK_ENCNC - encode an array as a character string
      SUBROUTINE TASK_ENCNC ( NDIMS, DIMS, CVALS, STRING, STATUS )
*    Description :
*     Convert the given multidimensional array into characters and
*     concatenate the values into a string with separators. The
*     dimensions of the array are delimited by [] following the ADAM
*     syntax.
*     There is a routine for each type C, D, I, L, R.
*    Invocation :
*     CALL TASK_ENCNC ( NDIMS, DIMS, CVALS, STRING, STATUS )
*    Parameters :
*     NDIMS=INTEGER (given)
*           number of dimensions of the given array
*     DIMS(NDIMS)=INTEGER (given)
*           the dimensions of the given array
*     CVALS(1:*)=CHARACTER*(*) (given)
*           the given array, treated as a vector
*     STRING=CHARACTER*(*) (returned)
*           the returned string
*     STATUS=INTEGER
*    Method :
*     Convert each of the given values into an element in a 1-D array of
*     strings. Then call the ADAM string building routine to concatenate
*     the strings into a single string with the correct syntax.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     06.11.1987:  original (REVAD::BDK)
*     29.04.1989:  make it generic (AAOEPP::WFL)
*      4.10.1992:  use CHR (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
 
*    Import :
      INTEGER NDIMS        ! number of dimensions of the given array
 
      INTEGER DIMS(NDIMS)  ! the dimensions of the given array
 
      CHARACTER*(*) CVALS(1:*) ! the given array, treated as a vector
 
*    Export :
      CHARACTER*(*) STRING ! the returned string
 
*    Status :
      INTEGER STATUS
 
*    Local variables :
      CHARACTER*(40) CARRAY(20)  ! store for original type conversions
      INTEGER TOTNUM             ! number of items for conversion
      INTEGER J                  ! loop counter
      INTEGER NCHAR              ! size of encoded item
*-
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
      TOTNUM = 1
      DO J = 1, NDIMS
         TOTNUM = TOTNUM * DIMS(J)
      ENDDO
 
      J = 0
      DO WHILE ( ( J .LT. TOTNUM ) .AND. ( STATUS .EQ. SAI__OK ) )
         J = J + 1
         CALL CHR_CTOC( CVALS(J), CARRAY(J), NCHAR )
      ENDDO
 
      CALL STRING_BUILDARR ( NDIMS, DIMS, CARRAY, STRING, STATUS )
 
      END
