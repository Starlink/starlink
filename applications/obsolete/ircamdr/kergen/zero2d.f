*+  ZERO2D - sets all elements of a 2-dimensional array to zero
      SUBROUTINE ZERO2D( DIMSX, DIMSY, ARRAY, STATUS )
*    Description :
*     All elements of the 2-dimensional array, ARRAY, are set equal to zero.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL ZERO2D( DIMS, ARRAY, STATUS )
*    Parameters :
*     DIMS( 2 ) = INTEGER( READ )
*           Dimensions of the 2-D array to be set to all zeros.
*     ARRAY( DIMS(1), DIMS(2) ) = REAL( UPDATE )
*           All elements of this array will be set to zero.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        For all rows of the array
*           For all elements in a row
*              Value of element becomes zero
*           Endfor
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     05/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMSX, ! dimensions of array
     :  DIMSY ! dimensions of array
*    Import-Export :
      REAL
     :  ARRAY( DIMSX, DIMSY ) ! array to be set to all zeros
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X, ! pointer to element along first dimension of array
     :  Y  !    "     "    "      "   second    "      "   "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       do all rows of the array
         DO Y = 1, DIMSY

*          do all elements of each row
            DO X = 1, DIMSX

*             set value of element to zero
               ARRAY( X, Y ) = 0.0
            ENDDO
         ENDDO
      ENDIF

      END
