*+  ZERO2D - sets all elements of a 2-dimensional array to zero

      SUBROUTINE ZERO2D( DIM1, DIM2, ARRAY, STATUS )
*
*    Description :
*
*     All elements of the 2-dimensional array, ARRAY, are set equal to
*     zero. An immediate return will occur if STATUS has an error value
*     on entry.
*
*    Invocation :
*
*     CALL ZERO2D( DIM1, DIM2, ARRAY, STATUS )
*
*    Arguments :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array to be set to all zeros.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array to be set to all zeros.
*     ARRAY( DIM1, DIM2 ) = REAL( UPDATE )
*           All elements of this array will be set to zero.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then
*        For all rows of the array
*           For all elements in a row
*              Value of element becomes zero
*           Endfor
*        Endfor
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR)
*
*    History :
*     05/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 12: Renamed parameters section to arguments and tidied
*                  (RL.STAR::CUR)
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER
     :  DIM1, DIM2

*    Import-Export :

      REAL
     :  ARRAY( DIM1, DIM2 )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X,                     ! Pointer to element along first
                               ! dimension of array
     :  Y                      ! Pointer to element along second
                               ! dimension of array
*-

*    Check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Do all rows of the array

         DO  Y = 1, DIM2

*          Do all elements of each row

            DO  X = 1, DIM1

*             Set value of element to zero

               ARRAY( X, Y ) = 0.0
            END DO
         END DO
      END IF

      END
