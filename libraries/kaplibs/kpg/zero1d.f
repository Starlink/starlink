*+  ZERO1D - sets all elements of the 1-dimensional array to zero

      SUBROUTINE ZERO1D( SIZE, ARRAY, STATUS )
*
*    Description :
*
*     All elements of the 1-dimensional array, ARRAY, are set equal to
*     zero. An immediate return will occur if STATUS has an error value
*     on entry.
*
*    Invocation :
*
*     CALL ZERO1D( SIZE, ARRAY, STATUS )
*
*    Arguments :
*
*     SIZE   = INTEGER( READ )
*           Number of elements in the array to be set to all zeros.
*     ARRAY( SIZE ) = REAL( UPDATE )
*           All elements of this array will be set to zero.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then
*        For all elements in the array
*           Value of element becomes zero
*        Endfor
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*     12/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 12: Renamed parameters section to arguments and tidied
*                  (RL.STAR::CUR)
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER
     :  SIZE

*    Export :

      REAL
     :  ARRAY( SIZE )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X            ! pointer to elements of array
*-

*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       do all elements of array

         DO  X = 1, SIZE

*          set value of element to zero

            ARRAY( X ) = 0.0
         ENDDO
      ENDIF

      END
