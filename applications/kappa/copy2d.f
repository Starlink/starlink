*+  COPY2D - copy one 2-D array into another

      SUBROUTINE COPY2D( DIM1, DIM2, ARRIN, ARROUT, STATUS )
*
*    Description :
*
*     The input 2-D array, ARRIN, of dimension DIM1, DIM2, is copied
*     into the output 2-D array, ARROUT, of the same dimensions.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL COPY2D( DIM1, DIM2, ARRIN, ARROUT, STATUS )
*
*    Arguments :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d arrays.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d arrays.
*     ARRIN( DIM1, DIM2 ) = REAL( READ )
*         Array to be copied.
*     ARROUT( DIM1, DIM2 ) = REAL( WRITE )
*         Will be returned containing a copy of the input array.
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then
*        For all lines of the input and output arrays
*           For all the points in a line
*              Output array point is set to the value of input array
*                point
*           Endfor
*        Endfor
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     01/12/1983 : Original version                     (ROE::ASOC5)
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

      REAL
     :  ARRIN( DIM1, DIM2 )

*    Export :

      REAL
     :  ARROUT( DIM1, DIM2 )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X,                     ! Index to input/output array elements,
                               ! 1st dimension
     :  Y                      ! Index to input/output array elements,
                               ! 2nd dimension
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       for all lines of input/output arrays

         DO  Y = 1, DIM2

*          for all points in line

            DO  X = 1, DIM1

*             output array point is set to value of input array point

               ARROUT( X, Y ) = ARRIN( X, Y )
            END DO
         END DO
      END IF

      END
