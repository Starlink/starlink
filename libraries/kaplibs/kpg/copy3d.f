*+  COPY3D - copy one 3-D array into another

      SUBROUTINE COPY3D( DIM1, DIM2, DIM3, ARRIN, ARROUT, STATUS )
*
*    Description :
*
*     The input 3-D array, ARRIN, of dimensions in DIM1, DIM2, DIM3, is
*     copied into the output 3-D array, ARROUT, of the same dimensions.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL COPY3D( DIM1, DIM2, DIM3, ARRIN, ARROUT, STATUS )
*
*    Arguments :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d arrays.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d arrays.
*     DIM3 = INTEGER( READ )
*         The third dimension of the 2-d arrays.
*     ARRIN( DIM1, DIM2, DIM3 ) = REAL( READ )
*         Array to be copied.
*     ARROUT( DIM1, DIM2, DIM3 ) = REAL( WRITE )
*         Will be returned containing a copy of the input array.
*     STATUS = INTEGER( READ )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur.
*
*    Method :
*
*     If no error on entry then
*        For all bands of the input and output arrays
*           For all the lines of a band
*              For all the points in a line
*                 Output array point is set to value of input array
*                   point
*              Endfor
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
*     02/12/1983 : Original version                     (ROE::ASOC5)
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
     :  DIM1, DIM2, DIM3

      REAL
     :  ARRIN( DIM1, DIM2, DIM3 )

*    Export :

      REAL
     :  ARROUT( DIM1, DIM2, DIM3 )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  X,                     ! Index to input/output array elements,
                               ! 1st dimension
     :  Y,                     ! Index to input/output array elements,
                               ! 2nd dimension
     :  Z                      ! Index to input/output array elements,
                               ! 3rd dimension
*-

*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       for all bands of input/output arrays

         DO  Z = 1, DIM3

*          for all lines of band

            DO  Y = 1, DIM2

*             for all points in line

               DO  X = 1, DIM1

*                output array point is set to value of input array point

                  ARROUT( X, Y, Z ) = ARRIN( X, Y, Z )
               END DO
            END DO
         END DO
      END IF

      END
