*+  GLTBSB - automatically deglitches bad pixels from a 2-d array

      SUBROUTINE GLTBSB ( DIM1, DIM2, ARRAY, NODEGL, STATUS )

*    Description :
*
*     This routine deglitches undefined pixels from a 2-d array. The
*     input array is searched for pixels that are bad, as defined by the
*     magic-value method. Each time a bad pixel is found, the
*     it is replaced by the value of the local median using the normal
*     deglitching subroutine. Thus the input array is updated.  The
*     number of deglitched pixels is returned.
*
*    Invocation :
*
*     CALL GLTBSB( DIM1, DIM2, ARRAY, NODEGL, STATUS )
*
*    Parameters :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     ARRAY( DIM1, DIM2 )  =  REAL( WRITE )
*         Output deglitched 2-d array
*     NODEGL  =  INTEGER( WRITE )
*         Number of deglitched bad pixels
*     STATUS  =  INTEGER( READ, WRITE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise counter of deglitched pixels
*     For all lines in the input array
*        For all pixels in the current line
*           If current pixel value equals the bad pixel value then
*              Call GLTCSB to do the deglitching
*              Increment deglitched-pixel counter by one
*           Endif
*        Endfor
*     Endfor
*     Return
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     1988 Mar 17 : First implementation (RL.STAR::CUR).
*     1989 Jul 27 : Passed array dimensions as separate variables
*                   (RL.STAR::CUR).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants

*    Import :

      INTEGER
     :    DIM1, DIM2           ! dimensions of input array

*    Import-Export :

      REAL
     :    ARRAY( DIM1, DIM2 )  ! array with bad pixels which is then
                               ! deglitched

*    Export :

      INTEGER
     :    NODEGL

*    Status :

      INTEGER  STATUS          ! global status parameter

*    Local variables :

      REAL
     :    OLDVAL,              ! old value for current pixel
     :    NEWVAL               ! new (deglitched) value for current
                               ! pixel
      INTEGER
     :    I, J                 ! loop counters

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .EQ. SAI__OK ) THEN

*       initialise the counter

         NODEGL   =  0

*       loop round all the lines of the input array

         DO  J  =  1, DIM2

*          loop round all the pixels in the current line

            DO  I  =  1, DIM1

*             see if current pixel is bad

               IF ( ARRAY( I, J ) .EQ. VAL__BADR ) THEN

*                pixel at current position is bad - deglitch it

                  CALL GLTCSB( DIM1, DIM2, I, J, ARRAY, OLDVAL, NEWVAL,
     :                         STATUS )

*                increment deglitch counter by one

                  NODEGL  =  NODEGL + 1

*             end of if-pixel-at-current-position-is-bad check

               END IF

*          end of loop round current line

            END DO

*       end of loop round all lines

         END DO

      END IF

*    return and end

      END

