*+  LAPLACESUB - subtracts Laplacian of input image to form output

      SUBROUTINE LAPLACESUB ( INARRAY, DIMS1, DIMS2, NUMBER,
     :                        OUTARRAY, STATUS )

*    Description :
*
*     This routine subtracts NUMBER times the Laplacian of the input
*     array from that array to create the output array. This can be
*     thought of as a convolution by
*
*                             -N   -N   -N
*                             -N   +8N  -N
*                             -N   -N   -N
*
*     where N is the integer number NUMBER. The convolution acts as
*     a unidirectional edge detector, and for a flat areas in the
*     input array, the convolution sums to zero.
*
*    Invocation :
*
*     CALL LAPLACESUB( INARRAY, DIMS, NUMBER, OUTARRAY, STATUS )
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Set the edges of the output array to zero
*     For all rows in array bar edges
*        For all pixels in current row bar edges
*           Evaluate convolution from pixels in 3x3 box round
*             current pixel in input array
*           Insert this value in poistion in output array
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
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     10-12-1985 : First implementation (UKTH::MARK)
*     17-04-1986 : Tidied and modified (REVA::MJM)
*     15-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! global SSE definitions

*    Import :

      INTEGER
     :     DIMS1, DIMS2,    ! dimensions of input and output arrays
     :     NUMBER           ! number of times Laplacian is subtracted

      REAL
     :     INARRAY( DIMS1, DIMS2 )   ! input array

*    Export :

      REAL
     :     OUTARRAY( DIMS1, DIMS2 )  ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :     I, J, II, JJ           ! general counter variables

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    first set the edges of the output array to zero
      DO  I  =  1, DIMS1
         OUTARRAY( I, 1 )          =  0.0
         OUTARRAY( I, DIMS2 )  =  0.0
      END DO

      DO  J  =  1, DIMS2
         OUTARRAY( 1, J )          =  0.0
         OUTARRAY( DIMS1, J )  =  0.0
      END DO

*    now perform the Laplacian convolution - loop round all rows
*    except edges
      DO  JJ  =  2, ( DIMS2 - 1 )

*       loop round all pixels except edge ones in current row
         DO  II  =  2, ( DIMS1 - 1 )

            OUTARRAY( II, JJ )  =  NUMBER *
     :               ( 8 * INARRAY( II, JJ ) ) -
     :               (     INARRAY( II+1, JJ )   +
     :                     INARRAY( II-1, JJ )   +
     :                     INARRAY( II, JJ-1 )   +
     :                     INARRAY( II, JJ+1 )   +
     :                     INARRAY( II+1, JJ+1 ) +
     :                     INARRAY( II+1, JJ-1 ) +
     :                     INARRAY( II-1, JJ+1 ) +
     :                     INARRAY( II-1, JJ-1 ) )

*       end of loop round pixels in current row
         END DO

*    end of loop round rows
      END DO


*    end and return
      END
