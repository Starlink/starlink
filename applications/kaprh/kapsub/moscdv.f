*+  MOSCDV - normalise mosaic 2-d data array with respect to mask values

      SUBROUTINE MOSCDV ( DIM1, DIM2, MASK, ARRAY, STATUS )

*    Description :
*
*     This routine takes a 2-d data array containing a mosaic of data
*     arrays and normalises it with respect to a mask. The value of a
*     given pixel in the mask array gives the number of valid images
*     that contributed 'flux' to that pixel in the data array. Thus,
*     this routine divides each pixel by the corresponding mask-array
*     value to normalise.
*
*    Invocation :
*
*     CALL MOSCDV ( DIM1, DIM2, MASK, ARRAY, STATUS )
*
*    Arguments :
*
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d arrays.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d arrays.
*     MASK( DIM1, DIM2 )  =  REAL( READ )
*         Array containing details of pixel contibutions
*     ARRAY( DIM1, DIM2 )  =  REAL( READ, WRITE )
*         Input array to be normalised
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     For all lines of input array
*        For all pixels of current line
*           If mask array pixel value is greater than one then
*              Divide data-array pixel value by mask-array pixel value
*           Endif
*        Endfor
*     Endfor
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*     Malcolm J. Currie RAL ( RAL::CUR )
*
*    History :
*
*     20-11-1986 : First implementation (HILO::MJM)
*     1988 May 31: Renamed from MOSAIC_DIV, and tidied to KAPPA
*                  conventions ( RAL::CUR )
*     1989 Aug  7: Passed array dimensions as separate variables
*                  (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE 'PRM_PAR'        ! PRIMDAT public constants

*    Import :

      INTEGER
     :  DIM1, DIM2             ! dimensions of input arrays

      REAL
     :  MASK( DIM1, DIM2 )     ! mask array

*    Import - Export :

      REAL
     :  ARRAY( DIM1, DIM2 )    ! data array to be normalised

*    Status :

      INTEGER  STATUS          ! global status parameter

*    Local variables :

      INTEGER
     :  I, J                   ! array counters

*-
*    Check status on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop round each line of the input array

      DO  J  =  1, DIM2

*       Loop round each pixel of the current line

         DO  I  =  1, DIM1

*          Check to see that at least two pixels have been added here
*          (and just to be safe that the pixel is not bad).

            IF ( MASK( I, J ) .GT. 1 .AND.
     :           ARRAY( I, J ) .NE. VAL__BADR ) THEN

*             Normalise the data-array pixel value

               ARRAY( I, J )  =  ARRAY( I, J ) / MASK( I, J )

            END IF

*       End of loop round pixels in current line of input data array

         END DO

*    End of loop round all lines of input data array

      END DO


*    Return and end

      END

