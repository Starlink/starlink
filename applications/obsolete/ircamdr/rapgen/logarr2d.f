
*+  LOGARR2D - take the logarithm of a 2-d array pixel by pixel

      SUBROUTINE LOGARR2D( INARRAY, DIMS1, DIMS2, BASE, OUTARRAY,
     :                     STATUS )

*    Description :
*
*     This routine fills the output array pixels with the results
*     of taking the logarithm of the pixels of the input array
*     to the base specified, i.e. New value = Log    ( Old value ).
*                                                Base
*     If the input pixel is negative, then a bad pixel value is
*     used for the output value.
*     To find the logarithm to any base, the following algorithm
*     is used, providing the base is positive and not equal to one :
*      New Value  =  Log ( Old Value ) / Log ( Base )
*                       e                   e
*
*    Invocation :
*
*     CALL LOGARR2D( INARRAY, DIMS, BASE, OUTARRAY, STATUS )
*
*    Parameters :
*
*     INARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( READ )
*         Array containing input image data
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of input and output arrays
*     BASE  =  REAL( READ )
*         Base of logarithm to be used - must be a positive number
*     OUTARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( WRITE )
*         Array containing results of processing input data
*     STATUS  =  INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - if not o.k. return immediately
*     If input base is positive and not equal to one
*        Work out factor which defines the base we are working in
*         - factor = 1/loge( base)
*        For all pixels of output array
*           If Inarray value is positive then
*              Outarray value  =  loge( Inarray value ) / factor
*           Else
*              Outarray value  =  bad value
*           Endif
*        Endfor
*     Else base is negative or equal to one
*        For all pixels of output array
*           Outarray value  =  bad value
*        Endfor
*     Endif
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
*     03-07-1986 :  First implementation (REVA::MJM)
*     12-Aug-1994   Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMS1, DIMS2               ! dimensions of arrays

      REAL
     :    INARRAY( DIMS1, DIMS2 ),   ! first input array
     :    BASE                    ! base of logarithm to be taken -
                                  ! must be input as positive number

*    Export :

      REAL
     :    OUTARRAY( DIMS1, DIMS2 )  ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local constants :

      REAL
     :    BADVAL                  ! 'bad pixel' value to be used
      PARAMETER( BADVAL  =  0.0 )      ! temporary usage

*    Local variables :

      INTEGER
     :    I, J                    ! counter variables

      REAL
     :    FACTOR                  ! factor used to define base being used

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    proceed according to input value of base
      IF ( BASE .GT. 0.0 .AND. BASE .NE. 1.0 ) THEN

*       this is o.k. - work out factor to be used in taking logs to the
*       specified base
         FACTOR  =  1.0 / LOG( BASE )

*       now loop round all the rows of the output image
         DO  J  =  1, DIMS2

*          now all the pixels in the current row
            DO  I  =  1, DIMS1

*             check current pixel value
               IF ( INARRAY( I, J ) .LE. 0.0 ) THEN

*                cannot take log of zero or negative - bad pixel
                  OUTARRAY( I, J )  =  BADVAL

               ELSE

*                input array pixel is o.k. - work out output value
                  OUTARRAY( I, J )  =  LOG( INARRAY( I, J ) ) * FACTOR

*             end of if-pixel-value-zero-or-negative check
               END IF

*          end of loop round pixels in current row
            END DO

*       end of loop round all rows
         END DO

*    else base is one, zero, or negative
      ELSE

*       loop round all rows in output array
         DO  J  =  1, DIMS2

*          loop round all pixels in current row
            DO  I  =  1, DIMS1

*             set output pixel value to bad
               OUTARRAY( I, J )  =  BADVAL

            END DO

         END DO

*    end of if-base-not-one-zero-or-negative check
      END IF


*    return
      END
