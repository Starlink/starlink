*+  LOGARR - take the logarithm of an array, pixel by pixel

      SUBROUTINE LOGARR( INARR, DIMS, BASE, OUTARR, STATUS )
*
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
*     CALL LOGARR( INARR, DIMS, BASE, OUTARR, STATUS )
*
*    Arguments :
*
*     INARR( DIMS )  =  REAL( READ )
*         Array containing input image data
*     DIMS  =  INTEGER( READ )
*         Dimensions of input and output arrays
*     BASE  =  REAL( READ )
*         Base of logarithm to be used - must be a positive number
*     OUTARR( DIMS )  =  REAL( WRITE )
*         Array containing results of processing input data
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - if o.k. reurn immediately
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
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     03-07-1986 :  First implementation (REVA::MJM)
*     1986 Aug 12:  Renamed from LOGARR2D, generalised to any array,
*                   completed prologue and nearly conformed to 
*                   Starlink standards (RL.STAR::CUR).
*     1986 Sep 2 :  Renamed parameters -> arguments section in prologue
*                   and added bad-pixel handling (RL.CUR::STAR).
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE 'PRM_PAR'           ! PRIMDAT public constants

*    Import :

      INTEGER
     :    DIMS

      REAL
     :    INARR( DIMS ),
     :    BASE

*    Export :

      REAL
     :    OUTARR( DIMS )

*    Status :

      INTEGER  STATUS

*    Local variables :

      INTEGER
     :    J                       ! counter variable
   
      REAL
     :    FACTOR                  ! factor used to define base being
                                  ! used

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    proceed according to input value of base

      IF ( BASE .GT. 0.0 .AND. BASE .NE. 1.0 ) THEN

*       this is o.k. - work out factor to be used in taking logs to the
*       specified base

         FACTOR  =  1.0 / LOG( BASE )

*       now loop round all the pixels of the output image

         DO  J  =  1, DIMS

*          check current pixel value

            IF ( INARR( J ) .LE. 0.0 ) THEN

*             cannot take log of zero or negative - bad pixel

               OUTARR( J )  =  VAL__BADR

            ELSE

*             input array pixel is o.k. - work out output value

               OUTARR( J )  =  LOG( INARR( J ) ) * FACTOR

*          end of if-pixel-value-zero-or-negative check

            END IF

*       end of loop round all rows

         END DO

*    else base is one, zero, or negative

      ELSE

*       loop round all pixels in output array

         DO  J  =  1, DIMS
 
*          set output pixel value to bad

            OUTARR( J )  =  VAL__BADR

         END DO

*    end of if-base-not-one-zero-or-negative check

      END IF

 999  CONTINUE

*    return

      END
