*+  EXPARR - take the exponential of an array, pixel by pixel

      SUBROUTINE EXPARR ( INARR, DIMS, BASE, OUTARR, STATUS )
*
*    Description :
*
*     This routine fills the output array pixels with the results
*     of taking the exponential of the pixels of the input array 
*     to the base specified, i.e. New value = Base ** Old value.
*     If the result is bigger than the maximum-allowed Vax real,
*     then a bad pixel value is used.
*
*    Invocation :
*
*     CALL EXPARR( INARR, DIMS, BASE, OUTARR, STATUS )
*
*    Arguments :
*
*     INARR( DIMS )  =  REAL( READ )
*         Array containing input image data
*     DIMS  =  INTEGER( READ )
*         Dimension of input and output arrays
*     BASE  =  REAL( READ )
*         Base of exponential to be used - must be a positive number
*     OUTARR( DIMS )  =  REAL( WRITE )
*         Array containing results of processing input data
*     STATUS  =  INTEGER( READ )
*         Global status value
*
*    Method :
*
*     Check for error on entry - if not o.k. return immediately
*     If input base is less than one then
*        Large negative pixels will cause us to overflow - work out
*         negative limit and set positive checking flag false
*     Elseif input base is greater than one then
*        Large positive pixels will cause us to overflow - work out
*         positive limit and set positive checking flag true
*     Else base is one
*        Any input pixel value is o.k. - set maximum allowable pixel
*         value to be very large and set positive checking flag true
*     Endif
*     If positive checking then
*        For all pixels of the output array
*           If Inarray pixel has value greater than specified maximum
*             then
*              Outarray pixel = Bad value
*           Else
*              If input pixel is valid
*                 Outarray pixel = Base ** Inarray pixel
*              Else
*                 Outarray pixel = Bad value
*              Endif
*           Endif
*        Endfor
*     Else negative checking required
*        For all pixels of the output array
*           If Inarray pixel has value less than specified minimum then
*              Outarray pixel = Bad value
*           Else
*              If input pixel is valid
*                 Outarray pixel = Base ** Inarray pixel
*              Else
*                 Outarray pixel = Bad value
*              Endif
*           Endif
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
*     02-07-1986 :  First implementation (REVA::MJM)
*     1986 Aug 12:  Renamed from EXPARR2D, generalised to any array,
*                   completed prologue and nearly conformed to 
*                   Starlink standards (RL.STAR::CUR).
*     1986 Sep 2 :  Renamed parameters -> arguments section in prologue,
*                   corrected bad-pixel handling and tidied 
*                   (RL.CUR::STAR).
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
     :    J                       ! counter variables 
   
      REAL
     :    MAXVAL,                 ! maximum valid pixel value allowed
                                  ! for exponentiation
     :    MINVAL                  ! minimum valid pixel value allowed

      LOGICAL                     ! true if :
     :    POSCHK                  ! we are in danger from large positive
                                  ! pixels, as opposed to large
                                  ! negative ones

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    work out pixel value checking limits according to input value of
*    base

      IF ( BASE .LT. 1.0 ) THEN

*       the danger pixel values are large negative ones - work out the
*       limit

         MINVAL  =  LOG( ABS( VAL__BADR ) ) / LOG( BASE )

*       set positive checking flag false, i.e. we will be checking for
*       excessively negative values

         POSCHK  =  .FALSE.

      ELSE IF ( BASE .GT. 1.0 ) THEN

*       large positive pixel values will cause problems - work out limit

         MAXVAL  =  LOG( ABS( VAL__BADR ) ) / LOG( BASE )

*       set positive checking flag true

         POSCHK  =  .TRUE.

*    else base is one

      ELSE

*       any pixel value is o.k. as 1**anything = 1 - set limit to large
*       number

         MAXVAL  =  ABS( VAL__BADR )

*       set positive checking flag true

         POSCHK  =  .TRUE.

*    end of if-base-value-less-than-one check

      END IF

*    depending on value of limit checking flag continue

      IF ( POSCHK ) THEN

*       we will be checking for pixel values larger than the calculated
*       positive maximum
*       now loop round all pixels of the output array

         DO  J  =  1, DIMS

*          specified maximum

            IF ( INARR( J ) .GE. MAXVAL ) THEN

*             too large a pixel value - stick the 'bad pixel' value 
*             into the output pixel

               OUTARR( J )  =  VAL__BADR

*             else a valid exponentiation may take place

            ELSE

               IF ( INARR( J ) .NE. VAL__BADR ) THEN

*                set output pixel to be base to the power input pixel
*                value for valid pixels

                  OUTARR( J )  =  BASE ** INARR( J )
               ELSE
                  OUTARR( J )  =  VAL__BADR
               END IF

*             end of if-pixel-value-too-large check

            END IF

         END DO

*    else negative limit checking is required

      ELSE

*       now loop round all pixels of the output array

         DO  J  =  1, DIMS

*          check pixel value for an value less than the specified
*          minimum

            IF ( INARR( J ) .LE. MINVAL ) THEN

*             too negative a pixel value - stick the 'bad pixel' value 
*             into the output pixel

               OUTARR( J )  =  VAL__BADR

*          else a valid exponentiation may take place

            ELSE

               IF ( INARR( J ) .NE. VAL__BADR ) THEN

*                set output pixel to be base to the power input pixel
*                value for valid pixels

                  OUTARR( J )  =  BASE ** INARR( J )
               ELSE
                  OUTARR( J )  =  VAL__BADR
               END IF

*          end of if-pixel-value-too-negative check

            END IF

         END DO

*    end of if-positive-limit-checking-required check

      END IF

 999  CONTINUE

*    return

      END
