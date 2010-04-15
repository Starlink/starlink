
*+  POWARR2D - raise a 2-d array pixel by pixel to a given power

      SUBROUTINE POWARR2D ( INARRAY, DIMS1, DIMS2, POWER, OUTARRAY,
     :                      STATUS )

*    Description :
*
*     This routine fills the output array pixels with the results
*     of raising the pixels of the input array to the power
*     specified, i.e. New value = Old value ** Power.
*     If the result is bigger than the maximum allowed Vax real,
*     then a bad pixel value is used.
*     Negative input values are also converted to a bad pixel value
*     in the output.
*
*    Invocation :
*
*     CALL POWARR2D( INARRAY, DIMS, POWER, OUTARRAY, STATUS )
*
*    Parameters :
*
*     INARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( READ )
*         Array containing input image data
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of input and output arrays
*     POWER  =  REAL( READ )
*         Power to be used
*     OUTARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( WRITE )
*         Array containing results of processing input data
*     STATUS  =  INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - if not o.k. return immediately
*     If input power is negative then
*        Very small pixel values may cause us to overflow - work out
*         minimum limit and set large checking flag false
*     Else input power is greater than zero
*        Large positive pixels may cause us to overflow - work out
*         maximum limit and set large checking flag true
*     Endif
*     If large checking then
*        For all pixels of the output array
*           If Inarray pixel is negative or has value greater
*            than specified maximum then
*              Outarray pixel = Bad value
*           Else
*              Outarray pixel = Inarray pixel ** Power
*           Endif
*        Endfor
*     Else small checking required
*        For all pixels of the output array
*           If Inarray pixel is negative or has value less than
*            specified minimum then
*              Outarray pixel = Bad value
*           Else
*              Outarray pixel = Inarray pixel ** Power
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
*
*    History :
*
*     03-07-1986 :  First implementation (REVA::MJM)
*     27-May 1994   Changed 'vaxmax' value to PRIMDAT's NUM__MAX
*                   for machine independence (SKL@JACH)
*     14-Jul-1994   Changed arguments to input DIMS separately
*                   so that routine will compile  (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PRM_PAR'          ! PRIMDAT constants

*    Import :

      INTEGER
     :    DIMS1,               ! dimensions of arrays
     :    DIMS2               ! dimensions of arrays

      REAL
     :    INARRAY( DIMS1, DIMS2 ),   ! first input array
     :    POWER                   ! power to be used

*    Export :

      REAL
     :    OUTARRAY( DIMS1, DIMS2 )  ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local constants :

      REAL
     :    VAXMAX,                 ! maximum valid real
     :    BADVAL                  ! 'bad pixel' value to be used
      PARAMETER( VAXMAX  =  NUM__MAXR ) ! no longer VAX specific
      PARAMETER( BADVAL  =  0.0 )      ! temporary usage

*    Local variables :

      INTEGER
     :    I, J                    ! counter variables

      REAL
     :    MAXVAL,                 ! maximum valid pixel value allowed
     :    MINVAL                  ! minimum   "     "     "      "

      LOGICAL                     ! true if :
     :    LARGECHECK              ! we are in danger from large pixel values
                                  ! as opposed to small ones

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

      MAXVAL = 0.0
      MINVAL = 0.0

*    work out pixel value checking limits according to input value of power
      IF ( POWER .LT. 0.0 ) THEN

*       when the power is negative, the danger pixel values are small
*       positive ones - work out the limit
         MINVAL  =  EXP( LOG( VAXMAX ) / POWER )

*       set large checking flag false, i.e. we will be checking for
*       numbers which are too small
         LARGECHECK  =  .FALSE.

      ELSE IF ( POWER .GT. 0.0 ) THEN

*       large positive pixel values will cause problems - work out limit
         MAXVAL  =  EXP( LOG( VAXMAX ) / POWER )

*       set large checking flag true
         LARGECHECK  =  .TRUE.

*    else power is zero
      ELSE

*       any pixel value is o.k. as anything**0 = 1 - set limit to large
*       number
         MAXVAL  =  VAXMAX

*       set large checking flag true
         LARGECHECK  =  .TRUE.

*    end of if-power-value-less-than-one check
      END IF


*    depending on value of limit checking flag continue
      IF ( LARGECHECK ) THEN

*       we will be checking for pixel values larger than the calculated
*       positive maximum
*       now loop round all rows of the output array
         DO  J  =  1, DIMS2

*          loop round all pixels of the current row
            DO  I  =  1, DIMS1

*             check pixel value for an value greater than the
*             specified maximum or negative
               IF ( INARRAY( I, J ) .GE. MAXVAL .OR.
     :              INARRAY( I, J ) .LE. 0.0 ) THEN

*               check if input is negative
	         IF( INARRAY( I, J) .LE. 0.0) THEN

*                 check if power is even
	           IF( IFIX( POWER/2.0)*2 .EQ. IFIX( POWER)) THEN

*                   value and power ok so raise to power
	             OUTARRAY( I, J) = ABS( INARRAY( I, J))**POWER

	           ELSE

*                   stick the 'bad pixel' value into the output pixel
                     OUTARRAY( I, J )  =  BADVAL

	           END IF

	         ELSE

*                 stick the 'bad pixel' value into the output pixel
                   OUTARRAY( I, J )  =  BADVAL

	         END IF

*             else a valid calculation may take place
               ELSE

*                set output pixel to input pixel value raised to the power
                  OUTARRAY( I, J )  =  INARRAY( I, J ) ** POWER

*             end of if-pixel-value-too-large check
               END IF

            END DO

         END DO

*    else small limit checking is required
      ELSE

*       now loop round all rows of the output array
         DO  J  =  1, DIMS2

*          loop round all pixels of the current row
            DO  I  =  1, DIMS1

*             check pixel value for an value smaller than the
*             specified minimum or negative
               IF ( INARRAY( I, J ) .LE. MINVAL .OR.
     :              INARRAY( I, J ) .LE. 0.0 ) THEN

*               check if input is negative
	         IF( INARRAY( I, J) .LE. 0.0) THEN

*                 check if power is even
	           IF( IFIX( POWER/2.0)*2 .EQ. IFIX( POWER)) THEN

*                   value and power ok so raise to power
	             OUTARRAY( I, J) = ABS( INARRAY( I, J))**POWER

	           ELSE

*                   stick the 'bad pixel' value into the output pixel
                     OUTARRAY( I, J )  =  BADVAL

	           END IF

	         ELSE

*                 stick the 'bad pixel' value into the output pixel
                   OUTARRAY( I, J )  =  BADVAL

	         END IF

*             else a valid calculation may take place
               ELSE

*                set output pixel to input pixel value raised to the power
                  OUTARRAY( I, J )  =  INARRAY( I, J ) ** POWER

*             end of if-pixel-value-too-small check
               END IF

            END DO

         END DO

*    end of if-large-limit-checking-required check
      END IF


*    return
      END
