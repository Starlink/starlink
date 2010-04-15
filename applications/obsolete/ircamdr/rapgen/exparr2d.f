
*+  EXPARR2D - take the exponential of a 2-d array pixel by pixel

      SUBROUTINE EXPARR2D ( INARRAY, DIMS1, DIMS2, BASE, OUTARRAY,
     :                      STATUS )

*    Description :
*
*     This routine fills the output array pixels with the results
*     of taking the exponential of the pixels of the input array
*     to the base specified, i.e. New value = Base ** Old value.
*     If the result is bigger than the maximum allowed Vax real,
*     then a bad pixel value is used.
*
*    Invocation :
*
*     CALL EXPARR2D( INARRAY, DIMS, BASE, OUTARRAY, STATUS )
*
*    Parameters :
*
*     INARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( READ )
*         Array containing input image data
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of input and output arrays
*     BASE  =  REAL( READ )
*         Base of exponential to be used - must be a positive number
*     OUTARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( WRITE )
*         Array containing results of processing input data
*     STATUS  =  INTEGER( READ, WRITE )
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
*           If Inarray pixel has value greater than specified maximum then
*              Outarray pixel = Bad value
*           Else
*              Outarray pixel = Base ** Inarray pixel
*           Endif
*        Endfor
*     Else negative checking required
*        For all pixels of the output array
*           If Inarray pixel has value less than specified minimum then
*              Outarray pixel = Bad value
*           Else
*              Outarray pixel = Base ** Inarray pixel
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
*     02-07-1986 :  First implementation (REVA::MJM)
*     27-May-1994   Changed VAX specific max real to PRIMDAT's NUM__MAXR
*                   (SKL@JACH)
*     15-JUL-1994   Changed arguments so that DIMS are input separately
*                   so that routine will compile (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions
      INCLUDE  'PRM_PAR'          ! PRIMDAT constants
*    Import :

      INTEGER
     :    DIMS1,               ! dimensions of arrays
     :    DIMS2                ! dimensions of arrays

      REAL
     :    INARRAY( DIMS1, DIMS2 ),   ! first input array
     :    BASE                    ! base of exponential to be taken
                                  ! must be input as positive number

*    Export :

      REAL
     :    OUTARRAY( DIMS1, DIMS2 )  ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local constants :

      REAL
     :    VAXMAX,                 ! maximum valid real
     :    BADVAL                  ! 'bad pixel' value to be used
      PARAMETER( VAXMAX  =  NUM__MAXR )  ! No longer Vax specific
      PARAMETER( BADVAL  =  0.0 )      ! temporary usage

*    Local variables :

      INTEGER
     :    I, J                    ! counter variables

      REAL
     :    MAXVAL,                 ! maximum valid pixel value allowed for
                                  ! exponentiation
     :    MINVAL                  ! minimum valid pixel value allowed

      LOGICAL                     ! true if :
     :    POSCHECK                ! we are in danger from large positive
                                  ! pixels, as opposed to large negative ones

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    work out pixel value checking limits according to input value of base
      IF ( BASE .LT. 1.0 ) THEN

*       the danger pixel values are large negative ones - work out the limit
         MINVAL  =  LOG( VAXMAX ) / LOG( BASE )

*       set positive checking flag false, i.e. we will be checking for
*       excessively negative values
         POSCHECK  =  .FALSE.

      ELSE IF ( BASE .GT. 1.0 ) THEN

*       large positive pixel values will cause problems - work out limit
         MAXVAL  =  LOG( VAXMAX ) / LOG( BASE )

*       set positive checking flag true
         POSCHECK  =  .TRUE.

*    else base is one
      ELSE

*       any pixel value is o.k. as 1**anything = 1 - set limit to large
*       number
         MAXVAL  =  VAXMAX

*       set positive checking flag true
         POSCHECK  =  .TRUE.

*    end of if-base-value-less-than-one check
      END IF


*    depending on value of limit checking flag continue
      IF ( POSCHECK ) THEN

*       we will be checking for pixel values larger than the calculated
*       positive maximum
*       now loop round all rows of the output array
         DO  J  =  1, DIMS2

*          loop round all pixels of the current row
            DO  I  =  1, DIMS1

*             check pixel value for an value greater than the
*             specified maximum
               IF ( INARRAY( I, J ) .GE. MAXVAL ) THEN

*                too large a pixel value - stick the 'bad pixel' value
*                into the output pixel
                  OUTARRAY( I, J )  =  BADVAL

*             else a valid exponentiation may take place
               ELSE

*                set output pixel to be base to the power input pixel value
                  OUTARRAY( I, J )  =  BASE ** INARRAY( I, J )

*             end of if-pixel-value-too-large check
               END IF

            END DO

         END DO

*    else negative limit checking is required
      ELSE

*       now loop round all rows of the output array
         DO  J  =  1, DIMS2

*          loop round all pixels of the current row
            DO  I  =  1, DIMS1

*             check pixel value for an value less than the
*             specified minimum
               IF ( INARRAY( I, J ) .LE. MINVAL ) THEN

*                too negative a pixel value - stick the 'bad pixel' value
*                into the output pixel
                  OUTARRAY( I, J )  =  BADVAL

*             else a valid exponentiation may take place
               ELSE

*                set output pixel to be base to the power input pixel value
                  OUTARRAY( I, J )  =  BASE ** INARRAY( I, J )

*             end of if-pixel-value-too-negative check
               END IF

            END DO

         END DO

*    end of if-positive-limit-checking-required check
      END IF


*    return
      END
