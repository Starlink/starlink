*+  ROOT2D - takes the square root of 2-d array pixel by pixel

      SUBROUTINE ROOT2D ( INARRAY, DIMS1, DIMS2, OUTARRAY, STATUS )

*    Description :
*
*     This routine fills the output array pixels with the results
*     of taking the square root of the input array
*     specified, i.e. New value = Old value ** 0.5
*
*    Invocation :
*
*     CALL ROOT2D( INARRAY, DIMS, OUTARRAY, STATUS )
*
*    Parameters :
*
*     INARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( READ )
*         Array containing input image data
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of input and output arrays
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
*     11-OCT-1994   Changed DIM arguments for UNIX compiler (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMS1, DIMS2               ! dimensions of arrays

      REAL
     :    INARRAY( DIMS1, DIMS2 )   ! first input array

*    Export :

      REAL
     :    OUTARRAY( DIMS1, DIMS2 )  ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local constants :

*    Local variables :

      INTEGER
     :    I, J                    ! counter variables

*-
*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) THEN

         RETURN

      ENDIF

*    we will be checking for pixel values smaller than 0.0
*    now loop round all rows of the output array
      DO  J  =  1, DIMS2

*       loop round all pixels of the current row
         DO  I  =  1, DIMS1

*         check that input not illegal for this operation
            IF ( INARRAY( I, J ) .LE. 0.0) THEN

*            set output to 0.0 if input illegal
               OUTARRAY( I, J )  =  0.0

*          else a valid calculation may take place
            ELSE

*             set output pixel to input pixel value rooted
               OUTARRAY( I, J )  =  SQRT( INARRAY( I, J ))

*          end of if-pixel-value-too-large check
            END IF

         END DO

      END DO

*    return
      END
