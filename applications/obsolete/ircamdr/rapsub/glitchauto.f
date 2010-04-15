
*+  GLITCHAUTO - deglitches an image searching automatically for bad pixels

      SUBROUTINE GLITCHAUTO ( DIMS1, DIMS2, ARRAY, BADVAL, STATUS )

*    Description :
*
*     This routine deglitches an image. The input array is automatically
*     searched for an input pixel value, which is defined to be the bad
*     pixel value. Each time one is found, the deglitching subroutine is
*     called to replace the bad pixel value with the value of the local
*     median. The array is updated each time a bad pixel is found, and
*     final resulting array returned.
*
*    Invocation :
*
*     CALL GLITCHAUTO( DIMS, ARRAY, BADVAL; STATUS )
*
*    Parameters :
*
*     DIMS( 2 )  =  INTEGER( READ )
*         Dimensions of image
*     ARRAY( DIMS( 1 ), DIMS( 2 ) )  =  REAL( WRITE )
*         Output deglitched image
*     BADVAL  =  REAL( READ )
*         Pixel value (defined as bad) to be searched for
*     STATUS  =  INTEGER( READ, WRITE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise deglitched pixel counter
*     For all rows in the input array
*        For all pixels in the current row
*           If current pixel value equals the bad pixel value then
*              Call GLITCHSUB to do the business
*              Increment deglitched pixel counter by one
*           Endif
*        Endfor
*     Endfor
*     Output final value of deglitched pixel counter
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
*     13-12-1986 : First implementation (UKTH::MJM)
*     15-JULY-1994 Changed arguments to input DIMS separately so that
*                  routine will compile (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMS1,               ! dimensions of input array
     :    DIMS2                ! dimensions of input array

      REAL
     :    BADVAL                  ! value defined to be a bad pixel

*    Import-Export :

      REAL
     :    ARRAY( DIMS1, DIMS2 )  ! array holding deglitched data

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    DONE,                   ! number of valid positions deglitched
     :    I, J                    ! counters

      REAL
     :    OLDVAL,                 ! old value for current pixel
     :    NEWVAL                  ! new (deglitched) value for current pixel

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    initialise the counter
      DONE   =  0

*    loop round all the rows of the input array
      DO  J  =  1, DIMS2

*       loop round all the pixels in the current row
         DO  I  =  1, DIMS1

*          check the current pixel for 'goodness'
            IF ( ARRAY( I, J ) .EQ. BADVAL ) THEN

*             position is bad - deglitch it
               CALL GLITCHSUB( DIMS1, DIMS2, ARRAY, I, J, OLDVAL,
     :                         NEWVAL, STATUS )

*             increment valid deglitch counter by one
               DONE  =  DONE + 1

*          end of if-pixel-position-is-bad check
            END IF

*       end of loop round current row
         END DO

*    end of loop round all rows
      END DO

*    output message to give the number of pixel positions deglitched
      CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_SETI( 'DONE', DONE )
      CALL MSG_OUT( 'NDONE',
     : ' Number of valid positions deglitched  =  ^DONE', STATUS )
      CALL MSG_OUT( 'BLANK', ' ', STATUS )


*    return and end
      END
