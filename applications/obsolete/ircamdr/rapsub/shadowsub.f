*+  SHADOWSUB - creates a shadowed enhancement with integer shifting

      SUBROUTINE SHADOWSUB ( INARRAY, DIMS1, DIMS2, IXSHIFT, IYSHIFT,
     :                       OUTARRAY, STATUS )

*    Description :
*
*     This routine shifts the input array by the specified integer
*     amounts in both the x and y directions, and then subtracts
*     the input array from the shifted array to create the output,
*     which is then a shadow enhanced version of the input. Both
*     the shifting and subtraction are in fact done in the same loop.
*
*    Invocation :
*
*     CALL SHADOWSUB( INARRAY, NDIMS, DIMS, IXSHIFT, IYSHIFT,
*    :                OUTARRAY, STATUS )
*
*    Method :
*
*     Check for error on entry - if not o.k. return immediately
*     For all pixels of output image
*        If pixel has come from valid input array pixel after shifting then
*           Output pixel = shifted input pixel - unshifted input pixel
*        Else
*           Output pixel = 0
*        Endif
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
*     09-01-1986 :  First implementation
*                :  (REVA::MJM)
*     11-Oct-1994   Changed DIM arguments for UNIX compiler (SKL@JACH)
*
*    Type definitions :

      IMPLICIT  NONE              ! no implicit typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :   DIMS1,           ! x dimensions of input image
     :   DIMS2,           ! y dimensions of input image
     :   IXSHIFT,         ! integer pixel shift in x direction
     :   IYSHIFT          !    "      "     "    " y     "

      REAL
     :   INARRAY( DIMS1, DIMS2 )     ! input array

*    Export :

      REAL
     :   OUTARRAY( DIMS1, DIMS2 )  ! output array

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :   I, J,            ! counters for pixels in output array
     :   II, JJ           ! counters for corresponding pixels in
                          ! shifted input array

      LOGICAL
     :   INSIDE           ! true if current output pixel has come
                          ! from a valid shifted input pixel

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    loop round all the rows of the output array
      DO  J  =  1, DIMS2

*       evaluate the row in the input array from which the current
*       output row was shifted
         JJ  =  J - IYSHIFT

*       loop round all the pixels in the current row of the output
         DO  I  =  1, DIMS1

*          evaluate the x coord of the pixel in the input array
*          from which the current output array pixel was shifted
            II  =  I - IXSHIFT

*          evaluate the logical INSIDE, which is true if the current
*          output array pixel has a valid counterpart in the unshifted
*          input array
            INSIDE  =  ( II .GT. 1 .AND. II .LE. DIMS1 .AND.
     :                   JJ .GT. 1 .AND. JJ .LE. DIMS2 )

*          if INSIDE is true, then subtract the value of the current
*          unshifted pixel from the value of the corresponding
*          shifted pixel to get the output value - else set it
*          to zero
            IF ( INSIDE ) THEN
               OUTARRAY( I, J )  =  INARRAY( II, JJ ) - INARRAY( I, J )
            ELSE
               OUTARRAY( I, J )  =  0.0
            END IF

         END DO

      END DO

*    return

      END

