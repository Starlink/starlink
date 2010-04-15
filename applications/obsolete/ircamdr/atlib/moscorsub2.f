*+  MOSCORSUB2 - applies correction to image after moscor

      SUBROUTINE MOSCORSUB2 ( IDIMSX, IDIMSY, ARRIN, ODIMSX, ODIMSY,
     :	                      ARROUT, DCOFF, STATUS )

*    Description :
*
*     This routine subtracts a constant from an image
*
*    Invocation :
*
*     CALL MOSCORSUB2 ( IDIMSX, IDIMSY, ARRIN, ODIMSX, ODIMSY, ARROUT,
*                       DCOFF, STATUS )
*
*    Parameters :
*
*     IDIMSX  =  INTEGER( READ )
*     IDIMSY  =  INTEGER( READ )
*         Dimensions of input image
*     ARRIN( IDIMSX, IDIMSY )  =  REAL( READ )
*         Input image
*     ODIMSX  =  INTEGER( READ )
*     ODIMSY  =  INTEGER( READ )
*         Dimensions of output image
*     ARROUT( ODIMSX, ODIMSY )  =  REAL( READ )
*         Output image
*     DCOFF  =  REAL( read )
*         Calculated d.c. sky offset between first and second image
*     STATUS  =  INTEGER( UPDATE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
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
*     07-01-1988 : First implementation (UKTH::MJM)
*     02-01-1990 : This version adapted from MOFFDCSUB (JACH::CAA)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    IDIMSX,             ! dimensions of input image
     :    IDIMSY,             ! dimensions of input image
     :    ODIMSX,             !      "      " output "     "
     :    ODIMSY,             !      "      " output "     "
     :	  J,
     :	  K

      REAL
     :    ARRIN( IDIMSX, IDIMSY ),   ! input image
     :    ARROUT( ODIMSX, ODIMSY ),  ! output image
     :    DCOFF                      ! calculated  d.c. sky offset

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    loop around image subtracting dc offset
      DO J = 1, MIN( ODIMSY, IDIMSY)

        DO K = 1, MIN( ODIMSX, IDIMSX)

	  ARROUT( K, J) = ARRIN( K, J) - DCOFF

	END DO

      END DO

      END
