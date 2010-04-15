*+  MOSCORSUB3 - mosaics 2 images together with dc correction

      SUBROUTINE MOSCORSUB3 ( IDIMSX1, IDIMSY1, ARRIN1, IDIMSX2,
     :	                      IDIMSY2, ARRIN2, ODIMSX, ODIMSY, ARROUT,
     :	                      NXOFF, NYOFF, DCOFF, STATUS )

*    Description :
*
*     This routine mosaics 2 images together with dc correction of 2nd
*
*    Invocation :
*
*     CALL MOSCORSUB3 ( IDIMSX1, IDIMSY1, ARRIN1, IDIMSX2, IDIMSY2, ARRIN2,
*                       ODIMSX, ODIMSY, ARROUT, NXOFF, NYOFF, DCOFF, STATUS )
*
*    Parameters :
*
*     IDIMSX1  =  INTEGER( READ )
*     IDIMSY1  =  INTEGER( READ )
*         Dimensions of input image
*     ARRIN1( IDIMSX, IDIMSY )  =  REAL( READ )
*         Input image
*     IDIMSX2  =  INTEGER( READ )
*     IDIMSY2  =  INTEGER( READ )
*         Dimensions of input image
*     ARRIN2( IDIMSX, IDIMSY )  =  REAL( READ )
*         Input image
*     ODIMSX  =  INTEGER( READ )
*     ODIMSY  =  INTEGER( READ )
*         Dimensions of output image
*     ARROUT( ODIMSX, ODIMSY )  =  REAL( READ )
*         Output image
*     NXOFF  =  REAL( read )
*         X offset
*     NYOFF  =  REAL( read )
*         Y offset
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
     :    IDIMSX1,             ! dimensions of input image
     :    IDIMSY1,             ! dimensions of input image
     :    IDIMSX2,             ! dimensions of input image
     :    IDIMSY2,             ! dimensions of input image
     :    ODIMSX,             !      "      " output "     "
     :    ODIMSY,             !      "      " output "     "
     :	  J,
     :	  K,
     :	  NXOFF,
     :	  NYOFF,
     :	  XPIX,
     :	  YPIX,
     :	  npix

      REAL
     :    ARRIN1( IDIMSX1, IDIMSY1 ),   ! input image
     :    ARRIN2( IDIMSX2, IDIMSY2 ),   ! input image
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

*    initialize the output array to -infinity
      DO J = 1, ODIMSY
        DO K = 1, ODIMSY
	  ARROUT( K, J) = -1.0E38
        END DO
      END DO

*    loop around 1st image adding it into output image
      DO J = 1, IDIMSY1

        DO K = 1, IDIMSX1

	  IF( NXOFF .GE. 0 .AND. NYOFF .GE. 0) THEN
	    XPIX = K
	    YPIX = J
	  ELSE IF( NXOFF .GE. 0 .AND. NYOFF .LT. 0) THEN
	    XPIX = K
	    YPIX = J+ABS( NYOFF)
	  ELSE IF( NXOFF .LT. 0 .AND. NYOFF .GE. 0) THEN
	    XPIX = K+ABS( NXOFF)
	    YPIX = J
	  ELSE
	    XPIX = K+ABS( NXOFF)
	    YPIX = J+ABS( NYOFF)
	  END IF

	  ARROUT( XPIX, YPIX) = ARRIN1( K, J)

	END DO

      END DO

*    loop around 2nd image adding it into output image + subtracting dc
*    offset
      DO J = 1, IDIMSY2

        DO K = 1, IDIMSX2

	  IF( NXOFF .GE. 0 .AND. NYOFF .GE. 0) THEN
	    XPIX = K+ABS( NXOFF)
	    YPIX = J+ABS( NYOFF)
	  ELSE IF( NXOFF .GE. 0 .AND. NYOFF .LT. 0) THEN
	    XPIX = K+ABS( NXOFF)
	    YPIX = J
	  ELSE IF( NXOFF .LT. 0 .AND. NYOFF .GE. 0) THEN
	    XPIX = K
	    YPIX = J+ABS( NYOFF)
	  ELSE
	    XPIX = K
	    YPIX = J
	  END IF

	  IF( ARROUT( XPIX, YPIX) .LT. -0.9E38) THEN
	    NPIX = 1
	  ELSE
	    NPIX = 2
	  END IF

	  ARROUT( XPIX, YPIX) =
     :	   ( ARROUT( XPIX, YPIX)+( ARRIN2( K, J)-DCOFF))/NPIX

	END DO

      END DO

*    check for unset pixels and set to 0
      DO J = 1, ODIMSY
        DO K = 1, ODIMSY
	  IF( ARROUT( K, J) .LT. -0.9E38) THEN
	    ARROUT( K, J) = 0.0
	  END IF
        END DO
      END DO

      END
