      SUBROUTINE  FINDPEAKSUB ( NX, NY, ARRIN, XST, YST, XEN, YEN,
     :	                        XPEAK, YPEAK, PEAK, STATUS )

*    Description :
*
*    Invocation :
*
*    Method :
*
*    Authors :
*
*    History :
*
*     15/09/1983 : Original version (ROE::ASOC5)
*     21-10-1985 : Tidied up (REVA::MJM)
*
*    Type Definitions :

      IMPLICIT NONE             ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'         ! global SSE parameters

*    Status :

      INTEGER STATUS            ! global status parameter

*    Local Constants :

      INTEGER NDIMS
      PARAMETER ( NDIMS = 2 )   ! 2 dimensional arrays

*    Local variables :

      INTEGER
     :	NX,
     :	NY,
     :	J,
     :	K,
     :	XST,
     :	YST,
     :	XEN,
     :	YEN,
     :	XPEAK,
     :	YPEAK

      REAL
     :  ARRIN( NX, NY),   ! input data array
     :  PEAK

*-
*    check status on entry - return if not o.k.
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    initialize peak variables
      PEAK = -1.0E20

*    loop round all pixels in input array
      DO J = YST, YEN
        DO K = XST, XEN

	  IF( ARRIN( K, J) .GT. PEAK) THEN
	    PEAK = ARRIN( K, J)
	    XPEAK = K
	    YPEAK = J
	  END IF

        END DO
      END DO

      END
