*+  IMG_POLIDX - Fill index array with polar bin number
      SUBROUTINE IMG_POLIDX( NX, NY, POLX, POLY, RBIN,
     :                      AZANG, NRAD, NSECTOR, INDEX, STATUS )
     :
*
*    Description :
*
*     Produces a two dimensional index array describing the polar bin
*     number at X,Y given the polar attributes.
*
*    Author :
*
*     Dick Willingale (LTVAD::RW)
*
*    History :
*      5 Mar 1996 (DJA):
*        Original version
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*
*    Import :
*
      INTEGER			NX,NY            	! Input dimensions
							! square axis units
      REAL 			POLX,POLY               ! Centre of polar
      REAL 			RBIN                    ! Radial binsize (pixels)
      REAL 			AZANG                   ! Initial azimuthal angle (degs)
      INTEGER 			NRAD,NSECTOR            ! Output dimensions
*
*    Export :
*
      INTEGER 			INDEX(NX,NY)
*
*    Status :
*
      INTEGER 			STATUS
*
*    Local constants :
*
      REAL 			TWOPI
      PARAMETER 		( TWOPI = MATH__PI*2.0 )
*
*    Local variables :
*
      REAL 			ABIN                    ! Azimuthal binsize (rads)
      REAL 			AZRAD                   ! Azimnuth start in
                                                        ! radians
      REAL 			YOLD,XOLD
      REAL 			RAD                     ! Radius of pixel from centre
      REAL 			THETA

      INTEGER 			J,K,KK,JJ
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set angular bin size
      ABIN = TWOPI/MAX(REAL(NSECTOR),1.0)

*  Convert initial azimuthal angle into radians
      AZRAD = AZANG * MATH__DTOR

*  Now raster through array
      DO J = 1, NY
        YOLD=(REAL(J)-0.5)-POLY

        DO K = 1, NX

	  XOLD=(REAL(K)-0.5)-POLX

*      Calculate radius of element
	  RAD = SQRT(XOLD**2+YOLD**2)
          KK = INT( RAD / RBIN ) + 1
          KK = MIN( NRAD, KK )

*      Find position angle of element
          IF ( NSECTOR .GT. 1 ) THEN
	    THETA = ATAN2(YOLD,XOLD)

*        Subtract the initial azimuth angle
            THETA = THETA - AZRAD

*        Force position angle to be in range 0 to TWOPI
	    IF ( THETA .LT. 0.0 ) THETA=TWOPI+THETA
	    IF ( THETA .LT. 0.0 ) THETA=TWOPI+THETA
	    IF ( THETA .GT. TWOPI ) THETA=THETA-TWOPI
	    IF ( THETA .GT. TWOPI ) THETA=THETA-TWOPI
            JJ = INT(THETA/ABIN) + 1
            INDEX(K,J) = KK + (JJ-1)*NSECTOR
          ELSE
            INDEX(K,J) = KK
          END IF

*      End loop over 1st dimension
	END DO

*    End loop over 2nd dimension
      END DO

      END
