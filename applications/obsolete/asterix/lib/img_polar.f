*+  IMG_POLAR - Extract radial/azimuthal distribution from image array
      SUBROUTINE IMG_POLAR( NX, NY, ARRAY, VOK, VAR, QOK, QUAL,
     :                      BADBITS, UNITFACT, POLX, POLY, RBIN,
     :                      AZANG, NRAD, NSECTOR, COUNT, POL, PVAR,
     :                      PQUAL, STATUS )
     :
*
*    Description :
*
*     Produces a two dimensional radius vs azimuth array from a cartesian
*     array.
*
*    Author :
*
*     Dick Willingale (LTVAD::RW)
*
*    History :
*
*     26 Oct 88 : Original (LTVAD::RW)
*     30 Mar 89 : Modified for ISIS (LTVAD::RDS)
*     21 Feb 94 : Tidied up a bit, quality made optional (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Import :
*
      INTEGER			NX,NY            	! Input dimensions
      REAL 			ARRAY(NX,NY)        	! Input cartesian data
      LOGICAL 			VOK                 	! Variance's ok?
      REAL 			VAR(NX,NY)          	! Input variance array
      LOGICAL 			QOK                 	! Quality ok?
      BYTE 			QUAL(NX,NY)         	! Input quality array
      BYTE 			BADBITS             	! Quality mask
      DOUBLE PRECISION 		UNITFACT      		! Conversion factor
							! between pixels and
							! square axis units
      REAL 			POLX,POLY               ! Centre of polar
      REAL 			RBIN                    ! Radial binsize (pixels)
      REAL 			AZANG                   ! Initial azimuthal angle (degs)
      INTEGER 			NRAD,NSECTOR            ! Output dimensions
      INTEGER 			COUNT(NRAD,NSECTOR)     ! Workspace
*
*    Export :
*
      REAL 			POL(NRAD,NSECTOR)       ! Polar output data
      REAL 			PVAR(NRAD,NSECTOR)      ! Output variance
      BYTE 			PQUAL(NRAD,NSECTOR)     ! Output quality
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
      DOUBLE PRECISION 		RNN,RNN2                ! Normalising factors

      REAL 			ABIN                    ! Azimuthal binsize (rads)
      REAL 			AZRAD                   ! Azimnuth start in
                                                        ! radians
      REAL 			YOLD,XOLD
      REAL 			RAD                     ! Radius of pixel from centre
      REAL 			THETA

      INTEGER 			IXP,IYP,IRAD
      INTEGER 			NXL,NXH,NYL,NYH         ! Range of pixels to test
      INTEGER 			J,K,KK,JJ

      LOGICAL			OK			! This point ok?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set angular bin size
      ABIN = TWOPI/MAX(REAL(NSECTOR),1.0)

*    Convert initial azimuthal angle into radians
      AZRAD = AZANG * MATH__DTOR

*    Zero output arrays and counter
      DO J = 1, NSECTOR
	DO K = 1, NRAD
	  POL(K,J)=0.0
	  PVAR(K,J)=0.0
          COUNT(K,J)=0
	END DO
      END DO

*    Find pixel ranges about centre
      IXP=POLX+1.0
      IYP=POLY+1.0
      IRAD=RBIN*NRAD+2.0
      NXL = MAX(MIN(IXP-IRAD,NX),1)
      NXH = MAX(MIN(IXP+IRAD,NX),1)
      NYL = MAX(MIN(IYP-IRAD,NY),1)
      NYH = MAX(MIN(IYP+IRAD,NY),1)
      OK = .TRUE.

*    Now raster through array
      DO J = NYL, NYH
        YOLD=(REAL(J)-0.5)-POLY

        DO K = NXL, NXH

*        Quality test per pixel if present
          IF ( QOK ) OK = (QUAL(K,J).AND.BADBITS) .EQ. QUAL_GOOD

*        Is this pixel ok ?
          IF ( OK ) THEN
	    XOLD=(REAL(K)-0.5)-POLX

*          Calculate radius of element
            KK=0
            IF (.NOT.(XOLD.EQ.0.0.AND.YOLD.EQ.0.0)) THEN
	      RAD=SQRT(XOLD**2+YOLD**2)
	      KK=RAD/RBIN+1.
            END IF

*          Pixel inside maximum radius?
	    IF (KK.GT.0.AND.KK.LE.NRAD) THEN

*            Find position angle of element
	      THETA = ATAN2(YOLD,XOLD)

*            Subtract the initial azimuth angle
              THETA = THETA - AZRAD

*            Force position angle to be in range 0 to TWOPI
	      IF ( THETA .LT. 0.0 ) THETA=TWOPI+THETA
	      IF ( THETA .LT. 0.0 ) THETA=TWOPI+THETA
	      IF ( THETA .GT. TWOPI ) THETA=THETA-TWOPI
	      IF ( THETA .GT. TWOPI ) THETA=THETA-TWOPI
*
	      JJ=THETA/ABIN+1.0
	      IF (JJ.GT.0.AND.JJ.LE.NSECTOR) THEN
		POL(KK,JJ) = POL(KK,JJ) + ARRAY(K,J)
                IF ( VOK ) THEN
		  PVAR(KK,JJ) = PVAR(KK,JJ) + VAR(K,J)
                END IF
                COUNT(KK,JJ) = COUNT(KK,JJ) + 1
	      END IF

*          End radial test
	    END IF

*        End pixel ok test
          ENDIF

*      End loop over 1st dimension
	END DO

*    End loop over 2nd dimension
      END DO

*    Normalise to surface brightness per square axis unit
      DO K = 1, NRAD

	DO J = 1, NSECTOR

*        Mark gaps
	  IF ( COUNT(K,J) .LE. 0 ) THEN
	    PVAR(K,J) = 0.0
	    POL(K,J) = 0.0
            PQUAL(K,J) = QUAL_BAD

	  ELSE

*          Normalise and find standard deviations
	    RNN=1.0 / (REAL(COUNT(K,J)) * UNITFACT)
            RNN2=RNN*RNN

*          Set output variance
	    IF ( VOK ) THEN
	      PVAR(K,J) = PVAR(K,J)*RNN2
	    ELSE
	      PVAR(K,J) = ABS(POL(K,J)*RNN2)
	    END IF

*          Normalise
	    POL(K,J) = POL(K,J) * RNN

*          Set pixel good
	    PQUAL(K,J) = QUAL_GOOD

	  END IF

	END DO

      END DO

      END
