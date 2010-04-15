	SUBROUTINE POL_CALPT2( IDIMS1, IDIMS2, IMAGE_1, IMAGE_2, IMAGE_3,
     :	                      IMAGE_4, IMAGE_5, IMAGE_6, IMAGE_7,
     :	                      IMAGE_8, ELDN, ODIMS1, ODIMS2, Q_IMAGE,
     :	                      U_IMAGE, P_IMAGE, T_IMAGE, I_IMAGE,
     :	                      PI_IMAGE, UPI_IMAGE, PE_IMAGE, TE_IMAGE,
     :                        STATUS)

* Subroutine to calculate the polarization and position angle image from the
* 8 input image taken at the 4 waveplate positions, i.e. 0 degrees, 22.5
* degrees, 45 degrees and 67.5 degrees using IRPOL2/Wollaston prism

*  History
*    11-Aug-1994 Changed DIM arguments so that routine will compile (SKL@JACH)
*    27-Nov-1995: created this from pol_calpt (caa@jach)
*

	IMPLICIT NONE

	INTEGER IDIMS1, IDIMS2,
     :	        J,
     :	        K,
     :	        ODIMS1, ODIMS2,
     :	        STATUS

	REAL ELDN,
     :	     IMAGE_1( IDIMS1, IDIMS2),
     :	     IMAGE_2( IDIMS1, IDIMS2),
     :	     IMAGE_3( IDIMS1, IDIMS2),
     :	     IMAGE_4( IDIMS1, IDIMS2),
     :	     IMAGE_5( IDIMS1, IDIMS2),
     :	     IMAGE_6( IDIMS1, IDIMS2),
     :	     IMAGE_7( IDIMS1, IDIMS2),
     :	     IMAGE_8( IDIMS1, IDIMS2),
     :	     I_IMAGE( ODIMS1, ODIMS2),
     :	     P_IMAGE( ODIMS1, ODIMS2),
     :	     PE_IMAGE( ODIMS1, ODIMS2),
     :	     PI_IMAGE( ODIMS1, ODIMS2),
     :	     Q_IMAGE( ODIMS1, ODIMS2),
     :	     T_IMAGE( ODIMS1, ODIMS2),
     :	     TE_IMAGE( ODIMS1, ODIMS2),
     :	     U_IMAGE( ODIMS1, ODIMS2),
     :	     UPI_IMAGE( ODIMS1, ODIMS2)

*      Loops to scan through the input images

	DO J = 1, ODIMS2
	  DO K = 1, ODIMS1

*          pass each pixel value to the subroutine that calculates Q, U, P,
*         THETA, TOTAL INTENSITY POLARIZED INTENSITY and ERROR IMAGES


* FROM POLLY2...
*        CALL POL2_STOKESCAL( OINTEN0, EINTEN0, OINTEN45, EINTEN45, Q)
*        CALL POL2_STOKESCAL( OINTEN22, EINTEN45, OINTEN67, EINTEN67, U)
*        CALL POL2_POLCAL( Q, U, P)
*        CALL POL2_THETACAL( Q, U, T)
*        CALL POL2_ERRCAL( OINTEN0, EINTEN0, OINTEN45, EINTEN45,
*     :                    OINTEN22, EINTEN22, OINTEN67, EINTEN67,
*     :                    P, ELDN, PE, TE)
*        CALL POL2_INTCAL( OINTEN0, EINTEN0, OINTEN45, EINTEN45,
*     :                    OINTEN22, EINTEN22, OINTEN67, EINTEN67,
*     :                    I)

	    CALL POL2_STOKESCAL( IMAGE_1( K, J), IMAGE_2( K, J),
     :	                         IMAGE_3( K, J), IMAGE_4( K, J),
     :	                         Q_IMAGE( K, J))

	    CALL POL2_STOKESCAL( IMAGE_5( K, J), IMAGE_6( K, J),
     :	                         IMAGE_7( K, J), IMAGE_8( K, J),
     :	                         U_IMAGE( K, J))

	    CALL POL2_INTCAL( IMAGE_1( K, J), IMAGE_2( K, J),
     :	                      IMAGE_3( K, J), IMAGE_4( K, J),
     :	                      IMAGE_5( K, J), IMAGE_6( K, J),
     :	                      IMAGE_7( K, J), IMAGE_8( K, J),
     :	                      I_IMAGE( K, J))

	    CALL POL2_POLCAL( Q_IMAGE( K, J), U_IMAGE( K, J),
     :	                      P_IMAGE( K, J))

	    CALL POL2_THETACAL( Q_IMAGE( K, J), U_IMAGE( K, J),
     :	                        T_IMAGE( K, J))

	    CALL POL2_PICAL( P_IMAGE( K, J), I_IMAGE( K, J),
     :	                     PI_IMAGE( K, J))

	    CALL POL2_UPICAL( P_IMAGE( K, J), I_IMAGE( K, J),
     :	                      UPI_IMAGE( K, J))

	    CALL POL2_ERRCAL( IMAGE_1( K, J), IMAGE_2( K, J),
     :	                      IMAGE_3( K, J), IMAGE_4( K, J),
     :	                      IMAGE_5( K, J), IMAGE_6( K, J),
     :	                      IMAGE_7( K, J), IMAGE_8( K, J),
     :	                      P_IMAGE( K, J), ELDN, PE_IMAGE( K, J),
     :	                      TE_IMAGE( K, J))

	  END DO
	END DO

	END
