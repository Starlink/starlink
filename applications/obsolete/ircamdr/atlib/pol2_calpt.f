
	SUBROUTINE POL_CALPT( IDIMS1, IDIMS2, IMAGE_1, IMAGE_2, IMAGE_3,
     :	                      IMAGE_4, ELDN, ODIMS1, ODIMS2, Q_IMAGE,
     :	                      U_IMAGE, P_IMAGE, T_IMAGE, I_IMAGE,
     :	                      PI_IMAGE, UPI_IMAGE, PE_IMAGE, TE_IMAGE,
     :                        STATUS)

* Subroutine to calculate the polarization and position angle image from the
* 4 input image taken at the 4 waveplate positions, i.e. 0 degrees, 22.5
* degrees, 45 degrees and 67.5 degrees

*  History
*    11-Aug-1994 Changed DIM arguments so that routine will compile (SKL@JACH)
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

	    CALL POL_STOKESCAL( IMAGE_1( K, J), IMAGE_3( K, J),
     :	                        Q_IMAGE( K, J))

	    CALL POL_STOKESCAL( IMAGE_2( K, J), IMAGE_4( K, J),
     :	                        U_IMAGE( K, J))

	    CALL POL_INTCAL( IMAGE_1( K, J), IMAGE_2( K, J),
     :	                     IMAGE_3( K, J), IMAGE_4( K, J),
     :	                     I_IMAGE( K, J))

	    CALL POL_POLCAL( Q_IMAGE( K, J), U_IMAGE( K, J),
     :	                     P_IMAGE( K, J))

	    CALL POL_THETACAL( Q_IMAGE( K, J), U_IMAGE( K, J),
     :	                       T_IMAGE( K, J))

	    CALL POL_PICAL( P_IMAGE( K, J), I_IMAGE( K, J),
     :	                    PI_IMAGE( K, J))

	    CALL POL_UPICAL( P_IMAGE( K, J), I_IMAGE( K, J),
     :	                     UPI_IMAGE( K, J))

	    CALL POL_ERRCAL( IMAGE_1( K, J), IMAGE_2( K, J),
     :	                     IMAGE_3( K, J), IMAGE_4( K, J),
     :	                     P_IMAGE( K, J), ELDN, PE_IMAGE( K, J),
     :	                     TE_IMAGE( K, J))

	  END DO

	END DO

	END
