	SUBROUTINE LINCONT_LIN( DIMS_BIAS_1, DIMS_BIAS_2, BIAS_ARRAY,
     :                          NCBIAS, DIMS_IMAGE_1, DIMS_IMAGE_2,
     :	                        IMAGE_ARRAY, NCIMAGE, NUMCOEFFS, COEFFS)

* Description : Subtracts scaled bias from image and applies linearization
*               polynomial correction to result using coefficients passed
*               from above in COEFFS

* HISTORY
*   11-Aug-1994 Changed DIM arguments so that routine will compile (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  STATUS,                 ! status for adam calls
     :	  DIMS_BIAS_1,          ! dimensions of bias image
     :	  DIMS_BIAS_2,          ! dimensions of bias image
     :	  NCBIAS,                 ! number coadds in bias image
     :	  DIMS_IMAGE_1,         ! dimensions of data image
     :	  DIMS_IMAGE_2,         ! dimensions of data image
     :	  NCIMAGE,                ! number coadds in data image
     :	  NUMCOEFFS,              ! number linearization coefficients
     :	  J,                      ! looping variable
     :	  K                       ! looping variable

	REAL
     :	  BIAS_ARRAY( DIMS_BIAS_1, DIMS_BIAS_2),    ! bias image
     :	  IMAGE_ARRAY( DIMS_IMAGE_1, DIMS_IMAGE_2), ! data image
     :	  COEFFS( NUMCOEFFS),                           ! coefficients
     :	  VALMBIAS,                                     ! data-bias
     :	  VALLIN                                        ! corrected data

*       Initialize adam status

	STATUS = SAI__OK

*       Loops to scan through the pixels in the images

	DO J = 1, MIN( DIMS_IMAGE_2, DIMS_BIAS_2)

	  DO K = 1, MIN( DIMS_IMAGE_1, DIMS_BIAS_1)

*          subtracted scaled bias from scaled current data pixel after testing
*          that the scaling factors, the number of coadds are greater than 0

	    IF( NCIMAGE .GT. 0 .AND. NCBIAS .GT. 0) THEN

	      VALMBIAS = ( IMAGE_ARRAY( K, J)/REAL( NCIMAGE) -
     :	                   BIAS_ARRAY( K, J)/REAL( NCBIAS))

	    ELSE

	      CALL MSG_SETI( 'NCI', NCIMAGE)
	      CALL MSG_SETI( 'NCB', NCBIAS)
	      CALL MSG_OUT( 'MESSAGE',
     :	        'Error, number coadds illegal, IMAGE=^NCI, BIAS=^NCB',
     :	        STATUS)

	      RETURN

	    END IF

*          apply polynomial linearization correction

	    CALL LINCONT_POLY( NUMCOEFFS, COEFFS, VALMBIAS, VALLIN)

*          put linearized pixel back into data pixel after scaling up by the
*          number of coadds in raw data image and adding scaled bias back in

	    IMAGE_ARRAY( K, J) = ( VALLIN +
     :	               BIAS_ARRAY( K, J)/REAL( NCBIAS))*NCIMAGE

	  END DO

	END DO

	END
