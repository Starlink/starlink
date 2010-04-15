	SUBROUTINE LINCONT_NDR_LIN( DIMS_IMAGEX, DIMS_IMAGEY, IMAGE_ARRAY,
     :	                            IMAGE2_ARRAY, NCIMAGE, NUMCOEFFS,
     :	                            COEFFS, BASE, RANGE, RAT)

* Description :

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  STATUS,                 ! status for adam calls
     :	  DIMS_IMAGEX,            ! X dimension of input images,
     :	  DIMS_IMAGEY,            ! Y dimension of input images,
     :	  NCIMAGE,                ! number coadds in data image
     :	  NUMCOEFFS,              ! number linearization coefficients
     :	  J,                      ! looping variable
     :	  K                       ! looping variable

	REAL
     :	  IMAGE_ARRAY( DIMS_IMAGEX, DIMS_IMAGEY),        ! data image
     :	  IMAGE2_ARRAY( DIMS_IMAGEX, DIMS_IMAGEY),       ! data image KTC
     :	  COEFFS( NUMCOEFFS),                            ! coefficients
     :	  BASE,                                          ! base value
     :	  RANGE,                                         ! range value
     :	  READ2M1,                                       ! value phase-ktc
     :	  READ2,                                         ! value phase+ktc
     :	  READ2M1_LIN,                                   ! corrected data
     :	  RAT                                            ! ratio rr/exp

*       Initialize adam status

	STATUS = SAI__OK

*       Loops to scan through the pixels in the images

	DO J = 1, DIMS_IMAGEY

	  DO K = 1, DIMS_IMAGEX

	    IF( NCIMAGE .GT. 0) THEN

	      READ2M1 = IMAGE_ARRAY( K, J)

	      READ2 =
     :	       ( IMAGE_ARRAY( K, J)+IMAGE2_ARRAY( K, J))/REAL( NCIMAGE)

	    ELSE

	      CALL MSG_SETI( 'NCI', NCIMAGE)
	      CALL MSG_OUT( 'MESSAGE',
     :	        'Error, number coadds illegal, IMAGE=^NCI,',
     :	        STATUS)

	      RETURN

	    END IF

*          apply polynomial linearization correction

	    IF( NCIMAGE .GT. 0) THEN

	      READ2M1 = READ2M1/REAL( NCIMAGE)

	      CALL LINCONT_NDR_POLY( NUMCOEFFS, COEFFS, RAT, READ2M1,
     :	                             READ2M1_LIN)

	      IF( READ2 .LT. ( BASE+RANGE) .AND. READ2M1_LIN .LT. RANGE) THEN

	        READ2M1_LIN = READ2M1_LIN*REAL( NCIMAGE)

	      ELSE

	        READ2M1_LIN = RANGE

	      END IF

	    ELSE

	      READ2M1_LIN = RANGE

	    END IF

	    IMAGE_ARRAY( K, J) = READ2M1_LIN

	  END DO

	END DO

	END
