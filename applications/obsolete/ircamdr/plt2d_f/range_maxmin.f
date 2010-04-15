	SUBROUTINE RANGE_MAXMIN( DATA_ARRAY, RANGE, XMAXIMUM,
     :	                         XMINIMUM, STATUS)

* Description : Calculates the plus and minus RANGE on the mean value
*               in a sub-image

* History :
* 13-05-86 : added option to calculate mean,sigma in sub-image : REVA::CAA
* 25-Jul-1994 Changed error reporting to  ERR calls removed VALUE (SKL@JACH)
* endhistory

	IMPLICIT NONE

	INCLUDE 'ADAM_DEFNS'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
	INCLUDE 'SAE_PAR'
	INCLUDE 'PLT2DCOM'

	INTEGER J
	INTEGER K
	INTEGER STATUS
	INTEGER SUBXST
	INTEGER SUBYST
	INTEGER SUBXSZ
	INTEGER SUBYSZ

	REAL DATA_ARRAY( NX, NY)
	REAL MEAN_VALUE
	REAL RANGE
	REAL SUM
	REAL XMAXIMUM
	REAL XMINIMUM

	CHARACTER*20 NSIGMA_CALC

* get the option to calculate scaling over the full image or a sub-image

	CALL PAR_GET0C( 'NSIGMA_CALC', NSIGMA_CALC, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :	                'Error : RANGE_MAXMIN : illegal RANGE_CALC',
     :                  STATUS )
	  RETURN
	END IF

* define limits of maximum, minimum values

	XMAXIMUM = -1.0E20
	XMINIMUM = 1.0E20
	SUM = 0.0
	MEAN_VALUE = 0.0

* test if want to calculate range over FULL image or sub-image

	IF( NSIGMA_CALC .NE. 'SUBIMAGE') THEN

* calculate mean value in FULL image

	  DO J = 1, NY

	    DO K = 1, NX

	      SUM = SUM + DATA_ARRAY( K, J)

	    END DO

	  END DO

	  IF( ( NX*NY) .GT. 0) THEN

	    MEAN_VALUE = SUM / ( NX*NY)

	  ELSE

            CALL MSG_OUT('ERR',
     :	           'Error : RANGE_MAXMIN : illegal image size, i.e. 0',
     :                  STATUS )
	    RETURN

	  END IF

* here if want to calculate range over sub-image

	ELSE

* get the sub-image position/size from parameter system

	  CALL PAR_GET0I( 'NSIGMA_XST', SUBXST, STATUS)
	  CALL PAR_GET0I( 'NSIGMA_YST', SUBYST, STATUS)
	  CALL PAR_GET0I( 'NSIGMA_XSZ', SUBXSZ, STATUS)
	  CALL PAR_GET0I( 'NSIGMA_XSZ', SUBYSZ, STATUS)

	  IF( STATUS .NE. SAI__OK) THEN
            CALL ERR_REP('ERR',
     :	          'Error : RANGE_MAXMIN : illegal subimage area values',
     :                  STATUS )
	    RETURN
	  END IF

* test if the define sub-image is in the image and real

	  IF( SUBXST .GE. 1 .AND. SUBYST .GE. 1 .AND.
     :	    ( SUBXST+SUBXSZ-1) .LE. NX .AND.
     :	    ( SUBYST+SUBYSZ-1) .LE. NY) THEN

* calculate mean value in FULL image

	    DO J = SUBYST, ( SUBYST+SUBYSZ-1)

	      DO K = SUBXST, ( SUBXST+SUBXSZ-1)

	        SUM = SUM + DATA_ARRAY( K, J)

	      END DO

	    END DO

	    IF( ( SUBXSZ*SUBYSZ) .GT. 0) THEN

	      MEAN_VALUE = SUM / ( SUBXSZ*SUBYSZ)

	    ELSE

              CALL MSG_OUT('ERR',
     :	          'Error : RANGE_MAXMIN : illegal subimage size. i.e. 0',
     :                     STATUS )
	      RETURN

	    END IF

	  ELSE

            CALL MSG_OUT('ERR',
     :	          'Error : RANGE_MAXMIN : subimage area outside image',
     :                   STATUS )
	    RETURN

	  END IF
	END IF

* calculate maximum and minimum for display

	XMAXIMUM = MEAN_VALUE + RANGE
	XMINIMUM = MEAN_VALUE - RANGE

	END
