	SUBROUTINE NSIGMA_MAXMIN( DATA_ARRAY, NSIGMA_SORT, SIGMA_LEVEL,
     :	                          SIGMA_UP, SIGMA_DOWN, XMAXIMUM,
     :	                          XMINIMUM, STATUS)

* Description : Calculates the plus and minus N-sigma on the mean value
*               in a sub-image

* History :
* 13-05-86 : added option to calculate mean,sigma in sub-image : REVA::CAA
* 25-JUL-1994 Changed STR$ and LIB$ calls to CHR_ (SKL@JACH)
* 27-JUL-1994 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
* 02-Sep-1994 Removed unused variables flagged by UNIX compiler (SKL@JACH)
* endhistory

	IMPLICIT NONE

	INCLUDE 'ADAM_DEFNS'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'
	INCLUDE 'PLT2DCOM'

	INTEGER BAD_NUMBER
	INTEGER J
	INTEGER K
	INTEGER STATUS
	INTEGER SUBXST
	INTEGER SUBYST
	INTEGER SUBXSZ
	INTEGER SUBYSZ

	REAL DATA_ARRAY( NX, NY)
	REAL MEAN_VALUE
	REAL ONE_SIGMA
	REAL VARIANCE
	REAL SIGMA_DOWN
	REAL SIGMA_LEVEL
	REAL SIGMA_UP
	REAL SUM
	REAL SUMSQ
	REAL XMAXIMUM
	REAL XMINIMUM

	CHARACTER*20 NSIGMA_CALC
	CHARACTER*( *) NSIGMA_SORT

* get the option to calculate scaling over the full image or a sub-image

	CALL PAR_GET0C( 'NSIGMA_CALC', NSIGMA_CALC, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :                 'Error : NSIGMA_MAXMIN : illegal NSIGMA_CALC',
     :                 STATUS )
	  RETURN
	END IF

* define limits of maximum, minimum values

	XMAXIMUM = -1.0E20
	XMINIMUM = 1.0E20
	MEAN_VALUE = 0.0
	SUM = 0.0
	SUMSQ = 0.0
	ONE_SIGMA = 1.0
	BAD_NUMBER = 0

* test if want to calculate range over FULL image or sub-image

	IF( NSIGMA_CALC .NE. 'SUBIMAGE') THEN

* calculate mean value in FULL image

	  DO J = 1, NY

	    DO K = 1, NX

	      IF( ABS( DATA_ARRAY( K, J)) .GT. 1.0E30) THEN

	        BAD_NUMBER = BAD_NUMBER + 1

	      ELSE

	        SUM = SUM + DATA_ARRAY( K, J)
	        SUMSQ = SUMSQ + DATA_ARRAY( K, J)**2

	      END IF
	    END DO
	  END DO

	  IF( ( NX*NY) .GT. 0) THEN

	    MEAN_VALUE = SUM / ( NX*NY)

* calculate maximum and minimum from sigma level

            VARIANCE = ( SUMSQ - 2*MEAN_VALUE*SUM +
     :                  (NX*NY)*MEAN_VALUE**2)

            IF( (NX*NY) .EQ. 1 ) THEN

               VARIANCE  =  VARIANCE / REAL( NX*NY)

            ELSE

               VARIANCE  =  VARIANCE / ( REAL( NX*NY) - 1.0)

            ENDIF
	  ELSE

            CALL MSG_OUT('ERR',
     :            'Error : NSIGMA_MAXMIN : illegal image size, i.e. 0',
     :                 STATUS )
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
     :         'Error : NSIGMA_MAXMIN : illegal subimage area values',
     :                 STATUS )
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
	        SUMSQ = SUMSQ + DATA_ARRAY( K, J)**2

	      END DO
	    END DO

	    IF( ( SUBXSZ*SUBYSZ) .GT. 0) THEN

	      MEAN_VALUE = SUM / ( SUBXSZ*SUBYSZ)

* calculate maximum and minimum from sigma level

              VARIANCE = ( SUMSQ - 2*MEAN_VALUE*SUM +
     :	                 (SUBXSZ*SUBYSZ)*MEAN_VALUE**2)

              IF( (SUBXSZ*SUBYSZ) .EQ. 1 ) THEN

                 VARIANCE  =  VARIANCE / REAL( SUBXSZ*SUBYSZ)

              ELSE

                 VARIANCE  =  VARIANCE / ( REAL( SUBXSZ*SUBYSZ) - 1.0)

              ENDIF
	    ELSE

              CALL MSG_OUT('ERR',
     :         'Error : NSIGMA_MAXMIN : illegal subimage size. i.e. 0',
     :                 STATUS )
	      RETURN

	    END IF
	  ELSE

            CALL MSG_OUT('ERR',
     :         'Error : NSIGMA_MAXMIN : subimage area outside image',
     :                 STATUS )
	    RETURN

	  END IF
	END IF

* test if the nsigma sort is SAME or something else ...

        ONE_SIGMA =  SQRT( ABS( VARIANCE))

	IF( NSIGMA_SORT .EQ. 'SAME') THEN

* calculate one sigma and the maximum and minimum values to use in plot


	  XMAXIMUM = MEAN_VALUE + SIGMA_LEVEL*ONE_SIGMA
	  XMINIMUM = MEAN_VALUE - SIGMA_LEVEL*ONE_SIGMA

	ELSE

* calculate one sigma and the maximum and minimum values to use in plot

	  XMAXIMUM = MEAN_VALUE + SIGMA_UP*ONE_SIGMA
	  XMINIMUM = MEAN_VALUE - SIGMA_DOWN*ONE_SIGMA

	END IF

	IF( BAD_NUMBER .GT. 0) THEN

          CALL MSG_SETI( 'BAD', BAD_NUMBER )
          CALL MSG_OUT('BAD',
     :  'Warning ^BAD pixels have a value larger than 1e30', STATUS )

	END IF

	END
