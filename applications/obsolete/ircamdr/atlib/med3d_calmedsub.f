	SUBROUTINE MED3D_CALMEDSUB( NUM, ACTDATA, VALMAX, VALMIN, SUM, MEAN,
     :	                            MEDIAN, MODE)

	IMPLICIT  NONE

	INCLUDE  'SAE_PAR'

	INTEGER
     :    NUM                  ! number values in array

	REAL*8
     :    ACTDATA( NUM)        ! data array

	REAL
     :    SUM,                 ! sum of values
     :    MEAN,                ! mean of values
     :    MEDIAN,              ! median of value
     :    MODE,                ! mode of values
     :    VALMAX,              ! maximum value
     :    VALMIN               ! minimum value

	INTEGER
     :    I                    ! general array counters



*      Find the max and min in data

	VALMAX = -1.0E37
	VALMIN = 1.0E37

	DO I = 1, NUM

	  VALMIN = MIN( VALMIN, SNGL( ACTDATA( I)))
	  VALMAX = MAX( VALMAX, SNGL( ACTDATA( I)))

	END DO

*      Find sum and mean of data
	SUM = 0.0

	DO I = 1, NUM

	  SUM = SUM + SNGL( ACTDATA( I ))

	END DO

	MEAN = SUM/NUM

*      Now find median and estimate mode. mode = 3*median - 2*mean
	IF( IFIX( NUM/2.0+0.5)*2 .EQ. NUM) THEN

	  MEDIAN = ( ACTDATA( NUM/2) + ACTDATA( NUM/2+1))/2

	ELSE

	  MEDIAN = ACTDATA( NUM/2+1)

	END IF

	MODE = ( 3.0*MEDIAN) - ( 2.0*MEAN)

	END
