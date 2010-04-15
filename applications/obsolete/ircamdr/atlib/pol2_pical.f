	SUBROUTINE POL2_PICAL( POL, TOTINT, POLINT)

*      Subroutine to calculate the polarized intensity from the input
*      polarization and intensity values. This calculation is divide the
*      percentage polarization by 100 and mutiply by the total intensity

*      Define local variables

	REAL
     :	     POL,	! The PERCENTAGE POLARIZATION
     :	     POLINT	! The POLARIZED INTENSITY


*      Form the polarized intensity from the polarization and the intensity

	POLINT = ( POL/100.0)*TOTINT

	END
