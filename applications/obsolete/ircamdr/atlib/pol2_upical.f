	SUBROUTINE POL2_UPICAL( POL, TOTINT, UPOLINT)

*      Subroutine to calculate the unpolarized intensity from the input
*      polarization and intensity values. This calculation is 1 minus the
*      percentage polarization divided by 100 multiplied by the total intensity

*      Define local variables

	REAL
     :	     POL,	! The PERCENTAGE POLARIZATION
     :	     UPOLINT	! The UNPOLARIZED INTENSITY


*      Form the unpolarized intensity from the polarization and the intensity

	UPOLINT = (1.0 - POL/100.0)*TOTINT

	END
