	SUBROUTINE POL_INTCAL( INT_1, INT_2, INT_3, INT_4, TOTAL)

*      Subroutine to calculate the total intensity from 4 intensities at the
*      4 waveplate position in a polarization measurement i.e. 0 degrees,
*      22.5 degrees, 45 degrees and 67.5 degrees

*      Define local variables

	REAL INT_1,	! The intensity at 0 degrees
     :	     INT_2,	! The intensity at 22.5 degrees
     :	     INT_3,	! The intensity at 45 degrees
     :	     INT_4,	! The intensity at 67.5 degrees
     :	     TOTAL	! The total intensity

*      Calculate the sum of all the intensities to form the total

	TOTAL = ( INT_1 + INT_2 + INT_3 + INT_4)/2.0

	END
