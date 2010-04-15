	SUBROUTINE POL2_INTCAL( INT_1, INT_2, INT_3, INT_4,
     :	                        INT_5, INT_6, INT_7, INT_8, TOTAL)

*      Subroutine to calculate the total intensity from 8 intensities at the
*      4 waveplate position in a polarization measurement i.e. 0 degrees,
*      22.5 degrees, 45 degrees and 67.5 degrees

*      Define local variables

	REAL
     :	     INT_1,	! The o- intensity at 0 degrees
     :	     INT_2,	! The e- intensity at 0 degrees
     :	     INT_3,	! The o- intensity at 45 degrees
     :	     INT_4,	! The e- intensity at 45 degrees
     :	     INT_5,	! The o- intensity at 22.5 degrees
     :	     INT_6,	! The e- intensity at 22.5 degrees
     :	     INT_7,	! The o- intensity at 67.5 degrees
     :	     INT_8,	! The e- intensity at 67.5 degrees
     :	     TOTAL	! The total intensity

*      Calculate the sum of all the intensities to form the total

	TOTAL = ( ( INT_1 + INT_2 + INT_3 + INT_4 +
     :	            INT_5 + INT_6 + INT_7 + INT_8))/4.0

	END
