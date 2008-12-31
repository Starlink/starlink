	Subroutine PREC( Alpha, Delta, Epoch, JD, p_Alpha, p_Delta )
C
	Implicit None
C
C	Precession programme using the "rigorous formula" in the
C	Astronomical Almanac (86).  Alpha in decimal hours, Delta
C	in decimal degrees, Epoch is in years
C
	Double Precision pi, pi_180, radian
	Parameter( pi = 3.1415926536D+00 )
	Parameter( pi_180 = 0.01745329252D+00 )
	Parameter( radian = 57.29577951D+00 )

	Double Precision Alpha, Delta
c						! initial coordinates
	Double Precision Epoch, JD
	Double Precision p_Alpha, p_Delta
c						! output coordinates
C
	Double Precision r_Alpha, r_Delta
c						! in radian
	Double Precision Zeta, Zed, Theta, T
	Double Precision cos_Theta, sin_Theta
	Double Precision cos_Delta, sin_Delta
	Double Precision X, Y, Z
	Double Precision Work
C
c		Alpha, Delta to radians
	r_Alpha = Alpha * pi_180
	r_Delta = Delta * pi_180
C		convert to J2000.0
	T = ( Epoch - 2.0D+03 ) * 1.0D-02
	Call PREC_ANGLES( T, Zeta, Zed, Theta )
	r_Alpha = r_Alpha - Zed
	cos_Theta = Cos( Theta )
	sin_Theta = Sin( Theta )
	cos_Delta = Cos( r_Delta )
	sin_Delta = Sin( r_Delta )
	Work = Cos( r_Alpha )
	X = Sin( r_Alpha ) * cos_Delta
	Y = sin_Theta * sin_Delta + Work * cos_Theta * cos_Delta
	Z = cos_Theta * sin_Delta - Work * sin_Theta * cos_Delta
	Call THREED_ANGLES( X, Y, Z, r_Alpha, r_Delta )
	r_Alpha = r_Alpha - Zeta
C		Next change to the desired epoch
	T = ( JD - 2.4515445D+06 ) / 3.6525D+04
	Call PREC_ANGLES( T, Zeta, Zed, Theta )
	r_Alpha = r_Alpha + Zeta
	cos_Theta = Cos( Theta )
	sin_Theta = Sin( Theta )
	cos_Delta = Cos( r_Delta )
	sin_Delta = Sin( r_Delta )
	Work = Cos( r_Alpha )
	X = Sin( r_Alpha ) * cos_Delta
	Y = Work * cos_Theta * cos_Delta - sin_Theta * sin_Delta
	Z = Work * sin_Theta * cos_Delta + cos_Theta * sin_Delta
	Call THREED_ANGLES( X, Y, Z, r_Alpha, r_Delta )
	r_Alpha = r_Alpha + Zed + Pi * 2.0
	r_Alpha = Mod( r_Alpha, Pi * 2.0D+00 )
C		change into decimal hours/degrees
	p_Alpha = r_Alpha * radian
	p_Delta = r_Delta * radian
C
	End
C
C
C
	Subroutine PREC_ANGLES( T, Zeta, Zed, Theta )
C
	Implicit None
C
	Double Precision T
c				! Time since J2000.0
	Double Precision Zeta, Zed, Theta
C
	Zeta = ( ( 8.72664626D-08 * T + 1.464331242D-06 ) * T
     1				+ 1.118086019D-02 ) * T
	Zed = ( ( 8.901179185D-08 * T + 5.307546255D-06 ) * T
     1				+ 1.118086019D-02 ) * T
	Theta = ( ( -2.024581932D-07 * T - 2.068215164D-06 ) * T
     1				+ 9.71717297D-03 ) * T
C
	End
C
C
C
	Subroutine THREED_ANGLES( X, Y, Z, r_Alpha, r_Delta )
C
	Implicit None
C
	Double Precision pi
	Parameter( pi = 3.1415926536 )
C
	Double Precision X, Y, Z
c						! Orthogonal Vector
	Double Precision r_Alpha, r_Delta
c						! Radians
C
	Double Precision cos_Delta
C
	r_Delta = Asin( Z )
	cos_Delta = Sqrt( 1.0 - Z * Z )
	If( X .ge. 0.0 ) Then
	  r_Alpha = Acos( Y / cos_Delta )
	Else
	  r_Alpha = Pi * 2.0D+00 - Acos( Y / cos_Delta )
	End If
C
	End
