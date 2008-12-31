	Subroutine TOECLP( RA, Dec, JD, Lambda, Beta )
C
	Implicit None
C
	Double Precision fox
	Parameter( fox = 2.415020D+06 )
C
	Double Precision RA, Dec, JD, Lambda, Beta
C
	Double Precision DJD
C
	DJD = JD - fox
	Call EQECL_DS( DJD, RA, Dec, +1, Lambda, Beta )
C
	End
C
C
C
	Subroutine FMECLP( Lambda, Beta, JD, RA, Dec )
C
	Implicit None
C
	Double Precision fox
	Parameter( fox = 2.415020D+06 )
C
	Double Precision Lambda, Beta, JD, RA, Dec
C
	Double Precision DJD
C
	DJD = JD - fox
	Call EQECL_DS( DJD, Lambda, Beta, -1, RA, Dec )
C
	End
C
C
C
	subroutine EQECL_DS(DJD,X,Y,SW3,P,Q)
c ----------------------------------------------------------------------
c
c	Coded from Duffet p.59
c
	implicit none
C
	Double Precision DJD, X, Y, P, Q
	Integer SW3
C
	Double Precision T, C, EPS, EPS1, s_Eps, c_Eps
	Double Precision SY, CY, SX, CX, SQ, A, TY

	Double Precision d_Psi, d_Eps, RCP, RSP, Height
	Common / astro1 / d_Psi, d_Eps, RCP, RSP, Height

	Call SET_NUTATE( DJD )

c from subroutine OBLIQ, p.51

	T = DJD / 3.6525D+04
c					Shouldn't it be 3652422?
	C = ((( -1.81E-03*T) + 5.9E-03)*T + 4.6845E+01)*T
	EPS = 2.345229444E+01 - (C/3600.)

c end of subroutine OBLIQ

	EPS1 = EPS + d_Eps
	s_Eps = Sind( EPS1 )
	c_Eps = Cosd( EPS1 )

	CY=cosd(Y)
	SY=sind(Y)

	if(abs(CY).lt.1E-20)CY=1E-20
	TY = SY/CY
	CX=cosd(X)
	SX=sind(X)
	SQ=( SY*C_EPS) - (CY*S_EPS*SX*SW3)
	Q = atand( SQ/( sqrt(1-SQ*SQ)+1E-20 ) )
	A = (SX*C_EPS) + (TY*S_EPS*SW3)
	P = atand(A/CX)
	if(CX.lt.0)P = P + 180.0
	if(P.gt.360.0)P = P - 360.0
	if(P.lt.0)P = P + 360.0

	end
