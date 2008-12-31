*+SUN_ELONG
        double precision function SUN_ELONG( JD )

        implicit none

        double precision fox
        parameter( fox = 2.415020D+06 )
*                  JD at the beginning of the 20th century

        double precision jd

*       Description
*         Returns the ecliptic longitude of the sun for a given julian date
*
*       Arguments
*         jd             (i) : Julian date of interest
*         <SUN_ELONG>    (o) : Ecliptic longitude of the sun
*
*       Dependences
*         SET_NUTATE, nutation calculation subroutine
*         and SUNSUB, a Duffet-Smith routine included below
*
*       Origin
*         A simple wrapper invented as interface between the D-S routine
*         and the viewing program
*
*       Author
*         Koji Mukai (1992 Dec 30), original version
*-SUN_ELONG

        double precision djd, lsn, rsn

        double precision d_psi, d_eps, rcp, rsp, height
        common / astro1 / d_psi, d_eps, rcp, rsp, height

        djd = jd - fox
        call SUNSUB( djd, lsn, rsn )
*       now correct lsn for nutation
        call SET_NUTATE( djd )
        SUN_ELONG = lsn + d_psi - 5.69D-03

        end



c ----------------------------------------------------------------------
	subroutine SUNSUB(DJD,LSN,RSN)
c ----------------------------------------------------------------------
c
c from Duffet p.121
c	now returns LSN in degrees rather than radian
c
	implicit none
c
	Double Precision Pi, pi_180
	Parameter( Pi = 3.1415926536D+00 )
	Parameter( pi_180 = 0.01745329252D+00 )
C
	Double Precision DJD, LSN, RSN
C
	Double Precision T, T2, A, B, LS, MS, S, MA, M, EA, DLA, TNU2
	Double Precision Nu, A1, B1, C1, D1, E1, H1, DL, DR
C
        T = DJD / 3.6525D+04
	T2 = T*T

	A = 1.000021359D2*T
	B = 3.60D+2*( A - int(A) )
	LS = 2.7969668D2 + 3.025D-4*T2 + B
	A = 9.999736042D+01 * T
	B = 3.60D2*( A - int(A) )
	MS = 3.5847583D+02 - ( 1.5D-04 + 3.3D-06 * T)*T2 + B
	S = 1.675104D-02 - 4.18D-5*T - 1.26D-7*T2
	If( MS .lt. 0 ) Then
	  MA = ( 1 - Int( MS / 3.60D+2 ) ) * 3.6D+2 + MS
	Else
	  MA = MS
	End If

c subroutine ANOMALY, from Duffet P.113
c	This routine only works in radians ... EA is left in radians
c	(all other parts of this program is in degrees)
c	Need to have called INIT_PA( ) to set up conversion constants

	MA = MA * pi_180
	M = Mod( MA, Pi * 2.0D+00 )
	EA = M
	DLA = - S*Sin(EA)
	Do While( ABS( DLA ) .ge. 1.0D-09 )
	  DLA = DLA / (1.0D0 - (S * Cos( EA) ) )
	  EA = EA - DLA
	  DLA = EA - ( S * Sin( EA ) ) - M
	End Do

	TNU2 = sqrt( (1+S)/(1-S) )*tan( EA/2.0 )
	NU = 2.0 * atand(TNU2)

c subroutine ANOMALY ends

	A = 6.255209472D+01 * T
	B = 360.*( A - int(A) )
	A1 = 153.23 + B

	A = 1.251041894D+02 * T
	B = 360.*( A - int(A) )
	B1 = 216.57 + B

	A = 9.156766028D+01 * T
	B = 360.*( A - int(A) )
	C1 = 312.69 + B

	A = 1.236853095D+03 * T
	B = 360.*( A - int(A) )
	D1 = 350.74 - 1.44D-3*T2 + B

	E1 = 231.19 + 20.2*T

	A = 1.831353208D+02 * T
	B = 360.*( A - int(A) )
	H1 = 353.4 + B

	DL = 1.34D-03*cosd(A1) + 1.54D-03*cosd(B1)
     +     + 2D-03*cosd(C1) + 1.79D-03*sind(D1)
     +     + 1.78D-03*sind(E1)

	DR = 5.43D-06*sind(A1) + 1.575D-05*sind(B1)
     +     + 1.627D-05*sind(C1) + 3.076D-05*cosd(D1)
     +     + 9.27D-06*sind(H1)

	LSN = NU + (LS-MS+DL)
	RSN = 1.0000002*(1-S*cos(EA))+DR
	
	do while (LSN.lt.0)
		LSN = LSN + 3.6D+2
	end do

	do while (LSN.gt.3.6D+2)
		LSN = LSN - 3.6D+2
	end do
	
	return
	end
