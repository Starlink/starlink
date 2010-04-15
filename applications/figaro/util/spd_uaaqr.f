      REAL FUNCTION SPD_UAAQR( LGT_ARG, LGNU_ARG )
*+
*  Name:
*     SPD_UAAQ{DR}

*  Purpose:
*     Calculate the Planck function.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPD_UAAQR( LGT, LGNU )

*  Description:
*     This function accepts as arguments the decadic logarithm of the
*     black body temperature in Kelvin and the decadic logarithm of the
*     frequency in Hertz. It returns the decadic logarithm of the Planck
*     function
*
*             2 h nu^3          1
*     B_nu = ---------- ------------------
*                c^2     exp(h nu/kT) - 1
*
*               h = 6.626196     10^-34 J s    : Planck constant
*               k = 1.380622     10^-23 J/K    : Boltzmann constant
*               c = 2.997924562  10^8   m/s    : Maxwell constant
*           (h/k) = 4.7994280839 10^-11 J/Hz
*         2 h/c^2 = 1.4745274879 10^-50 W/(m^2 Hz sr Hz^3)
*                 = 1.4745274879 10^-24 (Jy/sr)/Hz^3
*
*     Straightforward use of above formula is prone to overflow or
*     division by zero. Underflow of the exp function cannot occur,
*     since its argument is always positive, its value greater than or
*     equal to 1.
*     -  The frequency may range over many orders of magnitude and
*        enters with its third power. To overcome this the Planck
*        function is used in its logarithmised form.
*     -  The exp function results in an overflow when its argument
*        exceeds approx 83. Thus if the argument is greater than 80, the
*        Wien approximation is used, i.e. the subtraction of 1 in the
*        denominator is neglected. Then we can use lg(exp(x)) = x lg(e).
*     -  The exp function is indistinguishable from 1 when its
*        argument is less than approx 10^-16. This would cause a divide
*        by zero error. Thus if the argument is less than 10^-14, the
*        Rayleigh-Jeans approximation is used.

*  Arguments:
*     LGT = REAL (Given)
*        log_10 of the black body temperature in K.
*     LGNU = REAL (Given)
*        log_10 of the frequency in Hz.

*  Returned Value:
*     SPD_UAAQR = REAL
*        log_10 of the intensity in Jy/sr.

*  References:
*     Lang, K.R., 1980, Astrophysical Formulae, Springer, Heidelberg,
*     Berlin, New York, p. XI, 21

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     28 Mar 1991 (hme):
*        Original version (PLANCK).
*     30 Oct 1991 (hme):
*        We not only need the Wien approximation to avoid overflow in
*        the exp-function, but also the Rayleigh-Jeans approximation to
*        avoid taking the logarithm of zero.
*     17 Jun 1992 (hme):
*        Logarithmic version, use Hz instead of 10^15 Hz.
*     25 Nov 1994 (hme):
*        Renamed from SPACC. Generalised for double and single
*        precision.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      REAL LGT_ARG
      REAL LGNU_ARG

*  Local Constants:
      DOUBLE PRECISION LGHOVK    ! lg[(h/k)/(K/Hz)]
      PARAMETER ( LGHOVK = -10.318810511545D0 )
      DOUBLE PRECISION LGHOVC    ! lg(2h/c**2)
      PARAMETER ( LGHOVC = -23.831347126999D0 )
      DOUBLE PRECISION LGE       ! lg(e)
      PARAMETER ( LGE    =  0.4342944819033D0 )

*  Local variables:
      DOUBLE PRECISION LGT
      DOUBLE PRECISION LGNU
      DOUBLE PRECISION LGHNKT
      DOUBLE PRECISION RESULT

*.

*  Cast given arguments.
      LGT  = LGT_ARG
      LGNU = LGNU_ARG

*  The argument of the crucial exp function.
      LGHNKT = LGHOVK + LGNU - LGT

*  If frequency >> temperature (hnu/kT>80), we need the Wien approx.
*  i.e. we approximate   exp() - 1 = exp()
*  and substitute        lg(exp()) = lg(e)*()
      IF ( LGHNKT .GT. 1.90309D0 ) THEN
         RESULT = LGHOVC + 3D0 * LGNU - LGE * 1D1**LGHNKT

*  Else if frequency << temperature, we need Rayleigh-Jeans approx.
*  i.e. we approximate   exp() = 1 + ()
*  or                    exp() - 1 = ()
      ELSE IF ( LGHNKT .LT. -14D0 ) THEN
         RESULT = LGHOVC + 3D0 * LGNU - LGHNKT

*  Else, we can use the proper Planck law.
      ELSE
         RESULT = LGHOVC + 3D0 * LGNU - LOG10( EXP(1D1**LGHNKT) - 1D0 )
      END IF

*  Cast returned value.
      SPD_UAAQR = RESULT

*  Return.
      END
