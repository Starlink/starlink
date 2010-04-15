      SUBROUTINE SPD_WFPD( FITPAR, PAR1, FVAL )
*+
*  Name:
*     SPD_WFPD

*  Purpose:
*     6-diluted-Planck objective function for E04DGF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WFPD( FITPAR, PAR1, FVAL )

*  Description:
*     This routine returns the value and gradient of the objective
*     function for the minimising routine PDA_UNCMND.
*     The objective function is a scaled chi-squared with the model
*     being the sum of up to six diluted Planck functions.
*     The objective function F(a) is
*
*               sum{w_i (d_i - f(x_i))^2}
*     F(a_k) = -----------------------------
*                        F_{scale}
*
*     where f is supposed to fit the data d_i as a function of x_i. w_i
*     are the weights, i.e. the reciprocal variances of d_i. f is the
*     sum of up to six diluted Planck functions, or rather its
*     logarithm:
*
*     f(x) = lg{ sum { 10^f_j(x) } }
*
*     f_j(x) = c_j + p_j * x + lgB_nu(s_j,x)
*
*     c_j: Scaling constant Theta,
*     p_j: Emissivity exponent alpha: epsilon proportional to nu^alpha,
*     s_j: log10 of colour temperature,
*     x:   log10 of frequency,
*     lgB_nu: log10 of the Planck function.
*
*     In practice f(x) is summed up from its components with
*
*     lg(A+B) = max{ lgA; lgB } + lg[ 1 + 10^(-|lgA-lgB|) ]
*
*     where the latter term may be neglected when the magnitudes of A
*     and B are very different.
*
*     The k-th fit parameter a_k corresponds to the scaling constant,
*     emissivity exponent, or colour temperature of the j-th component.
*     a_k are copies of the free parameters. Since not
*     all parameters are free, there is a permutation between
*     indexes k and j. The scaling is such that the guess c_{j,orig},
*     p_{j,orig}, s_{j,orig} is turned into a_k = 0, and that a_k is an
*     offset from the original guess. E04DGF tries to improve
*     the guess by varying a_k, and this routine must evaluate these to
*     calculate the fit function f(x) they represent. In fact this
*     routine must work out all parameters (fixed, free, tied)
*     in accordance with the a_k proposed by E04DGF. For the fixed
*     parameters this is trivial. For the free parameters:
*
*     c_j = c_{j,orig} + a_k1
*
*     p_j = p_{j,orig} + a_k2
*
*     s_j = s_{j,orig} + a_k3
*
*     Once the fixed and free parameters are known, the tied ones are
*     (jth component tied to mth):
*
*     c_j = c_m + c_{j,orig} - c_{m,orig}
*
*     p_j = p_m + p_{j,orig} - p_{m,orig}
*
*     s_j = s_m + s_{j,orig} - s_{m,orig}
*
*     x_i, d_i, w_i are transferred through a pointer in the common
*     block to the array MSKXDW. The upper bound of i is transferred
*     through the common block as well.
*     The parameters c_j, p_j, s_j can be fixed, free to fit, or
*     tied to another parameter. They are worked out from their original
*     guesses (transferred via the COMMON block SPD_WFCM), considering
*     the fit flags (also in the COMMON block), and the modifications to
*     free parameters as transferred from PDA_UNCMND in the array a = PAR1.
*     The underlying continuum is transferred via the COMMON block, as
*     is the scaling factor F_{scale}. (Here the continuum is zero.)

*  Arguments:
*     FITPAR = INTEGER (Given)
*        k_{max}. The number of parameters to be fitted.
*     PAR1( FITPAR ) = DOUBLE PRECISION (Given)
*        a_k. The current values of fit parameters.
*     FVAL = DOUBLE PRECISION (Returned)
*        F(a_k). The value of the objective function for given PAR1 and
*        other information obtained through IUSER and MSKXDW.
*     NDATA = INTEGER (Common /SPD_WFCM/)
*        The number of data points, i.e. 1/3 the length of MSKXDW.
*     MSKXDW( 1 ) = DOUBLE PRECISION (Given)
*        The packed array of masked X, DAT, WHT.
*        MSKXDW(         1 :   NDATA ): x values x_i.
*        MSKXDW(   NDATA+1 : 2*NDATA ): data values d_i.
*        MSKXDW( 2*NDATA+1 : 3*NDATA ): weight values w_i.
*        MSKXDW is not immediately available, but only as a pointer.
*        When passed to a subroutine as %VAL(DATAP) it becomes an array.
*     NCOMP = INTEGER (Common /SPFCM1/)
*        j_{max}. Number of components.
*     DCONT = DOUBLE PRECISION (Common /SPFCM1/)
*        The constant continuum underlying the profiles.
*        (Here this is ignored and assumed zero.)
*     FSCALE = DOUBLE PRECISION (Common /SPFCM1/)
*        Scaling factor. The objective function is proportional to
*        1/FSCALE. FSCALE should be such that the objective function is
*        of order unity near its minimum.
*     PARNO( 3*6 ) = INTEGER (Common /SPFCM1/)
*        Permutation vector for parameters.
*        PAR1(PARNO(I)) corresponds to PAR0(I). PAR1 is not part of this
*        COMMON block.
*        First in the array PAR1 come all the free scaling constants in
*        ascending order of component number. Then come all free
*        emissivity exponents in ascending order of component number.
*        Then come all colour temperatures.
*        Last in the array PAR1 come all fixed or tied or unused scaling
*        constants in descending order of component number. Before that
*        come all fixed or tied or unused emissivity exponents in
*        descending order of component number. Before that come all
*        fixed or tied of unused colour temperatures in descending order
*        of component number.
*     PARFLG( 3*6 ) = INTEGER (Common /SPFCM1/)
*        A packed version of the fit flags:
*        PARFLG={CFLAGS(1) ... CFLAGS(NCOMP),NCOMP+1,...,6,
*                PFLAGS(1) ... PFLAGS(NCOMP),NCOMP+1,...,6,
*                SFLAGS(1) ... SFLAGS(NCOMP),NCOMP+1,...,6}
*        For each component I a value e.g. CFLAGS(I)=0 indicates
*        that THETA(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that THETA(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that THETA(I) has to
*        keep a fixed offset from THETA(J).
*        Deviating from CFLAGS etc., unused components' parameters are
*        flagged as fixed.
*     PAR0( 3*6 ) = DOUBLE PRECISION (Common /SPFCM1/)
*        A packed version of the unscaled guess parameters:
*        PAR0={THETA(1) ... THETA(NCOMP),0,0,...,0,
*              ALPHA(1) ... ALPHA(NCOMP),0,0,...,0,
*              TEMPE(1) ... TEMPE(NCOMP),0,0,...,0}
*        Deviating from THETA etc., unused components' parameters are
*        set to 0 or 1, whichever causes less harm.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28 Jan 1993 (hme):
*        Adapted from SPABD.
*        Restructured, since f is not a simple sum of f_j.
*     25 Nov 1994 (hme):
*        Renamed from SPACT, common block in include file.
*     21 Nov 1995 (hme):
*        Replace E04DGF/E04DKF with PDA_UNCMND.
*     2005 June 2 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     15 Aug 2005 (timj):
*        ** -LOGDIF is not standards compliant. Use parentheses
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'SPD_WFCM'
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER FITPAR
      DOUBLE PRECISION PAR1( FITPAR )

*  Arguments Returned:
      DOUBLE PRECISION FVAL

*.

*  SPD_WFPD itself only combines the arguments from PDA_UNCMND with the
*  information in the SPD_WFCM common block. The extra layer is
*  necessary, since the common block contains a pointer to a
*  variable-size array.
      CALL SPD_WFPE( FITPAR, PAR1, FVAL, NDATA,
     :               %VAL( CNF_PVAL(DATAP) ), NCOMP, DCONT, FSCALE,
     :               PARNO, PARFLG, PAR0 )

      END




      SUBROUTINE SPD_WFPE( FITPAR, PAR1, FVAL, NDATA, MSKXDW,
     :   NCOMP, DCONT, FSCALE, PARNO, PARFLG, PAR0 )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INTEGER MAXCMP
      PARAMETER ( MAXCMP = 6 )

*  Arguments Given:
      INTEGER          FITPAR
      DOUBLE PRECISION PAR1( FITPAR )
      INTEGER          NDATA
      REAL             MSKXDW( 3*NDATA )
      INTEGER          NCOMP
      DOUBLE PRECISION DCONT
      DOUBLE PRECISION FSCALE
      INTEGER          PARNO(  3*MAXCMP )
      INTEGER          PARFLG( 3*MAXCMP )
      DOUBLE PRECISION PAR0(   3*MAXCMP )

*  Arguments Returned:
      DOUBLE PRECISION FVAL

*  Local Variables:
      INTEGER I                  ! Data point counter
      INTEGER J                  ! Component counter
      DOUBLE PRECISION ARG       ! Argument of exp function etc.
      DOUBLE PRECISION C( MAXCMP ) ! Scaling constants Theta
      DOUBLE PRECISION P( MAXCMP ) ! Emissivity exponents alpha
      DOUBLE PRECISION S( MAXCMP ) ! Temperatures lg(T)
      DOUBLE PRECISION XI        ! Current X value (lg nu)
      DOUBLE PRECISION FJ( MAXCMP ) ! f_j(x_i)
      DOUBLE PRECISION FI        ! f(x_i)
      DOUBLE PRECISION LOGDIF    ! Relative importance of components

*  External References:
      DOUBLE PRECISION SPD_UAAQD ! Planck function value

*.

*  Recover fixed centres, peaks, sigmas.
      DO 1 J = 1, NCOMP
         IF (         PARFLG(J) .EQ. J ) C(J) = PAR0(J)
         IF (  PARFLG(J+MAXCMP) .EQ. J ) P(J) = PAR0(J+MAXCMP)
         IF (PARFLG(J+2*MAXCMP) .EQ. J ) S(J) = PAR0(J+2*MAXCMP)
 1    CONTINUE

*  Recover free centres, peaks, sigmas.
      DO 2 J = 1, NCOMP
         IF ( PARFLG(J) .EQ. 0 )
     :      C(J) = PAR0(J) + PAR1(PARNO(J))
         IF ( PARFLG(J+MAXCMP) .EQ. 0 )
     :      P(J) = PAR0(J+MAXCMP) + PAR1(PARNO(J+MAXCMP))
         IF ( PARFLG(J+2*MAXCMP) .EQ. 0 )
     :      S(J) = PAR0(J+2*MAXCMP) + PAR1(PARNO(J+2*MAXCMP))
 2    CONTINUE

*  Recover tied centres, peaks, sigmas.
      DO 3 J = 1, NCOMP
         IF ( PARFLG(J) .NE. J .AND. PARFLG(J) .NE. 0 )
     :      C(J) = C(PARFLG(J)) + PAR0(J) - PAR0(PARFLG(J))
         IF ( PARFLG(J+MAXCMP) .NE. J .AND. PARFLG(J+MAXCMP) .NE. 0 )
     :      P(J) = P(PARFLG(J+MAXCMP))
     :           + PAR0(J+MAXCMP)
     :           - PAR0(PARFLG(J+MAXCMP)+MAXCMP)
         IF ( PARFLG(J+2*MAXCMP) .NE. J .AND.
     :        PARFLG(J+2*MAXCMP) .NE. 0 )
     :      S(J) = S(PARFLG(J+2*MAXCMP))
     :           + PAR0(J+2*MAXCMP)
     :           - PAR0(PARFLG(J+2*MAXCMP)+2*MAXCMP)
 3    CONTINUE

*  Initialise F and grad F, which are sums over data points i.
      FVAL = 0D0

*  Now tackle the value and gradient of the objective function.
*  Both are sums over the data points, i.e. index I.
      DO 16 I = 1, NDATA
         XI = DBLE( MSKXDW(I) )

*     For each component j=J, work out the f_j(x_i).
         DO 10 J = 1, NCOMP
            FJ(J) = C(J) + P(J) * XI + SPD_UAAQD( S(J), XI )
 10      CONTINUE

*     Sum up into f(x_i), but keep all f_j(x_i) in mind.
         FI = FJ(1)
         DO 11 J = 2, NCOMP
            LOGDIF = ABS( FI - FJ(J) )
            FI = MAX( FI, FJ(J) )
            IF ( LOGDIF .LT. 15D0 )
     :         FI = FI + LOG10( 1D0 + 1D1 ** (-LOGDIF) )
 11      CONTINUE

*     Add contribution from f(x_i) into F.
         ARG = DBLE( MSKXDW(I+NDATA) ) - FI
         FVAL = FVAL + DBLE(MSKXDW(I+2*NDATA)) * ARG * ARG

 16   CONTINUE

*  Final scaling.
      FVAL = FVAL / FSCALE

*  Return.
      END
