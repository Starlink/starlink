      SUBROUTINE SPD_WFTD( FITPAR, PAR1, FVAL )
*+
*  Name:
*     SPD_WFTD

*  Purpose:
*     6-triangle objective function for PDA_UNCMND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WFTD( FITPAR, PAR1, FVAL )

*  Description:
*     This routine returns the value of the objective
*     function for the minimising routine PDA_UNCMND.
*     The objective function is a scaled chi-squared with the model
*     being the sum of up to six triangle functions.
*     The objective function F(a) is
*
*               sum{w_i (d_i - f(x_i))^2}
*     F(a_k) = -----------------------------
*                        F_{scale}
*
*     where f is supposed to fit the data d_i as a function of x_i. w_i
*     are the weights, i.e. the reciprocal variances of d_i. f is the
*     sum of up to six triangle functions plus a constant fixed
*     continuum:
*
*     f(x) = cont + sum { p_j [ 1 +- (c_j - x) / s_j ] }
*          for x >< c_j
*
*     The k-th fit parameter a_k corresponds to the centre, peak, or
*     FWHM of the j-th component.
*     a_k are scaled versions of the free triangle parameters. Since not
*     all triangle parameters are free, there is a permutation between
*     indexes k and j. The scaling is such that the guess c_{j,orig},
*     p_{j,orig}, s_{j,orig} is turned into a_k = 0, and that the fit
*     result probably is within [-1;+1]. PDA_UNCMND tries to improve the
*     guess by varying a_k, and this routine must unscale these to
*     calculate the fit function f(x) they represent. In fact this
*     routine must work out all triangle parameters (fixed, free, tied)
*     in accordance with the a_k proposed by E04DGF. For the fixed
*     parameters this is trivial. For the free parameters:
*
*     c_j = c_{j,orig} + 2 s_{j,orig} a_k1
*
*     p_j = p_{j,orig} + p_{j,orig} a_k2
*
*     s_j = s_{j,orig} + s_{j,orig} a_k3
*
*     Once the fixed and free parameters are known, the tied ones are
*     (jth component tied to ith):
*
*     c_j = c_i + c_{j,orig} - c_{i,orig}
*
*     p_j = p_i * p_{j,orig} / p_{i,orig}
*
*     s_j = s_i * s_{j,orig} / s_{i,orig}
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
*     is the scaling factor F_{scale}.

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
*     NCOMP = INTEGER (Common /SPD_WFCM/)
*        j_{max}. Number of components.
*     DCONT = DOUBLE PRECISION (Common /SPD_WFCM/)
*        The constant continuum underlying the profiles.
*     FSCALE = DOUBLE PRECISION (Common /SPD_WFCM/)
*        Scaling factor. The objective function is proportional to
*        1/FSCALE. FSCALE should be such that the objective function is
*        of order unity near its minimum.
*     PARNO( 3*6 ) = INTEGER (Common /SPD_WFCM/)
*        Permutation vector for parameters.
*        PAR1(PARNO(I)) corresponds to PAR0(I). PAR1 is not part of this
*        COMMON block.
*        First in the array PAR1 come all the free line centres in
*        ascending order of component number. Then come all free line
*        peaks in ascending order of component number. Then come all
*        free line dispersions.
*        Last in the array PAR1 come all fixed or tied or unused centres
*        in descending order of component number. Before that come all
*        fixed or tied or unused peaks in descending order of component
*        number. Before that come all fixed or tied of unused line
*        dispersions in descending order of component number.
*     PARFLG( 3*6 ) = INTEGER (Common /SPD_WFCM/)
*        A packed version of the fit flags:
*        PARFLG={CFLAGS(1) ... CFLAGS(NCOMP),NCOMP+1,...,6,
*                PFLAGS(1) ... PFLAGS(NCOMP),NCOMP+1,...,6,
*                SFLAGS(1) ... SFLAGS(NCOMP),NCOMP+1,...,6}
*        For each component I a value e.g. CFLAGS(I)=0 indicates
*        that CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J). A positive value PFLAGS(I)
*        or SFLAGS(I)=J<I indicates that PEAK(I) or SIGMA(I) has to keep
*        a fixed ratio to PEAK(J) or SIGMA(J).
*        Deviating from CFLAGS etc., unused components' parameters are
*        flagged as fixed.
*     PAR0( 3*6 ) = DOUBLE PRECISION (Common /SPD_WFCM/)
*        A packed version of the unscaled guess parameters:
*        PAR0={CENTRE(1) ... CENTRE(NCOMP),0,0,...,0,
*                PEAK(1) ...   PEAK(NCOMP),0,0,...,0,
*               SIGMA(1) ...  SIGMA(NCOMP),1,1,...,1}
*        Deviating from CENTRE etc., unused components' parameters are
*        set to 0 or 1, whichever causes less harm.

*  Authors:
*     ajlf: Amadeu Fernandes (UoE)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31 Jan 1992 (ajlf):
*        Adapted from Specdre 0.6's SPFGAU.
*     23 Jul 1992 (hme):
*        Re-adapted from Specdre 0.7's SPFGAU.
*        Make locals C,P,S single precision.
*     12 Aug 1992 (hme):
*        Re-arrange COMMON block, N*8 byte groups first, NGAUSS last.
*     25 Nov 1994 (hme):
*        Renamed from SPABD, common block in include file.
*     20 Nov 1995 (hme):
*        Replace E04DGF/E04DKF with PDA_UNCMND.
*     2005 June 2 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
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

*  SPD_WFTD itself only combines the arguments from PDA_UNCMND with the
*  information in the SPD_WFCM common block. The extra layer is
*  necessary, since the common block contains a pointer to a
*  variable-size array.
      CALL SPD_WFTE( FITPAR, PAR1, FVAL, NDATA,
     :               %VAL( CNF_PVAL(DATAP) ), NCOMP, DCONT, FSCALE,
     :               PARNO, PARFLG, PAR0 )

      END




      SUBROUTINE SPD_WFTE( FITPAR, PAR1, FVAL, NDATA, MSKXDW,
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
      INTEGER I, J               ! Loop indices
      DOUBLE PRECISION C( MAXCMP ) ! centre positions
      DOUBLE PRECISION P( MAXCMP ) ! peak heights
      DOUBLE PRECISION S( MAXCMP ) ! dispersions
      DOUBLE PRECISION GAUSS     ! f(x) at any one x
      DOUBLE PRECISION ARG       ! Argument of exp function

*  Internal References:
      DOUBLE PRECISION SPD_UAATD ! Fit function value

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
     :      C(J) = PAR0(J) + 2D0 * PAR0(J+2*MAXCMP) * PAR1(PARNO(J))
         IF ( PARFLG(J+MAXCMP) .EQ. 0 )
     :      P(J) = PAR0(J+MAXCMP) * ( 1D0 + PAR1(PARNO(J+MAXCMP)) )
         IF ( PARFLG(J+2*MAXCMP) .EQ. 0 )
     :      S(J) = PAR0(J+2*MAXCMP) * ( 1D0 + PAR1(PARNO(J+2*MAXCMP)) )
 2    CONTINUE

*  Recover tied centres, peaks, sigmas.
      DO 3 J = 1, NCOMP
         IF ( PARFLG(J) .NE. J .AND. PARFLG(J) .NE. 0 )
     :      C(J) = C(PARFLG(J)) + PAR0(J) - PAR0(PARFLG(J))
         IF ( PARFLG(J+MAXCMP) .NE. J .AND. PARFLG(J+MAXCMP) .NE. 0 )
     :      P(J) = P(PARFLG(J+MAXCMP)) * PAR0(J+MAXCMP)
     :           / PAR0(PARFLG(J+MAXCMP)+MAXCMP)
         IF ( PARFLG(J+2*MAXCMP) .NE. J .AND.
     :        PARFLG(J+2*MAXCMP) .NE. 0 )
     :      S(J) = S(PARFLG(J+2*MAXCMP)) * PAR0(J+2*MAXCMP)
     :           / PAR0(PARFLG(J+2*MAXCMP)+2*MAXCMP)
 3    CONTINUE

*  Take precautions against divide by zero.
      DO 4 J = 1, NCOMP
         IF ( S(J) .EQ. 0. ) THEN
            S(J) = 1.
            P(J) = 0.
         END IF
 4    CONTINUE

*  Evaluate objective function F.
      FVAL = 0D0
      DO 6 I = 1, NDATA

*     Function f at x.
         GAUSS = DCONT
         DO 5 J = 1, NCOMP
            GAUSS = GAUSS
     :            + SPD_UAATD( C(J), P(J), S(J), DBLE(MSKXDW(I)) )
 5       CONTINUE

*     Add x's contribution to F.
         ARG = DBLE(MSKXDW(I+NDATA)) - GAUSS
         FVAL = FVAL + DBLE(MSKXDW(I+2*NDATA)) * ARG * ARG
 6    CONTINUE
      FVAL = FVAL / FSCALE

*  Return.
      END
