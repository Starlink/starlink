#if HAVE_CONFIG_H
#  include "config.h"
#endif
      SUBROUTINE SPD_WFGA( INFO, COVRSX, NELM, NCOMP_ARG, FITPAR,
     :                     FITDIM, CONT, XDWC, CFLAGS, PFLAGS,
     :                     SFLAGS, CENTRE, PEAK, SIGMA,
     :                     CHISQR, COVAR, FITTED, STATUS )
*+
*  Name:
*     SPD_WFGA

*  Purpose:
*     Fit multi-Gauss profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WFGA( INFO, COVRSX, NELM, NCOMP, FITPAR, FITDIM,
*        CONT, XDWC, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA,
*        CHISQR, COVAR, FITTED, STATUS )

*  Description:
*     This routine interfaces AUTOFIT with the fit routine and
*     its object function routine.

*  Arguments:
*     INFO = LOGICAL (Given)
*        Ignored.
*     COVRSX = LOGICAL (Given)
*        Ignored.
*     NELM = INTEGER (Given)
*        Size of the data arrays.
*     NCOMP = INTEGER (Given)
*        The number of components to be fitted.
*     FITPAR = INTEGER (Given)
*        The number of free parameters to be fitted.
*     FITDIM = INTEGER (Given)
*        max( 1, FITPAR ). For dimension purposes.
*     CONT = REAL (Given)
*        The level of the continuum underlying the Gauss components.
*        Any constant value for the continuum is possible. This is not
*        a fit parameter, but must be known a priori.
*     XDWC( 3*NELM ) = REAL (Given)
*        The packed array of masked x, data, and weights.
*        Weights should be 1/variance, 1 if variance is not known.
*        All the arrays must not contain bad values.
*        XDWC(        1 :   NELM ): x values,
*        XDWC(   NELM+1 : 2*NELM ): data values,
*        XDWC( 2*NELM+1 : 3*NELM ): weight values,
*     CFLAGS( 10 ) = INTEGER (Given)
*        For each component I a value CFLAGS(I)=0 indicates that
*        CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J).
*     PFLAGS( 10 ) = INTEGER (Given)
*        For each component I a value PFLAGS(I)=0 indicates that
*        PEAK(I) holds a guess which is free to be fitted.
*        A positive value PFLAGS(I)=I indicates that PEAK(I) is fixed.
*        A positive value PFLAGS(I)=J<I indicates that PEAK(I) has to
*        keep a fixed ratio to PEAK(J).
*     SFLAGS( 10 ) = INTEGER (Given)
*        For each component I a value SFLAGS(I)=0 indicates that
*        SIGMA(I) holds a guess which is free to be fitted.
*        A positive value SFLAGS(I)=I indicates that SIGMA(I) is fixed.
*        A positive value SFLAGS(I)=J<I indicates that SIGMA(I) has to
*        keep a fixed ratio to SIGMA(J).
*     CENTRE( 10 ) = REAL (Given and Returned)
*        Centre position for each component.
*     PEAK( 10 ) = REAL (Given and Returned)
*        Peak height for each component.
*     SIGMA( 10 ) = REAL (Given and Returned)
*        Dispersion for each component.
*     CHISQR = REAL (Returned)
*        The final chi-squared value for the fit. That is, if the
*        weights were 1/variance. If all weights were 1, then this is
*        the sum of squared residuals. Then the rms would be
*        SQRT(CHISQR/degrees_of_freedom).
*     COVAR( 1, 1 ) = DOUBLE PRECISION (Returned)
*        Unused.
*     FITTED = LOGICAL (Returned)
*        True if fit was successful.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine returns a bad status if
*        -  it was entered with one,
*        -  there are no free parameters or no degrees of freedom,
*        -  there are errors in the ties between parameters,
*        -  E04DGF returns with a bad NAG status (including the case
*           where the fit did not converge before the iteration limit),
*        -  a free peak or width parameters is fitted as exactly zero.
*        Good status is zero.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     01 May 1991 (hme):
*        Original version (DOFIT).
*     26 Jun 1991 (hme):
*        Calculate the covariance matrix for Gauss fit by calling
*        SPFHSS.
*        Proper error check after AUTOFIT.
*        Make CONTFIT oblivious, call E02ADF directly.
*        Replace variance by weights. Masked x, data, weights DOUBLE.
*     19 Jul 1991 (hme):
*        Replace AUTOFIT. Call E04DGF instead. Its objective function is
*        SPFGAU. Use a packed array XDWC instead of three separate.
*     22 Jul 1991 (hme):
*        Correct size of ALFA and UNITY. Scrap oblivious arguments.
*        Rename matrix A to COVAR. FITDIM.
*     23 Jul 1991 (hme):
*        Report no. of iterations from E04DGF.
*        Put fit results in ADAM parameters FITCENT, FITPEAK, FITFWHM.
*     29 Oct 1991 (hme):
*        Call the re-written SPFHSS. Rename ALFA to HESSE.
*     27 Nov 1991 (hme):
*        Call NAG with IFAIL=+1 to suppress NAG's error messages.
*        Polish FITGAUSS messages.
*     22 Apr 1992 (hme):
*        Disable polynomial fit. Make masked arrays _REAL. Provide for
*        using covariance row sums. Pass x values and weights separately
*        to SPFHSS. Don't put fitted values into ADAM parameters any
*        more.
*     12 Aug 1992 (hme):
*        Re-arrange COMMON block, N*8 byte groups first, NGAUSS last.
*     08 Jun 1993 (hme):
*        Also return chi-squared. This simplifies scaling of COVAR
*        matrix in case where all weights were 1.
*     10 Jun 1993 (hme):
*        Replace the half-baked concept of warnings with proper error
*        reporting. The calling routine must then supress reports and
*        ignore status.
*     23 Jun 1993 (hme):
*        After fit, see that sigma is positive.
*        Increase iteration limit to 200.
*     30 Jun 1993 (hme):
*        Reject zero peak or with as a result.
*     25 Nov 1994 (hme):
*        Renamed from SPFDFT, common block in include file.
*     23 Aug 1995 (hme):
*        For Figaro 5.0 adapt error reporting to use status 0/1 and
*        PAR_WRUSER. Don't work out covariance. No longer use NAG.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External References:
      EXTERNAL SPD_WFGC          ! Objective function for PDA_UNCMND

*  Global Variables:

*+
*  Name:
*     SPD_WFCM

*  Purpose:
*     Common block for Gauss fitting with PDA_UNCMND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INCLUDE 'SPD_WFCM'

*  Description:
*     This declares the common block used by the line fit routines
*     in this source file to transfer information through the PDA
*     minimisation routine to its objective function.

*  Constants:
*     MAXCMP = INTEGER
*        The number of line components provided for.

*  Variables:
*     NCOMP = INTEGER
*        Number of line components.
*     IUSER = INTEGER
*        This replaces the IUSER() argument to E04DGF.
*     POINTR = INTEGER
*        This is the address of the USER() argument to E04DGF.
*     DCONT = DOUBLE PRECISION
*        The constant continuum underlying the line profiles.
*     FSCALE = DOUBLE PRECISION
*        Scaling factor. The objective function is proportional to
*        1/FSCALE. FSCALE should be such that the objective function is
*        of order unity near its minimum.
*     PARNO( 3*MAXCMP ) = INTEGER
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
*     PARFLG( 3*MAXCMP ) = INTEGER
*        A packed version of the fit flags:
*        PARFLG={CFLAGS(1) ... CFLAGS(NCOMP),NCOMP+1,...,MAXCMP,
*                PFLAGS(1) ... PFLAGS(NCOMP),NCOMP+1,...,MAXCMP,
*                SFLAGS(1) ... SFLAGS(NCOMP),NCOMP+1,...,MAXCMP}
*        For each component I a value e.g. CFLAGS(I)=0 indicates
*        that CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J). A positive value PFLAGS(I)
*        or SFLAGS(I)=J<I indicates that PEAK(I) or SIGMA(I) has to keep
*        a fixed ratio to PEAK(J) or SIGMA(J).
*        Deviating from CFLAGS etc., unused components' parameters are
*        flagged as fixed.
*     PAR0( 3*MAXCMP ) = DOUBLE PRECISION
*        A packed version of the unscaled guess parameters:
*        PAR0={CENTRE(1) ... CENTRE(NCOMP),0,0,...,0,
*                PEAK(1) ...   PEAK(NCOMP),0,0,...,0,
*               SIGMA(1) ...  SIGMA(NCOMP),1,1,...,1}
*        Deviating from CENTRE etc., unused components' parameters are
*        set to 0 or 1, whichever causes less harm.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 Nov 1994 (hme):
*        Original version.
*     23 Aug 1995 (hme):
*        For Figaro 5.0 included directly in Fortran file, and MAXCMP
*        increased from 6 to 10. Add IUSER and POINTR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INTEGER MAXCMP
      PARAMETER ( MAXCMP = 10 )
      INCLUDE 'CNF_PAR'            ! CNF functions

*  Global Variables:
      INTEGER          NCOMP
      INTEGER          IUSER
      INTEGER          POINTR
      DOUBLE PRECISION DCONT
      DOUBLE PRECISION FSCALE
      INTEGER          PARNO(  3*MAXCMP )
      INTEGER          PARFLG( 3*MAXCMP )
      DOUBLE PRECISION PAR0(   3*MAXCMP )
      COMMON / SPD_WFCM / DCONT, FSCALE, PARNO, PARFLG, PAR0,
     :   NCOMP, IUSER, POINTR

*.

*  Arguments Given:
      LOGICAL INFO
      LOGICAL COVRSX
      INTEGER NELM
      INTEGER NCOMP_ARG
      INTEGER FITPAR
      INTEGER FITDIM
      REAL CONT
      REAL XDWC( 3*NELM )
      INTEGER CFLAGS( MAXCMP )
      INTEGER PFLAGS( MAXCMP )
      INTEGER SFLAGS( MAXCMP )

*  Arguments Given and Returned:
      REAL CENTRE( MAXCMP )
      REAL PEAK(   MAXCMP )
      REAL SIGMA(  MAXCMP )

*  Arguments Returned:
      REAL CHISQR
      DOUBLE PRECISION COVAR( 1, 1 )
      LOGICAL FITTED

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J, K            ! Loop indices
      INTEGER IFAIL1             ! NAG status
      DOUBLE PRECISION PAR1( 3*MAXCMP ) ! Permuted and scaled PAR0, guess
      DOUBLE PRECISION PAR2( 3*MAXCMP ) ! Permuted and scaled PAR0, fit
      DOUBLE PRECISION FVAL      ! Value of objective function
      DOUBLE PRECISION WORK( 3*MAXCMP*(3*MAXCMP+10) ) ! Work space
      LOGICAL ISREG              ! Pointer already registered with CNF.

*.

*  Check.
      IF ( STATUS .NE. 0 ) RETURN
      IF ( FITPAR .LT. 1 .OR.
     :     NCOMP_ARG  .LT. 1 .OR. NCOMP_ARG .GT. MAXCMP .OR.
     :     NELM   .LT. FITPAR ) THEN
         FITTED = .FALSE.
         CALL PAR_WRUSER( 'SPD_WFGA: Error: No fit ' //
     :      'performed. Probably too few or too many free ' //
     :      'parameters, or no degrees of freedom.', STATUS )
         STATUS = 1
         GO TO 500
      END IF


*  Check flags and parameters and put them into the common block.
      DO 1 I = 1, NCOMP_ARG

*     Check that ties are only to earlier components.
         IF ( CFLAGS(I) .LT. 0 .OR. CFLAGS(I) .GT. I .OR.
     :        PFLAGS(I) .LT. 0 .OR. PFLAGS(I) .GT. I .OR.
     :        SFLAGS(I) .LT. 0 .OR. SFLAGS(I) .GT. I ) THEN
            FITTED = .FALSE.
            CALL PAR_WRUSER( 'SPD_WFGA: Error: ' //
     :         'Can tie only to earlier component.', STATUS )
            STATUS = 1
            GO TO 500
         END IF

*     Check that centre ties are only to free centres.
         IF ( CFLAGS(I) .NE. 0 .AND. CFLAGS(I) .NE. I ) THEN
            IF ( CFLAGS(CFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               CALL PAR_WRUSER( 'SPD_WFGA: Error: ' //
     :            'Can tie only to free parameter.', STATUS )
               STATUS = 1
               GO TO 500
            END IF
         END IF

*     Check that peak ties are only to free peaks.
         IF ( PFLAGS(I) .NE. 0 .AND. PFLAGS(I) .NE. I ) THEN
            IF ( PFLAGS(PFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               CALL PAR_WRUSER( 'SPD_WFGA: Error: ' //
     :            'Can tie only to free parameter.', STATUS )
               STATUS = 1
               GO TO 500
            END IF
         END IF

*     Check that width ties are only to free widths.
         IF ( SFLAGS(I) .NE. 0 .AND. SFLAGS(I) .NE. I ) THEN
            IF ( SFLAGS(SFLAGS(I)) .NE. 0 ) THEN
               FITTED = .FALSE.
               CALL PAR_WRUSER( 'SPD_WFGA: Error: ' //
     :            'Can tie only to free parameter.', STATUS )
               STATUS = 1
               GO TO 500
            END IF
         END IF

*     Packed array of all Gauss parameters.
         PAR0(I)          = DBLE(CENTRE(I))
         PAR0(I+MAXCMP)   = DBLE(PEAK(I))
         PAR0(I+2*MAXCMP) = DBLE(SIGMA(I))

*     Packed array of all fit flags.
         PARFLG(I)          = CFLAGS(I)
         PARFLG(I+MAXCMP)   = PFLAGS(I)
         PARFLG(I+2*MAXCMP) = SFLAGS(I)
 1    CONTINUE

*  Fill the COMMON block, except for PARNO.
*  FSCALE is set to 2 for the initial direct call to SPD_WFGB. This is
*  in anticipation that the final value of the objective function
*  will be 1/2 of its value for the guess.
*  All flags for unused components are set to 1 (fixed) in the COMMON
*  block. Also all unused components' parameters are set to 0 or 1,
*  whichever causes less harm.
      NCOMP  = NCOMP_ARG
      DCONT  = DBLE(CONT)
      FSCALE = 2D0
      DO 2 I = NCOMP_ARG+1, MAXCMP
         PAR0(I)          = 0D0
         PAR0(I+MAXCMP)   = 0D0
         PAR0(I+2*MAXCMP) = 1D0
         PARFLG(I)          = I
         PARFLG(I+MAXCMP)   = I
         PARFLG(I+2*MAXCMP) = I
 2    CONTINUE

*  PARNO: Permutation of parameters,
*  such that free parameters come to the beginning of PAR1.
*  (PAR1 itself need not worry us here, since its guess value is
*  always == 0. But this is done in passing.)
      J = 0
      K = 3 * MAXCMP + 1
      DO 3 I = 1, 3*MAXCMP
         IF ( PARFLG(I) .EQ. 0 ) THEN

*        PAR0(I) is free and corresponds to PAR1(J).
            J = J + 1
            PARNO(I) = J
         ELSE

*        PAR0(I) is not free and corresponds to PAR1(K).
            K = K - 1
            PARNO(I) = K
         END IF
         PAR1(I) = 0D0
 3    CONTINUE

*  Check if permutation went all right.
      IF ( K .NE. J+1 .OR. J .NE. FITPAR ) THEN
         FITTED = .FALSE.
         CALL PAR_WRUSER( 'SPD_WFGA: Error: No fit ' //
     :      'performed. Flags inconsistent with number of free' //
     :      'parameters.', STATUS )
         STATUS = 1
         GO TO 500
      END IF

*  Stick the number of data points and the data into the common block.
*  Since the size of the data is not known at compile time, the common
*  block can store only a pointer to it. SPD_WFGC will de-reference that
*  pointer by calling SPD_WFGB with argument %VAL(POINTR).
*  Register this pointer with CNF so that it can be used on 64bit machines.
      IUSER = NELM
#if HAVE_PERCENTLOC
      POINTR = CNF_PREG( %LOC(XDWC), ISREG )
#elif HAVE_INTRINSIC_LOC
      POINTR = CNF_PREG( LOC(XDWC), ISREG )
#else
      do not have LOC or %LOC
#endif

*  Derive FSCALE by direct call to SPD_WFGB with initial FSCALE = 2.
      CALL SPD_WFGC( FITPAR, PAR1, FVAL )
      FSCALE = FVAL

*  This gives the initial chi squared.
      CHISQR = SNGL( FVAL * FSCALE )

*  Call PDA_UNCMND.
      IFAIL1 = 0
      CALL PDA_UNCMND( FITPAR, PAR1, SPD_WFGC, PAR2, FVAL, IFAIL1,
     :   WORK, 3*MAXCMP*(3*MAXCMP+10) )

*  Post-fit processing.
      IF ( IFAIL1 .GE. 0 .AND. IFAIL1 .LE. 3 ) THEN
         FITTED = .TRUE.

*     Last objective function value gives final chi squared.
         CHISQR = SNGL( FVAL * FSCALE )

*     Retrieve free parameters.
         DO 4 J = 1, NCOMP_ARG
            IF ( PARFLG(J) .EQ. 0 )
     :         CENTRE(J) = SNGL( PAR0(J)
     :                   + 2D0 * PAR0(J+2*MAXCMP) * PAR2(PARNO(J)) )
            IF ( PARFLG(J+MAXCMP) .EQ. 0 )
     :         PEAK(J)   = SNGL( PAR0(J+MAXCMP)
     :                   * ( 1D0 + PAR2(PARNO(J+MAXCMP)) ) )
            IF ( PARFLG(J+2*MAXCMP) .EQ. 0 )
     :         SIGMA(J)  = ABS( SNGL( PAR0(J+2*MAXCMP)
     :                   * ( 1D0 + PAR2(PARNO(J+2*MAXCMP)) ) ) )
 4       CONTINUE

*     Retrieve tied parameters.
         DO 5 J = 1, NCOMP_ARG
            IF ( PARFLG(J) .NE. J .AND. PARFLG(J) .NE. 0 )
     :         CENTRE(J) = CENTRE(PARFLG(J))
     :                   + SNGL( PAR0(J) - PAR0(PARFLG(J)) )
            IF ( PARFLG(J+MAXCMP) .NE. J .AND.
     :           PARFLG(J+MAXCMP) .NE. 0 )
     :         PEAK(J)   = PEAK(PARFLG(J+MAXCMP))
     :                   * SNGL( PAR0(J+MAXCMP)
     :                   / PAR0(PARFLG(J+MAXCMP)+MAXCMP) )
            IF ( PARFLG(J+2*MAXCMP) .NE. J .AND.
     :           PARFLG(J+2*MAXCMP) .NE. 0 )
     :         SIGMA(J)  = SIGMA(PARFLG(J+2*MAXCMP))
     :                   * SNGL( PAR0(J+2*MAXCMP)
     :                   / PAR0(PARFLG(J+2*MAXCMP)+2*MAXCMP) )
 5       CONTINUE

*     If any of the peak or width parameters turns out to be exactly
*     zero, we have an error condition. For one it is a non-existing
*     component.
         DO 6 J = 1, NCOMP_ARG
            IF ( PEAK(J) .EQ. 0. .OR. SIGMA(J) .EQ. 0. ) THEN
               FITTED = .FALSE.
               CALL PAR_WRUSER( 'SPD_WFGA: Error: Zero ' //
     :            'peak or width after fit.', STATUS )
               STATUS = 1
               GO TO 500
            END IF
 6       CONTINUE
      ELSE

*     PDA_UNCMND failed.
         FITTED = .FALSE.
         CALL PAR_WRUSER( 'SPD_WFGA: Error: PDA_UNCMND ' //
     :      'returned with error status.', STATUS )
         STATUS = 1
         GO TO 500
      END IF

*  Return.
 500  CONTINUE
      END



      SUBROUTINE SPD_WFGC( FITPAR, PAR1, FVAL )
*+
*  Name:
*     SPD_WFGC

*  Purpose:
*     Go-between for PDA_UNCMND and SPD_WFGB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WFGC( FITPAR, PAR1, FVAL )

*  Description:
*     This routine turns the pointer POINTR into an array while calling
*     SPD_WFGB.

*  Arguments:
*     FITPAR = INTEGER (Given)
*        k_{max}. The number of parameters to be fitted.
*     PAR1( FITPAR ) = DOUBLE PRECISION (Given)
*        a_k. The current values of fit parameters.
*     FVAL = DOUBLE PRECISION (Returned)
*        F(a_k). The value of the objective function for given PAR1 and
*        other information obtained through IUSER and MSKXDW.
*     %VAL(POINTR)( 3*IUSER ) = DOUBLE PRECISION (Given)
*        The packed array of masked X, DAT, WHT.
*        MSKXDW(         1 :   IUSER ): x values x_i.
*        MSKXDW(   IUSER+1 : 2*IUSER ): data values d_i.
*        MSKXDW( 2*IUSER+1 : 3*IUSER ): weight values w_i.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     23 Aug 1995 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:

*+
*  Name:
*     SPD_WFCM

*  Purpose:
*     Common block for Gauss fitting with PDA_UNCMND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INCLUDE 'SPD_WFCM'

*  Description:
*     This declares the common block used by the line fit routines
*     in this source file to transfer information through the PDA
*     minimisation routine to its objective function.

*  Constants:
*     MAXCMP = INTEGER
*        The number of line components provided for.

*  Variables:
*     NCOMP = INTEGER
*        Number of line components.
*     IUSER = INTEGER
*        This replaces the IUSER() argument to E04DGF.
*     POINTR = INTEGER
*        This is the address of the USER() argument to E04DGF.
*     DCONT = DOUBLE PRECISION
*        The constant continuum underlying the line profiles.
*     FSCALE = DOUBLE PRECISION
*        Scaling factor. The objective function is proportional to
*        1/FSCALE. FSCALE should be such that the objective function is
*        of order unity near its minimum.
*     PARNO( 3*MAXCMP ) = INTEGER
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
*     PARFLG( 3*MAXCMP ) = INTEGER
*        A packed version of the fit flags:
*        PARFLG={CFLAGS(1) ... CFLAGS(NCOMP),NCOMP+1,...,MAXCMP,
*                PFLAGS(1) ... PFLAGS(NCOMP),NCOMP+1,...,MAXCMP,
*                SFLAGS(1) ... SFLAGS(NCOMP),NCOMP+1,...,MAXCMP}
*        For each component I a value e.g. CFLAGS(I)=0 indicates
*        that CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J). A positive value PFLAGS(I)
*        or SFLAGS(I)=J<I indicates that PEAK(I) or SIGMA(I) has to keep
*        a fixed ratio to PEAK(J) or SIGMA(J).
*        Deviating from CFLAGS etc., unused components' parameters are
*        flagged as fixed.
*     PAR0( 3*MAXCMP ) = DOUBLE PRECISION
*        A packed version of the unscaled guess parameters:
*        PAR0={CENTRE(1) ... CENTRE(NCOMP),0,0,...,0,
*                PEAK(1) ...   PEAK(NCOMP),0,0,...,0,
*               SIGMA(1) ...  SIGMA(NCOMP),1,1,...,1}
*        Deviating from CENTRE etc., unused components' parameters are
*        set to 0 or 1, whichever causes less harm.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 Nov 1994 (hme):
*        Original version.
*     23 Aug 1995 (hme):
*        For Figaro 5.0 included directly in Fortran file, and MAXCMP
*        increased from 6 to 10. Add IUSER and POINTR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INTEGER MAXCMP
      PARAMETER ( MAXCMP = 10 )

*  Global Variables:
      INTEGER          NCOMP
      INTEGER          IUSER
      INTEGER          POINTR
      DOUBLE PRECISION DCONT
      DOUBLE PRECISION FSCALE
      INTEGER          PARNO(  3*MAXCMP )
      INTEGER          PARFLG( 3*MAXCMP )
      DOUBLE PRECISION PAR0(   3*MAXCMP )
      COMMON / SPD_WFCM / DCONT, FSCALE, PARNO, PARFLG, PAR0,
     :   NCOMP, IUSER, POINTR

*.

*  Global Constants:
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER FITPAR
      DOUBLE PRECISION PAR1( FITPAR )

*  Arguments Returned:
      DOUBLE PRECISION FVAL

*.

      CALL SPD_WFGB( FITPAR, PAR1, FVAL, %VAL(CNF_PVAL(POINTR)) )

      END



      SUBROUTINE SPD_WFGB( FITPAR, PAR1, FVAL, MSKXDW )
*+
*  Name:
*     SPD_WFGB

*  Purpose:
*     10-Gauss objective function for PDA_UNCMND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WFGB( FITPAR, PAR1, FVAL, MSKXDW )

*  Description:
*     This routine returns the value of the objective
*     function for the minimising routine PDA_UNCMND.
*     The objective function is a scaled chi-squared with the model
*     being the sum of up to ten Gauss functions.
*     The objective function F(a) is
*
*               sum{w_i (d_i - f(x_i))^2}
*     F(a_k) = -----------------------------
*                        F_{scale}
*
*     where f is supposed to fit the data d_i as a function of x_i. w_i
*     are the weights, i.e. the reciprocal variances of d_i. f is the
*     sum of up to six Gauss functions plus a constant fixed continuum:
*
*     f(x) = cont + sum{p_j exp{-0.5 [(x-c_j)/s_j]^2}}
*
*     The k-th fit parameter a_k corresponds to the centre, peak, or
*     sigma of the j-th component.
*     a_k are scaled versions of the free Gauss parameters. Since not
*     all Gauss parameters are free, there is a permutation between
*     indexes k and j. The scaling is such that the guess c_{j,orig},
*     p_{j,orig}, s_{j,orig} is turned into a_k = 0, and that the fit
*     result probably is within [-1;+1]. PDA_UNCMND tries to improve the
*     guess by varying a_k, and this routine must unscale these to
*     calculate the fit function f(x) they represent. In fact this
*     routine must work out all Gauss parameters (fixed, free, tied) in
*     accordance with the a_k proposed by PDA_UNCMND. For the fixed
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
*     x_i, d_i, w_i are transferred through in the array MSKXDW. The
*     upper bound of i is transferred through the COMMON block in IUSER.
*     The Gauss parameters c_j, p_j, s_j can be fixed, free to fit, or
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
*     MSKXDW( 3*IUSER ) = DOUBLE PRECISION (Given)
*        The packed array of masked X, DAT, WHT.
*        MSKXDW(         1 :   IUSER ): x values x_i.
*        MSKXDW(   IUSER+1 : 2*IUSER ): data values d_i.
*        MSKXDW( 2*IUSER+1 : 3*IUSER ): weight values w_i.
*     NCOMP = INTEGER (Common /SPD_WFCM/)
*        j_{max}. Number of Gauss components.
*     CONT = DOUBLE PRECISION (Common /SPD_WFCM/)
*        The constant continuum underlying the Gauss profiles.
*     FSCALE = DOUBLE PRECISION (Common /SPD_WFCM/)
*        Scaling factor. The objective function is proportional to
*        1/FSCALE. FSCALE should be such that the objective function is
*        of order unity near its minimum.
*     PARNO( 3*10 ) = INTEGER (Common /SPD_WFCM/)
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
*     PARFLG( 3*10 ) = INTEGER (Common /SPD_WFCM/)
*        A packed version of the fit flags:
*        PARFLG={CFLAGS(1) ... CFLAGS(NCOMP),NCOMP+1,...,10,
*                PFLAGS(1) ... PFLAGS(NCOMP),NCOMP+1,...,10,
*                SFLAGS(1) ... SFLAGS(NCOMP),NCOMP+1,...,10}
*        For each Gauss component I a value e.g. CFLAGS(I)=0 indicates
*        that CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J). A positive value PFLAGS(I)
*        or SFLAGS(I)=J<I indicates that PEAK(I) or SIGMA(I) has to keep
*        a fixed ratio to PEAK(J) or SIGMA(J).
*        Deviating from CFLAGS etc., unused components' parameters are
*        flagged as fixed.
*     PAR0( 3*10 ) = DOUBLE PRECISION (Common /SPD_WFCM/)
*        A packed version of the unscaled guess parameters:
*        PAR0={CENTRE(1) ... CENTRE(NCOMP),0,0,...,0,
*                PEAK(1) ...   PEAK(NCOMP),0,0,...,0,
*               SIGMA(1) ...  SIGMA(NCOMP),1,1,...,1}
*        Deviating from CENTRE etc., unused components' parameters are
*        set to 0 or 1, whichever causes less harm.
*     IUSER = INTEGER (Common /SPD_WFCM/)
*        The number of data points, i.e. 1/3 the length of MSKXDW.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     19 Jul 1991 (hme):
*        Original version.
*     29 Oct 1991 (hme):
*        Cosmetics.
*     20 Apr 1992 (hme):
*        Make data vector _REAL. Interpret the third array as
*        1/variance, i.e. directly as weights.
*     12 Aug 1992 (hme):
*        Re-arrange COMMON block, N*8 byte groups first, NGAUSS last.
*     25 Nov 1994 (hme):
*        Renamed from SPFGAU, common block in include file.
*     23 Aug 1995 (hme):
*        For Figaro 5.0, simplify to return only function value, no
*        gradient.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:

*+
*  Name:
*     SPD_WFCM

*  Purpose:
*     Common block for Gauss fitting with PDA_UNCMND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     INCLUDE 'SPD_WFCM'

*  Description:
*     This declares the common block used by the line fit routines
*     in this source file to transfer information through the PDA
*     minimisation routine to its objective function.

*  Constants:
*     MAXCMP = INTEGER
*        The number of line components provided for.

*  Variables:
*     NCOMP = INTEGER
*        Number of line components.
*     IUSER = INTEGER
*        This replaces the IUSER() argument to E04DGF.
*     POINTR = INTEGER
*        This is the address of the USER() argument to E04DGF.
*     DCONT = DOUBLE PRECISION
*        The constant continuum underlying the line profiles.
*     FSCALE = DOUBLE PRECISION
*        Scaling factor. The objective function is proportional to
*        1/FSCALE. FSCALE should be such that the objective function is
*        of order unity near its minimum.
*     PARNO( 3*MAXCMP ) = INTEGER
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
*     PARFLG( 3*MAXCMP ) = INTEGER
*        A packed version of the fit flags:
*        PARFLG={CFLAGS(1) ... CFLAGS(NCOMP),NCOMP+1,...,MAXCMP,
*                PFLAGS(1) ... PFLAGS(NCOMP),NCOMP+1,...,MAXCMP,
*                SFLAGS(1) ... SFLAGS(NCOMP),NCOMP+1,...,MAXCMP}
*        For each component I a value e.g. CFLAGS(I)=0 indicates
*        that CENTRE(I) holds a guess which is free to be fitted.
*        A positive value CFLAGS(I)=I indicates that CENTRE(I) is fixed.
*        A positive value CFLAGS(I)=J<I indicates that CENTRE(I) has to
*        keep a fixed offset from CENTRE(J). A positive value PFLAGS(I)
*        or SFLAGS(I)=J<I indicates that PEAK(I) or SIGMA(I) has to keep
*        a fixed ratio to PEAK(J) or SIGMA(J).
*        Deviating from CFLAGS etc., unused components' parameters are
*        flagged as fixed.
*     PAR0( 3*MAXCMP ) = DOUBLE PRECISION
*        A packed version of the unscaled guess parameters:
*        PAR0={CENTRE(1) ... CENTRE(NCOMP),0,0,...,0,
*                PEAK(1) ...   PEAK(NCOMP),0,0,...,0,
*               SIGMA(1) ...  SIGMA(NCOMP),1,1,...,1}
*        Deviating from CENTRE etc., unused components' parameters are
*        set to 0 or 1, whichever causes less harm.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     25 Nov 1994 (hme):
*        Original version.
*     23 Aug 1995 (hme):
*        For Figaro 5.0 included directly in Fortran file, and MAXCMP
*        increased from 6 to 10. Add IUSER and POINTR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INTEGER MAXCMP
      PARAMETER ( MAXCMP = 10 )

*  Global Variables:
      INTEGER          NCOMP
      INTEGER          IUSER
      INTEGER          POINTR
      DOUBLE PRECISION DCONT
      DOUBLE PRECISION FSCALE
      INTEGER          PARNO(  3*MAXCMP )
      INTEGER          PARFLG( 3*MAXCMP )
      DOUBLE PRECISION PAR0(   3*MAXCMP )
      COMMON / SPD_WFCM / DCONT, FSCALE, PARNO, PARFLG, PAR0,
     :   NCOMP, IUSER, POINTR

*.

*  Arguments Given:
      INTEGER FITPAR
      DOUBLE PRECISION PAR1( FITPAR )
      REAL MSKXDW( 1 )           ! Actual length 3*IUSER

*  Arguments Returned:
      DOUBLE PRECISION FVAL

*  Local Variables:
      INTEGER I, J               ! Loop indices
      DOUBLE PRECISION C( MAXCMP ) ! Gauss centre positions
      DOUBLE PRECISION P( MAXCMP ) ! Gauss peak heights
      DOUBLE PRECISION S( MAXCMP ) ! Gauss dispersions
      DOUBLE PRECISION GAUSS     ! f(x) at any one x
      DOUBLE PRECISION ARG       ! Argument of exp function

*.

*  Recover fixed centres, peaks, sigmas.
      DO 1 J = 1, NCOMP
         IF (          PARFLG(J) .EQ. J ) C(J) = PAR0(J)
         IF (   PARFLG(J+MAXCMP) .EQ. J ) P(J) = PAR0(J+MAXCMP)
         IF ( PARFLG(J+2*MAXCMP) .EQ. J ) S(J) = PAR0(J+2*MAXCMP)
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
     :      P(J) = P(PARFLG(J+MAXCMP))
     :           * PAR0(J+MAXCMP) / PAR0(PARFLG(J+MAXCMP)+MAXCMP)
         IF ( PARFLG(J+2*MAXCMP) .NE. J .AND.
     :        PARFLG(J+2*MAXCMP) .NE. 0 )
     :      S(J) = S(PARFLG(J+2*MAXCMP))
     :           * PAR0(J+2*MAXCMP) / PAR0(PARFLG(J+2*MAXCMP)+2*MAXCMP)
 3    CONTINUE

*  Take precautions against divide by zero.
      DO 4 J = 1, NCOMP
         IF ( S(J) .EQ. 0D0 ) THEN
            S(J) = 1D0
            P(J) = 0D0
         END IF
 4    CONTINUE

*  Evaluate objective function F.
      FVAL = 0D0
      DO 6 I = 1, IUSER

*     Fit function f at x.
         GAUSS = DCONT
         DO 5 J = 1, NCOMP
            ARG = ( MSKXDW(I) - C(J) ) / S(J)
            GAUSS = GAUSS + P(J) * EXP(-ARG*ARG/2D0)
 5       CONTINUE

*     Add x's contribution to F.
         ARG = MSKXDW(I+IUSER) - GAUSS
         FVAL = FVAL + MSKXDW(I+2*IUSER) * ARG * ARG
 6    CONTINUE
      FVAL = FVAL / FSCALE

*  Return.
      END
