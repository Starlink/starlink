      SUBROUTINE AUTOFIT( GX, GXVALS, WID, MLM, HNORM, RANG, RANGW,
     :   GZVALS, GZRESID, WHT, TOTW, GN, GINFP, GINFH, GINFW,
     :   CONP, CONH, CONW, ICHAINP, ICHAINH, ICHAINW,
     :   CHAINP, CHAINH, CHAINW, XTOL, GONFP, GONFH, GONFW,
     :   IFAIL1, IFAIL2 )
*+
*  Name:
*     AUTOFIT

*  Purpose:
*     Gauss profile optimisation

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL AUTOFIT( GX, GXVALS, WID, MLM, HNORM, RANG, RANGW, GZVALS,
*        GZRESID, WHT, TOTW, GN, GINFP, GINFH, GINFW, CONP, CONH, CONW,
*        ICHAINP, ICHAINH, ICHAINW, CHAINP, CHAINH, CHAINW, XTOL, GONFP,
*        GONFH, GONFW, IFAIL1, IFAIL2 )

*  Description:
*     Performs the optimization of the fitting of profiles
*     by superposed Gaussians. The algorith from Specdre is used,
*     extended to 10 components rather than six.

*  Arguments:
*     GX = INTEGER (Given)
*        Number of values in arrays GXVALS and GZVALS
*     GXVALS( GX ) = REAL (Given)
*        X values of the profile to be fitted
*     WID = REAL (Given)
*        Unused.
*     MLM = REAL (Given)
*        Unused.
*     HNORM = REAL (Given)
*        Unused.
*     RANG = REAL (Given)
*        Unused.
*     RANGW = REAL (Given)
*        Unused.
*     GZVALS( GX ) = REAL (Given)
*        Y values of the profile to be fitted
*     GZRESID( GX ) = REAL (Given)
*        Y values of residuals on initial fit
*     WHT( GX ) = REAL (Given)
*        Value of the weight for each Y value
*     TOTW = REAL (Given)
*        Unused.
*     GN = INTEGER (Given)
*        Number of fitting Gaussians
*     GINFP( GN ) = REAL (Given)
*        Position of peak of Gaussian
*     GINFH( GN ) = REAL (Given)
*        Height of Gaussian peak
*     GINFW( GN ) = REAL (Given)
*        Width (sigma) of Gaussian
*     CONP( GN ) = INTEGER (Given)
*        The array of the index number of the
*        Gaussians whose peak position is to be held
*     CONH( GN ) = INTEGER (Given)
*        The array of the index number of the
*        Gaussians whose peak height is to be held
*     CONW( GN ) = INTEGER (Given)
*        The array of the index number of the
*        Gaussians whose sigma is to be held
*     ICHAINP( GN ) = INTEGER (Given)
*        The array of the chaining index
*        number of the Gaussians to be chained in position
*     CHAINP( GN ) = REAL (Given)
*        The array of values of the line seperations
*        of lines to be locked
*     ICHAINH( GN ) = INTEGER (Given)
*        The array of the chaining index number
*        of the Gaussians to be chained in height
*     CHAINH( GN ) = REAL (Given)
*        The array of values of the line height
*        ratios to be locked
*     ICHAINW( GN ) = INTEGER (Given)
*        The array of the chaining index number
*        of the Gaussians to be chained in width
*     CHAINW( GN ) = REAL (Given)
*        The array of values of the line width
*        ratios of lines to be locked
*     XTOL = DOUBLE PRECISION (Given)
*        Unused.
*     GONFP( NG ) = REAL (Returned)
*        Position of peak of optimized Gaussian
*     GONFH( NG ) = REAL (Returned)
*        Height of optimized Gaussian peak
*     GONFW( NG ) = REAL (Returned)
*        Width (sigma) of optimized Gaussian
*     IFAIL1 = INTEGER (Returned)
*        Always returned zero.
*     IFAIL2 = INTEGER (Returned)
*        Returned zero if fit successful, returned one otherwise.

*  Implementation Status:
*     The previous, undocumented, limit of 4096 data points is still
*     valid. If more data points are given, the surplus is ignored.

*  Authors:
*     jrw: Jeremy Walsh (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     ?? Feb 1987 (jrw):
*        Original version.
*     25 Apr 1987 (jrw):
*        Handles weighting of the residuals
*     15 Sep 1987 (jrw):
*        Handles single constraints or chained sets of
*        Gaussian fit parameters
*     26 Jul 1994 (hme):
*        Make common blocks SAVE. HME / UoE, Starlink.
*     21 Dec 1994 (hme):
*        Re-write routine to interface to SPD_WFGA.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER GX
      INTEGER GN
      INTEGER CONP( GN )
      INTEGER CONH( GN )
      INTEGER CONW( GN )
      INTEGER ICHAINP( GN )
      INTEGER ICHAINH( GN )
      INTEGER ICHAINW( GN )
      REAL MLM
      REAL HNORM
      REAL RANG
      REAL RANGW
      REAL GXVALS(  GX )
      REAL WID
      REAL GZVALS(  GX )
      REAL GZRESID( GX )
      REAL WHT(     GX )
      REAL TOTW
      REAL GINFP(  GN )
      REAL GINFH(  GN )
      REAL GINFW(  GN )
      REAL CHAINP( GN )
      REAL CHAINH( GN )
      REAL CHAINW( GN )
      DOUBLE PRECISION XTOL

*  Arguments Returned:
      REAL GONFP( GN )
      REAL GONFH( GN )
      REAL GONFW( GN )

*  Status:
      INTEGER IFAIL1
      INTEGER IFAIL2

*  Local Constants:
      INTEGER MAXCMP
      PARAMETER ( MAXCMP = 10 )
      INTEGER MAXDAT
      PARAMETER ( MAXDAT = 4096 )

*  Local Variables:
      LOGICAL FITTED
      INTEGER STATUS
      INTEGER I, L
      INTEGER LGX
      INTEGER FITPAR, FITDIM
      INTEGER CFLAGS( MAXCMP )
      INTEGER PFLAGS( MAXCMP )
      INTEGER SFLAGS( MAXCMP )
      REAL CENTRE( MAXCMP )
      REAL PEAK(   MAXCMP )
      REAL SIGMA(  MAXCMP )
      REAL XDWC( 3*MAXDAT )
      REAL CHISQR
      DOUBLE PRECISION COVAR( MAXCMP*MAXCMP )

*.

*  Translate the Figaro constraints to Specdre fit flags and a guess.
*  ==================================================================

*  Initialise flags as free.
      DO 1001 I = 1, MAXCMP
         CFLAGS(I) = 0
         PFLAGS(I) = 0
         SFLAGS(I) = 0
         CENTRE(I) = 0.
         PEAK(I)   = 0.
         SIGMA(I)  = 1.
 1001 CONTINUE

*  Initialise guess as if all parameters free.
      DO 1002 I = 1, GN
         CENTRE(I) = GINFP(I)
         PEAK(I)   = GINFH(I)
         SIGMA(I)  = GINFW(I)
 1002 CONTINUE
      FITPAR = 3 * GN

*  Scan for fixed parameters.
      DO 1003 I = 1, GN
         IF ( CONP(I) .EQ. 1 ) THEN
            CFLAGS(I) = I
            FITPAR = FITPAR - 1
         END IF
         IF ( CONH(I) .EQ. 1 ) THEN
            PFLAGS(I) = I
            FITPAR = FITPAR - 1
         END IF
         IF ( CONW(I) .EQ. 1 ) THEN
            SFLAGS(I) = I
            FITPAR = FITPAR - 1
         END IF
 1003 CONTINUE

*  Scan non-fixed parameters for ties (chains).
      DO 1004 I = 1, GN
         IF ( CFLAGS(I) .EQ. 0 ) THEN

*        If I-th component member of a centre chain.
            IF ( ICHAINP(I) .GT. 0 ) THEN

*           Look for chain leader.
               L = 1
 1005          IF ( ICHAINP(L) .NE. ICHAINP(I) ) THEN
                  L = L + 1
                  GO TO 1005
               END IF

*           If I-th component is not the leader.
               IF ( L .LT. I ) THEN

*              Tie the component to the chain leader.
                  CFLAGS(I) = L
                  FITPAR = FITPAR - 1

*              Set the guess according to given offset.
*              Figaro may not have used the first member of the chain as
*              leader, but Specdre must do just that. Using the
*              differential offset should do the trick.
                  CENTRE(I) = CENTRE(L) + CHAINP(I) - CHAINP(L)
               END IF
            END IF
         END IF
         IF ( PFLAGS(I) .EQ. 0 ) THEN
            IF ( ICHAINH(I) .GT. 0 ) THEN
               L = 1
 1006          IF ( ICHAINH(L) .NE. ICHAINH(I) ) THEN
                  L = L + 1
                  GO TO 1006
               END IF
               IF ( L .LT. I ) THEN
                  PFLAGS(I) = L
                  FITPAR = FITPAR - 1
                  PEAK(I) = PEAK(L) * CHAINH(I) / CHAINH(L)
               END IF
            END IF
         END IF
         IF ( SFLAGS(I) .EQ. 0 ) THEN
            IF ( ICHAINW(I) .GT. 0 ) THEN
               L = 1
 1007          IF ( ICHAINW(L) .NE. ICHAINW(I) ) THEN
                  L = L + 1
                  GO TO 1007
               END IF
               IF ( L .LT. I ) THEN
                  SFLAGS(I) = L
                  FITPAR = FITPAR - 1
                  SIGMA(I) = SIGMA(L) * CHAINW(I) / CHAINW(L)
               END IF
            END IF
         END IF
 1004 CONTINUE


*  Pack the x values, data, and weights.
*  =====================================

      LGX = MIN( GX, MAXDAT )
      DO 1008 I = 1, LGX
         XDWC(I)       = GXVALS(I)
         XDWC(LGX+I)   = GZVALS(I)
         XDWC(2*LGX+I) = WHT(I)
 1008 CONTINUE


*  Perform the fit.
*  ================

*  No info messages, no covariance row sums available, continuum is
*  zero. Ignore CHISQR and COVAR.
      CALL ERR_MARK
         STATUS = 0
         FITDIM = MAX( 1, FITPAR )
         CALL SPD_WFGA( .FALSE., .FALSE., LGX, GN, FITPAR, FITDIM, 0.,
     :      XDWC, CFLAGS, PFLAGS, SFLAGS, CENTRE, PEAK, SIGMA,
     :      CHISQR, COVAR, FITTED, STATUS )
         IF ( STATUS .NE. 0 ) CALL ERR_FLUSH( STATUS )
      CALL ERR_RLSE


*  Translate the fit back to Figaro.
*  =================================

*  The status from NAG is not available at this level. On the other
*  hand, the Specdre routines would have reported any errors. Having
*  looked at the evaluation of IFAIL1/2 the calling routine GAUMENU
*  makes after calling this routine, IFAIL1=0 (OK) and IFAIL2=0 (OK) or
*  1 (failure) seems to cause the least wrong messages.
      DO 1009 I = 1, GN
         GONFP(I) = CENTRE(I)
         GONFH(I) = PEAK(I)
         GONFW(I) = SIGMA(I)
 1009 CONTINUE
      IFAIL1 = 0
      IFAIL2 = 0
      IF ( .NOT. FITTED ) IFAIL2 = 1

*  Return.
      END
