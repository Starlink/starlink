      SUBROUTINE SPD_WZGD( MAXBBS, INELM, NCOMP,
     :   THETA, ALPHA, TEMPE, INX, FITDAT, STATUS )
*+
*  Name:
*     SPD_WZGD

*  Purpose:
*     Calculate FITBB fit data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZGD( MAXBBS, INELM, NCOMP,
*        THETA, ALPHA, TEMPE, INX, FITDAT, STATUS )

*  Description:
*     This routine calculates the fit data array for FITBB. This is
*     a sum of several diluted Planck components.
*
*     Since the components to be added are known only as their common
*     logarithms, summing up is in practice:
*
*        log( A + B ) = max{ log A; log B }
*                     + log[ 1 + 10^( -| log A - log B | ) ]
*
*     The profile functions are diluted Planck curves
*
*        I_nu = 10^theta * nu^alpha  * B_nu( T, nu )
*
*     or rather the logarithms thereof.

*  Arguments:
*     MAXBBS = INTEGER (Given)
*        Size of arrays for components.
*     INELM = INTEGER (Given)
*        Size of arrays INX and FITDAT.
*     NCOMP = INTEGER (Given)
*        The value of NCOMP is the number of components. NCOMP must be
*        greater than or equal to 0 and less than or equal to MAXBBS.
*     THETA( MAXBBS ) = REAL (Given)
*        Scaling constant for each component.
*     ALPHA( MAXBBS ) = REAL (Given)
*        Emissivity exponent for each component.
*     TEMPE( MAXBBS ) = REAL (Given)
*        Colour temperature for each component.
*     INX( INELM ) = REAL (Given)
*        Given log10(frequency/Hz) values.
*     FITDAT( INELM ) = REAL (Returned)
*        Date array with fit values log10(intensity).
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set if the number of components is
*        out of range.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     timj: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26 Jan 1993 (hme):
*        Adapted from SPAAV.
*     27 Jan 1995 (hme):
*        Renamed from SPACR.
*     15 Aug 2005 (timj):
*        ** -LOGDIFF is not standards compliant. Use parentheses
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      INTEGER MAXBBS
      INTEGER INELM
      INTEGER NCOMP
      REAL THETA( MAXBBS )
      REAL ALPHA( MAXBBS )
      REAL TEMPE( MAXBBS )
      REAL INX( INELM )

*  Arguments Returned:
      REAL FITDAT( INELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, C               ! Loop indices
      REAL FITVAL                ! Single component fit value
      REAL LOGDIF                ! Logarithm of component ratio

*  Internal declarations:
      DOUBLE PRECISION SPD_UAAQD ! Planck function value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check number of components.
      IF ( NCOMP .LT. 0 .OR. NCOMP .GT. MAXBBS ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_WZGD_E01',
     :      'SPD_WZGD: Error evaluating diluted Planck profiles: ' //
     :      'Number of components is out of range.', STATUS )
         GO TO 500
      END IF

*  Work out the first component.
      DO 1 I = 1, INELM
         FITDAT(I) = THETA(1) + ALPHA(1) * INX(I)
     :             + SNGL( SPD_UAAQD( DBLE(TEMPE(1)), DBLE(INX(I)) ) )
 1    CONTINUE

*  Add one component after the other.
*  Note that the common logarithms of the addends are to be used and the
*  logarithm of the sum to be worked out. When the addends' ratio is
*  10**8 or more, the smaller addend is neglected.
      IF ( NCOMP .GT. 1 ) THEN
         DO 3 C = 2, NCOMP
            DO 2 I = 1, INELM
               FITVAL = THETA(C) + ALPHA(C) * INX(I)
     :                + SNGL( SPD_UAAQD(DBLE(TEMPE(C)),DBLE(INX(I))) )
               LOGDIF = ABS( FITDAT(I) - FITVAL )
               FITDAT(I) = MAX( FITDAT(I),  FITVAL )
               IF ( LOGDIF .LT. 8. )
     :            FITDAT(I) = FITDAT(I) + LOG10( 1. + 10. ** (-LOGDIF) )
 2          CONTINUE
 3       CONTINUE
      END IF

*  Return.
 500  CONTINUE
      END
