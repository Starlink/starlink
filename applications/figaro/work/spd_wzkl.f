      SUBROUTINE SPD_WZKL( NLOCAT, NIDENT, ROWNUM, ROWLEN,
     :   CENTRS, CENVAR, LOCATS, IDENTS, DATA, VARS, STATUS )
*+
*  Name:
*     SPD_WZKL

*  Purpose:
*     Sort identified features into results for ARCIDENT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZKL( NLOCAT, NIDENT, ROWNUM, ROWLEN, CENTRS, CENVAR,
*        LOCATS, IDENTS, DATA, VARS, STATUS )

*  Description:
*     This routine does some post-processing for ARCIDENT. Once Dave
*     Mills' algorithm has identified a number of features, their
*     observed locations and laboratory values must be merged into a
*     single results data array.
*
*     All located features get their place in the output. Those that
*     have been identified get their ID, others get the bad value as
*     ID. All laboratory value variances (ID variances) are zero.
*     All locations (centres) and their variances are copied
*     from input.

*  Arguments:
*     NLOCAT = INTEGER (Given)
*        The number of located features, these can be more than the
*        identified features.
*     NIDENT = INTEGER (Given)
*        The number of identified features.
*     ROWNUM = INTEGER (Given)
*        The row number in the output arrays, the counter of spectra.
*     ROWLEN = INTEGER (Given)
*        The length of each row in the result data array. This must be
*        at least twice NIDENT.
*     CENTRS( NLOCAT ) = REAL (Given)
*        The locations (centres) of the located features. In general
*        there are more located than identified features. CENTRS must be
*        strictly monotonically increasing.
*     CENVAR( NLOCAT ) = REAL (Given)
*        The variances corresponding to CENTRS.
*     LOCATS( NIDENT ) = REAL (Given)
*        The locations (centres) of the identified features. In general
*        there are more located than identified features. LOCATS must be
*        strictly monotonically increasing.
*     IDENTS( NIDENT ) = REAL (Given)
*        The identifications (laboratory values) corresponding to
*        LOCATS.
*     DATA( ROWLEN, ROWNUM ) = REAL (Given and Returned)
*        The output results data array. Only part of the ROWNUM-th row
*        is set, the rest is left unchanged. That row should have been
*        initialised with bad values before this routine is called.
*     VARS( ROWLEN, ROWNUM ) = REAL (Returned)
*        The output results variance array. Only part of the ROWNUM-th
*        row is set, the rest is left unchanged. That row should have
*        been initialised with bad values before this routine is called.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This may be set and an error reported if
*        NLOCAT .LT. NIDENT or ROWLEN .LT. NLOCAT.

*  Notes:
*     This routine assumes that the locations are strictly monotonically
*     increasing and that all identified features are included in the
*     located features.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     04 Jun 1993 (hme):
*        Original version.
*     11 Jun 1993 (hme):
*        Retain all located features, set non-identifications to bad
*        value.
*     25 Jan 1995 (hme):
*        Renamed from SPADS.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      INTEGER NLOCAT
      INTEGER NIDENT
      INTEGER ROWNUM
      INTEGER ROWLEN
      REAL CENTRS( NLOCAT )
      REAL CENVAR( NLOCAT )
      REAL LOCATS( NIDENT )
      REAL IDENTS( NIDENT )

*  Arguments Returned:
      REAL DATA( ROWLEN, ROWNUM )
      REAL VARS( ROWLEN, ROWNUM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Loop indices

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that there are at least as many locations as there are
*  identifications.
*  Check that the row length is at least twice the number of locations.
      IF ( NLOCAT .LT. NIDENT ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E16', 'ARCIDENT: Error: Fewer ' //
     :      'located than identified features.', STATUS )
         RETURN
      ELSE IF ( ROWLEN .LT. 2 * NLOCAT ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ARCIDENT_E17', 'ARCIDENT: Error: More ' //
     :      'located features than can be accommodated.', STATUS )
         RETURN
      END IF

*  Loop through the located features (counter is J).
*  I counts identified features.
      I = 1
      DO 1001 J = 1, NLOCAT

*     Copy location and its variance.
         DATA( 2*J-1, ROWNUM ) = CENTRS(J)
         VARS( 2*J-1, ROWNUM ) = CENVAR(J)

*     Set identification variance.
         VARS( 2*J, ROWNUM ) = 0.

*     Copy identification, if available.
         IF ( CENTRS(J) .EQ. LOCATS(I) ) THEN
            DATA( 2*J, ROWNUM ) = IDENTS(I)
            I = I + 1
         ELSE
            DATA( 2*J, ROWNUM ) = VAL__BADR
         END IF
 1001 CONTINUE

      END
