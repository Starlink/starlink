      SUBROUTINE SPD_WZKK( ROWNUM, ROWLEN, NCOMP1, ELMNOS, DATA, VARS,
     :   NCOMP2, CENTRS, PEAKS, CENVAR, STATUS )
*+
*  Name:
*     SPD_WZKK

*  Purpose:
*     Sort centres and peaks for ARCIDENT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZKK( ROWNUM, ROWLEN, NCOMP1, ELMNOS, DATA, VARS,
*        NCOMP2, CENTRS, PEAKS, CENVAR, STATUS )

*  Description:
*     This routine does some per-row preparation for ARCIDENT. It picks
*     the centres of line components from the appropriate row of the
*     data array and stores them in an array of their own. The same is
*     done for each following element in the data array, assuming that
*     that is the corresponding peak value. The peaks are stored in an
*     array separate from the centres.
*
*     This routine also picks the centre variances from the second given
*     array, but not the peak variances.
*
*     Data are picked only if the centre is non-bad, and the peak is
*     non-bad, and the centre variance is non-bad and positive.

*  Arguments:
*     ROWNUM = INTEGER (Given)
*        The number of the row to use in the results data array.
*     ROWLEN = INTEGER (Given)
*        The length of the rows in the results data array.
*     NCOMP1 = INTEGER (Given)
*        The number of components to pick from the results data array.
*        Some may turn out to have bad values as parameters. Which is
*        why NCOMP2 may be smaller than NCOMP1
*     ELMNOS( NCOMP1 ) = INTEGER (Given)
*        DATA(ELMNOS(I),ROWNUM) is the centre of the I-th line
*        component.
*     DATA( ROWLEN, ROWNUM ) = REAL (Given)
*        The results data array.
*     VARS( ROWLEN, ROWNUM ) = REAL (Given)
*        The results variance array.
*     NCOMP2 = INTEGER (Returned)
*        The number of components with non-bad parameters. Only these
*        are picked from DATA into CENTRS and PEAKS. The returned value
*        can legitimately be zero.
*     CENTRS( NCOMP1 ) = REAL (Returned)
*        The array of picked line centres. Only elements 1 to NCOMP2 are
*        significant. The rest is filled with bad values.
*     PEAKS( NCOMP1 ) = REAL (Returned)
*        The array of picked line peaks. Only elements 1 to NCOMP2 are
*        significant. The rest is filled with bad values.
*     CENVAR( NCOMP1 ) = REAL (Returned)
*        The array of picked line centre variances. Only elements 1 to
*        NCOMP2 are significant. The rest is filled with bad values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine assumes that ELMNOS(I)+1 never exceeds TNPAR.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     01 Jun 1993 (hme):
*        Original version.
*     25 Jan 1995 (hme):
*        Renamed from SPADR.
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
      INTEGER ROWNUM
      INTEGER ROWLEN
      INTEGER NCOMP1
      INTEGER ELMNOS( NCOMP1 )
      REAL DATA( ROWLEN, ROWNUM )
      REAL VARS( ROWLEN, ROWNUM )

*  Arguments Returned:
      INTEGER NCOMP2
      REAL CENTRS( NCOMP1 )
      REAL PEAKS( NCOMP1 )
      REAL CENVAR( NCOMP1 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      REAL CENT                  ! I-th centre
      REAL PEAK                  ! I-th peak
      REAL CVAR                  ! I-th centre variance

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset the output arrays.
      DO 1002 I = 1, NCOMP1
         CENTRS(I) = VAL__BADR
         PEAKS(I)  = VAL__BADR
         CENVAR(I) = VAL__BADR
 1002 CONTINUE

*  Loop through the components identified as line components.
*  If centre and peak are good, copy them to the next element of the
*  output arrays.
      NCOMP2 = 0
      DO 1001 I = 1, NCOMP1
         CENT = DATA( ELMNOS(I),   ROWNUM )
         PEAK = DATA( ELMNOS(I)+1, ROWNUM )
         CVAR = VARS( ELMNOS(I),   ROWNUM )
         IF ( CENT .NE. VAL__BADR .AND. PEAK .NE. VAL__BADR .AND.
     :        CVAR .NE. VAL__BADR .AND. CVAR .GT. 0. ) THEN
            NCOMP2 = NCOMP2 + 1
            CENTRS(NCOMP2) = CENT
            PEAKS(NCOMP2)  = PEAK
            CENVAR(NCOMP2) = CVAR
         END IF
 1001 CONTINUE

      END
