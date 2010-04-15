      SUBROUTINE POINC0( NPSMP, NPROF, DATA, AXIS, BAND, PRFWID, PPROF,
     :                   STATUS )
*+
*  Name:
*     POINC0

*  Purpose:
*     Re-sample the input point source profile.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINC0( NPSMP, NPROF, DATA, AXIS, BAND, PRFWID, PPROF, STATUS )

*  Description:
*     This subroutine is used by POINTCRDD to re-sample an ideal
*     point source profile associated with the given waveband at the
*     same rate as the IRAS data scans. The non-zero segment of the
*     result is put into a temporary working array and the pointer to
*     its first element is returned.

*  Arguments:
*     NPSMP = INTEGER (Given)
*        Number of samples in the input ideal point source profile data
*        array.
*     NPROF = INTEGER (Given)
*        Number of profile in the input ideal point source profile data
*        array.
*     DATA( NPSMP, NPROF ) = REAL (Given)
*        The input ideal point source profile data array.
*     AXIS( NPSMP ) = REAL (Given)
*        The in-scan axis of the point source profile, in arc-min from
*        its centre.
*     BAND = INTEGER (Given)
*        Waveband number with which the associated point source profile
*        is to be re-sampled.
*     PRFWID = INTEGER (Returned)
*        The number of the samples in the non-zero segment of the
*        re-sampled point source profile associated with the given
*        waveband.
*     PPROF = INTEGER (Returned)
*        Pointer to the first element of a temporary working array which
*        contains the non-zero segment of the re-sampled point source
*        profile associated with the given waveband.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1993 (WG):
*        Original version.
*     6-OCT-1994 (DCP):
*        Incorporated as is in new version of POINTCRDD
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 package data

*  Arguments Given:
      INTEGER NPSMP, NPROF
      REAL DATA( NPSMP, NPROF )
      REAL AXIS( NPSMP )
      INTEGER BAND

*  Arguments Returned:
      INTEGER PRFWID
      INTEGER PPROF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BG, ED             ! Begin and end sample index of the
                                 ! non-zero segment of the profile
      INTEGER LINNO              ! Line number of input data array
                                 ! associated with the given waveband
      REAL TMDR                  ! Time duration of the point source

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If only one profile contained in the input profile data array, use
*  it for all waveband.
      IF ( NPROF .EQ. 1 ) THEN
         LINNO = 1

*  Or if 4 profiles contained in the input profile data array, use the
*  one corresponding to the given waveband.
      ELSE IF ( NPROF .EQ. 4 ) THEN
         LINNO = BAND
      END IF

*  Find the non-zero segment of the profile to be resampled.
      CALL POIND0( NPSMP, NPROF, DATA, LINNO, BG, ED, STATUS )

*  Find the time duration of the point souce profile when scanned by
*  IRAS.
      TMDR = ( AXIS( ED ) - AXIS( BG ) ) / I90__SPEED

*  It will have the number of samples if sampled at the same rate as
*  IRAS scans. To simplify the later calculation, oddise the number.
      PRFWID = 2 * NINT( 0.5 * REAL( I90__SRATE( BAND ) ) * TMDR ) + 1

*  If it contains too few samples after resampling, set status, report
*  and then exit.
      IF ( PRFWID .LT. 11 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POINC0_ERR1', 'POINC0: The supplied point '/
     :                /'source profile is not long enough in its '/
     :                /'in-scan distance.', STATUS )
         GOTO 999
      END IF

*  Create a temporary working array to contain the resampled non-zero
*  segment.
      CALL PSX_CALLOC( PRFWID, '_REAL', PPROF, STATUS )

*  Re-sample the non-zero segment of the profile and put the result into
*  the temporary working array.
      CALL POIND1( NPSMP, NPROF, DATA, AXIS, BAND, PRFWID,
     :             %VAL( PPROF ), STATUS )

 999  CONTINUE

      END
