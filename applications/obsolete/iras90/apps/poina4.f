      SUBROUTINE POINA4( PPROFL, BAND, PRFWID, PPROF,
     :                   SPSQ, SIP, SP, SISQ, SI, S1, V,
     :                   STATUS )
*+
*  Name:
*     POINA4

*  Purpose:
*     To obtain a suitably sampled point source profile for the band required
*     and to calculate the correlation coefficients associated with it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA4( PPROFL, BAND, PRFWID, PPROF, SPSQ, SIP, SP, SISQ, SI,
*                  S1, V, STATUS )

*  Description:
*     This subroutine is used by POINTCRDD to re-sample an ideal
*     point source profile associated with the given waveband at the
*     same rate as the IRAS data scans. The non-zero segment of the
*     result is put into a temporary working array and the pointer to
*     its first element is returned.  The subroutine also calculates the
*     correlation coefficients associated with this point source profile.

*  Arguments:
*     PPROFL = CHARACTER (Given)
*        The name of the parameter used for the name of the Point source
*        profiles NDF.
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
*     SPSQ = DOUBLE PRECISION (Returned)
*        Sum of squared profile samples.
*     SIP = DOUBLE PRECISION (Returned)
*        Sum of sample indices times  profile samples.
*     SP = DOUBLE PRECISION (Returned)
*        Sum of profile samples.
*     SISQ =DOUBLE PRECISION (Returned)
*        Sum of squared sample indices.
*     SI = DOUBLE PRECISION (Returned)
*        Sum of sample indices.
*     S1 =DOUBLE PRECISION (Returned)
*        Sum of constant 1.
*     V = DOUBLE PRECISION (Returned)
*        Determinant of the symmatric matrix formed by SPSQ, SIP, SP,
*        SISQ, SI, S1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DCP: Diana Parsons (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     6-OCT-1994 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 package data
      INCLUDE 'PRM_PAR'          ! Primitive constants eg VAL__SMLD

*  Arguments Given:
      CHARACTER*( * )PPROFL
      INTEGER BAND

*  Arguments Returned:
      INTEGER PRFWID
      INTEGER PPROF
      DOUBLE PRECISION SPSQ
      DOUBLE PRECISION SIP
      DOUBLE PRECISION SP
      DOUBLE PRECISION SISQ
      DOUBLE PRECISION SI
      DOUBLE PRECISION S1
      DOUBLE PRECISION V

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NPSMP              ! Number of samples in source profile
      INTEGER NPROF              ! Number of profiles in profile file.
      INTEGER PFNDF              ! NDF id of the profile NDF
      INTEGER PPROFD             ! Pointer to the profile data array
      INTEGER PPROFX             ! Pointer to the profile axis array
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the ideal point source profile from the environment.
      CALL IRM_PROFL( PPROFL, NPSMP, NPROF, PPROFD, PPROFX, PFNDF,
     :                 STATUS )

*  Re-sample the source profile at the rate of the IRAS scan and copy
*  copy the non-zero segment of the profile associated with the input
*  CRDD waveband to the working array..
      CALL POINC0( NPSMP, NPROF, %VAL( PPROFD ), %VAL( PPROFX ), BAND,
     :             PRFWID, PPROF, STATUS )

*  If error happened, exit.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate pre-determined constants to be used when correlating data
*  segment with the point source template.
      CALL POINC1( PRFWID, %VAL( PPROF ), SPSQ, SIP, SP, SISQ, SI, S1,
     :             V, STATUS )

*  If the calculated determinant is zero, the given point source profile
*  does not have proper values. Set status, report and exit.
      IF ( ABS( V ) .LE. VAL__SMLD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POINTCRDD_ERR1', 'The supplied ideal point '/
     :                /'source profile does not contain proper values',
     :                  STATUS )

      END IF


      END
