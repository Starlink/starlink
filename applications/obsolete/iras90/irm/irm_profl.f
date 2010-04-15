      SUBROUTINE IRM_PROFL( PFILE, NSMP, NPROF, DPNTR, XPNTR, NDF,
     :                      STATUS )
*+
*  Name:
*     IRM_PROFL

*  Purpose:
*     Input a point source profile from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_PROFL( PFILE, NSMP, NPROF, DPNTR, XPNTR, NDF, STATUS )

*  Description:
*     This subroutine is used to input a point source profile from the
*     environment. See the help on application MAKEPORF for a
*     description of the required format on the input NDF. The returned
*     pointer DPNTR points to the first element of the mapped data
*     array and XPNTR points to the mapped AXIS CENTRE array of the
*     mapped data array.

*  Arguments:
*     PFILE = CHARACTER (Given)
*        The name of the parameter used to get the NDF file which
*        contains the point source profile.
*     NSMP = INTEGER (Returned)
*        Number of samples in the obtained point source profile.
*     NPROF = INTEGER (Returned)
*        The number of profile contained in the input NDF. It must be
*        either 4 (one for each waveband) or 1 (one for all wavebands).
*     DPNTR = INTEGER (Returned)
*        The pointer to the first element of the mapped data array.
*     XPNTR = INTEGER (Returned)
*        The pointer to the mapped AXIS CENTER array for the in-scan
*        axis.
*     NDF = INTEGER (Returned)
*        The ID of the point source profile NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*( * ) PFILE

*  Arguments Returned:
      INTEGER NSMP
      INTEGER NPROF
      INTEGER DPNTR
      INTEGER XPNTR
      INTEGER NDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*( 15 ) A1UNIT    ! Units of 1st axis centre avlues
      INTEGER DIM( 2 )           ! Size of the input NDF
      INTEGER EL                 ! Number of element in a mapped array
      INTEGER NDIM               ! Number of dimensions of input NDF
      LOGICAL THERE              ! A component existing flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a NDF file from the user.
      CALL NDF_ASSOC( PFILE, 'READ', NDF, STATUS )

*  If unable open a NDF file, exit.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get the shape of the input NDF.
      CALL NDF_DIM( NDF, 2, DIM, NDIM, STATUS )

*  If the size of the second dimension is not 1 or 4, set the status,
*  report and then exit.
      IF ( DIM( 2 ) .NE. 1 .AND. DIM( 2 ) .NE. 4 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_PROFL_ERR1', 'IRM_PROFL: Input NDF has '/
     :                /'incorrect dimensions.' , STATUS )
         GOTO 999
      END IF

*  Check that the AXIS CENTRE array exists for the first dimension.
      CALL NDF_ASTAT( NDF, 'CENTRE', 1, THERE, STATUS )
      IF ( .NOT.THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_PROFL_ERR2', 'IRM_PROF: Input NDF has no '/
     :                /'AXIS CENTRE values for its first dimension.',
     :                  STATUS )
         GOTO 999
      END IF

*  Check that the AXIS UNITS exists and the AXIS CENTRE values are in
*  arc-mins.
      CALL NDF_ASTAT( NDF, 'UNITS', 1, THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_ACGET( NDF, 'UNITS', 1, A1UNIT, STATUS )
         CALL CHR_UCASE( A1UNIT )
         CALL CHR_LDBLK( A1UNIT )
      END IF

*  If either no AXIS UNITS exits or the units is not in arc-min, set
*  status, report and exit.
      IF ( .NOT.THERE .OR. A1UNIT( : 7 ) .NE. 'ARC-MIN' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_PROFL_ERR3', 'IRM_PROFL: the axis of the '/
     :                /'input NDF either does not have an AXIS UNITS '/
     :                /'for its 1st dimension or the units is not '/
     :                /'in arc-min.', STATUS )
         GOTO 999
      END IF

*  Map the data array for reading.
      CALL NDF_MAP( NDF, 'Data', '_REAL', 'READ', DPNTR, EL, STATUS )

*  Map the axis centre array for reading.
      CALL NDF_AMAP( NDF, 'Centre', 1, '_REAL', 'READ', XPNTR, EL,
     :               STATUS )

*  Returne the number of samples and number of profiles contained in
*  in the input NDF.
      NSMP = DIM( 1 )
      NPROF = DIM( 2 )

 999  CONTINUE

      END
