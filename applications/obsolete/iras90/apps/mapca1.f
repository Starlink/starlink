      SUBROUTINE MAPCA1( IDC, INDF, CRDDF, IGRP1, IGRP2, IDA, NINCL,
     :                   INCLUD, LPCBND, UPCBND, ALLVAR, ANYVAR,
     :                   STATUS )
*+
*  Name:
*     MAPCA1

*  Purpose:
*     Extend image bounds to include data within a CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCA1( IDC, INDF, CRDDF, IGRP1, IGRP2, IDA, NINCL, INCLUD,
*                  LPCBND, UPCBND, ALLVAR, ANYVAR, STATUS )

*  Description:
*     The data from selected detectors within the CRDD file are
*     positioned within the image frame (pixel coordinates). If the
*     current image bounds do not include the data, then they are
*     extended so that they do. Note, the half-coverage areas at the
*     end of each scan are not included.
*
*     If the CRDD file has associated variance values then the flags
*     ALLVAR and ANYVAR are updated accordingly.
*
*     Finally, the CRDD file is added to the end of the NDF group
*     specified by IGRP2.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     INDF = INTEGER (Given)
*        An NDF identifier for the CRDD file.
*     CRDDF = INTEGER (Given)
*        The index of the CRDD file within the group identified by
*        IGRP1.
*     IGRP1 = INTEGER (Given)
*        The GRP identifier for the group containing the CRDD files.
*     IGRP2 = INTEGER (Given)
*        A GRP identifier specifying a group to which the CRDD file
*        is to be appended.
*     IDA = INTEGER (Given)
*        An IRA identifier specifying the mapping from sky to image
*        coordinates.
*     NINCL = INTEGER (Given)
*        The number of detector numbers stored in INCLUD.
*     INCLUD( NINCL ) = INTEGER (Given)
*        A list of detector numbers. Only the data from these detectors
*        is included in the image area.
*     LPCBND( 2 ) = DOUBLE PRECISION (Given and Returned)
*        The lower limits of the two pixel coordinate axes. On exit,
*        the image area is increased if necessary to ensure that the
*        required CRDD data lies within the image area.
*     UPCBND( 2 ) = DOUBLE PRECISION (Given and Returned)
*        The upper limits of the two pixel coordinate axes.
*     ALLVAR = LOGICAL (Given and Returned)
*        Returned .FALSE. if the CRDD file does not have a defined
*        VARIANCE component. Otherwise, it retains its value on entry.
*     ANYVAR = LOGICAL (Given and Returned)
*        Returned .TRUE. if the CRDD file does have a defined VARIANCE
*        component. Otherwise, it retains its value on entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-NOV-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      INTEGER IDC
      INTEGER INDF
      INTEGER CRDDF
      INTEGER IGRP1
      INTEGER IGRP2
      INTEGER IDA
      INTEGER NINCL
      INTEGER INCLUD( NINCL )

*  Arguments Given and Returned:
      DOUBLE PRECISION LPCBND( 2 )
      DOUBLE PRECISION UPCBND( 2 )
      LOGICAL ALLVAR
      LOGICAL ANYVAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DETIND( I90__MAXDT )!The list of detector indices.
      CHARACTER NAME*(GRP__SZNAM)! The name of the CRDD file.
      INTEGER NDETS              ! No. of detectors which can be
                                 ! included.
      LOGICAL THERE              ! True if the CRDD file has a defined
                                 ! VARIANCE component.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert each detector number which is to be included in the map, to
*  the corresponding detector index (if it exists in the CRDD file).
      CALL IRC_DINDS( IDC, NINCL, INCLUD, DETIND, NDETS, STATUS )

*  Find the pixel limits needed to encompass all the required data from
*  this CRDD file (excluding the half coverage areas at each end of the
*  scan). This uses the inverse mapping calculated for the first CRDD
*  file.
      CALL IRC_LIMIT( IDC, NDETS, DETIND, .TRUE., IDA, LPCBND, UPCBND,
     :                STATUS )

*  See if the NDF has a defined VARIANCE component. If not, set a flag
*  to indicate that not all input CRDD files have VARIANCE components.
      CALL NDF_STATE( INDF, 'VARIANCE', THERE, STATUS )
      ALLVAR = ALLVAR .AND. THERE

*  Set a flag to indicate if any input CRDD files have defined VARIANCE
*  components.
      IF( THERE ) ANYVAR = .TRUE.

*  Append the NDF name to the end of the group identified by IGRP2.
      CALL GRP_GET( IGRP1, CRDDF, 1, NAME, STATUS )
      CALL GRP_PUT( IGRP2, 1, NAME, 0, STATUS )

      END
