      SUBROUTINE GCB2_SAVE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     GCB2_SAVE

*  Purpose:
*     Saves Grafix Control Block to FITS file object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB2_SAVE( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Jul 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'                                 ! GCB globals
*        G_MEMPTR = INTEGER (given)
*           Active GCB data area

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			GCBPTR
      INTEGER			NBYTE
      INTEGER			NSCAL
      INTEGER			NSTRUC
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find size required
      CALL GCB_CSIZE( NBYTE, NSCAL, NSTRUC, STATUS )

*  Create workspace
      CALL DYN_MAPB( 1, NBYTE, GCBPTR, STATUS )

*  Copy semi-compressed GCB to workspace
      CALL GCB_SAVE_SUB( NSCAL, NSTRUC, %VAL(G_MEMPTR), %VAL(GCBPTR),
     :                                                       STATUS )

*  Save the GCB
      CALL GCB2_SAVE_INT( ARGS(1), NBYTE, GCBPTR, STATUS )

*  Free the workspace
      CALL DYN_UNMAP( GCBPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB2_SAVE', STATUS )

      END



      SUBROUTINE GCB2_SAVE_INT( FID, NBYTE, GCBPTR, STATUS )
*+
*  Name:
*     GCB2_SAVE_INT

*  Purpose:
*     Saves mapped compressedf GCB to FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB2_SAVE_INT( FID, NBYTE, GCBPTR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     NBYTE = INTEGER (given)
*        Number of bytes to save
*     GCBPTR = INTEGER (given)
*        Address of compressed GCB data
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Jul 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'                                 ! GCB globals
*        G_VERSION = REAL (given)
*           GCB version number

*  Arguments Given:
      INTEGER                   FID, NBYTE, GCBPTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			GCBDAT			! GCB hdu data id
      INTEGER			GCBHDU			! GCB hdu id

      LOGICAL			DIDCRE			! Did we create the GCB?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate GCB extension data
      CALL ADI2_CFIND( FID, 'GCB', '@', ' ', .TRUE., .FALSE.,
     :                 'BYTE', 1, NBYTE, DIDCRE, GCBDAT, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Write data to the cache object
        CALL ADI_CPUT( GCBDAT, 'Value', 'BYTE', 1, NBYTE,
     :                 %VAL(GCBPTR), STATUS )

*    Release the cache
        CALL ADI_ERASE( GCBDAT, STATUS )

*    Write some keywords if we created the GCB
        IF ( DIDCRE ) THEN

*      Locate the HDU
          CALL ADI2_FNDHDU( FID, 'GCB', .FALSE., GCBHDU, STATUS )

*      Write GCB keywords
          CALL ADI2_HPKYC( GCBHDU, 'CONTENT', 'GRAFIX CONTROL',
     :                      'Contents of extension', STATUS )
          CALL ADI2_HPKYR( GCBHDU, 'GCBVERSN', G_VERSION,
     :                      'Version of GCB description', STATUS )
          CALL ADI2_HPKYC( GCBHDU, 'EXTNAME', 'GCB',
     :              'Extension contains ASTERIX graphics attributes',
     :                    STATUS )

*      Release the GCB hdu
          CALL ADI_ERASE( GCBHDU, STATUS )

        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'GCB2_SAVE_INT', STATUS )
      END IF

      END
