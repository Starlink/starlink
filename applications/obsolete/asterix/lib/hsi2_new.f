      SUBROUTINE HSI2_NEW( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI2_NEW

*  Purpose:
*     Create new history in an FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI2_NEW( NARG, ARGS, OARG, STATUS )

*  Description:
*     Creates the HISTORY structure in an FITS file to another. Existing
*     structure is deleted if present.

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
*     Creates a PSF_SLOT property on the property list of the first
*     argument if one is not already present.

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Deficiencies:
*     The constants in this routine should be loadable resources. This
*     would enable the user to turn off all history creation (somehow).

*  References:
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
*     10 Feb 1995 (RB):
*        Converted to new linked structure format.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'ADI_PAR'					! ADI constants
      INCLUDE 'DAT_PAR'					! HDS constants

*  Arguments Given:
      INTEGER			NARG			! # arguments
      INTEGER			ARGS(*)			! Method arguments

*  Arguments Returned:
      INTEGER			OARG			! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*8		UMODE
        PARAMETER		( UMODE = 'NORMAL' )
      INTEGER			ESIZE
        PARAMETER		( ESIZE = 10 )

*  Local Variables:
      CHARACTER*18		TSTR			! Time string
      CHARACTER*80		CMNT			! FITS keyword comment
      CHARACTER*8		KEYWORD			! FITS keyword name

      INTEGER			HHDU			! HISTORY HDU id
      INTEGER			PHDU			! Primary HDU id
      INTEGER			NCHILD			! Number of HDU children
      INTEGER			NDIG			! Number of digits

      LOGICAL			DIDCRE			! Extension created?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No output from this method
      OARG = ADI__NULLID

*  History exists?
*  Delete existing history in output
      CALL ADI2_CFIND( ARGS(1), 'HISTORY', ' ', ' ', .FALSE., .TRUE.,
     :                 ' ', 0, 0, DIDCRE, HHDU, STATUS )
      IF ( HHDU .NE. ADI__NULLID ) THEN
        CALL MSG_PRNT( 'Erasing existing history structure...' )
      END IF

*  Create and locate new structure
      CALL ADI2_CFIND( ARGS(1), 'HISTORY', ' ', ' ', .TRUE., .FALSE.,
     :                 ' ', 0, 0, DIDCRE, HHDU, STATUS )

*  Get time string
      CALL TCI_CTIME( TSTR, STATUS )

*  Fill values of these objects
      CALL ADI2_HPKYC( HHDU, 'EXTNAME', 'HISTORY',
     :                 'Contains ASTERIX HISTORY structure', STATUS )
      CALL ADI2_HPKYC( HHDU, 'TYPE', 'HISTORY',
     :                 'Data type of this extension', STATUS )
      CALL ADI2_HPKYC( HHDU, 'CREATED', TSTR,
     :                 'Date & time of HISTORY creation', STATUS )
      CALL ADI2_HPKYC( HHDU, 'UPDATE', UMODE,
     :                 'Update mode for further additions', STATUS )
      CALL ADI2_HPKYC( HHDU, 'PARENT', 'PRIMARY',
     :                 'Parent HDU of this extension', STATUS )
      CALL ADI2_HPKYI( HHDU, 'CHILDREN', 0,
     :                 'Number of children for this HDU', STATUS )

*  Link into the top level
      CALL ADI2_CFIND( ARGS(1), ' ', ' ', ' ', .FALSE., .FALSE.,
     :                 ' ', 0, 0, DIDCRE, PHDU, STATUS )
      CALL ADI2_HGKYI( PHDU, 'CHILDREN', NCHILD, CMNT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        NCHILD = 1
      ELSE
        NCHILD = NCHILD + 1
      END IF
      CALL ADI2_HPKYI( PHDU, 'CHILDREN', NCHILD,
     :                 'Number of children for this HDU', STATUS )
      CALL CHR_ITOC( NCHILD, KEYWORD, NDIG )
      KEYWORD = 'CHILD' // KEYWORD(:NDIG)
      CALL ADI2_HPKYC( PHDU, KEYWORD, 'HISTORY',
     :                 'Name of child extension', STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'HSI2_NEW', STATUS )
      END IF

      END
