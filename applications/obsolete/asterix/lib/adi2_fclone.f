      SUBROUTINE ADI2_FCLONE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     ADI2_FCLONE

*  Purpose:
*     Clone a FITS file to a new file of the specified name

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FCLONE( NARG, ARGS, OARG, STATUS )

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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     18 Aug 1995 (DJA):
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
      INTEGER			NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*132		FNAME			! File name
      CHARACTER*80 		FPATH			! Sub-file path stuff

      INTEGER			BSIZE			! Block size
      INTEGER			CHDU			! I/p current HDU
      INTEGER			FPLEN			! FPATH length
      INTEGER			FSTAT			! FITSIO status
      INTEGER			HDU			! HDU number
      INTEGER			HDUTYP			! HDU type code
      INTEGER			IUNIT			! Loop over HDUs
      INTEGER			ILUN			! I/p logical unit
      INTEGER			LFILEC			! Last filename char
      INTEGER			OLUN			! O/p logical unit

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get logical unit of input file
      CALL ADI2_GETLUN( ARGS(1), ILUN, STATUS )

*  Get number of current HDU
      CALL FTGHDN( ILUN, CHDU )

*  Extract filename
      CALL ADI_GET0C( ARGS(2), FNAME, STATUS )

*  Parse the file specification into file, hdu and keyword names
      CALL ADI2_PARSE( FNAME, LFILEC, HDU, FPATH, FPLEN, STATUS )

*  Grab logical unit from system
      CALL FIO_GUNIT( OLUN, STATUS )

*  Rename any old file if necessary
      CALL ADI2_EXIREN( FNAME(:LFILEC), STATUS )

*  Try to create file
      BSIZE = 1
      FSTAT = 0
      CALL FTINIT( OLUN, FNAME(:LFILEC), BSIZE, FSTAT )

*  Created ok?
      IF ( FSTAT .EQ. 0 ) THEN

*    Create new instance of a FITSfile object
        CALL ADI_NEW0( 'FITSfile', OARG, STATUS )

*    Copy HDUs from input to new output file
        IUNIT = 1
        DO WHILE ( FSTAT .EQ. 0 )
          CALL FTMAHD( ILUN, IUNIT, HDUTYP, FSTAT )
          IF ( FSTAT .EQ. 0 ) THEN
            IF ( IUNIT .GT. 1 ) THEN
              CALL FTCRHD( OLUN, FSTAT )
              CALL FTMAHD( OLUN, IUNIT, HDUTYP, FSTAT )
            END IF
            CALL FTCOPY( ILUN, OLUN, 0, FSTAT )
          END IF
          IUNIT = IUNIT + 1
        END DO

*    Close and re-open for update
        FSTAT = 0
        CALL FTCLOS( OLUN, FSTAT )
        CALL FTOPEN( OLUN, FNAME(:LFILEC), 1, BSIZE, FSTAT )

*    Inherit user specified HDU number and sub-file path from input
        CALL ADI_CGET0I( ARGS(1), 'UserHDU', HDU, STATUS )
        CALL ADI_CPUT0I( OARG, 'UserHDU', HDU, STATUS )
        CALL ADI_THERE( ARGS(1), 'Fpath', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET0C( ARGS(1), 'Fpath', FPATH, STATUS )
          CALL ADI_CPUT0C( ARGS(1), 'Fpath', FPATH(:CHR_LEN(FPATH)),
     :                     STATUS )
        END IF

*    Initialise HDU cursor
        CALL ADI_CPUT0I( OARG, '.CurHdu', -1, STATUS )

*    Write extra info into the file handle object
        CALL ADI_CPUT0I( OARG, 'Lun', OLUN, STATUS )
        CALL ADI_CPUT0I( OARG, 'BlockSize', BSIZE, STATUS )

      ELSE

*    Return unit to system
        CALL FIO_PUNIT( OLUN, STATUS )

*    Report an error
        CALL ADI2_FITERP( FSTAT, STATUS )

      END IF

*  Move input back to original position
      FSTAT = 0
      CALL FTMAHD( ILUN, CHDU, HDUTYP, FSTAT )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FCLONE', STATUS )

      END
