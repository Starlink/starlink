      SUBROUTINE ADI1_FCREAT( FILE, ID, FID, STATUS )
*+
*  Name:
*     ADI1_FCREAT

*  Purpose:
*     Create a new HDS file, or HDS file component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_FCREAT( FILE, ID, FID, STATUS )

*  Description:
*     Create a new HDS file.

*  Arguments:
*     FILE = INTEGER (given)
*        ADI identifier of string holding name of file to create
*     ID = INTEGER (given)
*        ADI identifier of object to link to the new file
*     FID = INTEGER (returned)
*        The identifier of the HDSfile object created
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private, FITS

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     1 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER			FILE			! File name to open
      INTEGER			ID			! Template object

*  Arguments Returned:
      INTEGER			FID			! New file object

*  Status:
      INTEGER 			STATUS                  ! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	FLOC			! File locator
      CHARACTER*(DAT__SZLOC)	LOC			! New object locator
      CHARACTER*(DAT__SZLOC)	SFLOC			! File object locator
      CHARACTER*132		FNAME			! File name

      INTEGER			DIMS(DAT__MXDIM)	! Create object dims
      INTEGER			FNCH			! 1st char of filename
      INTEGER			FSUBC, LSUBC		! Sub-struc char pos's
      INTEGER			LFILEC			! Last filename char
      INTEGER			NDIM			! Dimensionality
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract filename
      CALL ADI_GET0C( FILE, FNAME, STATUS )

*  Parse the file specification into file name and sub-structure
      CALL ADI1_PARSE( FNAME, LFILEC, FSUBC, LSUBC, STATUS )

*  Choose the object type. If we have no input object defined create an
*  anonymous structure
      IF ( ID .EQ. ADI__NULLID ) THEN
        HTYPE = 'UNKNOWN'
        NDIM = 0
      ELSE

*      Get object class
        CALL ADI_CLASS( ID, ICLASS, STATUS )

        NDIM = 0

      END IF

*  If the user supplied sub-structure then assume that the file already
*  exists, and that we are required to create a new object with specified
*  full structure spec. Otherwise, we create a new file.
      IF ( FSUBC .GT. 0 ) THEN

*    Try to open file - successful?
        CALL HDS_OPEN( FNAME(:LFILEC), 'UPDATE', FLOC, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN

*      The user has supplied the name of the new object as the last item
*      in the sub-structure specification...
          FNCH = LSUBC
          DO WHILE ( (FNCH.GE.FSUBC) .AND. (FNAME(FNCH:FNCH).NE.'.') )
            FNCH = FNCH - 1
          END DO
          FNCH = FNCH + 1

*      If FNCH equals FSUBC then only one level of substructure has been
*      specified in which case we have nothing to find. If FNCH > FSUBC
*      then locate the substructure.
          IF ( FNCH .GT. FSUBC ) THEN
            CALL ADI1_FIND( FLOC, FNAME(FSUBC:FNCH-1), SLOC, STATUS )
            CALL DAT_PRMRY( .TRUE., SLOC, .TRUE., STATUS )
            CALL DAT_ANNUL( FLOC, STATUS )
            FLOC = SLOC
          END IF

*      Create the new object
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_NEW( FLOC, FNAME(FNCH:LSUBC), HTYPE, NDIM,
     :                    DIMS, STATUS )

*        If successful promote the derived locator to that the file will be
*        closed when LOC is annulled
            IF ( STATUS .EQ. SAI__OK ) THEN
              CALL DAT_FIND( FLOC, FNAME(FNCH:LSUBC), LOC, STATUS )
              CALL DAT_PRMRY( .TRUE., LOC, .TRUE., STATUS )
              CALL DAT_ANNUL( FLOC, STATUS )
            END IF

          END IF

        END IF

      ELSE

*    Use the filename as the top level object name
        FNCH = LFILEC
        DO WHILE ( (FNCH.GT.0) .AND. (FNAME(FNCH:FNCH).NE.'/') )
          FNCH = FNCH - 1
        END DO
        FNCH = FNCH + 1

*    Create the new file
        CALL HDS_NEW( FNAME(:LFILEC), FNAME(FNCH:LFILEC), HTYPE,
     :                NDIM, DIMS, LOC, STATUS )

      END IF

*  Created ok?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Create new instance of a FITSfile object
        CALL ADI_NEW0( 'HDSfile', FID, STATUS )

*    Write the locator
        CALL ADI_CPUT0C( FID, '.Locator', LOC, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_FCREAT', STATUS )

      END
