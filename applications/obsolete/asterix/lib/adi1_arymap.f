      SUBROUTINE ADI1_ARYMAP( LOC, TYPE, MODE, MLOC, PTR, NELM, STATUS )
*+
*  Name:
*     ADI1_ARYMAP

*  Purpose:
*     Map a primitive array or ARRAY structure object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_ARYMAP( LOC, TYPE, MODE, MLOC, PTR, NELM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator to array
*     TYPE = CHARACTER*(*) (given)
*        The type to map with
*     MODE = CHARACTER*(*) (given)
*        The access mode, READ, UPDATE or WRITE
*     MLOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator to mapped object, if any
*     PTR = INTEGER (returned)
*        The mapped data address
*     NELM = INTEGER (returned)
*        Number of mapped data elements
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
*     Only handles 1 dimensional spaced arrays. Seeing as ASTERIX was the
*     only package to use spaced arrays, and we've never written them
*     other than for axis arrays, we're pretty safe here.

*  References:
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Aug 1995 (DJA):
*        Original version.
*     13 Aug 1997 (RB):
*        Check that the VARIANT component exists.
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
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*(*)		TYPE, MODE

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC)	MLOC
      INTEGER			PTR, NELM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ACLOC			! ARRAY component
      CHARACTER*(DAT__SZTYP)	ATYPE			! Actual array type
      CHARACTER*(DAT__SZTYP)	HTYPE			! HDS style type name
      CHARACTER*10		VARNT			! Array variant name

      DOUBLE PRECISION		BASE, SCALE		! Spaced array descrip

      INTEGER			DIMS(DAT__MXDIM)	! Array dimensions
      INTEGER			NDIM			! Array dimensionality

      LOGICAL			PRIM			! Object is primitive?
      LOGICAL			THERE			! Component exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defaults
      MLOC = DAT__NOLOC

*  Construct HDS type
      HTYPE = '_'//TYPE

*  Is object primitive?
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( PRIM ) THEN

*    Clone a copy of the locator for mapping
        CALL DAT_CLONE( LOC, MLOC, STATUS )

*    Map the object
        CALL DAT_MAPV( MLOC, HTYPE, MODE, PTR, NELM, STATUS )

*  Otherwise structured ARRAY
      ELSE

*    Get variant allowed under SGP/38
        IF DAT_THERE( LOC, 'VARIANT', THERE, STATUS ) THEN
          CALL CMP_GET0C( LOC, 'VARIANT', VARNT, STATUS )
        ELSE
          VARNT = 'SIMPLE'
        END IF

*    Simple array variant?
        IF ( VARNT .EQ. 'SIMPLE' ) THEN

*      Locate the DATA item
          CALL DAT_FIND( LOC, 'DATA', MLOC, STATUS )

*      And map it
          CALL DAT_MAPV( MLOC, HTYPE, MODE, PTR, NELM, STATUS )

*    The scaled array variant
        ELSE IF ( VARNT .EQ. 'SCALED' ) THEN

*    The spaced array variant
        ELSE IF ( VARNT .EQ. 'SPACED' ) THEN

*    Get array shape and total number of elements
        CALL ADI1_ARYSHP( LOC, DAT__MXDIM, DIMS, NDIM, ATYPE, STATUS )
        CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*      Read base and scale values. Read them into DOUBLE PRECISION
*      variables even though the access type isn't necessarily double. We
*      don't access the values directly here so this doesn't matter. To
*      be tidy we should map two more dynamic arrays of length one element
          CALL DAT_FIND( LOC, 'BASE', ACLOC, STATUS )
          CALL DAT_GET( ACLOC, HTYPE, 0, 0, BASE, STATUS )
          CALL DAT_ANNUL( ACLOC, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ADI1_ARYMAP_2', 'Unable to read BASE '/
     :                    /'component from spaced array', STATUS )
            GOTO 99
          END IF
          CALL DAT_FIND( LOC, 'SCALE', ACLOC, STATUS )
          CALL DAT_GET( ACLOC, HTYPE, 0, 0, SCALE, STATUS )
          CALL DAT_ANNUL( ACLOC, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ADI1_ARYMAP_3', 'Unable to read SCALE '/
     :                    /'component from spaced array', STATUS )
            GOTO 99
          END IF

*      Map workspace of required type
          CALL DYN_MAPT( 1, NELM, HTYPE, PTR, STATUS )

*      Fill workspace with regular values
          CALL ARR_REG1T( HTYPE, BASE, SCALE, NELM, %VAL(PTR), STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'V', VARNT )
          CALL ERR_REP( 'ADI1_ARYMAP_1', 'Unsupported array '/
     :                  /'variant ^V', STATUS )

        END IF

      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_ARYMAP', STATUS )

      END
