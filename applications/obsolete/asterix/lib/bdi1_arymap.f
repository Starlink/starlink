      SUBROUTINE BDI1_ARYMAP( LOC, TYPE, MODE, MAPDYN, PSID, PTR,
     :                        NELM, WBPTR, STATUS )
*+
*  Name:
*     BDI1_ARYMAP

*  Purpose:
*     Map a primitive array or ARRAY structure object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_ARYMAP( LOC, TYPE, MODE, MAPDYN, PSID, PTR, NELM,
*                       WBPTR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator to array
*     TYPE = CHARACTER*(*) (given)
*        The type to map with
*     MODE = CHARACTER*(*) (given)
*        The access mode, READ, UPDATE or WRITE
*     MAPDYN = LOGICAL (given)
*        Force data to mapped copied to dynamic memory
*     PSID = INTEGER (given)
*        ADI identifier of private storage area
*     PTR = INTEGER (returned)
*        The mapped data address
*     NELM = INTEGER (returned)
*        Number of mapped data elements
*     WBPTR = INTEGER (returned)
*        Write back procedure address
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Aug 1995 (DJA):
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
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*(*)		TYPE, MODE
      INTEGER			PSID
      LOGICAL			MAPDYN

*  Arguments Returned:
      INTEGER			PTR, NELM, WBPTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI1_ARYWB
      EXTERNAL			UTIL_PLOC
        INTEGER			UTIL_PLOC

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	ACLOC			! ARRAY component
      CHARACTER*(DAT__SZTYP)	ATYPE			! Actual array type
      CHARACTER*(DAT__SZTYP)	HTYPE			! HDS style type name
      CHARACTER*3		MMODE			! Mapping mode
      CHARACTER*(DAT__SZLOC)	SLOC			! Locator to save
      CHARACTER*10		VARNT			! Array variant name

      DOUBLE PRECISION		BASE, SCALE		! Spaced array descrip

      INTEGER			DIMS(DAT__MXDIM)	! Array dimensions
      INTEGER			FPTR			! Mapped file object
      INTEGER			NDIM			! Array dimensionality

      LOGICAL			PRIM			! Object is primitive?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defaults
      MMODE = 'loc'
      FPTR = 0
      SLOC = DAT__NOLOC
      WBPTR = UTIL_PLOC( BDI1_ARYWB )

*  Construct HDS type
      HTYPE = '_'//TYPE

*  Get array shape and total number of elements
      CALL ADI1_ARYSHP( LOC, DAT__MXDIM, DIMS, NDIM, ATYPE, STATUS )
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  Is object primitive?
      CALL DAT_PRIM( LOC, PRIM, STATUS )
      IF ( PRIM ) THEN

*    Make dynamic copy if required
        IF ( MAPDYN ) THEN

*      Map workspace
          CALL DYN_MAPT( 1, NELM, HTYPE, PTR, STATUS )

*      Extract HDS data into workspace
          CALL DAT_GET( LOC, HTYPE, NDIM, DIMS, %VAL(PTR), STATUS )

*    Otherwise simply map
        ELSE

*      Clone a copy of the locator for mapping
          CALL DAT_CLONE( LOC, SLOC, STATUS )

*      Map the object
          CALL DAT_MAPV( SLOC, HTYPE, MODE, FPTR, NELM, STATUS )
          PTR = FPTR

        END IF

*  Otherwise structured ARRAY
      ELSE

*    Get variant allowed under SGP/38
        CALL CMP_GET0C( LOC, 'VARIANT', VARNT, STATUS )

*    Simple array variant?
        IF ( VARNT .EQ. 'SIMPLE' ) THEN

*      Make dynamic copy if required
          IF ( MAPDYN ) THEN

*        Map workspace
            CALL DYN_MAPT( 1, NELM, HTYPE, PTR, STATUS )

*        Extract HDS data into workspace
            CALL DAT_FIND( LOC, 'DATA', ACLOC, STATUS )
            CALL DAT_GET( ACLOC, HTYPE, NDIM, DIMS, %VAL(PTR), STATUS )
            CALL DAT_ANNUL( ACLOC, STATUS )

          ELSE

*        Locate the DATA item
            CALL DAT_FIND( LOC, 'DATA', SLOC, STATUS )

*        And map it
            CALL DAT_MAPV( SLOC, HTYPE, MODE, PTR, NELM, STATUS )

          END IF

*    The scaled array variant
        ELSE IF ( VARNT .EQ. 'SCALED' ) THEN

*    The spaced array variant
        ELSE IF ( VARNT .EQ. 'SPACED' ) THEN

*      Read base and scale values. Read them into DOUBLE PRECISION
*      variables even though the access type isn't necessarily double. We
*      don't access the values directly here so this doesn't matter. To
*      be tidy we should map two more dynamic arrays of length one element
          CALL DAT_FIND( LOC, 'BASE', ACLOC, STATUS )
          CALL DAT_GET( ACLOC, HTYPE, 0, 0, BASE, STATUS )
          CALL DAT_ANNUL( ACLOC, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'BDI1_ARYMAP_2', 'Unable to read BASE '/
     :                    /'component from spaced array', STATUS )
            GOTO 99
          END IF
          CALL DAT_FIND( LOC, 'SCALE', ACLOC, STATUS )
          CALL DAT_GET( ACLOC, HTYPE, 0, 0, SCALE, STATUS )
          CALL DAT_ANNUL( ACLOC, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'BDI1_ARYMAP_3', 'Unable to read SCALE '/
     :                    /'component from spaced array', STATUS )
            GOTO 99
          END IF

*      Map workspace of required type
          CALL DYN_MAPT( 1, NELM, HTYPE, PTR, STATUS )

*      Fill workspace with regular values
          CALL ARR_REG1T( HTYPE, BASE, SCALE, NELM, %VAL(PTR), STATUS )

*      Data is dynamic
          MMODE = 'dyn'

*      Clone a copy of the locator for mapping
          CALL DAT_CLONE( LOC, SLOC, STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'V', VARNT )
          CALL ERR_REP( 'BDI1_ARYMAP_1', 'Unsupported array '/
     :                  /'variant ^V', STATUS )

        END IF

      END IF

*  Store details in private store
      CALL BDI1_STOMAP( PSID, MMODE, SLOC, FPTR, PTR, NELM, 0, TYPE,
     :                  MODE, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_ARYMAP', STATUS )

      END
