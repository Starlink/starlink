      SUBROUTINE BDI1_ARYMAP( BID, LOC, TYPE, MODE, ENDIM, EDIMS,
     :                        PSID, PTR, NELM, STATUS )
*+
*  Name:
*     BDI1_ARYMAP

*  Purpose:
*     Map a primitive array or ARRAY structure object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_ARYMAP( BID, LOC, TYPE, MODE, ENDIM, EDIMS, PSID, PTR, NELM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     BID = INTEGER (given)
*        ADI identifier of top of object chain
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator to array
*     TYPE = CHARACTER*(*) (given)
*        The type to map with
*     MODE = CHARACTER*(*) (given)
*        The access mode, READ, UPDATE or WRITE
*     ENDIM = INTEGER (given)
*        The expected dimensionality according to the data model
*     EDIMS[] = INTEGER (given)
*        The expected dimensions according to the data model
*     PSID = INTEGER (given)
*        ADI identifier of private storage area
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
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*(*)		TYPE, MODE
      INTEGER			BID, PSID, ENDIM, EDIMS(*)

*  Arguments Returned:
      INTEGER			PTR, NELM

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
      CHARACTER*20		ITEM			! The item name
      CHARACTER*3		MSYS			! Mapping system
      CHARACTER*(DAT__SZLOC)	SLOC			! Locator to save
      CHARACTER*10		VARNT			! Array variant name

      DOUBLE PRECISION		BASE, SCALE		! Spaced array descrip
      DOUBLE PRECISION		DBUF			! Scalar data buffer

      INTEGER			DIMS(DAT__MXDIM)	! Array dimensions
      INTEGER			ENELM			! Expected # elements
      INTEGER			FPTR			! Mapped file object
      INTEGER			NDIM			! Array dimensionality
      INTEGER			SSIZE			! Scalar size

      LOGICAL			PRIM			! Object is primitive?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defaults
      MSYS = 'loc'
      FPTR = 0
      SLOC = DAT__NOLOC

*  Construct HDS type
      HTYPE = '_'//TYPE

*  Expect number of data elements
      CALL ARR_SUMDIM( ENDIM, EDIMS, ENELM )

*  Get array shape and total number of elements
      CALL ADI1_ARYSHP( LOC, DAT__MXDIM, DIMS, NDIM, ATYPE, STATUS )
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*  Is object primitive?
      CALL DAT_PRIM( LOC, PRIM, STATUS )

*  Object is scalar?
      IF ( NDIM .EQ. 0 ) THEN

*    Map workspace of required type
        CALL DYN_MAPT( 1, ENELM, HTYPE, PTR, STATUS )

*    Data is dynamic
        MSYS = 'dyn'

*    Extract the scalar value
        CALL DAT_GET( LOC, HTYPE, 0, 0, DBUF, STATUS )

*    Size in bytes of scalar
        IF ( HTYPE .EQ. '_DOUBLE' ) THEN
          SSIZE = VAL__NBD
        ELSE IF ( (HTYPE .EQ. '_UBYTE') .OR. (HTYPE .EQ. '_BYTE') ) THEN
          SSIZE = VAL__NBB
        ELSE IF ( (HTYPE .EQ. '_UWORD') .OR. (HTYPE .EQ. '_WORD') ) THEN
          SSIZE = VAL__NBW
        ELSE
          SSIZE = VAL__NBR
        END IF

*    Fill mapped array with copies of scalar data
        IF ( (ENELM .GT. NELM) .OR. (NDIM.EQ.0) ) THEN
          CALL BDI1_ARYMAP_REP( SSIZE, DBUF, ENELM, %VAL(PTR), STATUS )
        END IF

*    Clone a copy of the locator for mapping
        CALL DAT_CLONE( LOC, SLOC, STATUS )

*  Otherwise if number of elements differ we report an error
      ELSE IF ( ENELM .NE. NELM ) THEN
        CALL ADI_NAME( PSID, ITEM, STATUS )
        CALL MSG_SETC( 'IT', ITEM )
c        CALL BDI0_DESCID( BID, 'F', STATUS )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'The dimensions of item ^IT '/
     :           /'differ from those expected - check the program '/
     :                /'which created this file', STATUS )
        GOTO 99

*  Object is primitive (and matches dimensions from here on)
      ELSE IF ( PRIM ) THEN

*    Clone a copy of the locator for mapping
        CALL DAT_CLONE( LOC, SLOC, STATUS )

*    Map the object
        CALL DAT_MAPV( SLOC, HTYPE, MODE, FPTR, NELM, STATUS )
        PTR = FPTR

*  Otherwise structured ARRAY
      ELSE

*    Get variant allowed under SGP/38
        CALL CMP_GET0C( LOC, 'VARIANT', VARNT, STATUS )

*    Simple array variant?
        IF ( VARNT .EQ. 'SIMPLE' ) THEN

*      Locate the DATA item
          CALL DAT_FIND( LOC, 'DATA', SLOC, STATUS )

*      And map it
          CALL DAT_MAPV( SLOC, HTYPE, MODE, PTR, NELM, STATUS )

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
          MSYS = 'dyn'

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
      CALL BDI1_STOMAP( PSID, MSYS, SLOC, FPTR, PTR, ENDIM, EDIMS,
     :                  UTIL_PLOC( BDI1_ARYWB ), TYPE, MODE, STATUS )

*  Always return expected number of elements
      NELM = ENELM

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_ARYMAP', STATUS )

      END



      SUBROUTINE BDI1_ARYMAP_REP( SIZE, IN, N, OUT, STATUS )
*+
*  Name:
*     BDI1_ARYMAP_REP

*  Purpose:
*     Replicate the byte pattern IN into OUT N times

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_ARYMAP_REP( SIZE, IN, N, OUT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     SIZE = INTEGER (given)
*        Number of bytes in IN
*     IN[] = BYTE (given)
*        Data to be replicated
*     N = INTEGER (given)
*        Number of copies to make
*     OUT[] = BYTE (returned)
*        Copies of IN
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

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

*  Arguments Given:
      INTEGER			N, SIZE
      BYTE			IN(*)

*  Arguments Returned:
      BYTE			OUT(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I,J,K			! Loop variables
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make copies of input
      J = 1
      DO I = 1, N
        DO K = 1, SIZE
          OUT(J) = IN(K)
          J = J + 1
        END DO
      END DO

      END
