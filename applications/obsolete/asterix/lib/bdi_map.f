      SUBROUTINE BDI_MAP( ID, ITEMS, TYPE, MODE, PTRS, STATUS )
*+
*  Name:
*     BDI_MAP

*  Purpose:
*     Map the named items with the specified type and mode

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_MAP( ID, ITEMS, TYPE, MODE, PTRS, STATUS )

*  Description:
*     Maps the items specified by the ITEMS string with a type and mode
*     specified by TYPE and MODE. The pointers to the resulting areas
*     of memory are returned in PTRS.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be mapped
*     TYPE = CHARACTER*(*) (given)
*        The type with whichthe mapping will be performed
*     MODE = CHARACTER*(*) (given)
*        The access mode for the items. May also have an initialiser
*        appended such as /ZERO or /BAD (or /TRUE or /FALSE for logical
*        arrays) if the mode is write.
*     PTRS[] = INTEGER (returned)
*        The pointers to the mapped items
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Aug 1995 (DJA):
*        Original version.
*     13 Mar 1996 (DJA):
*        Added initialiser option
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			ID
      CHARACTER*(*)		ITEMS, TYPE, MODE

*  Arguments Returned:
      INTEGER			PTRS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      CHARACTER*10              INIT                    ! Initialiser
      CHARACTER*20		LITEM			! Local item name
      CHARACTER*20              LMODE			! Local copy of mode
      CHARACTER*7 		LTYPE			! Local copy of type

      INTEGER			ARGS(5)			! Function args
      INTEGER			C1, C2, CP		! Character pointers
      INTEGER			IDUM			! Dummy return from ADI
      INTEGER			IITEM			! Item counter
      INTEGER			LITL			! Used length of LITEM
      INTEGER			LSTAT			! Local status
      INTEGER			PSID			! Private item storage
      INTEGER			MCOUNT			! Object map count
      INTEGER			NELM			! # mapped values
      INTEGER			OARG			! Return value
      INTEGER			RVALS(2)		! Method return data

      LOGICAL			INITP			! Initialiser present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( BDI__PKG ) ) CALL BDI0_INIT( STATUS )

*  First function argument is the identifier
      ARGS(1) = ID
      ARGS(3) = 0

*  Second is the linked file object
      CALL ADI_GETLINK( ID, ARGS(2), STATUS )

*  Fourth is the mapping type
      CALL UDI0_CHKTYP( TYPE, LTYPE, STATUS )
      CALL ADI_NEWV0C( LTYPE, ARGS(4), STATUS )

*  Fifth is the mapping mode
      LMODE = MODE
      CALL CHR_UCASE( LMODE )

*  Look for the initialiser
      CP = INDEX( LMODE, '/' )
      IF ( CP .EQ. 0 ) THEN
        INITP = .FALSE.
      ELSE
        INITP = .TRUE.
        INIT = LMODE(CP+1:)
        LMODE = LMODE(:CP-1)
        IF ( LMODE(1:1) .NE. 'W' ) THEN
          CALL MSG_SETC( 'I', INIT )
          CALL MSG_SETC( 'M', LMODE )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Illegal initialiser present; cannot be '/
     :                  /'used in ^M mode', STATUS )
        END IF

      END IF
      CALL ADI_NEWV0C( LMODE, ARGS(5), STATUS )

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Check item name is valid, and make a local copy. Removes any
*    special item names such as E_Axis_Label.
        CALL BDI0_CHKITM( ID, ITEMS(C1:C2), LITEM, LITL, STATUS )

*    Check that item can be mapped
        CALL BDI0_CHKOP( LITEM(:LITL), 'Map', STATUS )

*    Locate private storage for this item
        CALL ADI0_LOCPST( ID, LITEM(:LITL), .TRUE., PSID, STATUS )

*    Get map count
        CALL ADI_CGET0I( PSID, 'MapCount', MCOUNT, STATUS )

*    Already mapped?
        IF ( MCOUNT .GT. 0 ) THEN

*      Extract pointer. Should check that mode and type match...
          CALL ADI_CGET0I( PSID, 'Ptr', PTRS(IITEM), STATUS )

        ELSE

*      Construct string for this item
          CALL ADI_NEWV0C( LITEM(:LITL), ARGS(3), STATUS )

*      Invoke the function
          CALL ADI_FEXEC( 'FileItemMap', 5, ARGS, OARG, STATUS )

*      Success?
          IF ( (STATUS .EQ. SAI__OK) .AND. (OARG.NE.ADI__NULLID) ) THEN

*        Extract pointer & number of elements from return value
            CALL ADI_GET1I( OARG, 2, RVALS, IDUM, STATUS )
            PTRS(IITEM) = RVALS(1)
            NELM = RVALS(2)
            CALL ADI_ERASE( OARG, STATUS )

*        Initialise?
            IF ( INITP .AND. (LMODE(1:1) .EQ. 'W') ) THEN

*          Switch on data type
              IF ( LTYPE .EQ. 'REAL' ) THEN
                CALL BDI0_INITR( INIT, NELM, %VAL(PTRS(IITEM)), STATUS )
              ELSE IF ( LTYPE .EQ. 'DOUBLE' ) THEN
                CALL BDI0_INITD( INIT, NELM, %VAL(PTRS(IITEM)), STATUS )
              ELSE IF ( LTYPE .EQ. 'INTEGER' ) THEN
                CALL BDI0_INITI( INIT, NELM, %VAL(PTRS(IITEM)), STATUS )
              ELSE IF ( LTYPE .EQ. 'LOGICAL' ) THEN
                CALL BDI0_INITL( INIT, NELM, %VAL(PTRS(IITEM)), STATUS )
              ELSE IF ( LTYPE .EQ. 'BYTE' ) THEN
                CALL BDI0_INITB( INIT, NELM, %VAL(PTRS(IITEM)), STATUS )
              ELSE IF ( LTYPE .EQ. 'UBYTE' ) THEN
                CALL BDI0_INITUB( INIT, NELM, %VAL(PTRS(IITEM)),
     :                            STATUS )
              ELSE IF ( LTYPE .EQ. 'WORD' ) THEN
                CALL BDI0_INITW( INIT, NELM, %VAL(PTRS(IITEM)), STATUS )
              ELSE IF ( LTYPE .EQ. 'UWORD' ) THEN
                CALL BDI0_INITUW( INIT, NELM, %VAL(PTRS(IITEM)),
     :                            STATUS )
              ELSE
                CALL MSG_SETC( 'T', TYPE )
                STATUS = SAI__ERROR
                CALL ERR_REP( ' ', 'WRITE mode initialisation is not'/
     :                             /' supported for type ^T', STATUS )
              END IF
            END IF

*      Mapping failed
          ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETC( 'ITEM', LITEM(:LITL) )
            LSTAT = SAI__OK
            CALL BDI0_DESCID( ID, 'F', STATUS )
            CALL ERR_REP( 'BDI_MAP_1', 'Unable to map item '/
     :                    /'^ITEM in file ^F', STATUS )
          ELSE
            PTRS(IITEM) = 0
          END IF

        END IF

*    Adjust map count
        CALL BDI0_ADJMCT( PSID, 1, STATUS )

*    Release private storage
        CALL ADI_ERASE( PSID, STATUS )

*    Release the item string
        CALL ERR_BEGIN( STATUS )
        CALL ADI_ERASE( ARGS(3), STATUS )
        CALL ERR_END( STATUS )

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )

      END DO

*  Scrub the temporary strings holding type and mode if they've been created
      CALL ERR_BEGIN( STATUS )
      CALL ADI_ERASE( ARGS(4), STATUS )
      CALL ADI_ERASE( ARGS(5), STATUS )
      CALL ERR_END( STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_MAP', STATUS )

      END
