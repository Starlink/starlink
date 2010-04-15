      SUBROUTINE DSA_FREE_WORKSPACE( MSLOT, STATUS )
*+
*  Name:
*     DSA_FREE_WORKSPACE

*  Purpose:
*     Release previously obtained work space or work array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_FREE_WORKSPACE( MSLOT, STATUS )

*  Description:
*     When work space is obtained through a call to DSA_GET_WORKSPACE,
*     or through a call to DSA_GET_WORK_ARRAY, a handle value (MSLOT)
*     is returned to identify the work space obtained. The work space
*     can be released through a call to DSA_FREE_WORKSPACE, specifying
*     that slot number.

*  Arguments:
*     MSLOT = INTEGER (Given)
*        The map slot that was returned by the mapping routine to its
*        caller. This is a handle into the DSA global variables, which
*        in turn enable this routine to take the proper action.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine will try to function even if
*        entered with a bad status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     09 Jul 1987(ks):
*        Original version.
*     28 Sep 1989(ks):
*        Now uses $DELTVA for large arrays.
*     21 Aug 1992(ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992(ks):
*        Introduced GEN_SUCCESS to handle tests on status values
*        that follow the VMS convention.
*     29 Aug 1992(ks):
*        "INCLUDE" filenames now upper case.
*     02 Sep 1992(ks):
*        Now uses PSX_FREE instead of the VMS routines
*        LIB$FREE_VM and SYS$DELTVA.
*     25 Nov 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Move the action to DSA1_FREWKS.
*        Translate between application-side status and Starlink status.
*     23 Jan 1997 (mjcl):
*        Changed DSA1_FREWKS to use PSX_FREE rather than NDF_ call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! Global status

*.

*  Begin error context and translate status.
      ISTAT = STATUS
      CALL ERR_MARK
      STATUS = SAI__OK

*  Call the work routine.
      CALL DSA1_FREWKS( MSLOT, STATUS )

*  Translate status and end error context.
      IF ( ISTAT .NE. 0 ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = ISTAT
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = ISTAT
      END IF
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE DSA1_FREWKS( MSLOT, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Begin error context.
      CALL ERR_BEGIN( STATUS )

*  Check map slot number in range and used.
      IF ( MSLOT .LT. 1 .OR. MSLOT .GT. DSA__MAXMAP ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T006', MSLOT )
         CALL ERR_REP( 'FDA_E013', 'DSA_FREWKS: Attempt ' //
     :      'to release invalid work space slot ^FDA_T006.', STATUS )
         GO TO 500
      END IF
      IF ( .NOT. DSA__MAPUSD(MSLOT) ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T006', MSLOT )
         CALL ERR_REP( 'FDA_E014', 'DSA_FREWKS: Attempt to ' //
     :      'release the unused work space slot ^FDA_T006.', STATUS )
         GO TO 500
      END IF

*  Check that this is a work space and not some other data.
      IF ( DSA__MAPREF(MSLOT) .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T006', MSLOT )
         CALL ERR_REP( 'FDA_E015', 'DSA_FREWKS: Attempt to ' //
     :      'release data as if they were a work space ' //
     :      '(slot ^FDA_T006).', STATUS )
         GO TO 500
      END IF

*  Free the previously allocated workspace.
      CALL PSX_FREE( DSA__MAPID1(MSLOT), STATUS )

*  Reset the map slot.
      DSA__MAPUSD(MSLOT) = .FALSE.
      DSA__MAPID1(MSLOT) = NDF__NOID
      DSA__MAPAXI(MSLOT) = 0
      DSA__MAPREF(MSLOT) = 0
      DSA__MAPPT1(MSLOT) = 0
      DSA__MAPPT2(MSLOT) = 0
      DSA__MAPNAM(MSLOT) = ' '
      DSA__MAPLO1(MSLOT) = DAT__NOLOC
      DSA__MAPLO2(MSLOT) = DAT__NOLOC

*  Return.
 500  CONTINUE
      CALL ERR_END( STATUS )
      END
