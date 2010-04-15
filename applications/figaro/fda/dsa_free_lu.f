      SUBROUTINE DSA_FREE_LU( LU, STATUS )
*+
*  Name:
*     DSA_FREE_LU

*  Purpose:
*     Release a logical unit number.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_FREE_LU( LU, STATUS )

*  Description:
*     This routine releases a logical unit number that has previously
*     been obtained by DSA_GET_LU, and closes any file that might be
*     open on it.

*  Arguments:
*     LU = INTEGER (Given)
*        The unit number.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine tries to function even when
*        entered with bad status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Mar 1988 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        Introduced GEN_SUCCESS to handle tests on status values
*        that follow the VMS convention.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     21 Jul 1993 (hme):
*        No longer call gen_errmsg if lib$free_lun fails.
*        And call fio_punit anyway.
*     25 Nov 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
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
      INTEGER LU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ISTAT              ! Global status
      INTEGER SLOT               ! The unit slot
      INTEGER IGNORE             ! Ignored status

*.

*  Begin error context and translate status.
      ISTAT = STATUS
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find the slot used for the unit.
      DO 1 SLOT = 1, DSA__MAXLU
         IF ( DSA__LUUSD(SLOT) .AND. DSA__LUNUM(SLOT) .EQ. LU ) GO TO 2
 1    CONTINUE
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T005', LU )
         CALL ERR_REP( 'FDA_E012', 'DSA_FREE_LU: Attempt to ' //
     :      'release Fortran logical unit ^FDA_T005, which was not ' //
     :      'allocated by this system.', STATUS )
         GO TO 500
 2    CONTINUE

*  Close the unit (any file possibly open on it).
      CLOSE( LU, IOSTAT=IGNORE )

*  Release the unit.
      CALL FIO_PUNIT( LU, STATUS )

*  Clear the slot.
      DSA__LUUSD(SLOT) = .FALSE.
      DSA__LUNUM(SLOT) = 0

*  Tidy up.
 500  CONTINUE

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
