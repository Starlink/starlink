      SUBROUTINE DSA_GET_LU( LU, STATUS )
*+
*  Name:
*     DSA_GET_LU

*  Purpose:
*     Obtain a free logical unit number.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_LU( LU, STATUS )

*  Description:
*     This routine returns the number of a currently unused Fortran
*     logical unit. The unit number is remembered by the DSA system
*     and DSA_CLOSE will automatically close any file open on that
*     unit and will release the unit number. DSA_FREE_LU will also close
*     the file and release the unit.

*  Arguments:
*     LU = INTEGER (Returned)
*        The unit number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 Mar 1988 (ks):
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
*        No longer call gen_errmsg if lib$get_lun fails.
*        And call fio_gunit anyway.
*     26 Nov 1995 (hme):
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

*  Arguments Returned:
      INTEGER LU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! Free slot for new unit number

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find a free slot.
      DO 1 SLOT = 1, DSA__MAXLU
         IF ( .NOT. DSA__LUUSD(SLOT) ) GO TO 2
 1    CONTINUE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'FDA_E017', 'DSA_GET_LU: ' //
     :      'No free logical unit slot left, all are in use.', STATUS )
         GO TO 500
 2    CONTINUE

*  Attempt to get a spare logical unit. If this succeeds, fill in slot.
      CALL FIO_GUNIT( LU, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         DSA__LUUSD(SLOT) = .TRUE.
         DSA__LUNUM(SLOT) = LU
      END IF

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
