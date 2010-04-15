      SUBROUTINE DSA_CLOSE( STATUS )
*+
*  Name:
*     DSA_CLOSE

*  Purpose:
*     Close down the DSA system for a Figaro application.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_CLOSE( STATUS )

*  Description:
*     DSA_CLOSE should be called to close down the DSA_ routines at
*     the end of a Figaro program. Once this routine has been called,
*     it is then possible to call DSA_OPEN to reinitialise the system.
*     This routine closes and releases all Fortran logical units
*     allocated by DSA_GET_LU, releases and closes all referenced NDFs,
*     and releases all work spaces allocated by DSA_GET_WORKSPACE or
*     DSA_GET_WORK_ARRAY. Finally it ends the NDF context that was begun
*     by DSA_OPEN.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine tries to do its job even if
*        entered with a bad status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Jul 1987 (ks):
*        Original version.
*     01 Mar 1988 (ks):
*        Code to close down Fortran logical units added.
*     05 Jul 1988 (ks):
*        Modified to use DSA_CHECK_STRUCTURE.
*     22 Jul 1988 (ks):
*        Calls to post processing routines added.  Also call to
*        FIG_SETERR added.
*     30 Nov 1988 (ks):
*        Flushing of FITS structure added.
*     13 Feb 1990 (ks):
*        DSA_FLUSH_FITS changed to DSA__FLUSH_FITS.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     17 Dec 1992. (ks):
*        Added call to DSA_RENAME_TEMP. And added setting of
*        SYS_STATUS as replacement for FIG_SETERR.
*     21 Jul 1993 (hme):
*        Use DSA_FREE_LU.
*     19 Jul 1995 (hme):
*        Use DSA_FREE_LU.
*     26 Nov 1995 (hme):
*        FDA library.
*     26 Jan 1996 (hme):
*        Check axis/data shape consistency.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     21 Dec 2000 (acd):
*        Removed unused variable.
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

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER ISTAT              ! Global status
      INTEGER I                  ! Loop index
      INTEGER IGNORE             ! Ignored status

*.

*  Begin error context and translate status.
      ISTAT = STATUS
      CALL ERR_MARK
      STATUS = SAI__OK

*  Close and release all Fortran logical units.
      DO 1 I = 1, DSA__MAXLU
         IF ( DSA__LUUSD(I) ) THEN
            CLOSE( DSA__LUNUM(I), IOSTAT=IGNORE )
            CALL FIO_PUNIT( DSA__LUNUM(I), STATUS )
            DSA__LUUSD(I) = .FALSE.
            DSA__LUNUM(I) = 0
         END IF
 1    CONTINUE

*  Close structures and reset reference slots.
      DSA__BADQUA = .FALSE.
      DO 2 I = 1, DSA__MAXREF
         IF ( DSA__REFUSD(I) ) CALL DSA1_CLOSTR( I, STATUS )
 2    CONTINUE

*  Unamp and reset map slots.
*  Since all structures are closed, all remaining maps should be
*  work spaces.
      DO 4 I = 1, DSA__MAXMAP
         IF ( DSA__MAPUSD(I) ) CALL DSA1_FREWKS( I, STATUS )
 4    CONTINUE

*  Close the NDF context.
      CALL NDF_END( STATUS )

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
