      SUBROUTINE DSA_GET_WORKSPACE( NBYTES, ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA_GET_WORKSPACE

*  Purpose:
*     Get a temporary NDF for a byte array of specified size.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_WORKSPACE( NBYTES, ADDRESS, MSLOT, STATUS )

*  Description:
*     Given a number of bytes, this routine obtains work space for a
*     byte array of the specified size, and returns its address. It
*     also returns a slot number which should be used to refer to the
*     memory in order to release it later. A work space is released
*     through a call to DSA_FREE_WORKSPACE. DSA_CLOSE will also release
*     any work spaces left allocated.

*  Arguments:
*     NBYTES = INTEGER (Given)
*        The size of the array to be allocated.
*     ADDRESS = INTEGER (Returned)
*        The memory address of the work array.
*     MSLOT = INTEGER (Returned)
*        A handle that can be used later to release the work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     30 Jun 1987 (ks):
*        Original version.
*     28 Sep 1989 (ks):
*        Modified to use $EXPREG above a certain array size -
*        this helps prevent fragmentation.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        Introduced GEN_SUCCESS to handle tests on status values
*        that follow the VMS convention.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     02 Sep 1992 (ks):
*        Now uses PSX_MALLOC instead of the VMS routines
*        LIB$GET_VM and SYS$EXPREG.
*     26 Nov 1995 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NBYTES

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Just get a work array of type 'BYTE'.
      CALL DSA_GET_WORK_ARRAY( NBYTES, 'BYTE', ADDRESS, MSLOT, STATUS )

*  Return.
      END
