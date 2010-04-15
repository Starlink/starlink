      SUBROUTINE DSA_GET_WORK_ARRAY( NELM, TYPE,
     :   ADDRESS, MSLOT, STATUS )
*+
*  Name:
*     DSA_GET_WORK_ARRAY

*  Purpose:
*     Get a temporary NDF for an array of specified type and size.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_GET_WORK_ARRAY( NELM, TYPE, ADDRESS, MSLOT, STATUS )

*  Description:
*     Given a type (e.g. 'FLOAT') and a number of elements, this routine
*     obtains work space for an array of the specified size and type,
*     and returns its address. It also returns a slot number which
*     should  be used to refer to the memory in order to release it
*     later. A work array is released through a call to
*     DSA_FREE_WORKSPACE.

*  Arguments:
*     NELM = INTEGER (Given)
*        The size of the array to be allocated.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of array to be allocated. Together with the number
*        of elements this determines the number of bytes.
*     ADDRESS = INTEGER (Returned)
*        The memory address of the work array.
*     MSLOT = INTEGER (Returned)
*        A handle that can be used later to release the work space.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     30 Jun 1987 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     25 Nov 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Translate between application-side status and Starlink status.
*     23 Jan 1997 (mjc):
*        Use PSX_MALLOC instead of NDF_TEMP.
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
      INTEGER NELM
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER ADDRESS
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Functions Called:
      INTEGER DSA_TYPESIZE

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find a free map slot.
      CALL DSA1_MSNEW( MSLOT, STATUS )

*  Allocate memory reqested.
      CALL PSX_MALLOC( NELM * DSA_TYPESIZE( TYPE, STATUS ),
     :     ADDRESS, STATUS )

*  Fill map slot.
      IF ( STATUS .EQ. SAI__OK ) THEN
         DSA__MAPUSD(MSLOT) = .TRUE.
         DSA__MAPID1(MSLOT) = ADDRESS
         DSA__MAPNAM(MSLOT) = '*'
      END IF

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
