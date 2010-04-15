      SUBROUTINE DTA_STRUC( PATH, STRUCT, STATUS )
*+
*  Name:
*     DTA_STRUC

*  Purpose:
*     Return whether or not a named data object is a structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_STRUC( PATH, STRUCT, DTA_STATUS )

*  Description:
*     This routine tells whether a named data object is a structure or a
*     primitive. A dimension or cell specification at the end of the
*     path is ignored.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The DTA structure name. This should be in the standard form for
*        structure names, i.e. with dots separating the component names.
*        Case is unimportant. A dimension or cell specification at the
*        end is ignored.
*     STRUCT = LOGICAL (Returned)
*        True if the named object is a structure, false if it is a
*        primitive object.
*     DTA_STATUS = INTEGER (Returned)
*        The DTA status.

*  Authors:
*     ks: Keith Shortridge (CIT, AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     16 Jun 1986 (ks):
*        Original version.
*     10 Jan 1992 (ks):
*        Syntax of include statements changed to remove VMS logical
*        names and to use lower case, to enable compilation on a SUN.
*     24 Jan 1992 (ks):
*        Calls to EMS added to control error reporting.
*     12 Mar 1993 (hme):
*        Changed CHARACTER*15 to *(DAT__SZLOC).
*     07 Mar 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Arguments Given:
      CHARACTER * ( * ) PATH

*  Arguments Returned:
      LOGICAL STRUCT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDOT1, IDOT2, IBRA ! Pointers into string
      INTEGER NDIM               ! Dimensionality
      INTEGER DIMS( DAT__MXDIM ) ! Dimension or cell number
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

*  Internal References:
      INTEGER CHR_LEN            ! Used length of a string

*.

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Split off any cell specification.
      CALL DTA1_SPLIT( PATH, DAT__MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( IBRA .EQ. 0 ) IBRA = 1 + CHR_LEN( PATH )

*  Locate the object.
      CALL DTA1_LOC( PATH(:IBRA-1), LOC, STATUS )

*  Ask HDS whether this is a structure.
      CALL DAT_STRUC( LOC, STRUCT, STATUS )

*  Release locator.
      CALL DAT_ANNUL( LOC, STATUS )

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END
