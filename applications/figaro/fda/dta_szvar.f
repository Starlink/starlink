      SUBROUTINE DTA_SZVAR( PATH, MXDIM, NDIM, DIMS, STATUS )
*+
*  Name:
*     DTA_SZVAR

*  Purpose:
*     Return the dimensions of a named structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_SZVAR( PATH, MXDIM, NDIM, DIMS, DTA_STATUS )

*  Description:
*     This routine returns the dimensions of a named data object.
*     A dimension or cell specification at the end of the
*     path is ignored.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The DTA structure name. This should be in the standard form for
*        structure names, i.e. with dots separating the component names.
*        Case is unimportant.
*     MXDIM = INTEGER (Given)
*        The maximum number of dimensions to return - i.e. the dimension
*        of DIMS.
*     NDIM = INTEGER (Returned)
*        The number of dimensions of the structure.
*     DIMS( MXDIM ) = INTEGER (Returned)
*        Array in which the dimensions of the named object are returned.
*     DTA_STATUS = INTEGER (Returned)
*        The DTA status.

*  Authors:
*     ks: Keith Shortridge (CIT, AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     ajh: Anthony Holloway (UoM, Starlink)
*     tdca: Tim Ash (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Oct 1982 (ks):
*        Original version.
*     13 Mar 1986 (ks):
*        Modified to use HDS routines.
*     10 Jan 1992 (ks):
*        Syntax of include statements changed to remove VMS logical
*        names and to use lower case, to enable compilation on a SUN.
*     24 Jan 1992 (ks):
*        Calls to EMS added to control error reporting.
*     12 Mar 1993 (hme):
*        Changed CHARACTER*15 to *(DAT__SZLOC).
*     01 Mar 1996 (hme):
*        FDA library.
*     24 Jun 1999 (tdca+ajh):
*        Fixed bug in DTA1_SPLIT call.
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
      INTEGER MXDIM

*  Arguments Returned:
      INTEGER NDIM
      INTEGER DIMS( MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDOT1, IDOT2, IBRA ! Pointers into string
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS type

*  Internal References:
      INTEGER CHR_LEN            ! Used length of string

*.

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Split off any cell specification.
      CALL DTA1_SPLIT( PATH, MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( IBRA .EQ. 0 ) IBRA = 1 + CHR_LEN( PATH )

*  Locate the object.
      CALL DTA1_LOC( PATH(:IBRA-1), LOC, STATUS )

*  Find the HDS shape.
      CALL DAT_SHAPE( LOC, MXDIM, DIMS, NDIM, STATUS )

*  For character types, insert the string length as first dimension.
      CALL DAT_TYPE( LOC, TYPE, STATUS )
      IF ( TYPE(:6) .EQ. '_CHAR*' ) THEN
         IF ( MXDIM .LT. 1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T016', PATH )
            CALL ERR_REP( 'FDA_E087', 'DTA_SZVAR: Error finding ' //
     :         'shape of the character object ^FDA_T016. ' //
     :         'Unable to return string length as first dimension.',
     :         STATUS )
            GO TO 500
         END IF
         CALL DTA1_DECDIM( TYPE(7:CHR_LEN(TYPE)), 1,
     :      NDIM, DIMS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         CALL DAT_SHAPE( LOC, MXDIM-1, DIMS(2), NDIM, STATUS )
         NDIM = NDIM + 1
      END IF

*  Tidy up.
 500  CONTINUE
      CALL DAT_ANNUL( LOC, STATUS )

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
