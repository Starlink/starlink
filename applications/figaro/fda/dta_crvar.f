      SUBROUTINE DTA_CRVAR( PATH, TYPE, STATUS )
*+
*  Name:
*     DTA_CRVAR

*  Purpose:
*     Create an object specified by its DTA object name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DTA_CRVAR( PATH, TYPE, DTA_STATUS )

*  Description:
*     This routine creates a named structure, of a given type. The
*     parent of the structure must already exist; that is, the variable
*     must have a name of the form 'environment.object' (environment may
*     have components as well) and there must already exist a structure
*     with the name 'environment'. The object itself must not already
*     exist. Dimensional information is obtained from PATH as well -
*     i.e. name can be of the form 'environment.obj[dimension list]',
*     where the dimension list is a set of integers separated by commas.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The DTA structure name.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of the variable. Can be any string. If it is not one
*        of the recognised DSA types, it is treated as a structure
*        type. In that case is must comply with the rules for HDS
*        component types. TYPE is case-independent.
*     DTA_STATUS = INTEGER (Returned)
*        The DTA status.

*  Authors:
*     ks: Keith Shortridge (CIT, AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18 Nov 1982 (ks):
*        Original version.
*     12 Mar 1986 (ks):
*        Re-written to use HDS routines.
*     08 Jan 1992 (ks):
*        Syntax of include statements changed to remove VMS logical
*        names and to use lower case, to enable compilation on a SUN.
*     24 Jan 1992 (ks):
*        Calls to EMS added to control error reporting.
*     12 Mar 1993 (hme):
*        Changed CHARACTER*15 to *(DAT__SZLOC).
*     28 Jul 1993 (hme):
*        Disuse STR$UPCASE.
*     29 Feb 1996 (hme):
*        FDA library.
*     04 Mar 1996 (hme):
*        Allow CHAR type as well, where the first dimension is the string
*        length.
*     08 Mar 1996 (hme):
*        Use DTA_SPLIT.
*     12 Mar 1996 (hme):
*        Initialise numeric primitives to bad value. (Turned off again)
*     2 January 1998 (acd):
*        Modified to check whether the given component already exists.
*        If it does then it is deleted and recreated.  Note that it is
*        necessary to delete and recreate the component rather than
*        simply annulling the status in order to properly handle the
*        cases where either the new component is of a different data
*        type or (more likely in practice) it is a character string of a
*        different size.
*     21 Dec 2000 (acd):
*        Commented out unused variables (to correspond to the commented
*        out code).
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) PATH
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Call the work routine.
      CALL DTA1_CRVAR( PATH, TYPE, STATUS )

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



      SUBROUTINE DTA1_CRVAR( PATH, TYPE, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'DAT_ERR'          ! Standard DAT error codes

*  Arguments Given:
      CHARACTER * ( * ) PATH
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGNORE             ! Ignored
      INTEGER IDOT1, IDOT2, IBRA ! Pointers into PATH
*     INTEGER PNTR, NELM         ! Map pointer and array size
      INTEGER NDIM               ! Dimensionality of cell specification
      INTEGER DIMS( DAT__MXDIM ) ! Dimensions or cell
      CHARACTER * ( DAT__SZNAM ) NAME ! The component name
      CHARACTER * ( DAT__SZTYP ) HDSTYP ! The type in HDS speak
      CHARACTER * ( DAT__SZLOC ) LOC ! HDS locator

*  Internal References:
      LOGICAL CHR_SIMLR          ! String equality bar case

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Split the path into parent and component.
*  Also gives us the dimension specification at the end.
      CALL DTA1_SPLIT( PATH, DAT__MXDIM,
     :   IDOT1, IDOT2, IBRA, NDIM, DIMS, STATUS )
      IF ( IDOT2 .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'FDA_T016', PATH )
         CALL ERR_REP( 'FDA_E082', 'DTA1_CRVAR: Error creating the ' //
     :      'object ^FDA_T016. The DTA object name cannot be ' //
     :      'split into parent and new component.', STATUS )
         GO TO 500
      END IF
      IF ( IBRA .GT. 0 ) THEN
         NAME = PATH(IDOT2+1:IBRA-1)
      ELSE
         NAME = PATH(IDOT2+1:)
      END IF

*  Locate the parent.
      CALL DTA1_LOC( PATH(:IDOT2-1), LOC, STATUS )


*  Translate type, finalise dimensions, create object.

*  Character type.
      IF ( CHR_SIMLR( TYPE, 'CHAR' ) ) THEN
         IF ( NDIM .LT. 1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T016', PATH )
            CALL ERR_REP( 'FDA_E083', 'DTA1_CRVAR: Error creating ' //
     :         'the character-type object ^FDA_T016. The DTA object ' //
     :         'name contains no string length specification.', STATUS )
            GO TO 500
         END IF
         HDSTYP = '_CHAR*'
         CALL CHR_ITOC( DIMS(1), HDSTYP(7:), IGNORE )
         CALL ERR_MARK
         CALL DAT_NEW( LOC, NAME, HDSTYP, NDIM-1, DIMS(2), STATUS )
         IF (STATUS .EQ. DAT__COMEX) THEN
            CALL ERR_ANNUL (STATUS)

            CALL DAT_ERASE (LOC, NAME, STATUS)
            CALL DAT_NEW( LOC, NAME, HDSTYP, NDIM-1, DIMS(2), STATUS )
         END IF
         CALL ERR_RLSE
      ELSE
         CALL DSA1_NDFTYP( TYPE, HDSTYP, STATUS )

*  Structure type.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            HDSTYP = TYPE
            CALL ERR_MARK
            CALL DAT_NEW(  LOC, NAME, HDSTYP, NDIM, DIMS, STATUS )
            IF (STATUS .EQ. DAT__COMEX) THEN
               CALL ERR_ANNUL (STATUS)

               CALL DAT_ERASE (LOC, NAME, STATUS)
               CALL DAT_NEW(  LOC, NAME, HDSTYP, NDIM, DIMS, STATUS )
            END IF
            CALL ERR_RLSE

*  Numeric type.
         ELSE
            CALL ERR_MARK
            CALL DAT_NEW( LOC, NAME, HDSTYP, NDIM, DIMS, STATUS )
            IF (STATUS .EQ. DAT__COMEX) THEN
               CALL ERR_ANNUL (STATUS)

               CALL DAT_ERASE (LOC, NAME, STATUS)
               CALL DAT_NEW( LOC, NAME, HDSTYP, NDIM, DIMS, STATUS )
            END IF
            CALL ERR_RLSE
*           CALL CMP_MAPV( LOC, NAME, HDSTYP, 'WRITE',
*    :         PNTR, NELM, STATUS )
*           CALL DSA1_BFILL( HDSTYP, NELM, %VAL( CNF_PVAL(PNTR) ),
*    :                       STATUS )
*           CALL CMP_UNMAP( LOC, NAME, STATUS )
         END IF
      END IF


*  Return.
 500  CONTINUE
      CALL DAT_ANNUL( LOC, STATUS )
      END
