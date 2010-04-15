      SUBROUTINE DSA_DATA_TYPE( DSAREF, TYPE, STRUCT, STATUS )
*+
*  Name:
*     DSA_DATA_TYPE

*  Purpose:
*     Return the type of the main data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_DATA_TYPE( DSAREF, TYPE, STRUCT, STATUS )

*  Description:
*     This routine returns the data type of the main data array
*     of a structure. If the data array is a structured type, this is
*     also indicated. For a simple NDF the type is returned as
*     'SIMPLE/FLOAT', 'SIMPLE/DOUBLE', etc and STRUCT is returned as
*     true. For a primitive NDF the type is returned as 'FLOAT',
*     'DOUBLE' etc and STRUCT is returned as false.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The type of the NDF's data component in DSA speak.
*     STRUCT = LOGICAL (Returned)
*        Whether the array is non-primitive (i.e. simple).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
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
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF

*  Arguments Returned:
      CHARACTER * ( * ) TYPE
      LOGICAL STRUCT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( 6 ) DSATYP   ! The data type in DSA speak
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! The NDF data type
      CHARACTER * ( DAT__SZLOC ) TLOC( 2 ) ! HDS locators

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find the reference slot.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Find out if the NDF's DATA_ARRAY is primitive.
      CALL NDF_LOC( DSA__REFID1(SLOT), 'READ', TLOC(1), STATUS )
      CALL DAT_FIND(  TLOC(1), 'DATA_ARRAY', TLOC(2), STATUS )
      CALL DAT_STRUC( TLOC(2), STRUCT, STATUS )
      CALL DAT_ANNUL( TLOC(2), STATUS )
      CALL DAT_ANNUL( TLOC(1), STATUS )

*  Find out the NDF's data type.
      CALL NDF_TYPE( DSA__REFID1(SLOT), 'DATA', NDFTYP, STATUS )
      CALL DSA1_DSATYP( NDFTYP, DSATYP, STATUS )

*  Prepend 'SIMPLE' if the NDF is not primitive.
      IF ( STRUCT ) THEN
         TYPE = 'SIMPLE/' // DSATYP
      ELSE
         TYPE = DSATYP
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
