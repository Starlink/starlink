      SUBROUTINE DSA_SEEK_AXIS( DSAREF, AXIS, EXIST, STATUS )
*+
*  Name:
*     DSA_SEEK_AXIS

*  Purpose:
*     Determine whether or not an axis centre array exists.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SEEK_AXIS( DSAREF, AXIS, EXIST, STATUS )

*  Description:
*     This routine looks to see if an NDF contains an axis structure.
*     Contrary to earlier implementations, axis information exists either
*     not at all in an NDF or for all axes in the NDF. Also contrary to
*     earlier implementations, no check is performed as to the validity
*     of the shape of the axis centre or width arrays.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     AXIS = INTEGER (Given)
*        Ignored.
*     EXIST = LOGICAL (Returned)
*        True/false if the axis component exists/does not exist.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Jun 1987 (ks):
*        Original version.
*     08 Dec 1989 (ks):
*        Minor bug in format of error messages fixed.
*     19 Jan 1990 (ks):
*        Modified to use DSA__ routines to get data structure details
*        instead of assuming original Figaro format.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     30 Jan 1996 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Move the action to DSA1_SKAX.
*        Translate between application-side status and Starlink status.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      INTEGER AXIS

*  Arguments Returned:
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SLOT               ! The reference slot

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Look up the reference name in the tables, then call the work routine.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      CALL DSA1_SKAX( SLOT, AXIS, EXIST, STATUS )

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



      SUBROUTINE DSA1_SKAX( SLOT, AXIS, EXIST, STATUS )

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER SLOT
      INTEGER AXIS

*  Arguments Returned:
      LOGICAL EXIST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NDIM               ! NDF dimensionality
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of NDF

*.

*  Return immediately on bad status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the validity of the value of AXIS.
      CALL NDF_DIM( DSA__REFID1(SLOT), NDF__MXDIM, DIMS, NDIM, STATUS )
      IF ( AXIS .LT. 1 .OR. AXIS .GT. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'FDA_T002', AXIS )
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL ERR_REP( 'FDA_E048', 'DSA_SKAX: Invalid axis ' //
     :      'number (^FDA_T002) for reference ^FDA_T001.', STATUS )
         GO TO 500
      END IF

*  It is sufficient to check the state of the NDF's AXIS structure.
      CALL NDF_STATE( DSA__REFID1(SLOT), 'AXIS', EXIST, STATUS )

*  Exit.
 500  CONTINUE
      END
