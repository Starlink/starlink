      SUBROUTINE ARY_SAME( IARY1, IARY2, SAME, ISECT, STATUS )
*+
*  Name:
*     ARY_SAME

*  Purpose:
*     Enquire if two arrays are part of the same base array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SAME( IARY1, IARY2, SAME, ISECT, STATUS )

*  Description:
*     The routine determines whether two array identifiers refer to
*     parts of the same base array.  If so, it also determines whether
*     they intersect.

*  Arguments:
*     IARY1 = INTEGER (Given)
*        Identifier for the first array (or array section).
*     IARY2 = INTEGER (Given)
*        Identifier for the second array (or array section).
*     SAME = LOGICAL (Returned)
*        Whether the identifiers refer to parts of the same base array.
*     ISECT = LOGICAL (Returned)
*        Whether the arrays intersect.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Two arrays (or array sections) are counted as intersecting if
*     (i) they both refer to the same base array and (ii) altering
*     values in one of the arrays can result in the values in the other
*     array changing in consequence.

*  Algorithm:
*     -  Import the two array identifiers.
*     -  Obtain indices to the two data object entries in the DCB.
*     -  See if the two arrays refer to the same data object. If not,
*     then they cannot intersect
*     -  If they refer to the same object, then calculate their mapping
*     region bounds, as if they were going to be mapped for access.
*     -  If either array does not have a mapping transfer window, then
*     they cannot intersect.
*     -  Otherwise, see if their mapping transfer windows intersect. If
*     so, then the two arrays intersect.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1989 (RFWS):
*        Original version.
*     26-SEP-1989 (RFWS):
*        Corrected typo in error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXDCB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IARY1
      INTEGER IARY2

*  Arguments Returned:
      LOGICAL SAME
      LOGICAL ISECT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB1              ! 1st array index in the ACB
      INTEGER IACB2              ! 2nd array index in the ACB
      INTEGER IDCB1              ! Index to DCB entry for array 1
      INTEGER IDCB2              ! Index to DCB entry for array 2
      INTEGER LMRB( ARY__MXDIM ) ! Lower bounds of mapping region
      INTEGER LMTR1( ARY__MXDIM ) ! Lower bounds mapping tr. region 1
      INTEGER LMTR2( ARY__MXDIM ) ! Lower bounds mapping tr. region 2
      INTEGER LX( ARY__MXDIM )   ! Lower bound of intersection region
      INTEGER UMRB( ARY__MXDIM ) ! Upper bounds of mapping region
      INTEGER UMTR1( ARY__MXDIM ) ! Upper bounds mapping tr. region 1
      INTEGER UMTR2( ARY__MXDIM ) ! Upper bounds mapping tr. region 2
      INTEGER UX( ARY__MXDIM )   ! Upper bound of intersection region
      LOGICAL MRFUL1             ! 1st mapping transfer region full?
      LOGICAL MRFUL2             ! 2nd mapping transfer region full?
      LOGICAL MTREX1             ! 1st mapping transfer region exists?
      LOGICAL MTREX2             ! 2nd mapping transfer region exists?
      LOGICAL WHOLE1             ! 1st array mapped whole?
      LOGICAL WHOLE2             ! 2nd array mapped whole?

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the two array identifiers.
      CALL ARY1_IMPID( IARY1, IACB1, STATUS )
      CALL ARY1_IMPID( IARY2, IACB2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain indices to the two data object entries in the DCB.
         IDCB1 = ACB_IDCB( IACB1 )
         IDCB2 = ACB_IDCB( IACB2 )

*  See if the two arrays refer to the same data object.
         SAME = IDCB1 .EQ. IDCB2

*  If not, then they cannot intersect.
         IF ( .NOT. SAME ) THEN
            ISECT = .FALSE.

*  If they refer to the same data object, then calculate their mapping
*  region bounds, as if they were going to be mapped for access.
         ELSE
            CALL ARY1_GMRB( IACB1, MTREX1, MRFUL1, WHOLE1, LMRB, UMRB,
     :                      LMTR1, UMTR1, STATUS )
            CALL ARY1_GMRB( IACB2, MTREX2, MRFUL2, WHOLE2, LMRB, UMRB,
     :                      LMTR2, UMTR2, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If either array does not have a mapping transfer window, then they
*  cannot intersect.
               IF ( .NOT. ( MTREX1 .AND. MTREX2 ) ) THEN
                  ISECT = .FALSE.

*  Otherwise, they intersect if their mapping transfer windows
*  intersect.
               ELSE
                  CALL ARY1_XSBND( ARY__MXDIM, LMTR1, UMTR1,
     :                             ARY__MXDIM, LMTR2, UMTR2,
     :                             ARY__MXDIM, LX, UX, ISECT, STATUS )
               END IF
            END IF
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_SAME_ERR',
     :   'ARY_SAME: Error determining if two array identifiers ' //
     :   'refer to parts of the same base array.', STATUS )
         CALL ARY1_TRACE( 'ARY_SAME', STATUS )
      END IF

      END
