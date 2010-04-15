      SUBROUTINE POSCA1( NCRDDF, NITEM, WORK1, WORK2, NUSED, STATUS )
*+
*  Name:
*     POSCA1

*  Purpose:
*     Sort the information about the CRDD files.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POSCA1( NCRDDF, NITEM, WORK1, WORK2, NUSED, STATUS )

*  Description:
*     This routines sorts the information stored in the work file first
*     by SOP number, and then by cross scan distance.

*  Arguments:
*     NCRDDF = INTEGER (Given)
*        The number of NDFs in the input group.
*     NITEM = INTEGER (Given)
*        The number of items of information stored about eacg CRDD file
*        in the work space.
*     WORK1( NCRDDF, NITEM ) = DOUBLE PRECISION (Given and Returned)
*        The array holding information about each CRDD file (see routine
*        POSCA0). On return, the entries for each CRDD file are ordered
*        so that crss scan distance increases monotonically within each
*        block.
*     WORK2( NCRDDF, NITEM ) = DOUBLE PRECISION (Given and Returned)
*        Extra work space.
*     NUSED = INTEGER (Returned)
*        The number of entries in the work array holding usable
*        information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER NCRDDF
      INTEGER NITEM

*  Arguments Given and Returned:
      DOUBLE PRECISION WORK1( NCRDDF, NITEM )
      DOUBLE PRECISION WORK2( NCRDDF, NITEM )

*  Arguments Returned:
      INTEGER NUSED

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INDEX              ! Scan index within work array.
      INTEGER I                  ! Loop count.
      INTEGER ITEM               ! Item index within work arrays.
      INTEGER LSOP               ! Previous SOP number.
      INTEGER NSCANS             ! No. of scans in current block.
      INTEGER START              ! Index of first scan in current block
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      START = 0

*  Sort the data so that SOP numbers monotonically from low indices to
*  high indices.
      CALL IRM_SORTD( .TRUE., 7, NCRDDF, NITEM, WORK1, NUSED, STATUS )

*  Set the SOP of the "previous" NDF to an impossible value (-1)
      LSOP = -1

* Set the number of NDFs recorded in WORK2, to zero.
      NSCANS = 0

*  Loop round each NDF, looking for blocks of NDFs with the same SOP
*  number. Such blocks are internally sorted by cross scan distance. The
*  information for all NDFs in each of these blocks is stored in the
*  second work array, WORK2.
      DO INDEX = 1, NUSED

*  If the SOP for this NDF is different to the previous NDF, then the
*  block is complete.
         IF( NINT( WORK1( INDEX, 7 ) ) .NE. LSOP ) THEN

*  If the number of NDFs recored in WORK2 is greater than 1, sort them
*  according to cross scan distance.
            IF( NSCANS .GT. 1 ) THEN

*  First, fill the un-used part of WORK2 with bad OBS values.
               DO I = NSCANS + 1, NCRDDF
                  WORK2( I, 2 ) = VAL__BADD
               END DO

*  Now sort them.
               CALL IRM_SORTD( .TRUE., 2, NCRDDF, NITEM, WORK2, NSCANS,
     :                         STATUS )

*  Copy the sorted values back to WORK1.
               DO I = 1, NSCANS
                  DO ITEM = 1, NITEM
                     WORK1( I - 1 + START, ITEM ) = WORK2( I, ITEM )
                  END DO
               END DO

            END IF

*  Reset the number of scans in the current block to zero, and the index
*  of the first NDF to the current index.
            NSCANS = 0
            START = INDEX

*  Save the current SOP number.
            LSOP = NINT( WORK1( INDEX, 7 ) )

         END IF

*  Increment the number of NDFs recorded in WORK2.
         NSCANS = NSCANS + 1

*  Record the data for this NDF in WORK2.
         DO ITEM = 1, NITEM
            WORK2( NSCANS, ITEM ) = WORK1( INDEX, ITEM )
         END DO

      END DO

*  Finish

      END
