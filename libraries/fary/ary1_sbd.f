      SUBROUTINE ARY1_SBD( BAD, IACB, STATUS )
*+
*  Name:
*     ARY1_SBD

*  Purpose:
*     Set the bad pixel flag for an array with an entry in the ACB.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_SBD( BAD, IACB, STATUS )

*  Description:
*     The routine sets a bad pixel flag value for an entry in the ACB.
*     A call to this routine constitutes a declaration about the
*     presence (or otherwise) of "bad" values in an array and, hence,
*     their presence (or otherwise) in the data region with which the
*     array is associated via its mapping transfer region. The routine
*     starts by modifying the specified ACB bad pixel flag entry. Then,
*     if appropriate, it updates the actual data object bad pixel flag.
*     Finally, it checks other ACB entries which refer to the same data
*     object and modifies their bad pixel flags if this proves
*     necessary as a result of overlapping mapping transfer regions.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Bad pixel flag value to be set.
*     IACB = INTEGER (Given)
*        Index to the ACB entry for the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Obtain an index to the data object entry in the DCB and ensure
*     that state information is available for it.
*     -  If the data object is in an undefined state, then there is
*     nothing to do.
*     -  Otherwise, set the bad pixel flag for the specified ACB entry.
*     -  If the ACB entry has a data transfer window, then obtain
*     mapping region bounds information for it.
*     -  If the mapping transfer region exists, then update the actual
*     data object bad pixel flag if appropriate (taking account of the
*     flag value and the extent of the mapping transfer region).
*     -  Search for other ACB entries which refer to the same data
*     object and also have a data transfer window.
*     -  If such entries are found, then obtain mapping region bounds
*     information for them.
*     -  If the bad pixel flag is being set .TRUE., then set this value
*     for any ACB entry which is associated with the same data object
*     and whose mapping transfer region overlaps with the initial one.
*     -  If the bad pixel flag is being set .FALSE., then set this
*     value for any ACB entry which is associated with the same data
*     object and whose mapping transfer region lies entirely within the
*     initial one.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUL-1989 (RFWS):
*        Original version.
*     27-JUL-1989 (RFWS):
*        Substantial re-write to use mapping transfer regions instead of
*        data transfer windows to test for array intersection, etc.
*     7-SEP-1989 (RFWS):
*        Changed to use new argument list for ARY1_GMRB.
*     5-MAR-1990 (RFWS):
*        Added code to handle the case where a bad pixel flag value is
*        being set for a data object whose state is undefined.
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
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        DCB_STA( ARY__MXDCB ) = LOGICAL (Read)
*           Whether the data object's values are defined.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_BAD( ARY_MXACB ) = LOGICAL (Write)
*           Whether there may be "bad" values in the data transfer
*           window (if it exists) associated with each ACB entry.
*        ACB_DTWEX( ARY__MXACB ) = LOGICAL (Read)
*           Whether the data transfer window exists.
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to the data object entry in the DCB.

*  Arguments Given:
      LOGICAL BAD
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACBT              ! Index to test entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDCBT              ! Index to test entry in the DCB
      INTEGER LBAD( ARY__MXDIM ) ! Lower bounds of "bad pixel data"
      INTEGER LMRB( ARY__MXDIM ) ! Lower mapping region bounds
      INTEGER LTEST( ARY__MXDIM ) ! Lower bounds of test region
      INTEGER LX( ARY__MXDIM )   ! Lower bounds of intersection region
      INTEGER NEXT               ! Next ACB slot which is in use
      INTEGER UBAD( ARY__MXDIM ) ! Upper bounds of "bad pixel data"
      INTEGER UMRB( ARY__MXDIM ) ! Upper mapping region bounds
      INTEGER UTEST( ARY__MXDIM ) ! Upper bounds of test region
      INTEGER UX( ARY__MXDIM )   ! Upper bounds of intersection region
      LOGICAL INSECT             ! Whether bounds intersect
      LOGICAL INSIDE             ! Whether bounds lie inside
      LOGICAL MRFULL             ! Mapping region filled by data?
      LOGICAL MTREX              ! Mapping transfer region exists?
      LOGICAL WHOLE              ! Whole data object to be mapped?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB and ensure that
*  state information is available for it.
      IDCB = ACB_IDCB( IACB )
      CALL ARY1_DSTA( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  There is nothing to do if the data object is in an undefined state;
*  its bad pixel flag is then always .TRUE. and cannot be over-ridden.
*  If it is in a defined state, then set the bad pixel flag in the ACB
*  appropriately.
         IF ( DCB_STA( IDCB ) ) THEN
            ACB_BAD( IACB ) = BAD

*  If the ACB entry has a data transfer window associated with it, then
*  obtain mapping region bounds information for it, as if it were going
*  to be mapped for access. The resulting mapping transfer region
*  indicates the region of actual data to which the array has access.
            IF ( ACB_DTWEX( IACB ) ) THEN
               MTREX = .FALSE.
               CALL ARY1_GMRB( IACB, MTREX, MRFULL, WHOLE,
     :                         LMRB, UMRB, LBAD, UBAD, STATUS )

*  If the mapping transfer region exists, then the the array has access
*  to actual data, so the bad pixel flag for the data object may need
*  to be changed.
               IF ( ( STATUS .EQ. SAI__OK ) .AND. MTREX ) THEN

*  If the bad pixel flag is .TRUE., then there may be "bad" values in
*  the mapping transfer region, so set the data object flag accordingly.
                  IF ( BAD ) THEN
                     CALL ARY1_DSBD( .TRUE., IDCB, STATUS )

*  If the bad pixel flag is .FALSE., then there are no "bad" values in
*  the mapping transfer region, but the data object flag can only be
*  set accordingly if this comprises the whole data object.
                  ELSE
                     IF ( WHOLE ) CALL ARY1_DSBD( .FALSE., IDCB,
     :                                            STATUS )
                  END IF

*  The effect on other ACB entries, whose mapping transfer regions may
*  overlap must now be considered. Loop to consider all other ACB
*  entries which might be affected.
                  IACBT = 0
                  NEXT = 0
1                 CONTINUE       ! Start of 'DO WHILE' loop
                  CALL ARY1_NXTSL( ARY__ACB, IACBT, NEXT, STATUS )
                  IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                 ( NEXT .NE. 0 ) ) THEN
                     IACBT = NEXT

*  Select other ACB entries which refer to the same data object and
*  have a data transfer window associated with them.
                     IDCBT = ACB_IDCB( IACBT )
                     IF ( ( IDCBT .EQ. IDCB ) .AND.
     :                    ( IACBT .NE. IACB ) .AND.
     :                    ACB_DTWEX( IACBT ) ) THEN

*  Calculate mapping region bounds information for each ACB entry being
*  tested.
                        MTREX = .FALSE.
                        CALL ARY1_GMRB( IACBT, MTREX, MRFULL, WHOLE,
     :                                  LMRB, UMRB, LTEST, UTEST,
     :                                  STATUS ) 

*  The entry can only be affected if its mapping transfer region
*  exists; otherwise it has no actual data associated with it.
                        IF ( ( STATUS .EQ. SAI__OK ) .AND. MTREX ) THEN

*  If the bad pixel flag is .TRUE., then data associated with the ACB
*  entry being tested may contain "bad" values if its mapping transfer
*  region intersects with that of the initial ACB entry. Test if this
*  is so, and set its flag accordingly.
                           IF ( BAD ) THEN
                              INSECT = .FALSE.
                              CALL ARY1_XSBND( ARY__MXDIM, LBAD, UBAD,
     :                                         ARY__MXDIM, LTEST, UTEST,
     :                                         ARY__MXDIM, LX, UX,
     :                                         INSECT, STATUS )
                              IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                             INSECT ) THEN
                                 ACB_BAD( IACBT ) = .TRUE.
                              END IF

*  If the bad pixel flag is .FALSE., then we can only be sure there are
*  no "bad" values in the data associated with the ACB entry being
*  tested if its mapping transfer region lies entirely inside that of
*  the initial ACB entry. Test if this is so, and set its flag
*  accordingly.
                           ELSE
                              INSIDE = .FALSE.
                              CALL ARY1_INBND( ARY__MXDIM, LBAD, UBAD,
     :                                         ARY__MXDIM, LTEST, UTEST,
     :                                         INSIDE, STATUS )
                              IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :                             INSIDE ) THEN
                                 ACB_BAD( IACBT ) = .FALSE.
                              END IF
                           END IF
                        END IF
                     END IF
                     GO TO 1     ! End of 'DO WHILE' loop
                  END IF
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_SBD', STATUS )

      END
