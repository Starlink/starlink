      SUBROUTINE EDI_SUBSET( IFID, SEL, OFID, STATUS )
*+
*  Name:
*     EDI_SUBSET

*  Purpose:
*     Copy lists from input to output object, subsetting using SEL array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_SUBSET( IFID, SEL, OFID, STATUS )

*  Description:
*     Copy lists from input to output object, subsetting the events using
*     the SEL array. It is assumed that the object referenced by OFID has
*     been configured for the reduced number of events to be copied.

*  Arguments:
*     IFID = INTEGER (given)
*        ADI identifier of input object, derived from EventDS
*     SEL[] = BYTE (given)
*        Selector. Set to non-zero if event is to be copied
*     OFID = INTEGER (given)
*        ADI identifier of output object, derived from EventDS
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     The input event table is divided into blocks of BLOCKSIZE events to
*     keep memory usage to reasonable limits.

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     3 Jan 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER			IFID, OFID
      BYTE			SEL(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      INTEGER                 	BLOCKSIZE            	! Maximum # elements
        PARAMETER            	(BLOCKSIZE = 262144 ) 	! per section

*  Local Variables:
      CHARACTER*20  		LNAME    		! Current list name
      CHARACTER*7  		MTYPE                	! Mapping type

      INTEGER                 	ADPTR                	! A vector data array
      INTEGER                 	AQPTR                	! A vector quantum array
      INTEGER                 	BSTART      	   	! Block start index
      INTEGER                 	BEND      	   	! Block end index
      INTEGER                 	BLEN      	   	! Block length
      INTEGER			CBPTR			! Counts per block store
      INTEGER		      	CCOUNT			! Events to copy per block
      INTEGER                 	COPY                 	! Pointer to copy array
      INTEGER                 	EVENTS               	! Number of events in EVDS
      INTEGER                 	I              		! Loop counters
      INTEGER                 	IBLOCK               	! Loop over blocks
      INTEGER			ILID			! Input list id
      INTEGER			LID			! List identifier
      INTEGER			NBLK			! # blocks to copy
      INTEGER                 	NLISTS               	! Number of LISTs
      INTEGER                 	ODPTR                	! Output list data
      INTEGER		      	OFFSET		   	! Output data offset
      INTEGER			OLID			! Output list id
      INTEGER                 	OQPTR                	! Output list quantum

      LOGICAL			QVEC			! Vector quantum?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get number of lists and events in input
      CALL EDI_GETNS( IFID, EVENTS, NLISTS, STATUS )

*  Allocate array to hold number of events to be copied per block
      NBLK = EVENTS/BLOCKSIZE + 1
      CALL DYN_MAPI( 1, NBLK, CBPTR, STATUS )

*  Count number of events to be copied in each block
      BSTART = 1
      DO IBLOCK = 1, NBLK

*    End of the block
        BEND = MIN(BSTART + BLOCKSIZE -1, EVENTS)
        BLEN = BEND - BSTART + 1

*    Count non-zeros
        CCOUNT = 0
        DO I = 1, NIN
          IF ( SEL(I) .NE. 0 ) CCOUNT = CCOUNT + 1
        END DO
        CALL ARR_SELEM1I( CBPTR, NBLK, IBLOCK, CCOUNT, STATUS )

*    Next block
        BSTART = BSTART + BLOCKSIZE

      END DO

*  Loop over input lists
      DO I = 1, NLISTS

*    Locate input list
        CALL EDI_IDX( IFID, I, ILID, STATUS )
        CALL ADI_CGET0C( ILID, 'Name', LNAME, STATUS )

*    Make copy for output
        CALL ADI_COPY( ILID, OLID, STATUS )

*    Decide on mapping type
        CALL EDI_MTYPE( ILID, MTYPE, STATUS )

*    Create the list
        CALL EDI_CREAT( OFID, OLID, STATUS )

*    Loop over blocks
        OFFSET = 0
        BSTART = 1
        DO IBLOCK = 1, EVENTS/BLOCKSIZE + 1

*      End of the block
          BEND = MIN(BSTART + BLOCKSIZE -1, EVENTS)
          BLEN = BEND - BSTART + 1

*      Any selected events in this block?
          CALL ARR_ELEM1I( CBPTR, NBLK, IBLOCK, CCOUNT, STATUS )
          IF ( CCOUNT .EQ. 0 ) GOTO 50

*      Map the input event block
          CALL EDI_MAP( IFID, LNAME, MTYPE, 'READ', BSTART, BEND,
     :                  ADPTR, STATUS )
          CALL EDI_MAP( OFID, LNAME, MTYPE, 'WRITE', OFFSET + 1,
     :                  OFFSET + CCOUNT, ODPTR, STATUS )

*      Does list have vector quantum?
          CALL ADI_THERE( LID, 'Quantum', QVEC, STATUS )
          IF ( QVEC ) THEN
            CALL ADI_CGET0L( LID, 'VectorQuantum', QVEC, STATUS )
          END IF

*      Ditto the quantum
          IF ( QVEC ) THEN
            CALL EDI_QMAP( IFID, LNAME, MTYPE, 'READ', BSTART, BEND,
     :                  AQPTR, STATUS )
            CALL EDI_QMAP( OFID, LNAME, MTYPE, 'WRITE', OFFSET + 1,
     :                  OFFSET + CCOUNT, OQPTR, STATUS )
          END IF

*      Copy the data
          IF ( MTYPE(1:1) .EQ. 'D' ) THEN
            CALL EDI_SUBSET_COPD( BLEN, %VAL(ADPTR), QVEC, %VAL(AQPTR),
     :                  SEL(BSTART), %VAL(ODPTR), %VAL(OQPTR), STATUS )
          ELSE
            CALL EDI_SUBSET_COPI( BLEN, %VAL(ADPTR), QVEC, %VAL(AQPTR),
     :                  SEL(BSTART), %VAL(ODPTR), %VAL(OQPTR), STATUS )
          END IF

*      Unmap the slices
          CALL EDI_UNMAP( IFID, LNAME, STATUS )
          CALL EDI_UNMAP( OFID, LNAME, STATUS )

*      Annul vector quantum slices
          IF ( QVEC ) THEN
            CALL EDI_QUNMAP( IFID, LNAME, STATUS )
            CALL EDI_QUNMAP( OFID, LNAME, STATUS )
          END IF

*      Adjust output pointer
 50       BSTART = BSTART + BLOCKSIZE
          OFFSET = OFFSET + CCOUNT

*    Next block
        END DO

*  Next list
      END DO

*  Release block count storage
      CALL DYN_UNMAP( CBPTR, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_SUBSET', STATUS )

      END



*+  EDI_SUBSET_COPD - Copy DATA & QUANTUM values
      SUBROUTINE EDI_SUBSET_COPD( EVENTS, IN, QOK, QIN, COPY,
     :                            OUT, QOUT, STATUS )
*    Description :
*     Copies from IN to OUT if COPY is true
*    Authors :
*     Phil Andrews
*    History :
*      1-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER        EVENTS

      DOUBLE PRECISION IN(*)
      LOGICAL          QOK
      DOUBLE PRECISION QIN(*)

      BYTE            COPY(*)
*
*    Export :
*
      DOUBLE PRECISION QOUT(*)
      DOUBLE PRECISION OUT(*)
*
*    Local variables :
*
      INTEGER        I, J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      J = 0

      IF ( QOK ) THEN
        DO I = 1, EVENTS
          IF ( COPY(I) .NE. 0 ) THEN
            J = J + 1
            OUT(J) = IN(I)
            QOUT(J) = QIN(J)
          END IF
        END DO
      ELSE
        DO I = 1, EVENTS
          IF ( COPY(I) .NE. 0 ) THEN
            J = J + 1
            OUT(J) = IN(I)
          END IF
        END DO
      END IF

      END



*+  EDI_SUBSET_COPI - Copy DATA & QUANTUM values
      SUBROUTINE EDI_SUBSET_COPI( EVENTS, IN, QOK, QIN, COPY,
     :                            OUT, QOUT, STATUS )
*    Description :
*     Copies from IN to OUT if COPY is true
*    Authors :
*     Phil Andrews
*    History :
*      1-JUN-1989 :  Original  (PLA_AST88@uk.ac.bham.sr.star)
*    Type Definitions :
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER        EVENTS
      LOGICAL        QOK
      INTEGER        IN(*)
      INTEGER        QIN(*)

      BYTE           COPY(*)
*
*    Export :
*
      INTEGER        QOUT(*)
      INTEGER        OUT(*)
*
*    Local variables :
*
      INTEGER        I, J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      J = 0

      IF ( QOK ) THEN
        DO I = 1, EVENTS
          IF ( COPY(I) .NE. 0 ) THEN
            J = J + 1
            OUT(J) = IN(I)
            QOUT(J) = QIN(J)
          END IF
        END DO
      ELSE
        DO I = 1, EVENTS
          IF ( COPY(I) .NE. 0 ) THEN
            J = J + 1
            OUT(J) = IN(I)
          END IF
        END DO
      END IF

      END
