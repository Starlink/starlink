      SUBROUTINE TCI_MERGE( NMER, MERID, OUTID, TBASE, TOFFS, STATUS )
*+
*  Name:
*     TCI_MERGE

*  Purpose:
*     Merges timing information from one timing structure into another

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI_MERGE( NMER, MERID, OUTID, TBASE, TOFFS, STATUS )

*  Description:
*     Merging timing description consists of the following steps,
*
*     1) Merge the observation start times. The earliest is chosen
*     2) Merge exposure times. These are simply added
*     3) Merge the observation lengths. The merged observation length
*        is the sum of the inputs
*     4) Merge the live time slots

*  Arguments:
*     NMER = INTEGER (given)
*        Number of timing structures to merge
*     MERID[] = INTEGER (given)
*        ADI identifiers of the timing structures to merge
*     OUTID = INTEGER (returned)
*        ADI identifier of the timing structure to merge into
*     TBASE = INTEGER (returned)
*        Number of input which formed reference time
*     TOFFS[] = DOUBLE PRECISION (returned)
*        The timing offset of each input wrt the new timing origin (secs)
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
*     {algorithm_description}...

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
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/tci.html

*  Keywords:
*     package:tci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Sep 1995 (DJA):
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
      INTEGER			NMER, MERID(*)

*  Arguments Returned:
      INTEGER			TBASE, OUTID
      DOUBLE PRECISION		TOFFS(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			TCI_MERGE_BADLIVE
        LOGICAL			TCI_MERGE_BADLIVE

*  Local Variables:
      DOUBLE PRECISION		BASE_TAI		! Base TAI of all i/ps
      DOUBLE PRECISION	        DVAL			! Value from input id
      DOUBLE PRECISION		MJD			! Base date
      DOUBLE PRECISION		OBL			! Max obs length
      DOUBLE PRECISION		TAI			! Base atomic time
      DOUBLE PRECISION		TEFF			! Effective exposure
      DOUBLE PRECISION		TEXP			! Exposure time
      DOUBLE PRECISION		TOFFSET			! Offset for live time

      INTEGER			I			! Loop over datasets
      INTEGER			ILEN			! Input live time len
      INTEGER			IONPTR, IOFPTR, IDPTR	! Input live time data
      INTEGER			IMJD			! Index of earliest MJD
      INTEGER			ITAI			! Index of earliest TAI
      INTEGER			IVAL			! Value from input
      INTEGER			NLSLOT			! Max # o/p live slots
      INTEGER			ONPTR, OFPTR, ODPTR	! Live time workspace
      INTEGER			ONLSLOT			! Actual # o/p live slots
      INTEGER			START			! Copy origin

      LOGICAL			BADLIVE			! Found duff live times
      LOGICAL			MERDUR			! Merge live time dur'n
      LOGICAL			MEREFF			! Merge eff exposure
      LOGICAL			MEREXP			! Merge exposure time
      LOGICAL			MERLIV			! Merge live times
      LOGICAL			MERMJD			! Merge base dates
      LOGICAL			MEROBL			! Merge obs length
      LOGICAL			MERTAI			! Merge base TAIs
      LOGICAL			OK			! Validity check
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      BASE_TAI = 0D0
      MERDUR = .TRUE.
      MEREXP = .TRUE.
      MEREFF = .TRUE.
      MERLIV = .TRUE.
      MERMJD = .TRUE.
      MEROBL = .TRUE.
      MERTAI = .TRUE.
      TEXP = 0D0
      TEFF = 0D0
      MJD = VAL__MAXD
      TAI = VAL__MAXD
      OBL = 0D0
      NLSLOT = 0

*  Scan inputs checking to see which stuff we'll merge
      DO I = 1, NMER

*    Simple exposure time
        CALL ADI_THERE( MERID(I), 'Exposure', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0D( MERID(I), 'Exposure', DVAL, STATUS )
          TEXP = TEXP + DVAL
        ELSE
          MEREXP = .FALSE.
        END IF

*    Effective exposure time
        CALL ADI_THERE( MERID(I), 'EffExposure', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0D( MERID(I), 'EffExposure', DVAL, STATUS )
          TEFF = TEFF + DVAL
        ELSE
          MEREFF = .FALSE.
        END IF

*    Observation length
        CALL ADI_THERE( MERID(I), 'ObsLength', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0D( MERID(I), 'ObsLength', DVAL, STATUS )
          OBL = OBL + DVAL
        ELSE
          MEROBL = .FALSE.
        END IF

*    Base MJD
        CALL ADI_THERE( MERID(I), 'MJDObs', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0D( MERID(I), 'MJDObs', DVAL, STATUS )
          IF ( DVAL .LT. MJD ) THEN
            MJD = DVAL
            IMJD = I
          END IF
        ELSE
          MERMJD = .FALSE.
        END IF

*    Base TAI
        CALL ADI_THERE( MERID(I), 'TAIObs', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CGET0D( MERID(I), 'TAIObs', DVAL, STATUS )
          IF ( DVAL .LT. TAI ) THEN
            TAI = DVAL
            ITAI = I
          END IF
        ELSE
          MERTAI = .FALSE.
        END IF

*    Live times
        CALL ADI_THERE( MERID(I), 'LiveOn', OK, STATUS )
        IF ( OK ) THEN
          CALL ADI_CSIZE( MERID(I), 'LiveOn', IVAL, STATUS )
          NLSLOT = NLSLOT + IVAL
        ELSE
          MERLIV = .FALSE.
        END IF
        CALL ADI_THERE( MERID(I), 'LiveDur', OK, STATUS )
        IF ( .NOT. OK ) THEN
          MERDUR = .FALSE.
        END IF

      END DO

*  Create new output structure
      CALL ADI_NEW0( 'TimingInfo', OUTID, STATUS )

*  Are absolute times defined?
      IF ( MERMJD .OR. MERTAI ) THEN

*    Consistency check
        IF ( MERMJD .AND. MERTAI ) THEN
          IF ( IMJD .NE. ITAI ) THEN
            CALL MSG_PRNT( 'WARNING : Inconsistent timing data in '/
     :                        /'inputs, ignoring atomic time data' )
            MERTAI = .FALSE.
          END IF
        ELSE
          ITAI = IMJD
        END IF

*    Write timing origin
        IF ( MERMJD ) THEN
          CALL ADI_CPUT0D( OUTID, 'MJDObs', MJD, STATUS )
        END IF
        IF ( MERTAI ) THEN
          CALL ADI_CPUT0D( OUTID, 'TAIObs', TAI, STATUS )
        ELSE IF ( MERMJD ) THEN
          CALL TCI_MJD2TAI( MJD, TAI )
          CALL ADI_CPUT0D( OUTID, 'TAIObs', TAI, STATUS )
        END IF

*    Calculate timing offsets
        TBASE = ITAI
        BASE_TAI = TAI
        DO I = 1, NMER

*      Get TAI of observation
          IF ( MERMJD ) THEN
            CALL ADI_CGET0D( MERID(I), 'MJDObs', DVAL, STATUS )
            CALL TCI_MJD2TAI( DVAL, TAI )
          ELSE
            CALL ADI_CGET0D( MERID(I), 'TAIObs', TAI, STATUS )
          END IF

*      Store offset
          TOFFS(I) = (TAI - BASE_TAI)*86400D0

        END DO

      ELSE

*    Warning
        IF ( NMER .GT. 1 ) THEN
          CALL MSG_PRNT( 'No absolute time frame available - assuming '/
     :                   /'inputs have same timing origin' )
        END IF

*    Default offsets
        DO I = 1, NMER
          TOFFS(I) = 0D0
        END DO

      END IF

*  Write exposure times
      IF ( MEROBL ) THEN
        CALL ADI_CPUT0D( OUTID, 'ObsLength', OBL, STATUS )
      END IF
      IF ( MEREXP ) THEN
        CALL ADI_CPUT0D( OUTID, 'Exposure', TEXP, STATUS )
      END IF
      IF ( MEREFF ) THEN
        CALL ADI_CPUT0D( OUTID, 'EffExposure', TEFF, STATUS )
      END IF

*  Merge live times
      IF ( MERLIV ) THEN

*    Get workspace for maximum possible number of output slots
        CALL DYN_MAPD( 1, NLSLOT, ONPTR, STATUS )
        CALL DYN_MAPD( 1, NLSLOT, OFPTR, STATUS )
        IF ( MERDUR ) THEN
          CALL DYN_MAPD( 1, NLSLOT, ODPTR, STATUS )
        END IF

*    Loop over inputs
        START = 0
        BADLIVE = .FALSE.
        DO I = 1, NMER

*      Length of this dataset's live time lists
          CALL ADI_CSIZE( MERID(I), 'LiveOn', ILEN, STATUS )

*      Map input live time data
          CALL ADI_CMAPD( MERID(I), 'LiveOn', 'READ', IONPTR, STATUS )
          CALL ADI_CMAPD( MERID(I), 'LiveOff', 'READ', IOFPTR, STATUS )

*        Convert to absolute TAI if ON/OFF and DURATION units are consistent
          TOFFSET = BASE_TAI + TOFFS(I)
          IF ( MERDUR ) THEN

            CALL ADI_CMAPD( MERID(I), 'LiveDur', 'READ', IDPTR, STATUS )

            IF ( I .EQ. 1 ) THEN
              IF ( TCI_MERGE_BADLIVE( %VAL(IONPTR), %VAL(IOFPTR),
     :                                      %VAL(IDPTR) ) ) THEN
                BADLIVE = .TRUE.
                CALL MSG_PRNT( 'Live time ON and OFF units are'/
     :            /' inconsistent with DURATION values in dataset 1' )
                TOFFSET = 0.0D0
              END IF
            END IF
          END IF

          CALL TCI_MERGE_COPYD( START, ILEN, .FALSE., %VAL(IONPTR),
     :                            TOFFSET, %VAL(ONPTR), STATUS )
          CALL TCI_MERGE_COPYD( START, ILEN, .FALSE., %VAL(IOFPTR),
     :                            TOFFSET, %VAL(OFPTR), STATUS )

          IF ( MERDUR ) THEN
            CALL TCI_MERGE_COPYD( START, ILEN, .FALSE., %VAL(IDPTR),
     :                                     0.0, %VAL(ODPTR), STATUS )
          END IF
          START = START + ILEN

*      Release input data
          CALL ADI_CUNMAP( MERID(I),  'LiveOn', IONPTR, STATUS )
          CALL ADI_CUNMAP( MERID(I),  'LiveOn', IOFPTR, STATUS )
          IF ( MERDUR ) THEN
            CALL ADI_CUNMAP( MERID(I),  'LiveDur', IDPTR, STATUS )
          END IF

        END DO

*    Sort live time components into increasing order of the ON times
*    and subtract off the BASE_TAI of the output dataset
        IF ( BADLIVE ) THEN
          CALL TCI_MERGE_SORT( NLSLOT, MERDUR, %VAL(ONPTR), %VAL(OFPTR),
     :                           %VAL(ODPTR), 0.0D0, ONLSLOT, STATUS )
        ELSE
          CALL TCI_MERGE_SORT( NLSLOT, MERDUR, %VAL(ONPTR), %VAL(OFPTR),
     :                 %VAL(ODPTR), BASE_TAI + TOFFS(TBASE), ONLSLOT,
     :                 STATUS )
        END IF

*    Write output live times
        CALL ADI_CPUT1D( OUTID, 'LiveOn', ONLSLOT, %VAL(ONPTR), STATUS )
        CALL ADI_CPUT1D( OUTID, 'LiveOff', ONLSLOT, %VAL(OFPTR),
     :                   STATUS )
        IF ( MERDUR ) THEN
          CALL ADI_CPUT1D( OUTID, 'LiveDur', ONLSLOT, %VAL(ODPTR),
     :                     STATUS )
        END IF

*    Release workspace
        CALL DYN_UNMAP( ONPTR, STATUS )
        CALL DYN_UNMAP( OFPTR, STATUS )
        IF ( MERDUR ) THEN
          CALL DYN_UNMAP( ODPTR, STATUS )
        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI_MERGE', STATUS )

      END



*+  TCI_MERGE_COPYD - Copy from one real vector to another with START & OFFSET.
      SUBROUTINE TCI_MERGE_COPYD( START, LENGTH, USEOFFSET, IN,
     :                                    OFFSET, OUT, STATUS )
*    Description :
*
*     IN is copied to OUT starting at START, with OFFSET added to the values
*
*    History :
*
*     12 Oct 88 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                START                    ! Index value for start of copy
      INTEGER                LENGTH                   ! Length of IN array

      LOGICAL                USEOFFSET

      DOUBLE PRECISION       IN(LENGTH)               ! Array to be copied
      DOUBLE PRECISION       OFFSET                   ! Value to add to IN
*
*    Import-Export :
*
      DOUBLE PRECISION       OUT(*)                   ! Array to be written
*
*    Status :
*
      INTEGER                STATUS
*
*    Local variables :
*
      INTEGER                I                        ! Loop counter
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( USEOFFSET ) THEN
        DO I = 1, LENGTH
          OUT(START+I) = IN(I) + OFFSET
        END DO
      ELSE
        DO I = 1, LENGTH
          OUT(START+I) = IN(I)
        END DO
      END IF

      END



*+  TCI_MERGE_SORT - Sorts LIVE_TIME components using ON values
      SUBROUTINE TCI_MERGE_SORT( N, DUROK, ON, OFF, DUR, BASE_TAI,
     :                                             NDIFF, STATUS )
*
*    Method  :
*
*     After the sort, identical live time slots are removed.
*
*    Author :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     19 Feb 91 : Original
*
*    Type declarations :
*
      IMPLICIT NONE
*
      INTEGER	       STATUS		      ! Run-time error code
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER          N                      ! Number of data points
      LOGICAL          DUROK                  ! Duration OK?
      DOUBLE PRECISION BASE_TAI               ! New base time
*
*    Import/Export :
*
      DOUBLE PRECISION ON(N), OFF(N), DUR(N)  ! The data to sort
*
*    Export :
*
      INTEGER          NDIFF                  ! Number of different live time
*
*    Local variables :
*
      DOUBLE PRECISION SWAPON,SWAPOFF,SWAPDUR ! Temporary data values

      INTEGER          I,J,L,IR               !

      LOGICAL          SAME                   ! Slots the same?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      NDIFF = 1
      L = N/2+1
      IR = N
      IF ( N .EQ. 1 ) GOTO 50

*    Sort into ascending time order
 10   CONTINUE
         IF ( L .GT. 1 ) THEN
            L = L - 1
            SWAPON = ON(L)
            SWAPOFF = OFF(L)
            IF ( DUROK ) SWAPDUR = DUR(L)

         ELSE
            SWAPON = ON(IR)
            SWAPOFF = OFF(IR)
            ON(IR) = ON(1)
            OFF(IR) = OFF(1)
            IF ( DUROK ) THEN
               SWAPDUR = DUR(IR)
               DUR(IR) = DUR(1)
            END IF
            IR = IR - 1
            IF ( IR .EQ. 1 ) THEN
               ON(1) = SWAPON
               OFF(1) = SWAPOFF
               IF ( DUROK ) DUR(1) = SWAPDUR
               GOTO 30
            END IF
         END IF
         I = L
         J = L + L
 20      IF ( J .LE. IR ) THEN
            IF ( J .LT. IR ) THEN
               IF ( ON(J) .LT. ON(J+1) ) J = J + 1
            END IF
            IF ( SWAPON .LT. ON(J)) THEN
               ON(I) = ON(J)
               OFF(I) = OFF(J)
               IF ( DUROK ) DUR(I) = DUR(J)
               I = J
               J = J + J
            ELSE
               J = IR + 1
            END IF
            GOTO 20
         END IF
         ON(I) = SWAPON
         OFF(I) = SWAPOFF
         IF ( DUROK ) DUR(I) = SWAPDUR
      GOTO 10

*    Remove identical slots
 30   NDIFF = 1
      DO I = 2, N

*       Compare with last slot
         IF ( DUROK ) THEN
            SAME = ( ON(I) .EQ. ON(NDIFF) ) .AND.
     :             ( OFF(I) .EQ. OFF(NDIFF) ) .AND.
     :             ( DUR(I) .EQ. DUR(NDIFF) )
         ELSE
            SAME = ( ON(I) .EQ. ON(NDIFF) ) .AND.
     :                        ( OFF(I) .EQ. OFF(NDIFF) )
         END IF

*       If different store the slot
         IF ( .NOT. SAME ) THEN
            NDIFF = NDIFF + 1
            IF ( I .NE. NDIFF ) THEN
               ON(NDIFF) = ON(I)
               OFF(NDIFF) = OFF(I)
               IF ( DUROK ) DUR(NDIFF) = DUR(I)
            END IF
         END IF

      END DO

*    Adjust times to offsets from BASE_TAI
 50   DO I = 1, NDIFF
        ON(I) = ON(I) - BASE_TAI
        OFF(I) = OFF(I) - BASE_TAI
      END DO

      END



*+  TCI_MERGE_BADLIVE - Are live time units up the creek
      LOGICAL FUNCTION TCI_MERGE_BADLIVE( ON, OFF, DUR )
*    Description :
*
*     If the duration as derived by OFF-ON is within 1% of DUR corrected
*     to days, then the ON and OFF times are in days and therefore suspect.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     21 Feb 91 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      DOUBLE PRECISION         ON,OFF,DUR
*-

      TCI_MERGE_BADLIVE = ( ABS((OFF-ON)*86400.0D0/DUR-1.0) .LT. 0.01 )

      END
