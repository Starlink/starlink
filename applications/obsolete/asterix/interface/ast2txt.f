      SUBROUTINE EXPORT( STATUS )
*+
*  Name:
*     EXPORT

*  Purpose:
*     Write contents of a dataset to ascii, optionally with IMPORT keywords

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL EXPORT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Writes out all the useful axis, data, error, variance and quality
*     values of a dataset to a specified text file. EXPORT also optionally
*     writes a header for use with the IMPORT program.

*  Usage:
*     export {parameter_usage}

*  Environment Parameters:
*     INP = LITERAL (read)
*        The input binned dataset, may be primitive
*     OUT = LITERAL (read)
*        The output text file
*     IMPREAD = LOGICAL (read)
*        Write IMPORT compatible header?

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

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {task_references}...

*  Keywords:
*     export, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      2 Mar 1992 V1.6-0 (DJA):
*        Original version.
*      6 Aug 1992 V1.6-1 (DJA):
*        Fixed bug wiudth scalar axis widths
*     25 Feb 1994 V1.7-0 (DJA):
*        Use BIT_ routines to do bit manipulations
*     24 Nov 1994 V1.8-0 (DJA):
*        Now use USI for user interface
*     15 Jan 1995 V1.8-1 (DJA):
*        New data interfaces
*     12 Sep 1995 V2.0-0 (DJA):
*        Full ADI port.
*      3 Apr 1996 V2.0-1 (DJA):
*        Added output of grouping array
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Constants:
      CHARACTER*30		VERSION
        PARAMETER		( VERSION = 'EXPORT Version 2.2-0' )

*  Local Variables:
      CHARACTER*80              LABUN(2)                ! Label & units
      CHARACTER*80              STRING                  ! Utility string
      CHARACTER*80              TITLE                   ! Dataset title
      CHARACTER*20              TYPE                    ! Input dataset type

      REAL                      AV1, AV2                ! First 2 axis values

      INTEGER                   APTR(ADI__MXDIM)        ! Axis data ptr
      INTEGER                   AWPTR(ADI__MXDIM)       ! Axis width ptr
      INTEGER                   DIMS(ADI__MXDIM)        ! Dimensions
      INTEGER                   DPTR                    ! Ptr to data
      INTEGER                   GPTR                    ! Ptr to grouping
      INTEGER                   IAX                     ! Loop over axes
      INTEGER			IFID			! Input file identifier
      INTEGER                   IMASK                   ! Input quality mask
      INTEGER                   NAX                     ! Number of axes
      INTEGER                   NCHAR                   ! # char used in STRING
      INTEGER                   NDIM                    ! Dimensionality
      INTEGER                   QPTR                    ! Ptr to quality
      INTEGER                   OFD                     ! Output FIO descriptor
      INTEGER                   VPTR                    ! Ptr to variance

      LOGICAL                   AOK(ADI__MXDIM)         ! Axis ok?
      LOGICAL                   ANORM                   ! Axis normalised?
      LOGICAL                   AWOK(ADI__MXDIM)        ! Axis widths ok?
      LOGICAL                   DECREASING              ! Axis values decreasing?
      LOGICAL                   DOK                     ! DATA ok?
      LOGICAL                   EOK                     ! ERRORS ok?
      LOGICAL                   GOK                     ! GROUP ok?
      LOGICAL                   IMPREAD                 ! IMPORT readable
      LOGICAL			OK			! Validity check
      LOGICAL                   QOK                     ! QUALITY ok?
      LOGICAL                   VOK                     ! VARIANCE ok?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Get input dataset
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Get output file
      CALL FIO_ASSOC( 'OUT', 'WRITE', 'LIST', 0, OFD, STATUS )

*  Write IMPORT readable keywords?
      CALL USI_GET0L( 'IMPREAD', IMPREAD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  First the data
      CALL BDI_CHK( IFID, 'Data', DOK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. DOK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No data present in $INP', STATUS )
        GOTO 99
      ELSE
        CALL BDI_MAPR( IFID, 'Data', 'READ', DPTR, STATUS )
      END IF

*  Check QUALITY
      CALL BDI_CHK( IFID, 'Quality', QOK, STATUS )
      IF ( QOK ) THEN
        CALL BDI_GET0I( IFID, 'QualityMask', IMASK, STATUS )
        CALL BDI_MAPUB( IFID, 'Quality', 'READ', QPTR, STATUS )
      END IF

*  Check VARIANCE
      EOK = .FALSE.
      CALL BDI_CHK( IFID, 'Variance', VOK, STATUS )
      IF ( VOK ) THEN
        CALL BDI_MAPR( IFID, 'Variance', 'READ', VPTR, STATUS )
      END IF

*  Check grouping
      CALL BDI_CHK( IFID, 'Grouping', GOK, STATUS )
      IF ( GOK ) THEN
        CALL BDI_MAPI( IFID, 'Grouping', 'READ', GPTR, STATUS )
      END IF

*  Any axes?
      CALL BDI_CHK( IFID, 'Axes', OK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT( 'WARNING : No axes present in input' )
        NAX = 0

      ELSE

*    Get info for each axis
        DO IAX = 1, NDIM

*      Axis data present?
          CALL BDI_AXCHK( IFID, IAX, 'Data', AOK(IAX), STATUS )
          IF ( AOK(IAX) ) THEN

*        Axis data values
            CALL BDI_AXMAPR( IFID, IAX, 'Data', 'READ', APTR(IAX),
     :                       STATUS )

*        Axis widths?
            CALL BDI_AXCHK( IFID, IAX, 'Width', AWOK(IAX), STATUS )
            IF ( AWOK(IAX) ) THEN
              CALL BDI_AXMAPR( IFID, IAX, 'Width', 'READ',
     :                             AWPTR(IAX), STATUS )
            END IF

          END IF
        END DO

      END IF

*  Write IMPORT readable header?
      IF ( IMPREAD ) THEN

*    Construct TOPLEVEL component
        CALL ADI_TYPE( IFID, TYPE, STATUS )
        CALL BDI_GET0C( IFID, 'Title', TITLE, STATUS )
        CALL EXPORT_SET( 'TYPE', TYPE )
        CALL EXPORT_SET( 'TITLE', TITLE )
        CALL EXPORT_WRITE( OFD, 'TOPLEVEL ^TYPE ^TITLE', STATUS )

*    For each dimension
        DO IAX = 1, NDIM

*      Everything ok?
          IF ( AOK(IAX) ) THEN

*        Get axis label and units
            CALL BDI_AXGET0C( IFID, IAX, 'Label,Units', LABUN, STATUS )

*        Put NORM token
            CALL BDI_AXGET0L( IFID, IAX, 'Normalised', ANORM, STATUS )
            IF ( ANORM ) THEN
              CALL MSG_SETC( 'NORM', 'NORM' )
            ELSE
              CALL MSG_SETC( 'NORM', ' ' )
            END IF

*        Put DECREASING token if needed
            DECREASING = .FALSE.
            IF ( DIMS(IAX) .GT. 1 ) THEN
              CALL ARR_ELEM1R( APTR(IAX), DIMS(IAX), 1, AV1, STATUS )
              CALL ARR_ELEM1R( APTR(IAX), DIMS(IAX), 2, AV2, STATUS )
              DECREASING = ( AV2 .LT. AV1 )
            END IF

*        Set the tokens
            CALL EXPORT_SET( 'LABEL', LABUN(1) )
            CALL EXPORT_SET( 'UNITS', LABUN(2) )
            IF ( DECREASING ) THEN
              CALL MSG_SETC( 'DECREASING', 'DECREASING' )
            ELSE
              CALL MSG_SETC( 'DECREASING', ' ' )
            END IF

*        Write the descriptor
            CALL EXPORT_WRITE( OFD, 'AXIS ^LABEL ^UNITS ^NORM'/
     :                         /' ^DECREASING', STATUS )

*        Width descriptor?
            IF ( AWOK(IAX) ) THEN
              CALL EXPORT_WRITE( OFD, 'WIDTH', STATUS )
            END IF

*      Otherwise write dummy axis
          ELSE
            CALL EXPORT_WRITE( OFD, 'AXIS', STATUS )

          END IF

        END DO

*    The data
        CALL BDI_GET0C( IFID, 'Label,Units', LABUN, STATUS )
        CALL EXPORT_SET( 'LABEL', LABUN(1) )
        CALL EXPORT_SET( 'UNITS', LABUN(2) )
        CALL EXPORT_WRITE( OFD, 'DATA ^LABEL ^UNITS', STATUS )

*    Quality
        IF ( QOK ) THEN
          CALL CHR_ITOC( IMASK, STRING, NCHAR )
          CALL EXPORT_SET( 'MASK', STRING(:NCHAR) )
          CALL EXPORT_WRITE( OFD, 'QUALITY ^MASK', STATUS )
        END IF

*    Grouping
        IF ( GOK ) THEN
          CALL EXPORT_WRITE( OFD, 'GROUP', STATUS )
        END IF

*    Asymmetric errors if present
        IF ( EOK ) THEN
          CALL EXPORT_WRITE( OFD, 'LOERROR', STATUS )
          CALL EXPORT_WRITE( OFD, 'UPERROR', STATUS )
        ELSE IF ( VOK ) THEN
          CALL EXPORT_WRITE( OFD, 'VARIANCE', STATUS )
        END IF

      END IF

*  Pad dimensions to 7D
      CALL AR7_PAD( NDIM, DIMS, STATUS )
      DO IAX = NDIM+1, ADI__MXDIM
        AOK(IAX) = .FALSE.
        AWOK(IAX) = .FALSE.
      END DO

*  Write data values
      CALL EXPORT_INT( DIMS(1), DIMS(2), DIMS(3), DIMS(4), DIMS(5),
     :                 DIMS(6), DIMS(7), NDIM,
     :                 AOK, APTR, AWOK, AWPTR,
     :                 DOK, %VAL(DPTR), QOK, %VAL(QPTR), GOK,
     :                 %VAL(GPTR), VOK, %VAL(VPTR), OFD, STATUS )

*  Release input
      CALL USI_CANCL( 'INP', STATUS )

*  Close output file
      CALL FIO_CLOSE( OFD, STATUS )

*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  EXPORT_INT - Write data arrays to output file
      SUBROUTINE EXPORT_INT( L1, L2, L3, L4, L5, L6, L7, NDIM,
     :                       AOK, APTR, AWOK, AWPTR,
     :                       DOK, DATA, QOK, QUAL, GOK, GRP, VOK, VAR,
     :                       FD, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Mar 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Import :
*
      INTEGER                 L1,L2,L3,L4,L5,L6,L7   ! Data dimensions
      INTEGER                 NDIM                   ! Dimensionality
      LOGICAL                 AOK(ADI__MXDIM)        ! Axes present?
      INTEGER                 APTR(ADI__MXDIM)       ! Axis data vals
      LOGICAL                 AWOK(ADI__MXDIM)       ! Axis width present?
      INTEGER                 AWPTR(ADI__MXDIM)      ! Axis width vals
      LOGICAL                 DOK, QOK, GOK, VOK          ! Items there?
      INTEGER		      GRP(L1,L2,L3,L4,      	! Group values
     :                                L5,L6,L7)
      REAL                    DATA(L1,L2,L3,L4,      ! Data values
     :                                L5,L6,L7)
      BYTE                    QUAL(L1,L2,L3,L4,      ! QUALITY values
     :                                L5,L6,L7)
      REAL                    VAR(L1,L2,L3,L4,       ! VARIANCE values
     :                               L5,L6,L7)
      INTEGER                 FD                     ! Output ile descriptor
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*132           LINE                   ! Output buffer

      REAL                    AXV                    ! Axis value
      REAL                    AXW                    ! Axis width value

      INTEGER                 ACB(ADI__MXDIM)        ! Axis column LHS
      INTEGER                 ACE(ADI__MXDIM)        ! Axis column RHS
      INTEGER                 COL                    ! Character position
      INTEGER                 CMAX                   ! Last column used
      INTEGER                 DCOL                   ! DATA column
      INTEGER                 GCOL                   ! GROUP column
      INTEGER                 I,J,K,L,M,N,O          ! Loops over data
      INTEGER                 IAX                    ! Loop over axes
      INTEGER                 QCOL                   ! QUALITY column
      INTEGER                 VCOL                   ! VARIANCE column
      INTEGER                 WCB(ADI__MXDIM)        ! Width column LHS
      INTEGER                 WCE(ADI__MXDIM)        ! Width column RHS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up output columns
      COL = 1
      DO IAX = 1, ADI__MXDIM
        ACB(IAX) = COL
        IF ( AOK(IAX) ) THEN
          COL = COL + 15
        ELSE IF ( IAX .LE. NDIM ) THEN
          COL = COL + 6
        END IF
        ACE(IAX) = COL - 1
        IF ( AWOK(IAX) ) THEN
          WCB(IAX) = COL
          COL = COL + 15
          WCE(IAX) = COL - 1
        END IF
      END DO
      DCOL = COL
      COL = COL + 15
      IF ( QOK ) THEN
        QCOL = COL
        COL = COL + 5
      END IF
      IF ( GOK ) THEN
        GCOL = COL
        COL = COL + 10
      END IF
      IF ( VOK ) THEN
        VCOL = COL
        COL = COL + 15
      END IF
      CMAX = COL

*    Formats
 5    FORMAT( 1PG14.6 )
 10   FORMAT( I6 )

*    Clear buffer
      LINE = ' '

*   Loop over data

*    Loop over dimension 7
      DO O = 1, L7

*      Get dimension 7 axis value
        IF ( AOK(7) ) THEN
          CALL ARR_ELEM1R( APTR(7), L7, O, AXV, STATUS )
          WRITE( LINE(ACB(7):ACE(7)), 5 ) AXV
          IF ( AWOK(7) ) THEN
            CALL ARR_ELEM1R( AWPTR(7), L7, O, AXW, STATUS )
            WRITE( LINE(WCB(7):WCE(7)), 5 ) AXW
          END IF
        ELSE IF ( NDIM .GT. 6 ) THEN
          WRITE( LINE(ACB(7):ACE(7)), 10 ) O
        END IF

*      Loop over dimension 6
        DO N = 1, L6

*        Get dimension 6 axis value
          IF ( AOK(6) ) THEN
            CALL ARR_ELEM1R( APTR(6), L6, N, AXV, STATUS )
            WRITE( LINE(ACB(6):ACE(6)), 5 ) AXV
            IF ( AWOK(6) ) THEN
              CALL ARR_ELEM1R( AWPTR(6), L6, N, AXW, STATUS )
              WRITE( LINE(WCB(6):WCE(6)), 5 ) AXW
            END IF
          ELSE IF ( NDIM .GT. 5 ) THEN
            WRITE( LINE(ACB(6):ACE(6)), 10 ) N
          END IF

*        Loop over dimension 5
          DO M = 1, L5

*          Get dimension 5 axis value
            IF ( AOK(5) ) THEN
              CALL ARR_ELEM1R( APTR(5), L5, M, AXV, STATUS )
              WRITE( LINE(ACB(5):ACE(5)), 5 ) AXV
              IF ( AWOK(5) ) THEN
                CALL ARR_ELEM1R( AWPTR(5), L5, M, AXW, STATUS )
                WRITE( LINE(WCB(5):WCE(5)), 5 ) AXW
              END IF
            ELSE IF ( NDIM .GT. 4 ) THEN
              WRITE( LINE(ACB(5):ACE(5)), 10 ) M
            END IF

*          Loop over dimension 4
            DO L = 1, L4

*            Get dimension 4 axis value
              IF ( AOK(4) ) THEN
                CALL ARR_ELEM1R( APTR(4), L4, L, AXV, STATUS )
                WRITE( LINE(ACB(4):ACE(4)), 5 ) AXV
                IF ( AWOK(4) ) THEN
                  CALL ARR_ELEM1R( AWPTR(4), L4, L, AXW, STATUS )
                  WRITE( LINE(WCB(4):WCE(4)), 5 ) AXW
                END IF
              ELSE IF ( NDIM .GT. 3 ) THEN
                WRITE( LINE(ACB(4):ACE(4)), 10 ) L
              END IF

*            Loop over dimension 3
              DO K = 1, L3

*              Get dimension 3 axis value
                IF ( AOK(3) ) THEN
                  CALL ARR_ELEM1R( APTR(3), L3, K, AXV, STATUS )
                  WRITE( LINE(ACB(3):ACE(3)), 5 ) AXV
                  IF ( AWOK(3) ) THEN
                    CALL ARR_ELEM1R( AWPTR(3), L3, K, AXW, STATUS )
                    WRITE( LINE(WCB(3):WCE(3)), 5 ) AXW
                  END IF
                ELSE IF ( NDIM .GT. 2 ) THEN
                  WRITE( LINE(ACB(3):ACE(3)), 10 ) K
                END IF

*              Loop over dimension 2
                DO J = 1, L2

*                Get dimension 2 axis value
                  IF ( AOK(2) ) THEN
                    CALL ARR_ELEM1R( APTR(2), L2, J, AXV, STATUS )
                    WRITE( LINE(ACB(2):ACE(2)), 5 ) AXV
                    IF ( AWOK(2) ) THEN
                      CALL ARR_ELEM1R( AWPTR(2), L2, J, AXW, STATUS )
                      WRITE( LINE(WCB(2):WCE(2)), 5 ) AXW
                    END IF
                  ELSE IF ( NDIM .GT. 1 ) THEN
                    WRITE( LINE(ACB(2):ACE(2)), 10 ) J
                  END IF

*                Loop over dimension 1
                  DO I = 1, L1

*                  Get dimension 1 axis value
                    IF ( AOK(1) ) THEN
                      CALL ARR_ELEM1R( APTR(1), L1, I, AXV, STATUS )
                      WRITE( LINE(ACB(1):ACE(1)), 5 ) AXV
                      IF ( AWOK(1) ) THEN
                        CALL ARR_ELEM1R( AWPTR(1), L1, I, AXW, STATUS )
                        WRITE( LINE(WCB(1):WCE(1)), 5 ) AXW
                      END IF
                    ELSE
                      WRITE( LINE(ACB(1):ACE(1)), 10 ) I
                    END IF

*                  Write the data value
                    WRITE( LINE(DCOL:DCOL+13), 5 ) DATA(I,J,K,L,M,N,O)

*                  Quality value
                    IF ( QOK ) THEN
                      WRITE( LINE(QCOL:QCOL+3), '(I4)' )
     :                                           QUAL(I,J,K,L,M,N,O)
                    END IF

*                  Group value
                    IF ( GOK ) THEN
                      WRITE( LINE(GCOL:GCOL+8), '(I8)' )
     :                                           GRP(I,J,K,L,M,N,O)
                    END IF

*                  Variance value
                    IF ( VOK ) THEN
                      WRITE( LINE(VCOL:VCOL+13), 5 )
     :                                           VAR(I,J,K,L,M,N,O)
                    END IF

*                  Write the text
                    CALL FIO_WRITE( FD, LINE(:CMAX), STATUS )

                  END DO

                END DO
              END DO
            END DO
          END DO
        END DO
      END DO

      END



*+  EXPORT_SET - Set a named token for output to IMPORT header
      SUBROUTINE EXPORT_SET( TOKEN, TEXT )
*
*    Description :
*
*     Sets an MSG token with a value of the named IMPORT token plus
*     text (in quotes if it includes spaces).
*
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Mar 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      CHARACTER*(*)           TOKEN              ! MSG token to set
      CHARACTER*(*)           TEXT               ! Token text (if any)
*
*    Function declarations :
*
      INTEGER                 CHR_LEN
*
*    Local variables :
*
      CHARACTER*200           LTXT
      INTEGER                 TLEN               ! Non-blank length of TEXT
*-

*    Only set if non-blank
      IF ( TEXT .GT. ' ' ) THEN

*      Find length of data
        TLEN = CHR_LEN( TEXT )

*      Set token and enclose data in double quotes if it has a space in it
        IF ( INDEX(TEXT(:TLEN),' ') .EQ. 0 ) THEN
          LTXT = TOKEN//' '//TEXT(:TLEN)
          CALL MSG_SETC( TOKEN, LTXT )
        ELSE
          LTXT = TOKEN//' "'//TEXT(:TLEN)//'"'
          CALL MSG_SETC( TOKEN, LTXT )
        END IF

      ELSE
        CALL MSG_SETC( TOKEN, ' ' )

      END IF

      END



*+  EXPORT_WRITE - Expand and write a line of text to output file
      SUBROUTINE EXPORT_WRITE( FD, TEXT, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Mar 92 : Original (DJA)
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
      INTEGER                FD              ! Output file descriptor
      CHARACTER*(*)          TEXT            ! Output text
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*500          BUF             ! Output buffer

      INTEGER                BLEN            ! Chars used in BUF
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Expand text
      CALL MSG_MAKE( TEXT, BUF, BLEN )

*  Write text
      CALL FIO_WRITE( FD, BUF(:BLEN), STATUS )

      END
