*+  EXPORT - Write contents of a dataset, optionall with IMPORT keywords
      SUBROUTINE EXPORT( STATUS )
*
*    Description :
*
*     Writes out all the useful axis, data, error, variance and quality
*     values of a dataset to a specified text file. EXPORT also optionally
*     writes a header for use with the IMPORT program.
*
*    Environment parameters :
*
*     INP = UNIV(R)
*       The input binned dataset, may be primitive
*     OUT = FILE(W)
*       The output text file
*     IMPREAD = LOGICAL(R)
*       Write IMPORT compatible header?
*
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      2 Mar 92 : V1.6-0  Original (DJA)
*      6 Aug 92 : V1.6-1  Fixed bug wiudth scalar axis widths (DJA)
*     25 Feb 94 : V1.7-0  Use BIT_ routines to do bit manipulations (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*80            LABEL                  ! Data label
      CHARACTER*(DAT__SZLOC)  ILOC                   ! Input dataset
      CHARACTER*80            STRING                 ! Utility string
      CHARACTER*80            TITLE                  ! Dataset title
      CHARACTER*(DAT__SZTYP)  TYPE                   ! Input dataset type
      CHARACTER*80            UNITS                  ! Data units

      REAL                    ABASE, ASCALE          ! Regular axis values
      REAL                    AWIDTH(DAT__MXDIM)     ! Axis width values if reg

      INTEGER                 APTR(DAT__MXDIM)       ! Axis data ptr
      INTEGER                 AWPTR(DAT__MXDIM)      ! Axis width ptr
      INTEGER                 DIMS(DAT__MXDIM)       ! Dimensions
      INTEGER                 DPTR                   ! Ptr to data
      INTEGER                 IAX                    ! Loop over axes
      INTEGER                 IMASK                  ! Input quality mask
      INTEGER                 NAX                    ! Number of axes
      INTEGER                 NCHAR                  ! # char used in STRING
      INTEGER                 NDIM                   ! Dimensionality
      INTEGER                 NVAL                   ! Axis dimension
      INTEGER                 QDIMS(DAT__MXDIM)      ! QUALITY dimensions
      INTEGER                 QNDIM                  ! QUALITY dimensionality
      INTEGER                 QPTR                   ! Ptr to quality
      INTEGER                 OFD                    ! Output FIO descriptor
      INTEGER                 VDIMS(DAT__MXDIM)      ! VARIANCE dimensions
      INTEGER                 VNDIM                  ! VARIANCE dimensionality
      INTEGER                 VPTR                   ! Ptr to variance

      BYTE                    QMASK                  ! Input quality mask

      LOGICAL                 AOK(DAT__MXDIM)        ! Axis ok?
      LOGICAL                 ANORM                  ! Axis normalised?
      LOGICAL                 AREG(DAT__MXDIM)       ! Axis regular?
      LOGICAL                 AWOK(DAT__MXDIM)       ! Axis widths ok?
      LOGICAL                 AWUNIF(DAT__MXDIM)     ! Axis widths uniform?
      LOGICAL                 DECREASING             ! Axis values decreasing?
      LOGICAL                 DOK                    ! DATA ok?
      LOGICAL                 EOK                    ! ERRORS ok?
      LOGICAL                 IMPREAD                ! IMPORT readable
      LOGICAL                 INPRIM                 ! Input is primitive?
      LOGICAL                 QOK                    ! QUALITY ok?
      LOGICAL                 VOK                    ! VARIANCE ok?
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER (VERSION = 'EXPORT Version 1.7-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version id
      CALL MSG_PRNT( VERSION )

*    Start up ASTERIX
      CALL AST_INIT()

*    Get input object
      CALL USI_ASSOCI( 'INP', 'READ', ILOC, INPRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL DAT_TYPE( ILOC, TYPE, STATUS )

*    Get output filename
      CALL FIO_ASSOC( 'OUT', 'WRITE', 'LIST', 0, OFD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write IMPORT readable keywords?
      CALL PAR_GET0L( 'IMPREAD', IMPREAD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Find out about dataset

*    First the data
      CALL BDA_CHKDATA( ILOC, DOK, NDIM, DIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( .NOT. DOK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No data present in $INP', STATUS )
        GOTO 99
      ELSE
        CALL BDA_MAPDATA( ILOC, 'READ', DPTR, STATUS )
      END IF

*    Other data items
      IF ( INPRIM ) THEN
        NAX = 0
        QOK = .FALSE.
        EOK = .FALSE.
        VOK = .FALSE.

      ELSE

        EOK = .FALSE.

*      Check QUALITY
        CALL BDA_CHKQUAL( ILOC, QOK, QNDIM, QDIMS, STATUS )
        IF ( QOK ) THEN
          CALL BDA_GETMASK( ILOC, QMASK, STATUS )
          CALL BDA_MAPQUAL( ILOC, 'READ', QPTR, STATUS )
        END IF

*      Check VARIANCE
        CALL BDA_CHKVAR( ILOC, VOK, VNDIM, VDIMS, STATUS )
        IF ( VOK ) THEN
          CALL BDA_MAPVAR( ILOC, 'READ', VPTR, STATUS )
        END IF

*      Any axes?
        CALL BDA_CHKAXES( ILOC, NAX, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
        IF ( NAX .EQ. 0 ) THEN
          CALL MSG_PRNT( 'WARNING : No axes present in structured'/
     :                                                /' dataset' )
        ELSE

*        Get info for each
          DO IAX = 1, NAX
            CALL BDA_CHKAXIS( ILOC, IAX, AOK(IAX), STATUS )
            IF ( AOK(IAX) ) THEN

*            Axis data values
              CALL BDA_CHKAXVAL( ILOC, IAX, AOK(IAX), AREG(IAX),
     :                                           NVAL, STATUS )
              CALL BDA_MAPAXVAL( ILOC, 'READ', IAX, APTR(IAX),
     :                                                STATUS )

*            Normal widths?
              CALL BDA_CHKAXWID( ILOC, IAX, AWOK(IAX), AWUNIF(IAX),
     :                                               NVAL, STATUS )
              IF ( AWOK(IAX) ) THEN
                IF ( AWUNIF(IAX) ) THEN
                  CALL BDA_GETAXWID( ILOC, IAX, AWIDTH(IAX), STATUS )
                ELSE
                  CALL BDA_MAPAXWID( ILOC, 'READ', IAX, AWPTR(IAX),
     :                                                     STATUS )
                END IF
              END IF

            END IF
          END DO

        END IF
      END IF

*    Write IMPORT readable header?
      IF ( IMPREAD ) THEN

*      Construct TOPLEVEL component
        CALL EXPORT_SET( 'TYPE', TYPE )
        CALL BDA_GETTITLE( ILOC, TITLE, STATUS )
        CALL EXPORT_SET( 'TITLE', TITLE )
        CALL EXPORT_WRITE( OFD, 'TOPLEVEL ^TYPE ^TITLE', STATUS )

*      For each dimension
        DO IAX = 1, NDIM

*        Everything ok?
          IF ( AOK(IAX) ) THEN

*          Put LABEL token
            CALL BDA_GETAXLABEL( ILOC, IAX, LABEL, STATUS )
            CALL EXPORT_SET( 'LABEL', LABEL )

*          Put UNITS token
            CALL BDA_GETAXUNITS( ILOC, IAX, UNITS, STATUS )
            CALL EXPORT_SET( 'UNITS', UNITS )

*          Put NORM token
            CALL BDA_GETAXNORM( ILOC, IAX, ANORM, STATUS )
            IF ( ANORM ) THEN
              CALL MSG_SETC( 'NORM', 'NORM' )
            ELSE
              CALL MSG_SETC( 'NORM', ' ' )
            END IF

*          Put DECREASING token if needed
            IF ( AREG(IAX) ) THEN
              CALL BDA_GETAXVAL( ILOC, IAX, ABASE, ASCALE, NVAL,
     :                                                  STATUS )
              DECREASING = (ASCALE.LT.0.0)
            ELSE
              DECREASING = .FALSE.
            END IF
            IF ( DECREASING ) THEN
              CALL MSG_SETC( 'DECREASING', 'DECREASING' )
            ELSE
              CALL MSG_SETC( 'DECREASING', ' ' )
            END IF

*          Scalar axis widths?
            IF ( AWOK(IAX) .AND. AWUNIF(IAX) ) THEN
              CALL CHR_RTOC( AWIDTH(IAX), STRING, NCHAR )
              CALL EXPORT_SET( 'SCALAR_WIDTH', STRING(:NCHAR) )
            ELSE
              CALL EXPORT_SET( 'SCALAR_WIDTH', ' ' )
            END IF

*          Write the descriptor
            CALL EXPORT_WRITE( OFD, 'AXIS ^LABEL ^UNITS ^NORM'/
     :                   /' ^SCALAR_WIDTH ^DECREASING', STATUS )

*          Width descriptor?
            IF ( AWOK(IAX) .AND. .NOT. AWUNIF(IAX) ) THEN
              CALL EXPORT_WRITE( OFD, 'AXWIDTH', STATUS )
            END IF

*        Otherwsie write dummy axis
          ELSE
            CALL EXPORT_WRITE( OFD, 'AXIS', STATUS )

          END IF

        END DO

*      Reset uniform axis widths
        DO IAX = 1, NDIM
          IF ( AWOK(IAX) .AND. AWUNIF(IAX) ) THEN
            AWOK(IAX) = .FALSE.
          END IF
        END DO

*      The data
        CALL BDA_GETLABEL( ILOC, LABEL, STATUS )
        CALL EXPORT_SET( 'LABEL', LABEL )
        CALL BDA_GETUNITS( ILOC, UNITS, STATUS )
        CALL EXPORT_SET( 'UNITS', UNITS )
        CALL EXPORT_WRITE( OFD, 'DATA ^LABEL ^UNITS', STATUS )

*      Quality
        IF ( QOK ) THEN
          IMASK = QMASK
          CALL CHR_ITOC( IMASK, STRING, NCHAR )
          CALL EXPORT_SET( 'MASK', STRING(:NCHAR) )
          CALL EXPORT_WRITE( OFD, 'QUALITY ^MASK', STATUS )
        END IF

*      Asymmetric errors if present
        IF ( EOK ) THEN
          CALL EXPORT_WRITE( OFD, 'LOERROR', STATUS )
          CALL EXPORT_WRITE( OFD, 'UPERROR', STATUS )
        ELSE IF ( VOK ) THEN
          CALL EXPORT_WRITE( OFD, 'VARIANCE', STATUS )
        END IF

      END IF

*    Pad dimensions to 7D
      DO IAX = NDIM+1, DAT__MXDIM
        DIMS(IAX) = 1
        AOK(IAX) = .FALSE.
        AWOK(IAX) = .FALSE.
      END DO

*    Write data values
      CALL EXPORT_INT( DIMS(1), DIMS(2), DIMS(3), DIMS(4), DIMS(5),
     :                 DIMS(6), DIMS(7), NDIM,
     :                 AOK, APTR, AWOK, AWPTR,
     :                 DOK, %VAL(DPTR), QOK, %VAL(QPTR),
     :                 VOK, %VAL(VPTR), OFD, STATUS )

*    Release input
      CALL BDA_RELEASE( ILOC, STATUS )

*    Close output file
      CALL FIO_CLOSE( OFD, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  EXPORT_INT - Write data arrays to output file
      SUBROUTINE EXPORT_INT( L1, L2, L3, L4, L5, L6, L7, NDIM,
     :                       AOK, APTR, AWOK, AWPTR,
     :                       DOK, DATA, QOK, QUAL, VOK, VAR,
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
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER                 L1,L2,L3,L4,L5,L6,L7   ! Data dimensions
      INTEGER                 NDIM                   ! Dimensionality
      LOGICAL                 AOK(DAT__MXDIM)        ! Axes present?
      INTEGER                 APTR(DAT__MXDIM)       ! Axis data vals
      LOGICAL                 AWOK(DAT__MXDIM)       ! Axis width present?
      INTEGER                 AWPTR(DAT__MXDIM)      ! Axis width vals
      LOGICAL                 DOK, QOK, VOK          ! Items there?
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

      INTEGER                 ACB(DAT__MXDIM)        ! Axis column LHS
      INTEGER                 ACE(DAT__MXDIM)        ! Axis column RHS
      INTEGER                 COL                    ! Character position
      INTEGER                 CMAX                   ! Last column used
      INTEGER                 DCOL                   ! DATA column
      INTEGER                 I,J,K,L,M,N,O          ! Loops over data
      INTEGER                 IAX                    ! Loop over axes
      INTEGER                 QCOL                   ! QUALITY column
      INTEGER                 VCOL                   ! VARIANCE column
      INTEGER                 WCB(DAT__MXDIM)        ! Width column LHS
      INTEGER                 WCE(DAT__MXDIM)        ! Width column RHS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up output columns
      COL = 1
      DO IAX = 1, DAT__MXDIM
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
      INTEGER                 TLEN               ! Non-blank length of TEXT
*-

*    Only set if non-blank
      IF ( TEXT .GT. ' ' ) THEN

*      Find length of data
        TLEN = CHR_LEN( TEXT )

*      Set token and enclose data in double quotes if it has a space in it
        IF ( INDEX(TEXT(:TLEN),' ') .EQ. 0 ) THEN
          CALL MSG_SETC( TOKEN, TOKEN//' '//TEXT(:TLEN) )
        ELSE
          CALL MSG_SETC( TOKEN, TOKEN//' "'//TEXT(:TLEN)//'"' )
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

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Expand text
      CALL MSG_MAKE( TEXT, BUF, BLEN )

*    Write text
      CALL FIO_WRITE( FD, BUF(:BLEN), STATUS )

      END
