      SUBROUTINE GRF_TZONE( ZBASE, ZONE, ZCLEAR, STATUS )
*+
*  Name:
*     SUBROUTINE GRF_TZONE

*  Purpose:
*     Define a GKS zone on the current workstation using zone codes
*     similar to those used in DIPSO (originating from GRAFX).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_TZONE( ZBASE, ZONE, ZCLEAR, STATUS )

*  Arguments:
*     ZBASE = INTEGER (Given)
*        Integer containing SGS identifier for base zone (set by grf_ZINIT).
*     ZONE = INTEGER (Given)
*        Number of plotting zone required (as defined in DIPSO):
*           0   Entire surface,
*           1   Top left quarter,
*           2   Top right quarter,
*           3   Bottom left quarter,
*           4   Bottom right quarter,
*           5   Top half,
*           6   Bottom half,
*           7   Left half,
*           8   Right half.
*     ZCLEAR = LOGICAL (Given)
*        Clear workstation surface if zone overlaps.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     07-SEP-87 (PCTR):
*       IUEDR Vn. 2.0
*     09-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     19-SEP-94 (MJC):
*       IUEDR Vn. 3.1-4
*       Fixed zone release bug.
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER ERR           ! error status
      INTEGER GALWAY        ! GKS unconditional workstation clear (GCLRWK)
      INTEGER GCONDI        ! GKS conditional workstation clear (GCLRWK)
      PARAMETER (ERR = -3, GALWAY = 1, GCONDI = 0 )

*   Arguments Given:
      INTEGER ZBASE
      INTEGER ZONE

      LOGICAL ZCLEAR

*   Status:
      INTEGER STATUS

*   Local Variables:
      REAL X1BASE           ! LHS NDC of base zone
      REAL X2BASE           ! RHS NDC of base zone
      REAL Y1BASE           ! bottom NDC of base zone
      REAL Y2BASE           ! top NDC of bas zone
      REAL XM               ! x-axis extent of zone in metres
      REAL YM               ! y-axis extent of zone in metres
      REAL X1               ! LHS NDC of zone
      REAL X2               ! RHS NDC of zone
      REAL Y1               ! bottom NDC of zone
      REAL Y2               ! top NDC of zone

      LOGICAL IZOVLP(0:8)   ! zone overlap truth table

      INTEGER CONID         ! GKS workstation conection ID
      INTEGER ERRIND        ! GKS error indicator
      INTEGER IWLAST        ! last GKS workstation type
      INTEGER IZON          ! zone loop index
      INTEGER IZONID        ! GKS zone ID
      INTEGER WKID          ! GKS workstation ID
      INTEGER WTYPE         ! GKS workstation type

*   Data:
      DATA IZOVLP / 9*.FALSE. /
      DATA IWLAST / 0 /

*.

*   Save local variables.
      SAVE IZOVLP
      SAVE IWLAST

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Flush plotting.
      CALL SGS_FLUSH

*   Test for ZONE out of range.
      IF ( ZONE.GT.-1 .AND. ZONE.LT.9 ) THEN

*      Current GKS workstation ID and type.
         CALL SGS_ICURW( WKID )
         CALL GQWKC( WKID, ERRIND, CONID, WTYPE )

*      Initialise truth table for a new device.
         IF ( WTYPE .NE. IWLAST ) THEN
            DO IZON = 0, 8
               IZOVLP( IZON ) = .FALSE.
            END DO
            IWLAST = WTYPE
         END IF

*      Determine current zone identifier.
         CALL SGS_ICURZ( IZONID )

*      Select base zone.
         CALL SGS_SELZ( ZBASE, STATUS )

*      Release last current zone.
         CALL SGS_RELZ( IZONID )

*      Trap zone selection errors.
         IF ( STATUS .EQ. SAI__OK ) THEN

*         Determine base zone bounds.
            CALL SGS_IZONE( X1BASE, X2BASE, Y1BASE, Y2BASE, XM, YM )

*         Check for any overlap.
            IF ( IZOVLP( ZONE ) ) THEN

*            Clear workstation if required.
               IF ( ZCLEAR ) THEN
                  CALL GCLRWK( WKID, GCONDI )

*               Reset overlap truth table.
                  DO IZON = 0, 8
                     IZOVLP( IZON ) = .FALSE.
                  END DO
               END IF
            END IF

*         Calculate bounds of new zone.

*         Entire surface.
            IF ( ZONE .EQ. 0 ) THEN
               X1 = X1BASE
               X2 = X2BASE
               Y1 = Y1BASE
               Y2 = Y2BASE

*            Reset overlap truth table.
               DO IZON = 0, 8
                  IZOVLP( IZON ) = .TRUE.
               END DO

*         Top left quarter.
            ELSE IF ( ZONE .EQ. 1 ) THEN
               X1 = X1BASE
               X2 = X2BASE / 2.0
               Y1 = Y2BASE / 2.0
               Y2 = Y2BASE

*            Reset overlap truth table.
               IZOVLP( 0 ) = .TRUE.
               IZOVLP( 1 ) = .TRUE.
               IZOVLP( 5 ) = .TRUE.
               IZOVLP( 7 ) = .TRUE.

*         Top right quarter.
            ELSE IF ( ZONE .EQ. 2 ) THEN
               X1 = X2BASE / 2.0
               X2 = X2BASE
               Y1 = Y2BASE / 2.0
               Y2 = Y2BASE

*            Reset overlap truth table.
               IZOVLP( 0 ) = .TRUE.
               IZOVLP( 2 ) = .TRUE.
               IZOVLP( 5 ) = .TRUE.
               IZOVLP( 8 ) = .TRUE.

*         Bottom left quarter.
            ELSE IF ( ZONE .EQ. 3 ) THEN
               X1 = X1BASE
               X2 = X2BASE / 2.0
               Y1 = Y1BASE
               Y2 = Y2BASE / 2.0

*            Reset overlap truth table.
               IZOVLP( 0 ) = .TRUE.
               IZOVLP( 3 ) = .TRUE.
               IZOVLP( 6 ) = .TRUE.
               IZOVLP( 7 ) = .TRUE.

*         Bottom right quarter.
            ELSE IF ( ZONE .EQ. 4 ) THEN
               X1 = X2BASE / 2.0
               X2 = X2BASE
               Y1 = Y1BASE
               Y2 = Y2BASE / 2.0

*            Reset overlap truth table.
               IZOVLP( 0 ) = .TRUE.
               IZOVLP( 4 ) = .TRUE.
               IZOVLP( 6 ) = .TRUE.
               IZOVLP( 8 ) = .TRUE.

*         Top half.
            ELSE IF ( ZONE .EQ. 5 ) THEN
               X1 = X1BASE
               X2 = X2BASE
               Y1 = Y2BASE / 2.0
               Y2 = Y2BASE

*            Reset overlap truth table.
               IZOVLP( 0 ) = .TRUE.
               IZOVLP( 1 ) = .TRUE.
               IZOVLP( 2 ) = .TRUE.
               IZOVLP( 5 ) = .TRUE.
               IZOVLP( 7 ) = .TRUE.
               IZOVLP( 8 ) = .TRUE.

*         Bottom half.
            ELSE IF ( ZONE .EQ. 6 ) THEN
               X1 = X1BASE
               X2 = X2BASE
               Y1 = Y1BASE
               Y2 = Y2BASE / 2.0

*            Reset overlap truth table.
               IZOVLP( 0 ) = .TRUE.
               IZOVLP( 3 ) = .TRUE.
               IZOVLP( 4 ) = .TRUE.
               IZOVLP( 6 ) = .TRUE.
               IZOVLP( 7 ) = .TRUE.
               IZOVLP( 8 ) = .TRUE.

*         Left half.
            ELSE IF ( ZONE .EQ. 7 ) THEN
               X1 = X1BASE
               X2 = X2BASE / 2.0
               Y1 = Y1BASE
               Y2 = Y2BASE

*            Reset overlap truth table.
               IZOVLP( 0 ) = .TRUE.
               IZOVLP( 1 ) = .TRUE.
               IZOVLP( 3 ) = .TRUE.
               IZOVLP( 5 ) = .TRUE.
               IZOVLP( 6 ) = .TRUE.
               IZOVLP( 7 ) = .TRUE.

*         Right half.
            ELSE IF ( ZONE .EQ. 8 ) THEN
               X1 = X2BASE / 2.0
               X2 = X2BASE
               Y1 = Y1BASE
               Y2 = Y2BASE

*            Reset overlap truth table.
               IZOVLP( 0 ) = .TRUE.
               IZOVLP( 2 ) = .TRUE.
               IZOVLP( 4 ) = .TRUE.
               IZOVLP( 5 ) = .TRUE.
               IZOVLP( 6 ) = .TRUE.
               IZOVLP( 8 ) = .TRUE.
            END IF

*         Define zone.
            CALL SGS_ZONE( X1, X2, Y1, Y2, IZONID, STATUS )

*         Select defined zone.
            IF ( STATUS .EQ. SAI__OK ) THEN

*            No error in zone definition - select zone.
               CALL SGS_SELZ( IZONID, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_FLUSH( STATUS )
                  STATUS = ERR
               END IF
            END IF
         END IF

*   For ZONE out of range.
      ELSE
         STATUS = ERR
      END IF

      END
