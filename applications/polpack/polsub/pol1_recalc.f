      SUBROUTINE POL1_RECALC( CI, DBTYPE, ALL, STATUS )
*+
*  Name:
*     POL1_RECALC

*  Purpose:
*     Re-calculate the values of derived columns in a catalogue

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_RECALC( CI, DBTYPE, ALL, STATUS )

*  Description:
*     This routine re-calculates derived columns in a catalogue, based
*     on the current values in the Stokes parameter columns (I, Q, U, DI,
*     DQ, DU). The specified form of de-biasing is applied to the new P
*     and PI values.

*  Arguments:
*     CI = INTEGER (Given)
*        The CAT identifier for the catalogue.
*     DBTYPE = CHARACTER * ( * ) (Given)
*        The type of de-biasing:
*        - "AS": The traditional asymptotic estimator.
*        - "MAS": The modified asymptotic estimator.
*        - "None": No de-biasing.
*     ALL = LOGICAL (Given)
*        If .TRUE., recalculate new values for all derived columns (P,
*        PI, ANG, DP, DPI, DANG). Otherwise recalculate new values for
*        the P and PI columns only.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     14-APR-2020 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'CAT_PAR'          ! CAT_ constants
      INCLUDE 'AST_PAR'          ! AST_ constants

*  Arguments Given:
      INTEGER CI
      CHARACTER DBTYPE*(*)
      LOGICAL ALL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDANG              ! CAT identifier for ANG column
      INTEGER IDDANG             ! CAT identifier for DANG column
      INTEGER IDDI               ! CAT identifier for DI column
      INTEGER IDDP               ! CAT identifier for DP column
      INTEGER IDDPI              ! CAT identifier for DPI column
      INTEGER IDDQ               ! CAT identifier for DQ column
      INTEGER IDDU               ! CAT identifier for DU column
      INTEGER IDI                ! CAT identifier for I column
      INTEGER IDP                ! CAT identifier for P column
      INTEGER IDPI               ! CAT identifier for PI column
      INTEGER IDQ                ! CAT identifier for Q column
      INTEGER IDU                ! CAT identifier for U column
      INTEGER IROW               ! Row index
      INTEGER NROW               ! No. of rows in catalogue
      LOGICAL NULL               ! Was no value available?
      REAL DI                    ! I error
      REAL DPI                   ! PI error
      REAL DQ                    ! Q error
      REAL DU                    ! U error
      REAL I                     ! I value
      REAL IP                    ! PI value
      REAL IP2                   ! Squared PI value
      REAL P                     ! P value
      REAL Q                     ! Q value
      REAL Q2                    ! Q squared value
      REAL T                     ! ANG value
      REAL U                     ! U value
      REAL U2                    ! U squared value
      REAL VIP                   ! Variance of PI
      REAL VP                    ! Variance on P value
      REAL VQ                    ! Variance on Q value
      REAL VT                    ! Variance on ANG value
      REAL VU                    ! Variance on U value
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of rows in the catalogue.
      CALL CAT_TROWS( CI, NROW, STATUS )

*  Get the required column identifiers (P, PI, I, Q, U and DPI)
      CALL POL1_GTCOL( CI, 'P', .TRUE., IDP, STATUS )
      CALL POL1_GTCOL( CI, 'PI', .TRUE., IDPI, STATUS )
      CALL POL1_GTCOL( CI, 'DPI', .TRUE., IDDPI, STATUS )
      CALL POL1_GTCOL( CI, 'I', .TRUE., IDI, STATUS )
      CALL POL1_GTCOL( CI, 'Q', .TRUE., IDQ, STATUS )
      CALL POL1_GTCOL( CI, 'U', .TRUE., IDU, STATUS )

      IF( ALL ) THEN
         CALL POL1_GTCOL( CI, 'DI', .TRUE., IDDI, STATUS )
         CALL POL1_GTCOL( CI, 'DQ', .TRUE., IDDQ, STATUS )
         CALL POL1_GTCOL( CI, 'DU', .TRUE., IDDU, STATUS )
         CALL POL1_GTCOL( CI, 'DP', .TRUE., IDDP, STATUS )
         CALL POL1_GTCOL( CI, 'ANG', .TRUE., IDANG, STATUS )
         CALL POL1_GTCOL( CI, 'DANG', .TRUE., IDDANG, STATUS )
      END IF

*  Loop round each row.
      DO IROW = 1, NROW

*  Read the input row into the CAT current row buffer.
         CALL CAT_RGET( CI, IROW, STATUS )

*  Get the required input values.
         CALL CAT_EGT0R( IDI, I, NULL, STATUS )
         IF( NULL ) I = VAL__BADR
         CALL CAT_EGT0R( IDQ, Q, NULL, STATUS )
         IF( NULL ) Q = VAL__BADR
         CALL CAT_EGT0R( IDU, U, NULL, STATUS )
         IF( NULL ) U = VAL__BADR

         IF( ALL ) THEN
            CALL CAT_EGT0R( IDDI, DI, NULL, STATUS )
            IF( NULL ) DI = VAL__BADR
            CALL CAT_EGT0R( IDDQ, DQ, NULL, STATUS )
            IF( NULL ) DQ = VAL__BADR
            CALL CAT_EGT0R( IDDU, DU, NULL, STATUS )
            IF( NULL ) DU = VAL__BADR
         ELSE
            DQ = VAL__BADR
            DU = VAL__BADR
            DI = VAL__BADR
         END IF

*  Calculate the new values
         IF( Q .NE. VAL__BADR .AND. U .NE. VAL__BADR ) THEN

*  Get the squared polarised intensity.
            Q2 = Q*Q
            U2 = U*U
            IP2 = Q2 + U2

*  Get the variance on the polarised intensity (if not ALL then we
*  re-use the existing variance value).
            IF( ALL ) THEN
               IF( DQ .NE. VAL__BADR .AND. DU .NE. VAL__BADR .AND.
     :             IP2 .GT. 0.0 ) THEN
                  VQ = DQ*DQ
                  VU = DU*DU
                  VIP = ( Q2*VQ + U2*VU )/IP2
                  IF( VIP .LT. 0.0 ) VIP = 0.0
               ELSE
                  VQ = VAL__BADR
                  VU = VAL__BADR
                  VIP = VAL__BADR
               END IF

            ELSE
               VQ = VAL__BADR
               VU = VAL__BADR

               CALL CAT_EGT0R( IDDPI, DPI, NULL, STATUS )
               IF( NULL ) THEN
                  DPI = VAL__BADR
                  VIP = VAL__BADR
               ELSE
                  VIP = DPI*DPI
               END IF

            END IF

*  De-bias the polarised intensity.
            IF( DBTYPE .EQ. 'AS' ) THEN
               IF( VIP .NE. VAL__BADR ) THEN
                  IP = SQRT( MAX( 0.0, IP2 - VIP ) )
               ELSE
                  IP = VAL__BADR
               END IF

            ELSE IF( DBTYPE .EQ. 'MAS' ) THEN
               IF( VIP .NE. VAL__BADR ) THEN
                  IP = SQRT( IP2 )
                  IF( IP .GT. 0.0 ) THEN
                     IP = IP - 0.5*VIP*( 1.0 - EXP( -IP2/VIP ) )/IP
                  ELSE
                     IP = 0.0
                  END IF
               ELSE
                  IP = VAL__BADR
               END IF

            ELSE
               IP = SQRT( IP2 )
            END IF

*  Percentage polarisation (value and error).
            IF( I .GT. 0.0 .AND. I .NE. VAL__BADR .AND.
     :          IP .NE. VAL__BADR ) THEN
               P = 100 * IP / I
               IF( VIP .NE. VAL__BADR ) THEN
                  VP = 10000.0*( VIP/(I**2) + DI*DI*IP2/(I**4) )
                  IF( VP .LT. 0.0 ) VP = 0.0
               ELSE
                  VP = VAL__BADR
               END IF
            ELSE
               P = VAL__BADR
               VP = VAL__BADR
            END IF

*  Polarisation angle (value and error).
            IF( ALL .AND. U .NE. 0.0 .OR. Q .NE. 0.0 ) THEN
               T = AST__DR2D * 0.5 * ATAN2( U, Q )
               IF( IP2 .NE. 0.0 .AND. VQ .NE. VAL__BADR .AND.
     :                                VU .NE. VAL__BADR ) THEN
                  VT = AST__DR2D*AST__DR2D*( Q2*VU + U2*VQ )/
     :                                       ( 4.0*IP2*IP2 )
                  IF( VT .LT. 0.0 ) VT = 0.0
               ELSE
                  VT = VAL__BADR
               END IF
            ELSE
               T = VAL__BADR
               VT = VAL__BADR
            END IF

*  Cannot calculate anything if Q or U is bad.
         ELSE
            P = VAL__BADR
            IP = VAL__BADR
            T = VAL__BADR
            VP = VAL__BADR
            VIP = VAL__BADR
            VT = VAL__BADR
         END IF

*  Store the new calues in the catalogue
         CALL CAT_PUT0R( IDP, P, ( P .EQ. VAL__BADR ), STATUS )
         CALL CAT_PUT0R( IDPI, IP, ( IP .EQ. VAL__BADR ), STATUS )

         IF( ALL ) THEN
            CALL CAT_PUT0R( IDANG, T, ( T .EQ. VAL__BADR ), STATUS )
            CALL CAT_PUT0R( IDDP, SQRT( VP ), ( VP .EQ. VAL__BADR ),
     :                      STATUS )
            CALL CAT_PUT0R( IDDPI, SQRT( VIP ), ( VIP .EQ. VAL__BADR ),
     :                      STATUS )
            CALL CAT_PUT0R( IDDANG, SQRT( VT ), ( VT .EQ. VAL__BADR ),
     :                      STATUS )
         END IF

      END DO

      END
