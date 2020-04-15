      SUBROUTINE POL1_DEBIAS( CI, DBTYPE, STATUS )
*+
*  Name:
*     POL1_DEBIAS

*  Purpose:
*     Re-calculate the P and PI values in a catalogue

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_DEBIAS( CI, DBTYPE, STATUS )

*  Description:
*     This routine re-calculates the P and PI values in a catlogue, based
*     on the I, Q and U values in the catalogue, using a specified form of
*     de-biasing.

*  Arguments:
*     CI = INTEGER (Given)
*        The CAT identifier for the catalogue.
*     DBTYPE = CHARACTER * ( * ) (Given)
*        The type of de-biasing:
*        - "AS": The traditional asymptotic estimator.
*        - "MAS": The modified asymptotic estimator.
*        - "None": No de-biasing.
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

*  Arguments Given:
      INTEGER CI
      CHARACTER DBTYPE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDDPI              ! CAT identifier for DPI column
      INTEGER IDI                ! CAT identifier for I column
      INTEGER IDP                ! CAT identifier for P column
      INTEGER IDPI               ! CAT identifier for PI column
      INTEGER IDQ                ! CAT identifier for Q column
      INTEGER IDU                ! CAT identifier for U column
      INTEGER IROW               ! Row index
      INTEGER NROW               ! No. of rows in catalogue
      LOGICAL NULL               ! Was no value available?
      REAL DPI                   ! PI error
      REAL I                     ! I value
      REAL IP                    ! PI value
      REAL IP2                   ! Squared PI value
      REAL P                     ! P value
      REAL Q                     ! Q value
      REAL U                     ! U value
      REAL VIP                   ! Variance of PI
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of rows in the catalogue.
      CALL CAT_TROWS( CI, NROW, STATUS )

*  Get the required column identifiers (P, PI, I, Q, U and DPI)
      CALL POL1_GTCOL( CI, 'P', .TRUE., IDP, STATUS )
      CALL POL1_GTCOL( CI, 'PI', .TRUE., IDPI, STATUS )
      CALL POL1_GTCOL( CI, 'I', .TRUE., IDI, STATUS )
      CALL POL1_GTCOL( CI, 'Q', .TRUE., IDQ, STATUS )
      CALL POL1_GTCOL( CI, 'U', .TRUE., IDU, STATUS )
      CALL POL1_GTCOL( CI, 'DPI', .TRUE., IDDPI, STATUS )

*  Loop round each row.
      DO IROW = 1, NROW

*  Read the input row into the CAT current row buffer.
         CALL CAT_RGET( CI, IROW, STATUS )

*  Get the required values (I, Q, U and DPI)
         CALL CAT_EGT0R( IDI, I, NULL, STATUS )
         IF( .NOT. NULL ) CALL CAT_EGT0R( IDQ, Q, NULL, STATUS )
         IF( .NOT. NULL ) CALL CAT_EGT0R( IDU, U, NULL, STATUS )
         IF( .NOT. NULL ) CALL CAT_EGT0R( IDDPI, DPI, NULL, STATUS )

* If none are null, calculate the new P and PI
         IF( .NOT. NULL ) THEN
            IP2 = Q*Q + U*U
            VIP = DPI*DPI

            IF( DBTYPE .EQ. 'AS' ) THEN
               IP = SQRT( MAX( 0.0, IP2 - VIP ) )

            ELSE IF( DBTYPE .EQ. 'MAS' ) THEN
               IP = SQRT( IP2 )
               IF( IP .GT. 0.0 ) THEN
                  IP = IP - 0.5*VIP*( 1.0 - EXP( -IP2/VIP ) )/IP
               ELSE
                  IP = 0.0
               END IF

            ELSE
               IP = SQRT( IP2 )
            END IF

            IF( I .GT. 0.0 ) THEN
               P = 100 * IP / I
            ELSE
               P = VAL__BADR
            END IF

         ELSE
            P = VAL__BADR
            IP = VAL__BADR
         END IF

*  Store the new calues in the catalogue
         CALL CAT_PUT0R( IDP, P, ( P .EQ. VAL__BADR ), STATUS )
         CALL CAT_PUT0R( IDPI, IP, ( IP .EQ. VAL__BADR ), STATUS )
      END DO

      END
