      SUBROUTINE UNBKG( IBKG, NSUB, RSUB, WSUB, DSUB, QSUB )
*+
*  Name:
*     SUBROUTINE UNBKG

*  Purpose:
*     Unflag pixels marked as background spikes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL UNBKG( IBKG, NSUB, RSUB, WSUB, DSUB, QSUB )

*  Arguments:
*     IBKG = INTEGER  (Given)
*        Background channel index.
*     NSUB = INTEGER  (Given)
*        Number of subset pixels.
*     RSUB = REAL*8( NSUB ) (Given)
*        R-coordinate array.
*     WSUB = REAL*8( NSUB ) (Given)
*        W-coordinate array.
*     DSUB = INTEGER*2( NSUB ) (Given)
*        Data values.
*     QSUB = BYTE( NSUB ) (Given and Returned)
*        Quality values.

*  Method:
*     This sets all background channel pixels with QSPK severity to
*     "0" severity.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER IBKG            ! Background channel index.
      INTEGER NSUB            ! Number of subset pixels.

      REAL*8 RSUB( NSUB )     ! R-coordinates.
      REAL*8 WSUB( NSUB )     ! W-coordinates.

      INTEGER*2 DSUB( NSUB )  ! Data values.

*  Arguments Given and Returned:
      BYTE QSUB( NSUB )       ! Qual values.

*  Global Variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMBKG'
      INCLUDE 'CMWAV'
      INCLUDE 'CMCEN'

*  Local Variables:
      REAL*8 R                ! Pixel R.
      REAL*8 W                ! Pixel wavelength.

      INTEGER I               ! Wavelength index.
      INTEGER J               ! Pixel index.
*.

      DO J = 1, NSUB
         W = WSUB( J )
         I = MAX( 1,
     :       MIN( NWAV, NINT( REAL( ( W - WAV1 ) / DWAV ) ) + 1 ) )
         IF ( QBKG( I ) .EQ. 0 ) THEN
            R = RSUB( J ) + SCEN( I )
            IF ( R.GE.RBKG( 1, IBKG ) .AND.
     :           R.LE.RBKG( 2, IBKG ) ) THEN
               IF ( QSUB( J ) .EQ. QSPK ) THEN
                  QSUB( J ) = 0
               END IF
            END IF
         END IF
      END DO

      END
