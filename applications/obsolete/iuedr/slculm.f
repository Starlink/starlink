      SUBROUTINE SLCULM( STATUS )

*+
*
*   Name:
*      SUBROUTINE SLCULM
*
*   Description:
*      Use image display cursor to select values for XP and YP -
*      the image display limits.
*
*   Routine History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          09-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     19-OCT-94     IUEDR Vn. 3.1-9
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Export:
      INTEGER STATUS     ! status return

*   Local constants:
      INTEGER CURMOD     ! GKS cursor mode (0 default)
      PARAMETER (CURMOD = 0)

*   Local variables:
      REAL X1            ! REAL*8 version of XS(1) for GRF_CUZONE call
      REAL X2            ! REAL*8 version of XS(2) for GRF_CUZONE call
      REAL Y1            ! REAL*8 version of YS(1) for GRF_CUZONE call
      REAL Y2            ! REAL*8 version of YS(2) for GRF_CUZONE call

      INTEGER XS(2)      ! local version of XP
      INTEGER YS(2)      ! local version of YP
      INTEGER KEY        ! hit key

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   XP(1) and YP(1)
      CALL GRF_CUZONE( '12', CURMOD, KEY, X1, Y1, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading cursor\\', STATUS )

      ELSE IF ( KEY .LE. 0 ) THEN
         CALL ERROUT( 'Error: unexpected cursor hit\\', STATUS )

      ELSE
         XS(1) = NINT(REAL(X1))
         YS(1) = NINT(REAL(Y1))

*      XP(2) and YP(2)
         X2 = X1
         Y2 = Y1
         CALL GRF_CUZONE( '12', CURMOD, KEY, X2, Y2, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: reading cursor\\', STATUS )

         ELSE IF ( KEY .LE. 0 ) THEN
            CALL ERROUT( 'Error: unexpected cursor hit\\', STATUS )

         ELSE
            XS(2) = NINT(REAL(X2))
            YS(2) = NINT(REAL(Y2))

            IF ( XS(2) .LT. XS(1) ) CALL MSC_ISWAP( XS(1), XS(2) )
            IF ( YS(2) .LT. YS(1) ) CALL MSC_ISWAP( YS(1), YS(2) )

*         XP - write parameter
            CALL WRPARI( 'XP\\', 2, XS, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERRPAR( 'XP\\' )
               CALL ERROUT( ': parameter write error\\', STATUS )

            ELSE

*            YP - write parameter
               CALL WRPARI( 'YP\\', 2, YS, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERRPAR( 'YP\\' )
                  CALL ERROUT( ': parameter write error\\', STATUS )
               END IF
            END IF
         END IF
      END IF

      END
