      SUBROUTINE tape_SHOP( STATUS )
*+
*   Name:
*      SUBROUTINE tape_SHOP
*
*   Description:
*      Print current tape position.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      29-OCT-81
*         AT4 version.
*      Paul Rees          26-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          01-JUN-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      Get tape position from MT library and print those parts that
*      are defined.
*
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*   Global Variables:
      INCLUDE 'CMTAPE'

*   Global constants:
      INTEGER MTBOF      ! beginning of tape file
      INTEGER MTTOF      ! tail of tape file
      INTEGER MTUNDEF    ! undefined file, positionor block
      INTEGER OK         ! status OK
      PARAMETER (MTBOF=1, MTTOF=2, MTUNDEF=0, OK=0)

*   Export:
      INTEGER STATUS     ! status return

*   Local Variables:
      INTEGER BLOCK
      INTEGER FILE

      LOGICAL START
      LOGICAL MOVED
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get tape position.
      CALL MAG_POS( TCHAN, FILE, START, BLOCK, MOVED, STATUS )

*   Print tape position
      CALL line_WCONT('%p Tape is positioned\\')

      IF ( BLOCK .EQ. 0 ) THEN
         CALL line_WCONT(' at undefined position in\\')

      ELSE IF ( START ) THEN
         IF ( BLOCK .EQ. 1 ) THEN
            CALL line_WCONT(' at the start of\\')

         ELSE
            CALL line_WRITI(' at block %i relative to start of\\',
     :           BLOCK)
         END IF

      ELSE
         IF ( BLOCK .EQ. 1 ) THEN
            CALL line_WCONT(' at the end of\\')

         ELSE
            CALL line_WRITI(' at block %i relative to end of\\', BLOCK)
         END IF
      END IF

      IF ( FILE .NE. 0 ) THEN
         CALL line_WRITI(' file %i\\', FILE)

      ELSE
         CALL line_WCONT(' undefined file\\')
      END IF

      CALL PRTBUF( STATUS )

 999  CONTINUE

      END
