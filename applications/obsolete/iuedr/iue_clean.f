      SUBROUTINE CLEAN( NXA, NYA, ZA, QA, THRESH )
*+
*  Name:
*     SUBROUTINE USR_CLEAN

*  Description:
*     Marks all pixels in an image which fall below the
*     supplied threshold value BAD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CLEAN( NXA, NYA, ZA, QA, THRESH )

*  Arguments:
*     NXA = INTEGER (Given)
*        Size of X-axis of image.
*     NYA = INTEGER (Given)
*        Size of Y-axis of image.
*     ZA = INTEGER*2( NXA, NYA ) (Given)
*        Array of image pixel values.
*     QA = BYTE( NXA, NYA ) (Given and Returned)
*        Image data aulity description array.
*     THRESH = REAL*8 (Given)
*        Minimum accepted value.

*  Authors:
*     MJC: Martin Clayton (Starlink)
*     {enter_new_authors_here}

*  History:
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     26-APR-95 (MJC):
*       Now marks data quality as altered so that the Data and Quality file
*       is updated.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER NXA                ! Size of axis one.
      INTEGER NYA                ! Size of axis two.

      INTEGER*2 ZA( NXA, NYA )   ! Image data array.

      REAL*8 THRESH              ! Minimum accepted value.

*  Arguments Given and Returned:
      BYTE QA( NXA, NYA )        ! Data Quality array.

*  Local Variables:
      INTEGER I                  ! Loop index.
      INTEGER J                  ! Loop index.
      INTEGER DQ                 ! Pixel data quality flag.
      INTEGER TOTAL              ! Total number of pixels marked BAD.
*.

*  Check through pixels, marking them bad if their value falls
*  below the threshold value supplied.
      TOTAL = 0
      DO I = 1, NXA
         DO J = 1, NYA
            IF ( ZA( I, J ) .LE. THRESH ) THEN
               CALL DQ_WRPK( 1, 2, 1, DQ )
               CALL DQ_ITOU( DQ, QA( I, J ) )
               TOTAL = TOTAL + 1
            END IF
         END DO
      END DO

*  Mark Data and Quality file as requiring update.
      IF ( TOTAL .GT. 0 ) THEN
         CALL MODUEQ
      END IF

      END
