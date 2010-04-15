************************************************************************

      SUBROUTINE INTELL ( NX, NY, IMAGE, GRID, GS, NXL, NXH, NYL, NYH,
     :                    SATURE, CODE, SUM, AREA )

*+
*  Name :
*     INTELL
*
*  Purpose :
*     This sums the product of the image and grid arrays
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL INTELL( NX, NY, IMAGE, GRID, GS, NXL, NXH, NYL, NYH,
*    :             SATURE, CODE, SUM, AREA )
*
*  Description :
*     This sums the product of the image and grid arrays between the limits
*     ignoring bad pixels.
*
*  Arguments :
*     NX = INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     IMAGE( NX, NY ) = REAL (Given)
*        Array containing image
*     GRID( GS, GS ) = REAL (Given)
*        Work space array
*     GS = INTEGER (Given)
*        Size of grid array
*     NXL = INTEGER (Given)
*        X value for lower integration limit
*     NXH = INTEGER (Given)
*        X value for upper integration limit
*     NYL = INTEGER (Given)
*        Y value for lower integration limit
*     NYH = INTEGER (Given)
*        Y value for upper integration limit
*     SATURE = REAL (Given)
*        User supplied saturation level
*     CODE = CHARACTER (Returned)
*        Error code flag
*     SUM = REAL (Returned)
*        Value of the integration
*     AREA = REAL (Returned)
*        Integrated area inside cursor
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-OCT-1987 (NE):
*        Original version.
*     10-JAN-1988 (NE):
*        Allow for bad pixels and sum area of ellipse
*     10-JAN-1992 (NE):
*        Limit size of grid array
*     6-NOV-1996 (PWD):
*        Converted sensitive arithmetic to DOUBLE PRECISION
*        in attempt to cure excessive Linux "rounding errors".
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'PRM_PAR'


*  Arguments Given :
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      INTEGER GS
      REAL GRID( GS, GS )
      INTEGER NXL
      INTEGER NXH
      INTEGER NYL
      INTEGER NYH
      REAL SATURE

*  Arguments Returned :
      CHARACTER * ( 2 ) CODE
      REAL SUM
      REAL AREA

*  Local Variables :
      LOGICAL BAD, SAT

      INTEGER I, II, J, JJ

      REAL VALUE
      DOUBLE PRECISION LSUM
      DOUBLE PRECISION LAREA
*.

*   Do some initialisations
      LAREA = 0.0D0
      LSUM = 0.0D0
      BAD = .FALSE.
      SAT = .FALSE.

*   Step through the arrays between the integration limits
      DO J = NYL, NYH

*   Calculate index into grid array
         JJ = J - NYL + 1

*   Check that the index lies within the image array
         IF ( ( J .GE. 1 ) .AND. ( J .LE. NY ) ) THEN

*   Step through the arrays between the integration limits
            DO I = NXL, NXH

*   Calculate index into grid array
               II = I - NXL + 1

*   Check that the index lies within the image array
               IF ( ( I .GE. 1 ) .AND. ( I .LE. NX ) ) THEN

                  VALUE = IMAGE( I, J )

*   Check that the pixel value is valid
                  IF ( VALUE .NE. VAL__BADR ) THEN

*   Check that the pixel is not saturated
                     IF ( VALUE .LE. SATURE ) THEN
                        LAREA = LAREA + DBLE( GRID( II, JJ ) )
                        LSUM = LSUM +
     :                         DBLE( VALUE ) * DBLE( GRID( II, JJ ) )

                     ELSE
                        SAT = .TRUE.
                        LAREA = LAREA + DBLE( GRID( II, JJ ) )
                        LSUM = LSUM +
     :                         DBLE( VALUE ) * DBLE( GRID( II, JJ ) )
                     ENDIF

*   Flag a bad pixel
                  ELSE
                     BAD = .TRUE.

                  ENDIF
               ENDIF
            ENDDO

         ENDIF
      ENDDO

*   Indicate the error codes
      IF ( BAD ) THEN
         CODE = 'B'
      ENDIF

      IF ( SAT ) THEN
         CODE = 'S'
      ENDIF

*   And convert output into REAL.
      AREA = REAL( LAREA )
      SUM = REAL( LSUM )
      END

