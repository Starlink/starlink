      SUBROUTINE OBJPLT(XPLOT,YPLOT,R,N, STATUS )
*+
*   This routine plots a square and rotated exploded cross for
*   each supplementary object.
*
*   Gets
*   ----
*      XPLOT,YPLOT  - Co-ordinates of centre of the object.
*      R            - Half-width of the object.
*      N            - Number of the object (negative).
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

        CALL SQU (XPLOT,YPLOT,R, STATUS )
        CALL TILTX (XPLOT,YPLOT,R, STATUS )
        CALL NUMB (XPLOT,YPLOT,R,-N, STATUS )

        END

