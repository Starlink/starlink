      SUBROUTINE SPD_WZKH( NCOMP, TNPAR, NPARA, COMPTY, PARATY,
     :   NLINES, ELMNOS, STATUS )
*+
*  Name:
*     SPD_WZKH

*  Purpose:
*     Find the line components in a results structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZKH( NCOMP, TNPAR, NPARA, COMPTY, PARATY,
*        NLINES, ELMNOS, STATUS )

*  Description:
*     This routine performs some preparatory work for ARCIDENT, given
*     the results structure from its input NDF.
*
*     This routine goes through the given arrays of component and
*     parameter types and works out in which elements of the results
*     data array the centres of Gauss or triangle component would be
*     found. It also returns how many Gauss or triangle components
*     exist.

*  Arguments:
*     NCOMP = INTEGER (Given)
*        The number of components in the results structure.
*     TNPAR = INTEGER (Given)
*        The total number of parameters in the results structure.
*     NPARA( NCOMP ) = INTEGER (Given)
*        For each component the number of parameters allocated to that
*        component.
*     COMPTY( NCOMP ) = CHARACTER * ( * ) (Given)
*        The component types.
*     PARATY( TNPAR ) = CHARACTER * ( * ) (Given)
*        The parameter types.
*     NLINES = INTEGER (Returned)
*        The number of line components (Gauss or triangle) found.
*     ELMNOS( NCOMP ) = INTEGER (Returned)
*        For I = 1 to NLINES and if DATA is the data array of the
*        results structure, DATA(ELMNOS(I),1,,,,) is the centre of the
*        I-th line component found. The rest of the array is filled with
*        zeros.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine assumes that the given information is consistent and
*     describes a valid results structure.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Jun 1993 (hme):
*        Original version.
*     25 Jan 1995 (hme):
*        Renamed from SPADP.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCOMP
      INTEGER TNPAR
      INTEGER NPARA( NCOMP )
      CHARACTER * ( * ) COMPTY( NCOMP )
      CHARACTER * ( * ) PARATY( TNPAR )

*  Arguments Returned:
      INTEGER NLINES
      INTEGER ELMNOS( NCOMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NC, TC, I

*  Internal References:
      LOGICAL CHR_SIMLR

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset ELMNOS.
      DO 1 I = 1, NCOMP
         ELMNOS(I) = 0
 1    CONTINUE

*  Loop through components.
*  TC counts through all parameters.
      NLINES = 0
      TC = 1
      DO 3 NC = 1, NCOMP

*     If it is a line.
         IF ( CHR_SIMLR( 'Gauss', COMPTY(NC) )            .OR.
     :        CHR_SIMLR( 'triangle', COMPTY(NC) )         .OR.
     :        CHR_SIMLR( 'Gauss feature', COMPTY(NC) )    .OR.
     :        CHR_SIMLR( 'triangle feature', COMPTY(NC) )      ) THEN

*        Loop through the parameters of the line.
            DO 2 I = TC, TC+NPARA(NC)-1
               IF ( CHR_SIMLR( 'centre', PARATY(I) ) ) THEN
                  NLINES = NLINES + 1
                  ELMNOS(NLINES) = I
               END IF
 2          CONTINUE
         END IF

*     Increment parameter counter to skip to next component.
         TC = TC + NPARA(NC)
 3    CONTINUE

      END
