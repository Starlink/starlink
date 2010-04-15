      SUBROUTINE SUB_LISTO( IPARI, STATUS )
*+
*  Name:
*     SUB_LISTO

*  Purpose:
*     This program reads the intermediate data file and creates
*     a formatted file for output to a printer.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBLISTO( IPARI, STATUS )

*  Description:
C      Formerly PROGRAM PRINT
C
C      This program reads the intermediate data file and creates a
C      formatted file for output to a printer.
C
C      Most of the hardwork is done by the existing PRINT subroutine
C      in CHART.
C

*  Arguments:
*     IPARI = INTEGER (Given and Returned)
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*  ANO: Someone (Somewhere)
*     {enter_new_authors_here}

*  History:
*     {date} ({author_identifier}):
*        Original version.
*     22-FEB-1993 (AJJB):
*        Conversion to ADAM and proper commenting
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'MAIN'             ! CHART control common blocks

*  Globals used from MAIN.FOR:
*
*        AO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE RA AT EQUINOX 'EQUOUT'
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*        {descriptions_of_global_variables_referenced}...

*  Arguments Given:
      INTEGER IPARI

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER*3 FLDNUM, TEXT * 60

      INTEGER IFLD, IEND
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C
C
C   There may be several sets of parameters, corresponding to
C   several fields.
C
C   So - keep restoring until an error occurs.
C
      IFLD=1
  100 CONTINUE
         CALL RESTORE(IEND,IPARI, STATUS )
         IF (IEND.GT.0) GO TO 899
         WRITE  (FLDNUM,'(I3)') IFLD
         TEXT='Field number '//FLDNUM//' processed'
         CALL MSG_OUT(' ', TEXT, STATUS )
         CALL MSG_OUT( ' ', 'Output written to output.lis', STATUS )
         IFLD=IFLD+1
         CALL SORT( STATUS )
         CALL HDNG( STATUS )
         CALL CONST(AO,DO, STATUS )
         CALL OUTPUT( STATUS )
         GO TO 100
  899 CONTINUE
      END
