      PROGRAM ADAM_TEST
*+
*  Name:
*     ADAM_TEST

*  Purpose:
*     To test the ADAM installation - not its functionality

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     ADAM_TEST

*  Description:
*     Compiling and linking this program will give a good indication
*     that the ADAM library is correctly installed

*  Authors:
*     ENV: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1993 (ENV):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE               ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! SAE global constants
      INCLUDE 'ADAM_DEFNS'        ! ADAM global constants
      INCLUDE 'ADAM_ERR'          ! ADAM status values

*  Local Variables:
      INTEGER STATUS              ! Status
      INTEGER LENGTH              ! Length of process name
      CHARACTER*15 NAME           ! Process name
*.

      STATUS = SAI__OK

*  Call an ADAM routine
      CALL ADAM_PRCNAM( NAME, LENGTH, STATUS )
      
      IF ( STATUS .EQ. SAI__OK ) THEN
         PRINT *, NAME(1:LENGTH), ' OK'
      ELSE
         PRINT *, 'ADAM_TEST failed'
      END IF

      END
