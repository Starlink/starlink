      SUBROUTINE TRN1_GTNVL( NMOD, NVAR, NVIN, NVOUT, STATUS )








*+
*  Name:
*     TRN1_GTNVL

*  Purpose:
*     get numbers of variables from a compiled module list.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_GTNVL( NMOD, NVAR, NVIN, NVOUT, STATUS )

*  Description:
*     The routine returns the number of input and output variables from
*     the transformation described by a compiled module list (CML).

*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     10-MAY-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      INTEGER NMOD              ! The number of modules in the list
      INTEGER NVAR( NMOD + 1 )  ! CML array containing the number of
                                ! variables in the data stream between
                                ! each pair of modules


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      INTEGER NVIN              ! Number of input variables for the
                                ! transformation
      INTEGER NVOUT             ! Number of output variables for the
                                ! transformation


*  Status:
      INTEGER STATUS


*  External References:
*     <declarations for external function references>


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
*     <declarations for local variables>


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Extract the numbers of variables.

*   ...number of input variables to the first module:
      NVIN = NVAR( 1 )

*   ...number of output variables from the last module:
      NVOUT = NVAR( NMOD + 1 )


*   Exit routine.
      END
