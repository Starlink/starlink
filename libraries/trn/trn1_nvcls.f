      SUBROUTINE TRN1_NVCLS( NVIN, NVOUT, CLASS, STATUS )








*+
*  Name:
*     TRN1_NVCLS

*  Purpose:
*     combine numbers of variables with a classification.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_NVCLS( NVIN, NVOUT, CLASS, STATUS )

*  Description:
*     The routine modifies the logical classification array for a
*     uni-directional transformation to reflect additional constraints
*     imposed by the numbers of input and output variables used by the
*     transformation.

*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'TRN_PAR'          ! TRN_ public constants


*  Arguments Given:
      INTEGER NVIN              ! Number of input variables
      INTEGER NVOUT             ! Number of output variables


*  Arguments Given and Returned:
      LOGICAL CLASS( TRN__MXCLS )
                                ! Classification array to modify


*  Arguments Returned:
*     <declarations and descriptions for exported arguments>


*  Status:
      INTEGER STATUS            ! Error status


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


*   The classification array only needs modification if the
*   transformation reduces the number of variables.
      IF( NVOUT .LT. NVIN ) THEN


*   In this case, the transformation cannot be isotropic, nor can any
*   properties of the determinant of its Jacobian matrix be defined.
*   Clear the appropriate elements of the classification array.
        CLASS( TRN__ISOT ) = .FALSE.
        CLASS( TRN__POSDT ) = .FALSE.
        CLASS( TRN__NEGDT ) = .FALSE.
        CLASS( TRN__CONDT ) = .FALSE.
        CLASS( TRN__UNIDT ) = .FALSE.
      ENDIF


*   Exit routine.
      END
