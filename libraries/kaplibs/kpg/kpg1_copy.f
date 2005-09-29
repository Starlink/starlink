      SUBROUTINE KPG1_COPY( TYPE, NEL, IPIN, IPOUT, STATUS )
*+
*  Name:
*     KPG1_COPY

*  Purpose:
*     To copy an array of a given type to another array of the same
*     type.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_COPY( TYPE, NEL, IPIN, IPOUT, STATUS )

*  Description:
*     This routine calls the appropriate version of the VEC_<T>TO<T>
*     routines to do the copy.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type to be copied. Must be one of the HDS numeric
*        types, _BYTE, _UBYTE, _WORD, _UWORD, _INTEGER, _REAL or _DOUBLE.
*     NEL = INTEGER (Given)
*        The number of elements in the vectorised arrays pointed to by
*        IPIN and IPOUT.
*     IPIN = INTEGER (Given)
*        Pointer to the data to be copied.
*     IPOUT = INTEGER (Given and Returned)
*        Pointer to the array to contain the copied data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses array pointers.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1991 (PDRAPER):
*        Original version.
*     29-SEP-2005 (PDRAPER):
*        Converted into KAPLIBS routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER NEL
      INTEGER IPIN

*  Arguments Given and Returned:
      INTEGER IPOUT

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variable:
      INTEGER IERR              ! Not used
      INTEGER NERR              ! Not used
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy data. Note BAD can be set .FALSE. or .TRUE. as that doesn't
*  matter for data of the same type. IERR and NERR can be ignored as 
*  "conversion" is guaranteed.
      IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL VEC_DTOD( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_REAL' ) THEN
         CALL VEC_RTOR( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL VEC_ITOI( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_WORD' ) THEN
         CALL VEC_WTOW( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_UWORD' ) THEN
         CALL VEC_UWTOUW( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                    STATUS )

      ELSE IF( TYPE .EQ. '_BYTE' ) THEN
         CALL VEC_BTOB( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                  %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                  STATUS )

      ELSE IF( TYPE .EQ. '_UBYTE' ) THEN
         CALL VEC_UBTOUB( .FALSE., NEL, %VAL( CNF_PVAL( IPIN ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ), IERR, NERR,
     :                    STATUS )
      ELSE

*  Bad TYPE.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_COPY',
     :   'Error copying array, bad numeric type (possible programming'//
     :   ' error)', STATUS )
      END IF
      END
