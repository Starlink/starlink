      SUBROUTINE DSA_SET_STRUCT_VAR (VARIABLE,VALUE,STATUS)
*+
*  Name:
*     DSA_SET_STRUCT_VAR

*  Purpose:
*     Set a variable used in a structure definition.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_SET_STRUCT_VAR( VARIABLE, VALUE, STATUS )

*  Description:
*     Structure definitions read in by DSA_READ_STRUCT_DEF often have
*     variable parameters that control the details of the structure -
*     the number of elements in an array, for example. These variables
*     may be set by this routine, which should be called before the
*     structure definition is used, but after it has been read.

*  Arguments:
*     VARIABLE = CHARACTER * ( * ) (Given)
*        The name of the variable to be set. Case is not significant.
*     VALUE = CHARACTER * ( * ) (Given)
*        The string to be used in place of the variable when the
*        definition is used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     31 Aug 1987 (ks):
*        Original version.
*     21 Aug 1992 (ks):
*        Automatic portability modifications ("INCLUDE" syntax etc)
*        made.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     04 Mar 1996 (hme):
*        FDA library.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'DSA_STRUCTURE'    ! Global structure variables

*  Arguments Given:
      CHARACTER * ( * ) VARIABLE
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FOUND              ! Indicates variable found in common
      INTEGER I                  ! Loop index through symbol names
      CHARACTER * ( 32 ) SYMBOL  ! Upper case version of symbol name
      CHARACTER * ( 32 ) VAR_UC  ! Upper case version of VARIABLE

*.

*  Return immediately on bad status
      IF ( STATUS .NE. 0 ) RETURN

*  We will do all comparisions in upper case
      VAR_UC = VARIABLE
      CALL CHR_UCASE( VAR_UC )

*  Look through the variables in the common tables, and see if
*  we can set the one specified.
      FOUND = .FALSE.
      DO I = 1, SYMBOLS_DEFINED
         IF ( SYMBOL_STATE(I) .NE. EQUATE ) THEN
            SYMBOL = SYMBOL_NAMES(I)
            CALL CHR_UCASE( SYMBOL )
            IF ( SYMBOL .EQ. VAR_UC ) THEN
               FOUND = .TRUE.
               SYMBOL_VALUES(I) = VALUE
               SYMBOL_STATE(I)  = DEFINED_VAR
               GO TO 320     ! Break out of I loop
            END IF
         END IF
      END DO
  320 CONTINUE

*  Error if not found
      IF ( .NOT. FOUND ) THEN
         CALL ERR_MARK
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'FDA_T027', VAR_UC )
            CALL ERR_REP( 'FDA_E079', 'DSA_SET_STRUCT_VAR: ' //
     :         'Unable to set the structure variable ^FDA_T027, ' //
     :         'since it was not defined as a variable.', STATUS )
            CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE
         STATUS = 1
      END IF

      END
