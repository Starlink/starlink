      SUBROUTINE ECH_SET_CONTEXT( TYPE, DESCRIPTION )
*+
*  Name:
*     ECHOMOP - ECH_SET_CONTEXT

*  Purpose:
*     Sets a context variable as requested.

*  Description:
*     This routine sets the context variables as requested.
*     Context variables are used to describe large scale operating conditions
*     such as BATCH mode, graphics availability, monolith control etc.
*     A context variable also records the currently executing module for
*     in-depth tracing purposes.

*  Invocation:
*     CALL ECH_SET_CONTEXT( TYPE, DESCRIPTION )

*  Arguments:
*     TYPE = CHAR (Given)
*        Type of context variable.
*     DESCRIPTION = CHAR (Given)
*        Value of context variable.

*  Method:
*     If context is 'PROBLEM' then
*      If problem is not the same as the last one (if any) then
*        If memory-exhaustion, dump details of memory usage to a file
*        Access HELP library for detailed advice on problem
*      Else briefly report a repeated problem
*      Endif
*     Else if 'REWIND'ing to repeat an option then
*        Determine option number to be repeated
*     Else if setting a 'CONTROL' context then
*        If monolith, set flag
*     Else if 'ROUTINE' context then
*        If database creator, set special flag allowing file creation
*     Endif

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_CONTEXT.INC'
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_ECHOMOP.INC'
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      CHARACTER*( * ) TYPE
      CHARACTER*( * ) DESCRIPTION

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER I
      INTEGER ILEN

      CHARACTER*80 WORK_STRING
      CHARACTER*64 LAST_PROBLEM
      CHARACTER*64 LOC_DESC
      COMMON / ECH_SET_CONT / LAST_PROBLEM

*  Functions Called:
      INTEGER CHR_LEN

*  Data Statements:
      DATA LAST_PROBLEM  / 'UNINITIALISED' /
*.

*   If problem...
      IF ( TYPE .EQ. 'PROBLEM' ) THEN
         IF ( DESCRIPTION .NE. LAST_PROBLEM ) THEN
            LAST_PROBLEM = DESCRIPTION
            LOC_DESC = DESCRIPTION
            ILEN = CHR_LEN( DESCRIPTION )
            DO I = 2, ILEN - 1
               IF ( LOC_DESC( I : I ) .EQ.  ' ' ) THEN
                  IF ( LOC_DESC( I - 1 : I - 1 ) .NE.  ' ' .AND.
     :                 LOC_DESC( I + 1 : I + 1 ) .NE.  ' ' ) THEN
                     LOC_DESC( I : I ) = '_'
                  END IF
               END IF
            END DO

*        If promblem is memory exhaustion, dump details of memory usage to
*        a file.
            IF ( DESCRIPTION .EQ. 'VMem exhausted' )
     :         CALL ECH_DMP_MEM_USE( STATUS )

*        Access HELP library for advice on problem.
            WORK_STRING = 'Problem ' // LOC_DESC
            CALL ECH_HELP( WORK_STRING, .FALSE., STATUS )

         ELSE
            REPORT_STRING = ' Problem repeated: ' //
     :            DESCRIPTION( :CHR_LEN( DESCRIPTION ) ) // '.'
            CALL ECH_REPORT( 0, REPORT_STRING )
         END IF

*  If 'REWIND'ing to repeat an option then find option number to be repeated.
      ELSE IF ( TYPE .EQ. 'REWIND' ) THEN
         DO I = 1, NUM_OPTIONS
            IF ( OPTION_MODULE( I ) .EQ. DESCRIPTION ) THEN
               CONTEXT_MODE = I
               GO TO 100
            END IF
         END DO
 100     CONTINUE

      ELSE IF ( TYPE .EQ. 'CONTROL' ) THEN
         IF ( DESCRIPTION .EQ. 'MONOLITH' ) THEN
            CONTEXT_MODE = CTX_ECHOMOP_SHELL
         END IF

      ELSE IF ( TYPE .EQ. 'ROUTINE' ) THEN

*     If database creator, set special flag allowing file creation.
         IF ( DESCRIPTION .EQ. 'ECH_FTRDB' ) THEN
            CONTEXT_MODE = CTX_FTRDB_CREATOR
         END IF
      END IF

      END
