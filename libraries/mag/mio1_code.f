      SUBROUTINE MIO1_CODE(CODE, STATUS)
*+
*  Name:
*     MIO1_CODE
 
*  Purpose:
*     In Unix this is a dummy routine which copies its input to its output.
*     Its sole purpose is to make applications built using MIO to work
*     on VMS and Unix systems.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     CALL MIO1_CODE(CODE, STATUS)
 
*  Description:
*     In Unix it simply sets STATUS = CODE
 
*  Arguments:
*     CODE=INTEGER (Given)
*        The input status.
*     STATUS=INTEGER (Returned)
*        The returned corresponding MIO status code.
 
*  Authors:
*     Jack Giddings (ZUVAD::JRG)
*     {enter_new_authors_here}
 
*  History:
*     01-Feb-1983:  Original. (ZUVAD::JRG)
*     18-Dec-1989:  Check only low 16 bits  (RLVAD::AJC)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     14-Jan-1992:  re-written as a dummy routine for unix systems
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}
 
*  Notes:
*     Formerly known as MIO_$CODE.
*
*     This is the Unix version.
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Arguments Given:
      INTEGER CODE              ! Input status
 
*  Arguments Returned:
      INTEGER STATUS            ! Output status
 
      STATUS = CODE
 
      END
