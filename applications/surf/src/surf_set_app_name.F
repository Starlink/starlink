      SUBROUTINE SURF_SET_APP_NAME( TASKNAME, STATUS )
*+
*  Name:
*     SURF_SET_APP_NAME

*  Purpose:
*     Set application name for NDF history

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     PRE-PROCESSED SUBROUTINE

*  Invocation:
*     CALL SURF_SET_APP_NAME( TSKNAME, STATUS )

*  Description:
*     This routine sets the application name to be of the form
*
*         TASKNAME   ( PACKAGE VN.n-n )
*
*     That is, the taskname (which would be the default history
*     if we did not change it) with the package name and the
*     version number. The version number is specified at compile time.

*  Arguments:
*     TASKNAME = _CHAR (Given)
*        Name of the task.
*     STATUS = _INTEGERS (Given & Returned)
*        Global status

*  Authors:
*     Tim Jenness (t.jenness@jach.hawaii.edu)

*  History:
*     $Log$
*     Revision 1.1  1999/05/15 01:45:42  timj
*     First version
*

*  Notes:
*     - This routine should be compiled using the C pre-processor
*       with a define for the PKG_VERS cpp variable. This variable
*       should include the required quotes (since CPP will not change
*       quoted strings). An example could be:
*                 f77 -DPKG_VERS="'5.2-4'" surf_set_app_name.F
*       The V is prepended automatically.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SURF_PAR'        ! Package name
      INCLUDE 'SAE_PAR'         ! Status
      INCLUDE 'PAR_PAR'         ! Size of task name

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*  Arguments Given:
      CHARACTER * (*) TASKNAME

*  Status:
      INTEGER STATUS

*  Local Constants:
      CHARACTER * (10) VERSION         ! Version number of monolith
      PARAMETER ( VERSION = PKG_VERS ) ! PKG_VERS replaced by CPP

*  Local Variables:
      INTEGER I                 ! Loop counter
      INTEGER IPOSN             ! Position in string
      CHARACTER * (40) STRING   ! application string

*.

*     Check status
      IF (STATUS .NE. SAI__OK) RETURN

*     Create the version string
*     Start by storing the task name
      STRING = TASKNAME

*     Explicitly pad the string with ' ' out to PAR__SZNAM
      DO I = CHR_LEN(STRING)+1, PAR__SZNAM
         STRING(I:I) = ' '
      END DO

*     Set append position to PAR__SZNAM
      IPOSN = PAR__SZNAM

*     Append a space and (
      CALL CHR_APPND(' (', STRING, IPOSN)

*     Append the package name
      CALL CHR_APPND(PACKAGE, STRING, IPOSN)
      
*     Append a space and a 'V'
      CALL CHR_APPND(' V', STRING, IPOSN)

*     Append the version string
      CALL CHR_APPND(VERSION, STRING, IPOSN)

*     Close bracket
      CALL CHR_APPND(')', STRING, IPOSN)

*     Set the application name
      CALL NDF_HAPPN( STRING, STATUS)

      END
