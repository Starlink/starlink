 
      SUBROUTINE MIO_MOUNT(DEVICE, ACMODE, STATUS)
*+
*  Name:
*     MIO_MOUNT
 
*  Purpose:
*     Mount tape on drive - dummy in Unix.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     CALL MIO_MOUNT(DEVICE, MODE, STATUS)
 
*  Description:
*     Mount a tape on a tape drive.
 
*  Arguments:
*     DEVICE=CHARACTER*(*) (Given)
*        A character string containing the name of the tape to be mounted.
*     MODE=CHARACTER*(*) (Given)
*        Expression specifying the access mode:  'READ', 'WRITE' or
*        'UPDATE' (read and write), as appropriate.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.
 
*  Authors:
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}
 
*  History:
*     15-Jul-1983:  Original.  (UCL::SLW)
*     22-OCT-1984:  Mount with operator assistance (RLVAD::TGD)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     17-Jan-1992:  Changed to dummy routine for Unix.
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}
 
*  Notes:
*     This is the Unix version.
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Arguments Given:
      CHARACTER*(*) DEVICE      ! Name of tape drive
      CHARACTER*(*) ACMODE      ! access mode
 
*  Arguments Returned:
*    Status return :
      INTEGER STATUS            ! status return
 
 
*.
 
      END
