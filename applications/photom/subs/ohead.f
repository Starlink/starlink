************************************************************************

      SUBROUTINE OHEAD ( NX, NY, CLIP, SEE, STATUS )

*+
*  Name :
*     OHEAD
*
*  Purpose :
*     Write a header block for optimal extraction
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL OHEAD( NX, NY, CLIP, STATUS )
*
*  Description :
*     Write a header block
*
*  Arguments :
*     NX = INTEGER (Given)
*        Size of data array
*     NY = INTEGER (Given)
*        Size of data array
*     CLIP = REAL (Given)
*        Clipping radius
*     SEE = REAL (Given)
*        Approximate seeing in pixels
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     AA: Alasdair Allan (Starlink, Keele University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     25-JAN-1999
*          Original version
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Initialise
*        STATUS so that messages are output.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global parameters:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given :
      INTEGER NX
      INTEGER NY
      REAL CLIP, SEE

*  Status :
      INTEGER STATUS

*  Local Variables :
      CHARACTER * ( MSG__SZMSG ) TEXT
*.

*   Initialise STATUS.
      STATUS = SAI__OK

*   Write out the header to the terminal
      CALL MSG_OUT( ' ', '========================================'//
     :                   '============================', STATUS )

      CALL MSG_OUT( ' ', '         nx       ny        clip        see',
     :              STATUS )

      WRITE( TEXT, '( 2X, 2I9, F11.2, F11.2 )' ) NX, NY, CLIP, SEE
      CALL MSG_OUT( ' ', TEXT, STATUS )

      CALL MSG_OUT( ' ', ' ', STATUS )

      CALL MSG_OUT( ' ', '           x        y      fwhm      '//
     :              'fwhm     rot    code', STATUS )

      END

