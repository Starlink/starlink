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
*     {enter_new_authors_here}
*
*  History :
*     25-JAN-1999
*          Original version
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-
      
*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER NX
      INTEGER NY
      REAL CLIP, SEE
      
*  Status :
      INTEGER STATUS

*  Local Variables :
      CHARACTER * 68 TEXT
*.


*   Write out the header to the terminal
      WRITE( TEXT, '(''================================'//
     :                   '===================================='')' )
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE( TEXT, '(''         nx       ny        clip'//
     :                   '        see'')' )    
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE( TEXT, '( 2X, 2I9, F11.2, F11.2 )' )
     :          NX, NY, CLIP, SEE
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE ( TEXT, '('' '')' )
      CALL MSG_OUT( ' ', TEXT, STATUS )
      WRITE( TEXT, '(''           x        y      fwhm'//
     :        '      fwhm     rot    code'')' )
      CALL MSG_OUT( ' ', TEXT, STATUS )
  

      END

