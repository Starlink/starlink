************************************************************************

      SUBROUTINE TETIME ( TLOC, CETIME, ETIME )

*+
*  Name :
*     TETIME
*
*  Purpose :
*     Extract an exposure time from an HDS structure pointed to by the
*     path name CETIME.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL TETIME( TLOC, CETIME, ETIME )
*
*  Description :
*     Extract an exposure time from an HDS structure pointed to by the
*     path name CETIME. CETIME can have substructures seperated by '.'s.
*     If the structure cannot be found the ETIME = 1.0 is returned.
*
*  Arguments :
*     TLOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Top level locator.
*     CETIME = CHARACTER * ( * ) (Given)
*        String containing path to HDS structure.
*     ETIME = REAL (Returned)
*        Exposure time.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-OCT-1989 (NE):
*        Original version.
*     10-AUG-1990 (NE):
*        Add error context
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'

      INCLUDE 'DAT_PAR'


*  Arguments Given :
      CHARACTER * ( DAT__SZLOC ) TLOC
      CHARACTER * ( * ) CETIME

*  Arguments Returned :
      REAL ETIME

*  Local Constants :
      INTEGER MXELEM
      PARAMETER ( MXELEM = 16 )

*  Local Variables :
      INTEGER J, NLEV, STATUS

      CHARACTER TEXT * 80
      CHARACTER * ( DAT__SZLOC ) ALOC, BLOC
      CHARACTER * ( DAT__SZNAM ) COMPS( MXELEM )
*.

*   Initialise the exposure time to 1 just in case
      ETIME = 1.0

*   Set the status to zero
      STATUS = SAI__OK

*   Start a new error context
      CALL ERR_MARK

*   Clone the top level locator
      CALL DAT_CLONE( TLOC, ALOC, STATUS )

*   Seperate the path name into its components
      CALL REF_SPLIT( CETIME, MXELEM, NLEV, COMPS, STATUS )

*   Step through the components to find the exposure time
*   Trust the inherited status to skip routines if there is a problem
      DO J = 1, NLEV
         CALL DAT_FIND( ALOC, COMPS( J ), BLOC, STATUS )
         CALL DAT_ANNUL( ALOC, STATUS )
         CALL DAT_CLONE( BLOC, ALOC, STATUS )
         CALL DAT_ANNUL( BLOC, STATUS )
      ENDDO

*   Extract the exposure time
      CALL DAT_GET0R( ALOC, ETIME, STATUS )
      CALL DAT_ANNUL( ALOC, STATUS )

*   If there are any problems then return an exposure time of 1
      IF ( STATUS .NE. SAI__OK ) THEN
         ETIME = 1.0

*   Otherwise print out the exposure time
      ELSE
         WRITE( TEXT, '(''Exposure time = '', F7.2 )' ) ETIME
         CALL MSG_OUT( ' ', TEXT, STATUS )
      ENDIF

*   Annul any errors in this routine and end the error context
      CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE

      END

* $Id$
