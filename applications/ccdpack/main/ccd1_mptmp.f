      SUBROUTINE CCD1_MPTMP( ID, ACCESS, POINT, STATUS )
*+
*  Name:
*     CCD1_MPTMP

*  Purpose:
*     Maps in temporary workspace created by CCD1_MKTMP.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MPTMP( ID, ACCESS, POINT, STATUS )

*  Description:
*     Use this routine when mapping in non-NDF temporary disk-file
*     space allocated by CCD1_MKTMP. Access is given to vectorised
*     quantities of any numeric type. This routine returns a pointer
*     to the workspace. Unmap or release the workspace using the
*     functions.
*
*         CCD1_UNTMP   - which unmaps the workspace
*         CCD1_FRTMP   - which frees the workspace (unmapping and
*                        releasing the locator)
*
*     Only use these routines or not all at all. The UNTMP and FRTMP
*     routines accept -1 as their first argument (instead of an
*     identifier) this either unmaps ALL workspace or unmaps and annuls
*     ALL workspace (no nesting).

*  Arguments:
*     ID = INTEGER (Given)
*        Identifier to the workspace which is to be mapped in.
*        (Returned by CCD1_MKTMP).
*     ACCESS = CHARACTER * ( * ) (Given)
*        The type of access required. Must be one of READ, UPDATE
*        or WRITE.
*     POINT = INTEGER (Returned)
*        Pointer to the mapped in workspace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1993 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameterisations
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameterisations

*  Global Variables:
      INCLUDE 'CCD1_TMPCM'       ! Temporary workspace common block
*        CCD1_TMPPO( CCD1__MXPNT ) = INTEGER (Read and Write)
*           Array of pointers to any data which is allocated. This is
*           updated with the value of the pointer on mapping and the
*           value is removed on unmapping.
*        CCD1_TMPLO( CCD1__MXPNT ) = CHARACTER (Read)
*           Array of locators to any data objects created by this
*           routine. This is updated when objects are annulled.

*  Arguments Given:
      INTEGER ID
      CHARACTER * ( * ) ACCESS

*  Arguments Returned:
      INTEGER POINT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TYPE * ( DAT__SZTYP )  ! Type of mapped data.
      INTEGER EL                 ! Number of data elements mapped

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the identifier value is valid.
      IF ( ID .LE. 0 ) THEN

*  Invalid identifier.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_MPTMP1',
     :'  CCD1_MPTMP: Invalid workspace identifier', STATUS )

      ELSE

*  Ok check that workspace is not already mapped.
         IF ( CCD1_TMPPO( ID ) .NE. -1 ) THEN

*  Already mapped in.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_MPTMP2',
     :      '  CCD1_MPTMP: Workspace already mapped', STATUS )
         ELSE

*  Ok map in the workspace.
            CALL DAT_TYPE( CCD1_TMPLO( ID ), TYPE, STATUS )
            CALL DAT_MAPV( CCD1_TMPLO( ID ), TYPE, ACCESS, POINT,
     :                     EL, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               CCD1_TMPPO( ID ) = POINT
            ELSE
               POINT = -1
               CALL ERR_REP( 'CCD1_MPTMP3',
     :         '  CCD1_MPTMP: Failed to map in workspace', STATUS )
            END IF
         END IF
      END IF

      END
* $Id$
