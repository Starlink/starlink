      SUBROUTINE PGP1_CHKUN ( UNIT, RUD, STATUS )
*+
*  Name:
*     PGP1_CHKUN

*  Purpose:
*     Get unit descriptor from unit number

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PGP1_CHKUN ( UNIT, RUD, STATUS )

*  Description:
*     Given the PGPLOT Unit Number, the corresponding Unit
*     descriptor is found.

*  Arguments:
*     UNIT = INTEGER (Given)
*        The PGPLOT Unit identifier
*     RUD = INTEGER (Returned)
*        The unit descriptor 
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Algorithm:
*     The Unit number is looked up in the PGPPA Common block.
*     If it is found, then the descriptor corresponding to
*     its position in the table is returned.

*  Authors:
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     28-JAN-1992 (DLT):
*        Original Version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'        ! SAE Symbolic Constants

      INCLUDE 'PAR_PAR'        ! PAR Symbolic Constants

      INCLUDE 'pgp_err'        ! PGP Error codes

      INCLUDE 'pgpenv_par'                   ! PGP Environment Symbolic Constants

*  Import:
      INTEGER UNIT                      ! Unit number

*  Export:
      INTEGER RUD                       ! relative unit descriptor

*  Status return:
      INTEGER STATUS                    ! status return

*  Global variables:
      INCLUDE 'pgppa_cmn'               ! PGP Parameter Table

*  Local variables:
      INTEGER I                         ! Loop index
      LOGICAL DONE                      ! loop controller
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

      DONE = .FALSE.
      I = 1

      DO WHILE ( ( .NOT. DONE ) .AND. ( I .LE. PGP__MXPAR ) )

         IF ( ( .NOT. PFREE(I) ) .AND. ( UNIT .EQ. PDESC(I) ) ) THEN
            RUD = I
            DONE = .TRUE.
         ELSE
            I = I + 1
         ENDIF

      ENDDO

      IF ( .NOT. DONE ) THEN

*      Set status 
*      This is an internal routine and it is known that more helpful
*      messages will be produced by the calling routines.
         STATUS = PGP__UNKPA
      ENDIF

      END
