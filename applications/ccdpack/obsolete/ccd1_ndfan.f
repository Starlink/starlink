      SUBROUTINE CCD1_NDFAN( STATUS )
*+
*  Name:
*     CCD1_NDFAN

*  Purpose:
*     Annuls locators to NDFs opened by CCDPACK routines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFAN( STATUS )

*  Description:
*     The routine annuls the locators associated with NDFs which have
*     been opened within CCDPACK via the usual routine for NDF lists
*     (or single NDFs), i.e. CCD1_NDFAC and CCD1_NDFAB. This routine
*     should always be called after these routines.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-OCT-1991 (PDRAPER):
*        Original version.
*     2-FEB-1994 (PDRAPER):
*        Now annuls locators instead of closing container files.
*        HDS should now perform this task correctly.
*     11-SEP-1995 (PDRAPER):
*        Bug prevented release of all locators except the first.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK system constants

*  Global Variables:
      INCLUDE 'CCD1_NDFCM'       ! Common block to hold top level NDF
                                 ! locators
*        CCD1_TLOC( CCD1__MXNDF ) = CHARACTER * ( DAT__SZLOC )
*                                                       (Read and Write)
*           Locators to the NDFs accessed.
*        CCD1_NACC = INTEGER (Read and Write)
*           Number of valid locators stored in CCD1_TLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      LOGICAL OK                 ! Locator is valid

*.

*  This code attempts annul locators even when status is set.
*  Begin a new error reporting context.
      CALL ERR_BEGIN( STATUS )

*  Annul all active NDF locators.
      DO 1 I = 1, CCD1_NACC
         CALL DAT_VALID( CCD1_TLOC( I ), OK, STATUS )
         IF ( OK ) THEN
            CALL DAT_ANNUL( CCD1_TLOC( I ), STATUS )
            CCD1_TLOC( I ) = DAT__NOLOC
         END IF
 1    CONTINUE

*  Set the number of unclosed NDFs to zero.
      CCD1_NACC = 0

*  End the error reporting context.
      CALL ERR_END( STATUS )

      END
* $Id$
