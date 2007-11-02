      SUBROUTINE NDG_ENDPV( STATUS )
*+
*  Name:
*     NDG_ENDPV

*  Purpose:
*     End an NDF provenance block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ENDPV( STATUS )

*  Description:
*     This routine should be called to mark the end of an NDF 
*     provenance block. The block should have been started by a 
*     matching call to NDG_BEGPV. Note, provenance blocks must 
*     not be nested.
*
*     During a provenance block, a list is maintained of all the 
*     existing NDFs that have been read (either in read or update 
*     mode) during the block. Another list is maintained of all 
*     the NDFs that have been written (either existing NDFs accessed 
*     in update mode or new NDFs) during the block.
*
*     When the block ends, the provenance information within each 
*     NDF in the second list is modified to include all the NDFs in the
*     first list as parents.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-OCT-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Global Variables:
      INTEGER RDGRP              ! Group holding input NDFs
      INTEGER WRGRP              ! Group holding output NDFs
      COMMON /NDG_PRV/ RDGRP, WRGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL NDG1_HNDLR

*  Local Variables:
      CHARACTER RDNDF*(GRP__SZNAM)
      CHARACTER WRNDF*(GRP__SZNAM)
      INTEGER INDF1
      INTEGER INDF2
      INTEGER IR
      INTEGER IW
      INTEGER NR
      INTEGER NW
      INTEGER PLACE
      INTEGER WRGRP2
      INTEGER RDGRP2
      LOGICAL THERE
*.

*  Begin a new error reporting context (we want to clean up even if an
*  error has occurred).
      CALL ERR_BEGIN( STATUS )

*  Indicate that the routine NDG1_HNDLR should no longer be called 
*  whenever an NDF is opened or closed.
      CALL NDF_HNDLR( "READ_EXISTING_NDF", NDG1_HNDLR, .FALSE., STATUS )
      CALL NDF_HNDLR( "WRITE_EXISTING_NDF", NDG1_HNDLR, .FALSE., 
     :                STATUS )
      CALL NDF_HNDLR( "UPDATE_EXISTING_NDF", NDG1_HNDLR, .FALSE., 
     :                STATUS )
      CALL NDF_HNDLR( "OPEN_NEW_NDF", NDG1_HNDLR, .FALSE., STATUS )
      CALL NDF_HNDLR( "CLOSE_NDF", NDG1_HNDLR, .FALSE., STATUS )

*  Indicate that the PROVENANCE extension should be propagated by
*  default when NDF_PROP or NDF_SCOPY is called.
      CALL NDF_TUNE( 1, 'PXTPROVENANCE', STATUS )

*  End the error reporting context.
      CALL ERR_END( STATUS )

*  If no error has occurred, update the provenance information in each
*  NDF writen during the provenance block. 
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Purge duplicate entries from the two groups. These can be caused if
*  the same NDF is opened several times.
         CALL GRP_PURGE( WRGRP, WRGRP2, STATUS )
         CALL GRP_PURGE( RDGRP, RDGRP2, STATUS )

*  Loop round each output NDF
         CALL GRP_GRPSZ( WRGRP2, NW, STATUS )
         DO IW = 1, NW
            CALL GRP_GET( WRGRP2, IW, 1, WRNDF, STATUS )

*  Get an NDF identifier for it.
            CALL NDF_OPEN( DAT__ROOT, WRNDF, 'UPDATE', 'OLD', INDF1, 
     :                     PLACE, STATUS )            

*  If this output NDF has a PROVENANCE extenion, leave it as it is.
            CALL NDF_XSTAT( INDF1, 'PROVENANCE', THERE, STATUS )
            IF( .NOT. THERE ) THEN

*  Otherwise, loop round each input NDF
               CALL GRP_GRPSZ( RDGRP2, NR, STATUS )
               DO IR = 1, NR
                  CALL GRP_GET( RDGRP2, IR, 1, RDNDF, STATUS )

*  Existing NDFs that were opened for UPDATE will be in both groups. Check
*  to make sure we are not establishing an NDF as its own parent.
                  IF( WRNDF .NE. RDNDF ) THEN 

*  Get an NDF identifier for it.
                     CALL NDF_OPEN( DAT__ROOT, RDNDF, 'READ', 'OLD', 
     :                              INDF2, PLACE, STATUS )            

*  Modify the provenance information in INDF1 to include INDF2 as a parent of 
*  INDF1. This also transfers all the ancestors of INDF2 to INDF1.
                     CALL NDG_PTPRV( INDF1, INDF2, ' ', .FALSE., 
     :                               STATUS )

*  Annul the NDF identifiers.
                     CALL NDF_ANNUL( INDF2, STATUS )               
                  END IF
               END DO
            END IF

            CALL NDF_ANNUL( INDF1, STATUS )               
         END DO

*  Delete the GRP groups.
         CALL GRP_DELET( RDGRP2, STATUS )
         CALL GRP_DELET( WRGRP2, STATUS )

      END IF

      CALL GRP_DELET( RDGRP, STATUS )
      CALL GRP_DELET( WRGRP, STATUS )

      END
