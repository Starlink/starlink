      SUBROUTINE CCD1_NDFGB( CNAME, USEEXT, NDFNAM, FACNAM,
     :                       NDFGID, FACGID, NNDF, STATUS )
*+
*  Name:
*     CCD1_NDFGB

*  Purpose:
*     To access a group of NDFs and an associated factor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFGB( CNAME, USEEXT, NDFNAM, FACNAM, NDFGID,
*                      FACGID, NNDF, STATUS )

*  Description:
*     The routine accesses an NDG group of NDF names, via the ADAM
*     parameter name NDFNAM. The number of NDFs in the NDG group is
*     NNDF. After the NDF names have been accessed an attempt is made
*     to get associated "exposure" factors. These may be looked for in
*     the NDF extensions if the USEEXT argument is TRUE, otherwise the
*     user will be prompted for the values. If the user is prompted
*     then they will have the chance to return an exact number of
*     factors or just one (which will be used to default all the
*     factors). The parameter used for this is FACNAM.
*
*     If USEEXT is true then the factors accessed from the NDF
*     extensions will depend on value of the CNAME argument. This
*     should be the name of a valid sub-type of the .TIMES structure
*     (e.g. DARK or FLASH).

*  Arguments:
*     CNAME = CHARACTER * ( * ) (Given)
*        The name of the sub-component of the .TIMES structure of the
*        NDF's CCDPACK extension which contains the factor to access.
*        (Probably one of EXPOSURE, DARK or FLASH.)
*     USEEXT = LOGICAL (Given and Returned)
*         Whether to attempt to get the relative factors from the NDF
*         extensions. This is set false on exit if the values were not
*         actually gotten from the NDF extensions.
*     NDFNAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter name used to prompt for the NDFs.
*     FACNAM = CHARACTER * ( * ) (Given)
*        The associated factor name.
*     NDFGID = INTEGER (Returned)
*        The NDG group identifier for the input NDFs.
*     FACGID = INTEGER (Returned)
*        The GRP group identifier for the string representation of the
*        associated parameters.
*     NNDF = INTEGER (Returned)
*        The number of NDF identifiers returned from user.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1997, 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUL-1991 (PDRAPER):
*        Original Version
*     21-NOV-1991 (PDRAPER):
*        Added the one return capability.
*     5-JAN-1994 (PDRAPER):
*        Added capability to look in NDF extension for associated
*        values.
*     2-FEB-1994 (PDRAPER):
*        Added ACCESS argument.
*     3-MAR-1997 (PDRAPER):
*        Removed LOC argument from IRG_NDFEX call.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'PRM_PAR'          ! Primdat constants (VAL__)

*  Arguments Given:
      CHARACTER * ( * ) CNAME
      CHARACTER * ( * ) NDFNAM
      CHARACTER * ( * ) FACNAM

*  Arguments Given and Returned:
      LOGICAL USEEXT

*  Arguments Returned:
      INTEGER NDFGID
      INTEGER FACGID
      INTEGER NNDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( VAL__SZD ) CFACT ! Character version of factor
      DOUBLE PRECISION FACTOR    ! Value of factor obtained from NDF extension
      INTEGER I                  ! Loop variable
      INTEGER NCHAR              ! Number of characters in string
      INTEGER NDFID              ! NDF identifier
      INTEGER SIZE               ! Number of factors returned
      LOGICAL ALLOK              ! Got all extension values
      LOGICAL OK                 ! Extension value ok

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access a list of NDFs.
      CALL CCD1_NDFGR( NDFNAM, NDFGID, NNDF, STATUS )

*  If we're to get the names of the NDFs from the extension then
*  try to do this.
      IF( NNDF .GT. 0 ) THEN
         ALLOK = .TRUE.
         IF ( USEEXT ) THEN

*  Create a GRP group to contain the values.
            CALL GRP_NEW( 'CCD1_NDFGB:FACTS', FACGID, STATUS )

*  Loop reading the values from the NDF extensions. Need to get NDF
*  identifiers for these.
            DO 1 I = 1, NNDF
               CALL NDG_NDFAS( NDFGID, I, 'READ', NDFID, STATUS )
               CALL CCG1_FCH1D( NDFID, 'TIMES', CNAME, 1,
     :                          FACTOR, OK, STATUS )
               ALLOK = ALLOK .AND. OK
               IF ( OK ) THEN

*  Enter value into group.
                  CALL CHR_DTOC( FACTOR, CFACT, NCHAR )
                  CALL GRP_PUT( FACGID, 1, CFACT( 1 : NCHAR ), 0,
     :                          STATUS )
               END IF

*  Release the NDF.
               CALL NDF_ANNUL( NDFID, STATUS )
 1          CONTINUE
         END IF

*  Check that all values have been extracted. If not then get values
*  through the parameter system
         IF ( .NOT. ALLOK .AND. USEEXT ) THEN
            CALL MSG_SETC( 'CNAME', CNAME )
            CALL MSG_SETC( 'FACNAM', FACNAM )
            CALL CCD1_MSG( ' ',
     :' Warning - failed to get timing factors (.TIMES.^CNAME) '//
     :' from NDF extensions. Using values from parameter ^FACNAM.',
     : STATUS )

*  Delete the group in preparation for the creation of a new one.
            CALL CCD1_GRDEL( FACGID, STATUS )

*  Set USEEXT to false to indicate this.
            USEEXT = .FALSE.
         END IF
         IF ( .NOT. USEEXT ) THEN

*  Access an exact number of strings as double precision values, or
*  allow user to return one value which will be expanded to fill all
*  the exquired factors.
            CALL CCG1_IRHGD( FACNAM, NNDF, NNDF, .TRUE., SIZE, FACGID,
     :                       STATUS )
         END IF
      ELSE
         IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_NDFGB',
     :      '  No NDFs accessed', STATUS )
         END IF
      END IF
      END
* $Id$
