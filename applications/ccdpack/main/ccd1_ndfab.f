      SUBROUTINE CCD1_NDFAB( CNAME, USEEXT, NDFNAM, MAXNDF, FACNAM,
     :                       INGRP, FACTS, NNDF, STATUS )
*+
*  Name:
*     CCD1_NDFAB

*  Purpose:
*     To access a sequence of NDFs and an associated factor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NDFAB( CNAME, USEEXT, NDFNAM, MAXNDF, FACNAM,
*                      INGRP, FACTS, NNDF, STATUS )

*  Description:
*     The routine accesses a sequence of NDFs, using the NDG system
*     whose ADAM parameter name NDFNAM.  An NDG group is returned
*     containing the NDF names.  A maximum number of NDFs MAXNDF may be
*     returned. The actual number of NDFs is returned in NNDF.  After
*     the NDFs have been accessed an attempt is made to get associated
*     "exposure" factors. These may be looked for in the NDF extensions
*     if the USEEXT argument is TRUE, otherwise the user will be
*     prompted for the values. If the user is prompted then they will
*     have the chance to return an exact number of factors or just one
*     (which will be used to default all the factors). The parameter
*     used for this is FACNAM.
*
*     If USEEXT is true then the factors accessed from the NDF
*     extensions will depend on value of the CNAME argument. This should
*     be the name of a valid sub-type of the .TIMES structure (e.g. DARK
*     or FLASH).

*  Arguments:
*     CNAME = CHARACTER * ( * ) (Given)
*        The name of the sub-component of the .TIMES structure of the
*        NDF's CCDPACK extension which contains the factor to access.
*        (Probably one of EXPOSURE DARK FLASH.)
*     USEEXT = LOGICAL (Given)
*         Whether to attempt to get the relative factors from the NDF
*     NDFNAM = CHARACTER * ( * ) (Given)
*        The ADAM parameter name used to prompt for the NDFs.
*     MAXNDF = INTEGER (Given)
*        The maxiumum number of NDFs allowed.
*     FACNAM = CHARACTER * ( * ) (Given)
*        The associated factor name.
*     INGRP = INTEGER (Returned)
*        An NDG identifier for the group of NDFs selected by the user.
*     FACTS( MAXNDF ) = DOUBLE PRECISION (Returned)
*        The factors returned from the user (defaults to one).
*     NNDF = INTEGER (Returned)
*        The number of NDF identifiers returned from user ( less than
*        MAXNDF)
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993-1994 Science & Engineering Research
*     Council. Copyright (C) 2001 Central Laboratory of the Research
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
*     28-MAY-1991 (PDRAPER):
*        Changed from CCD1_NDFAC to also access associated factors.
*     9-JUL-1991 (PDRAPER):
*        Changed to use IRG system.
*     7-OCT-1993 (PDRAPER):
*        Added facility to get factors from NDF extensions
*     2-FEB-1994 (PDRAPER):
*        Added ACCESS argument.
*     13-FEB-2001 (MBT):
*        Modified to work with Sets.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER CNAME * ( * )
      LOGICAL USEEXT
      INTEGER MAXNDF
      CHARACTER NDFNAM * ( * )
      CHARACTER FACNAM * ( * )

*  Arguments Returned:
      INTEGER INGRP
      DOUBLE PRECISION FACTS( MAXNDF )
      INTEGER NNDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER SIZE               ! Number of factors returned
      INTEGER I                  ! Loop variable
      INTEGER INDF               ! NDF identifier
      LOGICAL OK                 ! Extension value ok
      LOGICAL ALLOK              ! Got all extension values

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access a list of NDFs, up to a maximum of MAXNDF.
      CALL CCD1_NDFGL( NDFNAM, 1, MAXNDF, INGRP, NNDF, STATUS )

*  If we're to get the names of the NDFs from the extension then
*  try to do this.
      IF( NNDF .GT. 0 ) THEN
         ALLOK = .TRUE.
         IF ( USEEXT ) THEN

*  Loop reading the values from the NDF extensions.
            DO 1 I = 1, NNDF
               CALL NDG_NDFAS( INGRP, I, 'READ', INDF, STATUS )
               CALL CCG1_FCH1D( INDF, 'TIMES', CNAME, 1,
     :                          FACTS( I ), OK, STATUS )
               CALL NDF_ANNUL( INDF, STATUS )
               ALLOK = ALLOK .AND. OK
 1          CONTINUE
         END IF

*  Check that all values have been extracted. If not then get values
*  through the parameter system
         IF ( .NOT. ALLOK .AND. USEEXT ) THEN
            CALL MSG_SETC( 'CNAME', CNAME )
            CALL MSG_SETC( 'FACNAM', FACNAM )
            CALL CCD1_MSG( ' ',
     :' Warning - failed to get timing factors (.TIMES.^CNAME) '//
     :' from NDF extensions; using values from parameter ^FACNAM',
     : STATUS )
         END IF
         IF ( .NOT. ALLOK .OR. .NOT. USEEXT ) THEN

*  Access an exact number of strings as double precision values, or
*  allow use to return one value which will be expanded to fill all
*  the exquired factors.
            CALL CCG1_IRHLD( FACNAM, NNDF, NNDF, .TRUE., SIZE, FACTS,
     :                       STATUS )
         END IF
      ELSE
         IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CCD1_NDFAB',
     :      '  No NDFs accessed', STATUS )
         END IF
      END IF
      END
* $Id$
