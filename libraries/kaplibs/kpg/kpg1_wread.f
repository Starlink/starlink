      SUBROUTINE KPG1_WREAD( LOC, NAME, IAST, STATUS )
*+
*  Name:
*     KPG1_WREAD

*  Purpose:
*     Read an AST Object from an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WREAD( LOC, NAME, IAST, STATUS )

*  Description:
*     This routine reads an AST Object from a component of the supplied
*     HDS object. The component name is specified by the caller. The
*     component must have a type of WCS, must be scalar, and must contain
*     a single one-dimensional array component with name DATA and type _CHAR.
*     AST__NULL is returned in IAST, and no error is reported if the
*     named component does not exist.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        The locator to the HDS object.
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the component within the HDS object to read.
*        If a blank name is supplied, the object itself is used.
*     IAST = INTEGER (Returned)
*        Pointer to the AST Object returned. Returned equal to AST__NULL
*        if no Object can be read.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1998 (DSB):
*        Original version.
*     12-SEP-2005 (TIMJ):
*        Use KPG1_ASRGN rather than KPG1_ASREG
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'KPG_PAR'          ! KPG_ constants
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'KPG_AST'          ! KPG AST common blocks.
*        ASTLC = CHARACTER * ( DAT__SZLOC ) (Write)
*           Locator to HDS _CHAR array holding AST_ data.
*        ASTLN = INTEGER (Write)
*           Next element to use in HDS _CHAR array holding AST_ data.
*        ASTPT = INTEGER (Write)
*           Pointer to mapped HDS _CHAR array holding AST_ data.

*  Arguments Given:
      CHARACTER LOC*(*)
      CHARACTER NAME*(*)

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL KPG1_RDAST        ! Read AST_ data from an HDS object

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) WCSLOC ! Locator to WCS structure
      CHARACTER * ( DAT__SZTYP ) TYPE ! HDS component type string
      INTEGER CHAN               ! Pointer to AST_ Channel
      INTEGER CLEN               ! Character string length
      INTEGER DIM( DAT__MXDIM )  ! HDS object dimensions
      INTEGER NDIM               ! Number of HDS object dimensions
      LOGICAL THERE              ! HDS component exists?

*.

*  Initialise the returned AST Object pointer.
      IAST = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure all KAPPA non-graphical AST IntraMaps are registered.
      CALL KPG1_ASRGN( STATUS )

*  See if the named component is present in the supplied structure.
      IF( NAME .NE. ' ' ) THEN
         CALL DAT_THERE( LOC, NAME, THERE, STATUS )
      ELSE
         THERE = .TRUE.
      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, obtain a locator for it and determine its type and shape.
         IF ( THERE ) THEN

            WCSLOC = DAT__NOLOC
            IF( NAME .NE. ' ' ) THEN
               CALL DAT_FIND( LOC, NAME, WCSLOC, STATUS )
            ELSE
               CALL DAT_CLONE( LOC, WCSLOC, STATUS )
            END IF

            CALL DAT_TYPE( WCSLOC, TYPE, STATUS )
            CALL DAT_SHAPE( WCSLOC, DAT__MXDIM, DIM, NDIM, STATUS )

*  Check that the component is of type 'WCS' and report an error if it
*  is not.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( TYPE .NE. 'WCS' ) THEN
                  STATUS = SAI__ERROR
                  CALL DAT_MSG( 'WCS', WCSLOC )
                  CALL MSG_SETC( 'BADTYPE', TYPE )
                  CALL ERR_REP( 'KPG1_WREAD_WTYPE',
     :                    'The HDS object ^WCS has an invalid type ' //
     :                    'of ''^BADTYPE''; it should be of ' //
     :                    'type ''WCS''.', STATUS )

*  Also check that the component is scalar and report an error if it is
*  not.
               ELSE IF ( NDIM .NE. 0 ) THEN
                  STATUS = SAI__ERROR
                  CALL DAT_MSG( 'WCS', WCSLOC )
                  CALL MSG_SETI( 'BADNDIM', NDIM )
                  CALL ERR_REP( 'KPG1_WREAD_WNDIM',
     :                 'The HDS object ^WCS is ^BADNDIM-dimensional; '//
     :                 'it should be scalar.', STATUS )
               END IF
            END IF

*  See if the WCS structure contains the mandatory DATA component.
            CALL DAT_THERE( WCSLOC, 'DATA', THERE, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If it does not, then report an error.
               IF ( .NOT. THERE ) THEN
                  STATUS = SAI__ERROR
                  CALL DAT_MSG( 'WCS', WCSLOC )
                  CALL ERR_REP( 'KPG1_WREAD_NODAT',
     :                    'The DATA component is missing from the ' //
     :                    'HDS object ^WCS', STATUS )

*  Otherwise, obtain a locator for it, and determine its type and shape.
               ELSE
                  ASTLC = DAT__NOLOC
                  CALL DAT_FIND( WCSLOC, 'DATA', ASTLC, STATUS )
                  CALL DAT_TYPE( ASTLC, TYPE, STATUS )
                  CALL DAT_SHAPE( ASTLC, DAT__MXDIM, DIM, NDIM, STATUS )

*  Check that the DATA component has type _CHAR and report an error if
*  it does not.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( TYPE( : 6 ) .NE. '_CHAR*' ) THEN
                        STATUS = SAI__ERROR
                        CALL DAT_MSG( 'WCS', WCSLOC )
                        CALL MSG_SETC( 'BADTYPE', TYPE )
                        CALL ERR_REP( 'KPG1_WREAD_DTYPE',
     :                          'The DATA component in the HDS ' //
     :                          'object ^WCS has an invalid type ' //
     :                          'of ''^BADTYPE''; it should be of ' //
     :                          'type ''_CHAR''.', STATUS )

*  Check that the DATA component is one-dimensional and report an error
*  if it is not.
                     ELSE IF ( NDIM .NE. 1 ) THEN
                        STATUS = SAI__ERROR
                        CALL DAT_MSG( 'WCS', WCSLOC )
                        CALL MSG_SETI( 'BADNDIM', NDIM )
                        CALL ERR_REP( 'KPG1_WREAD_DNDIM',
     :                          'The DATA component in the HDS ' //
     :                          'object ^WCS is ' //
     :                          '^BADNDIM-dimensional; it should be ' //
     :                          'one-dimensional.', STATUS )

                     END IF
                  END IF

*  Determine the DATA component's character string length and check
*  that it is not too short to hold AST_ data. Report an error if it
*  is.
                  CALL DAT_CLEN( ASTLC, CLEN, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( CLEN .LT. KPG__MLAST ) THEN
                        STATUS = SAI__ERROR
                        CALL DAT_MSG( 'WCS', WCSLOC )
                        CALL MSG_SETI( 'CLEN', CLEN )
                        CALL MSG_SETI( 'MINLEN', KPG__MLAST )
                        CALL ERR_REP( 'KPG1_WREAD_WCDTS',
     :                          'The DATA component in the HDS ' //
     :                          'object ^WCS has a character ' //
     :                          'string length of ^CLEN; it should ' //
     :                          'have a length of at least ^MINLEN.',
     :                          STATUS )
                     END IF
                  END IF

*  Map the DATA component for READ access.
                  CALL DAT_MAP( ASTLC, '_CHAR', 'READ', NDIM, DIM,
     :                          ASTPT, STATUS )

*  Create an AST_ Channel to read from the DATA component. Supply the
*  KPG1_RDAST routine as the "source" routine for extracting the data.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     CHAN = AST_CHANNEL( KPG1_RDAST, AST_NULL, ' ',
     :                                   STATUS )

*  Initialise the index of the first element in the _CHAR array to be
*  used by the source function.
                     ASTLN = 1

*  Read an Object from the Channel, thus transferring the data.
                     IAST = AST_READ( CHAN, STATUS )

*  If an error occurred during data transfer, report a contextual error
*  message.
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL DAT_MSG( 'OBJECT', ASTLC )
                        CALL ERR_REP( 'KPG1_WREAD_READ',
     :                          'Error while reading AST_ data from ' //
     :                          'the HDS object ^OBJECT.', STATUS )
                     END IF

*  Annul the Channel pointer, thus deleting the Channel.
                     CALL AST_ANNUL( CHAN, STATUS )
                  END IF

*  Annul the DATA component locator.
                  CALL DAT_ANNUL( ASTLC, STATUS )
               END IF
            END IF

*  Annul the WCS structure locator.
            CALL DAT_ANNUL( WCSLOC, STATUS )
         END IF
      END IF

*  If an error has occurred, annul the returned AST pointer.
      IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( IAST, STATUS )

      END
