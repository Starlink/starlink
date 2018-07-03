      SUBROUTINE IRQ_FIND( INDF, LOCS, XNAME, STATUS )
*+
*  Name:
*     IRQ_FIND

*  Purpose:
*     Find quality name information within an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ_FIND( INDF, LOCS, XNAME, STATUS )

*  Description:
*     A search is made through the extensions contained within the
*     supplied NDF for an HDS structure containing quality name
*     information. Such information is held in an HDS object named
*     QUALITY_NAMES (these objects can be created using IRQ_NEW).  If
*     no such object is found, an error is reported and the status
*     IRQ__NOQNI is returned (if more than one such object is found,
*     an error is reported and the status IRQ__MULT is returned).  The
*     name of the NDF extension in which the object was found is
*     returned in XNAME. An array of five HDS locators is returned which
*     is needed when calling other IRQ routines. The first locator
*     points to a temporary object which holds a cloned identifier for
*     the NDF, the other four point to components of the QUALITY_NAMES
*     structure contained in the NDF.  IRQ_RLSE should be called to
*     annul these locators (and the NDF identifier) when no further
*     access to the NDFs quality names information is required.
*
*     The LOCS argument returned by this routine specifies the NDF
*     which will be operated on by subsequent IRQ routines.
*     Specifically, LOCS determines the bounds of the NDF. Care should
*     therefore be taken that subsequent calls to IRQ routines refer to
*     the NDF specified by the INDF argument to this routine, and not
*     for instance to a section of the NDF which will in general have
*     different bounds.

*  Arguments:
*     INDF = INTEGER (Given)
*        The input NDF.
*     LOCS(5) = CHARACTER * ( * ) (Returned)
*        A set of HDS locators as described above.  The character
*        variables supplied for this argument should have a declared
*        length equal to symbolic constant DAT__SZLOC. These locator
*        are annuled by calling IRQ_RLSE.
*     XNAME = CHARACTER * ( * ) (Returned)
*        The name of the NDF extension in which the quality name
*        information was found. The character variable supplied for
*        this argument should have a declared length equal to symbolic
*        constant DAT__SZNAM.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-JUL-1991 (DSB):
*        Original version.
*     17-JAN-2002 (DSB):
*        Check that NDF extension is an HDS Structure before using
*        it with HDS routines which require a structure.
*     7-MAR-2008 (DSB):
*        Check that NDF extension is a scalar before using it with HDS
*        routines which require a scalar.
*     10-JUL-2008 (TIMJ):
*        Initialise QNLOC and XNAME to resolve valgrind warnings.
*     3-JUL-2018 (DSB):
*        Ensure cloned NDF identifier is annulled if an error occurs.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'NDF_PAR'          ! NDF constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.


*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      CHARACTER LOCS(5)*(*)
      CHARACTER XNAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BAD                ! True if there may be bad pixels in
                                 ! the QUALITY component of the NDF.
      INTEGER CINDF              ! A cloned identifier for the supplied
                                 ! NDF.
      INTEGER I                  ! Loop index.
      INTEGER NFOUND             ! No. of QUALITY_NAMES structures
                                 ! found in the NDF.
      CHARACTER QNLOC*(DAT__SZLOC)! Locator to QUALITY_NAMES structure.
      INTEGER SIZE               ! No. of elements in array
      LOGICAL THERE              ! True if QUALITY_NAMES structure was
                                 ! found in the current extension.
      LOGICAL WRACC              ! True if write access is available to
                                 ! the NDF.
      CHARACTER XLOC*(DAT__SZLOC)! Locator to current extension.
      CHARACTER XN*(DAT__SZNAM)  ! Current extension name.
      INTEGER XNUMB              ! No. of extensions in the NDF.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of QUALITY_NAMES structures found to zero.
      NFOUND = 0
      QNLOC = DAT__NOLOC
      XNAME = ' '

*  Check that the supplied character variables LOCS are long enough to
*  hold HDS locators.
      IF( LEN( LOCS( 1 ) ) .NE. DAT__SZLOC ) THEN
         STATUS = IRQ__LSHRT
         CALL ERR_REP( 'IRQ1_FIND_ERR1',
     :  'IRQ_FIND: Declared length of character variables within LOCS'//
     :  ' is wrong', STATUS )
      END IF

*  Find the number of extensions in the NDF.
      CALL NDF_XNUMB( INDF, XNUMB, STATUS )

*  Loop round each extension.
      DO I = 1, XNUMB

*  Get a locator to the extension.
         CALL NDF_XNAME( INDF, I, XN, STATUS )
         CALL NDF_XLOC( INDF, XN, 'READ', XLOC, STATUS )

*  See if the extension is a scalar structure containing a component
*  named QUALITY_NAMES.
         CALL DAT_STRUC( XLOC, THERE, STATUS )
         CALL DAT_SIZE( XLOC, SIZE, STATUS )
         IF( SIZE .NE. 1 ) THERE = .FALSE.
         IF( THERE ) CALL DAT_THERE( XLOC, IRQ__QINAM, THERE, STATUS )

*  If it does, get a locator to it, return the extension name and
*  increment the number of structures found.
         IF( THERE ) THEN
            CALL DAT_FIND( XLOC, IRQ__QINAM, QNLOC, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               XNAME = XN
               NFOUND = NFOUND + 1
            END IF
         END IF

*  Annul the locator to the extension.
         CALL DAT_ANNUL( XLOC, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

      END DO

*  If all has gone OK, but no suitable structures were found, report an
*  error.
      IF( STATUS .EQ. SAI__OK ) THEN
         IF( NFOUND .EQ. 0 ) THEN
            STATUS = IRQ__NOQNI
            CALL ERR_REP( 'IRQ_FIND_ERR2',
     :                  'IRQ_FIND: No quality names information found.',
     :                    STATUS )

*  If more than one structure was found, report an error.
         ELSE IF( NFOUND .GT. 1 ) THEN
            STATUS = IRQ__MULT
            CALL ERR_REP( 'IRQ_FIND_ERR3',
     :   'IRQ_FIND: NDF contains conflicting quality names information',
     :                    STATUS )

*  If a single QUALITY_NAMES structure was found, check that the
*  structure is complete by checking for the existence of a component
*  called VALID, which should have been the last component to be added
*  when the structure was created by IRQ_NEW.
         ELSE

            CALL DAT_THERE( QNLOC, IRQ__VANAM, THERE, STATUS )

*  If the structure was not completed, check to see if write access
*  is available to the NDF.
            IF( .NOT. THERE ) THEN
               CALL NDF_ISACC( INDF, 'WRITE', WRACC, STATUS )

*  If write access is available, delete the incomplete structure.
               IF( WRACC ) THEN
                  CALL NDF_XLOC( INDF, XNAME, 'UPDATE', XLOC, STATUS )
                  CALL DAT_ERASE( XLOC, IRQ__QINAM, STATUS )
                  CALL DAT_ANNUL( XLOC, STATUS )

*  Report an error indicating that no quality name information has been
*  found.
                  IF( STATUS .EQ. SAI__OK ) THEN
                     STATUS = IRQ__NOQNI
                     CALL ERR_REP( 'IRQ_FIND_ERR4',
     :                  'IRQ_FIND: No quality names information found.',
     :                    STATUS )
                  END IF

*  If write access is not available, report an error but do not attempt
*  to delete the incomplete structure.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = IRQ__BADQN
                  CALL ERR_REP( 'IRQ_FIND_ERR5',
     :             'IRQ_FIND: Invalid quality names information found.',
     :                           STATUS )
               END IF

*  If the structure was OK, create a temporary structure to hold a
*  cloned NDF identifier and find the locators to the QUAL, LAST_USED,
*  NFREE and FREE components of the QUALITY_NAMES structure.  These
*  locators together with the cloned NDF identifier are annulled when
*  IRQ_RLSE is called.
            ELSE
               CALL NDF_CLONE( INDF, CINDF, STATUS )
               CALL IRQ1_TEMP( '_INTEGER', 0, 0, LOCS(1), STATUS )
               CALL DAT_PUT0I( LOCS(1), CINDF, STATUS )

               CALL DAT_FIND( QNLOC, IRQ__QUNAM, LOCS(2), STATUS )
               CALL DAT_FIND( QNLOC, IRQ__LUNAM, LOCS(3), STATUS )
               CALL DAT_FIND( QNLOC, IRQ__NFNAM, LOCS(4), STATUS )
               CALL DAT_FIND( QNLOC, IRQ__FRNAM, LOCS(5), STATUS )

               IF( STATUS .NE. SAI__OK ) THEN
                  CALL NDF_ANNUL( CINDF, STATUS )
                  CALL DAT_PUT0I( LOCS(1), NDF__NOID, STATUS )
               END IF

            END IF
         END IF
      END IF

*  Report an error if BAD pixels may be present in the QUALITY
*  component.
      CALL NDF_BAD( INDF, 'QUALITY', .FALSE., BAD, STATUS )
      IF( BAD .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRQ__QBAD
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRQ_FIND_ERR6',
     :  'IRQ_FIND: The QUALITY component of ^NDF contains BAD pixels',
     :                     STATUS )
      END IF

*  Annul the locator to the quality names information.
      CALL DAT_ANNUL( QNLOC, STATUS )

*  If an error occurred, give context information.
 999  CONTINUE
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRQ_FIND_ERR7',
     :   'IRQ_FIND: Unable to locate usable quality names information'//
     :   ' in NDF ^NDF', STATUS )
      END IF

      END
