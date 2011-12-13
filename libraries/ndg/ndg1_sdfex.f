      SUBROUTINE NDG1_SDFEX( IGRP1, RECURS, IGRPD, IGRPB, IGRPT, IGRPH,
     :                       IGRPS, LOC, DIR, NAM, TYP, SLICE, FOUND,
     :                       STATUS )
*+
*  Name:
*     NDG1_SDFEX

*  Purpose:
*     Searches for NDFs within an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_SDFEX( IGRP1, RECURS, IGRPD, IGRPB, IGRPT, IGRPH, IGRPS,
*                      LOC, DIR, NAM, TYP, SLICE, FOUND, STATUS )

*  Description:
*     The supplied HDS object is searched for NDFs, and the paths to any
*     NDFs found within it are appended to the end of group IGRP.
*     Here, an NDF is defined as an HDS structure containing a component
*     called DATA_ARRAY which can be accessed by the ARY library.

*     Note, if RECURS is .FALSE., only NDFs stored explicitly within the
*     supplied object are included in the returned group (i.e. NDFs within
*     sub-components are ignored).
*
*     The supplied fields are stored in the other groups for each found
*     NDF.

*  Arguments:
*     IGRP = INTEGER (Given)
*        An identifier for the group to which the NDF paths should
*        be appended.
*     RECURS = LOGICAL (Given)
*        Indicates if the search should recurse through each structure to
*        find all NDFs within the supplied object, no matter where they
*        are located. If RECUR is .TRUE. the returned paths in IGRP are
*        ordered by depth within the supplied structure. The NDFs near
*        the top of the structure come before those lower down in the
*        structure.
*     IGRPD = INTEGER (Given)
*        An identifier for the group to which the directory field for each
*        NDF found should be appended.
*     IGRPB = INTEGER (Given)
*        An identifier for the group to which the file base name for each
*        NDF found should be appended.
*     IGRPT = INTEGER (Given)
*        An identifier for the group to which the file type for each
*        NDF found should be appended.
*     IGRPH = INTEGER (Given)
*        An identifier for the group to which the HDS component path for
*        each NDF found should be appended.
*     IGRPS = INTEGER (Given)
*        An identifier for the group to which the NDF slice specification
*        for each NDF found should be appended.
*     LOC = CHARACTER * ( * ) (Given)
*        The locator to the object to be searched for NDFs.
*     DIR = CHARACTER * ( * ) (Given)
*        The directory field to store in IGRPD.
*     NAM = CHARACTER * ( * ) (Given)
*        The filer base name field to store in IGRPB.
*     TYP = CHARACTER * ( * ) (Given)
*        The file type field to store in IGRPT.
*     SLICE = CHARACTER * ( * ) (Given)
*        An NDF slice specification to append to the path of each NDF
*        found in the supplied object. This is stored in IGRPS (but not
*        IGRPH).
*     FOUND = LOGICAL (Given and Returned)
*        Returned .TRUE. if one or more NDFs were found in the supplied
*        object. Returned unchanged otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     {enter_new_authors_here}

*  History:
*     15-FEB-1999 (DSB):
*        Original version.
*     21-DEC-1999 (DSB):
*        Changed to remove recursive searching through components of the
*        supplied object. Only NDFs stored directly within the supplied
*        object are now returned.
*     2-APR-2001 (DSB):
*        Changed definition of an NDF to "an HDS structure containing
*        a component called DATA_ARRAY which can be accessed by the ARY
*        library".
*     12-FEB-2009 (DSB):
*        Added argument RECURS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'DAT_PAR'          ! HDS constants.

*  Arguments Given:
      INTEGER IGRP1
      LOGICAL RECURS
      INTEGER IGRPD
      INTEGER IGRPB
      INTEGER IGRPT
      INTEGER IGRPH
      INTEGER IGRPS
      CHARACTER LOC*(*)
      CHARACTER DIR*(*)
      CHARACTER NAM*(*)
      CHARACTER TYP*(*)
      CHARACTER SLICE*(*)

*  Arguments Given and Returned:
      LOGICAL FOUND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BN2*(GRP__SZNAM) ! File base name
      CHARACTER CLOC*(DAT__SZLOC)! Locator to component or array cell
      CHARACTER DIR2*(GRP__SZFNM)! Full directory spec
      CHARACTER PATH*(GRP__SZNAM)! Path to NDF object
      CHARACTER PATH2*(GRP__SZNAM)! Path to NDF object with full dir path
      CHARACTER SEC2*1           ! NDF section (should always be blank)
      CHARACTER SUF2*(GRP__SZFNM)! Suffix (eg HDS comp. path)
      CHARACTER VLOC*(DAT__SZLOC)! Locator to vectorised array
      CHARACTER XLOC*(DAT__SZLOC)! Locator to next object to be checked
      INTEGER DIM( DAT__MXDIM )  ! Dimensions of array component
      INTEGER DOT                ! Index of first "."
      INTEGER IARY               ! ARY identifier for array structure
      INTEGER IAT                ! Index of end of directory field
      INTEGER ICELL              ! Cell index
      INTEGER ICOMP              ! Component index
      INTEGER IGRP2              ! Group holding locators to be checked
      INTEGER ILOC               ! No. of locators checked so far
      INTEGER IPATH              ! Index of start of HDS component path
      INTEGER LPATH              ! Used length of PATH
      INTEGER LPATH2             ! Used length of PATH2
      INTEGER NCELL              ! No. of cells in array
      INTEGER NCOMP              ! No. of components in object
      INTEGER NDIM               ! No. of dimensions in array component
      INTEGER NLOC               ! No. of locators in group IGRP2
      INTEGER PAR                ! INdex of first ")"
      LOGICAL CHECK              ! Check the current structure for NDFs?
      LOGICAL ISANDF             ! Is object an NDF?
      LOGICAL STRUCT             ! Is object a structure?
      LOGICAL SUPPLD             ! Is supplied object currently being checked?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a GRP group to hold locators to the objects to be checked.
      CALL GRP_NEW( 'Locators', IGRP2, STATUS )

*  Store a clone of the locator for the supplied object in the group.
*  This is initially the only object to be checked. If this object is
*  not an NDF but is found to contain sub-structures, locators for these
*  will be added to the end of the group and will be checked in their turn.
      CALL DAT_CLONE( LOC, XLOC, STATUS )
      CALL GRP_PUT( IGRP2, 1, XLOC, 0, STATUS )

*  Indicate that the group currently contains one locator.
      NLOC = 1

*  Initialise the number of locators checked so far.
      ILOC = 0

*  Indicate that we are about to check the supplied object.
      SUPPLD = .TRUE.

*  Loop until the last locator in the group has been checked.
      DO WHILE( ILOC .LT. NLOC .AND. STATUS .EQ. SAI__OK )

*  Get the next locator to be checked.
         ILOC = ILOC + 1
         CALL GRP_GET( IGRP2, ILOC, 1, XLOC, STATUS )

*  Only check structures (not primitive values).
         CALL DAT_STRUC( XLOC, STRUCT, STATUS )
         IF( STRUCT .AND. STATUS .EQ. SAI__OK ) THEN

*  See if this object contains a component called DATA_ARRAY which can be
*  accessed by the ARY library. If so, the object is assumed to be an NDF.
            CALL ARY_FIND( XLOC, 'DATA_ARRAY', IARY, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL ARY_ANNUL( IARY, STATUS )
               ISANDF = .TRUE.
            ELSE
               CALL ERR_ANNUL( STATUS )
               ISANDF = .FALSE.
            END IF

*  If so, just add the full object path to the end of the supplied group.
            IF( ISANDF ) THEN

*  Assign the object path to a message token.
               CALL DAT_MSG( 'PATH', XLOC )

*  Store the message token in a local variable.
               CALL MSG_LOAD( ' ', '^PATH', PATH2, LPATH2, STATUS )

*  Split the object path up into directory, basename, suffix and section
*  (the section specified will be blank since DAT_MSG does not know about
*  NDF sections).
               CALL NDG1_FPARS( PATH2( : LPATH2 ), 0, DIR2, BN2, SUF2,
     :                          SEC2, STATUS )

*  DAT_MSG always puts in a full directory specification, even if the NDF
*  is in the current working directory. To avoid this, reconstruct the
*  object path, replacing the directory path with the supplied directory
*  path (which will be blank if the NDF is in the current working
*  directory). Note the index of the first character in the HDS component
*  path in this string. This is the first character following the file
*  base name.
               PATH = ' '
               LPATH = 0
               CALL CHR_APPND( DIR, PATH, LPATH )
               CALL CHR_APPND( BN2, PATH, LPATH )
               IPATH = LPATH + 1
               CALL CHR_APPND( SUF2, PATH, LPATH )

*  Save the individual fields in the supplied groups.
               CALL GRP_PUT( IGRPD, 1, DIR, 0, STATUS )
               CALL GRP_PUT( IGRPB, 1, NAM, 0, STATUS )
               CALL GRP_PUT( IGRPT, 1, TYP, 0, STATUS )
               IF( IPATH .LE. LPATH ) THEN
                  CALL GRP_PUT( IGRPH, 1, PATH( IPATH : ), 0, STATUS )
               ELSE
                  CALL GRP_PUT( IGRPH, 1, ' ', 0, STATUS )
               END IF
               CALL GRP_PUT( IGRPS, 1, SLICE, 0, STATUS )

*  Append any supplied slice spec. to the path.
               CALL CHR_APPND( SLICE, PATH, LPATH )

*  Save the full NDF spec. in the supplied group.
               CALL GRP_PUT( IGRP1, 1, PATH( : LPATH ), 0, STATUS )
               FOUND = .TRUE.

*  Decide if the NDF structure should be checked for contained NDFs.
               CHECK = RECURS

*  If the current structure is not an NDF, continue to check it if it is
*  the supplied object or if we are searching recursively through the entire
*  structure.
            ELSE
               CHECK = ( SUPPLD .OR. RECURS )
            END IF

*  If we are checking the current structure for further NDFs...
            IF( CHECK ) THEN

*  See if it is a scalar or an array.
               CALL DAT_SHAPE( XLOC, DAT__MXDIM, DIM, NDIM, STATUS )

*  First deal with scalars...
               IF( NDIM .EQ. 0 ) THEN

*  Find the number of components in the object.
                  CALL DAT_NCOMP( XLOC, NCOMP, STATUS )

*  Add a locator for each component to the end of the group of locators
*  to be checked.
                  DO ICOMP = 1, NCOMP
                     CALL DAT_INDEX( XLOC, ICOMP, CLOC, STATUS )
                     CALL GRP_PUT( IGRP2, 1, CLOC, 0, STATUS )
                  END DO

*  Increment the number of locators in the group.
                  NLOC = NLOC + NCOMP

*  We only check arrays if we are recursing.
               ELSE IF( RECURS ) THEN

*  Vectorise the array.
                  CALL DAT_VEC( XLOC, VLOC, STATUS )

*  Add a locator for each cell to the end of the group of locators
*  to be checked.
                  CALL DAT_SIZE( VLOC, NCOMP, STATUS )
                  DO ICOMP = 1, NCOMP
                     CALL DAT_CELL( VLOC, 1, ICOMP, CLOC, STATUS )
                     CALL GRP_PUT( IGRP2, 1, CLOC, 0, STATUS )
                  END DO

*  Free the vectorised array locator.
                  CALL DAT_ANNUL( VLOC, STATUS )

*  Increment the number of locators in the group.
                  NLOC = NLOC + NCOMP

               END IF

            END IF

         END IF

*  Indicate we are no longer checking the supplied object.
         SUPPLD = .FALSE.

*  Annul the locator just checked.
         CALL DAT_ANNUL( XLOC, STATUS )

      END DO

*  Delete the GRP group.
      CALL GRP_DELET( IGRP2, STATUS )

      END
