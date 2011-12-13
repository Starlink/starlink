      SUBROUTINE GRP1_MODIF( BSIZE, SLOT1, SLOT2, INDEX, EDEP, EIFILE,
     :                       NADDED, STATUS )
*+
*  Name:
*     GRP1_MODIF

*  Purpose:
*     Copy a basis group to another group

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_MODIF( BSIZE, SLOT1, SLOT2, INDEX, EDEP, EIFILE, NADDED,
*                      STATUS )

*  Description:
*     The contents of the supplied basis group (SLOT1) are inserted
*     into another group (SLOT2) starting at the index specified by
*     INDEX. The supplemental information stored with the names is
*     modified to describe the new names. The supplied depth of
*     indirection (EDEP), and indirection file index (EIFILE) are
*     stored as the DEPTH and FILE attributes for the new names. The
*     GRP identified issued for the basis group is stored as the MODGRP
*     attribute for all the new names, and the index within SLOT1 from
*     which each name was copied is stored as the MODIN attribute of
*     each name.

*  Arguments:
*     BSIZE = INTEGER (Given)
*        The number of elements from the basis group which are to be
*        copied. Elements with indices higher than BSIZE are not copied.
*     SLOT1 = INTEGER (Given)
*        The slot number for the basis group.
*     SLOT2 = INTEGER (Given)
*        The slot number for the group into which the names are to be
*        inserted.
*     INDEX = INTEGER (Given)
*        The index within group SLOT2 at which the first name is to be
*        stored.
*     EDEP = INTEGER (Given)
*        The DEPTH attribute to be stored with each new name in group
*        SLOT2.
*     EIFILE = INTEGER (Given)
*        The index within the FILES array (see routine GRP1_PTIND) at
*        which the the indirection file name is stored. This file name
*        will be returned as the FILE attribute for the new names.
*     NADDED = INTEGER (Returned)
*        The number of names addded to the group SLOT2. This will equal
*        the size of group SLOT1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     26-JAN-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'          ! GRP public constants.
      INCLUDE 'GRP_ERR'          ! GRP error values.

*  Global Variables:
      INCLUDE 'GRP_COM'          ! GRP common blocks.
*        CMN_CHK( GRP__MAXG ) = INTEGER (Read)
*           The GRP identifier issued for each slot.

*  Arguments Given:
      INTEGER BSIZE
      INTEGER SLOT1
      INTEGER SLOT2
      INTEGER INDEX
      INTEGER EDEP
      INTEGER EIFILE

*  Arguments Returned:
      INTEGER NADDED

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GRP1_INIT         ! Initalise GRP common blocks.

*  Local Variables:
      CHARACTER BNAME*(GRP__SZNAM)! Current basis name
      INTEGER DEP                ! Indirection depth for basis name
      INTEGER IFILE              ! Index of indirection file for basis name
      INTEGER II                 ! Loop count
      INTEGER MODGP              ! Basis group GRP id. for basis name
      INTEGER MODIN              ! Basis group index for basis name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of names added to the output group.
      NADDED = 0

*  Loop round each of the names in the basis group.
      DO II = 1, BSIZE

*  Get the next basis name, and store it in the output group.
         CALL GRP1_GTELM( SLOT1, II, BNAME, DEP, IFILE, MODGP,
     :                    MODIN, STATUS )

         CALL GRP1_PTELM( SLOT2, INDEX + NADDED, BNAME, EDEP,
     :                    EIFILE, CMN_CHK( SLOT1 ), II, STATUS )

*  Increment the number of names added to the output group.
         NADDED = NADDED + 1

      END DO

      END
