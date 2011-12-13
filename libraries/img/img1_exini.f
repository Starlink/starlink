      SUBROUTINE IMG1_EXINI( SLOT, XNAME, CREATE, ESLOT, STATUS )
*+
* Name:
*    IMG1_EXINI

*  Purpose:
*     Initialises IMG_ access to an NDF extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_EXINI( SLOT, XNAME, CREATE, ESLOT, STATUS )

*  Description:
*     This routine checks if an NDF has an extension. If not and
*     "WRITE" access is allowed then an extension will be created. If an
*     extension is created or located then an HDS locator to it is
*     retained for future use.
*
*     If the extension type is "FITS" and "WRITE" access is allowed
*     then a memory resident copy of the FITS block is kept until the
*     NDF is released or until the extension itself is released. If
*     "READ" access only is allowed then the FITS block is mapped. The
*     FITS "block" is a 1-D character array whose elements are of size
*     _CHAR*80. This procedure could be quite slow if the actual
*     "physical" disk file was changed (using DAT_ALTER).
*
*     None of the above is performed if the NDF extension has already
*     been accessed.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number of the NDF.
*     XNAME = CHARACTER * ( * ) (Given)
*        The extension name.
*     CREATE  = LOGICAL (Given)
*        Whether or not the creation of the extension is allowed. If
*        an item is to be read then this should be .FALSE.
*     ESLOT = INTEGER (Returned)
*        The slot number assigned to the extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - If the extension is "FITS" then this is copied into memory if
*     "WRITE access to the NDF is available. Otherwise the FITS block is
*     just mapped.
*     - Appropriate action must be taken to release or renew the NDF
*     extension before an application exits.
*     - "WRITE" access is considered as equivalent to "UPDATE" as
*     otherwise the extension would be cleared. This effect must be
*     performed explicitly if required.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     19-JUL-1994 (PDRAPER):
*        Original version.
*     20-SEP-1994 (PDRAPER):
*        Removed PSX memory allocation calls.
*     15-NOV-1994 (PDRAPER):
*        Now always assumes UPDATE access to an NDF as this is now the
*        IMG default. All the code relating to readonly access is
*        removed.
*     28-NOV-1994 (PDRAPER):
*        Re-track as readonly access to input NDFs is allowed.
*        Re-incorporated the code for mapping FITS blocks.
*     20-APR-1999 (PDRAPER):
*        Modified to use CNF_PVAL to deference C memory pointers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'IMG_CONST'       ! IMG_ constants
      INCLUDE 'IMG_ERR'         ! IMG_ error codes
      INCLUDE 'NDF_PAR'         ! NDF_ constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT parameters
      INCLUDE 'CNF_PAR'         ! CNF parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'         ! IMG Extension Control Block
*        ECB_XNAME( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER * ( NDF__SZXNM ) (Read and Write)
*        The name of the extension
*
*        ECB_XLOC( IMG__MXPAR, IMG__MXEXT ) =
*           CHARACTER ( DAT__SZLOC ) (Write)
*        The locator to the extension.
*
*        ECB_FTSP( IMG__MXPAR ) = INTEGER (Write)
*        Pointer to mapped FITS block.
*
*        ECB_FTSN( IMG__MXPAR ) = INTEGER (Write)
*        Number of entries in the FITS block.

      INCLUDE 'IMG_PCB'         ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      INTEGER SLOT
      CHARACTER * ( * ) XNAME
      LOGICAL CREATE

*  Arguments Returned:
      INTEGER ESLOT

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL IMG1_INIT        ! Initialise common blocks
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR         ! Two case independent character
                                ! strings are the same

*  Local Variables:
      CHARACTER * ( 3 ) END( 1 ) ! 'END' keyword as array element
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to extension
      INTEGER I                 ! Loop variable
      INTEGER NFITS             ! Number of elements in FITS block
      INTEGER NOUT              ! Number of copied FITS records
      INTEGER POINT             ! Pointer to FITS block
      LOGICAL CANMOD            ! If modification of the NDF is allowed
      LOGICAL EXISTS            ! Extension exists
      LOGICAL FITS              ! True if extension is FITS

*  Local Data:
      DATA END / 'END' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set type of extension.
      FITS = CHR_SIMLR( XNAME, 'FITS' )

*  Check if access is already available to this extension.
      DO 1 I = 1, IMG__MXEXT
         IF ( CHR_SIMLR( ECB_XNAME( SLOT, I ), XNAME ) ) THEN

*  Access to this extension is already available so do nothing.
            ESLOT = I
            GO TO 99
         END IF
 1    CONTINUE

*  Check if the extension exists.
      CALL NDF_XSTAT( PCB_INDF( SLOT ), XNAME, EXISTS, STATUS )

*  Check if we can modify it.
      CALL NDF_ISACC( PCB_INDF( SLOT ), 'WRITE', CANMOD, STATUS )

*  Extension must exist, or it must be possible to create one before
*  proceeding.
      IF ( EXISTS .OR. ( CREATE .AND. CANMOD ) ) THEN
         IF ( .NOT. EXISTS ) THEN

*  Extension doesn't exist so create one.
            IF ( FITS ) THEN

*  FITS block. Create this as an array of size 1 with the correct
*  string size. The record needs the special keyword 'END'.
               CALL NDF_XNEW( PCB_INDF( SLOT ), XNAME, '_CHAR*80', 1,
     :                        1, LOC, STATUS )
               CALL DAT_PUT( LOC, '_CHAR*80', 1, 1, END, STATUS )
            ELSE

*  Usual extension is a structure.
               CALL NDF_XNEW( PCB_INDF( SLOT ), XNAME, 'EXTENSION',
     :                        0, 0, LOC, STATUS )
            END IF
         ELSE

*  Extension exists so get a locator to it...
            IF ( CANMOD ) THEN
               CALL NDF_XLOC( PCB_INDF( SLOT ), XNAME, 'UPDATE', LOC,
     :                        STATUS )
            ELSE

*  Cannot write to extension so just use READ access.
               CALL NDF_XLOC( PCB_INDF( SLOT ), XNAME, 'READ', LOC,
     :                        STATUS )
            END IF
         END IF

*  If all's well proceed and attempt to locate a free slot.
         IF ( STATUS .EQ. SAI__OK ) THEN
            ESLOT = 0
            DO 2 I = 1, IMG__MXEXT
               IF ( ECB_XNAME( SLOT, I ) .EQ. ' ' ) THEN
                  ESLOT = I
                  GO TO 3
               END IF
 2          CONTINUE
 3          CONTINUE

*  Has an extension slot been found? If not release one (this results in
*  an inefficiency later but is better than stopping).
            IF ( ESLOT .EQ. 0  ) THEN

*  Release the final slot.
               ESLOT = IMG__MXEXT
               CALL IMG1_FREXT( SLOT, ESLOT, STATUS )

*  and allocate this to the new extension.
            END IF

*  Record the extension locator and its name in the extension control
*  blocks.
            ECB_XNAME( SLOT, ESLOT ) = XNAME
            ECB_XLOC( SLOT, ESLOT ) = LOC

*  If the extension is the "FITS block" then create a copy if access
*  allows modification, otherwise just map it.
            IF ( FITS ) THEN
               CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', POINT, NFITS,
     :                        STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( CANMOD ) THEN

*  Create a copy of the FITS block in memory (this must be copied back
*  using the IMG1_FREXT routine before the application exits). Otherwise
*  we just keep the FITS block mapped.
                     CALL IMG1_CALLO( 80, NFITS, ECB_FTSP( SLOT ),
     :                                STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        ECB_FTSN( SLOT ) = NFITS

*  Copy the FITS block. Note the lengths of the strings are appended
*  after the last genuine argument, this is how most UNIX compilers pass
*  character string lengths.
                        CALL IMG1_FTSCP( %VAL( CNF_PVAL( POINT  ) ),
     :                                   NFITS,
     :                                   %VAL(CNF_PVAL(ECB_FTSP(SLOT))),
     :                                   NOUT, STATUS,
     :                                   %VAL( CNF_CVAL( 80 ) ),
     :                                   %VAL( CNF_CVAL( 80 ) ))

*  And release the real data.
                        CALL DAT_UNMAP( LOC, STATUS )
                     ELSE

*  Failed to allocate memory.
                        ECB_FTSP( SLOT ) = IMG__NOPTR
                        CALL ERR_REP( 'IMG1_EXINI_NOMEM', 'Failed ' //
     :                       'to access FITS extension (memory ' //
     :                       'allocation problems).', STATUS )
                        GO TO 99
                     END IF
                  ELSE

*  Readonly access to extension. Keep FITS block mapped.
                     ECB_FTSP( SLOT ) = POINT
                     ECB_FTSN( SLOT ) = NFITS
                  END IF
               ELSE

*  Failed when mapping FITS block.
                  CALL ERR_REP( 'IMG1_EXINI_NOFITS',
     :                 'Failed to access FITS extension.', STATUS )
                  GO TO 99
               END IF
            END IF
         END IF
      ELSE

*  NDF extension doesn't exist and we're not allowed to create it. Set
*  an error status make a report and exit.
         STATUS = IMG__NOACC
         CALL MSG_SETC( 'XNAME', XNAME )
         CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
         IF ( CANMOD ) THEN
            CALL ERR_REP( 'IMG1_EXINI_NOACC',
     :'The image ^NDF does not contain a ^XNAME extension.', STATUS )
         ELSE
            CALL ERR_REP( 'IMG1_EXINI_NOACC',
     :'The image ^NDF does not contain a ^XNAME extension and one ' //
     :           'cannot be created.', STATUS )
         END IF
      END IF

*  Exit quickly label.
 99   CONTINUE
      END
* $Id$
