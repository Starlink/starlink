      SUBROUTINE IMG1_FREXT( SLOT, ESLOT, STATUS )
*+
* Name:
*    IMG1_FREXT

*  Purpose:
*     Frees the resources associated with an NDF extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_FREXT( SLOT, ESLOT, STATUS )

*  Description:
*     This routine frees up any resources associated with an extension.
*     The NDF is identified by its slot number and the extension by its
*     index. The resources associated with an extension are a record of
*     its name and a locator to it. An extension may also have a
*     pointer to a FITS block or a pointer to a stack locators which
*     trace HDS primitives in the extension. FITS blocks will be
*     memory resident so need to be copied back into the data file.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number of the NDF.
*     ESLOT = INTEGER (Given)
*        The index of the extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine attempts to execute even if status is bad on entry.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     19-JUL-1994 (PDRAPER):
*        Original version.
*     8-SEP-1994 (PDRAPER):
*        Added code to clean up extension traces.
*     20-SEP-1994 (PDRAPER):
*        Removed PSX calls.
*     15-NOV-1994 (PDRAPER):
*        No readonly access now allowed. FITS blocks are always memory
*        copies.
*     28-NOV-1994 (PDRAPER):
*        Back-tracked to allow some readonly access for input NDFs. This
*        is determined by file protections.
*     16-DEC-1996 (PDRAPER):
*        Moved modification of extension size into modified block. 
*        Uninitialized variables were breaking this code on Linux.
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
*           CHARACTER ( DAT__SZLOC ) (Read and Write)
*        The locator to the extension.
*
*        ECB_FTSP( IMG__MXPAR ) = INTEGER (Read and Write)
*        Pointer to mapped FITS block.
*
*        ECB_FTSN( IMG__MXPAR ) = INTEGER (Read and Write)
*        Number of entries in the FITS block.
*
*        ECB_XPSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read and Write)
*        Pointers to the stack of extension locators.
*
*        ECB_XNSTK( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read and Write)
*        The number of locators in an extension stack.
*
*        ECB_XNLEN( IMG__MXPAR, IMG__MXEXT ) = INTEGER (Read and Write)
*        Length of the (hds_) trace of the extension.

      INCLUDE 'IMG_PCB'         ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers
      
*  Arguments Given:
      INTEGER SLOT
      INTEGER ESLOT

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL IMG1_INIT        ! Initialise common blocks
      EXTERNAL IMG1_NCEL
      CHARACTER * ( DAT__SZLOC ) IMG1_NCEL ! Returns element from
                                           ! character array

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Temporary locator
      INTEGER I                 ! Loop variable
      INTEGER IPFITS            ! Pointer to re-mapped FITS block
      INTEGER NFITS             ! Number of FITS records
      INTEGER NOUT              ! Number of copied FITS records
      LOGICAL CANMOD            ! Can modify NDF.
*.

*  Start an error context.
      CALL ERR_BEGIN( STATUS )

*  If the extension is a FITS block, we may need to copy the data back
*  to the NDF.
      IF ( ECB_XNAME( SLOT, ESLOT ) .EQ. 'FITS' ) THEN

*  Check access available to the NDF.
         CALL NDF_ISACC( PCB_INDF( SLOT ), 'WRITE', CANMOD, STATUS )
         IF ( CANMOD ) THEN 
         
*  Need to re-access the NDF FITS block, make sure that it is big enough
*  then copy the contents of the memory resident version into it.
            CALL DAT_ALTER( ECB_XLOC( SLOT, ESLOT ), 1,
     :                      ECB_FTSN( SLOT ), STATUS )
            CALL DAT_MAPV( ECB_XLOC( SLOT, ESLOT ), '_CHAR*80',
     :                    'UPDATE', IPFITS, NFITS, STATUS )

*  Now copy the FITS block.
            CALL IMG1_FTSCP( %VAL( CNF_PVAL( ECB_FTSP( SLOT ) ) ), 
     :                       NFITS, %VAL( CNF_PVAL( IPFITS  ) ), 
     :                       NOUT, STATUS,
     :                       %VAL( 80 ), %VAL( 80 ) )

*  and unmap it.
            CALL DAT_UNMAP( ECB_XLOC( SLOT, ESLOT ), STATUS )
            CALL IMG1_CFREE( ECB_FTSP( SLOT ), STATUS )

*  Check that NOUT is the correct size, if not need to alter the FITS
*  block size (this should always mean a shrinking in size not an
*  enlargement, so unused elements at the end are released, this is a
*  sign that entries have been deleted).
            IF ( NOUT .NE. NFITS ) THEN
               CALL DAT_ALTER( ECB_XLOC( SLOT, ESLOT ), 1, NOUT, 
     :                         STATUS )
            END IF
         ELSE

*  NDF access is readonly so just need to unmap FITS block.
            CALL DAT_UNMAP( ECB_XLOC( SLOT, ESLOT ), STATUS )
         END IF
         ECB_FTSN( SLOT ) = 0
      ELSE

*  Non-FITS extension. Clear the extension trace if this has been taken.
         IF ( ECB_XNSTK( SLOT, ESLOT ) .GE. 0 ) THEN

*  The count may be zero if a extension is traced but has no primitives,
*  check for this state. If 0 locators are present memory still needs to
*  be freed.
            IF ( ECB_XNSTK( SLOT, ESLOT ) .GT. 0 ) THEN
               DO 1 I = 1, ECB_XNSTK( SLOT, ESLOT )
                  LOC = IMG1_NCEL(%VAL(CNF_PVAL(ECB_XPSTK(SLOT,ESLOT))),
     :                            ECB_XNSTK( SLOT, ESLOT ), I,
     :                            STATUS, %VAL( DAT__SZLOC ) )
                  CALL DAT_ANNUL( LOC, STATUS )
 1             CONTINUE
            END IF

*  Release the memory and reset the stack counter to show that this is
*  now not in use.
            CALL IMG1_CFREE( ECB_XPSTK( SLOT, ESLOT ), STATUS )
            ECB_XNSTK( SLOT, ESLOT ) = -1
            ECB_XNLEN( SLOT, ESLOT ) = 0
         END IF
      END IF

*  Annul the extension locator and clear the extension name.
      CALL DAT_ANNUL( ECB_XLOC( SLOT, ESLOT ), STATUS )
      ECB_XNAME( SLOT, ESLOT ) = ' '

*  End the error context.
      CALL ERR_END( STATUS )
      END
* $Id$
