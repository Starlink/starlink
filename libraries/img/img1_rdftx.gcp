      SUBROUTINE IMG1_RDFT<T>( SLOT, ITEM, NOCCUR, VALUE, STATUS )
*+
* Name:
*    IMG1_RDFTx

*  Purpose:
*    Gets the value of a FITS item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG1_RDFTx( SLOT, ITEM, NOCCUR, VALUE, STATUS )

*  Description:
*     This routine returns the value associated with an occurrence of a
*     FITS keyword. A FITS block must have been already associated with
*     the indicated NDF. If the keyword is not located in the FITS block
*     then no error is reported and the value of VALUE remains
*     unchanged.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The slot number of the NDF.
*     ITEM = CHARACTER * ( * ) (Given)
*        The FITS keyword. This may be heirarchical.
*     NOCCUR = INTEGER (Given)
*        The occurrence of the keyword to return (less than equal to one
*        returns the first occurrence).
*     VALUE = <COMM> (Given and Returned)
*        The value associated with the keyword (if located).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     20-JUL-1994 (PDRAPER):
*        Original version.
*     1-SEP-1994 (PDRAPER):
*        Now doesn't report an error when fails to locate keyword.
*     20-APR-1999 (PDRAPER):
*        Modified to use CNF_PVAL to deference C memory pointers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'CNF_PAR'          ! CNF parameters

*  Global Variables:
      INCLUDE 'IMG_ECB'          ! IMG Extension Control Block
*        ECB_FTSP( IMG__MXPAR ) = INTEGER (Read)
*        Pointer to mapped FITS block.
*
*        ECB_FTSN( IMG__MXPAR ) = INTEGER (Read)
*        Number of entries in the FITS block.

      INCLUDE 'IMG_PCB'
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*        NDF identifiers.

*   Arguments Given:
      INTEGER SLOT
      CHARACTER * ( * ) ITEM
      INTEGER NOCCUR

*  Arguments Returned:
      <TYPE> VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL IMG1_INIT         ! Initialise common blocks

*  Local Variables:
      LOGICAL THERE              ! Item has been located in FITS block
      INTEGER AT                 ! Index of keyword if located

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Pass on the work to the appropriate routine. Note the %VAL(80)
*  follows the last genuine argument. This is the usual method used by
*  compilers for passing the lengths of strings on UNIX.
      CALL IMG1_GKEY<T>( ECB_FTSN( SLOT ), 
     :                   %VAL( CNF_PVAL( ECB_FTSP( SLOT ) ) ), 1,
     :                   ITEM, NOCCUR, THERE, VALUE, AT, STATUS,
     :                   %VAL( 80 ) )
      IF ( STATUS .NE. SAI__OK ) THEN

*  Beef up the report to include mention of the NDF name (should only
*  occur for format conversion error).
         CALL MSG_SETC( 'ITEM', ITEM )
         CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
         CALL ERR_REP( 'IMG1_RDFTX_NOVAL', 'Unable to get a value ' //
     :        'for the keyword ^ITEM in the FITS extension of NDF ' //
     :        '^NDF.', STATUS )
      END IF
      END

* $Id$
