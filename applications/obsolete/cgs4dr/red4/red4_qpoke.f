*+  RED4_QPOKE - Poke a value into a bad pixel mask.
      SUBROUTINE RED4_QPOKE( DIM1, DIM2, IPOS, JPOS, QVAL, MASK, OQVAL,
     :  STATUS )
*    Description :
*     This routine pokes the given element of a bad pixel mask
*     with a new value. The old value is returned.
*    Invocation :
*      CALL RED4_QPOKE( DIM1, DIM2, IPOS, JPOS, QVAL, MASK, OQVAL,
*     :  STATUS )
*    Parameters :
*     DIM1                = INTEGER( READ )
*           The first dimension of the MASK array
*     DIM2                = INTEGER( READ )
*           The second dimension of the MASK array
*     IPOS                = INTEGER( READ )
*           The column number of the element to be poked.
*     JPOS                = INTEGER( READ )
*           The row number of the element to be poked.
*     QVAL                = INTEGER( READ )
*           Value to write into the specified MASK element.
*           Only the least significant byte is used, so values outside
*           the range 0-255 are effectively modulo 255.
*     MASK( DIM1, DIM2 )  = UBYTE( WRITE )
*           The mask array to be poked.
*     QVAL                = INTEGER( WRITE )
*           The old value found in the specified MASK element.
*           Only the least significant byte is used, so values outside
*           the range 0-255 are effectively modulo 255.
*     STATUS              = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*     EQUIVALENCE is used to extract the least significant byte from
*     QVAL. This is rather machine-specific.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly    (JACH::PND)
*    History :
*     23-May-1990: Original version.               (SMB)
*     22-Feb-1993: Conform to error strategy       (PND)
*     02-Nov-1995: Remove equivalence              (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
*    Import :
      INTEGER
     :  DIM1,              ! First dimension of MASK array
     :  DIM2,              ! Second dimension of MASK array
     :  IPOS,              ! Row number to be poked.
     :  JPOS,              ! Column number to be poked.
     :  QVAL               ! Value to be poked into MASK(IPOS,JPOS)
*    Export :
      BYTE
     :  MASK( DIM1, DIM2 ) ! Data quality mask
      INTEGER
     :  OQVAL              ! Old value found in MASK(IPOS,JPOS)
*    Status :
      INTEGER STATUS
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Obtain the old value of the MASK array.
      OQVAL = MASK( IPOS, JPOS )

*   Poke the new value into the MASK array (using the above equivalance)
      MASK( IPOS, JPOS ) = MIN( MAX( -128, QVAL ), 127 )

      END
