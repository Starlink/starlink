*+ These are the SCULIB bitwise operator routines
*
*     Description:
*      Routines specifically designed for handling QUALITY bits
*      Bits start counting a 0
*      Do it this way just in case the FORTRAN is non-portable in a similar
*      way to NDF_FUNC (NDF_QMASK)
*    Authors:
*     T. Jenness (timj@jach.hawaii.edu)
*    History:
*     $Log$
*     Revision 1.1  1996/08/28 01:40:50  timj
*     Initial revision
*
*    endhistory
*-

*+ SCULIB_BITON - turn on a bit
      BYTE FUNCTION SCULIB_BITON (VAL, BIT)
*    Description:
*     Turns on a bit in a byte
*    Invocation:
*     NEWBYTE = SCULIB_BITOR(VAL, BIT)
*    Parameters:
*     VAL              = BYTE (Given)
*           The byte to be changed
*     BIT              = INT (Given)
*           The bit to be turned on
*
*    Type Definitions:
      IMPLICIT NONE
*    Global constants :
*    Import :
      BYTE VAL
      INTEGER BIT
*    Export:
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
*-

      SCULIB_BITON = IIBSET(IZEXT(VAL), BIT)

      END

*+ SCULIB_BITOF - turn off a bit
      BYTE FUNCTION SCULIB_BITOFF (VAL, BIT)
*    Description:
*     Turns off a bit in a byte
*    Invocation:
*     NEWBYTE = SCULIB_BITON(VAL, BIT)
*    Parameters:
*     VAL              = BYTE (Given)
*           The byte to be changed
*     BIT              = INT (Given)
*           The bit to be turned off
*
*    Type Definitions:
      IMPLICIT NONE
*    Global constants :
*    Import :
      BYTE VAL
      INTEGER BIT
*    Export:
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
*-

      SCULIB_BITOFF = IIBCLR(IZEXT(VAL), BIT)

      END

*+ SCULIB_BITOR - returns the bitwise OR of two bytes
      BYTE FUNCTION SCULIB_BITOR (VAL1, VAL2)
*    Description:
*     Turns off a bit in a byte
*    Invocation:
*     LOGOR = SCULIB_BITOR(VAL1, VAL2)
*    Parameters:
*     VAL1              = BYTE (Given)
*           First byte
*     VAL2              = BYTE (Given)
*           Second byte
*    Type Definitions:
      IMPLICIT NONE
*    Global constants :
*    Import :
      BYTE VAL1
      BYTE VAL2
*    Export:
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
*-

      SCULIB_BITOR = IIOR(IZEXT(VAL1), IZEXT(VAL2))

      END

