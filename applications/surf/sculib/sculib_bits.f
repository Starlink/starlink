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
*     Revision 1.3  1997/05/01 22:22:46  timj
*     Add SCULIB_BTOI to convert BYTE to INTEGER*4
*
*     Revision 1.2  1996/12/06 00:16:48  timj
*     Add SCULIB_BITAND
*
c Revision 1.1  1996/08/28  01:40:50  timj
c Initial revision
c
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
*     Returns the bitwise OR
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

*+ SCULIB_BITAND - returns the bitwise AND of two bytes
      BYTE FUNCTION SCULIB_BITAND (VAL1, VAL2)
*    Description:
*     Returns the bitwise AND
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

      SCULIB_BITAND = IIAND(IZEXT(VAL1), IZEXT(VAL2))

      END

*** INTEGER FUNCTIONS

*+ SCULIB_BTOI - Convert BYTE to INTEGER*4
      INTEGER FUNCTION SCULIB_BTOI (VAL)
*    Description:
*     Converts a BYTE to an INTEGER*4
*    Invocation:
*     IVAL = SCULIB_BTOI(VAL)
*    Parameters:
*     VAL              = BYTE (Given)
*           Input byte
*    Type Definitions:
      IMPLICIT NONE
*    Global constants :
*    Import :
      BYTE VAL
*    Export:
*    Status :
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
*-

      SCULIB_BTOI = JZEXT(VAL)

      END
