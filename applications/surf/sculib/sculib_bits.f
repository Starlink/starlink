*+ These are the SCULIB bitwise operator routines
*
*     Description:
*      Routines specifically designed for handling QUALITY bits
*      Bits start counting at 0
*      Do it this way just in case the FORTRAN is non-portable in a similar
*      way to NDF_FUNC (NDF_QMASK)
*
*    Functions:
*      BYTE SCULIB_BITON
*      BYTE SCULIB_BITOFF
*      BYTE SCULIB_BITOR
*      BYTE SCULIB_BITAND
*      BYTE SCULIB_BITTEST

*    Authors:
*     T. Jenness (timj@jach.hawaii.edu)
*    History:
*     $Log$
*     Revision 1.6  1999/05/25 23:36:05  timj
*     Add SCULIB_BITTEST
*
*     Revision 1.5  1997/05/30 18:31:50  timj
*     Use generic BYTE functions (IBSET instead of IIBSET etc).
*     Use NUM_ functions to convert BITNUM to BYTE (easier than changing
*     all the calls)
*
*     Revision 1.4  1997/05/29 20:50:17  timj
*     Remove SCULIB_BTOI (use NUM_UBTOI instead)
*
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

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'

      SCULIB_BITON = IBSET(VAL, NUM_ITOUB(BIT))

      END

*+ SCULIB_BITOFF - turn off a bit
      BYTE FUNCTION SCULIB_BITOFF (VAL, BIT)
*    Description:
*     Turns off a bit in a byte
*    Invocation:
*     NEWBYTE = SCULIB_BITOFF(VAL, BIT)
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
      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'

      SCULIB_BITOFF = IBCLR(VAL, NUM_ITOUB(BIT))

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

      SCULIB_BITOR = IOR(VAL1, VAL2)

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

      SCULIB_BITAND = IAND(VAL1, VAL2)

      END

*+ SCULIB_BITON - turn on a bit

      LOGICAL FUNCTION SCULIB_BITTEST (VAL, BIT)
*    Description:
*     Tests whether specified bit is turned on or not.
*     Returns TRUE if it is on, false otherwise.
*    Invocation:
*     NEWBYTE = SCULIB_BITTEST(VAL, BIT)
*    Parameters:
*     VAL              = BYTE (Given)
*           The byte to be tested
*     BIT              = INT (Given)
*           The bit to be tested
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

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'

      SCULIB_BITTEST = BTEST(VAL, NUM_ITOUB(BIT))

      END
