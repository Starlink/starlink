
* no plus (not a real prologue)

*  Name:
*     SCULIB_BITS

*  Purpose:
*     These are the SCULIB bitwise operator routines
*
*  Description:
*     Routines specifically designed for handling QUALITY bits
*     Bits start counting at 0
*     Do it this way just in case the FORTRAN is non-portable in a similar
*     way to NDF_FUNC (NDF_QMASK)

*  Invocation:
*     Various

*  Functions:
*     BYTE SCULIB_BITON
*     BYTE SCULIB_BITOFF
*     BYTE SCULIB_BITOR
*     BYTE SCULIB_BITAND
*     BYTE SCULIB_BITTEST

*  Authors:
*     T. Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.9  1999/08/06 02:24:39  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.8  1999/08/03 19:34:44  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.7  1999/07/30 19:58:13  timj
*     Minor header tweaks.
*
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
**************************************

      BYTE FUNCTION SCULIB_BITON (VAL, BIT)
*+
*  Name:
*     SCULIB_BITON

*  Purpose:
*     turn on a bit

*  Language:
*     Starlink Fortran 77 with VMS extensions

*  Invocation:
*     NEWBYTE = SCULIB_BITON( VAL, BIT )

*  Description:
*     Turns on a bit in a byte.

*  Arguments:
*     VAL              = BYTE (Given)
*           The byte to be changed
*     BIT              = INT (Given)
*           The bit to be turned on

*  Returned Value:
*     SCULIB_BITON = BYTE
*       Modified byte.


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      BYTE VAL
      INTEGER BIT

*  Arguments Returned:
*  Status:
*  External references:
*  Global variables:
*  Local Constants:
*  Local variables:

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'

*.

      SCULIB_BITON = IBSET(VAL, NUM_ITOUB(BIT))

      END


      BYTE FUNCTION SCULIB_BITOFF (VAL, BIT)
*+
*  Name:
*     SCULIB_BITOFF

*  Purpose:
*     Turn off a bit

*  Language:
*     Starlink Fortran 77 with VMS extensions

*  Invocation:
*     NEWBYTE = SCULIB_BITOFF ( VAL, BIT )

*  Description:
*     Turns off a bit in a byte

*  Arguments:
*     VAL              = BYTE (Given)
*           The byte to be changed
*     BIT              = INT (Given)
*           The bit to be turned off

*  Returned Value:
*     SCULIB_BITOFF = BYTE
*       Modified byte.

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      BYTE VAL
      INTEGER BIT

*  Arguments Returned:
*  Status:
*  External references:
*  Global variables:
*  Local Constants:
*  Local variables:

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'
*.

      SCULIB_BITOFF = IBCLR(VAL, NUM_ITOUB(BIT))

      END


      BYTE FUNCTION SCULIB_BITOR (VAL1, VAL2)
*+
*  Name:
*     SCULIB_BITOR

*  Purpose:
*     Returns the bitwise OR of two bytes

*  Language:
*     Starlink Fortran 77 with VMS extensions

*  Invocation:
*     LOGOR = SCULIB_BITOR(VAL1, VAL2)

*  Description:
*     Returns the bitwise OR

*  Arguments:
*     VAL1              = BYTE (Given)
*           First byte
*     VAL2              = BYTE (Given)
*           Second byte

*  Returned Value:
*     SCULIB_BITOR = BYTE
*       OR of bytes

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      BYTE VAL1
      BYTE VAL2

*  Arguments Returned:
*  Status:
*  External references:
*  Global variables:
*  Local Constants:
*  Local variables:

*.

      SCULIB_BITOR = IOR(VAL1, VAL2)

      END


      BYTE FUNCTION SCULIB_BITAND (VAL1, VAL2)
*+
*  Name:
*     SCULIB_BITAND

*  Purpose:
*     Calculate the bitwise AND of two bytes

*  Language:
*     Starlink Fortran 77 with VMS extensions

*  Invocation:
*     LOGAND = SCULIB_BITAND( VAL1, VAL2 )

*  Description:
*     Returns the bitwise AND

*  Arguments:
*     VAL1              = BYTE (Given)
*           First byte
*     VAL2              = BYTE (Given)
*           Second byte

*  Returned Value:
*     SCULIB_BITAND = BYTE
*       AND of bytes.

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
*  Arguments Given:
      BYTE VAL1
      BYTE VAL2

*  Arguments Returned:
*  Status:
*  External references:
*  Global variables:
*  Local Constants:
*  Local variables:

*.

      SCULIB_BITAND = IAND(VAL1, VAL2)

      END



      LOGICAL FUNCTION SCULIB_BITTEST (VAL, BIT)
*+
*  Name:
*     SCULIB_BITTEST

*  Purpose:
*     Test whether a bit is on.

*  Language:
*     Starlink Fortran 77 with VMS extensions

*  Invocation:
*     ISON = SCULIB_BITTEST( VAL, BIT )

*  Description:
*     Tests whether specified bit is turned on or not.
*     Returns TRUE if it is on, false otherwise.

*  Arguments:
*     VAL              = BYTE (Given)
*           The byte to be tested
*     BIT              = INT (Given)
*           The bit to be tested

*  Returned Value:
*     SCULIB_BITTEST = LOGICAL
*       True if specified bit is turned on.

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
*  Arguments Given:
      BYTE VAL
      INTEGER BIT

*  Arguments Returned:
*  Status:
*  External references:
*  Global variables:
*  Local Constants:
*  Local variables:

      INCLUDE 'NUM_DEC_CVT'
      INCLUDE 'NUM_DEF_CVT'

*.

      SCULIB_BITTEST = BTEST(VAL, NUM_ITOUB(BIT))

      END
