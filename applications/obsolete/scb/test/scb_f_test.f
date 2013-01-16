*+
*  Name:
*     scb_f_test

*  Purpose:
*     Test text for SCB Fortran parsing.

*  Type:
*     C program.

*  Description:
*     This file is a test text for the SCB tagging routines.
*     It contains subroutine calls and definitions.

*  Notes:
*     Note that it is never necessary to build this test package; the
*     presence of the source code as files which are syntactically correct
*     and (in some sense) representative of files found elsewhere in the
*     Starlink source code collection is all that is required.

*  Arguments:
*     None.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (IoA)

*  History:
*     23-NOV-1998 (MBT);
*        Original version.
*-

      PROGRAM SCB_TEST

      WRITE( 6, 61 )
 61   FORMAT( 'IN FORTRAN:' )
      CALL SCB_HW
      END

      SUBROUTINE SCB_HW
      CALL SCB_GREET( 'WORLD' )
      END

      SUBROUTINE SCB_GREET( TARGET )
      CHARACTER*5 TARGET, STRING
      STRING = TARGET
      CALL SCB_LOCASE( 5, STRING )
      WRITE( 6, 62) STRING
 62   FORMAT( '     HELLO ', A )
      END
