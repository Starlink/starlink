/*
*+
*  Name:
*     scb_locase

*  Purpose:
*     Test text for SCB C parsing.

*  Type:
*     C program.

*  Description:
*     This file is a test text for the SCB tagging routines.
*     It contains function calls, includes and function definitions.

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
*/


#include "f77.h"
#include <ctype.h>

F77_SUBROUTINE(scb_locase)(INTEGER(leng), CHARACTER(word) TRAIL(word)) {

   GENPTR_INTEGER(leng)
   GENPTR_CHARACTER(word)

   int i;

   for (i=0; i < *leng; i++) {
      word[i] = tolower(word[i]);
   }

}

