/*
*+
*  Name:
*     scb_c_test

*  Purpose:
*     Test text for SCB C parsing.

*  Type:
*     C program.

*  Description:
*     This file is a test text for the SCB tagging routines.
*     It contains function calls and function definitions.

*  Arguments:
*     None.

*  Notes:
*     Note that it is never necessary to build this test package; the
*     presence of the source code as files which are syntactically correct
*     and (in some sense) representative of files found elsewhere in the
*     Starlink source code collection is all that is required.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (IoA)

*  History:
*     23-NOV-1998 (MBT);
*        Original version.
*-
*/

#include <stdio.h>
#include <string.h>

#define SCB_OUT2(o1, o2) printf("     %s %s\n", o1, o2)

int scb_test();
int test_greet();

int main() {

   printf("In C:\n");
   scb_hw();
   return(0);

}

int scb_hw() {

   scb_greet("world");

}


int scb_greet(char *target) {

   SCB_OUT2("Hello", target);

}

