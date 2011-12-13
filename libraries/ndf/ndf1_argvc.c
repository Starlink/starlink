/*
*+
*  Name:
*     ndf1_argvc

*  Purpose:
*     Set or retrieve global argc/argv values

*  Language:
*     ANSI C

*  Synopsis:
*     char * const* ndf1_getargvc( int *argc, int * status);
*     void ndf1_setargvc( int argc, char *const argv[], int * status);

*  Description:
*     These functions let you set the command line argument list
*     in a static pointer and also retrieve it for later. These
*     are helper routines for ndfInit and ndf1_gtarg.
*
*     The memory is not copied. See the notes for ndfInit

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     17-AUG-2009 (TIMJ):
*         Initial version

*/

#include <stdio.h>

#include "ndf1.h"
#include "sae_par.h"

static int ndf1_argc = -1;
static char *const *ndf1_argv = NULL;

void ndf1_setargvc( int argc, char *const argv[], int * status) {
  if (*status != SAI__OK) return;

  ndf1_argc = argc;
  ndf1_argv = argv;
}

char * const* ndf1_getargvc( int *argc, int * status) {
  *argc = 0;
  if (*status != SAI__OK) return NULL;
  *argc = ndf1_argc;
  return ndf1_argv;
}
