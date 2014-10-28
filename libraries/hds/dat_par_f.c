#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dat_par.h"

   int main( int argc, char *argv[] )
   {
/* Name:                                                                    */
/*    dat_par_f                                                             */

/* Purpose:                                                                 */
/*    Generate the Fortran dat_par(.f) public include file for HDS.         */

/* Type of Module:                                                          */
/*    Main Program.                                                         */

/* Invocation:                                                              */
/*    dat_par_f                                                             */

/* Parameters:                                                              */
/*    None.                                                                 */

/* Description:                                                             */
/*    This program is used to generate the Fortran public include file      */
/*    dat_par(.f) for use by software which calls HDS routines. This method */
/*    is used so that this file may contain constants whose value is        */
/*    determined by the C compiler at compile time, depending on such       */
/*    things as the size of internal HDS structures which are not public.   */
/*    The dat_par(.f) file contents are written to the standard output.     */

/* Copyright:                                                               */
/*    Copyright (C) 1998 Central Laboratory of the Research Councils        */

/*  Licence:                                                                */
/*     This program is free software; you can redistribute it and/or        */
/*     modify it under the terms of the GNU General Public License as       */
/*     published by the Free Software Foundation; either version 2 of       */
/*     the License, or (at your option) any later version.                  */

/*     This program is distributed in the hope that it will be              */
/*     useful, but WITHOUT ANY WARRANTY; without even the implied           */
/*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR              */
/*     PURPOSE. See the GNU General Public License for more details.        */

/*     You should have received a copy of the GNU General Public            */
/*     License along with this program; if not, write to the Free           */
/*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,       */
/*     MA 02110-1301, USA                                                   */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    BKM: Brian McIlwrath (Starlink, RAL)                                  */
/*    PWD: Peter Draper (University of Durham)                              */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {enter_new_authors_here}                                              */

/* History:                                                                 */
/*    7-JUL-1993 (RFWS):                                                    */
/*       Original version.                                                  */
/*    4-JUN-2004 (BKM):                                                     */
/*       Reformat prolog.                                                   */
/*    5-SEP-2005 (PWD):                                                     */
/*       Use autoconf for feature discovery                                 */
/*    28-DEC-2005 (TIMJ):                                                   */
/*       Add DAT__FLEXT and DAT__SZFLX constants                            */
/*    {enter_changes_here}                                                  */

/* Bugs:                                                                    */
/*    {note_any_bugs_here}                                                  */

/*-                                                                         */

/*.                                                                         */

/* Write out the contents of the dat_par(.f) file, leaving the constant     */
/* values to be filled in.                                                  */
      (void) printf( "\
*+\n\
*  Name:\n\
*     DAT_PAR\n\
\n\
*  Purpose:\n\
*     Define public global constants for the DAT_ and HDS_ routines.\n\
\n\
*  Language:\n\
*     Starlink Fortran 77\n\
\n\
*  Type of Module:\n\
*     Global constants include file.\n\
\n\
*  Description:\n\
*     This file contains definitions of global constants which are used\n\
*     by the DAT_ and HDS_ routines within the HDS package and which\n\
*     may also be needed by software which calls these routines.\n\
\n\
*  Copyright:\n\
*     Copyright (C) 1998 Central Laboratory of the Research Councils\n\
\n\
*  Authors:\n\
*     Generated automatically by the dat_par_f program.\n\
*     {enter_new_authors_here}\n\
\n\
*  History:\n\
*     {enter_changes_here}\n\
\n\
*-\n\
\n\
*  Global Constants:\n\
\n\
*  Maximum number of object dimensions.\n\
      INTEGER DAT__MXDIM\n\
      PARAMETER ( DAT__MXDIM = %d )\n\
\n\
*  Size of locator.\n\
      INTEGER DAT__SZLOC\n\
      PARAMETER ( DAT__SZLOC = %d )\n\
\n\
*  Null (invalid) locator value.\n\
      CHARACTER * ( DAT__SZLOC ) DAT__NOLOC\n\
      PARAMETER ( DAT__NOLOC = \'%s\' )\n\
\n\
*  Null wild-card search context.\n\
      INTEGER DAT__NOWLD\n\
      PARAMETER ( DAT__NOWLD = %d )\n\
\n\
*  Root locator value.\n\
      CHARACTER * ( DAT__SZLOC ) DAT__ROOT\n\
      PARAMETER ( DAT__ROOT  = \'%s\' )\n\
\n\
*  Size of group name.\n\
      INTEGER DAT__SZGRP\n\
      PARAMETER ( DAT__SZGRP = %d )\n\
\n\
*  Size of access mode string.\n\
      INTEGER DAT__SZMOD\n\
      PARAMETER ( DAT__SZMOD = %d )\n\
\n\
*  Size of object name.\n\
      INTEGER DAT__SZNAM\n\
      PARAMETER ( DAT__SZNAM = %d )\n\
\n\
*  Size of type string.\n\
      INTEGER DAT__SZTYP\n\
      PARAMETER ( DAT__SZTYP = %d )\n\
\n\
*  Size of file extension.\n\
      INTEGER DAT__SZFLX\n\
      PARAMETER ( DAT__SZFLX = %d )\n\
\n\
*  Default file extension for HDS files.\n\
      CHARACTER * ( DAT__SZFLX ) DAT__FLEXT\n\
      PARAMETER ( DAT__FLEXT = \'%s\' )\n\
\n\
*.\n",

/* Specify the constant values.                                             */
      DAT__MXDIM, DAT__SZLOC, DAT__NOLOC, DAT__NOWLD, DAT__ROOT, DAT__SZGRP,
		     DAT__SZMOD, DAT__SZNAM, DAT__SZTYP, DAT__SZFLX,
		     DAT__FLEXT );

/* End of program.                                                          */
      exit( 0 );
      return 0;
   }
