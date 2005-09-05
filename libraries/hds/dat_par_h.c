#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */

   int main( int argc, char *argv[] )
   {
/* Name:                                                                    */
/*    dat_par_h                                                             */

/* Purpose:                                                                 */
/*    Generate the C dat_par.h public include file for HDS.                 */

/* Type of Module:                                                          */
/*    Main Program.                                                         */

/* Invocation:                                                              */
/*    dat_par_h                                                             */

/* Parameters:                                                              */
/*    None.                                                                 */

/* Description:                                                             */
/*    This program is used to generate the C public include file dat_par.h  */
/*    for use by software which calls HDS routines. This method is used so  */
/*    that this file may contain constants whose value is determined by the */
/*    C compiler at compile time, depending on such things as the size of   */
/*    internal HDS structures which are not public. The dat_par.h file      */
/*    contents are written to the standard output.                          */

/* Copyright:                                                               */
/*    Copyright (C) 1998 Central Laroratory of the Research Councils        */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    PWD: Peter W. Draper (JAC, Durham University)                         */
/*    {enter_new_authors_here}                                              */

/* History:                                                                 */
/*    7-JUL-1993 (RFWS):                                                    */
/*       Original version.                                                  */
/*    6-OCT-1998 (RFWS):                                                    */
/*       Pad string locator values out to correct length with spaces.       */
/*    1-SEP-2005 (PWD):                                                     */
/*       Deal with case when lengths of NOLOC and ROOT are greater than     */
/*       SZLOC                                                              */
/*    {enter_changes_here}                                                  */

/* Bugs:                                                                    */
/*    {note_any_bugs_here}                                                  */

/*-                                                                         */

/*.                                                                         */

/* Set up strings to be used to pad string locator values out to the        */
/* correct length (DAT__SZLOC characters).                                  */
      char noloc[DAT__SZLOC + 1];
      char root[DAT__SZLOC + 1];
      int i;

      strncpy( noloc, DAT__NOLOC, DAT__SZLOC );
      for ( i = strlen( noloc ); i < DAT__SZLOC; i++ ) {
          noloc[i] = ' ';
      }
      noloc[DAT__SZLOC] = '\0';

      strncpy( root, DAT__ROOT, DAT__SZLOC );
      for ( i = strlen( root ); i < DAT__SZLOC; i++ ) {
          root[i] = ' ';
      }
      root[DAT__SZLOC] = '\0';

/* Write out the contents of the dat_par.h file, leaving the constant       */
/* values to be filled in.                                                  */
      (void) printf( "\
#if !defined( DAT_PAR_INCLUDED ) /* dat_par.h already included? */\n\
#define DAT_PAR_INCLUDED 1\n\
/*\n\
*+\n\
*  Name:\n\
*     dat_par.h\n\
\n\
*  Purpose:\n\
*     Define public global constants for the dat_ and hds_ routines.\n\
\n\
*  Language:\n\
*     ANSI C\n\
\n\
*  Type of Module:\n\
*     Global constants (macro) include file.\n\
\n\
*  Description:\n\
*     This file contains macro definitions for global constants which\n\
*     are used by the dat_ and hds_ routines within the HDS package and\n\
*     which may also be needed by software which calls these routines.\n\
\n\
*  Copyright:\n\
*     Copyright (C) 1998 Central Laboratory of the Research Councils\n\
\n\
*  Authors:\n\
*     Generated automatically by the dat_par_h program.\n\
*     {enter_new_authors_here}\n\
\n\
*  History:\n\
*     {enter_changes_here}\n\
\n\
*-\n\
*/\n\
\n\
/* Global Constants: */\n\
\n\
#define DAT__MXDIM %d            /* Maximum number of object dimensions  */\n\
#define DAT__NOLOC \"%s\" /* Null (invalid) locator value */\n\
#define DAT__NOWLD %d            /* Null wild-card search context */\n\
#define DAT__ROOT \"%s\" /* Root locator value */\n\
#define DAT__SZGRP %d            /* Size of group name */\n\
#define DAT__SZLOC %d            /* Size of locator */\n\
#define DAT__SZMOD %d            /* Size of access mode string */\n\
#define DAT__SZNAM %d            /* Size of object name */\n\
#define DAT__SZTYP %d            /* Size of type string */\n\
\n\
/*. */\n\
#endif\n",

/* Specify the constant values.                                             */
      DAT__MXDIM, noloc, DAT__NOWLD, root,
      DAT__SZGRP, DAT__SZLOC, DAT__SZMOD, DAT__SZNAM, DAT__SZTYP );

/* End of program.                                                          */
      exit( 0 );
      return 0;
   }
