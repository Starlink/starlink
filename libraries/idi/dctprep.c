/*
*+
*  Name:
*     dctprep

*  Purpose:
*     DCT file management (to be read by IIDOPN)

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council.
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
*     P. Santin (Trieste Astronomical Observatory)

*  History:
*     01-DEC-1987 (PS):
*        Original Version

*-
*/

/* Input  File DEVICE.DCT                                            */
/* Output File DEVICE.DAT                                            */

/* include all common definitions */
# include    <stdio.h>
# include    <string.h>
# include    <stdlib.h>
                                 /* ERRORS & CONSTANTS definitions */
# include    "device.dep"
# include    "kwm.h"
# include    "idi.h"
# include    "idi_err.h"
# include    "idifuncs.h"

int main( void )

{
char    ic[2] , dev[5] , fildat[256] , fildct[256];
char    *sarg[ARGSIZE];
int     i, j, k , nit , dcterr=0;
int     arg[ARGSIZE];
int     ival;
FILE    *f;

getdctfile (fildct);

for (i = 0; i < ARGSIZE; i++)
   sarg[i] = malloc (SKWSIZE);

nit = ARGSIZE;
kws_xtr (fildct , "DESCR" , &nit , sarg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }
printf ("%s\n", sarg[0]);
printf (" Device OK [y/n] ? ");
scanf  ("%s", ic);

if ((ic[0] == 'n') || (ic[0] == 'N'))
   exit(dcterr);

getdatfile (fildat);

f = fopen (fildat , "w");
if (f == NULL)
   exit (dcterr);

/* Device MAIN STRUCTURE */
/* --------------------- */

nit = ARGSIZE;
kwi_xtr (fildct , "X_DIM" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }
fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "Y_DIM" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }
fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "N_DISPLAY" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }
fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "DEPTH" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }
fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "ZOOM_RANGE" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }
fprintf (f , "%d\n", arg[0]);
fprintf (f , "%d\n", arg[1]);

nit = ARGSIZE;
kwi_xtr (fildct , "N_LUT" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }
fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "LUT_DEPTH" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "N_CURSORS" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "N_ROI" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "N_INTERACTIONS" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "N_CONF" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "N_MEM" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "N_ITT" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kwi_xtr (fildct , "MEM_X_SIZE" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "MEM_Y_SIZE" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "MEM_DEPTH" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "MEM_TYPE" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");


/* INTERACTORS Definition */
/* ---------------------- */

nit = ARGSIZE;
kwi_xtr (fildct , "N_INTERACTORS" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }
fprintf (f , "%d\n", arg[0]);

nit = ARGSIZE;
kws_xtr (fildct , "INT_DEV" , &nit , sarg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   {
   if (strcmp(sarg[i] , "MOUSE") == 0)
      fprintf (f , "%8d", II_MOUSE);
   else
      fprintf (f , "%8d", II_KEYB);
   }
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "N_LOC" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "N_EVL" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "N_TRG" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

/* LOCATORS definition */
/* ------------------- */

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_X_MIN" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_X_MAX" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_Y_MIN" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_Y_MAX" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kws_xtr (fildct , "LOC_LEFT_LS" , &nit , sarg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   {
   if ( strncmp( sarg[i], "XK", 2 ) == 0 )
      ival = -301;
   else
      sscanf( sarg[i], "%d", &ival );
   fprintf (f , "%8d", ival);
   }
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_LEFT_HS" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kws_xtr (fildct , "LOC_RIGHT_LS" , &nit , sarg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   {
   if ( strncmp( sarg[i], "XK", 2 ) == 0 )
      ival = -302;
   else
      sscanf( sarg[i], "%d", &ival );
   fprintf (f , "%8d", ival);
   }
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_RIGHT_HS" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kws_xtr (fildct , "LOC_UP_LS" , &nit , sarg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   {
   if ( strncmp( sarg[i], "XK", 2 ) == 0 )
      ival = -300;
   else
      sscanf( sarg[i], "%d", &ival );
   fprintf (f , "%8d", ival);
   }
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_UP_HS" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kws_xtr (fildct , "LOC_DOWN_LS" , &nit , sarg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   {
   if ( strncmp( sarg[i], "XK", 2 ) == 0 )
      ival = -303;
   else
      sscanf( sarg[i], "%d", &ival );
   fprintf (f , "%8d", ival);
   }
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_DOWN_HS" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "LOC_HS_FACT" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

/* EVALUATORS definition */
/* --------------------- */

nit = ARGSIZE;
kwi_xtr (fildct , "EVL_TYPE" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kws_xtr (fildct , "EVL_DEF" , &nit , sarg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   {
   if ( strncmp( sarg[i], "XK", 2 ) == 0 )
      ival = -304;
   else
      sscanf( sarg[i], "%d", &ival );
   fprintf (f , "%8d", ival);
   }
fprintf (f , "\n");


nit = ARGSIZE;
kwi_xtr (fildct , "EVL_MIN" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "EVL_MAX" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%8d", arg[i]);
fprintf (f , "\n");

/* TRIGGERS definition */
/* ------------------- */

nit = ARGSIZE;
kws_xtr (fildct , "TRG_TYPE" , &nit , sarg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   {
   if (strcmp(sarg[i] , "BUTT") == 0)
      fprintf (f , "%5d", II_BUTT);
   else
      fprintf (f , "%5d", II_KEY);
   }
fprintf (f , "\n");

nit = ARGSIZE;
kwi_xtr (fildct , "TRG_DEF" , &nit , arg);
if (nit == 0)
   {
   dcterr = DCTFILERR;
   exit(dcterr);
   }

for (i = 0; i < nit; i++)
   fprintf (f , "%5d", arg[i]);
fprintf (f , "\n");


fclose (f);

exit(dcterr);
}

/***********************************************************************/
