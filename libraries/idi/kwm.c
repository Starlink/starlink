/*
*+
*  Name:
*     KWM.C
*
*  Purpose:
*     Integer & char multivalued keyword file management
*
*  Description:
*     Integer & char multivalued keyword file management
*
*  Contents:
*     kwi_xtr
*        Keywords integer multi-value extraction
*     kwi_upd
*        Keywords integer multi-value update
*     kws_xtr
*        Keywords string multi-value extraction
*     kwm_xtr
*        Keywords extraction from memory file
*     kwm_upd
*        Keywords update of memory file
*
*  Copyright:
*     Copyright (C) 1988, 1991 Science & Engineering Research Council.
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
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Added a memory structure that looks like the windows file
*/

/* System definitions */

# include <stdlib.h>
# include <stdio.h>
# include <string.h>

/* Package definitions */

# include "device.dep"
# include "kwm.h"
# include "idi.h"
# include "idifuncs.h"

/* Local definitions */

static char line[2048];

/* Define a memory structure that looks like the file */
static struct
   {
   int nwnda;
   int nwndg;
   int nwndi;
   int wa0[6];
   int wa1[6];
   int wa2[6];
   int wa3[6];
   int wa4[6];
   int wa5[6];
   int wa6[6];
   int wa7[6];
   int wa8[6];
   int wa9[6];
   int wg0[6];
   int wg1[6];
   int wg2[6];
   int wg3[6];
   int wg4[6];
   int wg5[6];
   int wg6[6];
   int wg7[6];
   int wg8[6];
   int wg9[6];
   int wi0[6];
   int wi1[6];
   int wi2[6];
   int wi3[6];
   int wi4[6];
   int wi5[6];
   int wi6[6];
   int wi7[6];
   int wi8[6];
   int wi9[6];
   } memfile = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

/******************************************************************************/

void kwi_xtr ( char filkeyw[], char keywid[], int* nitem, int ikeyw[] )

/*
*+
*  Name:
*     kwi_xtr
*
*  Purpose:
*     Keywords integer multi-value extraction
*
*  Invocation:
*     kwi_xtr( filkeyw , keywid , nitem, ikeyw )
*
*  Description:
*     Keywords integer multi-value extraction
*     Error flag : *nitem = 0 as output
*
*  Arguments:
*     filkeyw = char[]
*        Keywords file name
*     keywid = char[]
*        Keyword identifier
*     nitem = int
*        Number of integer values
*     ikeyw = int[]
*        Integer values array
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of fopen
*/

{

/* Local Variables */
FILE *fwnd;
char keyw[16];
int i, j, k, keywidl , nitf, found;

fwnd = fopen(filkeyw , "r");
if (fwnd == NULL)
   {
   *nitem = 0;
   return;
   }

keywidl = strlen (keywid);
found  = 0;
nitf = 0;

while  (found == 0)
   {
   i = 0;
   while (((line[i] = (char) fgetc(fwnd)) != '\n') && (line[i] != EOF))
      {
      if (line[i] == '/')             /*  / is the continuation char */
         {
         while ((line[i] = (char) fgetc(fwnd)) != '\n');
         i--;
         }
      i++;
      }
   if (line[i] == EOF )
      {
      *nitem = nitf;                  /* Error : Key not found */
      fclose(fwnd);
      return;
      }
   i = 0;
   while ( (i < keywidl) && (keywid[i] == line[i]) )
      i++;
   if ( (i == keywidl) && ((line[i] == ' ') || (line[i] == '=')) )
      found = 1;
   if (found == 1)
      {
      while (line[i] == ' ')
         i++;
      while (line[i] == '=')
         i++;
      for (j = 0; j < *nitem; j++)
         {
         while (line[i] == ' ')
            i++;
         k = 0;
         while ( (line[i] != ' ') && (line[i] != '\n') )
            keyw[k++] = line[i++];
         keyw[k] = '\0';
         if (strlen(keyw) != 0)
            {
            ikeyw[j] = atoi(keyw);
            nitf += 1;
            }
         }
      }
   }

*nitem = nitf;

fflush (fwnd);
fclose(fwnd);

return;
}

/******************************************************************************/

void kwi_upd ( char filkeyw[], char keywid[], int* nitem, int ikeyw[] )

/*
*+
*  Name:
*     kwi_upd
*
*  Purpose:
*     Keywords integer multi-value update
*
*  Invocation:
*     kwi_upd( filkeyw , keywid , nitem, ikeyw )
*
*  Description:
*     Keywords integer multi-value update
*     Error flag : *nitem = 0 as output
*
*  Arguments:
*     filkeyw = char[]
*        Keywords file name
*     keywid = char[]
*        Keyword identifier
*     nitem = int
*        Number of integer values
*     ikeyw = int[]
*        Integer values array
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of fopen
*/

{

/* Local Variables */
FILE *fwnd;
char sval[10];
int i, j, k, il, ic, ir, found , rlen,  keywidl;

fwnd = fopen(filkeyw , "r+");
if (fwnd == NULL)
   {
   *nitem = 0;
   return;
   }

keywidl = strlen (keywid);
found  = 0;
ir = 0;

while  (found == 0 )
   {
   ic = fread (line, 1, KWRECLEN, fwnd);
   if (ic == EOF )
      {
      fclose(fwnd);
      *nitem = 0;
      return;
      }
   ir++;

   i = 0;
   while ( (i < keywidl) && (keywid[i] == line[i]) )
      i++;
   if ( (i == keywidl) && ((line[i] == ' ') || (line[i] == '=')) )
      found = 1;
   if (found == 1)
      {
      while (line[i] == ' ')
         i++;
      while (line[i] == '=')
         i++;
      line[i+1] = '\0';
      ic = (ir - 1) * KWRECLEN;
      fseek(fwnd, ic, 0);
      rlen = i;
      for (j = 0; j < *nitem; j++)
         {
         sprintf(sval,"%d ",ikeyw[j]);
         rlen += strlen(sval);
         strcat (line, sval);
         }
      for (j = 0; j < KWRECLEN - rlen - 1; j++)
        strcat (line, " ");
      line[KWRECLEN-1] = '\n';
      fwrite (line, 1, KWRECLEN, fwnd);
      }
   }

fflush (fwnd);
fclose(fwnd);

return;
}

/******************************************************************************/

void kws_xtr ( char* filkeyw, char* keywid, int* nitem, char* skeyw[] )

/*
*+
*  Name:
*     kws_xtr
*
*  Purpose:
*     Keywords string multi-value extraction
*
*  Invocation:
*     kws_xtr( filkeyw , keywid , nitem , skeyw )
*
*  Description:
*     Keywords string multi-value extraction
*     Error flag : *nitem = 0 as output
*
*  Arguments:
*     filkeyw = char[]
*        Keywords file name
*     keywid = char[]
*        Keyword identifier
*     nitem = int
*        Number of string values
*     skeyw = char[]
*        String array
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of fopen
*/

{

/* Local Variables */
FILE *fwnd;
char  keyw[32];
int i, j, k, l , nitf, found , keywidl;

fwnd = fopen(filkeyw , "r");
if (fwnd == NULL)
   {
   *nitem = 0;
   return;
   }

keywidl = strlen (keywid);
found  = 0;
nitf = 0;

while  (found == 0)
   {
   i = 0;
   while (((line[i] = getc(fwnd)) != '\n') && (line[i] != EOF))
      {
      if (line[i] == '/')             /*  / is the continuation char */
         {
         while ((line[i] = getc(fwnd)) != '\n');
         i--;
         }
      i++;
      }
   if (line[i] == EOF )
      {
      *nitem = nitf;           /* Error : Key not found */
      fclose(fwnd);
      return;
      }
   i = 0;
   while ( (i < keywidl) && (keywid[i] == line[i]) )
      i++;
   if ( (i == keywidl) && ((line[i] == ' ') || (line[i] == '=')) )
      found = 1;
   if (found == 1)
      {
      while (line[i] == ' ')
         i++;
      while (line[i] == '=')
         i++;
      for (j = 0; j < *nitem; j++)
         {
         while (line[i] == ' ')
            i++;
         k = 0;
         while ( (line[i] != ' ') && (line[i] != '\n') )
            keyw[k++] = line[i++];
         keyw[k] = '\0';
         if (strlen(keyw) != 0)
            {
            strcpy (skeyw[j] , keyw);
            nitf += 1;
            }
         }
      }
   }

*nitem = nitf;
fclose(fwnd);

return;
}

/******************************************************************************/

void kwm_xtr ( char filkeyw[], char keywid[], int* nitem, int ikeyw[] )

/*
*+
*  Name:
*     kwm_xtr
*
*  Purpose:
*     Keywords extraction from memory file
*
*  Invocation:
*     kwm_xtr( filkeyw , keywid , nitem, ikeyw )
*
*  Description:
*     Keywords extraction from memory file
*     Error flag : *nitem = 0 as output
*
*  Arguments:
*     filkeyw = char[]
*        Keywords file name
*     keywid = char[]
*        Keyword identifier
*     nitem = int
*        Number of integer values
*     ikeyw = int[]
*        Integer values array
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*/

{

/* Local Variables */
int i, nitf;

nitf = 0;

if ( strcmp( keywid, "NWNDA" ) == 0 )
   {
   nitf = 1;
   ikeyw[0] = memfile.nwnda;
   }
if ( strcmp( keywid, "NWNDG" ) == 0 )
   {
   nitf = 1;
   ikeyw[0] = memfile.nwndg;
   }
if ( strcmp( keywid, "NWNDI" ) == 0 )
   {
   nitf = 1;
   ikeyw[0] = memfile.nwndi;
   }
if ( strcmp( keywid, "WA0" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa0[i];
   }
if ( strcmp( keywid, "WA1" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa1[i];
   }
if ( strcmp( keywid, "WA2" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa2[i];
   }
if ( strcmp( keywid, "WA3" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa3[i];
   }
if ( strcmp( keywid, "WA4" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa4[i];
   }
if ( strcmp( keywid, "WA5" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa5[i];
   }
if ( strcmp( keywid, "WA6" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa6[i];
   }
if ( strcmp( keywid, "WA7" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa7[i];
   }
if ( strcmp( keywid, "WA8" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa8[i];
   }
if ( strcmp( keywid, "WA9" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wa9[i];
   }
if ( strcmp( keywid, "WG0" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg0[i];
   }
if ( strcmp( keywid, "WG1" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg1[i];
   }
if ( strcmp( keywid, "WG2" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg2[i];
   }
if ( strcmp( keywid, "WG3" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg3[i];
   }
if ( strcmp( keywid, "WG4" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg4[i];
   }
if ( strcmp( keywid, "WG5" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg5[i];
   }
if ( strcmp( keywid, "WG6" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg6[i];
   }
if ( strcmp( keywid, "WG7" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg7[i];
   }
if ( strcmp( keywid, "WG8" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg8[i];
   }
if ( strcmp( keywid, "WG9" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wg9[i];
   }
if ( strcmp( keywid, "WI0" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi0[i];
   }
if ( strcmp( keywid, "WI1" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi1[i];
   }
if ( strcmp( keywid, "WI2" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi2[i];
   }
if ( strcmp( keywid, "WI3" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi3[i];
   }
if ( strcmp( keywid, "WI4" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi4[i];
   }
if ( strcmp( keywid, "WI5" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi5[i];
   }
if ( strcmp( keywid, "WI6" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi6[i];
   }
if ( strcmp( keywid, "WI7" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi7[i];
   }
if ( strcmp( keywid, "WI8" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi8[i];
   }
if ( strcmp( keywid, "WI9" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < 6; i++ )
      ikeyw[i] = memfile.wi9[i];
   }

*nitem = nitf;

return;
}

/******************************************************************************/

void kwm_upd ( char filkeyw[], char keywid[], int* nitem, int ikeyw[] )

/*
*+
*  Name:
*     kwm_upd
*
*  Purpose:
*     Keywords update of memory file
*
*  Invocation:
*     kwm_upd( filkeyw , keywid , nitem, ikeyw )
*
*  Description:
*     Keywords update of memory file
*     Error flag : *nitem = 0 as output
*
*  Arguments:
*     filkeyw = char[]
*        Keywords file name
*     keywid = char[]
*        Keyword identifier
*     nitem = int
*        Number of integer values
*     ikeyw = int[]
*        Integer values array
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     21-FEB-1991 (NE):
*        Orignal version
*/

{

/* Local Variables */
int i, nitf;

nitf = 0;

if ( strcmp( keywid, "NWNDA" ) == 0 )
   {
   nitf = 1;
   memfile.nwnda = ikeyw[0];
   }
if ( strcmp( keywid, "NWNDG" ) == 0 )
   {
   nitf = 1;
   memfile.nwndg = ikeyw[0];
   }
if ( strcmp( keywid, "NWNDI" ) == 0 )
   {
   nitf = 1;
   memfile.nwndi = ikeyw[0];
   }
if ( strcmp( keywid, "WA0" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa0[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA1" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa1[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA2" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa2[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA3" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa3[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA4" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa4[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA5" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa5[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA6" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa6[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA7" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa7[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA8" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa8[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WA9" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wa9[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG0" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg0[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG1" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg1[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG2" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg2[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG3" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg3[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG4" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg4[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG5" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg5[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG6" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg6[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG7" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg7[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG8" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg8[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WG9" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wg9[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI0" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi0[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI1" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi1[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI2" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi2[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI3" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi3[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI4" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi4[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI5" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi5[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI6" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi6[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI7" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi7[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI8" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi8[i] = ikeyw[i];
   }
if ( strcmp( keywid, "WI9" ) == 0 )
   {
   nitf = 6;
   for ( i = 0; i < nitf; i++ )
      memfile.wi9[i] = ikeyw[i];
   }

*nitem = nitf;

return;
}

