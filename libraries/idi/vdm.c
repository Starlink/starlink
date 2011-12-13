/*
*+
*  Name:
*     VDM.C
*
*  Purpose:
*     Virtual Display Management
*
*  Description:
*     Virtual Display Management
*
*  Contents:
*     VDM_CRE
*        Creates a new display window for subsequent operations
*     VDM_DEL
*        Deletes a display window
*     VDM_INQ
*        Inquire characteristics of a display window
*     VDM_MOD
*        Modify characteristics of a display window
*     VDM_SAV
*        Saves a G/I display window as a bitmap file
*     filwndcr
*        Window file creation
*     getkwfile
*        Provides keywords file name for Virtual Device Manager
*
*  Copyright:
*     Copyright (C) 1988, 1991, 1992, 1994 Science & Engineering Research Council.
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
*     23-MAY-1991 (NE):
*        Added stdlib.h for malloc, ctype for toupper
*     26-MAR-1992 (NE):
*        Added idi_err.h

*-
*/

/* System definitions */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* Package definitions */

#include "device.dep"
#include "kwm.h"
#include "vdm.h"
#include "idi_err.h"
#include "idi.h"
#include "idifuncs.h"

/* Local definitions */

static   int       arg[ARGSIZE] ;
static   char      filwnd[256];

/******************************************************************************/

int VDM_CRE ( char device[], int xoff, int yoff, int xdim, int ydim,
              char wtype, char display[] )

/*
*+
*  Name :
*     VDM_CRE
*
*  Purpose :
*     Creates a new display window for subsequent operations
*
*  Invocation :
*     status = VDM_CRE( device, xoff, yoff, xdim, ydim, wtype, display )
*
*  Description :
*     Creates a new display window for subsequent operations
*
*  Arguments :
*     device = char[]
*        Device type. Short device name or 'WS', considered as
*        environment variable
*     xoff = int
*        X offset
*     yoff = int
*        Y offset
*     xdim = int
*        X display dimension
*     ydim = int
*        Y display dimension
*     wtype = char
*        display type  A = Alphanumeric display
*                      G = Graphic display
*                      I = Pictoric display
*     display = char[]
*        Created full display name  <device>.<WXn>
*        WXn  [X = A / G / I] [n = 0 - 9]
*     status = int
*        status code
*
*  Authors :
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History :
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Use getdev instead of getenv for device name.
*        Use memory file for window records.
*     23-MAY-1991 (NE):
*        Removed definition of malloc, fopen, toupper
*     20-AUG-1991 (NE):
*        Get full display name from vdm_cr_*
*     09-MAR-1994 (DLT):
*        Free "dev" after use
*/

{

/* Local Variables */
char   *dev;
char   *wsl = "ws" , *wsu = "WS";
char   *getdev();
int    nit , nwa , nwg , nwi , devnamlen;
FILE   *fwnd;

int   status;

status = VD_SUCCESS;

wtype = (char) toupper ((int)(wtype));

if ((strncmp (device , wsl, 2) == 0) || (strncmp (device , wsu, 2) == 0))
   {

/* Get device name */
   dev = getdev();
   if (dev == NULL)
      {
      status = VD_NULLDEV;
      return (status);
       }
   }
else
   {
   devnamlen = strlen (device);
   dev = (char *)malloc (devnamlen + 1);
   strncpy (dev , device , devnamlen);
   dev[devnamlen] = '\0';
   }

/* Get number of windows of each type from memory file */
nit = 1;
kwm_xtr( filwnd , "NWNDA" , &nit , arg );
if (nit == 0)
   {
   status = VD_FILKWERR;
   return (status);
   }
nwa = arg[0];

kwm_xtr( filwnd , "NWNDG" , &nit , arg );
if (nit == 0)
   {
   status = VD_FILKWERR;
   return (status);
   }
nwg = arg[0];

kwm_xtr( filwnd , "NWNDI" , &nit , arg );
if (nit == 0)
   {
   status = VD_FILKWERR;
   return (status);
   }
nwi = arg[0];

switch (wtype)
   {
   case 'a':
   case 'A':

      if (nwa > 10)
         {
         status = VD_MAXWIND;
         return (status);
         }
      vdm_cr_a( dev, xoff, yoff, xdim, ydim, display, &status );
      break;

   case 'g':
   case 'G':

      if (nwg > 10)
         {
         status = VD_MAXWIND;
         return (status);
         }
      vdm_cr_g( dev, xoff, yoff, xdim, ydim, display, &status );
      break;

   case 'i':
   case 'I':

      if (nwi > 10)
         {
         status = VD_MAXWIND;
         return (status);
         }
      vdm_cr_i( dev, xoff, yoff, xdim, ydim, display, &status );
      break;

   default:

      status = VD_KWTYPERR;
      return (status);

   }

free( dev );
return (status);
}

/******************************************************************************/

int VDM_DEL ( char display[] )

/*
*+
*  Name :
*     VDM_DEL
*
*  Purpose :
*     Deletes a display window
*
*  Invocation :
*     status = VDM_DEL( display )
*
*  Description :
*     Deletes a display window
*
*  Arguments :
*     display = char[]
*        Display full name
*     status = int
*        status code
*
*  Authors :
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History :
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Use memory file for window records
*        Do not delete the window records file
*     23-MAY-1991 (NE):
*        Removed definition of toupper.
*        Define s0 as a character array.
*/

{

/* Local Variables */
char  device [DEVLEN+1] , wind [WINDLEN+1];
char  s0[6] = "NWND ";
int   i , nit , nwa , nwg , nwi;

int   status;

status = VD_SUCCESS;

if (strpbrk (display, ".") == 0)
   {
   status = VD_WINOTOPN;
   return (status);
   }

strncpy (device, display, DEVLEN);
device [DEVLEN] = '\0';
strncpy (wind , display + DEVLEN + 1 , WINDLEN);
for (i = 0; i < WINDLEN; i++)
   wind[i] = (char) toupper ((int)wind[i]);
wind [WINDLEN] = '\0';

/* Get window information from memory file */
nit = 1;
kwm_xtr (filwnd , wind , &nit , arg);

if (arg[0] == 0)
   {
   status = VD_WINOTOPN;
   return (status);
   }

vdm_del_x (device, wind, &status);

/* Get window information from memory file */
s0[4] = wind[1];
nit = 1;
kwm_xtr (filwnd , s0 , &nit , arg);
if (nit == 0)
   {
   status = VD_FILKWERR;
   return (status);
   }

/* Update window information in memory file */
arg[0] -= 1;
kwm_upd  (filwnd , s0 , &nit , arg);
if (nit == 0)
   {
   status = VD_FILKWERR;
   return (status);
   }

/* Get number of windows of each type from memory file */
kwm_xtr (filwnd , "NWNDA" , &nit , arg);
if (nit == 0)
   {
   status = VD_FILKWERR;
   return (status);
   }
nwa = arg[0];

kwm_xtr (filwnd , "NWNDG" ,&nit , arg);
if (nit == 0)
   {
   status = VD_FILKWERR;
   return (status);
   }
nwg = arg[0];

kwm_xtr (filwnd , "NWNDI" , &nit , arg);
if (nit == 0)
   {
   status = VD_FILKWERR;
   return (status);
   }
nwi = arg[0];

if ( (nwa == 0) && (nwg == 0) && (nwi == 0) )
/* Delete windows record file */
/*
   delete (filwnd);
*/

return (status);
}

/******************************************************************************/

int VDM_INQ ( char display[], int* xoff, int* yoff, int* xdim, int* ydim )

/*
*+
*  Name :
*     VDM_INQ
*
*  Purpose :
*     Inquire characteristics of a display window
*
*  Invocation :
*     status = VDM_INQ( display, xoff, yoff, xdim, ydim )
*
*  Description :
*     Inquire characteristics of a display window
*
*  Arguments :
*     display = char[]
*        Display full name
*     xoff = int
*        X offset
*     yoff = int
*        Y offset
*     xdim = int
*        X display dimension
*     ydim = int
*        Y display dimension
*     status = int
*        status code
*
*  Authors :
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History :
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of toupper
*/

{

/* Local Variables */
char  device [DEVLEN+1], wind [WINDLEN+1];
char   *wsl = "ws", *wsu = "WS";
int   i;

int   status;

status = VD_SUCCESS;

if ((strcmp (display , wsl) == 0) ||
    (strcmp (display , wsu) == 0))
   {
                           /* inquire physical device characteristics */
   strcpy (device , "");
   strcpy (wind , "");
   }
else
   {
                           /* inquire virtual display characteristics */
   if (strpbrk (display, ".") == 0)
      {
      status = VD_WINOTOPN;
      return (status);
      }

   strncpy (device, display, DEVLEN);
   device [DEVLEN] = '\0';
   strncpy (wind , display + DEVLEN + 1 , WINDLEN);
   for (i = 0; i < WINDLEN; i++)
      wind[i] = (char) toupper ((int)wind[i]);
   wind [WINDLEN] = '\0';
   }

vdm_inq_x (device, wind, xoff, yoff, xdim, ydim, &status);

return (status);
}

/******************************************************************************/

int VDM_MOD ( char display[], int vis, int dxoff, int dyoff, int dxdim,
              int dydim )

/*
*+
*  Name :
*     VDM_MOD
*
*  Purpose :
*     Modify characteristics of a display window
*
*  Invocation :
*     status = VDM_MOD( display, vis, dxoff, dyoff, dxdim, dydim )
*
*  Description :
*     Modify characteristics of a display window
*
*  Arguments :
*     display = char[]
*        Display full name
*     vis = int
*        Visibility  0 = display invisible
*                    1 = display visible
*     dxoff = int
*        Delta X offset
*     dyoff = int
*        Delta Y offset
*     dxdim = int
*        Delta X display dimension
*     dydim = int
*        Delta Y display dimension
*     status = int
*        status code
*
*  Authors :
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History :
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of toupper
*/

{

/* Local Variables */
char  device [DEVLEN+1], wind [WINDLEN+1];
int   i;

int   status;

status = VD_SUCCESS;

if (strpbrk (display, ".") == 0)
   {
   status = VD_WINOTOPN;
   return (status);
   }

strncpy (device, display, DEVLEN);
device [DEVLEN] = '\0';
strncpy (wind , display + DEVLEN + 1 , WINDLEN);
for (i = 0; i < WINDLEN; i++)
   wind[i] = (char) toupper ((int)wind[i]);
wind [WINDLEN] = '\0';

vdm_mod_x (device, wind, vis, dxoff, dyoff, dxdim, dydim, &status);

return (status);
}

/******************************************************************************/

int VDM_SAV ( char display[], char bmfile[] )

/*
*+
*  Name :
*     VDM_SAV
*
*  Purpose :
*     Saves a G/I display window as a bitmap file
*
*  Invocation :
*     status = VDM_SAV( display, bmfile )
*
*  Description :
*     Saves a G/I display window as a bitmap file
*
*  Arguments :
*     display = char[]
*        Display full name
*     bmfile = char[]
*        Bitmap file name
*     status = int
*        status code
*
*  Authors :
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History :
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of toupper
*/

{

/* Local Variables */
char  device [DEVLEN+1], wind [WINDLEN+1];
int   i;

int   status;

status = VD_SUCCESS;

if (strpbrk (display, ".") == 0)
   {
   status = VD_WINOTOPN;
   return (status);
   }

strncpy (device, display, DEVLEN);
device [DEVLEN] = '\0';
strncpy (wind , display + DEVLEN + 1 , WINDLEN);
for (i = 0; i < WINDLEN; i++)
   wind[i] = (char) toupper ((int)wind[i]);
wind [WINDLEN] = '\0';

if (wind[1] == 'A')
   {
   status = VD_SAVNOTALL;
   return (status);
   }

vdm_sav_x (device, wind, bmfile, &status);

return (status);
}

/******************************************************************************/

void filwndcr ( char filwindow[], int* status )

/*
*+
*  Name :
*     filwndcr
*
*  Purpose :
*     Window file creation
*
*  Invocation :
*     filwndcr( filwindow, status )
*
*  Description :
*     Window file creation
*
*  Arguments :
*     filwindow = char[]
*     status = int
*
*  Authors :
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History :
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     23-MAY-1991 (NE):
*        Removed definition of fopen
*/

{

/* Local Variables */
char *s0 = "NWND  = 0";
char *s1 = "W   = 0";
char s2[KWRECLEN - 7];
int i;
FILE *fwnd;


*status = VD_SUCCESS;

fwnd = fopen(filwindow,"w");
if (fwnd == NULL)
   {
   *status = VD_FILKWNOTCR;
   return;
   }

for (i = 0; i < KWRECLEN - 8; i++)
   s2[i] = ' ';
s2[KWRECLEN-10] = '\0';

s0[4] = 'A';
fprintf (fwnd , "%s%s\n" , s0, s2);
s0[4] = 'G';
fprintf (fwnd , "%s%s\n" , s0, s2);
s0[4] = 'I';
fprintf (fwnd , "%s%s\n" , s0, s2);

s2[KWRECLEN-10] = ' ';
s2[KWRECLEN-8] = '\0';

s1[1] = 'A';
s1[2] = '0';
for (i = 0; i < 10; i++)
   {
   fprintf(fwnd , "%s%s\n" , s1 , s2);
   s1[2]++;
   }

s1[1] = 'G';
s1[2] = '0';
for (i = 0; i < 10; i++)
   {
   fprintf(fwnd , "%s%s\n" , s1 , s2);
   s1[2]++;
   }

s1[1] = 'I';
s1[2] = '0';
for (i = 0; i < 10; i++)
   {
   fprintf(fwnd , "%s%s\n" , s1 , s2);
   s1[2]++;
   }

fclose(fwnd);

}

/******************************************************************************/

void getkwfile ( char filwnd[] )

/*
*+
*  Name :
*     getkwfile
*
*  Purpose :
*     Provides keywords file name for Virtual Device Manager
*
*  Invocation :
*     getkwfile( filwnd )
*
*  Description :
*     Provides keywords file name for Virtual Device Manager
*
*  Arguments :
*     filwnd = char[]
*        Keywords file name
*
*  Authors :
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*
*  History :
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Use getdev instead of getenv for device name.
*/

{

/* Local Variables */
char *device;
char *devtyp = "VDM_WS";
char *ext = ".wnd";
int i , devnamlen;
char *getdev();

/* Use getdev instead of getenv for device name */
device = getdev();

devnamlen = strlen (device);
strncpy (filwnd , device , devnamlen);
for (i = 0; i < 4; i++)
   filwnd [i + devnamlen] = ext[i];
filwnd [devnamlen + 4] = '\0';

return;
}

