/*
*+
*  Name:
*     IDIENV.C
*
*  Purpose:
*     Environment Service Routines
*
*  Description:
*     Environment Service Routines
*
*  Contents:
*     getdev
*        Get device name;
*     getdctfile
*        Get name of DCT file;
*     getdatfile
*        Get name of DCT -> DAT file;
*
*  Copyright:
*     Copyright (C) 1988, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*        Added unixlib.h for getenv
*/

/* System definitions */

#include    <stdio.h>
#include    <string.h>
#if HAVE_UNIXLIB_H
#include    <unixlib.h>
#else
#include    <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

/* Package definitions */

#include    "device.dep"
#include    "idi.h"
#include    "idi_err.h"
#include    "idistruct_e.h"
#include    "idifuncs.h"

/******************************************************************************/

char *getdev ()

/*
*+
*  Name:
*     getdev
*
*  Purpose:
*     Get device name
*
*  Invocation:
*     getdev()
*
*  Description:
*     Get default device name
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Use logical name IDI_XDT to access description file
*     23-MAY-1991 (NE):
*        Removed definition of getenv
*     22-JAN-1992 (DLT):
*        Add default device names depending on system
*     21-SEP-1992 (NE):
*        Rename sun4 macro to sun
*      8-JAN-1993 (NE):
*        Replace system dependent files with generic one
*/

{
/* Local Variables */
char *ididev;

ididev = getenv( "IDI_XDT" );
if (!ididev)
    {
    ididev = "xworks";
    }
return (ididev);
}

/******************************************************************************/

void getdctfile ( char fildct[] )

/*
*+
*  Name:
*     getdctfile
*
*  Purpose:
*     Get DCT file name
*
*  Invocation:
*     getdctfile( fildct )
*
*  Description:
*     Build Device Configuration Table file name
*
*  Arguments:
*     fildct = char[]
*        File name
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Replaced call to getenv with call to getdev for device name
*     23-MAY-1991 (NE):
*        Removed definition of getenv
*     22-JAN-1992 (DLT):
*        Add default path if IDI_DIR not set
*     14-OCT-1993 (DLT):
*        Cast strlen to int to suppress warning message on Solaris
*     27-JUL-1995 (DLT):
*        Seach for DCT file relative to PATH
*/

{

/* Local Variables */
char *ext = ".dct";
char *device;
char fil[256];
int i;

device = getdev();

strncpy (fil , device , strlen(device));
for (i = 0; i < (int)strlen(ext); i++)
   fil[i+strlen(device)] = ext[i];
fil[strlen(device)+strlen(ext)] = '\0';

getfile( fil, fildct);

return;
}

/******************************************************************************/

void getdatfile ( char fildat[] )

/*
*+
*  Name:
*     getdatfile
*
*  Purpose:
*     Get DAT file Name
*
*  Invocation:
*     getdatfile( fildat )
*
*  Description:
*     Build compiled version of Device Configuration Table file name
*
*  Arguments:
*     fildct = char[]
*        File name
*
*  Authors:
*     SANTIN: P. Santin (Trieste Astronomical Observatory)
*     NE: Nick Eaton (Durham University)
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     26-OCT-1988 (SANTIN):
*        Orignal version
*     21-FEB-1991 (NE):
*        Replaced call to getenv with call to getdev for device name
*     23-MAY-1991 (NE):
*        Removed definition of getenv
*     22-JAN-1992 (DLT):
*        Add default path if IDI_DIR not set
*     14-OCT-1993 (DLT):
*        Cast strlen to int to suppress warning message on Solaris
*     27-JUL-1995 (DLT):
*        Seach for DCT file relative to PATH
*/

{

/* Local Variables */
char *ext = ".dat";
char *device;
char fil[256];
int i;

device = getdev();

strncpy (fil , device , strlen(device));
for (i = 0; i < (int)strlen(ext); i++)
   fil[i+strlen(device)] = ext[i];
fil[strlen(device)+strlen(ext)] = '\0';

getfile( fil, fildat);

return;
}

/******************************************************************************/

void getfile ( char *file, char *name )

/*
*+
*  Name:
*     getfile
*
*  Purpose:
*     Get file name
*
*  Invocation:
*     getfile( file, name )
*
*  Description:
*     Build file name
*
*  Arguments:
*     file = char*
*        File to find
*     name = char[]
*        Full file name
*
*  Authors:
*     DLT: David Terrett (Starlink, RAL)
*
*  History:
*     27-JUL-1995 (DLT):
*/

{

/* Local Variables */
struct stat buffer;
char *dir;
char *path;
char *end;

/*
  Look for environment variable.
*/
    dir = getenv("IDI_DIR");
    if (dir)
    {
	strcpy(name, dir);
	strcat(name, "/");
	strcat(name, file);
    }
    else
    {

/*
   Search PATH
*/
	path = getenv("PATH");
	for (;;)
	{
	    end = strchr( path, ':');
	    if (end == NULL)
	    {
		strcpy(name, path);
		strcat(name, "/../etc/");
		strcat(name, file);
		break;
	    }
            strncpy( name, path, end-path);
            name[end-path] = '\0';
	    strcat(name, "/../etc/");
	    strcat(name, file);
            if (stat(name, &buffer) == 0 ) break;

	    path = end + 1;
	}
    }
    return;
}
