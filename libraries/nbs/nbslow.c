/*
*
*+
*  Name:
*     NBSLOW.C

*  Purpose:
*     Low level routines in the noticeboard system.

*  Language:
*     ANSI C

*  Description:
*     Note that where there is a STATUS argument, it is RETURNED, not MODIFIED.
*     In all cases status SAI__OK is returned on successful completion.
*
*     This module is split into several sections of routines, all of which are
*     called by the high-level NBS routines in module NBS.C.

*  Copyright:
*     Copyright (C) 1986-1990, 1993 Science & Engineering Research
*     Council. Copyright (C) 1995, 1999, 2004 Central Laboratory of the
*     Research Councils. Copyright (C) 2005 Particle Physics &
*     Astronomy Research Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     {original_author_entry}

*  History:
*     03-Feb-1986 (WFL):
*        Original version.
*     17-Jul-1987 (WFL):
*        Change names of INCLUDEd files and change NULL to NIL.
*     20-Jul-1987 (WFL):
*        Add maximum length arguments to string import and export routines,
*        plus, on import, use a user-supplied buffer. Also capitalise and
*        ignore spaces and non-printing characters.
*         Remove NBS_COPY (to replace with _CHMOVE macro).
*         Use default extension of .NBD for definition files.
*         Ignore trailing blanks on global section names.
*     21-Jul-1987 (WFL):
*        Remove NBS_PRIVATE_ITEM.
*         Excise all mention of OTHER_SELF.
*         Remove the NBS_*_FIELDS routines.
*         Add extra logical arg to relocation routines. If YES,
*         add offsets and do before recursive calls. If NO, subtract
*         offsets and do after recursive calls. Also split the IFS_OFFSET
*         arg into I_OFFSET and FBS_OFFSET.
*     06-Nov-1987 (WFL):
*        Portable VMS / UNIX version. Use unix / vms / strdescr
*         macros as appropriate.
*     10-Feb-1988 (WFL):
*        Sort out string descriptors to VMS system services
*     11-Feb-1988 (WFL):
*        Use macros for routine names and use NBS_ prefix when
*         strdescr is defined, NBC_ when it is not. Also use #module to
*         set module name explicitly. Change strdescr to c_string.
*     25-Mar-1988 (WFL):
*        Move file header definition to NBS_TYP.H.
*        Extra FILE_SIZE argument to NBS_WRITE_FILE and NBS_OPEN_FILE.
*         New NBS_OPEN_WRITE and NBS_UPDATE_FILE routines.
*     30-Mar-1988 (WFL):
*        Only use #module under VMS; under UNIX use first
*         character plus last ones for shared memory name.
*     10-Apr-1989 (WFL):
*        Use EXTERN rather than STATIC so that NBS and NBC
*         routines can be used intermingled (but not values alterable
*         using NBS_TUNE, since these are initialised)
*     02-Feb-1990 (WFL):
*        Add NBS_UNMAP_SECTION
*     06-Feb-1990 (WFL):
*        Account for PARENT and DATA being in unions
*     07-Feb-1990 (WFL):
*        Similarly SHAPE
*     05-Feb-1993 (DJA):
*        Added NBS_MLIST_ADD, _FIND and _UNMAP to handle restrictions
*        on mapping UNIX memory sections.
*        FORTRAN/C interface made portable using F77/CNF. NBS_STRIMP &
*        NBS_STREXP made macros to avoid extensive recoding, hence the
*        global rather than local declaration of these two routines.
*        Removed the UNIX include file sys/shm.h to prevent clash with GCC
*        definition.
*        No longer check for negative relocated addresses
*        in NBS_RELOCATE_ADDRESS.
*        #module moved to nbs_module.h due as ULTRIX compiler neither supports
*        this, nor ignores the directive.
*        Error reporting added using EMS package.
*     05-May-1993 (DJA):
*        Added NBS_SLEEPMS to provide UNIX portable milli-second timing. Added
*        exit handler NBS_MLIST_EXITHANDLER to remove mapped global sections
*        on image exit.
*     17-May-1995 (DJA):
*        Data space segments forced to sizeof(double) alignment. Ensures
*        mapped data segments returned to C/Fortran are correctly aligned
*        for numeric data types in those languages.
*     12-Sep-1995 (DJA):
*        Added NBS_MAKE_KEY.
*     27-Apr-1999 (BKM):
*        Add include of <string.h> and omit unnecessary declarations.
*     28-Jun-2004 (AA):
*        Changed ifdef logic for Mac OSX support.
*     12-Sep-2004 (TIMJ):
*        Minimize compiler warnings. Use config.h
*     2-Sep-2005 (TIMJ):
*        Move protoypes out of functions (gcc 4 objects)
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* System includes */
#if HAVE_TIME_H
# include <time.h>
#endif

/* Include files	*/

#include <ctype.h>		    /* Character classification macros	*/
#include <stdio.h>		    /* Standard i/o defns		*/
#include <stdlib.h>                 /* Standard environment interface   */

#ifdef vms
#include <descrip.h>		    /* VMS descriptor definitions	*/
#include <secdef.h>		    /* Global section definitions	*/
#include <ssdef.h>		    /* System service error codes	*/
#endif


/* Assume unix shared memory if we have sys/ipc.h */

#if HAVE_SYS_IPC_H
# include <errno.h>		    /* Error code definitions		*/
# include <sys/types.h>		    /* System dependent types		*/
# include <sys/ipc.h>		    /* Inter-process comms definitions	*/
# include <sys/shm.h>                /* Shared memory definitions        */
# include <sys/time.h>
# include <string.h>
#endif


/* Error codes */

#include "nbs_err.h"

/* Structure definitions */

#include "nbs_typ.h"

/* Macro definitions   */

#include "nbs_mac.h"

/* Protoypes */

#include "nbs1.h"

/* Error processing */

#include "sae_par.h"
#include "ems.h"

/* Local static data */

static mapping_id nbs_gl_mlist = NIL; /* Mapping list */



/* see Apple Developer Connection Tech Notes
  http://developer.apple.com/technotes/tn2002/tn2071.html */
#if defined(unix) || ( defined(__APPLE__) && defined(__MACH__) )

/*
*+
*  Name:
*     NBS_MAKE_KEY

*  Purpose:
*     Create a UNIX key from a section name

*  Language:
*     ANSI C

*  Invocation:
*     (key_t *) = NBS_MAKE_KEY(NAME)

*  Description:
*     Generates a key name given a name string.

*  Arguments:
*     char * NAME (Given)
*        Address of null terminated section name

*  Returned Value:
*     key_t NBS_MAKE_KEY
*        Integer memory key

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-Sep-1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Prior Requirements:
*     None

*-
*/
key_t NBS_MAKE_KEY(char *name)
  {
  key_t	   key = (key_t) 0;
  int	i;

/* Start of code */
/* The key is the sum of the character values of NAME, with the character */
/* position of each character used to shift the bits to left. This is a */
/* simple protection against anagrams mapping on to the same key value */
   for( i=0; name[i]; i++ )
     key += ((key_t) name[i]) << i;

   return key;
}
#endif

/*
*
*  Section Name:
*     NBS_ALLOCATE

*  Purpose:
*     Manage storage allocation.

*  Language:
*     ANSI C

*  Description:
*     During noticeboard definition, space is allocated from a fixed size
*     pre-allocated area (so that we can be sure that it is allocated
*     contiguously), so there needs to be a set of routines for managing
*     allocation. The DATA_ALLOC routine does not really allocate data, but
*     returns a pointer on the assumption that data starts at an arbitrary
*     base address.

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     {enter_changes_here}
*
*/

/* External definitions   */

extern char *nbs_ga_alloc_base; /* Start of allocation buffer */
extern char *nbs_ga_alloc_next; /* Next free byte             */
extern char *nbs_ga_alloc_last; /* Byte after last free byte  */
extern char *nbs_ga_alloc_data; /* Next "free byte" for data  */


/*
*+
*  Name:
*     NBS_INIT_ALLOC

*  Purpose:
*     Allocate an area of contiguous memory and return a pointer to its start.
*     Also initialise for allocating memory for noticeboard data.

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_INIT_ALLOC (NBYTES,DATA)

*  Description:
*     Use MALLOC to allocate the memory
*     Set the base for noticeboard data allocation.
*
*     Note that noticeboard data is not really allocated. A counter is simply
*     maintained that shows where the data address would be. When the definition
*     phase is complete these data addresses are relocated to start just above
*     the noticeboard definition.

*  Arguments:
*     int NBYTES (Given)
*        Number of bytes of contiguous memory to allocate.
*     char * DATA (Given)
*        Address to use as base for allocating data memory.

*  Returned Value:
*     char *NBS_INIT_ALLOC
*        Pointer to first character of allocated memory area.

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        MALLOC         Allocate dynamic memory

*  Prior Requirements:
*     None

*-
*/
char *NBS_INIT_ALLOC (int nbytes,char *data)
{
/* Start of code */

/* Allocate the dynamic memory. Set up the variables pointing to the first
   byte and to the byte past the last byte. */

   nbs_ga_alloc_base = malloc (nbytes);
   if (nbs_ga_alloc_base == NIL) {
      nbs_ga_alloc_next = NIL;
      nbs_ga_alloc_last = NIL;
   } else {
      nbs_ga_alloc_next = nbs_ga_alloc_base;
      nbs_ga_alloc_last = nbs_ga_alloc_base + nbytes;
   }

/* Set up the variable pointing to the first byte to be used for noticeboard
   data.    */

   nbs_ga_alloc_data = data;

/* Return a pointer to the first byte of the dynamic memory.	*/

   return (nbs_ga_alloc_base);
}

/*
*+
*  Name:
*     NBS_ALLOC

*  Purpose:
*     Allocate NBYTES bytes from the private allocation buffer.

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_ALLOC (NBYTES)

*  Description:
*     If there is not enough memory, return NIL.
*     Coerce size of memory segment up to an integral number of quadwords.
*     Otherwise increment the top-of-memory pointer and return a pointer to
*     the first allocated byte.
*
*     This routine is used for allocating memory during the definition phase
*     for everything except for noticeboard data.

*  Arguments:
*     int NBYTES (Given)
*        Number of bytes of contiguous memory to allocate.

*  Returned Value:
*     char *NBS_ALLOC
*        Pointer to first character of allocated memory area.

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     17-May-1995 (DJA):
*        Ensure data address is sizeof(double) aligned
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None

*  Prior Requirements:
*     NBS_INIT_ALLOC must have been called.

*-
*/
char *NBS_ALLOC (int nbytes)
{
   int lnbytes = nbytes;

/* Start of code */

/* If there not enough memory left return NIL. Otherwise increment the top-of-
   memory pointer and return a pointer to the first byte allocated. */

   if (nbs_ga_alloc_next+lnbytes > nbs_ga_alloc_last)
      return (NIL);
   else {
      nbs_ga_alloc_next += lnbytes;
      return (nbs_ga_alloc_next-lnbytes);
   }
}

/*
*+
*  Name:
*     NBS_DATA_ALLOC

*  Purpose:
*     Return a pointer to an imaginary data area used for keeping track of
*     where noticeboard data will live.

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_DATA_ALLOC (NBYTES)

*  Description:
*     Increment the top-of-memory pointer and return a pointer to the first
*     allocated byte.
*
*     This routine is used for allocating memory during the definition phase
*     for noticeboard data. It allocates relative to the base address specified
*     in the call to NBS_INIT_ALLOC. The addresses so allocated are later
*     relocated to place the data immediately above the definition part of
*     the noticeboard.
*
*     This routine is used for allocating memory during the definition phase
*     for everything except for noticeboard data.

*  Arguments:
*     int NBYTES (Given)
*        Number of bytes of contiguous memory to allocate.

*  Returned Value:
*     char *NBS_DATA_ALLOC
*        Pointer to first character of allocated memory area.

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     17-May-1995 (DJA):
*        Ensure data address is sizeof(double) aligned
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None

*  Prior Requirements:
*     NBS_INIT_ALLOC must have been called.

*-
*/
char *NBS_DATA_ALLOC (int nbytes)
{
   int lnbytes = nbytes;

/* Start of code */

/* Increment the top-of-memory pointer and return a pointer to the first byte
   allocated. */

   nbs_ga_alloc_data += lnbytes;
   return (nbs_ga_alloc_data-lnbytes);
}

/*
*+
*  Name:
*     NBS_DEINIT_ALLOC

*  Purpose:
*     Free the storage obtained with NBS_INIT_ALLOC.

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_DEINIT_ALLOC ()

*  Description:
*     Simply use the appropriate RTL routine.

*  Arguments:
*     None

*  Returned Value:
*     char *NBS_ALLOC
*        (Char *) The value returned by FREE.

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        FREE          Release dynamic memory
*     None

*  Prior Requirements:
*     NBS_INIT_ALLOC must have been called.

*-
*/
char *NBS_DEINIT_ALLOC ()
{
/* Start of code */

/* Simply use the C RTL FREE routine to return the dynamic memory.  */
   free( nbs_ga_alloc_base);
   return NULL;
}

/*
*
*  Section Name:
*     NBS_FILE

*  Purpose:
*     Read and write Noticeboards to files.

*  Description:
      The standard C RTL routines FOPEN, FWRITE and FREAD are used. The notice-
      board definition file has a default extension of .NBD and is written
      using two calls to FWRITE. The first writes a header that contains the
      following information.

      	    Version number of software that wrote the file
	    Size of file (not including header)
	    Size of noticeboard definition
	    Total size of noticeboard global section
	    Longword time at which file was created (as returned by TIME())

      The second simply writes the byte stream that comprises the noticeboard
      definition and optionally the noticeboard data.

      When reading from the file, the header is read first, the version is then
      checked and the definition is read (the size of the definition is known
      from the header). For reading the routines are split into an open routine,
      a read routine and a close routine. This is necessary to allow the caller
      to allocate the buffer into which to read the definition.

*  Language:
*     ANSI C

*  History:
*     25-Mar-1988 (WFL):
*        Original version.
*     {enter_changes_here}

*  Notes:
*     See NBS_WRITE_FILE, NBS_OPEN_FILE

*  Bugs:
*     {note_any_bugs_here}
*
*/

/*
*+
*  Name:
*     NBS_WRITE_FILE

*  Purpose:
*     Create a noticeboard definition file, write header record, then write
*     noticeboard definition.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_WRITE_FILE (NAME,DATA,FILE_SIZE,DEFN_SIZE,
*        					   SECTION_SIZE,STATUS)

*  Description:
*     Create the file with a default file extension of .NBD
*     Encode and write the header.
*     Write the noticeboard definition.
*     Close the file.

*  Arguments:
*     FORTRAN/C string NAME (Given)
*        Name of file. Default file extension is .NBD.
*     char *DATA (Given)
*        Pointer to first byte of noticeboard definition.
*     int FILE_SIZE (Given)
*         Total size of file. This will be either DEFN_SIZE or SECTION_SIZE
*         and is the number of bytes to write to the file following the header.
*     int DEFN_SIZE (Given)
*         Size of definition part of noticeboard.
*     int SECTION_SIZE (Given)
*         Total size of global section.
*     int *STATUS (Given and returned)
*         The global status. Possible codes are,
*        	NBS__CANTOPEN	=> Can't open the file
*        	NBS__CANTWRITE	=> Can't write the file

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated string handling. Added error reporting.
*
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        FCLOSE            	Close a file
*        FOPEN            	Open a file
*        FWRITE            	Write to a file
*        TIME            	Get the current time
*     NBS:
*        NBS_STRIMP        Import a string, converting to upper-case

*  Prior Requirements:
*     None

*-
*/
void
NBS_WRITE_FILE (
                RW_CHARACTER(name),
                RW_BYTE_ARRAY(data),
                int file_size,
                int defn_size,
                int section_size,
                W_INTEGER(status) TRAIL(name) )
{
  GENPTR_CHARACTER(name)
  GENPTR_BYTE_ARRAY(data)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   char			fname[MAXFILE+1];
   FILE 		*chan;
   struct header_info	header;
   int			num;

/* Start of code */

   *status = SAI__OK;

/* Import name and create the file. */

   NBS_STRIMP (fname,name,MAXFILE);
   fname[MAXFILE] = '\0';
#ifdef vms
   chan = fopen (fname,"w","dna=.nbd");
#else
   chan = fopen (fname,"w");
#endif
   if (chan == NIL)
      {
      *status = NBS__CANTOPEN;
      emsRep( "NBS_WRITE_FILE_CANTOPEN",
                 "Can't open noticeboard definition file", status );
      }

/* Encode and write the header.	*/

   else {
      header.version = NBSVERSION;
      header.file_size = file_size;
      header.defn_size = defn_size;
      header.section_size = section_size;
      header.time = time(NIL);
      num = fwrite (&header,HEADER_SIZE,1,chan);
      if (num == 0)
         {
         *status = NBS__CANTWRITE;
         emsRep( "NBS_WRITE_FILE_CANTWRITE",
                 "Can't write noticeboard definition file", status );
         }

/* Write the noticeboard definition and optionally the noticeboard data. */

      else {
         num = fwrite (data,1,file_size,chan);
	 if (num == 0)
            {
            *status = NBS__CANTWRITE;
            emsRep( "NBS_WRITE_FILE_CANTWRITE",
                 "Can't write noticeboard definition file", status );
            }
      }

/* Close the file.  */

      fclose (chan);
   }
}

/*
*+
*  Name:
*     NBS_OPEN_FILE

*  Purpose:
*     Open a noticeboard definition file and read and decode its header record.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_OPEN_FILE (NAME,CHAN,FILE_SIZE,DEFN_SIZE,SECTION_SIZE,STATUS)

*  Description:
*     Open the file.
*     Read its header record.
*     Decode the header record and check the software version.

*  Arguments:
*     FORTRAN/C string NAME (Given)
*        Name of file. Default file extension is .NBD.
*     FILE **CHAN (Returned)
*        Channel on which file has been opened.
*     int FILE_SIZE (Returned)
*         Total size of file. This will be either DEFN_SIZE or SECTION_SIZE
*         and is the number of bytes to write to the file following the header.
*     int DEFN_SIZE (Returned)
*         Size of definition part of noticeboard.
*     int SECTION_SIZE (Returned)
*         Total size of global section.
*     int *STATUS (Given and returned)
*         The global status. Possible codes are,
*        	NBS__CANTOPEN	=> Can't open the file
*        	NBS__CANTREAD	=> Can't read the file
*        	NBS__BADVERSION	=> File written with wrong s/w version

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Updated string handling. Added error reporting.
*
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        FOPEN             Open a file
*        FREAD             Read from a file
*     NBS:
*       NBS_STRIMP   Import a string, converting to upper-case

*  Prior Requirements:
*     None

*-
*/
void
NBS_OPEN_FILE ( RW_CHARACTER(name),
                FILE **chan,
                W_INTEGER(file_size),
                W_INTEGER(defn_size),
                W_INTEGER(section_size),
                W_INTEGER(status) TRAIL(name) )
{
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(file_size)
  GENPTR_INTEGER(defn_size)
  GENPTR_INTEGER(section_size)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   char			fname[MAXFILE+1];
   struct header_info	header;
   int			num;

/* Start of code */

   *status = SAI__OK;

/* Import name and open the file. */

   NBS_STRIMP (fname,name,MAXFILE);
   fname[MAXFILE] = '\0';
#ifdef vms
   *chan = fopen (fname,"r","dna=.nbd");
#else
   *chan = fopen (fname,"r");
#endif
   if (*chan == NIL)
      {
      *status = NBS__CANTOPEN;
      emsRep( "NBS_OPEN_FILE_CANTOPEN",
                 "Can't open noticeboard definition file", status );
      }

/* Read and decode the header.	*/

   else {
      num = fread (&header,HEADER_SIZE,1,*chan);
      if (num == 0)
         {
	 *status = NBS__CANTREAD;
         emsRep( "NBS_OPEN_FILE_CANTREAD",
                 "Can't read noticeboard definition file", status );
         }

/* Check that the file was written with the current software version.	*/

      else if (header.version != NBSVERSION)
         {
         *status = NBS__BADVERSION;
         emsRep( "NBS_OPEN_FILE_BADVER",
                "Noticeboard or definition file had wrong version", status );
         }
      else {
	 *file_size = header.file_size;
	 *defn_size = header.defn_size;
	 *section_size = header.section_size;
      }
   }
}

/*
*+
*  Name:
*     NBS_READ_FILE

*  Purpose:
*     Reads noticeboard data from opened and positioned file to memory.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_READ_FILE (CHAN,DEFN_SIZE,DATA,STATUS)

*  Description:
*     Simply read the data into the user's buffer.

*  Arguments:
*     FILE **CHAN (Given)
*        Channel on which file has been opened.
*     int DEFN_SIZE (Given)
*         Size of definition part of noticeboard.
*     int *STATUS (Given and returned)
*         The global status. Possible codes are,
*        	NBS__CANTOPEN	=> Can't open the file

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        FREAD            Read from a file

*  Prior Requirements:
*     None

*-
*/
void
NBS_READ_FILE ( FILE *chan,
                int defn_size,
                RW_BYTE_ARRAY(data),
                W_INTEGER(status) )
{
  GENPTR_BYTE_ARRAY(data)
  GENPTR_INTEGER(status)

/* Local variable declarations */

   int			num;

/* Start of code */

   *status = SAI__OK;

/* Simply read the file into the user's buffer.	*/

   num = fread (data,1,defn_size,chan);
   if (num == 0)
      {
      *status = NBS__CANTREAD;
      emsRep( "NBS_READ_FILE_CANTREAD",
                 "Can't read noticeboard definition file", status );
      }
}

/*
*+
*  Name:
*     NBS_CLOSE_FILE

*  Purpose:
*     Close a noticeboard definition file.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_CLOSE_FILE (CHAN)

*  Description:
*     Simply close the file using the appropriate RTL routine.

*  Arguments:
*     FILE **CHAN (Given)
*        Channel on which file has been opened.

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        FCLOSE            Close a file

*  Prior Requirements:
*     None

*-
*/
void
NBS_CLOSE_FILE ( RW_POINTER(chan) )
{
  GENPTR_POINTER(chan)

/* Start of code */

/* Simply close the file.   */

   fclose ((FILE *) chan);
}

/*
*+
*  Name:
*     NBS_OPEN_WRITE

*  Purpose:
*     Open a noticeboard definition file for append update access.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_OPEN_WRITE (SAVE_NAME,CHAN,STATUS)

*  Description:
*     Simply open the file for append update access.

*  Arguments:
*     char *SAVE_NAME (Given)
*        Name of file. Default file extension is .NBD.
*     FILE **CHAN (Returned)
*        Channel on which file has been opened for write access.
*     int *STATUS (Given and returned)
*         The global status. Possible codes are,
*        	NBS__CANTOPEN	=> Can't open the file

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1988, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     25-Mar-1988 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        FOPEN            Open a file

*  Prior Requirements:
*     None

*-
*/
void
NBS_OPEN_WRITE (char *save_name, FILE **chan, int *status)
{
/* Start of code */

   *status = SAI__OK;

/* Simply open the file. */

#ifdef vms
   *chan = fopen (save_name,"a+","dna=.nbd");
#else
   *chan = fopen (save_name,"a+");
#endif
   if (*chan == NIL)
      {
      *status = NBS__CANTOPEN;
      emsRep( "NBS_OPEN_WRITE_CANTOPEN",
                 "Can't open noticeboard definition file", status );
      }
}

/*
*+
*  Name:
*     NBS_UPDATE_FILE

*  Purpose:
*     Write the contents of a noticeboard to its noticeboard definition
*     file.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_UPDATE_FILE (CHAN,SECTION,SECTION_SIZE,STATUS)

*  Description:
*     Position the file to the beginning of the definition part.
*     Write the noticeboard definition and data to the file.
*
*     The file is assumed to be open and is not closed afterwards.

*  Arguments:
*     FILE *CHAN (Given)
*        Channel number on which file is open.
*     char *SECTION (Given)
*        Pointer to first byte of noticeboard.
*     int SECTION_SIZE (Given)
*        Size of noticeboard. It begins at an offset of
*        "length of header" into the file.
*     int *STATUS (Given and returned)
*         The global status. Possible codes are,
*        	NBS__CANTWRITE	=> Can't write the file

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1988, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     28-Mar-1988 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        FSEEK      Position to a given offset within a file
*        FWRITE            Write to a file

*  Prior Requirements:
*     None

*-
*/
void
NBS_UPDATE_FILE (FILE *chan,char *section,int section_size,int *status)
{

/* Local variable declarations */

   int seek;
   int num;

/* Start of code */

   *status = SAI__OK;

/* Position to the end of the file header. */

   seek = fseek (chan,HEADER_SIZE,0);
   if (seek != 0)
      {
      *status = NBS__CANTWRITE;
      emsRep( "NBS_UPDATE_FILE_CANTWRITE",
                 "Can't write noticeboard definition file", status );
      }

/* Update the noticeboard contents. */

   else {
      num = fwrite (section,1,section_size,chan);
      if (num == 0)
         {
         *status = NBS__CANTWRITE;
         emsRep( "NBS_WRITE_FILE_CANTWRITE",
                 "Can't write noticeboard definition file", status );
         }
   }
}

/*
*
*  Section Name:
*     NBS_MAP

*  Purpose:
*     Creation, mapping and unmapping of noticeboard global sections.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_UPDATE_FILE (CHAN,SECTION,SECTION_SIZE,STATUS)

*  Description:
*     There are three routines in this section. One creates a global section of
*     a given size and returns its starting address. One maps an existing
*     global section. The last one unmaps an existing global section.
*
*/

/*
*+
*  Name:
*     NBS_CREATE_SECTION

*  Purpose:
*     Create a global section with a given name and of a given size and return
*     its starting address.

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_CREATE_SECTION (NAME,SECTION_SIZE,STATUS)

*  Description:
*     Import the name and generate a descriptor with trailing blanks removed.
*     Create a demand-zero global section.
*     Return its starting address.

*  Arguments:
*     FORTRAN/C string NAME (Given)
*        Name of global section. Will be imported (ignores all white space
*        and non-printing characters and converts to upper case) before use.
*     int SECTION_SIZE (Given)
*        Total size of global section.
*     int *STATUS (Given and returned)
*         The global status. Possible codes are,
*        	NBS__SECTIONEXISTED => Section of this name existed
*        	System service status from SYS$CRMPSC

*  Returned Value:
*     (Char *) NBS_CREATE_SECTION
*        Pointer to first byte of global section.

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting and string handling.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     VMS RTL:
*        SYS$CRMPSC    Create and map a private or global section
*     UNIX RTL:
*        SHMGET        Locate a global section by name
*        SHMAT         Map a global section onto the process address space
*     C RTL:
*        STRLEN        Length of a string
*     NBS:
*        NBS_STRIMP    Import a string, converting to upper-case.
*        NBS_MLIST_ADD Add a mapped section to the list of
*                      sections mapped by this process

*  Prior Requirements:
*     None

*-
*/
char *NBS_CREATE_SECTION ( RW_CHARACTER(name), int section_size,
                           W_INTEGER(status) TRAIL(name) )
{
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

/* Common local variable declarations */

   int		flags;		/* Control section type, mode etc	     */
   char		tname[MAXNAME+1]; /* C string for imported name		     */
   char		*retadr[2];	/* Returned as start / end virtual addresses */

/* VMS-specific code */

#ifdef vms

/* External function declarations   */

   extern int	sys$crmpsc();

/* Local variable declarations */

   struct dsc$descriptor sname;	/* Descriptor for imported name	     */
   char		*inadr[2];	/* Used only indicate section in P0 region   */
   int		pagcnt;		/* Size of section in pages		     */

/* Start of code */

/* Import NAME to TNAME (ensure that TNAME is null-terminated even if all
   characters of NAME are significant). Then set up SNAME's descriptor to
   correspond to the imported NAME (white space and non-printing characters
   removed, and converted to upper-case).    */

   NBS_STRIMP (tname,name,MAXNAME);
   tname[MAXNAME] = '\0';
   sname.dsc$b_dtype = DSC$K_DTYPE_T;
   sname.dsc$b_class = DSC$K_CLASS_S;
   sname.dsc$w_length = strlen (tname);
   sname.dsc$a_pointer = tname;

/* This indicates that the section is to be mapped into P0. */

   inadr[0] = (char *) 0x200;

/* Section will be global, program region will be expanded, it will be write-
   able, it will be mapped to the pagefile and it will be demand zero.	*/

   flags = SEC$M_GBL | SEC$M_EXPREG | SEC$M_WRT | SEC$M_PAGFIL | SEC$M_DZRO;

/* Calculate number of pages in the section and then create and map it.	*/

   pagcnt = (section_size + 511) / 512;
   *status = sys$crmpsc (inadr,retadr,0,flags,&sname,0,0,0,pagcnt,0,0,0);

/* OK status means the section existed. For us this is an error. We hope to
   get SS$_CREATED status, which means that the section has just been created.*/

   if (*status == SS$_NORMAL)
      *status = NBS__SECTIONEXISTED;
   else if (*status == SS$_CREATED)
      *status = SAI__OK;

/* End of VMS-specific section 	*/

#endif

/* see Apple Developer Connection Tech Notes
  http://developer.apple.com/technotes/tn2002/tn2071.html */
#if defined(unix) || ( defined(__APPLE__) && defined(__MACH__) )

/* UNIX-specific section	*/

/* Local variable declarations	*/
   key_t	key;		/* Key to shared memory segment */
   int		memid;		/* Shared memory segment id */

/* Start of code	*/

/* Initially assume failure. All errors are reported using the error and
   message service */
   *status = NBS__SECTIONEXISTED;
   retadr[0] = (char *) -1;

/* Import NAME to TNAME (ensure that TNAME is null-terminated even if all
   characters of NAME are significant). */
   NBS_STRIMP (tname,name,MAXNAME);
   tname[MAXNAME] = '\0';

/* Make a key identifier from the name */
   key = NBS_MAKE_KEY( tname );

/* Create the shared memory segment. It must not already exist. Give all access
   to all processes.	*/
   flags = 0x1ff | IPC_CREAT | IPC_EXCL;
   if ((memid = shmget (key,section_size,flags)) == -1) {
      emsRep( "NBS_CREATE_SECTION_SECEXIST",
                 "Section already existed", status );
      }

/* Attach to it */
   else if ((int)(retadr[0] = shmat (memid,(char *)0,0)) == -1) {
      emsRep( "NBS_CREATE_SECTION_ERRMAP",
                 "Error mapping global memory section", status );
      }

/* If we get here, everything has worked.	*/

   else {
      *status = SAI__OK;
      NBS_MLIST_ADD ( YES, memid, retadr[0], status );
      }

/* End of UNIX-specific section.	*/

#endif

/* Return a pointer to the first byte in the section.	*/

   return (retadr[0]);
}

/*
*+
*  Name:
*     NBS_MAP_SECTION

*  Purpose:
*     Map a global section with a given name and return its starting address.

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_MAP_SECTION (NAME,STATUS)

*  Description:
*     Import the name and generate a descriptor with trailing blanks removed.
*     Map the global section.
*     Return its starting address.

*  Arguments:
*     FORTRAN/C string NAME (Given)
*        Name of global section. Will be imported (ignores all white space
*        and non-printing characters and converts to upper case) before use.
*     int *STATUS (Given and returned)
*         The global status. Possible codes are,
*        	NBS__SECTIONNOTFOUND => Section of this name not found
*        	System service status from SYS$MGBLSC

*  Returned Value:
*     (Char *) NBS_MAP_SECTION
*        Pointer to first byte of global section.

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting and string handling.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     VMS RTL:
*        SYS$MGBLSC     Map a global section
*     UNIX RTL:
*        SHMGET         Locate a global section by name
*        SHMAT          Map a global section onto the process address space
*     C RTL:
*        STRLEN         Length of a string
*     NBS:
*        NBS_STRIMP     Import a string, converting to upper-case.
*        NBS_MLIST_ADD  Add a mapped section to the section list
*        NBS_MLIST_FIND Locate a section in the section list

*  Prior Requirements:
*     None

*-
*/
char *NBS_MAP_SECTION ( RW_CHARACTER(name), W_INTEGER(status) TRAIL(name) )
{
  GENPTR_CHARACTER(name)
  GENPTR_INTEGER(status)

/* Common local variable declarations	*/

   char		*retadr[2];	/* Returned as start / end virtual addresses */
   char		tname[MAXNAME+1]; /* C string for imported name		     */
   long		flags;		/* Control section type, mode etc	     */

/* VMS-specific code */

#ifdef vms

/* External function declarations   */

   extern int	sys$mgblsc();

/* Local variable declarations */

   struct dsc$descriptor sname;	/* Descriptor for imported name	     */
   char		*inadr[2];	/* Used only indicate section in P0 region   */

/* Start of code */

/* Import NAME to TNAME (ensure that TNAME is null-terminated even if all
   characters of NAME are significant). Then set up SNAME's descriptor to
   correspond to the imported NAME (white space and non-printing characters
   removed, and converted to upper-case).    */

   NBS_STRIMP (tname,name,MAXNAME);
   tname[MAXNAME] = '\0';
   sname.dsc$b_dtype = DSC$K_DTYPE_T;
   sname.dsc$b_class = DSC$K_CLASS_S;
   sname.dsc$w_length = strlen (tname);
   sname.dsc$a_pointer = tname;

/* This indicates that the section is to be mapped into P0. */

   inadr[0] = (char *) 0x200;

/* Program region will be expanded and section will be writeable.   */

   flags = SEC$M_EXPREG | SEC$M_WRT;

/* Map the section. */

   *status = sys$mgblsc (inadr,retadr,0,flags,&sname,0,0);

/* OK status means the section existed. This is what we expect.	The only other
   status that we expect in normal circumstances is SS$_NOSUCHSEC.  */

   if (*status == SS$_NORMAL)
      *status = SAI__OK;
   else if (*status == SS$_NOSUCHSEC)
      {
      *status = NBS__SECTIONNOTFOUND;
      emsRep( "NBS_MAP_SECTION_SECNOTFOUND", "Section not found", status );
      }

/* End of VMS-specific section.	*/

#endif


/* see Apple Developer Connection Tech Notes
  http://developer.apple.com/technotes/tn2002/tn2071.html */
#if defined(unix) || ( defined(__APPLE__) && defined(__MACH__) )


/* UNIX-specific section	*/

/* Local variable declarations	*/

   key_t	key;		/* Key to shared memory segment */
   int		memid;		/* Shared memory segment id */

/* Start of code	*/

/* Initially assume failure. All errors are reported using the error and
   message service */

   *status = NBS__SECTIONNOTFOUND;

/* Import NAME to TNAME (ensure that TNAME is null-terminated even if all
   characters of NAME are significant). */

   NBS_STRIMP (tname,name,MAXNAME);
   tname[MAXNAME] = '\0';

/* Make a key identifier from the name */
   key = NBS_MAKE_KEY( tname );

/* Get the id of the shared memory segment - it must already exist.	*/
   flags = 0;
   if ((memid = shmget (key,0,flags)) == -1) {
      emsRep( "NBS_CREATE_SECTION_SECNOTFOUND",
                 "Section not found", status );
      }
   else if ((int)(retadr[0] = NBS_MLIST_FIND (memid,status)) != -1 )
      *status = SAI__OK;

/* Attach to the shared memory segment.	*/
   else if ((int)(retadr[0] = shmat (memid,(char*)0,0)) == -1) {
      emsRep( "NBS_MAP_SECTION_ERRMAP",
                 "Error mapping global memory section", status );
      }

/* If we get here, everything has worked. */
   else {
      *status = SAI__OK;
      NBS_MLIST_ADD ( NO, memid, retadr[0], status );
      }

/* End of UNIX-specific section.	*/

#endif

/* Return a pointer to the first byte in the section.	*/

   return (retadr[0]);
}

/*
*+
*  Name:
*     NBS_UNMAP_SECTION

*  Purpose:
*     Unmap a specified global section .

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_UNMAP_SECTION (START,SECTION_SIZE,STATUS)

*  Description:
*     Unmap the global section.

*  Arguments:
*     char *START (Given)
*        Start address of the global section.
*     int SECTION_SIZE (Given)
*         Total size of global section.
*     int *STATUS (Given and returned)
*         The global status. Possible codes are,
*        	System service status from SYS$DELTVA

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     02-Feb-1990 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Added error reporting and string handling.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     VMS RTL:
*        SYS$DELTVA        Map a global section
*     NBS:
*        NBS_MLIST_UNMAP   Unmap a section in the section list

*  Prior Requirements:
*     None

*-
*/
void
NBS_UNMAP_SECTION ( RW_POINTER(start), int section_size, W_INTEGER(status) )
{
  GENPTR_POINTER(start)
  GENPTR_INTEGER(status)

/* VMS-specific code */

#ifdef vms

/* External function declarations   */

   extern int	sys$deltva();

/* Local variable declarations */

   char		*inadr[2];	/* Start / end virtual addresses */

/* Start of code */

/* Set up start and end virtual addresses. */

   inadr[0] = start;
   inadr[1] = start + section_size - 1;

/* Unmap the section. */

   *status = sys$deltva (inadr,0,0);

/* OK status means the section has been successfully unmapped.	*/

   if (*status == SS$_NORMAL)
      *status = SAI__OK;

/* End of VMS-specific section.	*/

#endif


/* see Apple Developer Connection Tech Notes
  http://developer.apple.com/technotes/tn2002/tn2071.html */
#if defined(unix) || ( defined(__APPLE__) && defined(__MACH__) )


/* UNIX-specific section	*/

NBS_MLIST_UNMAP ((char *) start,status);

/* End of UNIX-specific section.	*/

#endif
}

/*
*
*  Section Name:
*     NBS_RELOCATE

*  Purpose:
*     Routines to relocate pointers. Various routines are provided to operate
*     at different levels.

*  Language:
*     ANSI C

*  Description:
*     These routines are used for relocating pointers before copying noticeboard
*     definitions to disc files or global sections and after having copied
*     noticeboard definitions from disc files or global sections. The highest-
*     level routine will recursively relocate all pointers in an entire notice-
*     board definition. It calls a lower-level routine that will do the same
*     for a single item descriptor and this in turn calls a lower-level routine
*     that will relocate a single address.
*
*     Efficiency is not a major concern here since relocation is only done when
*     noticeboard definitions are being saved or restored and when processes are
*     mapping new noticeboards. If efficiency were more important then the low-
*     est level routine should be a macro. Also the top-level routine is very
*     heavily recursive - there can be as many active calls as there are items
*     at a given level - and should ideally be re-written to avoid this recurs-
*     ion.

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     {enter_changes_here}
*
*/

/*
*+
*  Name:
*     NBS_RELOCATE_POINTERS

*  Purpose:
*     Recursively relocate all pointers in the hierarchical structure rooted
*     at a specified point.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_RELOCATE_POINTERS (ID,I_OFFSET,FBS_OFFSET,D_OFFSET,ADD)

*  Description:
*     Do nothing if passed a NIL identifier.
*     If adding offsets beforehand relocate pointers at the current level.
*     Make a recursive call to relocate the eldest child tree.
*     Make a recursive call to relocate the next sibling tree.
*     If subtracting offsets afterwards relocate pointers at the current level.
*
*     This routine may be called either to turn real virtual addresses into
*     process-independent numbers suitable for storing in a noticeboard defin-
*     ition file or a global section, or to turn such process-independent num-
*     bers into real virtual addresses. In the former case, the relocation
*     must be done after all use of the pointers as virtual addresses has been
*     completed (ie at the end of the routine). In the latter case, the reloc-
*     ation must be done before any use of the pointers as virtual addresses
*     (ie at the beginning of the routine).

*  Arguments:
*     int *ID  (Given)
*        Identifier of root of tree. If this is a NIL identifier then the
*        routine does nothing.
*     int I_OFFSET (Given)
*        Offset to apply to pointers to item descriptors
*     int FBS_OFFSET (Given)
*        Offset to apply to pointers to fixed information,
*        board information and shape information.
*     int D_OFFSET (Given)
*        Offset to apply to pointers to noticeboard data.
*     int ADD (Given)
*        Whether add the offsets at the beginning (1) or subtract them
*        at the end (0).

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_RELOCATE_ITEM     Relocate pointers for an item at a single level
*        NBS_RELOCATE_POINTERS Relocate pointers for an item and all
*                              lower-level items

*  Prior Requirements:
*     None

*-
*/
void
NBS_RELOCATE_POINTERS (item_id id,int i_offset,int fbs_offset,
                       int d_offset,int add)
{

/* Start of code */

/* If given a NIL identifier, do nothing at all.    */

   if (id != NIL) {

/* If the pointers are not virtual addresses on entry, relocate them so that
   they are.	*/

      if (add)
	 NBS_RELOCATE_ITEM (id,i_offset,fbs_offset,d_offset,add);

/* Recursively relocate all pointers in the eldest child and next sibling
   sub-trees.	*/

      NBS_RELOCATE_POINTERS (id->heir,i_offset,fbs_offset,d_offset,add);
      NBS_RELOCATE_POINTERS (id->sibling,i_offset,fbs_offset,d_offset,add);

/* If the pointers were virtual addresses on entry, relocate them so that
   they are not any more.	*/

      if (!add)
	 NBS_RELOCATE_ITEM (id,i_offset,fbs_offset,d_offset,add);
   }
}

/*
*+
*  Name:
*     NBS_RELOCATE_ITEM

*  Purpose:
*     Relocate all the pointers associated with a given item.

*  Language:
*     ANSI C

*  Invocation:
*     (Void) = NBS_RELOCATE_ITEM (ID,I_OFFSET,FBS_OFFSET,D_OFFSET,ADD)

*  Description:
*     Simply call the single address relocation routine once for each pointer
*     in the item descriptor, taking care to pass the correct offset for each
*     pointer.

*  Arguments:
*     int *ID  (Given)
*        Item identifier.
*     int I_OFFSET (Given)
*        Offset to apply to pointers to item descriptors
*     int FBS_OFFSET (Given)
*        Offset to apply to pointers to fixed information,
*        board information and shape information.
*     int D_OFFSET (Given)
*        Offset to apply to pointers to noticeboard data.
*     int ADD (Given)
*        Whether add the offsets at the beginning (1) or subtract them
*        at the end (0).

*  Returned Value:
*     (void)

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_RELOCATE_ADDRESS  Relocate a single address

*  Prior Requirements:
*     None

*-
*/
void
NBS_RELOCATE_ITEM (item_id id,int i_offset,int fbs_offset,int d_offset,int add)
{

/* Start of code */

/* Simply relocate each address one by one. */

   id->vp.parent  = (item_id)
   		   NBS_RELOCATE_ADDRESS ((char *) id->vp.parent,i_offset  ,add);
   id->heir       = (item_id)
   		   NBS_RELOCATE_ADDRESS ((char *) id->heir     ,i_offset  ,add);
   id->sibling    = (item_id)
   		   NBS_RELOCATE_ADDRESS ((char *) id->sibling  ,i_offset  ,add);
   id->fixed      = (fixed_id)
   		   NBS_RELOCATE_ADDRESS ((char *) id->fixed    ,fbs_offset,add);
   id->gs.shape   = (shape_id)
   		   NBS_RELOCATE_ADDRESS ((char *) id->gs.shape ,fbs_offset,add);
   id->board      = (board_id)
   	    	   NBS_RELOCATE_ADDRESS ((char *) id->board    ,fbs_offset,add);
   id->da.data    = (data_id)
   	    	   NBS_RELOCATE_ADDRESS ((char *) id->da.data  ,d_offset  ,add);
}

/*
*+
*  Name:
*     NBS_RELOCATE_ADDRESS

*  Purpose:
*     Relocate an address (provided it is not NIL and will not result in a
*     NIL address when relocated).

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_RELOCATE_ADDRESS (ADDRESS,OFFSET,ADD)

*  Description:
*     If subtracting the offset, negate it.
*     If the address is not NIL and the address after relocation will not be
*     NIL, return the relocated address.
*     Otherwise return the unaltered address.

*  Arguments:
*     int ADDRESS (Given)
*        Address which is to be offset.
*     int OFFSET (Given)
*        Offset to apply to the address.
*     int ADD (Given)
*        Whether add the offsets (1) or subtract them (0).

*  Returned Value:
*     (Char *) NBS_RELOCATE_ADDRESS
*        The relocated address.

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Removed check for negative address as both ULTRIX and SunOS can
*        generate such legal addresses.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     None

*  Prior Requirements:
*     None

*-
*/
char *NBS_RELOCATE_ADDRESS (char *address,int offset,int add)
{

/* Start of code */

/* If subtracting the offset, negate it.    */

   if (!add)
      offset = -offset;

/* If the address os not NIL and if the result of offsetting is not NIL
   return the offset address. Otherwise return the unaltered input address. */

   if (address != NIL && ((address + offset) != NIL))
      return ((char *) ((int) address + offset));
   else
      return (address);
}

/*
*
*  Section name:
*     NBS_STR

*  Purpose:
*     Utility routines for importing strings passed by descriptor to C strings
*     and exporting C strings to strings passed by descriptor.

*  Language:
*     ANSI C

*  Description:
*     There are two routines, one to import and one to export. Their interfaces
*     are modelled after the C RTL STRNCPY routine.

*  History:
*     23-Jul-87 - (WFL):
*        Original version.
*     22-Mar-93 - (DJA):
*        Extensive alterations to handle Fortran <-> C string conversions
*        on architectures supported by the Starlink F77 package.
*     {enter_changes_here}
*
*/

/*
*+
*  Name:
*     NBS_STRIMP

*  Purpose:
*     Import a C or Fortran string to a C null-terminated string.

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_STRIMP (OUT,IN,MAXCHAR)

*  Description:
*     Copy and capitalise all printing non-white characters up to the end
*     of string or the specified limit (whichever occurs first). If there is
*     room in the output string, append a zero byte.

*  Arguments:
*     char * OUT (Returned)
*        Output string.
*     FORTRAN/C string IN (Given)
*        Input string.
*     int MAXCHAR (Given)
*        Maximum number of characters to copy to the output string.

*  Returned Value:
*     (Char *) NBS_STRIMP
*        Pointer to the first character in the output string.

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Recoded using F77 and CNF packages.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        ISPRINT       Is a character a printing character?
*        ISSPACE       Is a character a white-space character?
*        TOUPPER       Convert a character to upper-case.
*        STRLEN        Length of C string

*  Prior Requirements:
*     None

*-
*/
#ifdef c_string
char *NBS_STRIMP ( char *out, RW_CHARACTER(in),
                   int maxchar TRAIL(in) )
{
#else
char *F77_EXTERNAL_NAME(nbs_strimp) ( char *out, RW_CHARACTER(in),
                                      int maxchar TRAIL(in) )
  {
  GENPTR_CHARACTER(in)
#endif

/* Local variable declarations	*/

   int		length;
   int		in_ptr;
   int		out_ptr;
   char         *in_cptr;

/* Start of code */

/* Determine length of input string.	*/

   in_cptr = in;
#ifdef c_string
   length = strlen (in);
#else
   length = in_length;
#endif

/* Loop through characters in the input string until it is exhausted or the
   output string is full. Only copy printing non-white-space characters and
   convert to upper-case.   */

   for (in_ptr=out_ptr = 0; in_ptr < length && out_ptr < maxchar; in_ptr++)
      if (!isprint (in_cptr[in_ptr]) || isspace (in_cptr[in_ptr]))
         ;
      else
         out[out_ptr++] = toupper (in_cptr[in_ptr]);

/* If there is room, append a null byte to the output string.	*/

   if (out_ptr < maxchar)
      out[out_ptr] = '\0';
   return (out);
}

/*
*+
*  Name:
*     NBS_STREXP

*  Purpose:
*     Export a C string to a C or FORTRAN string

*  Language:
*     ANSI C

*  Invocation:
*     NBS_STREXP (OUT,IN,MAXCHAR)

*  Description:
*     Copy and then space pad. The C string may not be terminated if it is of
*     the maximum length; hence the maxchar argument.

*  Arguments:
*     FORTRAN/C string OUT (Returned)
*        Input string.
*     char * IN (Given)
*        Output string.
*     int MAXCHAR (Given)
*        Maximum number of characters to copy from the input string.

*  Returned Value:
*     (Void)

*  Copyright:
*     Copyright (C) 1987, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     23-Jul-1987 (WFL):
*        Original version.
*     22-Mar-1993 (DJA):
*        Recoded using F77 and CNF packages.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     CNF:
*        CNF_EXPN      Export C string to FORTRAN string

*  Prior Requirements:
*     None

*-
*/
#ifdef c_string
void nbc_strexp( RW_CHARACTER(out),
                 char *in, int maxchar TRAIL(out) )
{
#else
void F77_EXTERNAL_NAME(nbs_strexp)( RW_CHARACTER(out),
                      char *in, int maxchar TRAIL(out) )
  {
  GENPTR_CHARACTER(out)
#endif

/* Local variable declarations	*/

#ifdef c_string
   int		length;
   int		in_ptr;
   int	     	out_ptr;
#endif

/* Start of code */

/* Determine length of output string.	*/

#ifdef c_string
   length = HUGE;

/* Loop through characters in the input string until it is exhausted or the
   output string is full.   */

   in_ptr = out_ptr = 0;
   while (in[in_ptr] != '\0' && in_ptr < maxchar && out_ptr < length)
      out[out_ptr++] = in[in_ptr++];

/* If there is space in the output string, terminate it */

   if (out_ptr < length)
      out[out_ptr++] = '\0';

#else
   cnfExpn( in, maxchar, out, out_length );

#endif

}




/* see Apple Developer Connection Tech Notes
  http://developer.apple.com/technotes/tn2002/tn2071.html */
#if defined(unix) || ( defined(__APPLE__) && defined(__MACH__) )


/*
*  Section name:
*     NBS_MLIST

*  Purpose:
*     Manage list of global sections mapped by this process

*  Language:
*     ANSI C

*  Description:
*     The UNIX shared memory system prohibits multiple mapping of the
*     same global section by the same process. As NBS relies on the
*     the ability to call the mapping routines twice (although it doesn't
*     rely on the mappings occurring to different process addresses) a
*     list of mappings is maintained. The NBS_MLIST routines add, delete
*     and locates entries within the mapping list.

*  History:
*     23-Mar-93 - (DJA):
*        Original version.
*     {enter_changes_here}
*
*/

/*
*+
*  Name:
*     NBS_MLIST_EXITHANDLER

*  Purpose:
*     Remove any global sections present on process exit

*  Language:
*     ANSI C

*  Invocation:
*     (void) = NBS_MLIST_EXITHANDLER()

*  Description:
*     A process using the UNIX version of NBS may leave behind mapped
*     global sections due to failed or absent NBS_LOSE_NOTICEBAORD calls.
*     This exit handler removes the mappings between the process address
*     space and all such sections in order that they can be removed from
*     the system without further actionn by the user.

*  Returned Value:
*     (Void)

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     30-Apr-1993 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     NBS:
*        NBS_MLIST_UNMAP   Unmap a global section from process address space

*  Prior Requirements:
*     None

*-
*/
void NBS_MLIST_EXITHANDLER ()
{
/* Local variable declarations	*/
  int            lstatus = SAI__OK;

  /* Keep reducing the mapping list until it reaches zero size */

  while ( nbs_gl_mlist && (lstatus==SAI__OK) )
    {

    /* Coerce the reference count to one. This is safe because UNIX can't */
    /* map the same section more than once, hence the reference count is  */
    /* simply an NBS limit imposed on external callers and can be ignored */

    nbs_gl_mlist->acount = 1;

    /* Remove the section */

    NBS_MLIST_UNMAP( nbs_gl_mlist->addr, &lstatus );
    }
}

/*
*+
*  Name:
*     NBS_MLIST_ADD

*  Purpose:
*     Add a mapped section to the process's mapped section list

*  Language:
*     ANSI C

*  Invocation:
*     (void) = NBS_MLIST_ADD(CREATOR,MEMID,ADDR,STATUS)

*  Description:
*     Adds the address of the specified mapped section to this process's
*     list of mapped sections. If the error NBS__IMPOSSIBLE is returned,
*     a programming error is indicated as the logic in the routines using
*     the NBS_MLIST_ functions should ensure that NBS_MLIST_FIND is called
*     first. The first time this routine is called, the exit handler to
*     remove unmapped global section is installed.

*  Arguments:
*     int CREATOR (Given)
*        Is the caller the first mapper of the noticeboard?
*     int MEMID (Given)
*        Global section memory identifier
*     char *ADDR (Given)
*        Address at which section has been mapped
*     int *STATUS (Given and returned)
*        Global status. Possible codes are,
*           NBS__IMPOSSIBLE => Section already mapped, indicates failure
*           of logic elsewhere in NBS

*  Returned Value:
*     (Void)

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     05-Feb-1993 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        MALLOC        Allocate dynamic memory

*  Prior Requirements:
*     None

*-
*/
void NBS_MLIST_ADD ( int creator, int memid, char *addr, int *status )
{
/* Local variable declarations	*/
  static int     first = YES;
  int            found = NO;
  mapping_id     mapping;

if ( nbs_gl_mlist )
  {
  mapping = nbs_gl_mlist;
  while ( mapping && !found )
    {
    if ( mapping->memid == memid )
      found = YES;
    else
      mapping = mapping->next;
    }
  }
else if ( first )
  {
#if HAVE_ATEXIT
  atexit( NBS_MLIST_EXITHANDLER );
#else
#  if HAVE_ON_EXIT
  on_exit( NBS_MLIST_EXITHANDLER, NULL );
#  else
    error "no way to register exit handler"
#  endif
#endif
  first = NO;
  }

if ( found )
  {
  *status = NBS__IMPOSSIBLE;
  emsRep( "NBS_MLIST_ADD_IMPOS",
             "Section already exists, internal NBS error", status );
  }
else
  {
  mapping = (mapping_id) malloc(sizeof(struct mapping_info));
  if ( mapping == NIL )
    {
    *status = NBS__INITALLOCFAILED;
    emsRep( "NBS_MLIST_ADD_ALLOCFAIL",
               "Memory allocation failure", status );
    }
  else
    {
    mapping->acount = 1;
    mapping->destroy = creator;
    mapping->memid = memid;
    mapping->addr = addr;
    mapping->next = nbs_gl_mlist;
    nbs_gl_mlist = mapping;
    }
  }
}

/*
*+
*  Name:
*     NBS_MLIST_FIND

*  Purpose:
*     Requests a copy of the pointer to a global section which may already
*     be mapped. If it is not already mapped, then this routine returns -1

*  Language:
*     ANSI C

*  Invocation:
*     (Char *) = NBS_MLIST_FIND (MEMID,STATUS)

*  Description:
*     The list of sections mapped by this process is searched for the
*     required memory identifier. If found, the process address at which
*     the section mapped is returned, otherwise -1.

*  Arguments:
*     int MEMID (Given)
*        Global section memory identifier
*     int *STATUS (Given and returned)
*        Global status. Possible codes are,

*  Returned Value:
*     (char * ) = NBS_MLIST_FIND
*        Start address of mapped global section

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     05-Feb-1993 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     C RTL:
*        MALLOC        Allocate dynamic memory

*  Prior Requirements:
*     None

*-
*/
char * NBS_MLIST_FIND ( int memid, int *status )
{

/* Local variable declarations	*/

  int              found = NO;
  char             *addr = (char *)-1;
  mapping_id       mapping = NIL;

if ( nbs_gl_mlist )
  {
  mapping = nbs_gl_mlist;
  while ( mapping && ! found )
    {
    if ( mapping->memid == memid )
      found = YES;
    else
      mapping = mapping->next;
    }
  }

if ( found )
  {
  mapping->acount++;
  addr = mapping->addr;
  }

return addr;
}

/*
*+
*  Name:
*     NBS_MLIST_UNMAP

*  Purpose:
*     Unmaps a global section mapped by this process given the address of
*     that mapping.

*  Language:
*     ANSI C

*  Invocation:
*     (void) = NBS_MLIST_UNMAP(ADDR,STATUS)

*  Description:
*     The list of sections mapped by this process is searched for the
*     required address. If not found, NBS__SECTIONNOTFOUND is returned,
*     otherwise the reference count is decreased by one. If the
*     reference count drops to zero, the section is unmapped. If the
*     destroy field of the map is set, the section is also destroyed.

*  Arguments:
*     char *ADDR (Given)
*        Address at which global section is mapped
*     int *STATUS (Given and returned)
*        Global status. Possible codes are,

*  Returned Value:
*     (Void)

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     05-Feb-1993 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     UNIX RTL:
*        SHMDT         Unmap global section from process address space
*        SHMCTL        Shared memory control operations
*     C RTL:
*        FREE          Release dynamic memory

*  Prior Requirements:
*     None

*-
*/
void NBS_MLIST_UNMAP ( char *addr, int *status )
{

/* Local variable declarations	*/

  int               found = NO;
  mapping_id        freemap;
  mapping_id        *mapping = &nbs_gl_mlist;

/* Locate the mapped section in the list of sections mapped by this process */

  while ( *mapping && !found )
    {
    if ( (*mapping)->addr == addr )
      found = YES;
    else
      mapping = &((*mapping)->next);
    }

/* Present in the list? */

  if ( found )
    {

/* Only unmap the section if the reference count goes to zero */

    (*mapping)->acount--;
    if ( ! (*mapping)->acount )
      {

/* Detach the section from the process address space */
      shmdt( (*mapping)->addr );

/* Destroy it if this process created it */
      if ( (*mapping)->destroy )
        shmctl( (*mapping)->memid, IPC_RMID, (struct shmid_ds *) 0 );

/* Remove this node from the mapping list */
      freemap = *mapping;
      (*mapping) = (*mapping)->next;
      free (freemap);
      }
    }
  else
    {
    *status = NBS__SECTIONNOTFOUND;
    emsRep( "NBS_MLIST_UNMAP_SECNOTFOUND",
               "No global section mapped at this address", status );
    }
}
#endif

/*
*+
*  Name:
*     NBS_SLEEPMS

*  Purpose:
*     Go to sleep for a given number of milliseconds.

*  Language:
*     ANSI C

*  Invocation:
*     (void) = NBS_SLEEPMS( msecs )

*  Description:
*     This function causes the program to go to sleep for a given number of
*     milliseconds. This function is needed to provide sub-second sleep
*     intervals as the standard C function, sleep, has a resolution of 1 sec.

*  Arguments:
*     msecs = int (Given)
*        The number of milliseconds to sleep for.

*  Returned Value:
*     (Void)

*  Notes:
*     -  If a negative time interval is given, the routine returns immediately.
*     -  The resolution of the timer on the Sun is 10ms.
*     -  The resolution of the timer on the DECstation is 3.906ms.
*     -  All actual sleep times will be in units of the timer resolution and
*        will be rounded up. E.g. a request to sleep for 15ms on a Sun will
*        result in an actual sleep time of 20ms.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     18-Jun-1992 (PMA):
*        Original version.
*     05-May-1993 (DJA):
*        Renamed to NBS_SLEEPMS. LIB$WAIT call used to provide VMS
*        functionality.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External Routines Used:
*     UNIX RTL:
*        SELECT              Connect to file descriptors, with timeout.
*     VMS RTL:
*        LIB$WAIT            Wait a specified number of milliseconds.

*  Prior Requirements:
*     None

*-
*/

#if HAVE_SYS_TIME_H
#  include <sys/time.h>
#endif

#if HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif

void NBS_SLEEPMS ( int msecs )
{
#ifdef vax
/* Local Variables:							    */

   static float wait_secs;      /* Interval to eait in seconds */

   wait_secs = ((float) msecs);   /* Convert to seconds */

   LIB$WAIT(&wait_secs);          /* And wait */

#else
/* Local Variables:							    */

   struct timeval time_struct;    /* The time structure to give to select.  */
   int ret;                       /* The return code from select.           */

/* Check for a positive time interval.					    */
   if ( msecs > 0 )
   {

/* Set up the time structure, allowing for times longer than 1 second.	    */
      if ( msecs < 1000 )
      {
         time_struct.tv_sec = 0;
         time_struct.tv_usec = msecs * 1000;
      }
      else
      {
         time_struct.tv_sec = msecs / 1000;
         time_struct.tv_usec = ( msecs % 1000 ) * 1000;
      }

/* Call select with null file descriptor sets to give the effect of going   */
/* to sleep for a given time.						    */
      ret = select( 0, (fd_set*)0, (fd_set*)0, (fd_set*)0, &time_struct );
   }
#endif
}
