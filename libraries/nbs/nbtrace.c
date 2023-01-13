/*
*+
*  Name:
*     nbtrace

*  Purpose:
*     List noticeboard contents

*  Language:
*     {routine_language}

*  Description:
*     List noticeboard contents. A single argument is expected - the name of
*     the noticeboard definition file. For primitive items, the current
*     dimensionality and the first few values are listed.

*  Copyright:
*     Copyright (C) 1986-1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     WFL: William Lupton (RGO)
*     {enter_new_authors_here}

*  History:
*     09-FEB-1986 (WFL):
*        Original.
*     03-Apr-1987 (WFL):
*        - Allow noticeboard name, not just definition file name.
*        - Where TYPE looks like an HDS primitive type, output
*          value in more helpful format.
*        - Allow file-name of up to 80 characters.
*        - Correct bugs to do with calculating significant lengths
*          and outputting tab characters.
*     07-Apr-1987 (WFL):
*        Don't throw away trailing spaces on _CHAR items
*     17-Jul-1987 (WFL):
*        Translate final status
*     22-Jul-1987 (WFL):
*        Output modified flag value as well
*     06-Nov-1987 (WFL):
*        Portable VMS / UNIX version. Assume that
*        the C binding is used for passing character strings.
*     11-Feb-1988 (WFL):
*        Allow TYPE's that look like stamdard C types to be
*          output in more helpful format as well
*     11-Feb-1988 (WFL):
*        Use NBC_ form routines (because use C strings)
*     12-Feb-1988 (WFL):
*        New interface to GET_VALUE and PUT_VALUE
*     16-Feb-1988 (WFL):
*        Use new GET_CHILDREN and GET_INFO routines
*     25-Mar-1988 (WFL):
*        Use new interface to GET_INFO
*     20-May-1988 (WFL):
*        Use new interfaces for input scalars
*     12-May-1989 (WFL):
*        Don't display trailing spaces
*     22-May-1989 (WFL):
*        Give Hex translation for integer types; be intelligent
*          about realising that an INT of size 1 is really a BYTE etc
*     10-Nov-1989 (WFL):
*        Support A.B.C syntax to determine dive-in point
*     13-Nov-1989 (WFL):
*        Support VMS-style wild cards in names
*     05-Feb-1990 (WFL):
*        Support _VARYING type
*     09-Feb-1990 (WFL):
*        Try to find the noticeboard first. Only restore the
*        definition from file if it didn't already exist.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
 */

/*
 * System includes
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/*
 *  NBS error codes
 */

#include "ems.h"
#include "sae_par.h"
#include "nbs_err.h"
#include "nbs_typ.h"
#include "nbs.h"

/*
 * internal prototypes
 */

void parse ( char item[], char file[], char name[], char *path[], int *np);
void list ( int level, char* patt[], int id);
void output ( int level, char * patt, int id, int *done);
int match( char *name, char *type );

void output_type(int id, char type[]);
void output_name(char name[]);
void output_shape(int id);
void output_value(char type[], int id);
void modify_type(char type[], int actbytes, char tmod[]);
int siglen(char string[]);

/*
 *  DELTA_INDENT is the extra indent for each level down the structure
 */

#define DELTA_INDENT 3

/*
 *  Utility expression evaluation
 */

#define MIN(i,j) ((i)<(j) ? (i) : (j))
#define ODD(i) ((i)&1 == 1)

int
main (argc,argv)

/*
 *  The single argument is the name of the noticeboard item, which has the form
 *  "dir#name.item1.item2..." where "#" represents any of "]>:" and "dir#" and
 *  the ".itemn" bits can be omitted. "name" is the noticeboard name and if the
 *  items are given then the structure is traced down to the specified point
 *  before being listed. If the parameter is not given then it is prompted for.
 */

int		argc;
char		*argv[];
{
   char		item[81];
   char		file[81];
   char		name[81];
   char		*path[11];
   int		i;
   int		id;
   int		np;
   int		tid;
   int		defn_size;
   int		section_size;
   int		version;
   int		pid;
   int		modified;
   int          restore_status;
   int	       	find_status;
   int		status=0;
   int          retval=EXIT_SUCCESS;

   emsBegin( &status );

   if (argc < 2) {
      printf ("Item name: ");
      scanf ("%s",item);
   } else
      strcpy (item,argv[1]);

   for (i = 0; i < 11; i++)
      path[i] = 0;
   parse (item,file,name,path+1,&np);

   find_status = 0;
   restore_status = 0;
   nbc_find_noticeboard (name,&id,&find_status);
   if (find_status == 0)
      printf ("Noticeboard is currently mapped by another process\n");
   else {
      nbc_restore_noticeboard (name,file,&restore_status);
      if (restore_status == NBS__DATANOTSAVED) {
         printf ("Noticeboard data not restored because it was not saved\n");
         restore_status = 0;
         }
      if (restore_status == 0) {
         find_status = 0;
         nbc_find_noticeboard (name,&id,&find_status);
         }
      }
   if (find_status != 0) {
      if (restore_status != 0) {
         printf ("Error restoring noticeboard\n");
         exit (restore_status);
         }
      else {
         printf ("Error finding noticeboard\n");
         exit (find_status);
         }
      }
   else {

      status = 0;
      if (path[1] == 0) {
         nbc_get_info (id,"VERSION",&version,&status);
         nbc_get_info (id,"SECTION_SIZE",&section_size,&status);
         nbc_get_info (id,"DEFN_SIZE",&defn_size,&status);
         nbc_get_info (id,"PID",&pid,&status);
         nbc_get_modified (id,&modified,&status);
         printf ("Software version   = %d (%x)\n",version,version);
         printf ("Size of section    = %d (%x)\n",section_size,section_size);
         printf ("Size of definition = %d (%x)\n",defn_size,defn_size);
         printf ("Noticeboard owner  = %d (%x)\n",pid,pid);
         printf ("Modified count     = %d (%x)\n\n",modified,modified);
         }

      status = 0;
      for (i = 1; i <= np; i++) {
         tid = id;
         nbc_find_item (tid,path[i],&id,&status);
         }
      if (status != 0) {
         printf ("Error following path\n");
         exit (status);
         }

      path[np]= 0;
      list (0,path+np,id);
      }

   if ( status == SAI__OK) {
     retval = EXIT_SUCCESS;
   } else {
     retval = EXIT_FAILURE;
   }
   emsEnd( &status );
   return retval;
}

void
parse (item,file,name,path,np)

/*+ PARSE
 *
 *  Parse supplied string, extracting file-name, noticeboard name and
 *  path to starting point. Return number of levels in path that do NOT
 *  contain any wild cards.
 */

char		item[];
char		file[];
char		name[];
char		*path[];
int		*np;
{
   int		i;
   int		dlen;
   int		flen;
   int		plen;
   int		wnum;
   int		pnum;
   char		*pptr;
   char		*fptr;

   dlen = strcspn (item,"]>:");
   if (dlen == strlen (item))
      dlen = 0;
   flen = strcspn (item+dlen,".") + dlen;

   strcpy (file,item);
   file[flen] = '\0';

   if (dlen == 0)
      strcpy (name,file);
   else
      strcpy (name,file+dlen+1);

   wnum = -1;
   pnum = 0;
   pptr = item + flen;
   fptr = file + flen;
   while (*pptr) {
      plen = strcspn (fptr+1,".");
      path[pnum] = fptr+1;
      for (i=0; i < plen; i++)
         path[pnum][i] = toupper (path[pnum][i]);
      path[pnum][plen] = '\0';
      if (wnum == -1 && strcspn (path[pnum],"%*") < plen)
         wnum = pnum;
      pnum += 1;
      pptr += 1 + plen;
      fptr += 1 + plen;
      }
   path[pnum] = 0;

   if (wnum != -1)
      *np = wnum;
   else
      *np = pnum;
}

void
list (level,patt,id)

/*+ LIST
 *
 *  Output details of noticeboard subtree starting at item pointed to by ID
 */

int		level;
char		*patt[];
int		id;
{
   int		done;
   int		item;
   int		son_id;
   int		status = 0;

   output (level,*patt,id,&done);

   if (done)
     {
     nbc_find_nth_item (id,1,&son_id,&status);

     if ( status == NBS__PRIMITIVE )
       emsAnnul( &status );
     else
       {
       for (item = 1; status == 0;
           item++, nbc_find_nth_item (id,item,&son_id,&status)) {
         level++;
         patt++;
         list (level,patt,son_id);
         patt--;
         level--;
         }
       if ( status == NBS__ITEMNOTFOUND )
         emsAnnul( &status );
       }
     }
}

void
output (level,patt,id,done)

/*+ OUTPUT
 *
 *  Output a line to STDOUT, summarising the noticeboard entry for the item
 *  pointed to by ID. If pattern supplied, only output for items whose names
 *  match it

 */

int		level;
char		*patt;
int		id;
int		*done;
{
   int		i;
   int		status;
   char		name[17];
   char		type[17];

   status = 0;
   nbc_get_name (id,name,&status);
   if (status != 0)
      strcpy (name,"?");
   *done = match (name,patt);

   if (*done) {
      for (i = 0; i < level * DELTA_INDENT; i++)
         printf (" ");
      output_type (id,type);
      output_name (name);
      output_shape (id);
      output_value (type,id);
      printf ("\n");
      }
}

int
match (word,patt)

/*+ MATCH
 *
 *  Determine whether a word matches a pattern containing VMS-style wild-card
 *  characters
 */

char		*word;
char		*patt;
{
   int		match;
   int		perc;
   int		star;

   match = 1;
   if (patt != 0) {
      while (*word && *patt && match) {
         for (perc = 0, star = 0;
              *patt == '%' || *patt == '*';
              patt++) {
            perc += *patt == '%';
            star += *patt == '*';
            }
         if (perc || star) {
            while (*word && perc) {
               word++;
               perc--;
               }
            if (perc)
               match = 0;
            else if (star)
               while (*word && (*word != *patt))
                  word++;
            }
         else if (*word++ != *patt++)
            match = 0;
         }
      while (*patt == '*')
         patt++;
      if (*word || *patt)
         match = 0;
      }
   return match;
}

void
output_type (id,type)

/*+ OUTPUT_TYPE
 *
 *  Output type of item pointed to by ID, followed by a tab
 */

int		id;
char		type[];
{
   int		status;

   status = 0;
   nbc_get_type (id,type,&status);
   if (status != 0)
      strcpy (type,"?");
   printf ("%s\t",type);
}

void
output_name (name)

/*+ OUTPUT_NAME
 *
 *  Output name of item
 */

char		name[];
{
   printf ("%s",name);
}

void
output_shape (id)

/*+ OUTPUT_SHAPE
 *
 *  If item pointed to by ID is primitive, output a string of the form
 *  "[(actdims) dim1,dim2,...,dimn]" (if there are ridiculously many dimensions
 *  then not all will be output). If item is structured output string of the
 *  form "(children)". Then in all cases output a tab
 */

int		id;

#define MAXDIMS 10

{
   int		actdims;
   int	     	i;
   int		maxdims;
   int		primitive;
   int		shape[MAXDIMS];
   int          children;
   int		status;

   status = 0;
   nbc_get_primitive (id,&primitive,&status);
   if (primitive) {
      maxdims = MAXDIMS;
      nbc_get_shape (id,&maxdims,shape,&actdims,&status);
      if (actdims > 0 && status == 0) {
	 printf ("[(%d) ",actdims);
	 for (i = 0; i < MIN (MAXDIMS,actdims); i++) {
	    printf ("%d",shape[i]);
	    if (i < MIN (MAXDIMS,actdims) - 1)
	       printf (",");
	 }
	 if (actdims > MAXDIMS)
	    printf (",...");
	 printf ("]");
      }
   }
   else {
      nbc_get_children (id,&children,&status);
      printf (" (%d)",children);
   }
   printf ("\t");

   if (status != 0)
      printf ("Failed to get item shape / number of children\n");
}

void
output_value (type,id)

/*+ OUTPUT_VALUE
 *
 *  If item pointed to by ID is primitive, output the first few values in
 *  the form "(actbytes/nbytes/modified) val1,val2,...,valn" (if there are too
 *  many values then not all will be output).
 *
 *  Where types look like the HDS standard primitive types output the values
 *  in appropriate format.
 */

char		type[];
int		id;

#define MAXBYTES 132

{
   int		actbytes;
   int		modified;
   int		i;
   int		j;
   int		nbytes;
   int		primitive;
   int		status;
   char		tmod[17];
   union	{
		int		    i[(MAXBYTES+3)/4];
		float		    r[(MAXBYTES+3)/4];
		double		    d[(MAXBYTES+7)/8];
		int		    l[(MAXBYTES+3)/4];
		char                c[MAXBYTES+1];
		struct		    {
				    unsigned short l;
				    char c[MAXBYTES-1];
				    } v;
		char		    b[MAXBYTES];
		unsigned char	    ub[MAXBYTES];
		short		    s[(MAXBYTES+1)/2];
		unsigned short	    us[(MAXBYTES+1)/2];
		} val;

   int		maxbytes = MAXBYTES;

   status = 0;
   val.c[maxbytes] = '\0';
   nbc_get_primitive (id,&primitive,&status);
   if (primitive) {
      nbc_get_size (id,&nbytes,&actbytes,&status);
      nbc_get_modified (id,&modified,&status);
      nbc_get_value (id,0,maxbytes,&val,&actbytes,&status);
      modify_type (type,actbytes,tmod);
      if (status == 0) {
	 printf ("(%d/%d/%d) ",actbytes,nbytes,modified);
	 if (actbytes > 0) {
	    for (i = 0, j = 0; i < MIN (maxbytes,actbytes); j++) {
	       if (strncmp(tmod, "_INTEGER", 2) == 0) {
		  printf ("%d (%x)",val.i[j],val.i[j]);
		  i += 4;
		  }
	       else if (strncmp(tmod, "_REAL", 2) == 0) {
		  printf ("%g",val.r[j]);
		  i += 4;
		  }
	       else if (strncmp(tmod, "_DOUBLE", 2) == 0) {
		  printf ("%g",val.d[j]);
		  i += 8;
		  }
	       else if (strncmp(tmod, "_LOGICAL", 2) == 0) {
		  if (val.l[j] & 1) printf ("TRUE"); else printf ("FALSE");
		  i += 4;
		  }
	       else if (strncmp(tmod, "_CHAR", 2) == 0) {
		  val.c[MIN (maxbytes,actbytes)] = '\0';
		  val.c[siglen (val.c)] = '\0';
		  printf ("%c%s%c",'"',val.c,'"');
		  i += MIN (maxbytes,actbytes);
		  }
	       else if (strncmp(tmod, "_VARIANT", 2) == 0) {
		  val.v.c[MIN (maxbytes-2,actbytes-2)] = '\0';
		  val.v.c[MIN (maxbytes-2,val.v.l)] = '\0';
		  printf ("%c%s%c",'"',val.v.c,'"');
		  i += MIN (maxbytes,actbytes);
		  }
	       else if (strncmp(tmod, "_BYTE", 2) == 0) {
		  printf ("%d (%x)",val.b[j],val.b[j]);
		  i += 1;
		  }
	       else if (strncmp(tmod, "_UBYTE", 3) == 0) {
		  printf ("%u (%x)",val.ub[j],val.ub[j]);
		  i += 1;
		  }
	       else if (strncmp(tmod, "_WORD", 2) == 0) {
		  printf ("%d (%x)",val.s[j],val.s[j]);
		  i += 2;
		  }
	       else if (strncmp(tmod, "_UWORD", 3) == 0) {
	          printf ("%u (%x)",val.us[j],val.us[j]);
		  i += 2;
		  }
               else {
		  printf ("%d (%x)",val.b[j],val.b[j]);
		  i += 1;
		  }
	       if (i < MIN (maxbytes,actbytes))
		  printf (",");
	    }
	    if (actbytes > maxbytes)
	       printf (",...");
	 }
      }
   }

   if (status != 0)
      printf ("Failed to get item value\n");
}

void
modify_type (type,actbytes,tmod)

/*+ MODIFY_TYPE
 *
 *  Modify type for the purposes of doing the best possible job of representing
 *  a value. Convert Figaro-like types to HDS types and adjust the precision of
 *  integer types if the current number of bytes is not a multiple of the
 *  number of bytes in the apparent type (eg, if type is _INTEGER but number of
 *  bytes is 1, change _INTEGER to _BYTE).
 */

char	type [];
int	actbytes;
char	tmod[];
{
   if (strncmp(type, "LONG", 4) == 0 ||
       strncmp(type, "INT",3) == 0)
      if (actbytes % 4 == 0)
	 strcpy (tmod,"_INTEGER");
      else if (actbytes % 2 == 0)
	 strcpy (tmod,"_WORD");
      else
	 strcpy (tmod,"_BYTE");
   else if (strncmp(type, "FLOAT", 5) == 0)
      strcpy (tmod,"_REAL");
   else if (strncmp(type, "DOUBLE", 6) == 0)
      strcpy (tmod,"_DOUBLE");
   else if (strncmp(type, "BOOL", 4) == 0)
      strcpy (tmod,"_LOGICAL");
   else if (strncmp(type, "CHAR", 4) == 0)
      strcpy (tmod,"_CHAR");
   else if (strncmp(type, "UNSIGNEDCHAR", 12) == 0)
      strcpy (tmod,"_UBYTE");
   else if (strncmp(type, "SHORTINT", 8) == 0 ||
 	    strncmp(type, "SHORT", 5) == 0)
      if (actbytes % 2 == 0)
	 strcpy (tmod,"_WORD");
      else
	 strcpy (tmod,"_BYTE");
   else if (strncmp(type, "UNSIGNEDSHORTINT", 16) == 0 ||
	    strncmp(type, "UNSIGNEDSHORT", 13) == 0)
      if (actbytes % 2 == 0)
	 strcpy (tmod,"_UWORD");
      else
	 strcpy (tmod,"_UBYTE");
   else
      strcpy (tmod,type);
}

int siglen (string)

/*+ SIGLEN
 *
 *  Calculate and return the significant length of a string
 */

char	string[];
{
   int	i;

   for (i = strlen (string); i > 0 && string[i-1] == ' '; i--)
      ;
   return (i);
}
