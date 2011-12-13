/*
*+
*  Name:
*     words.c

*  Purpose:
*     Create a tree-structured noticeboard that records different words found
*     in a file. Then allow enquiry of the existence of specific words.

*  Language:
*     {routine_language}

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
 */

/* Include files							    */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

/* prototypes */
int find_word(int envid,int n,char *word);

/* External variable declarations					    */

static int lsid[128];	/* Static ID's for words starting with given	    */
			/* letters.					    */

static int status;	/* Global status				    */

static FILE *fid;	/* Input file id				    */

int
main(argc,argv)
int	argc;
char	*argv[];
{

/* External function declarations */

void create_tree();
char *get_word();

/* Local variable declarations */

int n;
int envsid,dsid;
int envid;
int dum;
char file[81],word[81];

/* Start of code */

/* Expect two arguments: name of file and number of number of starting	    */
/* letters to group at the top-level.					    */

if (argc < 2)
    {
	printf ("File: ");
	scanf ("%s",file);
    }
else
    strcpy (file,argv[1]);
if ((fid = fopen (file,"r")) == 0)
    {
	printf ("Failed to open %s, will use stdin\n",file);
	fid = stdin;
    }
if (argc < 3)
    {
	printf ("Cluster: ");
	scanf ("%d",&n);
    }
else
    sscanf (argv[2],"%d",&n);

/* Begin noticeboard definition.					    */

nbc_tune ("max_defn_size",500000,&dum,&status);
nbc_begin_definition (&envsid,&status);

/* Create recursive tree structure to contain the word entries.		    */

create_tree (envsid,'A','Z',n);

/* Loop getting words and creating primitive items. 			    */

while (get_word (word))
  nbc_define_primitive(lsid[(int)word[0]],word,"word",0,0,&dsid,&status);

/* End definition, saving to file.					    */

nbc_end_definition ("words","noticeboard_save",&status);

/* Restore and find noticeboard.					    */

nbc_restore_noticeboard ("words","words",&status);
nbc_find_noticeboard ("words",&envid,&status);

/* Loop permitting user-specification of words and looking them up.	    */

if (status == 0)
    printf ("Word: ");
    while (scanf ("%s",word) != EOF)
	{
	    if (find_word (envid,n,word))
		printf ("%s was found.\n",word);
	    else
		printf ("%s was not found.\n",word);
	    printf ("Word: ");
	}

/* That's it. A later version can perhaps re-scan the file and amass word   */
/* counts.								    */

if (status)
    return (status);
}

void create_tree (envsid,first,last,n)

/* Create a recursive tree to hold the word entries.			    */

int envsid,first,last,n;
{

/* Local variable declarations */

int i,j,sid;
char name[16],type[16];

/* Start of code */

/* If zero step, all words will be at same level.			    */

if (n == 0)
    for (i=first;  i<=last;  i++)
	lsid[i] = envsid;

/* Otherwise, they go in batches of n.					    */

else
    for (i=first;  i<=last;  i+=n)
	{
	    j = i + n - 1;
	    if (j > last)
		j = last;
	    if (i == j)
		{
		    sprintf (name,"%c",i);
		    sprintf (type,"char");
		    nbc_define_structure (envsid,name,type,&lsid[i],&status);
		}
	    else
		{
		    sprintf (name,"%c-%c",i,j);
		    sprintf (type,"range");
		    nbc_define_structure (envsid,name,type,&sid,&status);
		    create_tree (sid,i,j,n/2);
		}
	}
}

int find_word (envid,n,word)

/* Create a recursive tree to hold the word entries.			    */

int envid,n;
char *word;
{

/* Local variable declarations */

int i,j,id;
int dstatus;

/* Start of code */

/* Calculate word index, based on its first character.			    */

i = toupper (word[0]) - 'A';

/* Search down to the appropriate level.				    */

while (n != 0)
    {
	j = (i / n) + 1;
	nbc_find_nth_item (envid,j,&id,&status);
	envid = id;
	i %= n;
	n /= 2;
    }

/* Is the word found at this level? */

dstatus = status;
nbc_find_item (envid,word,&id,&dstatus);
return id;
}

char *get_word (word)
char *word;
{

/* Local variable declarations */

int i;
char c = 0;

/* Start of code */

while (!ferror (fid) && !feof (fid) && (c=fgetc (fid)) && !isalpha (c))
    ;
i = 0;
word[i] = toupper (c);
while (!ferror (fid) && !feof (fid) && (c=fgetc (fid)) && isalpha (c))
    word[++i] = toupper (c);
word[++i] = '\0';
if (ferror (fid) || feof (fid))
    return 0;
else
    return word;
}
