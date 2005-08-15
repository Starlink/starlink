/*+
* Name:
*    tagstrip

*  Purpose:
*    Get list of tags starting with given name

*  Language:
*     C

*  Invocation:
*     Call from C
*     newtaglist = tagstrip( tagname, taglist )

*  Arguments:
*     tagname = char * (Given)
*        A string
*     taglist = char ** (Given)
*        Array of pointer to string

*  Description:
*     Produces a new list consisting of elements from the given list
*     of tagnames which begin with the given tagname.

*  Pitfalls:
*     [pitfall_description]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  External Routines Used:
*     None

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      29-NOV-1999 (AJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/
#include <stdlib.h>
#include <string.h>

char ** tagstrip( char *tagname, char **taglist ) {

char *prefix;       /* pointer to 'tagname.'   */
int prefsz;         /* length of 'tagname.'    */
int i;              /* counter                 */
char **tagptr;      /* pointer within taglist  */
char **cmptaglist;  /* pointer to the component taglist */

/* Append . to the tagname */
   prefix = (char *)malloc(strlen(tagname)+2);
   strcpy( prefix, tagname );
   strcat( prefix, "." );
   prefsz = strlen( prefix );

/* Find the number of elements in the new list and allocate the required
** number of pointers to char */
   i = 1;
   for ( tagptr=taglist; *tagptr; tagptr++ )
      if ( !strncmp( *tagptr, prefix, prefsz ) ) i++;
   cmptaglist = (char **)malloc(i*sizeof(char **));

/* Now fill the array of pointers to char */
   i = 0;
   for ( tagptr=taglist; *tagptr; tagptr++ ) {
       if ( !strncmp( *tagptr, prefix, prefsz ) )
           cmptaglist[i++] = *tagptr + prefsz;
   }

/* and terminate with NULL */
   cmptaglist[i] = NULL;

   return cmptaglist;
}
