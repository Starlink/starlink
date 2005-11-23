/*+
* getstringarray
*
* Purpose:
*    Produces an array of pointer to strings from an array of IDL strings
*
* Invovation:
*    Call from C
*      carr = getstringarray( ndims, dims, idlarray )
*
* Arguments:
*    ndims = int
*       The number of dimensions in the array;
*    dims = int[]
*       The array of dimensions
*    idlarray = IDL_STRING *
*       pointer to the array of IDL strings
*
* Returned value:
*    carr = **char
*       Pointer to an array of pointers to char
*
* Description:
*    Space is obtained for both the array of pointers and a copy of the
*    strings. The strings are copied from the IDL array to the obtained
*    space and pointers to eache string placed in the array of pointers.
*    The space obtained should be returned using the retstringarray function
*    when no longer required.
*    The array of pointers is terminated by NULL.
*-
*/
#include <stdlib.h>
#include <stdio.h>
#include "export.h"

#define TAG__SZNAM DAT__SZNAM+9

char **getstringarray( int ndims, int *dims, IDL_STRING *data ) {
int i;      /* Counter */
int nels;   /* Number of elements */
int size;   /* Number of char required for strings */
char **arr; /* Assembly area for array */
IDL_STRING *pdata;
char **p;   /* Pointer to next pointer to char in array */
char *pstr; /* Pointer to next string in C array */

/* Obtain the memory needed for the array of pointers */
/* Allow for the null terminator */
   for (i=0,nels=1;i<ndims;i++) nels*=dims[i];
   arr = (char **)malloc((nels+1)*sizeof(char *));
/* Obtain the memory needed for the actual strings */
   pdata = data;
   for (i=0,size=0;i<nels;i++) size+=pdata++->slen+1;
   pstr = (char *)malloc( size );

/* Now copy the IDL data for each element in turn */
   p = arr;
   for (i=0;i<nels;i++) {
      *p++ = pstr;
      strcpy( pstr, IDL_STRING_STR(data));
      pstr+=strlen(pstr)+1;
      data+=1;
   }
   *p = NULL;
   return arr;
}
/*+
* retstringarray
*
* Purpose:
*    To return space allocated by getstringarray.
*
* Invovcation:
*    Call from C
*       retstringarray( carr );
*
* Arguments:
*    carr = **char
*       A pointer to an array of pointers to char
*
* Description:
*    Frees the memory pointed at by the first element of the given array,
*    carr, then frees the array itself.
*-
*/
void retstringarray( char **carr ) {

   free( *carr );
   free( carr );
   return;
}
