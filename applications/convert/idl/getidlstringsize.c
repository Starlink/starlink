/* getidlstringsize
*
* returns the maximum (or only) string size used in an IDL string variable.
*
*/

#include <stdio.h>
#include "export.h"

int getidlstringsize( IDL_VPTR var, IDL_STRING *data ) {

int maxlen;              /* maximum string length so far */
int i;                   /* counter */
   if ( var->flags & IDL_V_ARR ) {
      for (maxlen=0,i=0;i < var->value.arr->n_elts;i++) {
         if ( ( data + i)->slen > maxlen ) {
            maxlen = (data + i)->slen;
         }
      }
   } else {
      maxlen = (int)data->slen;
   }
   return maxlen?maxlen:80;
}
