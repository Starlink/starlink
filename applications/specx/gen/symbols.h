/* symbols.h
*  Purpose:
*     To define the SPECX symbol table
*
*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*
*  History:
*      7-JUN-2000 (AJC):
*        Original version
*
*-
*/
#define MAX_TABLE 512
#define MAX_HASH 503

static struct symbol {
   char name[17];
   char type[5];
   int length;
   void *address;
} table[MAX_TABLE];

static struct symtab {
   char name[17];
   int value;
} hashtab[503];
