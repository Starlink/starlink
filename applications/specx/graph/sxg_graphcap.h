/* sxg_graphcap.h
*  Purpose:
*     To declare the SPECX devices structure
*     C version of the Fortran sxg_graphcap.inc file
*
*  Authors:
*     AJC: Alan Chipperfield (Starlink, RAL)
*
*  History:
*     24-JUL-2000 (AJC):
*        Original, produced for port to Linux
*
*/
#define MAXDEVS 40

static struct gcaps {
   F77_INTEGER_TYPE(dev_no);
   F77_CHARACTER_TYPE prompt[17];
   F77_CHARACTER_TYPE devname[17];
   F77_LOGICAL_TYPE(term);
   F77_LOGICAL_TYPE(dual);
   F77_LOGICAL_TYPE(colour);
   F77_LOGICAL_TYPE(hard);
   F77_CHARACTER_TYPE file[81];
} devices[MAXDEVS];


