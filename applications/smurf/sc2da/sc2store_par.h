/* sc2store_par.h - constants for scuba2 storage

   History :
    13Aug2004 : original (bdk)
    17Feb2005 : add SC2_HEAT (bdk)
    02Oct2005 : add SC2STORE_FLATLEN (bdk)
    02Feb2006 : use an enum for the indices (bdk)
    09Feb2006 : make SC2STORE_NUM last item in the enum (bdk)
    13Mar2006 : add SC2STORE__MAXFITS to replace FHEAD__MXREC (agg)
    14Mar2006 : protect against multiple includes (agg)
    26Jul2006 : remove parameters that are now defined in jcmt/state.h (timj)
*/

/* Check if these are defined already... */
#ifndef SC2STORE_PAR_DEFINED
#define SC2STORE_PAR_DEFINED

#define SC2STORE_FLATLEN 17  /* maximum length of flatfield algorithm name */
#define SC2STORE_UNITLEN 20  /* maximum length of data units string */
#define SC2STORE_LABLEN  40  /* maximum length of data label string */

#define SC2STORE__MAXFITS 256 /* Maximum number of FITS records */

#define SC2STORE__COL_INDEX 0    /* Index in data array for columns */
#define SC2STORE__ROW_INDEX 1    /* Index in data array for rows */
#define SC2STORE__BOL_LBND  0    /* Lower bounds of bolometer data arrays */

/* The upper bound has to be such that
 *    SIZEDIM = BOL_UBND - BOL_LBND + 1
 * so
 *    BOL_UBND = SIZEDIM + BOL_LBND - 1
 */

#endif /* SC2STORE_PAR_DEFINED */
