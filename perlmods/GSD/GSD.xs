/*        -*- C -*-
  
  perl-GSD glue - 95% complete
                                        timj@jach.hawaii.edu
 					Time-stamp: <13 Jan 96 1322 timj>

  Missing calls:   gsdGet0w()
                   gsdGet1w()
		   gsdGet1l()
		   gsdGet1b()

		   since I wasn't able to test any of these on real data

*/
#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */
#ifdef __cplusplus
}
#endif
 
/* The array handling code can be included here */
 
/* Deal with the packing of perl arrays to C pointers */
 
#include "arrays/arrays.h"
 
/* comment this out if you are linking with libarrays.a separately */
#include "arrays/arrays.c"

/* These are the GSD library header */
#include "gsd/gsd.h"
#include "gsd/gsd1.h"



/* GSD constants */
#define MAXDIMS 5
#define INQNMLEN 16
#define INQUNLEN 11


/* File descriptors */
typedef struct file_descriptor file_descriptor;
typedef struct item_descriptor item_descriptor;


static char labelbuff[41]; /* Generic label holder */
static char unitbuff[11];
static char nmbuff[16];
static char cdatvalbuff[17];

static int i;
static int first, last;

static char  *dimnm[MAXDIMS];        /* Pointer array for dimnames */
static char  *dimunt[MAXDIMS];        /* Pointer array for dimunits */

static void *pvals;       /* Pointer to arrays */


/* My code */

/* gsdinqsize buffers */


/* Translation of the old gsd_sys_location. This is the code from gsdGet1x.c
*/
 
static int nindex( int no_dims, int *bounds, int *subscripts )
{
  int cell_count, i, j;
  int index;
 
  index = subscripts[0];
  for ( i = no_dims; i >= 2; i-- )
    {  cell_count = subscripts[i-1] - 1;
    if ( cell_count > 0 )
      {  for ( j = 1; j <= i-1; j++ )
	cell_count *= bounds[j-1];
      index += cell_count;
      }
    }
  return index;
}


MODULE = GSD     PACKAGE = GSD

int
gsdOpenRead(filename, version, label, no_items, fptr, file_dsc, item_dsc, data_ptr)
  char *	filename 
  float		version = NO_INIT
  char *	label = NO_INIT
  int		no_items = NO_INIT
  FILE *	fptr = NO_INIT
  file_descriptor *	file_dsc = NO_INIT
  item_descriptor *	item_dsc = NO_INIT
  I32	data_ptr = NO_INIT
  PROTOTYPE: $$$$$$$$
  CODE:
    label = labelbuff; /* Point to static space */
    RETVAL = gsdOpenRead(filename, &version, label, &no_items, &fptr, 
             (void **) &file_dsc, (void **) &item_dsc, (void*) &data_ptr);
  OUTPUT:
  version
  label
  no_items
  fptr
  file_dsc
  item_dsc
  data_ptr  
  RETVAL

int
gsdClose(fptr, file_dsc, item_dsc, data_ptr)
  FILE *	fptr
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  I32	data_ptr 
  PROTOTYPE: $$$$
  CODE:
    RETVAL = gsdClose(fptr, file_dsc, item_dsc, (void*) data_ptr );
  OUTPUT:
  RETVAL

int
gsdFind(file_dsc, item_dsc, name, itemno, unit, type, array)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc 
  char *		name
  int          		itemno = NO_INIT
  char *		unit = NO_INIT
  char 		type = NO_INIT
  char 		array = NO_INIT
  PROTOTYPE: $$$$$$$
  CODE:
    unit = unitbuff;
    RETVAL = gsdFind( file_dsc, item_dsc, name, &itemno, unit, &type, &array );
    if ( array ) {
      array = '1';
      } else {
      array = '0';
      }
  OUTPUT:
  itemno
  unit
  type
  array
  RETVAL

int
gsdItem(file_dsc, item_dsc, itemno, name, unit, type, array)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc
  int                   itemno
  char *                name = NO_INIT
  char *                unit = NO_INIT
  char                 type = NO_INIT
  char                 array = NO_INIT
  PROTOTYPE: $$$$$$$
  CODE:
    name = nmbuff;
    unit = unitbuff;
    RETVAL = gsdItem( file_dsc, item_dsc, itemno, name, unit, &type, &array );
    if ( array ) {
      array = '1';
      } else {
      array = '0';
      }
  OUTPUT:
  name
  unit
  type
  array
  RETVAL

int
gsdGet0d(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  I32	data_ptr
  int           itemno
  double 	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0d( file_dsc, item_dsc, (void*) data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0i(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int           itemno
  int 		dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0i( file_dsc, item_dsc, (void*) data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0l(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  I32	data_ptr
  int           itemno
  char  	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0l( file_dsc, item_dsc, (void*) data_ptr, itemno, &dat_val);
    if ( dat_val ) {
      dat_val = '1';
      } else {
      dat_val = '0';
      }
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0r(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  I32	data_ptr
  int           itemno
  float 	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0r( file_dsc, item_dsc, (void*) data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0b(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  I32	data_ptr
  int           itemno
  char  	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0b( file_dsc, item_dsc, (void*) data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0w(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  I32	data_ptr
  int           itemno
  short 	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0w( file_dsc, item_dsc, (void*) data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL


int
gsdGet0c(file_dsc, item_dsc, data_ptr, itemno, cdat_val)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int           itemno
  char *	cdat_val = NO_INIT
 CODE:  
  cdat_val = cdatvalbuff;
  RETVAL = gsdGet0c( file_dsc, item_dsc, (void*) data_ptr, itemno, cdat_val);
 OUTPUT:
 cdat_val
 RETVAL
  
int
gsdInqSize(file_dsc, item_dsc, data_ptr, itemno,  dimnames, dimunits, dimvals, actdims, size)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int itemno
  char *	dimnames = NO_INIT
  char *	dimunits = NO_INIT
  int * 	dimvals  = NO_INIT
  int  	actdims  = NO_INIT
  int  	size     = NO_INIT
 CODE:
  dimvals = get_mortalspace(MAXDIMS,'i');
  dimnames = malloc(INQNMLEN*MAXDIMS);
  dimunits = malloc(INQUNLEN*MAXDIMS);

  for ( i = 0; i < MAXDIMS; i++ )
  {  
    dimnm[i] = dimnames + (i*INQNMLEN); dimunt[i] = dimunits + (i*INQUNLEN);
  }
  RETVAL = gsdInqSize(file_dsc, item_dsc, (void*) data_ptr, itemno, MAXDIMS, 
           (char **) dimnm, (char **) dimunt, dimvals, &actdims, &size);
  unpack1D( (SV*)ST(6), (void *)dimvals, 'i', actdims);

  for (i=0; i<actdims; i++) {
    av_store( (AV*) SvRV(ST(4)),i,newSVpv(dimnm[i],INQNMLEN) );
    av_store( (AV*) SvRV(ST(5)),i,newSVpv(dimunt[i],INQUNLEN) );
  }
  free(dimnames);
  free(dimunits);

 OUTPUT:
 dimvals
 actdims
 size
 RETVAL


int
gsdGet1d(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, values, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  double	* values = NO_INIT
  int actvals = NO_INIT
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  values = get_mortalspace(last  - first + 1,'d');
  RETVAL = gsdGet1d(file_dsc, item_dsc, (void*) data_ptr, itemno, ndims,
		    dimvals, start, end, values, &actvals); 
  unpack1D( (SV*)ST(8), (void *)values, 'd', actvals);
 OUTPUT:
 values
 actvals
 RETVAL

int
gsdGet1i(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, values, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  int	* values = NO_INIT
  int actvals = NO_INIT
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  values = get_mortalspace(last  - first + 1,'i');
  RETVAL = gsdGet1i(file_dsc, item_dsc, (void*) data_ptr, itemno, ndims,
		    dimvals, start, end, values, &actvals); 
  unpack1D( (SV*)ST(8), (void *)values, 'i', actvals);
 OUTPUT:
 values
 actvals
 RETVAL

int
gsdGet1r(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, values, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  float	* values = NO_INIT
  int actvals = NO_INIT
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  values = get_mortalspace(last  - first + 1,'f');
  RETVAL = gsdGet1r(file_dsc, item_dsc, (void*) data_ptr, itemno, ndims,
		    dimvals, start, end, values, &actvals); 
  unpack1D( (SV*)ST(8), (void *)values, 'f', actvals);
 OUTPUT:
 values
 actvals
 RETVAL

int
gsdGet1c(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, values, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  char	* values = NO_INIT
  int actvals = NO_INIT
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  values = malloc(16*(last-first+1));
  RETVAL = gsdGet1c(file_dsc, item_dsc, (void*) data_ptr, itemno, ndims,
		    dimvals, start, end, values, &actvals); 
  for (i=0; i<actvals; i++) {
    av_store( (AV*) SvRV(ST(8)),i,newSVpv(values+i*16,16) );
  }
  free(values);
 OUTPUT:
 actvals
 RETVAL



# need pointer versions that return packed string
#   so that I can manipulate memory for speed


int
gsdGet1dp(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, packed, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  char	* packed = NO_INIT
  int actvals = NO_INIT
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  pvals = get_mortalspace(last  - first + 1,'d');
  RETVAL = gsdGet1d(file_dsc, item_dsc, (void*) data_ptr, itemno, ndims,
		    dimvals, start, end, (double *) pvals, &actvals); 
  sv_setpvn((SV*)ST(8), (char *)pvals, sizeof(double)*actvals);
 OUTPUT:
 actvals
 RETVAL

int
gsdGet1ip(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, packed, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  char	* packed = NO_INIT
  int actvals = NO_INIT
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  pvals = get_mortalspace(last  - first + 1,'i');
  RETVAL = gsdGet1i(file_dsc, item_dsc, (void*) data_ptr, itemno, ndims,
		    dimvals, start, end, (int *) pvals, &actvals); 
  sv_setpvn((SV*)ST(8), (char *)pvals, sizeof(int)*actvals);
 OUTPUT:
 actvals
 RETVAL

int
gsdGet1rp(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, packed, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  I32        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  char	* packed = NO_INIT
  int actvals = NO_INIT
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  pvals = get_mortalspace(last  - first + 1,'f');
  RETVAL = gsdGet1r(file_dsc, item_dsc, (void*) data_ptr, itemno, ndims,
		    dimvals, start, end, (float *) pvals, &actvals); 
  sv_setpvn((SV*)ST(8), (char *)pvals, sizeof(float)*actvals);
 OUTPUT:
 actvals
 RETVAL


