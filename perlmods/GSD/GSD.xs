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

/* These are the GSD library functions */
#include "gsd.h"




/* GSD constants */
#define MAXDIMS 5
#define INQNMLEN 16
#define INQUNLEN 11



/* File descriptors */
/* These are defined as void since the GSD module allocates all the
   memory and stores a pointer to the struct in this pointer */
typedef void file_descriptor;
typedef void item_descriptor;
typedef FILE GSDFILE;

/* Somewhere to store the data */
typedef char gsd_data;

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
  GSDFILE *	fptr = NO_INIT
  file_descriptor *	file_dsc = NO_INIT
  item_descriptor *	item_dsc = NO_INIT
  gsd_data *	data_ptr = NO_INIT
  PREINIT:
   char labelbuff[41];
  PROTOTYPE: $$$$$$$$
  CODE:
    label = labelbuff; /* Point to some memory */
    RETVAL = gsdOpenRead(filename, &version, label, &no_items, &fptr, 
             (void **) &file_dsc, (void **) &item_dsc, (char**) &data_ptr);
  OUTPUT:
   version
   label
   no_items
   fptr
   file_dsc
   item_dsc
   data_ptr  
   RETVAL

# Internal routine since we need to close the file from the
# perl level before running this

int
gsdClose(fptr, file_dsc, item_dsc, data_ptr)
  GSDFILE *	fptr
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  gsd_data *	data_ptr 
  PROTOTYPE: $$$$
  CODE:
    RETVAL = gsdClose(fptr, file_dsc, item_dsc, data_ptr );
    fptr = NULL;
    file_dsc = NULL;
    item_dsc = NULL;
    data_ptr = NULL;
  OUTPUT:
   RETVAL
   fptr
   file_dsc
   item_dsc
   data_ptr

int
gsdFind(file_dsc, item_dsc, name, itemno, unit, type, array)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc 
  char *		name
  int          		itemno = NO_INIT
  char *		unit = NO_INIT
  char 		type = NO_INIT
  char 		array = NO_INIT
  PREINIT:
  char unitbuff[INQUNLEN]; 
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
  PREINIT:
    char unitbuff[INQUNLEN];
    char nmbuff[INQNMLEN];
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
  gsd_data *	data_ptr
  int           itemno
  double 	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0d( file_dsc, item_dsc, data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0i(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int           itemno
  int 		dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0i( file_dsc, item_dsc, data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0l(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  gsd_data *	data_ptr
  int           itemno
  char  	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0l( file_dsc, item_dsc, data_ptr, itemno, &dat_val);
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
  gsd_data *	data_ptr
  int           itemno
  float 	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0r( file_dsc, item_dsc, data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0b(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  gsd_data *	data_ptr
  int           itemno
  char  	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0b( file_dsc, item_dsc, data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL

int
gsdGet0w(file_dsc, item_dsc, data_ptr, itemno, dat_val)
  file_descriptor *	file_dsc
  item_descriptor *	item_dsc
  gsd_data *	data_ptr
  int           itemno
  short 	dat_val = NO_INIT
 CODE:
  RETVAL = gsdGet0w( file_dsc, item_dsc, data_ptr, itemno, &dat_val);
 OUTPUT:
 dat_val
 RETVAL


int
gsdGet0c(file_dsc, item_dsc, data_ptr, itemno, cdat_val)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *       data_ptr
  int           itemno
  char *	cdat_val = NO_INIT
 PREINIT:
  char cdatvalbuff[17];
 CODE:
  cdat_val = cdatvalbuff;
  RETVAL = gsdGet0c( file_dsc, item_dsc, data_ptr, itemno, cdat_val);
 OUTPUT:
 cdat_val
 RETVAL
  
int
gsdInqSize(file_dsc, item_dsc, data_ptr, itemno,  dimnames, dimunits, dimvals, actdims, size)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int itemno
  SV *	dimnames = NO_INIT
  SV *	dimunits = NO_INIT
  int * 	dimvals  = NO_INIT
  int  	actdims  = NO_INIT
  int  	size     = NO_INIT
 PREINIT:
  int i;
  char actdimnm[MAXDIMS][INQNMLEN];
  char actdimunt[MAXDIMS][INQNMLEN];
  char *ptrdimnm[MAXDIMS];
  char *ptrdimunt[MAXDIMS];
  int dimvalsmem[MAXDIMS];
 CODE:
		/* dimvals = get_mortalspace(MAXDIMS,'i'); */
		dimvals = dimvalsmem;

  /* copy the pointers to the start of each char to the array of pointers */
  /* This is from the notes in the gsdInqSize documenation */
  for ( i = 0; i < MAXDIMS; i++ ) {
    ptrdimnm[i] = actdimnm[i];
    ptrdimunt[i] = actdimunt[i];
  }
  /* Now actually run the command */
  RETVAL = gsdInqSize(file_dsc, item_dsc, data_ptr, itemno, MAXDIMS, 
           (char **) ptrdimnm, (char **) ptrdimunt, dimvals, &actdims, &size);
  
  /* unpack the integer array */ 
  unpack1D( (SV*)ST(6), (void *)dimvals, 'i', actdims);

  /* Copy the dim names and units to the relevant arrays */ 
  for (i=0; i<actdims; i++) {
    av_store( (AV*) SvRV(ST(4)),i,newSVpv(ptrdimnm[i],0) );
    av_store( (AV*) SvRV(ST(5)),i,newSVpv(ptrdimunt[i],0) );
  }
 OUTPUT:
  actdims
  size
  RETVAL


int
gsdGet1d(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, values, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  double	* values = NO_INIT
  int actvals = NO_INIT
 PREINIT:
  int first;
  int last;
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  values = get_mortalspace(last  - first + 1,'d');
  RETVAL = gsdGet1d(file_dsc, item_dsc, data_ptr, itemno, ndims,
		    dimvals, start, end, values, &actvals); 
  unpack1D( (SV*)ST(8), (void *)values, 'd', actvals);
 OUTPUT:
 actvals
 RETVAL

int
gsdGet1i(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, values, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  int	* values = NO_INIT
  int actvals = NO_INIT
 PREINIT:
  int first;
  int last;
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  values = get_mortalspace(last  - first + 1,'i');
  RETVAL = gsdGet1i(file_dsc, item_dsc, data_ptr, itemno, ndims,
		    dimvals, start, end, values, &actvals); 
  unpack1D( (SV*)ST(8), (void *)values, 'i', actvals);
 OUTPUT:
  actvals
  RETVAL

int
gsdGet1r(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, values, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  float	* values = NO_INIT
  int actvals = NO_INIT
 PREINIT:
  int first;
  int last;
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  values = get_mortalspace(last  - first + 1,'f');
  RETVAL = gsdGet1r(file_dsc, item_dsc, data_ptr, itemno, ndims,
		    dimvals, start, end, values, &actvals); 
  unpack1D( (SV*)ST(8), (void *)values, 'f', actvals);
 OUTPUT:
  actvals
  RETVAL

int
gsdGet1c(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, values, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  char	* values = NO_INIT
  int actvals = NO_INIT
 PREINIT:
  int first;
  int last;
  int i;
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  values = malloc(16*(last-first+1));
  RETVAL = gsdGet1c(file_dsc, item_dsc, data_ptr, itemno, ndims,
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
# The packed data are copied directly into a perl scalar
# They can be unpacked using the perl unpack command

int
gsdGet1dp(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, packed, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  char	* packed = NO_INIT
  int actvals = NO_INIT
 PREINIT:
  int first;
  int last;
  void * pvals;
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  pvals = get_mortalspace(last  - first + 1,'d');
  RETVAL = gsdGet1d(file_dsc, item_dsc, data_ptr, itemno, ndims,
		    dimvals, start, end, (double *) pvals, &actvals); 
  sv_setpvn((SV*)ST(8), (char *)pvals, sizeof(double)*actvals);
 OUTPUT:
 actvals
 RETVAL

int
gsdGet1ip(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, packed, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  char	* packed = NO_INIT
  int actvals = NO_INIT
 PREINIT:
  int first;
  int last;
  void * pvals;
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  pvals = get_mortalspace(last  - first + 1,'i');
  RETVAL = gsdGet1i(file_dsc, item_dsc, data_ptr, itemno, ndims,
		    dimvals, start, end, (int *) pvals, &actvals); 
  sv_setpvn((SV*)ST(8), (char *)pvals, sizeof(int)*actvals);
 OUTPUT:
 actvals
 RETVAL

int
gsdGet1rp(file_dsc, item_dsc, data_ptr, itemno, ndims, dimvals, start, end, packed, actvals)
  file_descriptor *     file_dsc
  item_descriptor *     item_dsc 
  gsd_data *        data_ptr
  int itemno
  int ndims
  int	* dimvals
  int	* start
  int	* end
  char	* packed = NO_INIT
  int actvals = NO_INIT
 PREINIT:
  int first;
  int last;
  void * pvals;
 CODE:
  first = nindex( ndims, dimvals, start );
  last  = nindex( ndims, dimvals, end   );
  pvals = get_mortalspace(last  - first + 1,'f');
  RETVAL = gsdGet1r(file_dsc, item_dsc, data_ptr, itemno, ndims,
		    dimvals, start, end, (float *) pvals, &actvals); 
  sv_setpvn((SV*)ST(8), (char *)pvals, sizeof(float)*actvals);
 OUTPUT:
 actvals
 RETVAL


