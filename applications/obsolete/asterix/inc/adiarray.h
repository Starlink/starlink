#if !defined(_ADI_ARRAY_H_)
#define _ADI_ARRAY_H_

typedef
  struct {
    ADIobj              data;           /* First data item */
    ADIobj              parent;         /* Parent array if slice */
    int                 dims[ADI__MXDIM];
    short               ndim;           /* Number of dimensions */
    }
  ADIarray;


#define		KT_DEFN_ary	ADI_G_tdef_ary
#define         _ary_data(_x)   ((ADIarray *) _ID_DATA(_x))
#define         _ary_q(_x)      (_ID_BLOCK(_x)->cdef==&KT_DEFN_ary)

extern ADIclassDef	KT_DEFN_ary;

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

void	ADIaryAlter( ADIobj id, char *name, int nlen,
		int ndim, int dims[], ADIstatus status );
void	ADIaryBaseInfo( ADIarray *ary, int indices[], int orig[],
		int **bdims, ADIobj *bdata, ADIstatus status );
ADIobj 	ADIaryCell( ADIarray *ary, int index[], ADIstatus status );
int	ADIaryCountNelm( int ndim, int dims[] );
void	ADIaryIndices( int ndim, int dims[], int offset, int index[],
		ADIstatus status );
void	ADIaryInit( ADIstatus status );
ADIobj	ADIaryNew( int ndim, int dims[], ADIobj dataobjs, ADIobj parent,
		ADIstatus status );
int 	ADIaryOffset( int ndim, int dims[], int index[] );
void	ADIaryPrint( ADIobj stream, ADIarray *ary, ADIstatus status );

#ifdef __cplusplus
}
#endif

#endif
