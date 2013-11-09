/*
 * Already done these definitions?
 */
#if !defined(_ADI_LIST_H_)
#define _ADI_LIST_H_ 1

typedef
  struct {
    ADIobj	car,cdr;
    }
  ADIlist;


/*
 * List cell type access
 */
#define         UT_cid_list   ADI_G_alloc_list
#define		_list_data(_x)	((ADIlist *) _DTDAT(_x))
#define         _CAR(_x)        (_list_data(_x)->car)
#define         _CDR(_x)        (_list_data(_x)->cdr)
#define         _CAAR(_x)       (_CAR(_CAR(_x)))
#define         _CDAR(_x)       (_CDR(_CAR(_x)))
#define         _CADR(_x)       (_CAR(_CDR(_x)))
#define         _CDDR(_x)       (_CDR(_CDR(_x)))
#define		_list_q(_x)	(_DTDEF(_x)->selfid==UT_cid_list)

#define 	_GET_CARCDR(_car,_cdr,_x) \
		{ADIlist	*ldata=_list_data(_x);\
		 (_car)=ldata->car;(_cdr)=ldata->cdr;}

#define 	_GET_CARCDR_A(_car,_cdr,_x) \
		{(_car) = &_list_data(_x)->car; (_cdr) = (_car) + 1;}

/*
 * Allocator object
 */
extern ADIobj UT_cid_list;

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Sub-package initialiser
 */
void   lstx_init( ADIstatus status );

/*
 * ADI internal routines
 */
ADIobj lstx_append( ADIobj lst1, ADIobj lst2, ADIstatus status );
ADIobj lstx_first( ADIobj lid, ADIstatus status );
ADIobj *lstx_nth( ADIobj lid, ADIinteger n, ADIstatus status );
ADIobj lstx_rest( ADIobj lid, ADIstatus status );
void   lstx_inscel( ADIobj id, ADIobj **ipoint, ADIstatus status );
int    lstx_len( ADIobj lid, ADIstatus status );
ADIobj lstx_cell( ADIobj aid, ADIobj bid, ADIstatus status );
ADIobj lstx_new2( ADIobj aid, ADIobj bid, ADIstatus status );
ADIobj lstx_revrsi( ADIobj id, ADIstatus status );
void   lstx_push( ADIobj obj, ADIobj *list, ADIstatus status );
void   lstx_sperase( ADIobj *list, ADIstatus status );
void   lstx_addtoset( ADIobj *list, ADIobj obj, ADIstatus status );

ADIobj lstx_construct_i( int narg, ADIobj args[], ADIstatus status );

/*
 * User C interface
 */
ADIobj ADInewList2( ADIobj aid, ADIobj bid, ADIstatus status );
ADIobj adix_assoc( ADIobj idx, ADIobj lst, ADIstatus status );
ADIobj adix_mapcar1( ADIobj (*proc)(ADIobj,ADIstatus),
		     ADIobj (*join)(ADIobj,ADIobj,ADIstatus),
		     ADIobj lst, ADIstatus status );
ADIlogical adix_eql_p( ADIobj x, ADIobj y, ADIstatus status );
ADIlogical adix_member( ADIobj element, ADIobj list,
			ADIlogical (*test)(ADIobj,ADIobj,ADIstatus),
			ADIstatus status );
ADIobj adix_removeif( ADIlogical (*test)(ADIobj,ADIobj,ADIstatus),
		      ADIobj args, ADIobj lst,
		      ADIstatus status );

#ifdef __cplusplus
}
#endif

/*
 * User FORTRAN interface
 */
#ifdef ADI_F77
F77_SUBROUTINE(adi_newlist2)( INTEGER(aid), INTEGER(bid), INTEGER(cid),
			      INTEGER(status) );
#endif

#endif
