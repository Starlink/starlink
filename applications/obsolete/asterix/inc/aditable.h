/*
 * Already done these definitions?
 */
#if !defined(_ADI_TABLE_H_)
#define _ADI_TABLE_H_ 1

typedef
  struct {
    ADIinteger	htsize;
    ADIobj	heads;
    }
  ADItable;


/*
 * Table type access
 */
#define         UT_cid_tbl    ADI_G_alloc_tbl
#define         _tbl_q(_x)      (_DTDEF(_x)->selfid==UT_cid_tbl)

#define         _tbl_data(_x)    ((ADItable *) _DTDAT(_x))

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Sub-package initialiser
 */
void   tblx_init( ADIstatus status );


/*
 * ADI internal routines
 */
ADIobj 		tblx_add( ADIobj *table, char *str, int slen, ADIobj data,
			ADIstatus status );
ADIobj 		tblx_find( ADIobj *table, char *str, int slen,
			ADIstatus status );
ADIobj 		tblx_findi( ADIobj *table, ADIobj str, ADIstatus status );
void 		tblx_hstats( ADIobj table, ADIstatus status );
ADIobj 		tblx_new( int size, ADIstatus status );
ADIobj 		tblx_sadd( ADIobj *table, char *str, int slen, ADIobj data,
			ADIstatus status );
ADIobj 		tblx_saddi( ADIobj *table, ADIobj str,
		   	ADIobj dataobj, ADIstatus status );
ADIlogical	tblx_scan( ADIobj *head, char *str, int slen, ADIobj **sptr,
			ADIstatus status );
ADIlogical	tblx_scani( ADIobj *head, ADIobj str, ADIobj **sptr,
			ADIobj **car, ADIstatus status );

#ifdef __cplusplus
}
#endif

/*
 * Allocator object
 */
extern ADIobj UT_cid_tbl;

#endif
