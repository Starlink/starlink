#if !defined(_ADI_KRNL_H_)
#define _ADI_KRNL_H_ 1

/*
 * Data access codes. Can be OR'ed together
 */

#define DA__DEFAULT      0x0000 	/* Default settings */
#define DA__SET		 0x0001		/* Data must be set */
#define DA__CREATE       0x0002		/* Create access is required */
#define DA__ARRAY        0x0004		/* Object must be an array */



extern ADIblock      *ADI_G_blks[];


extern ADIinterp	*ADI_G_curint;


extern ADIlogical ADI_G_init;
extern ADIobj     UT_cid_b;
extern ADIobj     UT_cid_ub;
extern ADIobj     UT_cid_w;
extern ADIobj     UT_cid_uw;
extern ADIobj     UT_cid_i;
extern ADIobj     UT_cid_r;
extern ADIobj     UT_cid_d;
extern ADIobj     UT_cid_l;
extern ADIobj     UT_cid_c;
extern ADIobj     UT_cid_p;
extern ADIobj     UT_cid_struc;
extern ADIobj     UT_cid_ref;
extern ADIobj     UT_cid_strm;
extern ADIclassDef	KT_DEFN_han;

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif
void 		adi_init( ADIstatus status );
void		adix_probe( ADIstatus status );
ADIobj 		adix_cls_nallocd( ADIclassDef *tdef, int ndim, int dims[],
			void *data, ADIstatus status );
ADIobj 		adix_cls_nalloc( ADIclassDef *tdef, int ndim, int dims[], ADIstatus status );
ADIobj 		adix_cls_alloc( ADIclassDef *tdef, ADIstatus status );
ADIlogical	adix_chkder( ADIclassDef *c1, ADIclassDef *c2, ADIstatus status );
void 		adix_delprp( ADIobj id, char *pname, int plen, ADIstatus status );
void 		adix_locprp( ADIobj id, char *pname, int plen, ADIobj *pid, ADIstatus status );
void 		adix_nprp( ADIobj id, int *nprp, ADIstatus status );
void 		adix_indprp( ADIobj id, int index, ADIobj *pid, ADIstatus status );
ADIobj		adix_config( char *attr, ADIobj newval, ADIstatus status );
void 		adix_delcmp( ADIobj id, char *cname, int clen, ADIstatus status );
void 		adix_loccmp( ADIobj id, char *cname, int clen, ADIobj *cid, ADIstatus status );
void 		adix_ncmp( ADIobj id, int *ncmp, ADIstatus status );
void 		adix_indcmp( ADIobj id, int index, ADIobj *cid, ADIstatus status );
ADIclassDef 	*adix_loccls( ADIobj name, ADIstatus status );
void		adix_cell( ADIobj id, char *name, int nlen, int ndim,
                        int index[], ADIobj *sid, ADIstatus status );
void		adix_cerase( ADIobj id, char *mem, int mlen, ADIstatus status );
ADIobj		adix_clone( ADIobj id, ADIstatus status );
void 		adix_ccopy( ADIobj in, char *inmem, int inmlen, ADIobj out,
			char *outmem, int outmlen, ADIstatus status );
ADIobj          adix_copy( ADIobj id, ADIstatus status );
ADIobj		adix_cmn( char *str, int slen, ADIstatus status );
ADIobj		adix_cmn_i( char *str, int slen, ADIlogical dstatic,
			ADIstatus status );
ADIobj		adix_cmnC( char *str, ADIstatus status );
ADIlogical 	adix_equal( ADIobj id1, ADIobj id2, ADIstatus status );
ADIlogical	adix_state( ADIobj id, char *name, int nlen,
			ADIstatus status );
ADIlogical	adix_there( ADIobj id, char *name, int nlen,
			ADIstatus status );
void		adix_putid( ADIobj id, char *mem, int mlen,
			ADIobj value, ADIstatus status );
void		adix_putiid( ADIobj id, ADIobj name,
			ADIobj value, ADIstatus status );
void 		adix_id_flush( char *grp, int glen, ADIstatus status );
void 		adix_id_link( ADIobj id, char *grp, int glen,
			ADIstatus status );
void 		adix_rep_nomf( ADIstatype ecode, char *whats, ADIobj name, int narg,
		    ADIobj args[], ADIstatus status );

void            ADIdefClass_e( char *name, int nlen, char *parents, int plen,
			char *members, int mlen, ADIobj *tid, ADIstatus status );
ADIobj		ADIdefClass_i( int narg, ADIobj args[], ADIstatus status );
void		ADIdefClassCluster( ADIobj clsid, ADIinteger number,
			ADIstatus status );
void            adix_defcpa( char *name, int nlen,
			ADIobj rtn, ADIobj *id, ADIstatus status );
void            adix_deffun( char *spec, int slen,
			ADIobj rtn, ADIobj *id, ADIstatus status );
void            adix_defgen( char *name, int nlen, char *options, int olen,
			ADIobj rtn, ADIobj *id, ADIstatus status );
void 		adix_defgdp( ADIobj genid, ADIobj fdisp, ADIstatus status );
void            adix_defmcf( char *name, int nlen,
			ADIobj rtn, ADIobj *id, ADIstatus status );
void            adix_defmth( char *spec, int slen,
			ADIobj rtn, ADIobj *id, ADIstatus status );
void		adix_defvar( char *name, int nlen, ADIlogical global,
			ADIobj value, ADIstatus status );
void 		adix_setvar( ADIobj name, ADIlogical global, ADIobj value,
                  ADIstatus status );
ADIobj         *adix_defmem( ADIobj *id, ADIstatus status );
char 	       *adix_idd( ADIobj id );
char 	       *adix_iddt( ADIobj id, ADIclassDef **tdef );
char  	       *adix_dtdat( ADIobj id, ADIstatus status );
ADIclassDef    *adix_dtdef( ADIobj id, ADIstatus status );
void		adix_erase( ADIobj *id, ADIstatus status );
void		adix_merase( ADIobj *id, ADIinteger nval, ADIstatus status );
ADIobj          adix_exec( char *func, int flen, int narg, ADIobj args[],
			ADIstatus status );
ADIobj		adix_exec2( char *func, int flen, ADIobj arg1, ADIobj arg2,
			ADIstatus status );
ADIobj          adix_execi( ADIobj func, int narg, ADIobj args[],
			ADIstatus status );
ADIobj		adix_execi2( ADIobj func, ADIobj arg1, ADIobj arg2,
			ADIstatus status );
ADIobj		adix_find( ADIobj id, char *name, int nlen, ADIstatus status );
ADIobj		adix_findi( ADIobj id, ADIobj name, ADIstatus status );

void            adix_get_n( int clang, ADIobj id, char *name,
			int nlen, int ndim, int mxdims[], ADIobj *clsid,
			int vsize, void *value, int nactdims[], ADIstatus status );
void 		adix_get_nn( int clang, ADIobj id, char *name, int nlen,
                  	char *cls, int clen, int ndim, int mxdims[], 
                  	void *value, int nactdims[], ADIstatus status );

void		ADIkrnlLocDat( ADIobj *id, char *name, int nlen, int flgs,
			       ADIobjRequest *objreq, ADIstatus status );

ADIobj 		adix_exemth( ADIobj generic, ADIobj method,
			int narg, ADIobj args[], ADIstatus status );
void 		adix_gthmth( ADIobj gen, int narg, ADIobj args[], int nmform,
			ADIobj *mform[], ADIlogical mfopri[],
			ADIobj mlist[], ADIstatus status );
void            adix_map_n( int clang, ADIobj id, char *name, int nlen,
			char *mode, int mlen, ADIobj *clsid,
			int vsize, void **value, ADIstatus status );
void            adix_map_t( int clang, ADIobj id, char *name,
			int nlen, char *type, int tlen, char *mode,
			int mlen, void **value, ADIstatus status );
void    	adix_mtacop( ADImta *ind, ADImta *outd, ADIstatus status );
void    	adix_mark( void );
void    	adix_mtaid( ADIobj id, ADImta *mta, ADIstatus status );
ADIobj		adix_namei( ADIobj id, ADIstatus status );
void		adix_name( ADIobj id, ADIlogical clang, char *buf, int blen,
			ADIstatus status );
void		adix_newn( ADIobj pid, char *name, int nlen, char *cls,
			int clen, int ndim, int dims[],
			ADIobj *id, ADIstatus status );
void 		adix_new_n( ADIlogical clang, ADIobj pid, char *name, int nlen,
			int ndim, int dims[], void *value,
			ADIobj *clsid, int vsize, ADIobj *id, ADIstatus status );
void 		adix_ntrunc( char *name, int *len );
void            adix_put_n( int clang, ADIobj id, char *name,
			int nlen, int ndim, int dims[], ADIobj *clsid,
			int vsize, void *value, ADIstatus status );
void            adix_put_nn( int clang, ADIobj id, char *name,
			int nlen, char *cls, int clen, int ndim, int dims[], 
			void *value, ADIstatus status );
void            adix_print( ADIobj stream, ADIobj id, int level,
			ADIlogical vonly, ADIstatus status );

ADIobj 		adix_istrm( ADIobj strm, ADIstatus status );
void 		adix_ostrm( ADIobj strm, int narg, ADIobj ids[], ADIinteger *tlen,
			ADIstatus status );
ADIinteger	adix_ostrmc( int narg, ADIobj ids[], ADIstatus status );

ADIobj		adix_qcls( ADIobj id, char *name, int nlen,
			ADIstatus status );
void    	adix_refadj( ADIobj id, int offset, ADIstatus status );
int     	adix_refcnt( ADIobj id, ADIstatus status );
void    	adix_rlse( void );
void            adix_set_n( int clang, ADIobj id, char *name,
			int nlen, int ndim, int dims[], ADIobj *clsid,
			int vsize, void *value, ADIstatus status );
void		adix_shape( ADIobj id, char *name, int nlen, int mxndim,
			int dims[], int *ndim, ADIstatus status );
void		adix_size( ADIobj id, char *name, int nlen, int *nelm,
			ADIstatus status );
void		adix_slice( ADIobj id, char *name, int nlen, int ndim,
			int diml[], int dimu[], ADIobj *sid,
			ADIstatus status );
void		adix_unmap_n( ADIobj id, char *name, int nlen, void *ptr,
			    ADIstatus status );
ADIobj 		adix_pl_fgeti( ADIobj *plist, ADIobj name, ADIstatus status );
ADIobj 		adix_pl_geti( ADIobj plist, ADIobj name, ADIstatus status );
void 		adix_pl_seti( ADIobj obj, ADIobj prop, ADIobj valid, ADIstatus status );
ADIobj 		adix_cdsp_vo( ADICB rtn, int narg, ADIobj args[], ADIstatus status );
ADIobj 		adix_cdsp_voo( ADICB rtn, int narg, ADIobj args[], ADIstatus status );
ADIobj 		adix_fdsp_vo( ADICB *rtn, int *narg, ADIobj args[], ADIstatus status );
ADIobj 		adix_fdsp_voo( ADICB *rtn, int *narg, ADIobj args[], ADIstatus status );

/*
 * Manipulating interpreters
 */
ADIinterp       *ADIkrnlGetInt( ADIstatus status );
ADIinterp       *ADIkrnlNewInt( ADIstatus status );
ADIinterp       *ADIkrnlSetInt( ADIinterp *inter, ADIstatus status );

/*
 * Creating kernel objects
 */
ADIobj          ADIkrnlNewEproc( ADIlogical is_c, ADICB func,
			ADIstatus status );
ADIobj 		ADIkrnlNewHan( ADIobj id, ADIlogical slice,
			ADIobjHan **hdata, ADIstatus status );
ADIobj		ADIkrnlNewMth( ADImethod *mdata, ADIstatus status );


/*
 * Defining system methods
 */
void 		ADIkrnlDefDestrucInt( ADIclassDef *cdef, ADIobj proc,
			ADIstatus status );
void 		ADIkrnlDefDestrucKint( ADIclassDef *cdef, ADIcMethodCB cmeth,
			ADIstatus status );
void		ADIkrnlDefDestruc( ADIobj clsid, ADIobj proc, ADIstatus status );
void		ADIkrnlDefPrnt( ADIobj clsid, ADIobj proc, ADIstatus status );

/*
 * Adding system definitions
 */
void 		ADIkrnlAddCmdPars( ADIcmdParTableEntry ctable[],
			ADIstatus status );
void		ADIkrnlAddCommonStrings( ADIcstrTableEntry stable[],
			ADIstatus status );
void 		ADIkrnlAddFuncs( ADIfuncTableEntry ftable[],
			ADIstatus status );
void		ADIkrnlAddGenerics( ADIgnrcTableEntry gtable[],
			ADIstatus status );
void		ADIkrnlAddMethods( ADImthdTableEntry mtable[],
			ADIstatus status );
void 		ADIkrnlAddPtypes( ADIptypeTableEntry ptable[],
			ADIstatus status );

void 		ADIkrnlExecO( ADIobj prc, ADIobj arg, ADIstatus status );
void 		ADIkrnlExecOO( ADIobj prc, ADIobj arg1, ADIobj arg2, ADIstatus status );

ADIclassDef    *ADIkrnlFindClsC( char *cls, int clen, ADIstatus status );
ADIobj 		ADIkrnlFindClsI( ADIobj name, ADIstatus status );
ADIobj 		ADIkrnlFindClsExt( char *cls, int clen, ADIstatus status );

ADIlogical 	ADIkrnlChkDerived( ADIobj id, char *name, int nlen,
			ADIstatus status );
#ifdef __cplusplus
}
#endif

#endif
