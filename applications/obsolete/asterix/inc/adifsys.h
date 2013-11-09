#if !defined(_ADI_FSYS_)
#define _ADI_FSYS_ 1

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

void    ADIfsysFileClose( ADIobj id, ADIstatus status );
void    ADIfsysFileComit( ADIobj id, ADIstatus status );
void	ADIfsysInit( ADIstatus status );


ADIobj  adix_base_SetLink( int narg, ADIobj args[], ADIstatus status );

void            adix_unlnk( ADIobj id, ADIobj lid, ADIstatus status );

void 		adix_defrcb( ADIobj rid, char *name, int nlen,
			ADIobj rtn, ADIstatus status );
void 		adix_defrep( char *name, int nlen, ADIobj *id,
			ADIstatus status );
void            adix_getfile( ADIobj id, ADIobj *fid, ADIstatus status );
ADIobj          adix_getlink( ADIobj id, ADIstatus status );
void            adix_getpath( ADIobj id, ADIlogical nulterm, int mxlen, char *buf, int *actlen, ADIstatus status );
void 		adix_locrcb( ADIobj rid, char *name, int nlen,
			ADIobj *rtn, ADIstatus status );
void		adix_locrep( char *name, int nlen, ADIobj *id, ADIstatus status );
void            adix_fcreat( char *fspec, int flen, ADIobj id, ADIobj *fid, ADIstatus status );
void            adix_fclone( ADIobj id, char *fspec, int flen, char *cls, int clen,
			ADIobj *fid, ADIstatus status );
ADIobj 		adix_link_efile( ADIobj id, char *cls, int clen, 
			ADIstatus status );
void            adix_fopen( char *fspec, int flen, char *cls, int clen,
			char *mode, int mlen, ADIobj *id, ADIstatus status );

#ifdef __cplusplus
}
#endif

extern
  ADIobj	ADI_G_replist;
  
#endif
