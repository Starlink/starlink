#if !defined(_ADI_MEM_H_)
#define _ADI_MEM_H_ 1

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

void    	ADImemStart( void );
void    	ADImemStop( ADIstatus status );

void    	ADImemFree( void *ptr, size_t nb, ADIstatus status );
char           *ADImemAlloc( size_t nb, ADIstatus status );

void    	ADImemFreeObj( ADIobj *id, int nval, ADIstatus status );
void    	ADImemInitBlock( ADIblockCtrl *ctrl, size_t size,
		      int nunit, ADIobj clsid, ADIstatus status );
ADIobj  	ADImemAllocObj( ADIclassDef *ctrl, int nval,
		      ADIstatus status );
ADIinteger 	ADImemIdOff( ADIobj id1, ADIobj id2, ADIstatus status );
ADIobj		ADImemIdAddOff( ADIobj id, ADIinteger off, ADIstatus );

#ifdef __cplusplus
}
#endif

/*
 * Exported data
 */
extern
  ADIblock   *ADI_G_blks[];           /* Block address array */

/*
 * End of include file already loaded test
 */
#endif

