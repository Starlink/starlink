/*
 * Already done these definitions?
 */
#if !defined(_ADI_PKG_H_)
#define _ADI_PKG_H_ 1

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

void		ADIpkgInit( ADIstatus status );
void		ADIpkgRequire( char *name, int nlen, ADIstatus status );
void		ADIcmdExec( ADIobj istream, ADIobj ostream, ADIstatus status );

#ifdef __cplusplus
}
#endif

#endif
