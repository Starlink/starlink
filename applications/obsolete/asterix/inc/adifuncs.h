#if !defined(_ADI_FUNC_H_)
#define _ADI_FUNC_H_

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Function prototypes
 */
void 		ADIfuncInit( ADIstatus status );

ADIobj		ADImkB( ADIbyte v, ADIstatus status );
ADIobj		ADImkUB( ADIubyte v, ADIstatus status );
ADIobj		ADImkW( ADIword v, ADIstatus status );
ADIobj		ADImkUW( ADIuword v, ADIstatus status );
ADIobj		ADImkI( ADIinteger v, ADIstatus status );
ADIobj		ADImkR( ADIreal v, ADIstatus status );
ADIobj		ADImkD( ADIdouble v, ADIstatus status );
ADIobj		ADImkL( ADIlogical v, ADIstatus status );

#ifdef __cplusplus
}
#endif

#endif
