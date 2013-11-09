#if !defined(_ADI_ERR_)
#define _ADI_ERR_ 1

/*
 *  Operating in standalone mode?
 */
#ifdef NOEMS
#define SAI__OK       0L
#define SAI__ERROR    1L
#define _ecode(_x)    (_x)

/*
 * Starlink environment? 1515 is our facility code from Starlink
 */
#else
#include "sae_par.h"
#define _ebase	      (134250498L + 65536L * 1515L)

#define _ecode(_x)    (_ebase + (_x)*8)
#endif

#define ADI__CONER    _ecode(1)
#define ADI__TRUNC    _ecode(2)
#define ADI__INVARG   _ecode(3)
#define ADI__SYMDEF   _ecode(4)
#define ADI__FATAL    _ecode(5)
#define ADI__IDINV    _ecode(6)
#define ADI__ILLKOP   _ecode(7)
#define ADI__NOTSET   _ecode(8)
#define ADI__EXISTS   _ecode(9)
#define ADI__NOMTH    _ecode(10)
#define ADI__MTHERR   _ecode(11)
#define ADI__NOMEMB   _ecode(12)
#define ADI__ISPRIM   _ecode(13)
#define ADI__ILLOP    _ecode(14)
#define ADI__OUTMEM   _ecode(15)
#define ADI__NOTACT   _ecode(16)
#define ADI__NOPROP   _ecode(17)
#define ADI__NOCOMP   _ecode(18)
#define ADI__EXCEED   _ecode(19)
#define ADI__NOTDEL   _ecode(20)
#define ADI__NONAME   _ecode(21)
#define ADI__PRGERR   _ecode(22)
#define ADI__MAPPED   _ecode(23)
#define ADI__NOTMAP   _ecode(24)
#define ADI__RDONLY   _ecode(25)
#define ADI__SYNTAX   _ecode(26)
#define ADI__ITREND   _ecode(27)
#define ADI__NOFUN    _ecode(28)
#define ADI__INTGTY   _ecode(29)
#define ADI__RETRY    _ecode(30)

#define ADI__CALNXTMTH  _ecode(50)
#define ADI__UNWIND   _ecode(51)

#endif

