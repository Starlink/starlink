#if !defined(AIO_MACROS)
#define AIO_MACROS

#include "f77.h"			/* Starlink C <-> Fortran interface */

#define AIO__MXFMTLEN        30

typedef
  enum
    {
    AIO_T_CHAR, AIO_T_INTEGER, AIO_T_LOGICAL, AIO_T_REAL, AIO_T_DOUBLE
    }
  AIOtypeCode;


typedef
  struct
    {
    AIOtypeCode		type;		/* Convert for this type */
    int			size;		/* Element size in bytes */
    char		*fmt;		/* Format to use */
    int			fmtlen;		/* Length of *fmt for Fortran */
    void                (*func)();      /* Convertor function */
    }
  AIOformatControl;

/*
 *  Convertor prototypes
 */
F77_SUBROUTINE(aio_itoc)();
F77_SUBROUTINE(aio_rtoc)();
F77_SUBROUTINE(aio_dtoc)();
F77_SUBROUTINE(aio_ltoc)();
F77_SUBROUTINE(aio_ctoc)();

#endif
