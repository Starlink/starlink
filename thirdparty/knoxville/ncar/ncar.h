/*+
 *  Name:
 *     ncar.h

 *  Purpose:
 *     Function prototypes and language interface for NCAR C routines.

 *  Language:
 *     ANSI C

 *  Type of Module:
 *     C header file

 *  Description:

 *  Authors:
 *     PCTR: P.C.T. Rees (Starlink, RAL)
 *     {enter_new_authors_here}

 *  History:
 *     7-FEB-1992 (PCTR):
 *        Original version.
 *     27-JUN-1995 (BKM):
 *        Add Linux support.
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *- */

#if !defined(NCAR_MACROS)
#define NCAR_MACROS
#endif

#ifdef vms                              /* VMS-specific definitions */

/* Macro Definitions: */
#define INT_BITS 32
#define INTEGER int

/* Function Prototypes: */
INTEGER iand( INTEGER *i1, INTEGER *i2 );
INTEGER ior( INTEGER *i1, INTEGER *i2 );
INTEGER ishft( INTEGER *i, INTEGER *nshift );

/* Macro Definitions: */
#define IAND( _i1, _i2 )\
	iand( -i1, _i2 )
#define IOR( _i1, _i2 )\
	ior( _i1, _i2 )
#define ISHFT( _i, _nshift )\
	ishft( _i, _nshift )

#endif

#ifdef sun                              /* SUN4-specific definitions */

/* Macro Definitions: */
#define INT_BITS 32
#define INTEGER int

/* Function Prototypes: */
INTEGER iand_( INTEGER *i1, INTEGER *i2 );
INTEGER ior_( INTEGER *i1, INTEGER *i2 );
INTEGER ishft_( INTEGER *i, INTEGER *nshift );

/* Macro Definitions: */
#define IAND( _i1, _i2 )\
	iand_( _i1, _i2 )
#define IOR( _i1, _i2 )\
	ior_( _i1, _i2 )
#define ISHFT( _i, _nshift )\
	ishft_( _i, _nshift )

#endif

#if defined(mips) || defined(__alpha) || defined(linux) 

/* Macro Definitions: */
#define INT_BITS 32
#define INTEGER int

/* Function Prototypes: */
INTEGER iand_( INTEGER *i1, INTEGER *i2 );
INTEGER ior_( INTEGER *i1, INTEGER *i2 );
INTEGER ishft_( INTEGER *i, INTEGER *nshift );

/* Macro Definitions: */
#define IAND( _i1, _i2 )\
	iand_( _i1, _i2 )
#define IOR( _i1, _i2 )\
	ior_( _i1, _i2 )
#define ISHFT( _i, _nshift )\
	ishft_( _i, _nshift )

#endif
