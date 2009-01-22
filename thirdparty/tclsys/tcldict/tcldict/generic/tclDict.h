/*
 * tclDict.h --
 *
 *	This header file describes the externally-visible facilities
 *	of the dict extension.
 *
 * Copyright (c) 1987-1994 The Regents of the University of California.
 * Copyright (c) 1993-1996 Lucent Technologies.
 * Copyright (c) 1994-1998 Sun Microsystems, Inc.
 * Copyright (c) 1998-2000 by Scriptics Corporation.
 * Copyright (c) 2002 by Kevin B. Kenny.  All rights reserved.
 * Copyright (c) 2004 by Pascal Scheffers
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * RCS: @(#) $Id$
 */

#ifndef _DICT
#define _DICT

#include <tcl.h>

/*
 * Windows needs to know which symbols to export.  Unix does not.
 * BUILD_sample should be undefined for Unix.
 */

#ifdef BUILD_dict
#undef TCL_STORAGE_CLASS
#define TCL_STORAGE_CLASS DLLEXPORT
#endif /* BUILD_dict */


/*
 * For C++ compilers, use extern "C"
 */

#ifdef __cplusplus
extern "C" {
#endif
    
typedef struct Tcl_Dict_ *Tcl_Dict;

/*
 * Structure definition for information used to keep track of searches
 * through dictionaries.  These fields should not be accessed by code
 * outside tclDictObj.c
 */

typedef struct {
    Tcl_HashSearch search;
    int epoch;
    Tcl_Dict dictionaryPtr;
} Tcl_DictSearch;


EXTERN CONST char *     Dict_InitStubs _ANSI_ARGS_((Tcl_Interp *interp,
                            CONST char *version, int exact));

EXTERN       int        Dict_Init _ANSI_ARGS_((Tcl_Interp *interp)) ;

#ifndef USE_TCL_STUBS

/*
 * When not using stubs, make it a macro.
 */

#define Dict_InitStubs(interp, version, exact) \
    Tcl_PkgRequire(interp, "dict", version, exact)

#endif /* USE_DICT_STUBS */

/*
 * Include the public function declarations that are accessible via
 * the stubs table.
 */

#include "dictDecls.h"

/*
 * end block for C++
 */
#ifdef __cplusplus
}
#endif

#endif /* _DICT */
