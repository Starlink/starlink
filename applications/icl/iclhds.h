/******************************************************************************
 *
 *	I C L H D S . H
 *
 * Header file for the ICL interface module to the Starlink Hierarchical Data
 * System (HDS) in module hds.c
 *
 * History:
 *   01-DEC-1993 (BKM+AJC):
 *      Created
 *
 ******************************************************************************
 *
 * Useful HDS constants
 */
#include "dat_par.h"
#define HDS_I_FNF 0x08c883cb
/*
 * External function and variable declarations for ICL module hds.c
 */
extern int   hds_flush(void);
extern int   start_hds(void);
extern int   deinit_hdsfile(void);
extern value reload_vars_hds(void);
extern value name_interpret_hds ( node *n, int op );
/*
 * Function declarations required for HDS itself
 */
#ifdef F77_SUBROUTINE

extern F77_SUBROUTINE(hds_start)( INTEGER(status) );
extern F77_SUBROUTINE(hds_open)( CHARACTER(file), CHARACTER(mode),
    CHARACTER(loc), INTEGER(status) TRAIL(file) TRAIL(mode) TRAIL(loc) );
extern F77_SUBROUTINE(hds_close)( CHARACTER(loc), INTEGER(status) TRAIL(loc));
extern F77_SUBROUTINE(hds_stop)( INTEGER(status) );
extern F77_SUBROUTINE(hds_show) (CHARACTER(topic), INTEGER(status)
    TRAIL(topic) );
extern F77_SUBROUTINE(hds_free) (CHARACTER(loc), INTEGER(status)
    TRAIL(loc) );
extern F77_SUBROUTINE(hds_new)( CHARACTER(file), CHARACTER(fcname),
    CHARACTER(fctype), INTEGER(ndim), INTEGER_ARRAY(dim), CHARACTER(loc),
    INTEGER(status) TRAIL(file) TRAIL(fcname) TRAIL(fctype) TRAIL(loc) );
extern F77_SUBROUTINE(hds_link) ( CHARACTER(loc), CHARACTER(name),
    INTEGER(status) TRAIL(loc) TRAIL(name) );
extern F77_SUBROUTINE(dat_ncomp)( CHARACTER(loc), INTEGER(ncomp), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_name)( CHARACTER(loc), CHARACTER(name),
    INTEGER(status) TRAIL(loc) TRAIL(name) );
extern F77_SUBROUTINE(dat_index)( CHARACTER(loc1), INTEGER(index), 
    CHARACTER(loc2), INTEGER(status) TRAIL(loc1) TRAIL(loc2) );
extern F77_SUBROUTINE(dat_type)( CHARACTER(loc), CHARACTER(type), 
    INTEGER(status) TRAIL(loc) TRAIL(type) );
extern F77_SUBROUTINE(dat_annul)( CHARACTER(loc), INTEGER(status)
    TRAIL(loc) );
extern F77_SUBROUTINE(dat_erase)( CHARACTER(loc), CHARACTER(name),
    INTEGER(status) TRAIL(loc) TRAIL(name) );
extern F77_SUBROUTINE(dat_find)( CHARACTER(loc1), CHARACTER(name), 
    CHARACTER(loc2), INTEGER(status) TRAIL(loc1) TRAIL(name) TRAIL(loc2) );
extern F77_SUBROUTINE(dat_new)( CHARACTER(loc), CHARACTER(name), 
    CHARACTER(type), INTEGER(ndims), INTEGER_ARRAY(dims), INTEGER(status) 
    TRAIL(loc) TRAIL(name) TRAIL(type) );
extern F77_SUBROUTINE(dat_there)( CHARACTER(loc), CHARACTER(name), 
    LOGICAL(there), INTEGER(status) TRAIL(loc) TRAIL(name) );
extern F77_SUBROUTINE(dat_clone)( CHARACTER(loc1), CHARACTER(loc2), 
    INTEGER(status) TRAIL(loc1) TRAIL(loc2) );
extern F77_SUBROUTINE(dat_get0i)( CHARACTER(loc), INTEGER(ival), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_put0i)( CHARACTER(loc), INTEGER(ival), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_get0r)( CHARACTER(loc), REAL(rval), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_put0r)( CHARACTER(loc), REAL(rval), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_get0d)( CHARACTER(loc), DOUBLE(dval), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_put0d)( CHARACTER(loc), DOUBLE(dval), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_get0l)( CHARACTER(loc), LOGICAL(lval), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_put0l)( CHARACTER(loc), LOGICAL(lval), 
    INTEGER(status) TRAIL(loc) );
extern F77_SUBROUTINE(dat_get0c)( CHARACTER(loc), CHARACTER(cval), 
    INTEGER(status) TRAIL(loc) TRAIL(cval) );
extern F77_SUBROUTINE(dat_put0c)( CHARACTER(loc), CHARACTER(cval), 
    INTEGER(status) TRAIL(loc) TRAIL(cval) );
#endif
