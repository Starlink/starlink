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
#include "star/hds.h"


/*
 * External function and variable declarations for ICL module hds.c
 */
extern int   hds_flush(void);
extern int   start_hds(void);
extern int   deinit_hdsfile(void);
extern value reload_vars_hds(void);
extern value name_interpret_hds ( node *n, int op );

/*
 * Internal prototypes for hds.c
 */

static int init_hdsfile(void);
void openpar( char *, char *, char*, HDSLoc **, int * );
static value proc_getpar( node * );
void openglobal( char *mode, HDSLoc **loc, int *status );

void ensure_exists( HDSLoc *loc,
                    char *parameter, char *type, char *btype,
                    HDSLoc **ploc,
                    int *status );
static value proc_setpar( node * );
static value proc_setglobal( node * );
static value proc_createglobal( node * );
value init_hds( void );

