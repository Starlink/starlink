/*+
 *  Name:
 *     ems.h

 *  Purpose:
 *     EMS_ C interface header file.

 *  Language:
 *     Starlink ANSI C

 *  Description:
 *     This include file contains the function prototypes for all
 *     EMS C interface routines and defines EMS__VERSN to be the major
 *     version number

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     19-SEP-1990 (PCTR):
 *        Original version.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     5-OCT-1993 (PCTR):
 *        Updated for Vn. 1.2-3
 *     28-SEP-1994 (AJC):
 *        V1.4 Added ems_facer_c and ems_errno_c
 *     21-JUN-1995 (AJC):
 *        V1.5 Added ems1_starf_c
 *     13-MAY-1999 (AJC):
 *        Added the emsXxx form of name
 *        and #define old_names = new_names
 *        Removed ems_tune/gtune/show/_c
 *        Added ems1_get_facility_error
 *     27-JUL-2001 (AJC):
 *        Removed emsFmtx
 *        Add emsExpnd, emsTune
 *     13-AUG-2001 (AJC):
 *        Removed emsFioer
 *        #define EMS__VERSN
 *     20-SEP-2001 (AJC):
 *        Added emsSetnc and point ems_setc_c at it
 *      3-MAR-2006 (TIMJ):
 *        Add emsSetu / emsSetp / emsSeti64
 *     30-JUL-2008 (PWD):
 *        Added emsGtune.
 *     31-JUL-2008 (PWD):
 *        Added emsStune and changed emsGtune to return the value as a result.
 *        Marked emsTune as deprecated.
 *     10-SEP-2008 (TIMJ):
 *        Copy from ems.h. Only includes F77 prototypes.
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *- */

#ifndef EMS_F77_DEFINED
#define EMS_F77_DEFINED

/* Fortran Wrappers Prototypes: */
#include "f77.h"
F77_SUBROUTINE(ems_annul) ( INTEGER(status ) );

F77_SUBROUTINE(ems_begin) ( INTEGER(status ) );

F77_SUBROUTINE(ems_eload) ( CHARACTER(param_f ),
                            INTEGER(parlen),
                            CHARACTER(opstr_f),
                            INTEGER(oplen),
                            INTEGER(status)
                            TRAIL( plength )
                            TRAIL( olength ) );

F77_SUBROUTINE(ems_expnd) ( CHARACTER(text),
                            CHARACTER(opstr),
                            INTEGER(oplen),
                            INTEGER(status)
                            TRAIL( tlength )
                            TRAIL( olength ) );

F77_SUBROUTINE (ems_fioer) ( CHARACTER(token),
                             INTEGER(iostat)
                             TRAIL(token) );

F77_SUBROUTINE(ems_end) ( INTEGER(status ) );

F77_SUBROUTINE(ems_errno) ( CHARACTER(token_f),
                            INTEGER(errno_f)
                            TRAIL( tlength ) );

F77_SUBROUTINE(ems_facer) ( CHARACTER(token_f),
                            INTEGER(status_f)
                            TRAIL( tlength ) );

F77_SUBROUTINE(ems_level) ( INTEGER(level ) );

F77_SUBROUTINE(ems_mark) ( void );

F77_SUBROUTINE(ems_mload) ( CHARACTER(param),
                            CHARACTER(text),
                            CHARACTER(opstr),
                            INTEGER(oplen),
                            INTEGER(status)
                            TRAIL( plength )
                            TRAIL( tlength )
                            TRAIL( olength ) );

F77_SUBROUTINE(ems_renew) ( void );

F77_SUBROUTINE(ems_rep) ( CHARACTER(param),
                          CHARACTER(text),
                          INTEGER(status)
                          TRAIL( plength )
                          TRAIL( tlength ) );

F77_SUBROUTINE(ems_rlse) ( void );

F77_SUBROUTINE(ems_setc) ( CHARACTER(token),
                           CHARACTER(cvalue)
                           TRAIL( tlength )
                           TRAIL( clength ) );

F77_SUBROUTINE(ems_setd) ( CHARACTER(token),
                           DOUBLE(dvalue)
                           TRAIL( tlength ) );

F77_SUBROUTINE(ems_seti) ( CHARACTER(token),
                           INTEGER(ivalue)
                           TRAIL( tlength ) );

F77_SUBROUTINE(ems_setk) ( CHARACTER(token),
                           INTEGER8(ivalue)
                           TRAIL( tlength ) );

F77_SUBROUTINE(ems_setl) ( CHARACTER(token),
                           INTEGER(lvalue)
                           TRAIL( tlength ) );

F77_SUBROUTINE(ems_setr) ( CHARACTER(token),
                           REAL(rvalue)
                           TRAIL( tlength ) );

F77_SUBROUTINE(ems_show) ( CHARACTER(topic),
                           INTEGER(status)
                           TRAIL( tlength ) );

F77_SUBROUTINE(ems_stat) ( INTEGER(status) );

F77_SUBROUTINE(ems_syser) ( CHARACTER(token),
                            INTEGER(systat)
                            TRAIL( tlength ) );

F77_SUBROUTINE(ems_tune) ( CHARACTER(list),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL( llength ) );

F77_SUBROUTINE (ems_gtune)( CHARACTER(key), INTEGER(value), INTEGER(status)
                            TRAIL(key) );

#endif	/* EMS_F77_DEFINED */
