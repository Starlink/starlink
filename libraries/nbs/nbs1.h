/*
*+
 *  Name:
 *    nbs1.h

 *  Purpose:
 *    Provide prototypes for internal NBS functions

 *  Language:
 *    ANSI C

 *  Type of module:
 *    C include file

 *  Description:
 *    Provide prototypes for internal NBS functions. Not a public
 *    file.

 *  Copyright:
 *    Copyright (C) 1986-2005 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 *  Authors:
 *    WFL: William Lupton
 *    TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *    2-SEP-2005 (TIMJ):
 *      Extract from nbslow.c

 *-
 */

#if !defined NBSLOW_MACROS

/* Variable types */
#include "nbs_mac.h"

/* Depending on whether c_string macro is defined or not, define macros
   for routine names and define module name */

#ifdef c_string

#ifdef vms
#define MODULE nbclow
#include "nbs_module.h"
#endif


/* C strings are used for character string arguments. Module name is
   "nbclow" and routine names are all prefixed "nbc_". */

#define NBS_MAKE_KEY		nbc_make_key
#define NBS_INIT_ALLOC		nbc_init_alloc
#define NBS_ALLOC		nbc_alloc
#define NBS_DATA_ALLOC		nbc_data_alloc
#define NBS_DEINIT_ALLOC	nbc_deinit_alloc
#define NBS_WRITE_FILE		nbc_write_file
#define NBS_OPEN_FILE		nbc_open_file
#define NBS_READ_FILE		nbc_read_file
#define NBS_CLOSE_FILE		nbc_close_file
#define NBS_OPEN_WRITE		nbc_open_write
#define NBS_UPDATE_FILE		nbc_update_file
#define NBS_CREATE_SECTION	nbc_create_section
#define NBS_MAP_SECTION		nbc_map_section
#define NBS_UNMAP_SECTION	nbc_unmap_section
#define NBS_RELOCATE_POINTERS	nbc_relocate_pointers
#define NBS_RELOCATE_ITEM	nbc_relocate_item
#define NBS_RELOCATE_ADDRESS	nbc_relocate_address
#define NBS_STRIMP		nbc_strimp
#define NBS_STREXP		nbc_strexp
#define NBS_MLIST_EXITHANDLER   nbc_mlist_exithandler
#define NBS_MLIST_ADD           nbc_mlist_add
#define NBS_MLIST_FIND          nbc_mlist_find
#define NBS_MLIST_UNMAP         nbc_mlist_unmap
#define NBS_SLEEPMS             nbc_sleepms

#else

#ifdef vms
#define MODULE nbslow
#include "nbs_module.h"
#endif

/* String descriptors used for character string arguments. Module name is
   "nbslow" and routine names are all prefixed "nbs_". */

/* Starlink packages to interface Fortran and C */

#include "cnf.h"

#define NBS_MAKE_KEY		F77_EXTERNAL_NAME(nbs_make_key)
#define NBS_INIT_ALLOC		F77_EXTERNAL_NAME(nbs_init_alloc)
#define NBS_ALLOC		F77_EXTERNAL_NAME(nbs_alloc)
#define NBS_DATA_ALLOC		F77_EXTERNAL_NAME(nbs_data_alloc)
#define NBS_DEINIT_ALLOC	F77_EXTERNAL_NAME(nbs_deinit_alloc)
#define NBS_WRITE_FILE		F77_EXTERNAL_NAME(nbs_write_file)
#define NBS_OPEN_FILE		F77_EXTERNAL_NAME(nbs_open_file)
#define NBS_READ_FILE		F77_EXTERNAL_NAME(nbs_read_file)
#define NBS_CLOSE_FILE		F77_EXTERNAL_NAME(nbs_close_file)
#define NBS_OPEN_WRITE		F77_EXTERNAL_NAME(nbs_open_write)
#define NBS_UPDATE_FILE		F77_EXTERNAL_NAME(nbs_update_file)
#define NBS_CREATE_SECTION	F77_EXTERNAL_NAME(nbs_create_section)
#define NBS_MAP_SECTION		F77_EXTERNAL_NAME(nbs_map_section)
#define NBS_UNMAP_SECTION	F77_EXTERNAL_NAME(nbs_unmap_section)
#define NBS_RELOCATE_POINTERS	F77_EXTERNAL_NAME(nbs_relocate_pointers)
#define NBS_RELOCATE_ITEM	F77_EXTERNAL_NAME(nbs_relocate_item)
#define NBS_RELOCATE_ADDRESS	F77_EXTERNAL_NAME(nbs_relocate_address)
#define NBS_MLIST_ADD           F77_EXTERNAL_NAME(nbs_mlist_add)
#define NBS_MLIST_FIND          F77_EXTERNAL_NAME(nbs_mlist_find)
#define NBS_MLIST_UNMAP         F77_EXTERNAL_NAME(nbs_mlist_unmap)
#define NBS_SLEEPMS             F77_EXTERNAL_NAME(nbs_sleepms)

/* NBS_STRIMP and NBS_STREXP macros invoke routine with extra argument if
   required by the Fortran interface functions */

#define NBS_STRIMP(loc,arg,loc_l) \
F77_EXTERNAL_NAME(nbs_strimp) (loc,CHARACTER_ARG(arg),loc_l TRAIL_ARG(arg))
#define NBS_STREXP(arg,loc,loc_l) \
F77_EXTERNAL_NAME(nbs_strexp) (CHARACTER_ARG(arg),loc,loc_l TRAIL_ARG(arg))

#endif

/* Internal protoypes */

key_t NBS_MAKE_KEY( char * );
char * NBS_INIT_ALLOC( int, char *);
char * NBS_ALLOC( int );
char * NBS_DATA_ALLOC( int );
char * NBS_DEINIT_ALLOC ();
void NBS_WRITE_FILE( RW_CHARACTER(), RW_BYTE_ARRAY(), int, int, int,
		     W_INTEGER() TRAIL(arg) );
void NBS_OPEN_FILE( RW_CHARACTER(), FILE**, W_INTEGER(), W_INTEGER(),
		    W_INTEGER(), W_INTEGER() TRAIL(arg) );
void NBS_READ_FILE ( FILE *, int, RW_BYTE_ARRAY(), W_INTEGER());
void NBS_CLOSE_FILE( RW_POINTER() );
void NBS_OPEN_WRITE( char*, FILE**, int*);
void NBS_UPDATE_FILE(FILE*, char*, int, int*);
char *NBS_CREATE_SECTION ( RW_CHARACTER(), int,
                           W_INTEGER() TRAIL(arg) );
char *NBS_MAP_SECTION( RW_CHARACTER(), W_INTEGER() TRAIL(arg) );
void NBS_UNMAP_SECTION ( RW_POINTER(), int, W_INTEGER() );
void NBS_RELOCATE_POINTERS( item_id, int, int, int, int);
void NBS_RELOCATE_ITEM( item_id, int, int, int, int );
char *NBS_RELOCATE_ADDRESS( char *, int, int );
#ifdef c_string
char *NBS_STRIMP ( char *, RW_CHARACTER(), int TRAIL(arg) );
void NBS_STREXP( RW_CHARACTER(), char *, int TRAIL(arg) );
#else
/* These are macros to account for the TRAIL without having to include it */
char * F77_EXTERNAL_NAME(nbs_strimp) ( char *, RW_CHARACTER(), int TRAIL(arg) );
void   F77_EXTERNAL_NAME(nbs_strexp) (RW_CHARACTER(), char *, int TRAIL(arg) );;
#endif

/* Only used for C interface */
#ifdef NBS_MLIST_EXITHANDLER
void NBS_MLIST_EXITHANDLER();
#endif

void NBS_MLIST_ADD( int, int, char*, int*);
char *NBS_MLIST_FIND( int, int*);
void NBS_MLIST_UNMAP (char *,int *);
void NBS_SLEEPMS( int );


#define NBSLOW_MACROS

#endif /* NBSLOW_MACROS */
