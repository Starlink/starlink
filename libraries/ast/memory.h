#if !defined( MEMORY_INCLUDED )  /* Include this file only once */
#define MEMORY_INCLUDED
/*
*+
*  Name:
*     memory.h

*  Purpose:
*     Define the interface to the Memory module.

*  Description:
*     This module defines functions which wrap up and extend the
*     standard C functions for performing memory allocation. They
*     provide better security against memory leaks, etc., and should
*     not be inter-mixed with the standard C functions.
*
*     Note that this module is not a class implementation, although it
*     resembles one.

*  Functions Defined:
*     Public:
*        None.
*
*     Protected:
*        astAppendString
*           Append a string to another string which grows dynamically.
*        astChrMatch
*           Case-insensitive string comparison.
*        astChrMatchN
*           Case-insensitive string comparison of an most N characters.
*        astFree
*           Free previously allocated memory.
*        astGrow
*           Allocate memory for an adjustable array.
*        astMalloc
*           Allocate memory.
*        astRealloc
*           Change the size of a dynamically allocated region of memory.
*        astSizeOf
*           Determine the size of a dynamically allocated region of memory.
*        astStore
*           Store data in dynamically allocated memory.
*        astString
*           Create a C string from an array of characters.
*        astStringArray
*           Create an array of C strings from an array of characters.
*        astChrLen
*           Returns length of a string without trailing white space, etc.
*        astSscanf
*           Like sscanf, but fixes certain platform-specific bugs in the
*           native sscanf implementation.
*        astTSizeOf
*           Determine the total size of a dynamically allocated region of memory.

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*     
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: D.S. Berry (Starlink)

*  History:
*     8-JAN-1996 (RFWS):
*        Original version.
*     26-JAN-1996 (RFWS)
*        Added function interfaces.
*     20-JUN-1996 (RFWS):
*        Added astString.
*     15-JUL-1996 (RFWS):
*        Use improved prologue style, etc. and make all functions protected.
*     11-SEP-1996 (RFWS):
*        Added astStringArray (original written by DSB).
*     18-MAR-1998 (RFWS):
*        Make interface available for writing foreign language and
*        graphics interfaces, etc.
*     18-MAR-1998 (RFWS):
*        Added explicit arguments to function macros.
*     29-JAN-2002 (DSB):
*        Added astChrLen and astSscanf.
*     15-NOV-2002 (DSB):
*        Added astChrMatch astChrMatchN.
*     23-FEB-2006 (DSB):
*        Added astMemCaching and AST__TUNULL.
*     2-MAR-2006 (DSB):
*        Only use astSscanf if the system on which AST was configured
*        showed the non-ANSI behaviour reported by Bill Joye.
*     21-JUN-2007 (DSB):
*        Added astVsprintf.
*-
*/

/* Include files. */
/* ============== */
/* Configuration results. */
/* ---------------------- */
#include <config.h>

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stddef.h>
#include "error.h"    

/* Macros. */
/* ======= */
#define AST__TUNULL -99999

/* Function prototypes. */
/* ==================== */
#if defined(astCLASS) || 1       /* Nominally protected, but available for */
                                 /* use in developing (e.g.) foreign */
                                 /* language or graphics interfaces. */
int astMemCaching_( int );
char **astChrSplit_( const char *, int * );
int astChrMatch_( const char *, const char * );
int astChrMatchN_( const char *, const char *, size_t );
char **astStringArray_( const char *, int, int );
char *astString_( const char *, int );
char *astVsprintf_( const char *, int, va_list );
int astSscanf_( const char *str, const char *format, ...);
size_t astSizeOf_( const void * );
size_t astTSizeOf_( const void * );
void *astFree_( void * );
void *astGrow_( void *, int, size_t );
void *astMalloc_( size_t );
void *astRealloc_( void *, size_t );
void *astStore_( void *, const void *, size_t );
size_t astChrLen_( const char * );
char *astAppendString_( char *, int *, const char * );

#ifdef MEM_DEBUG
void astActiveMemory_( const char * );
void astWatchMemory_( int );
void astFlushMemory_( int );
int astMemoryTune_( const char *, int );
int astMemoryID_( void * );
void *astMemoryPtr_( int );
void astMemoryAlarm_( const char * );
void astMemoryUse_( void *, const char * );
void astBeginPM_( void );
void astEndPM_( void );
#endif

#endif

/* Function interfaces. */
/* ==================== */
/* These wrap up the functions defined by this module. */

#define astChrMatch(str1,str2) astERROR_INVOKE(astChrMatch_(str1,str2))
#define astChrMatchN(str1,str2,n) astERROR_INVOKE(astChrMatchN_(str1,str2,n))
#define astFree(ptr) astERROR_INVOKE(astFree_(ptr))
#define astGrow(ptr,n,size) astERROR_INVOKE(astGrow_(ptr,n,size))
#define astMalloc(size) astERROR_INVOKE(astMalloc_(size))
#define astMemCaching(flag) astERROR_INVOKE(astMemCaching_(flag))
#define astRealloc(ptr,size) astERROR_INVOKE(astRealloc_(ptr,size))
#define astSizeOf(ptr) astERROR_INVOKE(astSizeOf_(ptr))
#define astTSizeOf(ptr) astERROR_INVOKE(astTSizeOf_(ptr))
#define astStore(ptr,data,size) astERROR_INVOKE(astStore_(ptr,data,size))
#define astAppendString(ptr,len,text) astERROR_INVOKE(astAppendString_(ptr,len,text))
#define astString(chars,nchars) astERROR_INVOKE(astString_(chars,nchars))
#define astStringArray(chars,nel,len) astERROR_INVOKE(astStringArray_(chars,nel,len))
#define astChrLen(string) astERROR_INVOKE(astChrLen_(string))
#ifdef HAVE_NONANSI_SSCANF
#define astSscanf astERROR_INVOKE(astSscanf_)
#else
#define astSscanf astERROR_INVOKE(sscanf)
#endif
#define astChrSplit(str,n) astERROR_INVOKE(astChrSplit_(str,n))
#define astVsprintf(format,split,args) astERROR_INVOKE(astVsprintf_(format,split,args))


/* Functions used for debugging memory leaks, etc */
#ifdef MEM_DEBUG

#define astActiveMemory(label) astERROR_INVOKE(astActiveMemory_(label))
#define astMemoryTune(name,value) astERROR_INVOKE(astMemoryTune_(name,value))
#define astWatchMemory(id) astERROR_INVOKE(astWatchMemory_(id))
#define astFlushMemory(leak) astERROR_INVOKE(astFlushMemory_(leak))
#define astBeginPM astERROR_INVOKE(astBeginPM_())
#define astEndPM astERROR_INVOKE(astEndPM_())
#define astMemoryID(ptr) astERROR_INVOKE(astMemoryID_(ptr))
#define astMemoryPtr(id) astERROR_INVOKE(astMemoryPtr_(id))
#define astMemoryAlarm(text) astERROR_INVOKE(astMemoryAlarm_(text))
#define astMemoryUse(ptr,text) astERROR_INVOKE(astMemoryUse_(ptr,text))

#else

#define astActiveMemory(label) 
#define astMemoryTune(name,value)
#define astWatchMemory(id)
#define astFlushMemory(leak) 
#define astBeginPM 
#define astEndPM
#define astMemoryID(ptr) 0
#define astMemoryPtr(id) NULL
#define astMemoryAlarm(text)
#define astMemoryUse(ptr,text) 

#endif

#endif
