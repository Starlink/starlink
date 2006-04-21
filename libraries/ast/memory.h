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
*-
*/

/* Include files. */
/* ============== */
/* Configuration results. */
/* ---------------------- */
#include <config.h>

/* C header files. */
/* --------------- */
#include <stddef.h>

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

#if defined(astCLASS) || 1       /* Nominally protected, but available for */
                                 /* use in developing (e.g.) foreign */
                                 /* language or graphics interfaces. */
#define astChrMatch(str1,str2) astChrMatch_(str1,str2)
#define astChrMatchN(str1,str2,n) astChrMatchN_(str1,str2,n)
#define astFree(ptr) astFree_(ptr)
#define astGrow(ptr,n,size) astGrow_(ptr,n,size)
#define astMalloc(size) astMalloc_(size)
#define astMemCaching(flag) astMemCaching_(flag)
#define astRealloc(ptr,size) astRealloc_(ptr,size)
#define astSizeOf(ptr) astSizeOf_(ptr)
#define astTSizeOf(ptr) astTSizeOf_(ptr)
#define astStore(ptr,data,size) astStore_(ptr,data,size)
#define astAppendString(ptr,len,text) astAppendString_(ptr,len,text)
#define astString(chars,nchars) astString_(chars,nchars)
#define astStringArray(chars,nel,len) astStringArray_(chars,nel,len)
#define astChrLen(string) astChrLen_(string)
#ifdef HAVE_NONANSI_SSCANF
#define astSscanf astSscanf_
#else
#define astSscanf sscanf
#endif
#define astChrSplit(str,n) astChrSplit_(str,n)
#endif

/* Functions used for debugging memory leaks, etc */
#ifdef MEM_DEBUG

#define astActiveMemory(label) astActiveMemory_(label)
#define astMemoryTune(name,value) astMemoryTune_(name,value)
#define astWatchMemory(id) astWatchMemory_(id)
#define astFlushMemory(leak) astFlushMemory_(leak)
#define astBeginPM astBeginPM_()
#define astEndPM astEndPM_()
#define astMemoryID(ptr) astMemoryID_(ptr)
#define astMemoryPtr(id) astMemoryPtr_(id)
#define astMemoryAlarm(text) astMemoryAlarm_(text)
#define astMemoryUse(ptr,text) astMemoryUse_(ptr,text)

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
