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

*  Copyright:
*     <COPYRIGHT_STATEMENT>

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
*-
*/

/* Include files. */
/* ============== */
/* C header files. */
/* --------------- */
#include <stddef.h>

/* Function prototypes. */
/* ==================== */
#if defined(astCLASS) || 1       /* Nominally protected, but available for */
                                 /* use in developing (e.g.) foreign */
                                 /* language or graphics interfaces. */
int astChrMatch_( const char *, const char * );
int astChrMatchN_( const char *, const char *, size_t );
char **astStringArray_( const char *, int, int );
char *astString_( const char *, int );
int astSscanf_( const char *str, const char *format, ...);
size_t astSizeOf_( void * );
void *astFree_( void * );
void *astGrow_( void *, int, size_t );
void *astMalloc_( size_t );
void *astRealloc_( void *, size_t );
void *astStore_( void *, const void *, size_t );
size_t astChrLen_( const char * );
char *astAppendString_( char *, int *, const char * );

#ifdef DEBUG
void astListIssued_( const char *label );
void astIdHandler_( void *, const char * );
void astSetWatchId_( int id );
int astSetPermMem_( int perm );
int astGetMemId_( void * );
void astMemCheckId_( int, void (*)( void *ptr ) );
void *astFindIdPtr_( int );
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
#define astRealloc(ptr,size) astRealloc_(ptr,size)
#define astSizeOf(ptr) astSizeOf_(ptr)
#define astStore(ptr,data,size) astStore_(ptr,data,size)
#define astAppendString(ptr,len,text) astAppendString_(ptr,len,text)
#define astString(chars,nchars) astString_(chars,nchars)
#define astStringArray(chars,nel,len) astStringArray_(chars,nel,len)
#define astChrLen(string) astChrLen_(string)
#define astSscanf astSscanf_
#endif

#ifdef DEBUG
#define astListIssued(label) astListIssued_(label)
#define astSetWatchId(id) astSetWatchId_(id)
#define astIdHandler(mem,text) astIdHandler_(mem,text)
#define astSetPermMem(perm) astSetPermMem_(perm)
#define astGetMemId(ptr) astGetMemId_(ptr)
#define astFindIdPtr(id) astFindIdPtr_(id)
#define astMemCheckId(id,fun) astMemCheckId_(id,fun)
#endif

#endif
