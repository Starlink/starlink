/*
*+
*  Name:
*     extreme.h
*
*  Purpose:
*     Macros for EXTREME package.
*
*  Language:
*     C
*
*  Type of module:
*     C include file.
*
*  Description:
*     This file may be included in C code which uses the INT_BIG identifier
*     as an integer type.  It defines the value of INT_BIG if it is not
*     already defined (e.g. on the compiler command line) as `int', and
*     if the system include file limits.h has already been included it 
*     sets up constants for the maximum and minimum values of the
*     resulting integral types.
*
*  Author:
*     MBT: Mark Taylor (Starlink)
*
*  History:
*     3-FEB-2000 (MBT):
*        Original version.
*-
*/

/* Define the INT_BIG macro if it has not already been defined. */
#ifdef INT_BIG
#else
#define INT_BIG int
#endif

/* If the system header file limits.h has been included, define new maximum
   and minimum constants INT_BIG_MAX, INT_BIG_MIN and UINT_BIG_MAX as 
   appropriate. */
#ifdef INT_MAX

#define int 1
#define long 2

#if INT_BIG == int
#define INT_BIG_MAX INT_MAX
#define INT_BIG_MIN INT_MIN
#define UINT_BIG_MAX UINT_MAX
#elif INT_BIG == long
#define INT_BIG_MAX LONG_MAX
#define INT_BIG_MIN LONG_MIN
#define UINT_BIG_MAX ULONG_MAX
#endif

#undef int
#undef long

#endif

/* $Id$ */
