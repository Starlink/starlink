/*+
 *  Name:
 *     SAE_PAR.H

 *  Purpose:
 *     Define the Starlink ADAM Environment public constants.

 *  Language:
 *     Starlink ANSI C

 *  Type of module:
 *     Global constants header file.

 *  Description:
 *     This file defines the ADAM status values for non-ADAM users.

 *  Authors:
 *     PCTR: P.C.T. Rees (STARLINK)
 *     TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *     12-JUN-1990 (PCTR):
 *        Original version.
 *     23-SEP-2005 (TIMJ):
 *        Use proper constants rather than the C preprocessor
 *     25-SEP-2005 (TIMJ):
 *        Prevent the constants being defined multiple times if
 *        the file is included multiple times.

 *  Bugs:

 *- */

#ifndef SAE_PAR_DEFINED
#define SAE_PAR_DEFINED

/* OK Status. */
enum     { SAI__OK = 0 }; 

/* Warning. */
enum     { SAI__WARN  = 148013859 };
 
/* Error. */
enum     { SAI__ERROR = 148013867 };

#endif  /* SAE_PAR_DEFINED */
