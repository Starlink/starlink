/*
*class++
*  Name:
*     Axis

*  Purpose:
*     Store axis information.

*  Constructor Function:
*     None.

*  Description:
*     The Axis class is used to store information associated with a
*     particular axis of a Frame. It is used internally by the AST
*     library and has no constructor function. You should encounter it
c     only within textual output (e.g. from astWrite).
f     only within textual output (e.g. from AST_WRITE).

*  Inheritance:
*     The Axis class inherits from the Object class.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: B.S. Berry (Starlink)

*  History:
*     1-MAR-1996 (RFWS):
*        Original version.
*     10-SEP-1996 (RFWS):
*        Added I/O facilities.
*     11-SEP-1996 (RFWS):
*        Added astAxisGap (written by DSB).
*     25-FEB-1998 (RFWS):
*        Added astAxisUnformat.
*     29-AUG-2001 (DSB):
*        Added AxisDistance and AxisOffset.
*     20-OCT-2002 (DSB):
*        Added Top and Bottom attributes.
*     8-JAN-2003 (DSB):
*        - Changed private InitVtab method to protected astInitAxisVtab
*        method.
*        - Include descriptive label for units string within a Dump.
*     24-JAN-2004 (DSB):
*        - Added astAxisFields.
*        - Added argument "fmt" to definition of AxisAbbrev.
*     3-FEB-2004 (DSB):
*        - Added "log" formatting using the "&" flag character in the
*        Format string.
*     15-SEP-2004 (DSB):
*        - If a format string is set which includes a wildcard precision
*        value (".*"), then use the Digits value to determine the precision
*        to be used.
*        - If the conversion code is of integer type (e.g. "%d") cast value
*        to integer before printing.
*     2-FEB-2005 (DSB):
*        - Avoid using astStore to allocate more storage than is supplied
*        in the "data" pointer. This can cause access violations since
*        astStore will then read beyond the end of the "data" area.
*     15-MAR-2005 (DSB):
*        - Avoid exponents in log format labels which are close to zero but
*        not quite zero.
*     14-FEB-2006 (DSB):
*        Override astGetObjSize.
*     30-JUN-2006 (DSB):
*        Guard against a null "str1" value in AxisAbbrev.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Axis


/* Header files. */
/* ============= */
#include "ast_err.h"             /* Error code definitions */

/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Object interface (parent class) */
#include "pointset.h"            /* Sets of coordinates (for AST__BAD) */
#include "channel.h"             /* I/O channels */
#include "axis.h"                /* Interface definition for this class */
#include "unit.h"                /* Definitions of physical units */
#include "globals.h"             /* Thread-safe global data access */

/* C header files. */
/* --------------- */
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are extended by this class. */
static int (* parent_getobjsize)( AstObject *, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );

/* Plain text equivalents. */
static const char *log_txt  = "10^";

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetDefaultFormat_Buff[ 0 ] = 0; \
   globals->AxisFormat_Buff[ 0 ] = 0; \
   globals->GetAxisNormUnit_Buff[ 0 ] = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Axis)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Axis,Class_Init)
#define class_vtab astGLOBAL(Axis,Class_Vtab)
#define getdefaultformat_buff astGLOBAL(Axis,GetDefaultFormat_Buff)
#define axisformat_buff astGLOBAL(Axis,AxisFormat_Buff)
#define getaxisnormunit_buff astGLOBAL(Axis,GetAxisNormUnit_Buff)
#define getattrib_buff astGLOBAL(Axis,GetAttrib_Buff)



/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getdefaultformat_buff[ AST__AXIS_GETDEFAULTFORMAT_BUFF_LEN + 1 ];
static char axisformat_buff[ AST__AXIS_GETDEFAULTFORMAT_BUFF_LEN + 1 ];
static char getaxisnormunit_buff[ AST__AXIS_GETAXISNORMUNIT_BUFF_LEN + 1 ];
static char getattrib_buff[ AST__AXIS_GETATTRIB_BUFF_LEN + 1 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstAxisVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#endif

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstAxis *astAxisId_( const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static const char *AxisAbbrev( AstAxis *, const char *, const char *, const char *, int * );
static const char *AxisFormat( AstAxis *, double, int * );
static int GetObjSize( AstObject *, int * );
static const char *GetAttrib( AstObject *, const char *, int * );
static const char *GetAxisFormat( AstAxis *, int * );
static const char *GetAxisLabel( AstAxis *, int * );
static const char *GetAxisSymbol( AstAxis *, int * );
static const char *GetAxisUnit( AstAxis *, int * );
static const char *GetAxisNormUnit( AstAxis *, int * );
static const char *GetDefaultFormat( AstAxis *, int * );
static char *ParseAxisFormat( const char *, int, int *, int *, int *, int *, int * );
static double AxisDistance( AstAxis *, double, double, int * );
static double AxisGap( AstAxis *, double, int *, int * );
static double AxisOffset( AstAxis *, double, double, int * );
static int AxisFields( AstAxis *, const char *, const char *, int, char **, int *, double *, int * );
static int AxisIn( AstAxis *, double, double, double, int, int * );
static int AxisUnformat( AstAxis *, const char *, double *, int * );
static int GetAxisDigits( AstAxis *, int * );
static int GetAxisDirection( AstAxis *, int * );
static int TestAttrib( AstObject *, const char *, int * );
static int TestAxisDigits( AstAxis *, int * );
static int TestAxisDirection( AstAxis *, int * );
static int TestAxisFormat( AstAxis *, int * );
static int TestAxisLabel( AstAxis *, int * );
static int TestAxisSymbol( AstAxis *, int * );
static int TestAxisUnit( AstAxis *, int * );
static int TestAxisNormUnit( AstAxis *, int * );
static void AxisNorm( AstAxis *, double *, int * );
static void AxisOverlay( AstAxis *, AstAxis *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void ClearAxisDigits( AstAxis *, int * );
static void ClearAxisDirection( AstAxis *, int * );
static void ClearAxisFormat( AstAxis *, int * );
static void ClearAxisLabel( AstAxis *, int * );
static void ClearAxisSymbol( AstAxis *, int * );
static void ClearAxisUnit( AstAxis *, int * );
static void Copy( const AstObject *, AstObject *, int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void SetAttrib( AstObject *, const char *, int * );
static void SetAxisDigits( AstAxis *, int, int * );
static void SetAxisDirection( AstAxis *, int, int * );
static void SetAxisFormat( AstAxis *, const char *, int * );
static void SetAxisLabel( AstAxis *, const char *, int * );
static void SetAxisSymbol( AstAxis *, const char *, int * );
static void SetAxisUnit( AstAxis *, const char *, int * );

static double GetAxisTop( AstAxis *, int * );
static int TestAxisTop( AstAxis *, int * );
static void ClearAxisTop( AstAxis *, int * );
static void SetAxisTop( AstAxis *, double, int * );

static double GetAxisBottom( AstAxis *, int * );
static int TestAxisBottom( AstAxis *, int * );
static void ClearAxisBottom( AstAxis *, int * );
static void SetAxisBottom( AstAxis *, double, int * );


/* Member functions. */
/* ================= */
static const char *AxisAbbrev( AstAxis *this, const char *fmt,
                               const char *str1, const char *str2, int *status ) {
/*
*+
*  Name:
*     astAxisAbbrev

*  Purpose:
*     Abbreviate a formatted Axis value by skipping leading fields.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "axis.h"
*     const char *astAxisAbbrev( AstAxis *this, const char *fmt,
*                                const char *str1, const char *str2 )

*  Class Membership:
*     Axis method.

*  Description:
*     This function compares two Axis values that have been formatted
*     (using astAxisFormat) and determines if they have any redundant
*     leading fields (i.e. leading fields in common which can be
*     suppressed when tabulating the values or plotting them on the
*     axis of a graph).

*  Parameters:
*     this
*        Pointer to the Axis.
*     fmt
*        Pointer to a constant null-terminated string containing the
*        format specifier used to format the two values.
*     str1
*        Pointer to a constant null-terminated string containing the
*        first formatted value. If this is null, the returned pointer
*        points to the start of the final field in str2.
*     str2
*        Pointer to a constant null-terminated string containing the
*        second formatted value.

*  Returned Value:
*     A pointer into the "str2" string which locates the first
*     character in the first field that differs between the two
*     formatted values.
*
*     If the two values have no leading fields in common, the returned
*     value will point at the start of string "str2". If the two
*     values are equal, it will point at the terminating null at the
*     end of this string.

*  Notes:
*     - This function assumes that the format specification used was
*     the same when both values were formatted.
*     - A pointer to the start of "str2" will be returned if this
*     function is invoked with the global error status set, or if it
*     should fail for any reason.
*-
*/

/* Local Variables: */
   const char *result;           /* Result pointer to return */

/* Initialise. */
   result = str2;

/* Check the global error status. */
   if ( !astOK ) return result;

/* In the Axis class, there is only one field in a formatted value.
   We return the value of "str2", unless the two strings are
   identical, in which case we return a pointer to the final null in
   "str2". */
   if( str1 && !strcmp( str1, str2 ) ) result += strlen( str2 );

/* Return the result. */
   return result;
}

static double AxisDistance( AstAxis *this, double v1, double v2, int *status ) {
/*
*+
*  Name:
*     astAxisDistance

*  Purpose:
*     Find the distance between two axis values.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "axis.h"
*     AxisDistance( AstAxis *this, double v1, double v2 )

*  Class Membership:
*     Axis method.

*  Description:
*     This function returns a signed value representing the axis increment
*     from axis value v1 to axis value v2.
*
*     For a simple Axis, this is a trivial operation. But for other
*     derived classes of Axis (such as a SkyAxis) this is not the case.

*  Parameters:
*     this
*        Pointer to the Axis.
*     v1
*        The first axis value
*     v2
*        The second axis value

*  Returned Value:
*     The axis increment from v1 to v2.

*  Notes:
*     - A value of AST__BAD is returned if either axis value is AST__BAD.
*     - A value of AST__BAD will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   double result;                /* Returned gap size */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check both axis values are OK, and form the returned increment. */
   if( v1 != AST__BAD && v2 != AST__BAD ) result = v2 - v1;

/* Return the result. */
   return result;
}

static int AxisFields( AstAxis *this, const char *fmt0, const char *str,
                       int maxfld, char **fields, int *nc, double *val, int *status ) {
/*
*+
*  Name:
*     astAxisFields

*  Purpose:
*     Identify numerical fields within a formatted Axis value.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "axis.h"
*     int astAxisFields( AstAxis *this, const char *fmt0, const char *str,
*                        int maxfld, char **fields, int *nc, double *val )

*  Class Membership:
*     Axis member function.

*  Description:
*     This function identifies the numerical fields within an Axis value
*     that have been formatted using astAxisFormat. It assumes that the
*     value was formatted using the supplied format string. It also
*     returns the equivalent floating point value.

*  Parameters:
*     this
*        Pointer to the Axis.
*     fmt0
*        Pointer to a constant null-terminated string containing the
*        format used when creating "str".
*     str
*        Pointer to a constant null-terminated string containing the
*        formatted value.
*     maxfld
*        The maximum number of fields to identify within "str".
*     fields
*        A pointer to an array of at least "maxfld" character pointers.
*        Each element is returned holding a pointer to the start of the
*        corresponding field  in "str" (in the order in which they occur
*        within "str"), or NULL if no corresponding field can be found.
*     nc
*        A pointer to an array of at least "maxfld" integers. Each
*        element is returned holding the number of characters in the
*        corresponding field, or zero if no corresponding field can be
*        found.
*     val
*        Pointer to a location at which to store the value
*        equivalent to the returned field values. If this is NULL,
*        it is ignored.

*  Returned Value:
*     The number of fields succesfully identified and returned.

*  Notes:
*     - Leading and trailing spaces are ignored.
*     - If the formatted value is not consistent with the supplied format
*     string, then a value of zero will be returned, "fields" will be
*     returned holding NULLs, "nc" will be returned holding zeros, and
*     "val" is returned holding VAL__BAD.
*     - Fields are counted from the start of the formatted string. If the
*     string contains more than "maxfld" fields, then trailing fields are
*     ignored.
*     - If this function is invoked with the global error status set, or
*     if it should fail for any reason, then a value of zero will be returned
*     as the function value, and "fields", "nc" and "val"  will be returned
*     holding their supplied values
*-
*/

/* Local Variables: */
   char log_esc[ 50 ];           /* Buffer for graphical delimiter string */
   const char *fmt;              /* Pointer to parsed Format string */
   const char *log_del;          /* Pointer to delimiter string */
   const char *p;                /* Pointer to next character */
   double value;                 /* Equivalent radians value */
   int ifld;                     /* Field index */
   int integ;                    /* Cast axis value to integer before printing? */
   int len;                      /* Length of formatted string */
   int log;                      /* Format as "10**x"? */
   int n;                        /* Number of characters read */
   int neg;                      /* Negate final value? */
   int result;                   /* Result fields count to return */
   int sign;                     /* Include leading sign in front of "10**x"? */
   int space;                    /* Include leading space in front of "10**x"? */

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise. */
   result = 0;
   for( ifld = 0; ifld < maxfld; ifld++ ) {
      fields[ ifld ] = NULL;
      nc[ ifld ] = 0;
   }
   if( val ) *val = AST__BAD;

/* Parse the Format string. This returns a collection of flags indicating
   if any AST specific formatting features are specified in the Format
   string. It also returns a pointer to a new Format string which is a
   standard C printf format specifier. Currently the only flags are "log"
   which indicates if the value should be formatted as "10**x" using
   the graphical escape sequences defined within the Plot class to produce
   "x" as a superscript of "10", "sign" which is used with log to indicate
   if a sign should always be included infront of the "10", and "space"
   which indicates if a leading space should be included infronyt of "10" if
   no sign is included. */
   fmt = ParseAxisFormat( fmt0, astGetAxisDigits( this ), &log, &sign,
                          &space, &integ, status );
   fmt = astFree( (void *) fmt );

   if( astOK ) {

/* Obtain the length of the formatted string. */
      len = (int) strlen( str );

/* First deal with "log" format. */
      if( log ) {

/* We need room for at least 2 fields. */
         if( maxfld > 1 ) {

/* Return a pointer to the first non-blank character. */
            p = str;
            while( *p == ' ' ) p++;
            fields[ 0 ] = (char *) p;

/* If the first non-blank character is a minus sign, note it and skip it. */
            neg = 0;
            if( *p == '-' ) {
               neg = 1;
               p++;

/* If the first non-blank character is a plus sign, and skip it. */
            } else if( *p == '+' ) {
               p++;
            }

/* Select the delimter.*/
            if(  astEscapes( -1 ) ) {
               astTuneC( "exdel", NULL, log_esc, sizeof( log_esc ) );
               log_del = log_esc;
            } else {
               log_del = log_txt;
            }

/* Check the remaining string starts with the correct delimiter. If
   so, store the number of characters in the first field and skip over the
   delimiter. */
            n = 0;
            if( strstr( p, log_del ) == p ) {
               nc[ 0 ] = p + 2  - fields[ 0 ];
               p += strlen( log_del );

/* Attempt to read a floating point value from the start of the remaining
   string. */
               if( 1 == sscanf( p, "%lg%n", &value, &n ) ) {

/* If succesfull, store the returned values. */
                  result = 2;
                  fields[ 1 ] = (char *) p;
                  nc[ 1 ] = n;
                  if( val ) {
                     *val = pow( 10.0, value );
                     if( neg ) *val = -(*val);
                  }

/* Otherwise, see if the string starts with <bad> */
               } else if( strstr( p, "<bad>" ) == p ) {

/* If succesfull, store the returned values. */
                  result = 2;
                  fields[ 1 ] = (char *) p;
                  nc[ 1 ] = 5;
                  if( val ) *val = 0.0;
               }

/* Zero is never formatted as an exponent. If the string starts with zero,
   return a single zero field. */
            } else if( 1 == sscanf( p, "%lg%n", &value, &n ) ) {
               if( value == 0.0 ) {
                  result = 1;
                  nc[ 0 ] = p + n  - fields[ 0 ];
                  if( val ) *val = 0.0;
               }
            }
         }

/* Now deal with normal decimal format */
      } else {

/* Attempt to read a floating point value from the formatted string. */
         if ( n = 0,
              ( 1 == sscanf( str, "%lg %n", &value, &n ) )
              && ( n >= len ) && maxfld > 0 ) {

/* If succesful, return a pointer to the first non-blank character. */
            p = str;
            while( *p == ' ' ) p++;
            fields[ 0 ] = (char *) p;

/* Find the last non-blank character. */
            p += len;
            while( p[ -1 ] == ' ' ) p--;

/* Return the number of characters in the field. */
            nc[ 0 ] = p - fields[ 0 ];

/* Return the field value. */
            if( val ) *val = value;

/* Indicate that we are returning one field. */
            result = 1;
         }
      }
   }

/* Return the result. */
   return result;
}

static const char *AxisFormat( AstAxis *this, double value, int *status ) {
/*
*+
*  Name:
*     astAxisFormat

*  Purpose:
*     Format a coordinate value for an Axis.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "axis.h"
*     const char *astAxisFormat( AstAxis *this, double value )

*  Class Membership:
*     Axis method.

*  Description:
*     This function returns a pointer to a string containing the formatted
*     (character) version of a coordinate value for an Axis. The formatting
*     applied is that specified by a previous invocation of the
*     astSetAxisFormat method. A suitable default format is applied if
*     necessary.

*  Parameters:
*     this
*        Pointer to the Axis.
*     value
*        The coordinate value to be formatted.

*  Returned Value:
*     A pointer to a null-terminated string containing the formatted value.

*  Notes:
*     -  The returned string pointer may point at memory allocated within
*     the Axis object, or at static memory. The contents of the string may be
*     over-written or the pointer may become invalid following a further
*     invocation of the same function or deletion of the Axis. A copy of the
*     string should therefore be made if necessary.
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Constants: */
#define ERRBUF_LEN 80

/* Local Variables: */
   astDECLARE_GLOBALS           /* Pointer to thread-specific global data */
   char *errstat;               /* Pointer for system error message */
   char errbuf[ ERRBUF_LEN ];   /* Buffer for system error message */
   char log_esc[ 50 ];          /* Buffer for graphical delimiter string */
   const char *fmt0;            /* Pointer to original Format string */
   const char *fmt;             /* Pointer to parsed Format string */
   const char *log_del;         /* Pointer to delimiter string */
   const char *result;          /* Pointer to formatted value */
   double x;                    /* The value to be formatted by sprintf */
   int integ;                   /* Cast axis value to integer before printing? */
   int log;                     /* Format as "10**x"? */
   int nc;                      /* Total number of characters written */
   int ncc;                     /* Number of characters written */
   int sign;                    /* Include leading sign in front of "10**x"? */
   int space;                   /* Include leading space in front of "10**x"? */
   int stat;                    /* Value of errno after error */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this);

/* Initialise. */
   result = NULL;
   nc = 0;
   x = value;

/* Check if a bad coordinate value was supplied and return a pointer to an
   appropriate string if necessary. */
   if ( value == AST__BAD ) {
      result = "<bad>";

/* Otherwise, obtain a pointer to the Format string. Note a private member
   function is used here in preference to an object method. This is because the
   syntax of the Format string may be extended by derived classes and we do not
   want to obtain a string that we cannot interpret here (where we are
   restricted to C format specifiers capable of formatting double values).
   Classes that extend the syntax should provide their own astAxisFormat method
   and may need to store the string in a separate location. The original
   location should not be re-used as the string it contains may be needed by
   the Axis astOverlay method when overlaying attributes on to another Axis
   object. */
   } else {
      fmt0 = GetAxisFormat( this, status );

/* Parse the Format string. This returns a collection of flags indicating
   if any AST specific formatting features are specified in the Format
   string. It also returns a pointer to a new Format string which is a
   standard C printf format specifier. Currently the only flags are "log"
   which indicates if the value should be formatted as "10**x" using
   the graphical escape sequences defined within the Plot class to produce
   "x" as a superscript of "10", "sign" which is used with log to indicate
   if a sign should always be included infront of the "10", and "space"
   which indicates if a leading space should be included infronyt of "10"
   if no sign is included. It also modifies ".*" precision fields by
   replacing the "*" by the current vale of the Digits attribute. */
      fmt = ParseAxisFormat( fmt0, astGetAxisDigits( this ), &log, &sign,
                             &space, &integ, status );
      if( astOK ) {

/* Format zero normally. */
         if( value == 0.0 ) log = 0;

/* If log format is required, find the value of the exponent "x", and
   initialise the returned string to hold the exponent and the graphical
   escape sequence which produces a superscript. Otherwise just format the
   supplied value. */
         if( log ) {

            if( sign ) {
               axisformat_buff[ 0 ] ='+';
               nc = 1;

            } else if( space ) {
               axisformat_buff[ 0 ] =' ';
               nc = 1;
            }

            if( value > 0 ) {
               x = log10( integ ? (int) value : value );

            } else {
               x = log10( integ ? (int) -value : -value );
               axisformat_buff[ 0 ] ='-';
               nc = 1;
            }

            if(  astEscapes( -1 ) ) {
               astTuneC( "exdel", NULL, log_esc, sizeof( log_esc ) );
               log_del = log_esc;
            } else {
               log_del = log_txt;
            }

            nc += sprintf( axisformat_buff + nc, "%s", log_del );

/* Round small exponents to zero. */
            if( fabs( x ) < 1.0E-10 ) x = 0.0;
         }
      }

/* Clear errno and attempt to format the value as if the Format string were
   a standard "sprintf" format. */
      if ( astOK ) {
         errno = 0;
         if( integ ) {
            ncc = sprintf( axisformat_buff + nc, fmt, (int) x );
         } else {
            ncc = sprintf( axisformat_buff + nc, fmt, x );
         }
         nc += ncc;

/* If log format is being used, terminate the string with an escape
   sequence which resets the graphical attributes to what they were at the
   start of the string. */
         if( log ) nc += sprintf( axisformat_buff + nc, "%%+" );

/* The possibilities for error detection are limited here, but check if an
   error value was returned and report an error. Include information from
   errno if it was set. */
         if ( ncc < 0 ) {
            stat = errno;
            if( stat ) {
#if HAVE_STRERROR_R
               strerror_r( stat, errbuf, ERRBUF_LEN );
               errstat = errbuf;
#else
               errstat = strerror( stat );
#endif
            } else {
               *errbuf = 0;
               errstat = errbuf;
            }
            astError( AST__FMTER, "astAxisFormat(%s): Error formatting a "
                      "coordinate value of %1.*G%s%s.", status, astGetClass( this ),
                      DBL_DIG, value, stat? " - " : "", errstat );
            astError( AST__FMTER, "The format string was \"%s\".", status, fmt );

/* Also check that the result buffer did not overflow. If it did, memory will
   probably have been corrupted but this cannot be prevented with "sprintf".
   Report the error and abort. */
         } else if ( nc > AST__AXIS_AXISFORMAT_BUFF_LEN ) {
            astError( AST__FMTER, "astAxisFormat(%s): Internal buffer "
                      "overflow while formatting a coordinate value of %1.*G "
                      "- result exceeds %d characters.", status, astGetClass( this ),
                      DBL_DIG, value, AST__AXIS_AXISFORMAT_BUFF_LEN );
            astError( AST__FMTER, "The format string was \"%s\".", status, fmt );

/* If succesfull, return a pointer to the buffer. */
         } else {
            result = axisformat_buff;
         }
      }

/* Free resources. */
      fmt = astFree( (void *) fmt );

   }

/* Return the result. */
   return result;

}
#undef ERRBUF_LEN

static double AxisGap( AstAxis *this, double gap, int *ntick, int *status ) {
/*
*+
*  Name:
*     astAxisGap

*  Purpose:
*     Find a "nice" gap for tabulating Axis values.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "axis.h"
*     double astAxisGap( AstAxis *this, double gap, int *ntick )

*  Class Membership:
*     Axis method.

*  Description:
*     This function returns a gap size which produces a nicely spaced
*     series of formatted Axis values, the returned gap size being as
*     close as possible to the supplied target gap size. It also
*     returns a convenient number of divisions into which the gap can
*     be divided.

*  Parameters:
*     this
*        Pointer to the Axis.
*     gap
*        The target gap size.
*     ntick
*        Address of an int in which to return a convenient number of
*        divisions into which the gap can be divided.

*  Returned Value:
*     The nice gap size.

*  Notes:
*     - A value of zero is returned if the supplied gap size is zero.
*     - A negative gap size is returned if the supplied gap size is negative.
*     - A value of zero will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   double absgap;                /* Absolute supplied gap size */
   double b;                     /* Decimal step size */
   double result;                /* Returned gap size */
   int index;                    /* Index into tables */
   int positive;                 /* Value is positive (or zero)? */

/* Local Data: */
   static double table1[ 10 ] =  /* Table of nice decimal gaps */
            { 1.0, 2.0, 2.0, 5.0, 5.0, 5.0, 5.0, 10.0, 10.0, 10.0 };
   static int table2[ 10 ] =     /* Table giving number of divisions */
            {   5,   4,   4,   5,   5,   5,   5,    5,    5,    5 };

/* Initialise. */
   result = 0.0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check that the supplied gap size is not zero. */
   if ( gap != 0.0 ) {

/* Determine if the supplied gap size is positive and obtain its
   absolute value. */
      positive = ( gap >= 0.0 );
      absgap = positive ? gap : -gap;

/* Obtain a value which has a 1 at the position of the most
   significant decimal digit in the target gap size and zeros at all
   other positions. */
      b = pow( 10.0, floor( log10( absgap ) ) );

/* This value is the basic "step size". Find the nearest whole number
   of steps in the supplied gap, and then use the look-up-table in
   "table1" to find the closest acceptable gap size. Convert this gap
   size back to an absolute value by multiplying by the step size. */
      index = (int) ( absgap / b + 0.5 ) - 1;
      result = b * table1[ index ];

/* If the target gap was negative, negate the result. */
      if( !positive ) result = -result;

/* Store the number of divisions in the gap. */
      if ( ntick ) *ntick = table2[ index ];
   }

/* Return the result. */
   return result;
}

static int AxisIn( AstAxis *this, double lo, double hi, double val, int closed, int *status ){
/*
*+
*  Name:
*     astAxisIn

*  Purpose:
*     Test if an axis value lies within a given interval.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "axis.h"
*     int AxisIn( AstAxis *this, double lo, double hi, double val, int closed )

*  Class Membership:
*     Axis member function.

*  Description:
*     This function returns non-zero if a given axis values lies within a
*     given axis interval.

*  Parameters:
*     this
*        Pointer to the Axis.
*     lo
*        The lower axis limit of the interval.
*     hi
*        The upper axis limit of the interval.
*     val
*        The axis value to be tested.
*     closed
*        If non-zero, then the lo and hi axis values are themselves
*        considered to be within the interval. Otherwise they are outside.

*  Returned Value:
*     Non-zero if the test value is inside the interval.

*  Class Applicability:
*     Axis
*        Uses simple Euclidean test
*     SkyAxis
*        All angles which are numerically between "lo" and "hi" are within
*        the interval. Angle outside this range are also within the interval
*        if they can be brought into the range by addition or subtraction
*        of a multiple of 2.PI.
*-
*/

/* For speed, omit the astOK check since no pointers are being used. */
   if( closed ) {
      return ( lo <= val && val <= hi );
   } else {
      return ( lo < val && val < hi );
   }
}

static void AxisNorm( AstAxis *this, double *value, int *status ) {
/*
*+
*  Name:
*     astAxisNorm

*  Purpose:
*     Normalise an Axis coordinate value.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "axis.h"
*     void astAxisNorm( AstAxis *this, double *value )

*  Class Membership:
*     Axis method.

*  Description:
*     This function converts an Axis coordinate value which might
*     potentially be unsuitable for display to a user (for instance,
*     may lie outside the expected range of values) into an acceptable
*     alternative value suitable for display.
*
*     Typically, for axes that represent cyclic values such as angles,
*     this function wraps an arbitrary coordinate value so that it
*     lies within the first cycle (say zero to 2*pi). For an ordinary
*     linear Axis, without constraints, this function will typically
*     return the original value unchanged.

*  Parameters:
*     this
*        Pointer to the Axis.
*     value
*        Pointer to the coordinate value to be normalised, which will
*        be modified in place.
*-
*/

/* In the Axis class there are no constraints, so simply return
   without action. */
   return;
}

static double AxisOffset( AstAxis *this, double v1, double dist, int *status ) {
/*
*+
*  Name:
*     astAxisOffset

*  Purpose:
*     Add an increment onto a supplied axis value.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "axis.h"
*     AxisOffset( AstAxis *this, double v1, double dist )

*  Class Membership:
*     Axis method.

*  Description:
*     This function returns an axis value formed by adding a signed axis
*     increment onto a supplied axis value.
*
*     For a simple Axis, this is a trivial operation. But for other
*     derived classes of Axis (such as a SkyAxis) this is not the case.

*  Parameters:
*     this
*        Pointer to the Axis.
*     v1
*        The supplied axis value
*     dist
*        The axis increment

*  Returned Value:
*     The axis value which is the specified increment away from v1.

*  Notes:
*     - A value of AST__BAD is returned if either axis value is AST__BAD.
*     - A value of AST__BAD will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   double result;                /* Returned gap size */

/* Initialise. */
   result = AST__BAD;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Check both axis values are OK, and form the returned axis value. */
   if( v1 != AST__BAD && dist != AST__BAD ) result = v1 + dist;

/* Return the result. */
   return result;
}

static void AxisOverlay( AstAxis *template, AstAxis *result, int *status ) {
/*
*+
*  Name:
*     astAxisOverlay

*  Purpose:
*     Overlay the attributes of a template Axis on to another Axis.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "axis.h"
*     void astAxisOverlay( AstAxis *template, AstAxis *result )

*  Class Membership:
*     Axis method.

*  Description:
*     This function overlays attributes of one Axis (the "template") on to
*     another Axis, so as to over-ride selected attributes of that second
*     Axis. Normally only those attributes which have been specifically set
*     in the template will be transferred. This implements a form of
*     defaulting, in which an Axis acquires attributes from the template, but
*     retains its original attributes (as the default) if new values have not
*     previously been explicitly set in the template.

*  Parameters:
*     template
*        Pointer to the template Axis, for which values should have been
*        explicitly set for any attribute which is to be transferred.
*     result
*        Pointer to the Axis which is to receive the new attribute values.

*  Returned Value:
*     void
*-
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Define a macro to overlay a single attribute. This tests if the attribute
   is set in the template Axis. If it is, its value is obtained and set in the
   result Axis also. */
#define OVERLAY(par) \
   if ( astTestAxis##par( template ) ) { \
      astSetAxis##par( result, astGetAxis##par( template ) ); \
   }
/* Overlay each Axis attribute in turn. */
   OVERLAY(Digits);
   OVERLAY(Direction);
   OVERLAY(Label);
   OVERLAY(Symbol);
   OVERLAY(Unit);

/* Handle the Format string slightly differently by using a private member
   function to obtain it. This is necessary in case derived classes have
   extended the string syntax (see the AxisFormat function for more
   details). */
   if ( TestAxisFormat( template, status ) ) {
      SetAxisFormat( result, GetAxisFormat( template, status ), status );
   }

/* Undefine macros local to this function. */
#undef OVERLAY
}

static int AxisUnformat( AstAxis *this, const char *string, double *value, int *status ) {
/*
*+
*  Name:
*     astAxisUnformat

*  Purpose:
*     Read a formatted coordinate value for an Axis.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "axis.h"
*     int astAxisUnformat( AstAxis *this, const char *string, double *value )

*  Class Membership:
*     Axis method.

*  Description:
*     This function reads a formatted coordinate value for an Axis
*     (supplied as a string) and returns the equivalent numerical
*     value as a double. It also returns the number of characters read
*     from the string.

*  Parameters:
*     this
*        Pointer to the Axis.
*     string
*        Pointer to a constant null-terminated string containing the
*        formatted coordinate value.
*     value
*        Pointer to a double in which the coordinate value read will be
*        returned.

*  Returned Value:
*     The number of characters read from the string to obtain the
*     coordinate value.

*  Notes:
*     - Any white space at the beginning of the string will be
*     skipped, as also will any trailing white space following the
*     coordinate value read. The function's return value will reflect
*     this.
*     - A function value of zero (and no coordinate value) will be
*     returned, without error, if the string supplied does not contain
*     a suitably formatted value.
*     - The string "<bad>" is recognised as a special case and will
*     generate the value AST__BAD, without error. The test for this
*     string is case-insensitive and permits embedded white space.
*     - A function result of zero will be returned and no coordinate
*     value will be returned via the "value" pointer if this function
*     is invoked with the global error status set, or if it should
*     fail for any reason.
*-
*/

/* Local Variables: */
   double coord;                 /* Coordinate value read */
   int nc;                       /* Number of characters read */

/* Initialise. */
   nc = 0;

/* Check the global error status. */
   if ( !astOK ) return nc;

/* See if the string can be read as a floating point number. If so,
   return its value. Also obtain the number of characters read,
   including any leading and trailing white space. */
   if ( 1 == astSscanf( string, "%lf %n", &coord, &nc ) ) {
      *value = coord;

/* Otherwise, see if the string starts with "<bad>", allowing mixed
   case and leading, embedded and trailing white space. If so, return
   the value AST__BAD. */
   } else if ( nc = 0,
               ( 0 == astSscanf( string, " < %*1[Bb] %*1[Aa] %*1[Dd] > %n", &nc )
               && ( nc > 0 ) ) ) {
      *value = AST__BAD;

/* If the string cannot be read, return a function result of zero. */
   } else {
      nc = 0;
   }

/* Return the number of characters read. */
   return nc;
}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     void ClearAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Axis member function (over-rides the astClearAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function clears the value of a specified attribute for an
*     Axis, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the Axis.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstAxis *this;                /* Pointer to the Axis structure */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Axis structure. */
   this = (AstAxis *) this_object;

/* Check the attribute name and clear the appropriate attribute. */

/* Digits. */
/* ------- */
   if ( !strcmp( attrib, "digits" ) ) {
      astClearAxisDigits( this );

/* Direction. */
/* ---------- */
   } else if ( !strcmp( attrib, "direction" ) ) {
      astClearAxisDirection( this );

/* Format. */
/* ------- */
   } else if ( !strcmp( attrib, "format" ) ) {
      astClearAxisFormat( this );

/* Label. */
/* ------ */
   } else if ( !strcmp( attrib, "label" ) ) {
      astClearAxisLabel( this );

/* Top. */
/* ---- */
   } else if ( !strcmp( attrib, "top" ) ) {
      astClearAxisTop( this );

/* Bottom. */
/* ------- */
   } else if ( !strcmp( attrib, "bottom" ) ) {
      astClearAxisBottom( this );

/* Symbol. */
/* ------- */
   } else if ( !strcmp( attrib, "symbol" ) ) {
      astClearAxisSymbol( this );

/* Unit. */
/* ----- */
   } else if ( !strcmp( attrib, "unit" ) ) {
      astClearAxisUnit( this );

/* Read-only attributes. */
/* --------------------- */
/* Test if the attribute name matches any of the read-only attributes
   of this class. If it does, then report an error. */
   } else if ( !strcmp( attrib, "normunit" ) ) {
      astError( AST__NOWRT, "astClear: Invalid attempt to clear the \"%s\" "
                "value for a %s.", status, attrib, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static const char *GetAxisNormUnit( AstAxis *this, int *status ){
/*
*+
*  Name:
*     astGetAxisNormUnit

*  Purpose:
*     Return the normalised Unit attribute for an Axis.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "axis.h"
*     const char *astGetAxisNormUnit( AstAxis *this ){

*  Class Membership:
*     Axis method.

*  Description:
*     This function normalised and returns the axis Unit attribute.
*     Normalisation refers to transformations such as "s*(m/s)" -> "m".

*  Parameters:
*     this
*        Pointer to the Axis.

*  Returned Value:
*     - Pointer to a null-terminated string containing the normalised
*     unit string.

*  Notes:
*     - The returned pointer points to a static memory buffer. The
*     contents of this buffer will be over-written on each invocation of
*     this function. A copy of the returned string should therefore be
*     taken if it will be needed later.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS        /* Pointer to thread-specific global data */
   const char *result;       /* Pointer to dynamic memory holding returned text */
   int nc;                   /* Length of normalised Unit string */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this);

/* Get the Axis Unit attrribute and normalise it. */
   result = astUnitNormaliser( astGetAxisUnit( this ) );

/* If successful, check that the resulting string will fit in the buffer.
   If not, report an error. */
   if( result ) {
      nc = strlen( result );
      if( nc > AST__AXIS_GETAXISNORMUNIT_BUFF_LEN ) {
         astError( AST__FMTER, "astGetAxisNormUnit(%s): Internal buffer "
                      "overflow while normalising the units string '%s' "
                      "- result exceeds %d characters.", status, astGetClass( this ),
                      result, AST__AXIS_GETAXISNORMUNIT_BUFF_LEN );
         result = astFree( (void *) result );

/* If so, copy it into the static buffer and free the dynamic memory returned
   by astUnitNormaliser. */
      } else {
         strcpy( getaxisnormunit_buff, result );
      }
      astFree( (void *) result );

      result = getaxisnormunit_buff;
   }

/* Return the answer. */
   return result;
}

static int GetObjSize( AstObject *this_object, int *status ) {
/*
*  Name:
*     GetObjSize

*  Purpose:
*     Return the in-memory size of an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     Axis member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied Axis,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the Axis.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstAxis *this;             /* Pointer to Axis structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the Axis structure. */
   this = (AstAxis *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by thsi class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astTSizeOf( this->label );
   result += astTSizeOf( this->format );
   result += astTSizeOf( this->symbol );
   result += astTSizeOf( this->unit );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Axis member function (over-rides the protected astGetAttrib
*     method inherited from the Object class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for an Axis, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the Axis.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Axis, or at static memory. The contents of the string
*     may be over-written or the pointer may become invalid following
*     a further invocation of the same function or any modification of
*     the Axis. A copy of the string should therefore be made if
*     necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstAxis*this;                 /* Pointer to the Axis structure */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Double attribute value */
   int digits;                   /* Digits attribute value */
   int direction;                /* Direction attribute value */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the Axis structure. */
   this = (AstAxis *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null-terminated string in an
   appropriate format.  Set "result" to point at the result string. */

/* Digits. */
/* ------- */
   if ( !strcmp( attrib, "digits" ) ) {
      digits = astGetAxisDigits( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", digits );
         result = getattrib_buff;
      }

/* Direction. */
/* ---------- */
   } else if ( !strcmp( attrib, "direction" ) ) {
      direction = astGetAxisDirection( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%d", direction );
         result = getattrib_buff;
      }

/* Top. */
/* ---- */
   } else if ( !strcmp( attrib, "top" ) ) {
      dval = astGetAxisTop( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* Bottom. */
/* ------- */
   } else if ( !strcmp( attrib, "bottom" ) ) {
      dval = astGetAxisBottom( this );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* Format. */
/* ------- */
   } else if ( !strcmp( attrib, "format" ) ) {
      result = astGetAxisFormat( this );

/* Label. */
/* ------ */
   } else if ( !strcmp( attrib, "label" ) ) {
      result = astGetAxisLabel( this );

/* Symbol. */
/* ------- */
   } else if ( !strcmp( attrib, "symbol" ) ) {
      result = astGetAxisSymbol( this );

/* Unit. */
/* ----- */
   } else if ( !strcmp( attrib, "unit" ) ) {
      result = astGetAxisUnit( this );

/* NormUnit. */
/* --------- */
   } else if ( !strcmp( attrib, "normunit" ) ) {
      result = astGetAxisNormUnit( this );

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
   return result;

}

static const char *GetDefaultFormat( AstAxis *this, int *status ){
/*
*  Name:
*     GetDefaultFormat

*  Purpose:
*     Return a pointer to a string holding the default Format value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     const char *GetDefaultFormat( AstAxis *this, int *status )

*  Class Membership:
*     Axis member function

*  Description:
*     This function returns a pointer to a string holding the default
*     Format value, which is based on the current Digits value.

*  Parameters:
*     this
*        A pointer to the Axis structure.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a static null-terminated character string containing
*     the default Format string.

*  Notes:
*     - A null string will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS         /* Pointer to thread-specific global data */

/* Check the global error status. */
   if ( !astOK ) return "";

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this);

/* Create the default format value and store it in the "format_buff"
   static variable. */
   (void) sprintf( getdefaultformat_buff, "%%1.%dG", astGetAxisDigits( this ) );

/* Return a pointer to the "format_buff" static variable. */
   return getdefaultformat_buff;
}

void astInitAxisVtab_(  AstAxisVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitAxisVtab

*  Purpose:
*     Initialise a virtual function table for an Axis.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "axis.h"
*     void astInitAxisVtab( AstAxisVtab *vtab, const char *name )

*  Class Membership:
*     Axis vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Axis class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes will be initialised if they have not already
*        been initialised.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the virtual function table belongs (it
*        is this pointer value that will subsequently be returned by the Object
*        astClass function).
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitObjectVtab( (AstObjectVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAAxis) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstObjectVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->AxisAbbrev = AxisAbbrev;
   vtab->AxisFields = AxisFields;
   vtab->AxisFormat = AxisFormat;
   vtab->AxisDistance = AxisDistance;
   vtab->AxisOffset = AxisOffset;
   vtab->AxisGap = AxisGap;
   vtab->AxisIn = AxisIn;
   vtab->AxisNorm = AxisNorm;
   vtab->AxisOverlay = AxisOverlay;
   vtab->AxisUnformat = AxisUnformat;
   vtab->ClearAxisDigits = ClearAxisDigits;
   vtab->ClearAxisDirection = ClearAxisDirection;
   vtab->ClearAxisFormat = ClearAxisFormat;
   vtab->ClearAxisLabel = ClearAxisLabel;
   vtab->ClearAxisSymbol = ClearAxisSymbol;
   vtab->ClearAxisUnit = ClearAxisUnit;
   vtab->GetAxisDigits = GetAxisDigits;
   vtab->GetAxisDirection = GetAxisDirection;
   vtab->GetAxisFormat = GetAxisFormat;
   vtab->GetAxisLabel = GetAxisLabel;
   vtab->GetAxisSymbol = GetAxisSymbol;
   vtab->GetAxisUnit = GetAxisUnit;
   vtab->GetAxisNormUnit = GetAxisNormUnit;
   vtab->SetAxisDigits = SetAxisDigits;
   vtab->SetAxisDirection = SetAxisDirection;
   vtab->SetAxisFormat = SetAxisFormat;
   vtab->SetAxisLabel = SetAxisLabel;
   vtab->SetAxisSymbol = SetAxisSymbol;
   vtab->SetAxisUnit = SetAxisUnit;
   vtab->TestAxisDigits = TestAxisDigits;
   vtab->TestAxisDirection = TestAxisDirection;
   vtab->TestAxisFormat = TestAxisFormat;
   vtab->TestAxisLabel = TestAxisLabel;
   vtab->TestAxisSymbol = TestAxisSymbol;
   vtab->TestAxisUnit = TestAxisUnit;
   vtab->TestAxisNormUnit = TestAxisNormUnit;

   vtab->ClearAxisTop = ClearAxisTop;
   vtab->GetAxisTop = GetAxisTop;
   vtab->SetAxisTop = SetAxisTop;
   vtab->TestAxisTop = TestAxisTop;

   vtab->ClearAxisBottom = ClearAxisBottom;
   vtab->GetAxisBottom = GetAxisBottom;
   vtab->SetAxisBottom = SetAxisBottom;
   vtab->TestAxisBottom = TestAxisBottom;

/* Save the inherited pointers to methods that will be extended, and replace
   them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

/* Declare the destructor, copy constructor and dump function. */
   astSetDelete( vtab, Delete );
   astSetCopy( vtab, Copy );
   astSetDump( vtab, Dump, "Axis", "Coordinate axis" );

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

static char *ParseAxisFormat( const char *fmt0, int digs, int *log, int *sign,
                              int *lspace, int *integ, int *status ){
/*
*  Name:
*     ParseAxisFormat

*  Purpose:
*     Parse the Format string for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     char *ParseAxisFormat( const char *fmt0, int digs, int *log, int *sign,
*                            int *lspace, int *integ, int *status )

*  Class Membership:
*     Axis member function

*  Description:
*     This function returns a collection of flags indicating if any AST
*     specific formatting features are specified in the supplied Format
*     string. It also returns a pointer to a new Format string which is a
*     standard C printf format specifier.

*  Parameters:
*     fmt0
*        The value of the Format attribute.
*     digs
*        The default number of digits of precision to use. This is used
*        if the given format specifier includes a wildcard precision (".*").
*        In this case, the returned format specifier will be modified to
*        include an explicit precision value equal to the supplied value
*        of "digs".
*     log
*        Pointer to an integer in which to store a flag indicating if the
*        if the axis value should be formatted as "10**x" using the graphical
*        escape sequences defined within the Plot class to produce "x" as a
*        superscript of "10". A non-zero value will be returned if the
*        supplied Format string has a '&' character in its printf <flags>
*        field (that is, between the leading '%' sign and the optional
*        printf field width).
*     sign
*        Pointer to an integer in which to store a flag indicating if a
*        sign character ('+' or '-') should always be included in front
*        of the "10" if "log" is returned non-zero. If "log" is returned
*        zero, then "sign" will also be zero. If "log" is non-zero, then
*        a non-zero value for "sign" will be returned if the supplied Format
*        string has a '+' character in its printf <flags> field (that is,
*        between the leading '%' sign and the optional printf field width).
*     lspace
*        Pointer to an integer in which to store a flag indicating if a
*        leading space should be included in front of the "10" if "log" is
*        returned non-zero and "sign" is returned zero. Otherwise, "lspace"
*        will also be zero. If "log" is non-zero, then a non-zero value for
*        "lspace" will be returned if the supplied Format string has a ' '
*        character in its printf <flags> field (that is, between the leading
*        '%' sign and the optional printf field width).
*     integ
*        Pointer to an integer in which to store a flag indicating if the
*        returned format specifier includes an integer conversion code
*        (e.g. %d) or floating point conversion code (e.g. "%.7G").
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a dynamically allocated null-terminated string containing
*     the modified Format string. This will be a copy of the supplied
*     Format string, but with any '&' flag removed. Any '+' or ' ' flag will
*     also be removed if "log" is returned as non-zero. An explicit
*     precision field will be included if the supplied format includes a
*     ".*" precision field.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   char *a;             /* Pointer to next char read from original format */
   char *b;             /* Pointer to next char to write to new format */
   char *c;             /* Pointer to next char read from original format */
   char *new;           /* Pointer to new returned string */
   char *perc;          /* Pointer to percent sign */
   char *result;        /* Pointer to the returned string */
   int hash;            /* Was a '#' flag found? */
   int len;             /* Used length of format string */
   int minus;           /* Was a '-' flag found? */
   int plus;            /* Was a '+' flag found? */
   int rlen;            /* Length of result */
   int space;           /* Was a ' ' flag found? */

/* Initialise. */
   result = NULL;
   *log = 0;
   *sign = 0;
   *lspace = 0;
   *integ = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Take a copy of the supplied string. Check the pointer can be used
   safely. */
   len = astChrLen( fmt0 );
   result = astStore( NULL, fmt0, len + 1 );
   if( astOK ) {
      result[ len ] = 0;

/* Find the first percent sign. Do nothing if none is found. */
      perc = strchr( result, '%' );
      if( perc ) {

/* Check each character following the percent sign until one is found
   which is not a legal printf flag, or a legal AST extension flag. Note
   which ones are present. */
         minus = 0;
         plus = 0;
         space = 0;
         hash = 0;

         a = perc;
         while( ++a ){
            if( *a == '-' ){
               minus = 1;
            } else if( *a == '+' ){
               plus = 1;
            } else if( *a == ' ' ){
               space = 1;
            } else if( *a == '#' ){
               hash = 1;
            } else if( *a == '&' ){
               *log = 1;
            } else {
               break;
            }
         }

/* If no '&' flag was found just return the unaltered copy of the
   supplied Format string. Otherwise, remove any '+' or ' ' flag. */
         if( *log ) {
            if( plus ) *sign = 1;
            if( space ) *lspace = 1;

/* Append any remaining flag characters to the output string. */
            perc++;
            if( minus ) *(perc++) = '-';
            if( hash ) *(perc++) = '#';

/* Copy the remaining characters down to fill up the gap left by the
   removed flags. */
            while( *a ) *(perc++) = *(a++);

/* Terminate the returned string. */
            *perc = 0;

         }
      }
   }

/* If the format specifier being returned does include a ".*" precision,
   replace the "*" with the value of the Digits attribute. */
   if( result ) {

/* Find the first percent sign. Do nothing if none is found. */
      a = strchr( result, '%' );
      if( a ) {

/* Check each character following the percent sign until one is found
   which is not a legal printf flag. */
         while( ++a ){
            if( *a != '-' && *a != '+' && *a != ' ' && *a != '#' ) break;
         }

/* Skip any field width (a decimal integer) following the flags. */
         a--;
         while( ++a ) {
            if( !isdigit( *a ) ) break;
         }

/* Get a pointer to the next alphabetic character. This will be the
   conversion code. If it an integer code, return *integ non-zero. */
         c = a - 1;
         while( ++c ) {
            if( isalpha( *c ) ) {
               if( *c == 'd' || *c == 'i' || *c == 'u' || *c == 'o' ||
                   *c == 'x' || *c == 'X' || *c == 'c' ) *integ = 1;
               break;
            }
         }

/* Go back to the end of the field width. If the next two characters are
   "." and "*", change the asterisk to the supplied "digs" value. */
         if( a[ 0 ] == '.' && a[ 1 ] == '*' ) {

/* Allocate memory to hold the extended format string (allowing 20
   characters for formatting the digs value - just in case something like
   INT_MAX is supplied by mistake), and store the existing string in it. */
            rlen = strlen( result );
            new = astMalloc( rlen + 22 );
            if( new ) memcpy( new, result, rlen + 1 );

/* Put the precision into the new string, following the field width. */
            b = new + ( a - result );
            b += sprintf( b, ".%d", digs );

/* Copy the remainder of the original format string to the new format
   string. */
            if( a[ 2 ] != 0 ) strcpy( b, a + 2 );

/* Use the new format string in place of the old.*/
            astFree( result );
            result = new;
         }
      }
   }

/* Return the result. */
   return result;

}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     Axis member function (over-rides the protected astSetAttrib
*     method inherited from the Object class).

*  Description:
*     This function assigns an attribute value for an Axis, the
*     attribute and its value being specified by means of a string of
*     the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in
*     lower case with no white space present. The value to the right
*     of the "=" should be a suitable textual representation of the
*     value to be assigned and this will be interpreted according to
*     the attribute's data type.  White space surrounding the value is
*     only significant for string attributes.

*  Parameters:
*     this
*        Pointer to the Axis.
*     setting
*        Pointer to a null terminated string specifying the new
*        attribute value.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstAxis *this;                /* Pointer to Axis structure */
   double dval;                  /* Double attribute value */
   int digits;                   /* Number of digits of precision */
   int direction;                /* Plot axis in normal direction? */
   int format;                   /* Offset of Format string */
   int label;                    /* Offset of Label string */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read from setting */
   int symbol;                   /* Offset of Symbol string */
   int unit;                     /* Offset of Unit string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Axis structure. */
   this = (AstAxis *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Digits. */
/* ------- */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "digits= %d %n", &digits, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisDigits( this, digits );

/* Direction. */
/* ---------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "direction= %d %n", &direction, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisDirection( this, direction );

/* Top. */
/* ---- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "top= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisTop( this, dval );

/* Bottom. */
/* ------- */
   } else if ( nc = 0,
        ( 1 == astSscanf( setting, "bottom= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisBottom( this, dval );

/* Format. */
/* ------- */
   } else if ( nc = 0,
        ( 0 == astSscanf( setting, "format=%n%*[^\n]%n", &format, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisFormat( this, setting + format );

/* Label. */
/* ------ */
   } else if ( nc = 0,
        ( 0 == astSscanf( setting, "label=%n%*[^\n]%n", &label, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisLabel( this, setting + label );

/* Symbol. */
/* ------- */
   } else if ( nc = 0,
        ( 0 == astSscanf( setting, "symbol=%n%*[^\n]%n", &symbol, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisSymbol( this, setting + symbol );

/* Unit. */
/* ----- */
   } else if ( nc = 0,
        ( 0 == astSscanf( setting, "unit=%n%*[^\n]%n", &unit, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisUnit( this, setting + unit );

/* Read-only attributes. */
/* --------------------- */
/* Define a macro to see if the setting string matches any of the
   read-only attributes of this class. */
#define MATCH(attrib) \
        ( nc = 0, ( 0 == astSscanf( setting, attrib "=%*[^\n]%n", &nc ) ) && \
                  ( nc >= len ) )

/* Use this macro to report an error if a read-only attribute has been
   specified. */
   } else if ( MATCH( "normunit" ) ) {
      astError( AST__NOWRT, "astSet: The setting \"%s\" is invalid for a %s.", status,
                setting, astGetClass( this ) );
      astError( AST__NOWRT, "This is a read-only attribute." , status);

/* Pass any unrecognised attribute setting to the parent method for further
   interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Axis member function (over-rides the astTestAttrib protected
*     method inherited from the Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate
*     whether a value has been set for one of an Axis' attributes.

*  Parameters:
*     this
*        Pointer to the Axis.
*     attrib
*        Pointer to a null-terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstAxis *this;                /* Pointer to the Axis structure */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Axis structure. */
   this = (AstAxis *) this_object;

/* Check the attribute name and test the appropriate attribute. */

/* Digits. */
/* ------- */
   if ( !strcmp( attrib, "digits" ) ) {
      result = astTestAxisDigits( this );

/* Direction. */
/* ---------- */
   } else if ( !strcmp( attrib, "direction" ) ) {
      result = astTestAxisDirection( this );

/* Top. */
/* ---- */
   } else if ( !strcmp( attrib, "top" ) ) {
      result = astTestAxisTop( this );

/* Bottom. */
/* ------- */
   } else if ( !strcmp( attrib, "bottom" ) ) {
      result = astTestAxisBottom( this );

/* Format. */
/* ------- */
   } else if ( !strcmp( attrib, "format" ) ) {
      result = astTestAxisFormat( this );

/* Label. */
/* ------ */
   } else if ( !strcmp( attrib, "label" ) ) {
      result = astTestAxisLabel( this );

/* Symbol. */
/* ------- */
   } else if ( !strcmp( attrib, "symbol" ) ) {
      result = astTestAxisSymbol( this );

/* Unit. */
/* ----- */
   } else if ( !strcmp( attrib, "unit" ) ) {
      result = astTestAxisUnit( this );

/* NormUnit. */
/* --------- */
   } else if ( !strcmp( attrib, "normunit" ) ) {
      result = astTestAxisNormUnit( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static int TestAxisNormUnit( AstAxis *this, int *status ){
/*
*  Name:
*     TestAxisNormUnit

*  Purpose:
*     Test if a NormUnit attribute value is set for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     int TestAxisNormUnit( AstAxis *this, int *status )

*  Class Membership:
*     Axis member function

*  Description:
*     This function returns a boolean result (0 or 1) to indicate
*     whether a value has been set for the NormUnit string.

*  Parameters:
*     this
*        Pointer to the Axis.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

   return astTestAxisUnit( this );
}


/* Functions which access class attributes. */
/* ---------------------------------------- */
/* Implement member functions to access the attributes associated with this
   class using the macros defined for this purpose in the "object.h" file. For
   a description of each attribute, see the class interface (in the associated
   .h file). */

/* Digits. */
/* ------- */
/* Clear the Digits value by setting it to -INT_MAX. */
astMAKE_CLEAR(Axis,AxisDigits,digits,-INT_MAX)

/* Supply a default of 7 digits if no value has been set. */
astMAKE_GET(Axis,AxisDigits,int,0,( this->digits != -INT_MAX ?
                                    this->digits : 7 ))

/* Constrain the Digits value being set to be at least 1. */
astMAKE_SET(Axis,AxisDigits,int,digits,( value > 1 ? value : 1 ))

/* The Digits value is set if it is not -INT_MAX. */
astMAKE_TEST(Axis,AxisDigits,( this->digits != -INT_MAX ))

/* Direction. */
/* ---------- */
/* Clear the Direction value by setting it to -INT_MAX. */
astMAKE_CLEAR(Axis,AxisDirection,direction,-INT_MAX)

/* Supply a default value of 1 if the Direction value is not set. */
astMAKE_GET(Axis,AxisDirection,int,0,( this->direction != -INT_MAX ?
                                       this->direction : 1 ))

/* Set a Direction value of 1 if any non-zero value is supplied. */
astMAKE_SET(Axis,AxisDirection,int,direction,( value != 0 ))

/* The Direction value is set if it is not -INT_MAX. */
astMAKE_TEST(Axis,AxisDirection,( this->direction != -INT_MAX ))

/* Top. */
/* -----*/
/* Clear the Top Direction value by setting it to AST__BAD. */
astMAKE_CLEAR(Axis,AxisTop,top,AST__BAD)

/* Supply a default value of DBL_MAX if the Top value is not set.*/
astMAKE_GET(Axis,AxisTop,double,0,( this->top != AST__BAD ? this->top : DBL_MAX))

/* Set the Top value. */
astMAKE_SET(Axis,AxisTop,double,top,(value))

/* The Top value is set if it is not AST__BAD. */
astMAKE_TEST(Axis,AxisTop,( this->top != AST__BAD ))

/* Bottom. */
/* --------*/
/* Clear the Bottom Direction value by setting it to AST__BAD. */
astMAKE_CLEAR(Axis,AxisBottom,bottom,AST__BAD)

/* Supply a default value of -DBL_MAX if the Bottom value is not set.*/
astMAKE_GET(Axis,AxisBottom,double,0.0,( this->bottom != AST__BAD ? this->bottom : -DBL_MAX))

/* Set the Bottom value. */
astMAKE_SET(Axis,AxisBottom,double,bottom,(value))

/* The Bottom value is set if it is not AST__BAD. */
astMAKE_TEST(Axis,AxisBottom,( this->bottom != AST__BAD ))

/* Format. */
/* ------- */
/* Clear the Format value by freeing the allocated memory and assigning a NULL
   pointer. */
astMAKE_CLEAR(Axis,AxisFormat,format,astFree( this->format ))

/* If the Format value is not set, return a pointer to a default Format
   string. */
astMAKE_GET(Axis,AxisFormat,const char *,NULL,( this->format ? this->format :
            GetDefaultFormat( this, status ) ) )

/* Set a Format value by freeing any previously allocated memory, allocating
   new memory, storing the string and saving the pointer to the copy. */
astMAKE_SET(Axis,AxisFormat,const char *,format,astStore( this->format, value,
                                                strlen( value ) + (size_t) 1 ))

/* The Format value is set if the pointer to it is not NULL. */
astMAKE_TEST(Axis,AxisFormat,( this->format != NULL ))

/* Label. */
/* ------ */
/* Clear the Label value by freeing the allocated memory and assigning a NULL
   pointer. */
astMAKE_CLEAR(Axis,AxisLabel,label,astFree( this->label ))

/* If the Label value is not set, supply a default value by way of a pointer
   to the constant string "Coordinate Axis". */
astMAKE_GET(Axis,AxisLabel,const char *,NULL,( this->label ? this->label :
                                               "Coordinate axis" ))

/* Set a Label value by freeing any previously allocated memory, allocating
   new memory, storing the string and saving the pointer to the copy. */
astMAKE_SET(Axis,AxisLabel,const char *,label,astStore( this->label, value,
                                              strlen( value ) + (size_t) 1 ))

/* The Label value is set if the pointer to it is not NULL. */
astMAKE_TEST(Axis,AxisLabel,( this->label != NULL ))

/* Symbol. */
/* ------- */
/* Clear the Symbol value by freeing the allocated memory and assigning a NULL
   pointer. */
astMAKE_CLEAR(Axis,AxisSymbol,symbol,astFree( this->symbol ))

/* If the Symbol value is not set, supply a default value by way of a pointer
   to the constant string "x". */
astMAKE_GET(Axis,AxisSymbol,const char *,NULL,( this->symbol ? this->symbol :
                                                               "x" ))

/* Set a Symbol value by freeing any previously allocated memory, allocating
   new memory, storing the string and saving the pointer to the copy. */
astMAKE_SET(Axis,AxisSymbol,const char *,symbol,astStore( this->symbol, value,
                                                strlen( value ) + (size_t) 1 ))

/* The Symbol value is set if the pointer to it is not NULL. */
astMAKE_TEST(Axis,AxisSymbol,( this->symbol != NULL ))

/* Unit. */
/* ----- */
/* Clear the Unit value by freeing the allocated memory and assigning a NULL
   pointer. */
astMAKE_CLEAR(Axis,AxisUnit,unit,astFree( this->unit ))

/* If the Unit value is not set, supply a default value by way of a pointer
   to the constant string "". */
astMAKE_GET(Axis,AxisUnit,const char *,NULL,( this->unit ? this->unit : "" ))

/* Set a Unit value by freeing any previously allocated memory, allocating
   new memory, storing the string and saving the pointer to the copy. */
astMAKE_SET(Axis,AxisUnit,const char *,unit,astStore( this->unit, value,
                                            strlen( value ) + (size_t) 1 ))

/* The Unit value is set if the pointer to it is not NULL. */
astMAKE_TEST(Axis,AxisUnit,( this->unit != NULL ))

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Axis objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Axis objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstAxis *in;                  /* Pointer to input Axis */
   AstAxis *out;                 /* Pointer to output Axis */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Axis structures. */
   in = (AstAxis *) objin;
   out = (AstAxis *) objout;

/* For safety, first clear any references to the input memory from
   the output Axis. */
   out->format = NULL;
   out->label = NULL;
   out->symbol = NULL;
   out->unit = NULL;

/* Make copies of the allocated strings and Objects. */
   if ( in->label ) out->label = astStore( NULL, in->label,
                                 strlen( in->label ) + (size_t) 1 );
   if ( in->format ) out->format = astStore( NULL, in->format,
                                   strlen( in->format ) + (size_t) 1 );
   if ( in->symbol ) out->symbol = astStore( NULL, in->symbol,
                                   strlen( in->symbol ) + (size_t) 1 );
   if ( in->unit ) out->unit = astStore( NULL, in->unit,
                               strlen( in->unit ) + (size_t) 1 );

/* If an error occurred, clean up by freeing all memory allocated above. */
   if ( !astOK ) {
      out->format = astFree( out->format );
      out->label = astFree( out->label );
      out->symbol = astFree( out->symbol );
      out->unit = astFree( out->unit );
   }
}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Axis objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Axis objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     void

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstAxis *this;                /* Pointer to Axis */

/* Obtain a pointer to the Axis structure. */
   this = (AstAxis *) obj;

/* Free all allocated memory. */
   this->format = astFree( this->format );
   this->label = astFree( this->label );
   this->symbol = astFree( this->symbol );
   this->unit = astFree( this->unit );
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Axis objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Axis class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Axis whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstAxis *this;                /* Pointer to the Axis structure */
   char comment[ 80 ];           /* Buffer for comment string */
   const char *sval;             /* Pointer to string value */
   const char *lab;              /* Pointer to unit label */
   double dval;                  /* Double value */
   int ival;                     /* Integer value */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Axis structure. */
   this = (AstAxis *) this_object;

/* Write out values representing the instance variables for the
   Axis class.  Accompany these with appropriate comment strings,
   possibly depending on the values being written.*/

/* In the case of attributes, we first use the appropriate (private)
   Test...  member function to see if they are set. If so, we then use
   the (private) Get... function to obtain the value to be written
   out.

   For attributes which are not set, we use the astGet... method to
   obtain the value instead. This will supply a default value
   (possibly provided by a derived class which over-rides this method)
   which is more useful to a human reader as it corresponds to the
   actual default attribute value.  Since "set" will be zero, these
   values are for information only and will not be read back. */

/* Label. */
/* ------ */
   set = TestAxisLabel( this, status );
   sval = set ? GetAxisLabel( this, status ) : astGetAxisLabel( this );
   astWriteString( channel, "Label", set, 1, sval, "Axis Label" );

/* Symbol. */
/* ------- */
   set = TestAxisSymbol( this, status );
   sval = set ? GetAxisSymbol( this, status ) : astGetAxisSymbol( this );
   astWriteString( channel, "Symbol", set, 1, sval, "Axis symbol" );

/* Unit. */
/* ----- */
   set = TestAxisUnit( this, status );
   sval = set ? GetAxisUnit( this, status ) : astGetAxisUnit( this );

/* Get any label associated with the unit string. */
   lab = astUnitLabel( sval );

/* Construct a comment including the above label (but only if it is not
   the same as the unit string) . */
   if( lab && strcmp( lab, sval ) ) {
      (void) sprintf( comment, "Axis units (%s)", lab );
   } else {
      (void) sprintf( comment, "Axis units" );
   }

/* Write out the Unit value. */
   astWriteString( channel, "Unit", set, 0, sval, comment );

/* Digits. */
/* ------- */
   set = TestAxisDigits( this, status );
   ival = set ? GetAxisDigits( this, status ) : astGetAxisDigits( this );
   astWriteInt( channel, "Digits", set, 0, ival,
                "Default formatting precision" );

/* Format. */
/* ------- */
   set = TestAxisFormat( this, status );
   sval = set ? GetAxisFormat( this, status ) : astGetAxisFormat( this );
   astWriteString( channel, "Format", set, 0, sval, "Format specifier" );

/* Direction. */
/* ---------- */
   set = TestAxisDirection( this, status );
   ival = set ? GetAxisDirection( this, status ) : astGetAxisDirection( this );
   astWriteInt( channel, "Dirn", set, 0, ival,
                ival ? "Plot in conventional direction (hint)" :
                       "Plot in reverse direction (hint)" );
/* Top. */
/* ---- */
   set = TestAxisTop( this, status );
   dval = set ? GetAxisTop( this, status ) : astGetAxisTop( this );
   astWriteDouble( channel, "Top", set, 0, dval, "Maximum legal axis value" );

/* Bottom. */
/* ------- */
   set = TestAxisBottom( this, status );
   dval = set ? GetAxisBottom( this, status ) : astGetAxisBottom( this );
   astWriteDouble( channel, "Bottom", set, 0, dval, "Minimum legal axis value" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAAxis and astCheckAxis functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Axis,Object)
astMAKE_CHECK(Axis)

AstAxis *astAxis_( const char *options, int *status, ...) {
/*
*+
*  Name:
*     astAxis

*  Purpose:
*     Create an Axis.

*  Type:
*     Public function.

*  Synopsis:
*     #include "axis.h"
*     AstAxis *astAxis( const char *options, int *status, ... )

*  Class Membership:
*     Axis constructor.

*  Description:
*     This function creates a new Axis and optionally initialises its
*     attributes.

*  Parameters:
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new Axis. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     status
*        Pointer to the inherited status variable.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new Axis.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstAxis *new;                 /* Pointer to new Axis */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the Axis, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitAxis( NULL, sizeof( AstAxis ), !class_init, &class_vtab,
                      "Axis" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new Axis'
   attributes. */
      va_start( args, status );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Axis. */
   return new;
}

AstAxis *astAxisId_( const char *options, ... ) {
/*
*  Name:
*     astAxisId_

*  Purpose:
*     Create an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     AstAxis *astAxisId_( const char *options, ... );

*  Class Membership:
*     Axis constructor.

*  Description:
*     This function implements the external (public) interface to the
*     astAxis constructor function. It returns an ID value (instead of
*     a true C pointer) to external users, and must be provided
*     because astAxis_ has a variable argument list which cannot be
*     encapsulated in a macro (where this conversion would otherwise
*     occur).
*
*     The variable argument list also prevents this function from
*     invoking astAxis_ directly, so it must be a re-implementation of
*     it in all respects, except for the final conversion of the
*     result to an ID value.

*  Parameters:
*     As for astAxis_.

*  Returned Value:
*     The ID value associated with the new Axis.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstAxis *new;                 /* Pointer to new Axis */
   va_list args;                 /* Variable argument list */

   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise the Axis, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitAxis( NULL, sizeof( AstAxis ), !class_init, &class_vtab,
                      "Axis" );

/* If successful, note that the virtual function table has been
   initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new Axis'
   attributes. */
      va_start( args, options );
      astVSet( new, options, NULL, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Axis. */
   return astMakeId( new );
}

AstAxis *astInitAxis_( void *mem, size_t size, int init,
                       AstAxisVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitAxis

*  Purpose:
*     Initialise an Axis.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "axis.h"
*     AstAxis *astInitAxis( void *mem, size_t size, int init,
*                           AstAxisVtab *vtab, const char *name )

*  Class Membership:
*     Axis initialiser.

*  Description:
*     This function is provided for use by class implementations to initialise
*     a new Axis object. It allocates memory (if necessary) to accommodate
*     the Axis plus any additional data associated with the derived class.
*     It then initialises an Axis structure at the start of this memory. If
*     the "init" flag is set, it also initialises the contents of a virtual
*     function table for an Axis at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Axis is to be created. This
*        must be of sufficient size to accommodate the Axis data
*        (sizeof(Axis)) plus any data used by the derived class. If a value
*        of NULL is given, this function will allocate the memory itself using
*        the "size" parameter to determine its size.
*     size
*        The amount of memory used by the Axis (plus derived class data).
*        This will be used to allocate memory if a value of NULL is given for
*        the "mem" parameter. This value is also stored in the Axis
*        structure, so a valid value must be supplied even if not required for
*        allocating memory.
*     init
*        A logical flag indicating if the Axis's virtual function table is
*        to be initialised. If this value is non-zero, the virtual function
*        table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be associated
*        with the new Axis.
*     name
*        Pointer to a constant null-terminated character string which contains
*        the name of the class to which the new object belongs (it is this
*        pointer value that will subsequently be returned by the astClass
*        method).

*  Returned Value:
*     A pointer to the new Axis.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstAxis *new;                 /* Pointer to new Axis */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitAxisVtab( vtab, name );

/* Initialise an Object structure (the parent class) as the first component
   within the Axis structure, allocating memory if necessary. */
   new = (AstAxis *) astInitObject( mem, size, 0, (AstObjectVtab *) vtab,
                                    name );

   if ( astOK ) {

/* Initialise the Axis data. */
/* ------------------------- */
/* Initialise all attributes to their "undefined" values. */
      new->digits = -INT_MAX;
      new->direction = -INT_MAX;
      new->format = NULL;
      new->label = NULL;
      new->symbol = NULL;
      new->unit = NULL;
      new->top = AST__BAD;
      new->bottom = AST__BAD;

/* If an error occurred, clean up by deleting the new Axis. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Axis. */
   return new;
}

AstAxis *astLoadAxis_( void *mem, size_t size,
                       AstAxisVtab *vtab, const char *name,
                       AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadAxis

*  Purpose:
*     Load an Axis.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "axis.h"
*     AstAxis *astLoadAxis( void *mem, size_t size,
*                           AstAxisVtab *vtab, const char *name,
*                           AstChannel *channel )

*  Class Membership:
*     Axis loader.

*  Description:
*     This function is provided to load a new Axis using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Axis structure in this memory, using data read from the input
*     Channel.
*
*     If the "init" flag is set, it also initialises the contents of a
*     virtual function table for a Axis at the start of the memory
*     passed via the "vtab" parameter.


*  Parameters:
*     mem
*        A pointer to the memory into which the Axis is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Axis data (sizeof(Axis)) plus any data used by derived
*        classes. If a value of NULL is given, this function will
*        allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Axis (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Axis structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstAxis) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Axis. If this is NULL, a pointer
*        to the (static) virtual function table for the Axis class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Axis" is used instead.

*  Returned Value:
*     A pointer to the new Axis.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstAxis *new;                 /* Pointer to the new Axis */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Axis. In this case the
   Axis belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstAxis );
      vtab = &class_vtab;
      name = "Axis";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitAxisVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Axis. */
   new = astLoadObject( mem, size, (AstObjectVtab *) vtab, name,
                        channel );

   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Axis" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Label. */
/* ------ */
/* Note that string values do not require any additional processing. */
      new->label = astReadString( channel, "label", NULL );

/* Symbol. */
/* ------- */
      new->symbol = astReadString( channel, "symbol", NULL );

/* Unit. */
/* ----- */
      new->unit = astReadString( channel, "unit", NULL );

/* Digits. */
/* ------- */
      new->digits = astReadInt( channel, "digits", -INT_MAX );
      if ( TestAxisDigits( new, status ) ) SetAxisDigits( new, new->digits, status );

/* Format. */
/* ------- */
      new->format = astReadString( channel, "format", NULL );

/* Direction. */
/* ---------- */
      new->direction = astReadInt( channel, "dirn", -INT_MAX );
      if ( TestAxisDirection( new, status ) ) SetAxisDirection( new, new->direction, status );

/* Top. */
/* ---- */
      new->top = astReadDouble( channel, "top", AST__BAD );
      if ( TestAxisTop( new, status ) ) SetAxisTop( new, new->top, status );

/* Bottom. */
/* ---- */
      new->bottom = astReadDouble( channel, "bottom", AST__BAD );
      if ( TestAxisBottom( new, status ) ) SetAxisBottom( new, new->bottom, status );

/* If an error occurred, clean up by deleting the new Axis. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Axis pointer. */
   return new;
}

/* Virtual function interfaces. */
/* ============================ */
/* These provide the external interface to the virtual functions defined by
   this class. Each simply checks the global error status and then locates and
   executes the appropriate member function, using the function pointer stored
   in the object's virtual function table (this pointer is located using the
   astMEMBER macro defined in "object.h").

   Note that the member function may not be the one defined here, as it may
   have been over-ridden by a derived class. However, it should still have the
   same interface. */

/* External interfaces for the attribute access functions are generated
   automatically by the macros that implement the access functions themselves.
   Hence, we need only provide external interfaces for a few additional
   functions here. */
const char *astAxisAbbrev_( AstAxis *this, const char *fmt,
                            const char *str1, const char *str2, int *status ) {
   if ( !astOK ) return str2;
   return (**astMEMBER(this,Axis,AxisAbbrev))( this, fmt, str1, str2, status );
}
const char *astAxisFormat_( AstAxis *this, double value, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Axis,AxisFormat))( this, value, status );
}
double astAxisDistance_( AstAxis *this, double v1, double v2, int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,Axis,AxisDistance))( this, v1, v2, status );
}
double astAxisOffset_( AstAxis *this, double v1, double dist, int *status ) {
   if ( !astOK ) return AST__BAD;
   return (**astMEMBER(this,Axis,AxisOffset))( this, v1, dist, status );
}
double astAxisGap_( AstAxis *this, double gap, int *ntick, int *status ) {
   if ( !astOK ) return 0.0;
   return (**astMEMBER(this,Axis,AxisGap))( this, gap, ntick, status );
}
void astAxisNorm_( AstAxis *this, double *value, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Axis,AxisNorm))( this, value, status );
}
void astAxisOverlay_( AstAxis *template, AstAxis *result, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(template,Axis,AxisOverlay))( template, result, status );
}
int astAxisUnformat_( AstAxis *this, const char *string, double *value, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Axis,AxisUnformat))( this, string, value, status );
}
int astAxisFields_( AstAxis *this, const char *fmt, const char *str,
                    int maxfld, char **fields, int *nc, double *val, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Axis,AxisFields))( this, fmt, str, maxfld, fields, nc, val, status );
}
int astAxisIn_( AstAxis *this, double lo, double hi, double val, int closed, int *status ){
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Axis,AxisIn))( this, lo, hi, val, closed, status );
}
const char *astGetAxisNormUnit_( AstAxis *this, int *status ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Axis,GetAxisNormUnit))( this, status );
}
int astTestAxisNormUnit_( AstAxis *this, int *status ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Axis,TestAxisNormUnit))( this, status );
}









