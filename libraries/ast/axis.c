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
*     <COPYRIGHT_STATEMENT>

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
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Axis

/* Define numerical constants for use in thie module. */
#define FORMAT_BUFF_LEN 50       /* Max length of default Format string */

/* Header files. */
/* ============= */
#include "ast_err.h"             /* Error code definitions */

/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Object interface (parent class) */
#include "pointset.h"            /* Sets of coordinates (for AST__BAD) */
#include "channel.h"             /* I/O channels */
#include "axis.h"                /* Interface definition for this class */

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
/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstAxisVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

/* Pointers to parent class methods which are extended by this class. */
static const char *(* parent_getattrib)( AstObject *, const char * );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_setattrib)( AstObject *, const char * );

/* Define other static variables. */
static char format_buff[ FORMAT_BUFF_LEN + 1 ]; /* Default Format string */

/* External Interface Function Prototypes. */
/* ======================================= */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstAxis *astAxisId_( const char *, ... );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static const char *AxisAbbrev( AstAxis *, const char *, const char * );
static const char *AxisFormat( AstAxis *, double );
static const char *GetAttrib( AstObject *, const char * );
static const char *GetAxisFormat( AstAxis * );
static const char *GetAxisLabel( AstAxis * );
static const char *GetAxisSymbol( AstAxis * );
static const char *GetAxisUnit( AstAxis * );
static double AxisGap( AstAxis *, double, int * );
static int AxisUnformat( AstAxis *, const char *, double * );
static int GetAxisDigits( AstAxis * );
static int GetAxisDirection( AstAxis * );
static int TestAttrib( AstObject *, const char * );
static int TestAxisDigits( AstAxis * );
static int TestAxisDirection( AstAxis * );
static int TestAxisFormat( AstAxis * );
static int TestAxisLabel( AstAxis * );
static int TestAxisSymbol( AstAxis * );
static int TestAxisUnit( AstAxis * );
static void AxisNorm( AstAxis *, double * );
static void AxisOverlay( AstAxis *, AstAxis * );
static void ClearAttrib( AstObject *, const char * );
static void ClearAxisDigits( AstAxis * );
static void ClearAxisDirection( AstAxis * );
static void ClearAxisFormat( AstAxis * );
static void ClearAxisLabel( AstAxis * );
static void ClearAxisSymbol( AstAxis * );
static void ClearAxisUnit( AstAxis * );
static void Copy( const AstObject *, AstObject * );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );
static void InitVtab( AstAxisVtab * );
static void SetAttrib( AstObject *, const char * );
static void SetAxisDigits( AstAxis *, int );
static void SetAxisDirection( AstAxis *, int );
static void SetAxisFormat( AstAxis *, const char * );
static void SetAxisLabel( AstAxis *, const char * );
static void SetAxisSymbol( AstAxis *, const char * );
static void SetAxisUnit( AstAxis *, const char * );

/* Member functions. */
/* ================= */
static const char *AxisAbbrev( AstAxis *this,
                               const char *str1, const char *str2 ) {
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
*     const char *astAxisAbbrev( AstAxis *this,
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
*     str1
*        Pointer to a constant null-terminated string containing the
*        first formatted value.
*     str1
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
   if ( !strcmp( str1, str2 ) ) result += strlen( str2 );

/* Return the result. */
   return result;
}

static const char *AxisFormat( AstAxis *this, double value ) {
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
#define BUFF_LEN 127            /* Maximum characters in result */

/* Local Variables: */
   const char *fmt;             /* Pointer to Format string */
   const char *result;          /* Pointer to formatted value */
   int nc;                      /* Number of characters written */
   int stat;                    /* Value of errno after error */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for result string */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

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
      fmt = GetAxisFormat( this );

/* Clear errno and attempt to format the value as if the Format string were
   a standard "sprintf" format. */
      if ( astOK ) {
         errno = 0;
         nc = sprintf( buff, fmt, value );

/* The possibilities for error detection are limited here, but check if an
   error value was returned and report an error. Include information from
   errno if it was set. */
         if ( nc < 0 ) {
            stat = errno;
            astError( AST__FMTER, "astAxisFormat(%s): Error formatting a "
                      "coordinate value of %1.*G%s%s.", astGetClass( this ),
                      DBL_DIG, value, stat? " - " : "",
                      stat ? strerror( stat ) : "" );
            astError( AST__FMTER, "The format string was \"%s\".", fmt );

/* Also check that the result buffer did not overflow. If it did, memory will
   probably have been corrupted but this cannot be prevented with "sprintf".
   Report the error and abort. */
         } else if ( nc > BUFF_LEN ) {
            astError( AST__FMTER, "astAxisFormat(%s): Internal buffer "
                      "overflow while formatting a coordinate value of %1.*G "
                      "- result exceeds %d characters.", astGetClass( this ),
                      DBL_DIG, value, BUFF_LEN );
            astError( AST__FMTER, "The format string was \"%s\".", fmt );

/* If no error was detected, return a pointer to the buffer. */
         } else {
            result = buff;
         }
      }
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static double AxisGap( AstAxis *this, double gap, int *ntick ) {
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
            { 1.0, 2.0, 2.0, 4.0, 5.0, 5.0, 5.0, 10.0, 10.0, 10.0 };
   static int table2[ 10 ] =     /* Table giving number of divisions */
            {   5,   4,   4,   4,   5,   5,   5,    5,    5,    5 };

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

static void AxisNorm( AstAxis *this, double *value ) {
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

static void AxisOverlay( AstAxis *template, AstAxis *result ) {
/*
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
   if ( TestAxisFormat( template ) ) {
      SetAxisFormat( result, GetAxisFormat( template ) );  
   }

/* Undefine macros local to this function. */
#undef OVERLAY
}

static int AxisUnformat( AstAxis *this, const char *string, double *value ) {
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
   if ( 1 == sscanf( string, "%lf %n", &coord, &nc ) ) {
      *value = coord;

/* Otherwise, see if the string starts with "<bad>", allowing mixed
   case and leading, embedded and trailing white space. If so, return
   the value AST__BAD. */
   } else if ( nc = 0,
               ( 0 == sscanf( string, " < %*1[Bb] %*1[Aa] %*1[Dd] > %n", &nc )
               && ( nc > 0 ) ) ) {
      *value = AST__BAD;

/* If the string cannot be read, return a function result of zero. */
   } else {
      nc = 0;
   }
   
/* Return the number of characters read. */
   return nc;
}

static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

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

/* Symbol. */
/* ------- */
   } else if ( !strcmp( attrib, "symbol" ) ) {
      astClearAxisSymbol( this );

/* Unit. */
/* ----- */
   } else if ( !strcmp( attrib, "unit" ) ) {
      astClearAxisUnit( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

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

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstAxis*this;                 /* Pointer to the Axis structure */
   const char *result;           /* Pointer value to return */
   int digits;                   /* Digits attribute value */
   int direction;                /* Direction attribute value */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */

/* Initialise. */
   result = NULL;

/* Check the global error status. */   
   if ( !astOK ) return result;

/* Obtain a pointer to the Axis structure. */
   this = (AstAxis *) this_object;

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Digits. */
/* ------- */
   if ( !strcmp( attrib, "digits" ) ) {
      digits = astGetAxisDigits( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", digits );
         result = buff;
      }

/* Direction. */
/* ---------- */
   } else if ( !strcmp( attrib, "direction" ) ) {
      direction = astGetAxisDirection( this );
      if ( astOK ) {
         (void) sprintf( buff, "%d", direction );
         result = buff;
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

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib );
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static void InitVtab( AstAxisVtab *vtab ) {
/*
*  Name:
*     InitVtab

*  Purpose:
*     Initialise a virtual function table for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     void InitVtab( AstAxisVtab *vtab )

*  Class Membership:
*     Axis member function.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Axis class.

*  Parameters:
*     vtab
*        Pointer to the virtual function table. The components used by
*        all ancestral classes should already have been initialised.
*/

/* Local Variables: */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAAxis) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that provide
   virtual methods for this class. */
   vtab->AxisAbbrev = AxisAbbrev;
   vtab->AxisFormat = AxisFormat;
   vtab->AxisGap = AxisGap;
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

/* Save the inherited pointers to methods that will be extended, and replace
   them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
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
}

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     void SetAttrib( AstObject *this, const char *setting )

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
*/

/* Local Variables: */
   AstAxis *this;                /* Pointer to Axis structure */
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

/* Test for each recognised attribute in turn, using "sscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Digits. */
/* ------- */
   if ( nc = 0,
        ( 1 == sscanf( setting, "digits= %d %n", &digits, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisDigits( this, digits );

/* Direction. */
/* ---------- */
   } else if ( nc = 0,
        ( 1 == sscanf( setting, "direction= %d %n", &direction, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisDirection( this, direction );

/* Format. */
/* ------- */
   } else if ( nc = 0,
        ( 0 == sscanf( setting, "format=%n%*[^\n]%n", &format, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisFormat( this, setting + format );

/* Label. */
/* ------ */
   } else if ( nc = 0,
        ( 0 == sscanf( setting, "label=%n%*[^\n]%n", &label, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisLabel( this, setting + label );

/* Symbol. */
/* ------- */
   } else if ( nc = 0,
        ( 0 == sscanf( setting, "symbol=%n%*[^\n]%n", &symbol, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisSymbol( this, setting + symbol );

/* Unit. */
/* ----- */
   } else if ( nc = 0,
        ( 0 == sscanf( setting, "unit=%n%*[^\n]%n", &unit, &nc ) )
        && ( nc >= len ) ) {
      astSetAxisUnit( this, setting + unit );

/* Pass any unrecognised attribute setting to the parent method for further
   interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for an Axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "axis.h"
*     int TestAttrib( AstObject *this, const char *attrib )

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

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }

/* Return the result, */
   return result;
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

/* Format. */
/* ------- */
/* Clear the Format value by freeing the allocated memory and assigning a NULL
   pointer. */
astMAKE_CLEAR(Axis,AxisFormat,format,astFree( this->format ))

/* If the Format value is not set, supply a default value by writing a C format
   specifier into the static "format_buff" buffer and returning a pointer to
   this. Base the number of digits in the format on the value obtained from
   the astGetAxisDigits method. */
astMAKE_GET(Axis,AxisFormat,const char *,NULL,( this->format ? this->format :
         ( (void) sprintf( format_buff, "%%1.%dG", astGetAxisDigits( this ) ),
         format_buff ) ))

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
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Axis objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for Axis objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.

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

/* Make copies of the allocated strings. */
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
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Axis objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for Axis objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.

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
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Axis objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Axis class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Axis whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

/* Local Variables: */
   AstAxis *this;                /* Pointer to the Axis structure */
   const char *sval;             /* Pointer to string value */
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
   set = TestAxisLabel( this );
   sval = set ? GetAxisLabel( this ) : astGetAxisLabel( this );
   astWriteString( channel, "Label", set, 1, sval, "Axis Label" );

/* Symbol. */
/* ------- */
   set = TestAxisSymbol( this );
   sval = set ? GetAxisSymbol( this ) : astGetAxisSymbol( this );
   astWriteString( channel, "Symbol", set, 1, sval, "Axis symbol" );

/* Unit. */
/* ----- */
   set = TestAxisUnit( this );
   sval = set ? GetAxisUnit( this ) : astGetAxisUnit( this );
   astWriteString( channel, "Unit", set, 0, sval, "Axis units" );

/* Digits. */
/* ------- */
   set = TestAxisDigits( this );
   ival = set ? GetAxisDigits( this ) : astGetAxisDigits( this );
   astWriteInt( channel, "Digits", set, 0, ival,
                "Default formatting precision" );

/* Format. */
/* ------- */
   set = TestAxisFormat( this );
   sval = set ? GetAxisFormat( this ) : astGetAxisFormat( this );
   astWriteString( channel, "Format", set, 0, sval, "Format specifier" );

/* Direction. */
/* ---------- */
   set = TestAxisDirection( this );
   ival = set ? GetAxisDirection( this ) : astGetAxisDirection( this );
   astWriteInt( channel, "Dirn", set, 0, ival,
                ival ? "Plot in conventional direction (hint)" :
                       "Plot in reverse direction (hint)" );
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAAxis and astCheckAxis functions using the macros
   defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Axis,Object,check,&class_init)
astMAKE_CHECK(Axis)

AstAxis *astAxis_( const char *options, ... ) {
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
*     AstAxis *astAxis( const char *options, ... )

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
   AstAxis *new;                 /* Pointer to new Axis */
   va_list args;                 /* Variable argument list */

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
      astVSet( new, options, args );
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
   AstAxis *new;                 /* Pointer to new Axis */
   va_list args;                 /* Variable argument list */

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
      astVSet( new, options, args );
      va_end( args );

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new Axis. */
   return astMakeId( new );
}

AstAxis *astInitAxis_( void *mem, size_t size, int init,
                       AstAxisVtab *vtab, const char *name ) {
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

/* Initialise an Object structure (the parent class) as the first component
   within the Axis structure, allocating memory if necessary. */
   new = (AstAxis *) astInitObject( mem, size, init, (AstObjectVtab *) vtab,
                                    name );

/* If necessary, initialise the virtual function table. */
/* ---------------------------------------------------- */
   if ( init ) InitVtab( vtab );
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

/* If an error occurred, clean up by deleting the new Axis. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new Axis. */
   return new;
}

AstAxis *astLoadAxis_( void *mem, size_t size, int init,
                       AstAxisVtab *vtab, const char *name,
                       AstChannel *channel ) {
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
*     AstAxis *astLoadAxis( void *mem, size_t size, int init,
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
*     init
*        A boolean flag indicating if the Axis's virtual function
*        table is to be initialised. If this value is non-zero, the
*        virtual function table will be initialised by this function.
*
*        If the "vtab" parameter is NULL, the "init" value is ignored
*        and the (static) virtual function table initialisation flag
*        for the Axis class is used instead.
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
   AstAxis *new;                 /* Pointer to the new Axis */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Axis. In this case the
   Axis belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstAxis );
      init = !class_init;
      vtab = &class_vtab;
      name = "Axis";
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Axis. */
   new = astLoadObject( mem, size, init, (AstObjectVtab *) vtab, name,
                        channel );

/* If required, initialise the part of the virtual function table used
   by this class. */
   if ( init && !class_init ) InitVtab( vtab );

/* Note if we have successfully initialised the (static) virtual
   function table owned by this class (so that this is done only
   once). */
   if ( astOK ) {
      if ( ( vtab == &class_vtab ) && init ) class_init = 1;

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
      if ( TestAxisDigits( new ) ) SetAxisDigits( new, new->digits );
      
/* Format. */
/* ------- */
      new->format = astReadString( channel, "format", NULL );

/* Direction. */
/* ---------- */
      new->direction = astReadInt( channel, "dirn", -INT_MAX );
      if ( TestAxisDirection( new ) ) SetAxisDirection( new, new->direction );

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
const char *astAxisAbbrev_( AstAxis *this,
                            const char *str1, const char *str2 ) {
   if ( !astOK ) return str2;
   return (**astMEMBER(this,Axis,AxisAbbrev))( this, str1, str2 );
}
const char *astAxisFormat_( AstAxis *this, double value ) {
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,Axis,AxisFormat))( this, value );
}
double astAxisGap_( AstAxis *this, double gap, int *ntick ) {
   if ( !astOK ) return 0.0;
   return (**astMEMBER(this,Axis,AxisGap))( this, gap, ntick );
}
void astAxisNorm_( AstAxis *this, double *value ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Axis,AxisNorm))( this, value );
}
void astAxisOverlay_( AstAxis *template, AstAxis *result ) {
   if ( !astOK ) return;
   (**astMEMBER(template,Axis,AxisOverlay))( template, result );
}
int astAxisUnformat_( AstAxis *this, const char *string, double *value ) {
   if ( !astOK ) return 0;
   return (**astMEMBER(this,Axis,AxisUnformat))( this, string, value );
}
