/*
*class++
*  Name:
*     FluxFrame

*  Purpose:
*     Measured flux description.

*  Constructor Function:
c     astFluxFrame
f     AST_FLUXFRAME

*  Description:
*     A FluxFrame is a specialised form of one-dimensional Frame which 
*     represents various systems used to represent the signal level in an
*     observation. The particular coordinate system to be used is specified 
*     by setting the FluxFrame's System attribute qualified, as necessary, by 
*     other attributes such as the units, etc (see the description of the 
*     System attribute for details).
*
*     All flux values are assumed to be measured at the same frequency or
*     wavelength (as given by the SpecVal attribute). Thus this class is
*     more appropriate for use with images rather than spectra.

*  Inheritance:
*     The FluxFrame class inherits from the Frame class.

*  Attributes:
*     In addition to those attributes common to all Frames, every
*     FluxFrame also has the following attributes:
*
*     - SpecVal: The spectral position at which the flux values are measured.

*  Functions:
c     In addition to those functions applicable to all Frames, the
c     following functions may also be applied to all FluxFrames:
f     In addition to those routines applicable to all Frames, the
f     following routines may also be applied to all FluxFrames:

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     6-DEC-2004 (DSB):
*        Original version.
*     14-DEC-2004 (DSB):
*        Added AST__SBRIGHT and AST__SBRIGHTW systems.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS FluxFrame

/* Define the first and last acceptable System values. */
#define FIRST_SYSTEM AST__FLUXDEN
#define LAST_SYSTEM AST__SBRIGHTW

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E8*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "unit.h"                /* Units management facilities */
#include "object.h"              /* Base Object class */
#include "frame.h"               /* Parent Frame class */
#include "fluxframe.h"           /* Interface definition for this class */
#include "mapping.h"             /* Coordinate Mappings */
#include "unitmap.h"             /* Unit Mappings */
#include "cmpmap.h"              /* Compound Mappings */
#include "zoommap.h"             /* Scaling Mappings */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h>
#include <math.h>

/* Module Variables. */
/* ================= */
/* Define the class virtual function table and its initialisation flag as
   static variables. */
static AstFluxFrameVtab class_vtab; /* Virtual function table */
static int class_init = 0;          /* Virtual function table initialised? */

/* Pointers to parent class methods which are used or extended by this
   class. */
static AstSystemType (* parent_getalignsystem)( AstFrame * );
static AstSystemType (* parent_getsystem)( AstFrame * );
static const char *(* parent_getattrib)( AstObject *, const char * );
static const char *(* parent_getdomain)( AstFrame * );
static const char *(* parent_getlabel)( AstFrame *, int );
static const char *(* parent_getsymbol)( AstFrame *, int );
static const char *(* parent_gettitle)( AstFrame * );
static const char *(* parent_getunit)( AstFrame *, int );
static int (* parent_match)( AstFrame *, AstFrame *, int **, int **, AstMapping **, AstFrame ** );
static int (* parent_subframe)( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame ** );
static int (* parent_testattrib)( AstObject *, const char * );
static void (* parent_setunit)( AstFrame *, int, const char * );
static void (* parent_clearattrib)( AstObject *, const char * );
static void (* parent_overlay)( AstFrame *, const int *, AstFrame * );
static void (* parent_setattrib)( AstObject *, const char * );
static void (* parent_setmaxaxes)( AstFrame *, int );
static void (* parent_setminaxes)( AstFrame *, int );
static void (* parent_setsystem)( AstFrame *, AstSystemType );
static void (* parent_clearsystem)( AstFrame * );
static void (* parent_clearunit)( AstFrame *, int );

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstSpecFrame *GetSpecFrame( AstFluxFrame * );
static AstSystemType DensitySystem( AstSystemType );
static AstSystemType GetAlignSystem( AstFrame * );
static AstSystemType GetDensitySystem( AstFluxFrame * );
static AstSystemType SystemCode( AstFrame *, const char * );
static AstSystemType ValidateSystem( AstFrame *, AstSystemType, const char * );
static const char *DefUnit( AstSystemType, const char *, const char * );
static const char *DensityUnit( AstSystemType );
static const char *FluxSystemString( AstSystemType );
static const char *GetDensityUnit( AstFluxFrame * );
static const char *GetDomain( AstFrame * );
static const char *GetLabel( AstFrame *, int );
static const char *GetSymbol( AstFrame *, int );
static const char *GetTitle( AstFrame * );
static const char *GetUnit( AstFrame *, int );
static const char *SystemLabel( AstSystemType );
static const char *SystemString( AstFrame *, AstSystemType );
static int GetActiveUnit( AstFrame * );
static int MakeFluxMapping( AstFluxFrame *, AstFluxFrame *, AstSystemType, AstMapping ** );
static int Match( AstFrame *, AstFrame *, int **, int **, AstMapping **, AstFrame ** );
static int SubFrame( AstFrame *, AstFrame *, int, const int *, const int *, AstMapping **, AstFrame ** );
static int TestActiveUnit( AstFrame * );
static int UnitsOK( AstSystemType, const char *, int, const char *, const char * );
static void ClearUnit( AstFrame *, int );
static void Copy( const AstObject *, AstObject * );
static void Delete( AstObject * );
static void Dump( AstObject *, AstChannel * );
static void Overlay( AstFrame *, const int *, AstFrame * );
static void SetMaxAxes( AstFrame *, int );
static void SetMinAxes( AstFrame *, int );
static void SetUnit( AstFrame *, int, const char * );

static AstSystemType GetSystem( AstFrame * );
static void SetSystem( AstFrame *, AstSystemType );
static void ClearSystem( AstFrame * );

static const char *GetAttrib( AstObject *, const char * );
static int TestAttrib( AstObject *, const char * );
static void ClearAttrib( AstObject *, const char * );
static void SetAttrib( AstObject *, const char * );

static double GetSpecVal( AstFluxFrame * );
static int TestSpecVal( AstFluxFrame * );
static void ClearSpecVal( AstFluxFrame * );
static void SetSpecVal( AstFluxFrame *, double );

/* Member functions. */
/* ================= */

static void ClearAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     FluxFrame member function (over-rides the astClearAttrib protected
*     method inherited from the Frame class).

*  Description:
*     This function clears the value of a specified attribute for a
*     FluxFrame, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to the FluxFrame structure */
   int len;                      /* Length of attrib string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_object;

/* Obtain the length of the "attrib" string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

/* SpecVal. */
/* -------- */
   if ( !strcmp( attrib, "specval" ) ) {
      astClearSpecVal( this );

/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib );
   }
}

static void ClearSystem( AstFrame *this_frame ) {
/*
*  Name:
*     ClearSystem

*  Purpose:
*     Clear the System attribute for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void ClearSystem( AstFrame *this_frame )

*  Class Membership:
*     FluxFrame member function (over-rides the astClearSystem protected
*     method inherited from the Frame class).

*  Description:
*     This function clears the System attribute for a FluxFrame.

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to FluxFrame structure */
   AstSystemType newsys;         /* System after clearing */
   AstSystemType oldsys;         /* System before clearing */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* Save the original system */
   oldsys = astGetSystem( this_frame );

/* Use the parent ClearSystem method to clear the System value. */
   (*parent_clearsystem)( this_frame );

/* Get the default System. */
   newsys = astGetSystem( this_frame );

/* If the system has actually changed. */
   if( newsys != oldsys ) {

/* Changing the System value will in general require the Units to change
   as well. If the used has previously specified the units to be used with
   the new system, then re-instate them (they are stored in the "usedunits"
   array in the FluxFrame structure). Otherwise, clear the units so that
   the default units will eb used with the new System. */
      if( (int) newsys < this->nuunits && this->usedunits &&
          this->usedunits[ (int) newsys ] ) {
         (*parent_setunit)( this_frame, 0, this->usedunits[ (int) newsys ] );
      } else {
         (*parent_clearunit)( this_frame, 0 );
      }

/* Also, clear all attributes which have system-specific defaults. */
      astClearLabel( this_frame, 0 );
      astClearSymbol( this_frame, 0 );
      astClearTitle( this_frame );
   }

}

static void ClearUnit( AstFrame *this_frame, int axis ) {
/*
*  Name:
*     astClearUnit

*  Purpose:
*     Clear the value of the Unit string for a FluxFrame's axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void ClearUnit( AstFrame *this_frame, int axis )

*  Class Membership:
*     FluxFrame member function (over-rides the astClearUnit method inherited
*     from the Frame class).

*  Description:
*     This function clears the Unit string for a specified axis of a 
*     FluxFrame. It also clears the UsedUnit item in the FluxFrame
*     structure corresponding to the current System.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     axis
*        The number of the axis (zero-based).
*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to the FluxFrame structure */
   int system;                   /* The FluxFrame's System value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astClearUnit" );

/* Clear the UsedUnit item for the current System, if current set. */
   system = (int) astGetSystem( this );
   if( system < this->nuunits && this->usedunits ) {
      this->usedunits[ system ] = astFree( this->usedunits[ system ] );
   }

/* Use the parent method to clear the Unit attribute of the axis. */
   (*parent_clearunit)( this_frame, axis );
}

static const char *DefUnit( AstSystemType system, const char *method,
                            const char *class ){
/*
*  Name:
*     DefUnit

*  Purpose:
*     Return the default units for a flux coordinate system type.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *DefUnit( AstSystemType system, const char *method,
*                          const char *class )

*  Class Membership:
*     FluxFrame member function.

*  Description:
*     This function returns a textual representation of the default 
*     units associated with the specified flux coordinate system.

*  Parameters:
*     system
*        The flux coordinate system.
*     method
*        Pointer to a string holding the name of the calling method.
*        This is only for use in constructing error messages.
*     class 
*        Pointer to a string holding the name of the supplied object class.
*        This is only for use in constructing error messages.

*  Returned Value:
*     As tring describing the default units. This string follows the
*     units syntax described in FITS WCS paper I "Representations of world
*     coordinates in FITS" (Greisen & Calabretta).

*  Notes:
*     - A NULL pointer is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   const char *result;           /* Value to return */

/* Initialize */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get an identifier for the default units. */
   if( system == AST__FLUXDEN ) {
      result = "W/m^2/Hz";

   } else if( system == AST__FLUXDENW ) {
      result = "W/m^2/Angstrom";

   } else if( system == AST__SBRIGHT ) {
      result = "W/m^2/Hz/arcmin**2";

   } else if( system == AST__SBRIGHTW ) {
      result = "W/m^2/Angstrom/arcmin**2";

/* Report an error if the coordinate system was not recognised. */
   } else {
      astError( AST__SCSIN, "%s(%s): Corrupt %s contains illegal System "
                "identification code (%d).", method, class, class, 
                (int) system );
   }

/* Return the result. */
   return result;
}

static AstSystemType DensitySystem( AstSystemType sys ) {
/*
*  Name:
*     DensitySystem

*  Purpose:
*     Obtain the System describing the spectral density for a FluxFrame
*     system.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     AstSystemType DensitySystem( AstSystemType sys )

*  Class Membership:
*     FluxFrame member function.

*  Description:
*     This function returns AST__FREQ if the FluxFrame system describes
*     a quantity measured per unit frequency, and returns AST__WAVELEN if 
*     the FluxFrame system describes a quantity measured per unit wavelength.

*  Parameters:
*     sys
*        A System value appropriate to a FluxFrame.

*  Returned Value:
*     The density System value.

*  Notes:
*     - AST__BADSYSTEM is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstSystemType result;         /* Value to return */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Categorise the supplied system. */
   if( sys == AST__FLUXDEN || sys == AST__SBRIGHT ) {
      result = AST__FREQ;

   } else if( sys == AST__FLUXDENW || sys == AST__SBRIGHTW ) {
      result = AST__WAVELEN;

   } else if( astOK ) {
      astError( AST__INTER, "DensitySystem(FluxFrame): The "
                "DensitySystem method does not yet support "
                "FluxFrame system %d (AST internal programming error).",
                sys );
   }

/* Return the result. */
   return result;
}

static const char *DensityUnit( AstSystemType sys ) {
/*
*  Name:
*     DensityUnit

*  Purpose:
*     Obtain the default units for the spectral density of a FluxFrame
*     system.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *DensityUnit( AstSystemType sys )

*  Class Membership:
*     FluxFrame member function.

*  Description:
*     This function returns "Hz" if the FluxFrame system describes
*     a quantity measured per unit frequency, and returns "Angstrom" if 
*     the FluxFrame system describes a quantity measured per unit wavelength.

*  Parameters:
*     sys
*        A FluxFrame system value.

*  Returned Value:
*     A pointer to a null-terminated string containing the Unit value.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Categorise the supplied FluxFrame system. */
   if( sys == AST__FLUXDEN || sys == AST__SBRIGHT ) {
      result = "Hz";

   } else if( sys == AST__FLUXDENW || sys == AST__SBRIGHTW ) {
      result = "Angstrom";

   } else if( astOK ) {
      astError( AST__INTER, "DensityUnit(FluxFrame): The DensityUnit "
                "method does not yet support FluxFrame system %d (AST "
                "internal programming error).", sys );
   }

/* Return the result. */
   return result;
}

static const char *FluxSystemString( AstSystemType system ) {
/*
*  Name:
*     FluxSystemString

*  Purpose:
*     Convert a FluxFrame coordinate system type code into a string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *FluxSystemString( AstSystemType system )

*  Class Membership:
*     FluxFrame member function 

*  Description:
*     This function converts a FluxFrame coordinate system type code
*     (System attribute value) into a string suitable for use as an
*     external representation of the coordinate system type.

*  Parameters:
*     system
*        The coordinate system type code.

*  Returned Value:
*     Pointer to a constant null-terminated string containing the
*     textual equivalent of the type code supplied.

*  Notes:
*     - A NULL pointer value is returned if the coordinate system
*     code was not recognised. This does not produce an error.
*     - A NULL pointer value is also returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "system" value against each possibility and convert to a
   string pointer. */
   switch ( system ) {

   case AST__FLUXDEN:
      result = "FLXDN";
      break;

   case AST__FLUXDENW:
      result = "FLXDNW";
      break;

   case AST__SBRIGHT:
      result = "SFCBR";
      break;

   case AST__SBRIGHTW:
      result = "SFCBRW";
      break;

   }

/* Return the result pointer. */
   return result;
}

static int GetActiveUnit( AstFrame *this_frame ) {
/*
*  Name:
*     GetActiveUnit

*  Purpose:
*     Obtain the value of the ActiveUnit flag for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     int GetActiveUnit( AstFrame *this_frame ) 

*  Class Membership:
*     FluxFrame member function (over-rides the astGetActiveUnit protected
*     method inherited from the Frame class).

*  Description:
*    This function returns the value of the ActiveUnit flag for a
*    FluxFrame, which is always 1.

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*  Returned Value:
*     The value to use for the ActiveUnit flag (1).

*/
   return 1;
}

static const char *GetAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *GetAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     FluxFrame member function (over-rides the protected astGetAttrib
*     method inherited from the Frame class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a FluxFrame, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     attrib
*        Pointer to a null-terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.

*  Returned Value:
*     - Pointer to a null-terminated string containing the attribute
*     value.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - The returned string pointer may point at memory allocated
*     within the FluxFrame, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the FluxFrame. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Constants: */
#define BUFF_LEN 50              /* Max. characters in result buffer */

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to the FluxFrame structure */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Attribute value */
   int len;                      /* Length of attrib string */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for string result */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "buff" as a null-terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* SpecVal */
/* ------- */
   if ( !strcmp( attrib, "specval" ) ) {
      dval = astGetSpecVal( this );
      if ( astOK ) {
         if( dval != AST__BAD ) {
            (void) sprintf( buff, "%.*g", DBL_DIG, dval );
            result = buff;
         } else {
            result = "<bad>";
         }
      }

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

static AstSystemType GetDensitySystem( AstFluxFrame *this ) {
/*
*+
*  Name:
*     astGetDensitySystem

*  Purpose:
*     Obtain the System describing the spectral density of a FluxFrame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fluxframe.h"
*     AstSystemType astGetDensitySystem( AstFluxFrame *this )

*  Class Membership:
*     FluxFrame method.

*  Description:
*     This function returns AST__FREQ if the FluxFrame system describes
*     a quantity measured per unit frequency, and returns AST__WAVELEN if 
*     the FluxFrame system describes a quantity measured per unit wavelength.

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*  Returned Value:
*     The System value.

*  Notes:
*     - AST__BADSYSTEM is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return AST__BADSYSTEM;

/* Get the FluxFrame system and categorise it. */
   return DensitySystem( astGetSystem( this ) );
}

static const char *GetDensityUnit( AstFluxFrame *this ) {
/*
*+
*  Name:
*     astGetDensityUnit

*  Purpose:
*     Obtain the default units for the spectral density of a FluxFrame.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *astGetDensityUnit( AstFluxFrame *this )

*  Class Membership:
*     FluxFrame method.

*  Description:
*     This function returns "Hz" if the FluxFrame system describes
*     a quantity measured per unit frequency, and returns "Angstrom" if 
*     the FluxFrame system describes a quantity measured per unit wavelength.

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*  Returned Value:
*     A pointer to a null-terminated string containing the Unit value.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get the FluxFrame system and categorise it. */
   return DensityUnit( astGetSystem( this ) );
}

static const char *GetDomain( AstFrame *this_frame ) {
/*
*  Name:
*     GetDomain

*  Purpose:
*     Obtain a pointer to the Domain attribute string for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *GetDomain( AstFrame *this )

*  Class Membership:
*     FluxFrame member function (over-rides the astGetDomain protected
*     method inherited from the Frame class).

*  Description:
*    This function returns a pointer to the Domain attribute string
*    for a FluxFrame.

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*  Returned Value:
*     A pointer to a constant null-terminated string containing the
*     Domain value.

*  Notes:
*     - The returned pointer or the string it refers to may become
*     invalid following further invocation of this function or
*     modification of the FluxFrame.
*     - A NULL pointer is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to FluxFrame structure */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* If a Domain attribute string has been set, invoke the parent method
   to obtain a pointer to it. */
   if ( astTestDomain( this ) ) {
      result = (*parent_getdomain)( this_frame );

/* Otherwise, provide a pointer to a suitable default string. */
   } else {
      result = "FLUX";
   }

/* Return the result. */
   return result;
}

static const char *GetLabel( AstFrame *this, int axis ) {
/*
*  Name:
*     GetLabel

*  Purpose:
*     Access the Label string for a FluxFrame axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *GetLabel( AstFrame *this, int axis )

*  Class Membership:
*     FluxFrame member function (over-rides the astGetLabel method inherited
*     from the Frame class).

*  Description:
*     This function returns a pointer to the Label string for a specified axis
*     of a FluxFrame.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     axis
*        Axis index (zero-based) identifying the axis for which information is
*        required.

*  Returned Value:
*     Pointer to a constant null-terminated character string containing the
*     requested information.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Constants: */
#define BUFF_LEN 200             /* Max characters in result string */

/* Local Variables: */
   AstMapping *map;              /* Mapping between units */
   AstSystemType system;         /* Code identifying type of flux coordinates */
   char *new_lab;                /* Modified label string */
   const char *result;           /* Pointer to label string */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for result string */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astGetLabel" );

/* Check if a value has been set for the required axis label string. If so,
   invoke the parent astGetLabel method to obtain a pointer to it. */
   if ( astTestLabel( this, axis ) ) {
      result = (*parent_getlabel)( this, axis );

/* Otherwise, identify the flux coordinate system described by the 
   FluxFrame. */
   } else {
      system = astGetSystem( this );

/* If OK, supply a pointer to a suitable default label string. */
      if ( astOK ) {
         result = strcpy( buff, SystemLabel( system ) );
         buff[ 0 ] = toupper( buff[ 0 ] );

/* Modify this default to take account of the current value of the Unit 
   attribute, if set. */
         if( astTestUnit( this, axis ) ) {

/* Find a Mapping from the default Units for the current System, to the 
   units indicated by the Unit attribute. This Mapping is used to modify
   the existing default label appropriately. For instance, if the default
   units is "Jy" and the actual units is "log(Jy)", then the default label
   of "Flux density" is changed to "log( Flux density )". */
            map = astUnitMapper( DefUnit( system, "astGetLabel", 
                                          astGetClass( this ) ),
                                 astGetUnit( this, axis ), result,
                                 &new_lab );
            if( new_lab ) {
               result = strcpy( buff, new_lab );
               new_lab = astFree( new_lab );
            }

/* Annul the unused Mapping. */
            if( map ) map = astAnnul( map );

         }
      }
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN

}

static AstSpecFrame *GetSpecFrame( AstFluxFrame *this ) {
/*
*  Name:
*     GetSpecFrame

*  Purpose:
*     Get a pointer to a SpecFrame associated with a FluxFrame

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     AstSpecFrame *GetSpecFrame( AstFluxFrame *this )

*  Class Membership:
*     FluxFrame member function 

*  Description:
*     This function returns a SpecFrame describing the spectral system in
*     which the FluxFrame's SpecVal attribute is stored. A default
*     SpecFrame is created and returned if the no SpecFrame was supplied
*     when the FluxFrame was created.

*  Parameters:
*     this
*        The FluxFrame.

*  Returned Value:
*     Pointer to the SpecFrame. It should be freed using astAnnul when no
*     longer needed.

*  Notes:
*     - A NULL pointer value is returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   AstSpecFrame *result;           /* Pointer value to return */
   AstSystemType sys;              /* FluxFrame System */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the FluxFrame contains a SpecFrame, return a clone of its pointer. */
   if( this->specframe ) {
      result = astClone( this->specframe );

/* Otherwise, create a SpecFrame appropriate to the FluxFrames System. */
   } else {
      result = astSpecFrame( "" );
      astSetSystem( result, astGetDensitySystem( this ) );
      astSetUnit( result, 0, astGetDensityUnit( this ) );
   }

/* Annul the result if an error occurred. */
   if( !astOK ) result = astAnnul( result );

/* Return the result pointer. */
   return result;
}

static const char *GetSymbol( AstFrame *this, int axis ) {
/*
*  Name:
*     GetSymbol

*  Purpose:
*     Obtain a pointer to the Symbol string for a FluxFrame axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *GetSymbol( AstFrame *this, int axis )

*  Class Membership:
*     FluxFrame member function (over-rides the astGetSymbol method inherited
*     from the Frame class).

*  Description:
*     This function returns a pointer to the Symbol string for a specified axis
*     of a FluxFrame.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     axis
*        Axis index (zero-based) identifying the axis for which information is
*        required.

*  Returned Value:
*     Pointer to a constant null-terminated character string containing the
*     requested information.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Constants: */
#define BUFF_LEN 200             /* Max characters in result string */

/* Local Variables: */
   AstMapping *map;              /* Mapping between units */
   AstSystemType system;         /* Code identifying type of sky coordinates */
   char *new_sym;                /* Modified symbol string */
   const char *result;           /* Pointer to symbol string */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for result string */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astGetSymbol" );

/* Check if a value has been set for the required axis symbol string. If so,
   invoke the parent astGetSymbol method to obtain a pointer to it. */
   if ( astTestSymbol( this, axis ) ) {
      result = (*parent_getsymbol)( this, axis );

/* Otherwise, identify the flux coordinate system described by the FluxFrame. */
   } else {
      system = astGetSystem( this );

/* If OK, supply a pointer to a suitable default Symbol string. */
      if ( astOK ) {

         if( system == AST__FLUXDEN ) {
	    result = "S_nu";

         } else if( system == AST__FLUXDENW ) {
	    result = "S_lambda";

         } else if( system == AST__SBRIGHT ) {
	    result = "mu_nu";

         } else if( system == AST__SBRIGHTW ) {
	    result = "mu_lambda";

/* Report an error if the coordinate system was not recognised. */
         } else {
	    astError( AST__SCSIN, "astGetSymbol(%s): Corrupt %s contains "
		      "invalid System identification code (%d).", 
                      astGetClass( this ), astGetClass( this ), (int) system );
         }

/* Modify this default to take account of the current value of the Unit 
   attribute, if set. */
         if( astTestUnit( this, axis ) ) {

/* Find a Mapping from the default Units for the current System, to the 
   units indicated by the Unit attribute. This Mapping is used to modify
   the existing default symbol appropriately. For instance, if the default
   units is "Jy" and the actual units is "log(Jy)", then the default symbol
   of "S_nu" is changed to "log( S_nu )". */
            map = astUnitMapper( DefUnit( system, "astGetSymbol", 
                                          astGetClass( this ) ),
                                 astGetUnit( this, axis ), result,
                                 &new_sym );
            if( new_sym ) {
               result = strcpy( buff, new_sym );
               new_sym = astFree( new_sym );
            }

/* Annul the unused Mapping. */
            if( map ) map = astAnnul( map );

         }
      }
   }

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static AstSystemType GetAlignSystem( AstFrame *this_frame ) {
/*
*  Name:
*     GetAlignSystem

*  Purpose:
*     Obtain the AlignSystem attribute for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "Specframe.h"
*     AstSystemType GetAlignSystem( AstFrame *this_frame )

*  Class Membership:
*     FluxFrame member function (over-rides the astGetAlignSystem protected
*     method inherited from the Frame class).

*  Description:
*     This function returns the AlignSystem attribute for a FluxFrame.

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*  Returned Value:
*     The AlignSystem value.

*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to FluxFrame structure */
   AstSystemType result;         /* Value to return */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* If a AlignSystem attribute has been set, invoke the parent method to obtain 
   it. */
   if ( astTestAlignSystem( this ) ) {
      result = (*parent_getalignsystem)( this_frame );

/* Otherwise, provide a suitable default. */
   } else {
      result = AST__FLUXDEN;
   }

/* Return the result. */
   return result;
}

static AstSystemType GetSystem( AstFrame *this_frame ) {
/*
*  Name:
*     GetSystem

*  Purpose:
*     Obtain the System attribute for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     AstSystemType GetSystem( AstFrame *this_frame )

*  Class Membership:
*     FluxFrame member function (over-rides the astGetSystem protected
*     method inherited from the Frame class).

*  Description:
*     This function returns the System attribute for a FluxFrame.

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*  Returned Value:
*     The System value.

*  Notes:
*     - AST__BADSYSTEM is returned if this function is invoked with
*     the global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to FluxFrame structure */
   AstMapping *map;              /* Pointer to unit Mapping */
   AstSystemType i;              /* System to check */
   AstSystemType result;         /* Value to return */
   const char *units;            /* FluxFrame units */
   int unitSet;                  /* Has a value been supplied for Unit? */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* See if a value has been assigned to the Unit attribute. */
   unitSet = astTestUnit( this_frame, 0 );

/* If a System attribute has been set, invoke the parent method to obtain 
   it. */
   if ( astTestSystem( this ) ) {
      result = (*parent_getsystem)( this_frame );

/* Otherwise, if the Unit attribute has been set, provide a suitable default 
   system based on the units. */
   } else if( unitSet ){

/* Loop round each known system value. If a Mapping can be found from the 
   current units to the default units for the system, then use the system as 
   the default system. */
      units = astGetUnit( this_frame, 0 );
      for( i = FIRST_SYSTEM; i <= LAST_SYSTEM; i++ ) {
         map = astUnitMapper( units, DefUnit( i, "astGetSystem", 
                                    astGetClass( this ) ), NULL, NULL );
         if( map ) {
            map = astAnnul( map );
            result = i;
            break;
         }
      }

/* Otherwise, report an error. */
      if( result == AST__BADSYSTEM && astOK ) {
         astError( AST__BADUN, "astGetSystem(%s): The current units (%s) "
                   "cannot be used with any of the supported flux systems.", 
                   astGetClass( this ), astGetUnit( this_frame, 0 ) );
      }

/* Otherwise, provide a suitable default based on the units. */
   } else {
      result = AST__FLUXDEN;
   }

/* Return the result. */
   return result;
}

static const char *GetTitle( AstFrame *this_frame ) {
/*
*  Name:
*     GetTitle

*  Purpose:
*     Obtain a pointer to the Title string for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *GetTitle( AstFrame *this_frame )

*  Class Membership:
*     FluxFrame member function (over-rides the astGetTitle method inherited
*     from the Frame class).

*  Description:
*     This function returns a pointer to the Title string for a FluxFrame.
*     A pointer to a suitable default string is returned if no Title value has
*     previously been set.

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*  Returned Value:
*     Pointer to a null-terminated character string containing the requested
*     information.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Constants: */
#define BUFF_LEN 200             /* Max characters in result string */

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to FluxFrame structure */
   AstSpecFrame *sf;             /* Pointer to SpecFrame structure */
   const char *result;           /* Pointer to result string */
   const char *sv;               /* Formatted SpecVal string */
   const char *su;               /* Units string */
   double specval;               /* SpecVal value */
   int pos;                      /* Buffer position to enter text */
   static char buff[ BUFF_LEN + 1 ]; /* Buffer for result string */
   
/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Initialise. */
   result = NULL;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* See if a Title string has been set. If so, use the parent astGetTitle
   method to obtain a pointer to it. */
   if ( astTestTitle( this ) ) {
      result = (*parent_gettitle)( this_frame );

/* Otherwise, we will generate a default Title string. */
   } else {

/* Classify the coordinate system type and create an appropriate Title
   string.  */
      if ( astOK ) {
         result = buff;

/* Begin with the system's default label. */
         pos = sprintf( buff, "%s", SystemLabel( astGetSystem( this ) ) );
         buff[ 0 ] = toupper( buff[ 0 ] );

/* Append the spectral position, if known. */
         specval = astGetSpecVal( this );
         sf = GetSpecFrame( this );
         if( specval != AST__BAD && sf ) {       
            sv = astFormat( sf, 0, specval );
            su = astGetUnit( sf, 0 );
            pos += sprintf( buff + pos, " at = %s %s", sv, su );
         }
         sf = astAnnul( sf );
      }
   }

/* If an error occurred, clear the returned pointer value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;

/* Undefine macros local to this function. */
#undef BUFF_LEN
}

static const char *GetUnit( AstFrame *this_frame, int axis ) {
/*
*  Name:
*     astGetUnit

*  Purpose:
*     Obtain a pointer to the Unit string for a FluxFrame's axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *GetUnit( AstFrame *this_frame, int axis )

*  Class Membership:
*     FluxFrame member function (over-rides the astGetUnit method inherited
*     from the Frame class).

*  Description:
*     This function returns a pointer to the Unit string for a specified axis
*     of a FluxFrame. If the Unit attribute has not been set for the axis, a
*     pointer to a suitable default string is returned instead.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     axis
*        The number of the axis (zero-based) for which information is required.

*  Returned Value:
*     A pointer to a null-terminated string containing the Unit value.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to the FluxFrame structure */
   AstSystemType system;         /* The FluxFrame's System value */
   const char *result;           /* Pointer value to return */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astGetUnit" );

/* If a value has been set for the Unit attribute, use the parent 
   GetUnit method to return a pointer to the required Unit string. */
   if( astTestUnit( this, axis ) ){
      result = (*parent_getunit)( this_frame, axis );

/* Otherwise, identify the flux coordinate system described by the 
   FluxFrame. */
   } else {
      system = astGetSystem( this );

/* Return a string describing the default units. */
      result = DefUnit( system, "astGetUnit", astGetClass( this ) );
   }

/* If an error occurred, clear the returned value. */
   if ( !astOK ) result = NULL;

/* Return the result. */
   return result;
}

void astInitFluxFrameVtab_(  AstFluxFrameVtab *vtab, const char *name ) {
/*
*+
*  Name:
*     astInitFluxFrameVtab

*  Purpose:
*     Initialise a virtual function table for a FluxFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fluxframe.h"
*     void astInitFluxFrameVtab( AstFluxFrameVtab *vtab, const char *name )

*  Class Membership:
*     FluxFrame vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the FluxFrame class.

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
   AstFrameVtab *frame;          /* Pointer to Frame component of Vtab */
   AstObjectVtab *object;        /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitFrameVtab( (AstFrameVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAFluxFrame) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_init variable to generate this unique value. */
   vtab->check = &class_init;

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */
   vtab->GetDensitySystem = GetDensitySystem;
   vtab->GetDensityUnit = GetDensityUnit;

   vtab->ClearSpecVal = ClearSpecVal;
   vtab->TestSpecVal = TestSpecVal;
   vtab->GetSpecVal = GetSpecVal;
   vtab->SetSpecVal = SetSpecVal;

/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   frame = (AstFrameVtab *) vtab;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;
   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;
   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;
   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

   parent_getdomain = frame->GetDomain;
   frame->GetDomain = GetDomain;

   parent_getsystem = frame->GetSystem;
   frame->GetSystem = GetSystem;
   parent_setsystem = frame->SetSystem;
   frame->SetSystem = SetSystem;
   parent_clearsystem = frame->ClearSystem;
   frame->ClearSystem = ClearSystem;

   parent_getalignsystem = frame->GetAlignSystem;
   frame->GetAlignSystem = GetAlignSystem;

   parent_getlabel = frame->GetLabel;
   frame->GetLabel = GetLabel;

   parent_getsymbol = frame->GetSymbol;
   frame->GetSymbol = GetSymbol;

   parent_gettitle = frame->GetTitle;
   frame->GetTitle = GetTitle;

   parent_clearunit = frame->ClearUnit;
   frame->ClearUnit = ClearUnit;

   parent_getunit = frame->GetUnit;
   frame->GetUnit = GetUnit;

   parent_setunit = frame->SetUnit;
   frame->SetUnit = SetUnit;

   parent_match = frame->Match;
   frame->Match = Match;

   parent_overlay = frame->Overlay;
   frame->Overlay = Overlay;

   parent_setmaxaxes = frame->SetMaxAxes;
   frame->SetMaxAxes = SetMaxAxes;

   parent_setminaxes = frame->SetMinAxes;
   frame->SetMinAxes = SetMinAxes;

   parent_subframe = frame->SubFrame;
   frame->SubFrame = SubFrame;

/* Store replacement pointers for methods which will be over-ridden by new
   member functions implemented here. */
   frame->GetActiveUnit = GetActiveUnit;
   frame->TestActiveUnit = TestActiveUnit;
   frame->ValidateSystem = ValidateSystem;
   frame->SystemString = SystemString;
   frame->SystemCode = SystemCode;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "FluxFrame", "Description of flux values" );

}

static int MakeFluxMapping( AstFluxFrame *target, AstFluxFrame *result,
                            AstSystemType align_sys, AstMapping **map ) {
/*
*  Name:
*     MakeFluxMapping

*  Purpose:
*     Generate a Mapping between two FluxFrames.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     int MakeFluxMapping( AstFluxFrame *target, AstFluxFrame *result,
*                          AstSystemType align_sys, MakeFluAstMapping **map )

*  Class Membership:
*     FluxFrame member function.

*  Description:
*     This function takes two FluxFrames and generates a Mapping that
*     converts between them, taking account of differences in their
*     coordinate systems, reference frequency, etc. 
*
*     In order to cut down the number of transformations to be considered,
*     the scheme works by first converting from the target frame to an
*     "alignment" Frame, using the attributes of the target to define the
*     transformation. A transformation is then found from the alignment
*     frame to the required result Frame,  using the attributes of the
*     result to define the transformation. The alignment Frame is
*     described by the supplied parameter "align_sys".

*  Parameters:
*     target
*        Pointer to the first FluxFrame.
*     result
*        Pointer to the second FluxFrame.
*     align_sys
*        The flux system in which to align the two FluxFrames.
*     map
*        Pointer to a location which is to receive a pointer to the
*        returned Mapping. The forward transformation of this Mapping
*        will convert from "target" coordinates to "result"
*        coordinates, and the inverse transformation will convert in
*        the opposite direction (all coordinate values in radians).

*  Returned Value:
*     Non-zero if the Mapping could be generated, or zero if the two
*     FluxFrames are sufficiently un-related that no meaningful Mapping
*     can be produced (in which case a NULL Mapping pointer will be
*     returned).

*  Notes:
*     A value of zero is returned if this function is invoked with the
*     global error status set or if it should fail for any reason.
*/

/* Local Variables: */
   AstFrameSet *fs;
   AstMapping *map1;  
   AstMapping *map2;  
   AstMapping *map3;  
   AstMapping *map4;  
   AstMapping *map5;  
   AstMapping *smap;  
   AstMapping *smap_in;  
   AstMapping *smap_out;  
   AstMapping *tmap;  
   AstSpecFrame *sfin1;
   AstSpecFrame *sfin2;
   AstSpecFrame *sfout1;
   AstSpecFrame *sfout2;
   AstSystemType rsys_in; 
   AstSystemType rsys_out; 
   AstSystemType sys_in; 
   AstSystemType sys_out; 
   double specval2;
   double specval;
   double specval_in;
   double specval_out;
   double zoom;
   int match;                     
   int sb_in;
   int sb_out;

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Initialise the returned values. */
   match = 0;
   *map = NULL;

/* Initialise to avoid compiler warnings. */
   map1 = NULL;
   map2 = NULL;
   map3 = NULL;

/* Note the target and result System */
   rsys_in = astGetSystem( target );
   rsys_out = astGetSystem( result );

/* First get a Mapping which converts from the units used in the target
   to the default units associated with the target's system. 
   ---------------------------------------------------------------------- */
   map1 = astUnitMapper( astGetUnit( target, 0 ), 
                         DefUnit( rsys_in, "MakeFluxMapping", "FluxFrame" ), 
                         NULL, NULL );

/* If the target system is surface brightness, change it to the
   corresponding flux density system. We are effectively converting from
   surface brightness to the flux density normalised to unit area. Also
   set flags indicating if the systems are surface brightness systems. */
   if( rsys_in == AST__SBRIGHT ) {
      sys_in = AST__FLUXDEN;
      sb_in = 1;
      
   } else if( rsys_in == AST__SBRIGHTW ) {
      sys_in = AST__FLUXDENW;
      sb_in = 1;

   } else {
      sys_in = rsys_in;
      sb_in = 0;
   }

/* Likewise if the result system is surface brightness, change it to the
   corresponding flux density system. */
   if( rsys_out == AST__SBRIGHT ) {
      sys_out = AST__FLUXDEN;
      sb_out = 1;

   } else if( rsys_out == AST__SBRIGHTW ) {
      sys_out = AST__FLUXDENW;
      sb_out = 1;

   } else {
      sys_out = rsys_out;
      sb_out = 0;
   }

/* Assume at this point in the chain of coversions that we have target values 
   in some form of flux density system (either frequency or wavelength). The 
   precise units do not matter at this point (so long as they are
   dimensionally correct for describing the relevant form of flux density). 
   When other systems are added (e.g. antenna temperature), some code
   will have to come before this point which produces a Mapping from (e.g.)
   antenna temperature to flux density. */


/* Get a Mapping from the default units for the input flux density system 
   to the default units for the output flux density system.
   ---------------------------------------------------------------------- */

/* If one but not both of the systems represent surface brightness, then
   we cannot form a Mapping. */
   if( sb_in != sb_out ) {
      zoom = AST__BAD;

/* If the input and output flux density systems are the same, then the 
   required Mapping is a UnitMap. */
   } else if( sys_in == sys_out ) {
      zoom = 1.0;

/* Otherwise, the required Mapping is a zoom map in which the scale factor is 
   the rate of change of the input spectral system with respect to the output
   spectral system, at the position given by the SpecVal attribute (we
   cannot do the conversion if the SpecVal values in the target and result
   differ). Each spectral system is either wavelength (in Angstrom) or
   frequency (in Hz), depending on whether the associated flux density
   system is "per Angstrom" or "per Hertz". The SpecVal value may be
   stored in some other system, so the first job is to create SpecFrames
   with the required system and units from the SpecFrames encapsulated
   within the target and result FluxFrames. Take deep copies of the two
   SpecFrames, and set their systems and units. */
   } else {
      sfin1 = GetSpecFrame( target );
      sfin2 = astCopy( sfin1 );
      astSetSystem( sfin2, DensitySystem( sys_in ) );
      astSetUnit( sfin2, 0, DensityUnit( sys_in ) );
   
      sfout1 = GetSpecFrame( result );
      sfout2 = astCopy( sfout1 );
      astSetSystem( sfout2, DensitySystem( sys_out ) );
      astSetUnit( sfout2, 0, DensityUnit( sys_out ) );
      
/* Indicate we do not yet have a zoom factor */
      zoom = AST__BAD;
   
/* Get the Mapping from output to input spectral coordinate system */
      fs = astConvert( sfout2, sfin2, "" );
      if( fs ) {
         tmap = astGetMapping( fs, AST__BASE, AST__CURRENT );
         fs = astAnnul( fs );
   
/* Simplify the Mapping. */
         smap = astSimplify( tmap );
         tmap = astAnnul( tmap );
   
/* We first need to transform the two SpecVal attributes into the input 
   coordinate system of the "smap" Mapping (i.e. the standardised result 
   FluxFrame), and check they are the same. For this we need the Mappings 
   from the SpecFrames stored in the FluxFrames to the modified copies 
   created above. */
         fs = astConvert( sfin1, sfin2, "" );
         if( fs ) {
            smap_in = astGetMapping( fs, AST__BASE, AST__CURRENT );
            fs = astAnnul( fs );
         } else {     
            smap_in = NULL;
         }
   
         fs = astConvert( sfout1, sfout2, "" );
         if( fs ) {
            smap_out = astGetMapping( fs, AST__BASE, AST__CURRENT );
            fs = astAnnul( fs );
         } else {     
            smap_out = NULL;
         }
      
/* Convert the target's SpecVal into the standardised target system */
         specval = astGetSpecVal( target );
         astTran1( smap_in, 1, &specval, 1, &specval2 );
   
/* Now convert it into the standardised result system. Note, we need to
   use "smap" in the inverse direction for this. */
         astTran1( smap, 1, &specval2, 0, &specval_in );
      
/* Convert the results's SpecVal into the standardised result system */
         specval = astGetSpecVal( result );
         astTran1( smap_out, 1, &specval, 1, &specval_out );
      
/* Check they are equal and good. */
         if( EQUAL( specval_in, specval_out ) && specval_in != AST__BAD ) {
   
/* If the siSimplified Mapping is a UnitMap the required rate of change 
   factor is 1.0. If it resuts in a ZoomMap, the required factor is 
   the zoom factor in the ZoomMap. */
            if( astIsAUnitMap( smap ) ) {
               zoom = 1.0;
   
            } else if( astIsAZoomMap( smap ) ) {
               zoom = astGetZoom( smap );
   
/* For any other type of Mapping, we must determine the rate of change factor 
   by differentiating the Mapping at the SpecVal position. */
            } else {
               specval = 0.5*( specval_in  + specval_out );
               zoom = astRate( smap, &specval, 0, 0 );
            }
         }
   
/* Free resources */
         if( smap_in ) smap_in = astAnnul( smap_in );
         if( smap_out ) smap_out = astAnnul( smap_out );
         smap = astAnnul( smap );
      }
   
      sfout1 = astAnnul( sfout1 );
      sfin1 = astAnnul( sfin1 );
      sfout2 = astAnnul( sfout2 );
      sfin2 = astAnnul( sfin2 );
   }

/* Create the required zoom map if a scaling factor was found. */
   if( zoom != AST__BAD ) {
      map2 = (AstMapping *) astZoomMap( 1, fabs( zoom ), "" );
   } else {
      map2 = NULL;
   }

/* Now get a Mapping which converts from the default units associated with 
   the results's system, to the units used in the result. 
   ----------------------------------------------------------------------- */
   map3 = astUnitMapper( DefUnit( rsys_out, "MakeFluxMapping", "FluxFrame" ), 
                         astGetUnit( result, 0 ), NULL, NULL );

/* Indicate a match was found and combine all Mapings in series. */
   if( map1 && map2 && map3 ) {
      match = 1;
      map4 = (AstMapping *) astCmpMap( map1, map2, 1, "" ); 
      map5 = (AstMapping *) astCmpMap( map4, map3, 1, "" ); 

/* Return the simplified Mapping. */
      *map = astSimplify( map5 );

/* Free resources. */
      map4 = astAnnul( map4 );
      map5 = astAnnul( map5 );
   }

/* Free resources. */
   if( map1 ) map1 = astAnnul( map1 );
   if( map2 ) map2 = astAnnul( map2 );
   if( map3 ) map3 = astAnnul( map3 );

/* If an error occurred, annul the returned Mapping and clear the returned 
   values. */
   if ( !astOK ) {
      *map = astAnnul( *map );
      match = 0;
   }

/* Return the result. */
   return match;
}

static int Match( AstFrame *template_frame, AstFrame *target,
                  int **template_axes, int **target_axes, AstMapping **map,
                  AstFrame **result ) {
/*
*  Name:
*     Match

*  Purpose:
*     Determine if conversion is possible between two coordinate systems.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     int Match( AstFrame *template, AstFrame *target,
*                int **template_axes, int **target_axes,
*                AstMapping **map, AstFrame **result )

*  Class Membership:
*     FluxFrame member function (over-rides the protected astMatch method
*     inherited from the Frame class).

*  Description:
*     This function matches a "template" FluxFrame to a "target" Frame and
*     determines whether it is possible to convert coordinates between them.
*     If it is, a mapping that performs the transformation is returned along
*     with a new Frame that describes the coordinate system that results when
*     this mapping is applied to the "target" coordinate system. In addition,
*     information is returned to allow the axes in this "result" Frame to be
*     associated with the corresponding axes in the "target" and "template"
*     Frames from which they are derived.

*  Parameters:
*     template
*        Pointer to the template FluxFrame. This describes the coordinate 
*        system (or set of possible coordinate systems) into which we wish to 
*        convert our coordinates.
*     target
*        Pointer to the target Frame. This describes the coordinate system in
*        which we already have coordinates.
*     template_axes
*        Address of a location where a pointer to int will be returned if the
*        requested coordinate conversion is possible. This pointer will point
*        at a dynamically allocated array of integers with one element for each
*        axis of the "result" Frame (see below). It must be freed by the caller
*        (using astFree) when no longer required.
*
*        For each axis in the result Frame, the corresponding element of this
*        array will return the index of the template FluxFrame axis from 
*        which it is derived. If it is not derived from any template 
*        FluxFrame axis, a value of -1 will be returned instead.
*     target_axes
*        Address of a location where a pointer to int will be returned if the
*        requested coordinate conversion is possible. This pointer will point
*        at a dynamically allocated array of integers with one element for each
*        axis of the "result" Frame (see below). It must be freed by the caller
*        (using astFree) when no longer required.
*
*        For each axis in the result Frame, the corresponding element of this
*        array will return the index of the target Frame axis from which it
*        is derived. If it is not derived from any target Frame axis, a value
*        of -1 will be returned instead.
*     map
*        Address of a location where a pointer to a new Mapping will be
*        returned if the requested coordinate conversion is possible. If
*        returned, the forward transformation of this Mapping may be used to
*        convert coordinates between the "target" Frame and the "result"
*        Frame (see below) and the inverse transformation will convert in the
*        opposite direction.
*     result
*        Address of a location where a pointer to a new Frame will be returned
*        if the requested coordinate conversion is possible. If returned, this
*        Frame describes the coordinate system that results from applying the
*        returned Mapping (above) to the "target" coordinate system. In
*        general, this Frame will combine attributes from (and will therefore
*        be more specific than) both the target and the template Frames. In
*        particular, when the template allows the possibility of transformaing
*        to any one of a set of alternative coordinate systems, the "result"
*        Frame will indicate which of the alternatives was used.

*  Returned Value:
*     A non-zero value is returned if the requested coordinate conversion is
*     possible. Otherwise zero is returned (this will not in itself result in
*     an error condition).

*  Notes:
*     -  A value of zero will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.

*  Implementation Notes:
*     This implementation addresses the matching of a FluxFrame class 
*     object to any other class of Frame. A FluxFrame will match any class 
*     of FluxFrame (i.e. possibly from a derived class) but will not match 
*     a less specialised class of Frame.
*/

/* Local Variables: */
   AstFrame *frame0;             /* Pointer to Frame underlying axis 0 */
   AstFluxFrame *template;       /* Pointer to template FluxFrame structure */
   int iaxis0;                   /* Axis index underlying axis 0 */
   int match;                    /* Coordinate conversion possible? */
   int target_naxes;             /* Number of target axes */

/* Initialise the returned values. */
   *template_axes = NULL;
   *target_axes = NULL;
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* Obtain a pointer to the template FluxFrame structure. */
   template = (AstFluxFrame *) template_frame;

/* Obtain the number of axes in the target Frame. */
   target_naxes = astGetNaxes( target );

/* Obtain pointers to the primary Frame which underlies the target axis. */
   astPrimaryFrame( target, 0, &frame0, &iaxis0 );

/* The first criterion for a match is that this Frame must be a FluxFrame 
   (or from a class derived from FluxFrame). */
   match = astIsAFluxFrame( frame0 );

/* Annul the Frame pointers used in the above tests. */
   frame0 = astAnnul( frame0 );

/* The next criterion for a match is that the template matches as a
   Frame class object. This ensures that the number of axes (1) and
   domain, etc. of the target Frame are suitable. Invoke the parent
   "astMatch" method to verify this. */
   if( match ) {
      match = (*parent_match)( template_frame, target,
                               template_axes, target_axes, map, result );
   }

/* If a match was found, annul the returned objects, which are not
   needed, but keep the memory allocated for the axis association
   arrays, which we will re-use. */
   if ( astOK && match ) {
      *map = astAnnul( *map );
      *result = astAnnul( *result );
   }

/* If the Frames still match, we next set up the axis association
   arrays. */
   if ( astOK && match ) {
      (*template_axes)[ 0 ] = 0;
      (*target_axes)[ 0 ] = 0;

/* Use the target's "astSubFrame" method to create a new Frame (the
   result Frame) with a copy of of the target axis. This process also 
   overlays the template attributes on to the target Frame and returns a 
   Mapping between the target and result Frames which effects the required 
   coordinate conversion. */
      match = astSubFrame( target, template, 1, *target_axes, *template_axes,
                           map, result );

/* If an error occurred, or conversion to the result Frame's coordinate 
   system was not possible, then free all memory, annul the returned 
   objects, and reset the returned value. */
      if ( !astOK || !match ) {
         *template_axes = astFree( *template_axes );
         *target_axes = astFree( *target_axes );
         if( *map ) *map = astAnnul( *map );
         if( *result ) *result = astAnnul( *result );
         match = 0;
      }
   }

/* Return the result. */
   return match;
}

static void Overlay( AstFrame *template, const int *template_axes,
                     AstFrame *result ) {
/*
*  Name:
*     Overlay

*  Purpose:
*     Overlay the attributes of a template FluxFrame on to another Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void Overlay( AstFrame *template, const int *template_axes,
*                   AstFrame *result )

*  Class Membership:
*     FluxFrame member function (over-rides the protected astOverlay method
*     inherited from the Frame class).

*  Description:
*     This function overlays attributes of a FluxFrame (the "template") on to
*     another Frame, so as to over-ride selected attributes of that second
*     Frame. Normally only those attributes which have been specifically set
*     in the template will be transferred. This implements a form of
*     defaulting, in which a Frame acquires attributes from the template, but
*     retains its original attributes (as the default) if new values have not
*     previously been explicitly set in the template.
*
*     Note that if the result Frame is a FluxFrame and a change of flux
*     coordinate system occurs as a result of overlaying its System
*     attribute, then some of its original attribute values may no
*     longer be appropriate (e.g. the Title, or attributes describing
*     its axes). In this case, these will be cleared before overlaying
*     any new values.

*  Parameters:
*     template
*        Pointer to the template FluxFrame, for which values should have been
*        explicitly set for any attribute which is to be transferred.
*     template_axes
*        Pointer to an array of int, with one element for each axis of the
*        "result" Frame (see below). For each axis in the result frame, the
*        corresponding element of this array should contain the (zero-based)
*        index of the template axis to which it corresponds. This array is used
*        to establish from which template axis any axis-dependent attributes
*        should be obtained.
*
*        If any axis in the result Frame is not associated with a template
*        axis, the corresponding element of this array should be set to -1.
*     result
*        Pointer to the Frame which is to receive the new attribute values.

*  Returned Value:
*     void

*  Notes:
*     -  In general, if the result Frame is not from the same class as the
*     template FluxFrame, or from a class derived from it, then attributes may
*     exist in the template FluxFrame which do not exist in the result Frame. 
*     In this case, these attributes will not be transferred.
*/


/* Local Variables: */
   AstFluxFrame *resff;          /* Result FluxFrame */
   AstFluxFrame *tmpff;          /* Template FluxFrame */
   AstSystemType new_system;     /* Code identifying new cordinates */
   AstSystemType old_system;     /* Code identifying old coordinates */
   const char *method;           /* Pointer to method string */
   const char *new_class;        /* Pointer to template class string */
   const char *old_class;        /* Pointer to result class string */
   int fluxframe;                /* Result Frame is a FluxFrame? */
   int resetSystem;              /* Was the template System value cleared? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Initialise strings used in error messages. */
   new_class = astGetClass( template );   
   old_class = astGetClass( result );   
   method = "astOverlay";

/* Get the old and new systems. */
   old_system = astGetSystem( result );
   new_system = astGetSystem( template );

/* If the result Frame is a FluxFrame, we must test to see if overlaying its
   System attribute will change the type of coordinate system it describes. 
   Determine the value of this attribute for the result and template 
   FluxFrames. */
   resetSystem = 0;
   fluxframe = astIsAFluxFrame( result );
   if( fluxframe ) {

/* If the coordinate system will change, any value already set for the result
   FluxFrame's Title will no longer be appropriate, so clear it. */
      if ( new_system != old_system ) {
         astClearTitle( result );

/* If the systems have the same default units, we can retain the current 
   Unit value. */
         if( strcmp( DefUnit( new_system, method, new_class ),
                     DefUnit( old_system, method, old_class ) ) ) { 
            astClearUnit( result, 0 );
         }

/* If necessary, clear inappropriate values for all those axis attributes
   whose access functions are over-ridden by this class (these access functions
   will then provide suitable defaults appropriate to the new coordinate system
   instead). */
         astClearLabel( result, 0 );
         astClearSymbol( result, 0 );
      }

/* Transfer the default SpecVal value and the SpecFrame. */
      resff = (AstFluxFrame *) result;
      tmpff = (AstFluxFrame *) template;
      resff->defspecval = tmpff->defspecval;
      if( resff->specframe ) astAnnul( resff->specframe );
      resff->specframe = tmpff->specframe ? astCopy( tmpff->specframe ) : NULL;

/* If the result Frame is not a FluxFrame, we must temporarily clear the
   System value since the System values used by this class are only
   appropriate to this class. */
   } else {
      if( astTestSystem( template ) ) {
         astClearSystem( template );
         resetSystem = 1;
      }
   }

/* Invoke the parent class astOverlay method to transfer attributes inherited
   from the parent class. */
   (*parent_overlay)( template, template_axes, result );

/* Reset the System value if necessary */
   if( resetSystem ) astSetSystem( template, new_system );

/* Check if the result Frame is a FluxFrame or from a class derived from
   FluxFrame. If not, we cannot transfer FluxFrame attributes to it as it is
   insufficiently specialised. In this case simply omit these attributes. */
   if ( fluxframe && astOK ) {

/* Define macros that test whether an attribute is set in the template and,
   if so, transfers its value to the result. */
#define OVERLAY(attribute) \
   if ( astTest##attribute( template ) ) { \
      astSet##attribute( result, astGet##attribute( template ) ); \
   }

/* Use the macro to transfer each FluxFrame attribute in turn. */
      OVERLAY(SpecVal)


   }

/* Undefine macros local to this function. */
#undef OVERLAY
}

static void SetAttrib( AstObject *this_object, const char *setting ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void SetAttrib( AstObject *this, const char *setting )

*  Class Membership:
*     FluxFrame member function (extends the astSetAttrib method inherited from
*     the Mapping class).

*  Description:
*     This function assigns an attribute value for a FluxFrame, the attribute
*     and its value being specified by means of a string of the form:
*
*        "attribute= value "
*
*     Here, "attribute" specifies the attribute name and should be in lower
*     case with no white space present. The value to the right of the "="
*     should be a suitable textual representation of the value to be assigned
*     and this will be interpreted according to the attribute's data type.
*     White space surrounding the value is only significant for string
*     attributes.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.

*  Returned Value:
*     void

*  Notes:
*     This protected method is intended to be invoked by the Object astSet
*     method and makes additional attributes accessible to it.
*/

/* Local Vaiables: */
   AstFluxFrame *this;           /* Pointer to the FluxFrame structure */
   double dval;                  /* Floating point attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* No. of characters read */
   int ulen;                     /* Used length of setting string */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_object;

/* Obtain the length of the setting string. */
   len = strlen( setting );

/* Obtain the used length of the setting string. */
   ulen = astChrLen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse the
   setting string and extract the attribute value (or an offset to it in the
   case of string values). In each case, use the value set in "nc" to check
   that the entire string was matched. Once a value has been obtained, use the
   appropriate method to set it. */

/* SpecVal. */
/* -------- */
   if ( nc = 0,
        ( 1 == astSscanf( setting, "specval= %lg %n", &dval, &nc ) )
        && ( nc >= len ) ) {
      astSetSpecVal( this, dval );

/* Pass any unrecognised setting to the parent method for further
   interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting );
   }
}

static void SetMaxAxes( AstFrame *this_frame, int maxaxes ) {
/*
*  Name:
*     SetMaxAxes

*  Purpose:
*     Set a value for the MaxAxes attribute of a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void SetMaxAxes( AstFrame *this, int maxaxes )

*  Class Membership:
*     FluxFrame member function (over-rides the astSetMaxAxes method
*     inherited from the Frame class).

*  Description:
*     This function sets the MaxAxes value for a FluxFrame to 1, which 
*     is the only valid value for a FluxFrame, regardless of the value 
*     supplied.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     maxaxes
*        The new value to be set (ignored).

*  Returned Value:
*     void.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Use the parent astSetMaxAxes method to set a value of 1. */
   (*parent_setmaxaxes)( this_frame, 1 );
}

static void SetMinAxes( AstFrame *this_frame, int minaxes ) {
/*
*  Name:
*     SetMinAxes

*  Purpose:
*     Set a value for the MinAxes attribute of a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void SetMinAxes( AstFrame *this, int minaxes )

*  Class Membership:
*     FluxFrame member function (over-rides the astSetMinAxes method
*     inherited from the Frame class).

*  Description:
*     This function sets the MinAxes value for a FluxFrame to 1, which is 
*     the only valid value for a FluxFrame, regardless of the value 
*     supplied.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     minaxes
*        The new value to be set (ignored).

*  Returned Value:
*     void.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Use the parent astSetMinAxes method to set a value of 1. */
   (*parent_setminaxes)( this_frame, 1 );
}

static void SetSystem( AstFrame *this_frame, AstSystemType newsys ) {
/*
*  Name:
*     SetSystem

*  Purpose:
*     Set the System attribute for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void SetSystem( AstFrame *this_frame, AstSystemType newsys )

*  Class Membership:
*     FluxFrame member function (over-rides the astSetSystem protected
*     method inherited from the Frame class).

*  Description:
*     This function sets the System attribute for a FluxFrame.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     newsys
*        The new System value to be stored.

*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to FluxFrame structure */
   AstSystemType oldsys;         /* Original System value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* Save the original System value */
   oldsys = astGetSystem( this_frame );

/* Use the parent SetSystem method to store the new System value. */
   (*parent_setsystem)( this_frame, newsys );

/* If the system has changed... */
   if( oldsys != newsys ) {

/* Changing the System value will in general require the Units to change
   as well. If the user has previously specified the units to be used with
   the new system, then re-instate them (they are stored in the "usedunits"
   array in the FluxFrame structure). Otherwise, clear the units so that
   the default units will eb used with the new System. */
      if( (int) newsys < this->nuunits && this->usedunits &&
          this->usedunits[ (int) newsys ] ) {
         (*parent_setunit)( this_frame, 0, this->usedunits[ (int) newsys ] );
      } else {
         (*parent_clearunit)( this_frame, 0 );
      }

/* Also, clear all attributes which have system-specific defaults. */
      astClearLabel( this_frame, 0 );
      astClearSymbol( this_frame, 0 );
      astClearTitle( this_frame );
   }
}

static void SetUnit( AstFrame *this_frame, int axis, const char *value ) {
/*
*  Name:
*     astSetUnit

*  Purpose:
*     Set a pointer to the Unit string for a FluxFrame's axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     void SetUnit( AstFrame *this_frame, int axis, const char *value )

*  Class Membership:
*     FluxFrame member function (over-rides the astSetUnit method inherited
*     from the Frame class).

*  Description:
*     This function stores a pointer to the Unit string for a specified axis
*     of a FluxFrame. It also stores the string in the "usedunits" array
*     in the FluxFrame structure, in the element associated with the
*     current System.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     axis
*        The number of the axis (zero-based) for which information is required.
*     unit
*        The new string to store.
*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to the FluxFrame structure */
   AstSystemType system;         /* The FluxFrame's System value */
   int i;                        /* Loop counter */
   int isystem;                  /* The FluxFrame's System value */

/* Check the global error status. */
   if ( !astOK ) return;

/* Use the parent SetUnit method to store the value in the Axis
   structure */
   (*parent_setunit)( this_frame, axis, value );

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_frame;

/* Validate the axis index. */
   astValidateAxis( this, axis, "astSetUnit" );

/* If the new units are appropriate for the current System, store the 
   supplied value as the UsedUnit for the current System. First ensure the 
   array is big enough. Free any previous value stored for the current 
   system. */
   system = astGetSystem( this );
   if( UnitsOK( system, value, 0, "astSetUnit", astGetClass( this ) ) ) {
      isystem = (int) astGetSystem( this );
      if( isystem >= this->nuunits ) {
         this->usedunits = astGrow( this->usedunits, isystem + 1, 
                                    sizeof(char *) );
         if( astOK ) {
            for( i = this->nuunits; i < isystem + 1; i++ ) this->usedunits[ i ] = NULL;
            this->nuunits = isystem + 1;
         }
      }

/* Now store a copy of the value, if it is different to the stored string. */
      if( astOK && ( !this->usedunits[ isystem ] ||
                     strcmp( this->usedunits[ isystem ], value ) ) ) {
         this->usedunits[ isystem ] = astStore( this->usedunits[ isystem ],
                                               value, strlen( value ) + 1 );
      }

/* If the new units are not appropriate for the current System, clear the
   System value. Use the parent ClearSystem function since the 
   astClearSystem implemented by this class will clear the units. */
   } else {
      (*parent_clearsystem)( this_frame );
   }

}

static int SubFrame( AstFrame *target_frame, AstFrame *template,
                     int result_naxes, const int *target_axes,
                     const int *template_axes, AstMapping **map,
                     AstFrame **result ) {
/*
*  Name:
*     SubFrame

*  Purpose:
*     Select axes from a FluxFrame and convert to the new coordinate 
*     system.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     int SubFrame( AstFrame *target, AstFrame *template,
*                   int result_naxes, const int *target_axes,
*                   const int *template_axes, AstMapping **map,
*                   AstFrame **result )

*  Class Membership:
*     FluxFrame member function (over-rides the protected astSubFrame 
*     method inherited from the Frame class).

*  Description:
*     This function selects a requested sub-set (or super-set) of the axes
*     from a "target" FluxFrame and creates a new Frame with copies of
*     the selected axes assembled in the requested order. It then
*     optionally overlays the attributes of a "template" Frame on to the
*     result. It returns both the resulting Frame and a Mapping that
*     describes how to convert between the coordinate systems described by
*     the target and result Frames. If necessary, this Mapping takes
*     account of any differences in the Frames' attributes due to the
*     influence of the template.

*  Parameters:
*     target
*        Pointer to the target FluxFrame, from which axes are to be 
*        selected.
*     template
*        Pointer to the template Frame, from which new attributes for the
*        result Frame are to be obtained. Optionally, this may be NULL, in
*        which case no overlaying of template attributes will be performed.
*     result_naxes
*        Number of axes to be selected from the target Frame. This number may
*        be greater than or less than the number of axes in this Frame (or
*        equal).
*     target_axes
*        Pointer to an array of int with result_naxes elements, giving a list
*        of the (zero-based) axis indices of the axes to be selected from the
*        target FluxFrame. The order in which these are given determines
*        the order in which the axes appear in the result Frame. If any of the
*        values in this array is set to -1, the corresponding result axis will
*        not be derived from the target Frame, but will be assigned default
*        attributes instead.
*     template_axes
*        Pointer to an array of int with result_naxes elements. This should
*        contain a list of the template axes (given as zero-based axis indices)
*        with which the axes of the result Frame are to be associated. This
*        array determines which axes are used when overlaying axis-dependent
*        attributes of the template on to the result. If any element of this
*        array is set to -1, the corresponding result axis will not receive any
*        template attributes.
*
*        If the template argument is given as NULL, this array is not used and
*        a NULL pointer may also be supplied here.
*     map
*        Address of a location to receive a pointer to the returned Mapping.
*        The forward transformation of this Mapping will describe how to
*        convert coordinates from the coordinate system described by the target
*        FluxFrame to that described by the result Frame. The inverse
*        transformation will convert in the opposite direction.
*     result
*        Address of a location to receive a pointer to the result Frame.

*  Returned Value:
*     A non-zero value is returned if coordinate conversion is possible
*     between the target and the result Frame. Otherwise zero is returned and
*     *map and *result are returned as NULL (but this will not in itself
*     result in an error condition). In general, coordinate conversion should
*     always be possible if no template Frame is supplied but may not always
*     be possible otherwise.

*  Notes:
*     -  A value of zero will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.

*  Implementation Notes:
*     -  This implementation addresses the selection of axes from a 
*     FluxFrame object. This results in another object of the same class 
*     only if the single FluxFrame axis is selected exactly once. 
*     Otherwise, the result is a Frame class object which inherits the 
*     FluxFrame's axis information (if appropriate) but none of the other 
*     properties of a FluxFrame.
*     -  In the event that a FluxFrame results, the returned Mapping will 
*     take proper account of the relationship between the target and result 
*     coordinate systems.
*     -  In the event that a Frame class object results, the returned Mapping
*     will only represent a selection/permutation of axes.

*  Implementation Deficiencies:
*     -  Any axis selection is currently permitted. Probably this should be
*     restricted so that each axis can only be selected once. The
*     astValidateAxisSelection method will do this but currently there are bugs
*     in the CmpFrame class that cause axis selections which will not pass this
*     test. Install the validation when these are fixed.
*/

/* Local Variables: */
   AstFluxFrame *target;      /* Pointer to the FluxFrame structure */
   AstFluxFrame *temp;        /* Pointer to copy of target FluxFrame */
   AstSystemType align_sys;   /* System in which to align the FluxFrames */
   int match;                 /* Coordinate conversion is possible? */
   int report;                /* Report errors if FluxFrames cannot be aligned? */

/* Initialise the returned values. */
   *map = NULL;
   *result = NULL;
   match = 0;

/* Check the global error status. */
   if ( !astOK ) return match;

/* Obtain a pointer to the target FluxFrame structure. */
   target = (AstFluxFrame *) target_frame;

/* Result is a FluxFrame. */
/* -------------------------- */
/* Check if the result Frame is to have one axis obtained by selecting
   the single target FluxFrame axis. If so, the result will also be 
   a FluxFrame. */
   if ( ( result_naxes == 1 ) && ( target_axes[ 0 ] == 0 ) ) {

/* Form the result from a copy of the target. */
      *result = astCopy( target );

/* Initialise a flag to indicate that MakeFluxMapping should not report
   errors if no Mapping can be created. */
      report = 0;

/* If required, overlay the template attributes on to the result FluxFrame.
   Also get the system in which to align the two FluxFrames. These are the 
   values from the template (if there is a template). */
      if ( template ) {
         astOverlay( template, template_axes, *result );
         if( astIsAFluxFrame( template ) ) {
            align_sys = astGetAlignSystem( template );

/* Since we now know that both the template and target are FluxFrames, it 
   should usually be possible to convert betwen them. If conversion is
   *not* possible then the user will probably be interested in knowing the 
   reason why conversion is not possible. Therefore, indicate that 
   MakeFluxMapping should report errors if no Mapping can be created. */
            report = 1;

         } else {
            align_sys = astGetAlignSystem( target );
         }

/* If no template was supplied, align in the System of the target. */
      } else {
         align_sys = astGetSystem( target );
      }

/* Generate a Mapping that takes account of changes in the coordinate system 
   between the target FluxFrame and the result FluxFrame. If this Mapping can 
   be generated, set "match" to indicate that coordinate conversion is 
   possible. If the template is a fluxframe, report errors if a match is not 
   possible. */
      match = ( MakeFluxMapping( target, (AstFluxFrame *) *result, 
                align_sys, map ) != 0 );

/* Result is not a FluxFrame. */
/* ------------------------------ */
/* In this case, we select axes as if the target were from the Frame
   class.  However, since the resulting data will then be separated
   from their enclosing FluxFrame, default attribute values may differ
   if the methods for obtaining them were over-ridden by the FluxFrame
   class. To overcome this, we ensure that these values are explicitly
   set for the result Frame (rather than relying on their defaults). */
   } else {

/* Make a temporary copy of the target FluxFrame. We will explicitly
   set the attribute values in this copy so as not to modify the original. */
      temp = astCopy( target );

/* Define a macro to test if an attribute is set. If not, set it
   explicitly to its default value. */
#define SET(attribute) \
   if ( !astTest##attribute( temp ) ) { \
      astSet##attribute( temp, astGet##attribute( temp ) ); \
   }

/* Set attribute values which apply to the Frame as a whole and which
   we want to retain, but whose defaults are over-ridden by the
   FluxFrame class. */
      SET(Domain)
      SET(Title)

/* Define a macro to test if an attribute is set for axis zero (the only
   axis of a FluxFrame). If not, set it explicitly to its default value. */
#define SET_AXIS(attribute) \
   if ( !astTest##attribute( temp, 0 ) ) { \
      astSet##attribute( temp, 0, \
                         astGet##attribute( temp, 0 ) ); \
   }

/* Use this macro to set explicit values for all the axis attributes
   for which the FluxFrame class over-rides the default value. */
      SET_AXIS(Label)
      SET_AXIS(Symbol)
      SET_AXIS(Unit)

/* Clear attributes which have an extended range of values allowed by
   this class. */
      astClearSystem( temp );

/* Invoke the astSubFrame method inherited from the Frame class to
   produce the result Frame by selecting the required set of axes and
   overlaying the template Frame's attributes. */
      match = (*parent_subframe)( (AstFrame *) temp, template,
                                  result_naxes, target_axes, template_axes,
                                  map, result );

/* Delete the temporary copy of the target FluxFrame. */
      temp = astDelete( temp );
   }

/* If an error occurred or no match was found, annul the returned
   objects and reset the returned result. */
   if ( !astOK || !match ) {
      if( *map ) *map = astAnnul( *map );
      if( *result ) *result = astAnnul( *result );
      match = 0;
   }

/* Return the result. */
   return match;

/* Undefine macros local to this function. */
#undef SET
#undef SET_AXIS
}

static AstSystemType SystemCode( AstFrame *this, const char *system ) {
/*
*  Name:
*     SystemCode

*  Purpose:
*     Convert a string into a coordinate system type code.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     AstSystemType SystemCode( AstFrame *this, const char *system )

*  Class Membership:
*     FluxFrame member function (over-rides the astSystemCode method
*     inherited from the Frame class).

*  Description:
*     This function converts a string used for the external
*     description of a coordinate system into a FluxFrame
*     coordinate system type code (System attribute value). It is the
*     inverse of the astSystemString function.

*  Parameters:
*     this
*        The Frame.
*     system
*        Pointer to a constant null-terminated string containing the
*        external description of the sky coordinate system.

*  Returned Value:
*     The System type code.

*  Notes:
*     - A value of AST__BADSYSTEM is returned if the sky coordinate
*     system description was not recognised. This does not produce an
*     error.
*     - A value of AST__BADSYSTEM is also returned if this function
*     is invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   AstSystemType result;      /* Result value to return */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "system" string against each possibility and assign the
   result. */
   if ( astChrMatch( "FLXDN", system ) ) {
      result = AST__FLUXDEN;

   } else if ( astChrMatch( "FLXDNW", system ) ) {
      result = AST__FLUXDENW;

   }else if ( astChrMatch( "SFCBR", system ) ) {
      result = AST__SBRIGHT;

   } else if ( astChrMatch( "SRCBR", system ) ) {
      result = AST__SBRIGHTW;

   }

/* Return the result. */
   return result;
}

static const char *SystemLabel( AstSystemType system ) {
/*
*  Name:
*     SystemLabel

*  Purpose:
*     Return a label for a coordinate system type code.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *SystemLabel( AstSystemType system )

*  Class Membership:
*     FluxFrame member function.

*  Description:
*     This function converts a FluxFrame coordinate system type code
*     (System attribute value) into a descriptive string for human readers.

*  Parameters:
*     system
*        The coordinate system type code.

*  Returned Value:
*     Pointer to a constant null-terminated string containing the
*     textual equivalent of the type code supplied.

*  Notes:
*     - A NULL pointer value is returned if the sky coordinate system
*     code was not recognised. This does not produce an error.
*     - A NULL pointer value is also returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

/* Local Variables: */
   const char *result;           /* Pointer value to return */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Match the "system" value against each possibility and convert to a
   string pointer. */
   switch ( system ) {

   case AST__FLUXDEN:
      result = "flux density";
      break;

   case AST__FLUXDENW:
      result = "flux wavelength density";
      break;

   case AST__SBRIGHT:
      result = "surface brightness";
      break;

   case AST__SBRIGHTW:
      result = "surface brightness (per wavelength)";
      break;

   }

/* Return the result pointer. */
   return result;
}

static const char *SystemString( AstFrame *this, AstSystemType system ) {
/*
*  Name:
*     SystemString

*  Purpose:
*     Convert a coordinate system type code into a string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     const char *SystemString( AstFrame *this, AstSystemType system )

*  Class Membership:
*     FluxFrame member function (over-rides the astSystemString method
*     inherited from the Frame class).

*  Description:
*     This function converts a FluxFrame coordinate system type code
*     (System attribute value) into a string suitable for use as an
*     external representation of the coordinate system type.

*  Parameters:
*     this
*        The Frame.
*     system
*        The coordinate system type code.

*  Returned Value:
*     Pointer to a constant null-terminated string containing the
*     textual equivalent of the type code supplied.

*  Notes:
*     - A NULL pointer value is returned if the coordinate system
*     code was not recognised. This does not produce an error.
*     - A NULL pointer value is also returned if this function is
*     invoked with the global error status set or if it should fail
*     for any reason.
*/

   return FluxSystemString( system );
}

static int TestActiveUnit( AstFrame *this_frame ) {
/*
*  Name:
*     TestActiveUnit

*  Purpose:
*     Test the ActiveUnit flag for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     int TestActiveUnit( AstFrame *this_frame ) 

*  Class Membership:
*     FluxFrame member function (over-rides the astTestActiveUnit protected
*     method inherited from the Frame class).

*  Description:
*    This function test the value of the ActiveUnit flag for a FluxFrame, 
*    which is always "unset". 

*  Parameters:
*     this
*        Pointer to the FluxFrame.

*  Returned Value:
*     The result of the test (0).

*/
   return 0;
}

static int TestAttrib( AstObject *this_object, const char *attrib ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a FluxFrame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     int TestAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     FluxFrame member function (over-rides the astTestAttrib protected
*     method inherited from the Frame class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a FluxFrame's attributes.

*  Parameters:
*     this
*        Pointer to the FluxFrame.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.

*  Returned Value:
*     One if a value has been set, otherwise zero.

*  Notes:
*     - This function uses one-based axis numbering so that it is
*     suitable for external (public) use.
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to the FluxFrame structure */
   int len;                      /* Length of attrib string */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and test the appropriate attribute. */

/* SpecVal. */
/* -------- */
   if ( !strcmp( attrib, "specval" ) ) {
      result = astTestSpecVal( this );

/* If the attribute is not recognised, pass it on to the parent method
   for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib );
   }

/* Return the result, */
   return result;
}

static int UnitsOK( AstSystemType system, const char *units, int report, 
                    const char *method, const char *class ) {
/*
*  Name:
*     UnitsOK

*  Purpose:
*     Check if a units string is appropriate for the current System.

*  Type:
*     Private function.

*  Synopsis:
*     #include "fluxframe.h"
*     int UnitsOK( AstSystemType system, const char *units, int report, 
*                  const char *method, const char *class ) 

*  Class Membership:
*     FluxFrame member function 

*  Description:
*     This function returns a non-zero value if the supplied units string
*     can be mapped to the defaultunits for the current System in the
*     supplied FluxFrame.

*  Parameters:
*     system 
*        The system type to check.
*     unit
*        The units string to check.
*     report
*        Should an error be reported if the units and system are
*        inconsistent?
*     method
*        String holding a method name to be used in error messages.
*     class
*        String holding a class name to be used in error messages.

*  Returns Value:
*     Non-zero if the units string can be used to describe the current
*     flux System. Zero otherwise.

*/

/* Local Variables: */
   AstMapping *map;
   int result;

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Get the Mapping from the default units for the supplied system to the
   supplied Units. */
   map = astUnitMapper( DefUnit( system, method, class ), units, NULL, NULL );

/* If a Mapping was found succesfully, annul it and return non-zero.
   Otherwise return zero. */
   if( map ) {
      result = 1;
      map = astAnnul( map );

   } else {
      result = 0;

/* Report an error if required. */
      if( report && astOK ) {
         astError( AST__BADUN, "%s(%s): The units (%s) and system (%s) "
                   "within the supplied %s are inconsistent.", method,
                   class, units, FluxSystemString( system ), class );
      }
   }

/* Return the result. */
   return result;
}

static int ValidateSystem( AstFrame *this, AstSystemType system, const char *method ) {
/*
*
*  Name:
*     ValidateSystem

*  Purpose:
*     Validate a value for a Frame's System attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "fluxframe.h"
*     int ValidateSystem( AstFrame *this, AstSystemType system, 
*                         const char *method )

*  Class Membership:
*     FluxFrame member function (over-rides the astValidateSystem method
*     inherited from the Frame class).

*  Description:
*     This function checks the validity of the supplied system value.
*     If the value is valid, it is returned unchanged. Otherwise, an
*     error is reported and a value of AST__BADSYSTEM is returned.

*  Parameters:
*     this
*        Pointer to the Frame.
*     system
*        The system value to be checked. 
*     method
*        Pointer to a constant null-terminated character string
*        containing the name of the method that invoked this function
*        to validate an axis index. This method name is used solely
*        for constructing error messages.

*  Returned Value:
*     The validated system value.

*  Notes:
*     - A value of AST__BADSYSTEM will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   AstSystemType result;              /* Validated system value */

/* Initialise. */
   result = AST__BADSYSTEM;

/* Check the global error status. */
   if ( !astOK ) return result;

/* If the value is out of bounds, report an error. */
   if ( system < FIRST_SYSTEM || system > LAST_SYSTEM ) {
         astError( AST__AXIIN, "%s(%s): Bad value (%d) given for the System "
                   "attribute of a %s.", method, astGetClass( this ),
                   (int) system, astGetClass( this ) );

/* Otherwise, return the supplied value. */
   } else {
      result = system;
   }

/* Return the result. */
   return result;
}

/* Functions which access class attributes. */
/* ---------------------------------------- */
/*
*att++
*  Name:
*     SpecVal

*  Purpose:
*     The spectral position at which flux values are measured.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute specifies the spectral position (frequency, wavelength, 
*     etc.), at which the values described by the FluxFrame are measured.
*     It is used when determining the Mapping between between FluxFrames.
*
*     The default value and spectral system used for this attribute are
*     both specified when the FluxFrame is created.

*  Applicability:
*     FluxFrame
*        All FluxFrames have this attribute.

*att--
*/
astMAKE_CLEAR(FluxFrame,SpecVal,specval,AST__BAD)
astMAKE_GET(FluxFrame,SpecVal,double,AST__BAD,((this->specval!=AST__BAD)?this->specval:this->defspecval))
astMAKE_SET(FluxFrame,SpecVal,double,specval,value)
astMAKE_TEST(FluxFrame,SpecVal,( this->specval != AST__BAD ))

/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for FluxFrame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout )

*  Description:
*     This function implements the copy constructor for FluxFrame objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstFluxFrame *in;             /* Pointer to input FluxFrame */
   AstFluxFrame *out;            /* Pointer to output FluxFrame */
   char *usedunit;               /* Pointer to an element of usedunits array */
   int i;                        /* Loop count */
   int nused;                    /* Size of "usedunits" array */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output FluxFrames. */
   in = (AstFluxFrame *) objin;
   out = (AstFluxFrame *) objout;

/* Nullify the pointers stored in the output object since these will
   currently be pointing at the input data (since the output is a simple
   byte-for-byte copy of the input). Otherwise, the input data could be
   freed by accidient if the output object is deleted due to an error
   occuring in this function. */
   out->usedunits = NULL;
   out->specframe = NULL;

/* Store the last used units in the output SpecMap. */
   if( in && in->usedunits ) {
      nused = in->nuunits;
      out->usedunits = astMalloc( nused*sizeof( char * ) );
      if( out->usedunits ) {
         for( i = 0; i < nused; i++ ) {
            usedunit = in->usedunits[ i ];
            if( usedunit ) {
               out->usedunits[ i ] = astStore( NULL, usedunit, 
                                               strlen( usedunit ) + 1 );
            } else {
               out->usedunits[ i ] = NULL;
            }
         }
      }
   }

/* Copy the SpecFrame */
   if( in->specframe ) out->specframe = astCopy( in->specframe );

/* If an error has occurred, free the output resources. */
   if( !astOK ) Delete( (AstObject *) out );

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for FluxFrame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj )

*  Description:
*     This function implements the destructor for FluxFrame objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/
   
/* Local Variables: */
   AstFluxFrame *this;
   int i;

/* Release the memory referred to in the FluxFrame structure. */
   this = (AstFluxFrame *) obj;
   if( this && this->usedunits ) {
      for( i = 0; i < this->nuunits; i++ ) {
         this->usedunits[ i ] = astFree( this->usedunits[ i ] );
      }
      this->usedunits = astFree( this->usedunits );
   }

/* Annulthe SpecFrame. */
   if( this->specframe ) this->specframe = astAnnul( this->specframe );

}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for FluxFrame objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel )

*  Description:
*     This function implements the Dump function which writes out data
*     for the FluxFrame class to an output Channel.

*  Parameters:
*     this
*        Pointer to the FluxFrame whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*/

/* Local Variables: */
   AstFluxFrame *this;           /* Pointer to the FluxFrame structure */
   char buff[ 20 ];              /* Buffer for item name */
   char comm[ 50 ];              /* Buffer for comment */
   double dval;                  /* Double value */
   int i;                        /* Loop count */
   int j;                        /* Loop count */
   int set;                      /* Attribute value set? */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the FluxFrame structure. */
   this = (AstFluxFrame *) this_object;

/* Write out values representing the instance variables for the
   FluxFrame class.  Accompany these with appropriate comment strings,
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

/* SpecVal. */
/* -------- */
   set = TestSpecVal( this );
   dval = set ? GetSpecVal( this ) : astGetSpecVal( this );
   if( dval != AST__BAD ) {
      astWriteDouble( channel, "SpcVl", set, 0, dval, "Spectral position" );
   }

/* The SpecFrame */
/* ------------- */
   if( this->specframe ) {
      astWriteObject( channel, "SpcFr", 1, 0, this->specframe, "SpcVl coord system" );
   }

/* Default SpecVal. */
/* ---------------- */
   if( this->defspecval != AST__BAD ) {
      astWriteDouble( channel, "DfSpc", 1, 0, this->defspecval, "Default spectral position" );
   }

/* UsedUnits */
/* --------- */
   if( this->usedunits ) {
      for( i = 0; i < this->nuunits; i++ ) {
         if( this->usedunits[ i ] ) {
            sprintf( buff, "U%s", astSystemString( this, (AstSystemType) i ));
            for( j = 2; j < strlen( buff ); j++ ) buff[ j ] = tolower( buff[ j ] );
            sprintf( comm, "Preferred units for %s", SystemLabel( (AstSystemType) i ) );
            astWriteString( channel, buff, 1, 0, this->usedunits[ i ], comm );
         }
      }
   }
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAFluxFrame and astCheckFluxFrame functions using the 
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(FluxFrame,Frame,check,&class_init)
astMAKE_CHECK(FluxFrame)

AstFluxFrame *astFluxFrame_( double specval, void *specfrm_void, 
                             const char *options, ... ) {
/*
*+
*  Name:
*     astFluxFrame

*  Purpose:
*     Create a FluxFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fluxframe.h"
*     AstFluxFrame *astFluxFrame( double specval, AstSpecFrame *specfrm, 
*                                 const char *options, ... )

*  Class Membership:
*     FluxFrame constructor.

*  Description:
*     This function creates a new FluxFrame and optionally initialises its
*     attributes.

*  Parameters:
*     specval
*        The spectral value to which the flux values refer, given in the
*        spectral coordinate system specified by "specfrm". The value
*        supplied for the "specval" parameter becomes the default value for 
*        the SpecVal attribute.
*     specfrm
*        A pointer to a SpecFrame describing the spectral coordinate system 
*        in which the "specval" parameter is given. A deep copy of this object 
*        is taken, so any subsequent changes to the SpecFrame using the 
*        supplied pointer will have no effect on the new FluxFrame.
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new FluxFrame. The syntax used is the same as for the
*        astSet method and may include "printf" format specifiers identified
*        by "%" symbols in the normal way.
*     ...
*        If the "options" string contains "%" format specifiers, then an
*        optional list of arguments may follow it in order to supply values to
*        be substituted for these specifiers. The rules for supplying these
*        are identical to those for the astSet method (and for the C "printf"
*        function).

*  Returned Value:
*     A pointer to the new FluxFrame.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-

*  Implementation Notes:
*     - This function implements the basic FluxFrame constructor which
*     is available via the protected interface to the FluxFrame class.
*     A public interface is provided by the astFluxFrameId_ function.
*/

/* Local Variables: */
   AstMapping *um;               /* Mapping from default to actual units */
   AstFluxFrame *new;            /* Pointer to new FluxFrame */
   AstSpecFrame *sfrm;           /* Pointer to SpecFrame */
   AstSystemType s;              /* System */
   const char *u;                /* Units string */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the SpecFrame structures provided. */
   sfrm = specfrm_void ? astCheckSpecFrame( specfrm_void ) : NULL;

/* Initialise the FluxFrame, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitFluxFrame( NULL, sizeof( AstFluxFrame ), !class_init, 
                           &class_vtab, "FluxFrame", specval, sfrm );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new FluxFrame's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* Check the Units are appropriate for the System. */
      u = astGetUnit( new, 0 );
      s = astGetSystem( new );
      um = astUnitMapper( DefUnit( s, "astFluxFrame", "FluxFrame" ), 
                          u, NULL, NULL );
      if( um ) {
         um = astAnnul( um );
      } else {
         astError( AST__BADUN, "astFluxFrame: Inappropriate units (%s) "
                   "specified for a %s axis.", u, SystemLabel( s ) );
      }      

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return a pointer to the new FluxFrame. */
   return new;
}

AstFluxFrame *astInitFluxFrame_( void *mem, size_t size, int init,
                                 AstFluxFrameVtab *vtab, const char *name,
                                 double specval, AstSpecFrame *specfrm ) {
/*
*+
*  Name:
*     astInitFluxFrame

*  Purpose:
*     Initialise a FluxFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fluxframe.h"
*     AstFluxFrame *astInitFluxFrame( void *mem, size_t size, int init,
*                                     AstFrameVtab *vtab, const char *name,
*                                     double specval, AstSpecFrame *specfrm)

*  Class Membership:
*     FluxFrame initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new FluxFrame object. It allocates memory (if
*     necessary) to accommodate the FluxFrame plus any additional data
*     associated with the derived class. It then initialises a
*     FluxFrame structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual function
*     table for a FluxFrame at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the FluxFrame is to be
*	created. This must be of sufficient size to accommodate the
*	FluxFrame data (sizeof(FluxFrame)) plus any data used by
*	the derived class. If a value of NULL is given, this function
*	will allocate the memory itself using the "size" parameter to
*	determine its size.
*     size
*        The amount of memory used by the FluxFrame (plus derived
*	class data). This will be used to allocate memory if a value of
*	NULL is given for the "mem" parameter. This value is also stored
*	in the FluxFrame structure, so a valid value must be supplied
*	even if not required for allocating memory.
*     init
*        A logical flag indicating if the FluxFrame's virtual function
*	table is to be initialised. If this value is non-zero, the
*	virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*	associated with the new FluxFrame.
*     name
*        Pointer to a constant null-terminated character string which
*	contains the name of the class to which the new object belongs
*	(it is this pointer value that will subsequently be returned by
*	the astGetClass method).
*     specval
*        The spectral value to which the flux values refer, given in the
*        spectral coordinate system specified by "specfrm". The value
*        supplied for the "specval" parameter becomes the default value for 
*        the SpecVal attribute. May be AST__BAD.
*     specfrm
*        A pointer to a SpecFrame describing the spectral coordinate system 
*        in which the "specval" parameter is given. A deep copy of this object 
*        is taken, so any subsequent changes to the SpecFrame using the 
*        supplied pointer will have no effect on the new FluxFrame. Should 
*        be NULL if "specval" is AST__BAD.

*  Returned Value:
*     A pointer to the new FluxFrame.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstFluxFrame *new;        /* Pointer to the new FluxFrame */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitFluxFrameVtab( vtab, name );

/* Initialise a 1D Frame structure (the parent class) as the first component
   within the FluxFrame structure, allocating memory if necessary. */
   new = (AstFluxFrame *) astInitFrame( mem, size, 0,
                                        (AstFrameVtab *) vtab, name, 1 );

   if ( astOK ) {

/* Initialise the FluxFrame data. */
/* ----------------------------- */
/* Initialise all attributes to their "undefined" values. */
      new->specval = AST__BAD;
      new->defspecval = specval;
      new->specframe = specfrm ? astCopy( specfrm ) : NULL;
      new->nuunits = 0;
      new->usedunits = NULL;

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );

   }

/* Return a pointer to the new object. */
   return new;
}

AstFluxFrame *astLoadFluxFrame_( void *mem, size_t size, AstFluxFrameVtab *vtab, 
                                 const char *name, AstChannel *channel ) {
/*
*+
*  Name:
*     astLoadFluxFrame

*  Purpose:
*     Load a FluxFrame.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "fluxframe.h"
*     AstFluxFrame *astLoadFluxFrame( void *mem, size_t size, AstFluxFrameVtab *vtab, 
*                                      const char *name, AstChannel *channel )

*  Class Membership:
*     FluxFrame loader.

*  Description:
*     This function is provided to load a new FluxFrame using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     FluxFrame structure in this memory, using data read from the
*     input Channel.

*  Parameters:
*     mem
*        A pointer to the memory into which the FluxFrame is to be
*        loaded.  This must be of sufficient size to accommodate the
*        FluxFrame data (sizeof(FluxFrame)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the FluxFrame (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the FluxFrame structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstFluxFrame) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new FluxFrame. If this is NULL, a pointer
*        to the (static) virtual function table for the FluxFrame class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "FluxFrame" is used instead.

*  Returned Value:
*     A pointer to the new FluxFrame.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Variables: */
   AstFluxFrame *new;            /* Pointer to the new FluxFrame */
   char buff[ 20 ];              /* Buffer for item name */
   char *sval;                   /* Pointer to string value */
   int i;                        /* Loop count */
   int j;                        /* Loop count */
   int sys;                      /* System value */

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if ( !astOK ) return new;

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this FluxFrame. In this case the
   FluxFrame belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstFluxFrame );
      vtab = &class_vtab;
      name = "FluxFrame";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitFluxFrameVtab( vtab, name );
         class_init = 1;
      }
   }

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built FluxFrame. */
   new = astLoadFrame( mem, size, (AstFrameVtab *) vtab, name,
                       channel );
   if ( astOK ) {

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
       astReadClassData( channel, "FluxFrame" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Default SpecVal */
/* --------------- */
      new->defspecval = astReadDouble( channel, "dfspc", AST__BAD );

/* SpecFrame  */
/* ---------- */
      new->specframe = astReadObject( channel, "spcfr", NULL );
      
/* SpecVal */
/* ------- */
      new->specval = astReadDouble( channel, "spcvl", AST__BAD );
      if ( TestSpecVal( new ) ) SetSpecVal( new, new->specval );

/* UsedUnits */
/* --------- */
      new->nuunits = 0;
      new->usedunits = NULL;
      for( sys = FIRST_SYSTEM; sys <= LAST_SYSTEM; sys++ ) {
         sprintf( buff, "u%s", astSystemString( new, (AstSystemType) sys ));
         for( j = 0; j < strlen( buff ); j++ ) buff[ j ] = tolower( buff[ j ] );
         sval = astReadString( channel, buff, NULL );
         if( sval ) {
            if( (int) sys >= new->nuunits ) {
               new->usedunits = astGrow( new->usedunits, sys + 1, 
                                          sizeof(char *) );
               if( astOK ) {
                  for( i = new->nuunits; i < sys + 1; i++ ) new->usedunits[ i ] = NULL;
                  new->nuunits = sys + 1;
               }
            } else {
               new->usedunits[ sys ] = astFree( new->usedunits[ sys ] );
            }
            if( astOK ) {
               new->usedunits[ sys ] = astStore( new->usedunits[ sys ],
                                                 sval, strlen( sval ) + 1 );
            }
         }
      }

/* If an error occurred, clean up by deleting the new FluxFrame. */
       if ( !astOK ) new = astDelete( new );
   }

/* Return the new FluxFrame pointer. */
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

AstSystemType astGetDensitySystem_( AstFluxFrame *this ){
   if ( !astOK ) return AST__BADSYSTEM;
   return (**astMEMBER(this,FluxFrame,GetDensitySystem))(this);
}

const char *astGetDensityUnit_( AstFluxFrame *this ){
   if ( !astOK ) return NULL;
   return (**astMEMBER(this,FluxFrame,GetDensityUnit))(this);
}


/* Special public interface functions. */
/* =================================== */
/* These provide the public interface to certain special functions
   whose public interface cannot be handled using macros (such as
   astINVOKE) alone. In general, they are named after the
   corresponding protected version of the function, but with "Id"
   appended to the name. */

/* Public Interface Function Prototypes. */
/* ------------------------------------- */
/* The following functions have public prototypes only (i.e. no
   protected prototypes), so we must provide local prototypes for use
   within this module. */
AstFluxFrame *astFluxFrameId_( double, void *, const char *, ... );

/* Special interface function implementations. */
/* ------------------------------------------- */
AstFluxFrame *astFluxFrameId_( double specval, void *specfrm_void, 
                               const char *options, ... ) {
/*
*++
*  Name:
c     astFluxFrame
f     AST_FLUXFRAME

*  Purpose:
*     Create a FluxFrame.

*  Type:
*     Public function.

*  Synopsis:
c     #include "fluxframe.h"
c     AstFluxFrame *astFluxFrame( double specval, AstSpecFrame *specfrm, 
c                                 const char *options, ... )
f     RESULT = AST_FLUXFRAME( SPECVAL, SPECFRM, OPTIONS, STATUS )

*  Class Membership:
*     FluxFrame constructor.

*  Description:
*     This function creates a new FluxFrame and optionally initialises
*     its attributes.
*
*     A FluxFrame is a specialised form of one-dimensional Frame which 
*     represents various systems used to represent the signal level in an
*     observation. The particular coordinate system to be used is specified 
*     by setting the FluxFrame's System attribute qualified, as necessary, by 
*     other attributes such as the units, etc (see the description of the 
*     System attribute for details).
*
*     All flux values are assumed to be measured at the same frequency or
*     wavelength (as given by the SpecVal attribute). Thus this class is
*     more appropriate for use with images rather than spectra.

*  Parameters:
c     specval
f     SPECVAL = DOUBLE PRECISION (Given)
*        The spectral value to which the flux values refer, given in the
*        spectral coordinate system specified by 
c        "specfrm". The value supplied for the "specval" 
f        SPECFRM. The value supplied for the SPECVAL
*        parameter becomes the default value for the SpecVal attribute.
*        A value of AST__BAD may be supplied if the spectral position is
*        unknown, but this may result in it not being possible for the
c        astConvert
*        AST_CONVERT 
*        function to determine a Mapping between the new FluxFrame and
*        some other FluxFrame.
c     specfrm
f     SPECFRM = INTEGER (Given)
*        A pointer to a SpecFrame describing the spectral coordinate system 
*        in which the 
c        "specval" 
f        SPECVAL
*        parameter is given. A deep copy of this object is taken, so any 
*        subsequent changes to the SpecFrame using the supplied pointer will 
*        have no effect on the new FluxFrame. 
c        A NULL pointer can be supplied if AST__BAD is supplied for "specval".
f        AST__NULL can be supplied if AST__BAD is supplied for SPECVAL.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new FluxFrame. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new FluxFrame. The syntax used is identical to that for the
f        AST_SET routine. If no initialisation is required, a blank
f        value may be supplied.
c     ...
c        If the "options" string contains "%" format specifiers, then
c        an optional list of additional arguments may follow it in
c        order to supply values to be substituted for these
c        specifiers. The rules for supplying these are identical to
c        those for the astSet function (and for the C "printf"
c        function).
f     STATUS = INTEGER (Given and Returned)
f        The global status.

*  Returned Value:
c     astFluxFrame()
f     AST_FLUXFRAME = INTEGER
*        A pointer to the new FluxFrame.

*  Notes:
*     - When conversion between two FluxFrames is requested (as when
c     supplying FluxFrames to astConvert),
f     supplying FluxFrames AST_CONVERT),
*     account will be taken of the nature of the flux coordinate systems 
*     they represent, together with any qualifying attribute values, including
*     the AlignSystem attribute. The results will therefore fully reflect the 
*     relationship between positions measured in the two systems. In addition, 
*     any difference in the Unit attributes of the two systems will also be 
*     taken into account.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astFluxFrame constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astFluxFrame_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - The variable argument list also prevents this function from
*     invoking astFluxFrame_ directly, so it must be a
*     re-implementation of it in all respects, except for the final
*     conversion of the result to an ID value.
*/

/* Local Variables: */
   AstMapping *um;               /* Mapping from default to actual units */
   AstFluxFrame *new;            /* Pointer to new FluxFrame */
   AstSpecFrame *sfrm;           /* Pointer to SpecFrame */
   AstSystemType s;              /* System */
   const char *u;                /* Units string */
   va_list args;                 /* Variable argument list */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Obtain and validate a pointer to the SpecFrame structures provided. */
   sfrm = specfrm_void ? astCheckSpecFrame( astMakePointer( specfrm_void ) ) : NULL;

/* Initialise the FluxFrame, allocating memory and initialising the virtual
   function table as well if necessary. */
   new = astInitFluxFrame( NULL, sizeof( AstFluxFrame ), !class_init, 
                           &class_vtab, "FluxFrame", specval, sfrm );

/* If successful, note that the virtual function table has been initialised. */
   if ( astOK ) {
      class_init = 1;

/* Obtain the variable argument list and pass it along with the options string
   to the astVSet method to initialise the new FluxFrame's attributes. */
      va_start( args, options );
      astVSet( new, options, args );
      va_end( args );

/* Check the Units are appropriate for the System. */
      u = astGetUnit( new, 0 );
      s = astGetSystem( new );
      um = astUnitMapper( DefUnit( s, "astFluxFrame", "FluxFrame" ), 
                          u, NULL, NULL );
      if( um ) {
         um = astAnnul( um );
      } else {
         astError( AST__BADUN, "astFluxFrame: Inappropriate units (%s) "
                   "specified for a %s axis.", u, SystemLabel( s ) );
      }      

/* If an error occurred, clean up by deleting the new object. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return an ID value for the new FluxFrame. */
   return astMakeId( new );
}




