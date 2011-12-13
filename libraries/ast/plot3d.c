/*
*class++
*  Name:
*     Plot3D

*  Purpose:
*     Provide facilities for 2D graphical output.

*  Constructor Function:
c     astPlot3D
f     AST_PLOT3D

*  Description:
*     A Plot3D is a specialised form of Plot that provides facilities
*     for producing 3D graphical output, including fully annotated 3D
*     coordinate grids. The base Frame in a Plot3D describes a 3-dimensional
*     "graphical" coordinate system. The axes of this coordinate system are
*     assumed to be right-handed (that is, if X appears horizontally to the
*     right and Y vertically upwards, then Z is out of the screen towards
*     the viewer), and are assumed to be equally scaled (that is, the same
*     units are used to measure positions on each of the 3 axes). The upper
*     and lower bounds of a volume within this graphical coordinate system
*     is specified when the Plot3D is created, and all subsequent graphics
*     are "drawn" in this volume.
*
*     The Plot3D class does not itself include any ability to draw on a
*     graphics device. Instead it calls upon function in an externally
*     supplied module (the "grf3d" module) to do the required drawing.
*     A module should be written that implements the functions of the
*     grf3d interface using the facilities of a specific graphics system
*     This module should then be linked into the application so that the
*     Plot3D class can use its functions (see the description of the
*     ast_link commands for details of how to do this). The grf3d interface
*     defines a few simple functions for drawing primitives such as straight
*     lines, markers and character strings. These functions all accept
*     positions in the 3D graphics coordinate system (the base Frame of the
*     Plot3D), and so the grf3d module must also manage the projection of
*     these 3D coordinates onto the 2D viewing surface, including the choice
*     of "eye"/"camera" position, direction of viewing, etc. The AST
*     library includes a sample implementation of the grf3d interface
*     based on the PGPLOT graphics system (see file grf3d_pgplot.c). This
*     implementation also serves to document the grf3d interface itself and
*     should be consulted for details before writing a new implementation.
*
*     The current Frame of a Plot3D describes a "physical" 3-dimensional
*     coordinate system, which is the coordinate system in which plotting
*     operations are specified when invoking the methods of the Plot3D
*     class. The results of each plotting operation are automatically
*     transformed into 3D graphical coordinates before being plotted
*     using the facilities of the grf3d module linked into the application.
*
*     You may select different physical coordinate systems in which to
*     plot (including the native graphical coordinate system itself)
*     by selecting different Frames as the current Frame of a Plot3D,
*     using its Current attribute.
*
*     Like any FrameSet, a Plot3D may also be used as a Frame. In this
*     case, it behaves like its current Frame, which describes the
*     physical coordinate system.
*
*     When used as a Mapping, a Plot3D describes the inter-relation
*     between 3D graphical coordinates (its base Frame) and 3D physical
*     coordinates (its current Frame).
*
*     Although the Plot3D class inherits from the Plot class, several of
*     the facilities of the Plot class are not available in the Plot3D
*     class, and an error will be reported if any attempt is made to use
*     them. Specifically, the Plot3D class does not support clipping
*     using the
*     astClip function.
f     AST_CLIP routine.
*     Nor does it support the specification of graphics primitive functions
*     at run-time using the
c     astGrfSet, astGrfPop, astGrfPush and astGetGrfContext functions.
f     AST_GRFSET, AST_GRFPOP, AST_GRFPUSH, and AST_GETGRFCONTEXT routines.

*  Inheritance:
*     The Plot3D class inherits from the Plot class.

*  Attributes:
*     In addition to those attributes common to all Plots, every
*     Plot3D also has the following attributes:
*
*     - Norm: Normal vector defining the 2D plane used for text and markers
*     - RootCorner: Specifies which edges of the 3D box should be annotated.
*
*     Some attributes of the Plot class refer to specific physical
*     coordinate axes (e.g. Gap, LabelUp, DrawAxes, etc). For a basic
*     Plot, the axis index must be 1 or 2, but for a Plot3D the axis index
*     can be 1, 2 or 3.
*
*     Certain Plot attributes are ignored by the Plot3D class (e.g. Edge,
*     DrawTitle, TitleGap, etc). Consult the Plot attribute documentation
*     for details.

*  Functions:
c     The Plot3D class does not define any new functions beyond those
f     The Plot3D class does not define any new routines beyond those
*     which are applicable to all Plots. Note, however, that the
*     following methods inherited from the Plot class cannot be used with
*     a Plot3D and will report an error if called:
c    - astBoundingBox, astClip, astCurve, astGenCurve,
c    astGetGrfContext, astGrfPop, astGrfPush, astGrfSet, astGridLine,
c    astPolyCurve.
f    - AST_BOUNDINGBOX, AST_CLIP, AST_CURVE, AST_GENCURVE,
f    AST_GETGRFCONTEXT, AST_GRFPOP, AST_GRFPUSH, AST_GRFSET,
f    AST_GRIDLINE, AST_POLYCURVE.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S. Berry (Starlink)

*  History:
*     6-JUN-2007 (DSB):
*        Original version.
*     6-SEP-2007 (DSB):
*        Re-code the astGrid function.
*     12-NOV-2007 (DSB):
*        Clear up compiler warnings.
*class--
*/

/* Module Macros. */
/* ============== */
/* Set the name of the class we are implementing. This indicates to
   the header files that define class interfaces that they should make
   "protected" symbols available. */
#define astCLASS Plot3D

/* Macros which return the maximum and minimum of two values. */
#define MAX(aa,bb) ((aa)>(bb)?(aa):(bb))
#define MIN(aa,bb) ((aa)<(bb)?(aa):(bb))

/* Macro to check for equality of floating point values. We cannot
   compare bad values directory because of the danger of floating point
   exceptions, so bad values are dealt with explicitly. */
#define EQUAL(aa,bb) (((aa)==AST__BAD)?(((bb)==AST__BAD)?1:0):(((bb)==AST__BAD)?0:(fabs((aa)-(bb))<=1.0E5*MAX((fabs(aa)+fabs(bb))*DBL_EPSILON,DBL_MIN))))

/* Integers identifying the 3 plotting surfaces */
#define XY 1
#define XZ 2
#define YZ 3

/* Integers identifying the 8 corners of the graphics cube. */
#define LLL 0
#define ULL 1
#define LUL 2
#define UUL 3
#define LLU 4
#define ULU 5
#define LUU 6
#define UUU 7

/* Identify the 4 edges of a Plot */
#define LEFT   0
#define TOP    1
#define RIGHT  2
#define BOTTOM 3

/* A macros that returns a pointer to the Plot that spans a given plane. */
#define GET_PLOT(plane) ( \
     ( plane == XY ) ? this->plotxy : ( \
     ( plane == XZ ) ? this->plotxz : ( \
     ( plane == YZ ) ? this->plotyz : NULL ) ) )


/*
*
*  Name:
*     MAKE_CLEAR3

*  Purpose:
*     Implement a method to clear a single value in a multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_CLEAR3(attr,component,assign,nval)

*  Class Membership:
*     Defined by the Plot3D class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstPlot *this, int axis )
*
*     and an external interface function of the form:
*
*        void astClear<Attribute>_( AstPlot *this, int axis )
*
*     which implement a method for clearing a single value in a specified
*     multi-valued attribute for an axis of a Plot3D.

*  Parameters:
*     attr
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. LabelAt in "astClearLabelAt").
*     component
*        The name of the class structure component that holds the attribute
*        value.
*     assign
*        An expression that evaluates to the value to assign to the component
*        to clear its value.
*     nval
*        Specifies the number of values in the multi-valued attribute. The
*        "axis" values supplied to the created function should be in the
*        range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_CLEAR3(attr,component,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attr( AstPlot3D *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astClear" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Assign the "clear" value. */ \
   } else { \
      this->component[ axis ] = (assign); \
   } \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astClear##attr##_( AstPlot3D *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,Plot3D,Clear##attr))( this, axis, status ); \
}


/*
*
*  Name:
*     MAKE_GET3

*  Purpose:
*     Implement a method to get a single value in a multi-valued attribute.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_GET3(attr,type,bad_value,assign,nval)

*  Class Membership:
*     Defined by the Plot3D class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static <Type> Get<Attribute>( AstPlot3D *this, int axis )
*
*     and an external interface function of the form:
*
*        <Type> astGet<Attribute>_( AstPlot3D *this, int axis )
*
*     which implement a method for getting a single value from a specified
*     multi-valued attribute for an axis of a Plot3D.

*  Parameters:
*     attr
*        The name of the attribute whose value is to be obtained, as it
*        appears in the function name (e.g. Label in "astGetLabel").
*     type
*        The C type of the attribute.
*     bad_value
*        A constant value to return if the global error status is set, or if
*        the function fails.
*     assign
*        An expression that evaluates to the value to be returned. This can
*        use the string "axis" to represent the zero-based value index.
*     nval
*        Specifies the number of values in the multi-valued attribute. The
*        "axis" values supplied to the created function should be in the
*        range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_GET3(attr,type,bad_value,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static type Get##attr( AstPlot3D *this, int axis, int *status ) { \
   type result;                  /* Result to be returned */ \
\
/* Initialise */ \
   result = (bad_value); \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astGet" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Assign the result value. */ \
   } else { \
      result = (assign); \
   } \
\
/* Check for errors and clear the result if necessary. */ \
   if ( !astOK ) result = (bad_value); \
\
/* Return the result. */ \
   return result; \
} \
/* External interface. */ \
/* ------------------- */  \
type astGet##attr##_( AstPlot3D *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return (bad_value); \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,Plot3D,Get##attr))( this, axis, status ); \
}

/*
*
*  Name:
*     MAKE_SET3

*  Purpose:
*     Implement a method to set a single value in a multi-valued attribute
*     for a Plot3D.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_SET3(attr,type,component,assign,nval)

*  Class Membership:
*     Defined by the Plot3D class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstPlot3D *this, int axis, <Type> value )
*
*     and an external interface function of the form:
*
*        void astSet<Attribute>_( AstPlot3D *this, int axis, <Type> value )
*
*     which implement a method for setting a single value in a specified
*     multi-valued attribute for a Plot3D.

*  Parameters:
*      attr
*         The name of the attribute to be set, as it appears in the function
*         name (e.g. LabelAt in "astSetLabelAt").
*      type
*         The C type of the attribute.
*      component
*         The name of the class structure component that holds the attribute
*         value.
*      assign
*         An expression that evaluates to the value to be assigned to the
*         component.
*      nval
*        Specifies the number of values in the multi-valued attribute. The
*        "axis" values supplied to the created function should be in the
*        range zero to (nval - 1). If a value of 0 is supplied, the
*        value of the Plot3D's Nin attribute is used instead.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_SET3(attr,type,component,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attr( AstPlot3D *this, int axis, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astSet" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Store the new value in the structure component. */ \
   } else { \
      this->component[ axis ] = (assign); \
   } \
} \
\
/* External interface. */ \
/* ------------------- */ \
void astSet##attr##_( AstPlot3D *this, int axis, type value, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Invoke the required method via the virtual function table. */ \
   (**astMEMBER(this,Plot3D,Set##attr))( this, axis, value, status ); \
}

/*
*
*  Name:
*     MAKE_TEST3

*  Purpose:
*     Implement a method to test if a single value has been set in a
*     multi-valued attribute for a class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_TEST3(attr,assign,nval)

*  Class Membership:
*     Defined by the Plot3D class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static int Test<Attribute>( AstPlot3D *this, int axis )
*
*     and an external interface function of the form:
*
*        int astTest<Attribute>_( AstPlot3D *this, int axis )
*
*     which implement a method for testing if a single value in a specified
*     multi-valued attribute has been set for a class.

*  Parameters:
*      attr
*         The name of the attribute to be tested, as it appears in the function
*         name (e.g. LabelAt in "astTestLabelAt").
*      assign
*         An expression that evaluates to 0 or 1, to be used as the returned
*         value. This can use the string "axis" to represent the zero-based
*         index of the value within the attribute.
*      nval
*        Specifies the number of values in the multi-valued attribute. The
*        "axis" values supplied to the created function should be in the
*        range zero to (nval - 1).

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_TEST3(attr,assign,nval) \
\
/* Private member function. */ \
/* ------------------------ */ \
static int Test##attr( AstPlot3D *this, int axis, int *status ) { \
   int result;                   /* Value to return */ \
\
/* Initialise */ \
   result = 0; \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* Validate the axis index. */ \
   if( axis < 0 || axis >= nval ){ \
      astError( AST__AXIIN, "%s(%s): Index (%d) is invalid for attribute " \
                #attr " - it should be in the range 1 to %d.", status, \
                "astTest" #attr, astGetClass( this ), \
                axis + 1, nval ); \
\
/* Assign the result value. */ \
   } else { \
      result = (assign); \
   } \
\
/* Check for errors and clear the result if necessary. */ \
   if ( !astOK ) result = 0; \
\
/* Return the result. */ \
   return result; \
} \
/* External interface. */ \
/* ------------------- */ \
int astTest##attr##_( AstPlot3D *this, int axis, int *status ) { \
\
/* Check the global error status. */ \
   if ( !astOK ) return 0; \
\
/* Invoke the required method via the virtual function table. */ \
   return (**astMEMBER(this,Plot3D,Test##attr))( this, axis, status ); \
}

/*
*
*  Name:
*     MAKE_CLEAR2

*  Purpose:
*     Implement a method to clear an element-specific attribute inherited
*     from the Plot class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_CLEAR2(attr)

*  Class Membership:
*     Defined by the Plot3d class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstPlot *this, int element )
*
*     which implements a method for clearing one of the element-specific
*     attributes (e.g. Size, Colour, Width, etc) inherited from the
*     parent Plot class.

*  Parameters:
*     attr
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. LabelAt in "astClearSize").

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_CLEAR2(attr) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attr( AstPlot *this_plot, int element, int *status ) { \
\
/* Local Variables: */ \
   AstPlot3D *this; \
   int axis3d; \
   int elem2d1; \
   int elem2d2; \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Clear the attribute value in the parent Plot structure. */ \
   (*parent_clear##attr)( this_plot, element, status ); \
\
/* If OK, clear the attribute in the encapsulated Plots. */ \
   if( astOK ) { \
      this = (AstPlot3D *) this_plot; \
\
/* Get the zero-based index of the 3D axis to which the supplied element \
   refers. Use an index of -1 to indicate that the element does not \
   relate to a specific axis. Also get the corresponding elements to use \
   with the two Plots that share the specified 3D axis. */ \
      axis3d = Element2D( this, element, &elem2d1, &elem2d2, status ); \
\
/* If the element is not axis-specific, clear the attribute value in all \
   three plots. */ \
      if( axis3d == -1 ) { \
         astClear##attr( this->plotxy, element ); \
         astClear##attr( this->plotxz, element ); \
         astClear##attr( this->plotyz, element ); \
\
/* Otherwise, clear the attribute in the two plots that share the \
   specified 3D axis. */ \
      } else { \
         astClear##attr( GET_PLOT(this->axis_plot1[ axis3d ]), elem2d1 ); \
         astClear##attr( GET_PLOT(this->axis_plot2[ axis3d ]), elem2d2 ); \
      } \
   } \
}

/*
*
*  Name:
*     MAKE_SET2

*  Purpose:
*     Implement a method to set an element-specific attribute inherited
*     from the Plot class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_SET2(attr,type)

*  Class Membership:
*     Defined by the Plot3d class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstPlot *this, int element, type value )
*
*     which implements a method for setting one of the element-specific
*     attributes (e.g. Size, Colour, Width, etc) inherited from the
*     parent Plot class.

*  Parameters:
*     attr
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. LabelAt in "astClearSize").
*     type
*        The attribute data type.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_SET2(attr,type) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attr( AstPlot *this_plot, int element, type value, int *status ) { \
\
/* Local Variables: */ \
   AstPlot3D *this; \
   int axis3d; \
   int elem2d1; \
   int elem2d2; \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Set the attribute value in the parent Plot structure. */ \
   (*parent_set##attr)( this_plot, element, value, status ); \
\
/* If OK, set the attribute in the encapsulated Plots. */ \
   if( astOK ) { \
      this = (AstPlot3D *) this_plot; \
\
/* Get the zero-based index of the 3D axis to which the supplied element \
   refers. Use an index of -1 to indicate that the element does not \
   relate to a specific axis. Also get the corresponding elements to use \
   with the two Plots that share the specified 3D axis. */ \
      axis3d = Element2D( this, element, &elem2d1, &elem2d2, status ); \
\
/* If the element is not axis-specific, clear the attribute value in all \
   three plots. */ \
      if( axis3d == -1 ) { \
         astSet##attr( this->plotxy, element, value ); \
         astSet##attr( this->plotxz, element, value ); \
         astSet##attr( this->plotyz, element, value ); \
\
/* Otherwise, clear the attribute in the two plots that share the \
   specified 3D axis. */ \
      } else { \
         astSet##attr( GET_PLOT(this->axis_plot1[ axis3d ]), elem2d1, value ); \
         astSet##attr( GET_PLOT(this->axis_plot2[ axis3d ]), elem2d2, value ); \
      } \
   } \
}


/*
*
*  Name:
*     MAKE_CLEAR1

*  Purpose:
*     Implement a method to clear an attribute inherited from the Plot class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_CLEAR1(attr)

*  Class Membership:
*     Defined by the Plot3d class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstPlot *this )
*
*     which implements a method for clearing a Plot3D attribute inherited
*     from the parent Plot class. It clears the attribute in all three
*     plots encapsulated within the Plot3D.

*  Parameters:
*     attr
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. LabelAt in "astClearLabelAt").

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_CLEAR1(attr) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attr( AstPlot *this_plot, int *status ) { \
\
/* Local Variables: */ \
   AstPlot3D *this; \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Clear the attribute value in the parent Plot structure. */ \
   (*parent_clear##attr)( this_plot, status ); \
\
/* If OK, clear the attribute in all three of the encapsulated Plots. */ \
   if( astOK ) { \
      this = (AstPlot3D *) this_plot; \
      astClear##attr( this->plotxy ); \
      astClear##attr( this->plotxz ); \
      astClear##attr( this->plotyz ); \
   } \
}

/*
*
*  Name:
*     MAKE_SET1

*  Purpose:
*     Implement a method to set an attribute inherited from the Plot class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_SET1(attr,type)

*  Class Membership:
*     Defined by the Plot3d class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstPlot *this, type value )
*
*     which implements a method for setting a Plot3D attribute inherited
*     from the parent Plot class. It sets the attribute in all three
*     plots encapsulated within the Plot3D.

*  Parameters:
*     attr
*        The name of the attribute to be set, as it appears in the function
*        name (e.g. LabelAt in "astSetLabelAt").
*     type
*        The C data type for the attribute value.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_SET1(attr,type) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attr( AstPlot *this_plot, type value, int *status ) { \
\
/* Local Variables: */ \
   AstPlot3D *this; \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Set the attribute value in the parent Plot structure. */ \
   (*parent_set##attr)( this_plot, value, status ); \
\
/* If OK, set the attribute in all three of the encapsulated Plots. */ \
   if( astOK ) { \
      this = (AstPlot3D *) this_plot; \
      astSet##attr( this->plotxy, value ); \
      astSet##attr( this->plotxz, value ); \
      astSet##attr( this->plotyz, value ); \
   } \
}

/*
*
*  Name:
*     MAKE_CLEAR

*  Purpose:
*     Implement a method to clear an attribute inherited from the Plot class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_CLEAR(attr,whichplots)

*  Class Membership:
*     Defined by the Plot3d class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Clear<Attribute>( AstPlot *this, int axis )
*
*     which implements a method for clearing an axis specific Plot3D
*     attribute inherited from the parent Plot class.

*  Parameters:
*     attr
*        The name of the attribute to be cleared, as it appears in the function
*        name (e.g. LabelAt in "astClearLabelAt").
*     whichplots
*        A value indicating which Plots should be affected. A negative value
*        means "all threee plots", a value of zero means "just the two plots
*        that touch at the specified axis in 3D space", a positive value
*        means "just the Plot that is used to label th 3D axis."

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_CLEAR(attr,whichplots) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Clear##attr( AstPlot *this_plot, int axis, int *status ) { \
\
/* Local Variables: */ \
   AstPlot *plot; \
   AstPlot3D *this; \
   int axis2d; \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Clear the attribute value in the parent Plot structure. This will \
   validate the axis index. */ \
   (*parent_clear##attr)( this_plot, axis, status ); \
\
/* If OK, clear the attribute for the relevant axis, or axes, of the Plots  \
   encapsulated inside the Plot3D. First get a pointer to the Plot3D \
   structure. */ \
   if( astOK ) { \
      this = (AstPlot3D *) this_plot; \
\
/* If requested clear the attribute in all three Plots. */ \
      if( whichplots < 0 ) { \
         astClear##attr( this->plotxy, axis ); \
         astClear##attr( this->plotxz, axis ); \
         astClear##attr( this->plotyz, axis ); \
\
/* Each axis in 3D graphics space is described by two of the encapsulated \
   Plots, but only one of these two Plots is used to generate labels for \
   the axis. Now deal with cases where we are clearing the attribute \
   value in both of the two Plots that describe the axis. */ \
      } else if ( whichplots == 0 ) { \
         if( axis == 0 ) { \
            astClear##attr( this->plotxy, 0 ); \
            astClear##attr( this->plotxz, 0 ); \
\
         } else if( axis == 1 ) { \
            astClear##attr( this->plotxy, 1 ); \
            astClear##attr( this->plotyz, 0 ); \
\
         } else { \
            astClear##attr( this->plotxz, 1 ); \
            astClear##attr( this->plotyz, 1 ); \
         } \
\
/* Now deal with cases where we are clearing the attribute value only in  \
   the Plot that is used to label the axis. */ \
      } else { \
         plot = AxisPlot( this, axis, &axis2d, status ); \
         astClear##attr( plot, axis2d ); \
      } \
   } \
}


/*
*
*  Name:
*     MAKE_GET

*  Purpose:
*     Implement a method to get the value of an attribute inherited from the
*     Plot class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot.h"
*     MAKE_GET(attr,type,bad_value)

*  Class Membership:
*     Defined by the Plot3D class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static <Type> Get<Attribute>( AstPlot *this, int axis )
*
*     which implements a method for getting a value for an axis specific
*     attribute for a Plot3D.

*  Parameters:
*     attr
*        The name of the attribute whose value is to be obtained, as it
*        appears in the function name (e.g. Label in "astGetLabel").
*     type
*        The C type of the attribute.
*     bad_value
*        A constant value to return if the global error status is set, or if
*        the function fails.

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*
*/

/* Define the macro. */
#define MAKE_GET(attr,type,bad_value) \
\
/* Private member function. */ \
/* ------------------------ */ \
static type Get##attr( AstPlot *this_plot, int axis, int *status ) { \
\
/* Local Variables: */ \
   AstPlot *plot; \
   AstPlot3D *this; \
   int axis2d; \
   type result; \
\
/* Initialise */ \
   result = (bad_value); \
\
/* Check the global error status. */ \
   if ( !astOK ) return result; \
\
/* See if the attribute value is set in the parent Plot structure. If so, \
   use the parent get method to get its value. */ \
   if( astTest##attr( this_plot, axis ) ) { \
      result = (*parent_get##attr)( this_plot, axis, status ); \
\
/* If the attribute value is not set in the parent Plot structure, get \
   the default value from the Plot that is used to label the 3D axis. The \
   parent test method called above will have reported an error if the axis \
   index is invalid, so check astOK here. */ \
   } else if( astOK ) { \
      this = (AstPlot3D *) this_plot; \
      plot = AxisPlot( this, axis, &axis2d, status ); \
      result = astGet##attr( plot, axis2d ); \
   } \
\
/* Return the result. */ \
   return result; \
}

/*
*
*  Name:
*     MAKE_SET

*  Purpose:
*     Implement a method to set a value for an attribute inherited from the
*     Plot class.

*  Type:
*     Private macro.

*  Synopsis:
*     #include "plot3d.h"
*     MAKE_SET(attr,type,whichplots)

*  Class Membership:
*     Defined by the Plot3d class.

*  Description:
*     This macro expands to an implementation of a private member function of
*     the form:
*
*        static void Set<Attribute>( AstPlot *this, int axis, <Type> value )
*
*     which implements a method for setting a value for an axis specific
*     attribute inherited from the parent Plot class.

*  Parameters:
*      attr
*         The name of the attribute to be set, as it appears in the function
*         name (e.g. LabelAt in "astSetLabelAt").
*      type
*         The C type of the attribute.
*     whichplots
*        A value indicating which Plots should be affected. A negative value
*        means "all threee plots", a value of zero means "just the two plots
*        that touch at the specified axis in 3D space", a positive value
*        means "just the Plot that is used to label the 3D axis."

*  Notes:
*     -  To avoid problems with some compilers, you should not leave any white
*     space around the macro arguments.
*-
*/

/* Define the macro. */
#define MAKE_SET(attr,type,whichplots) \
\
/* Private member function. */ \
/* ------------------------ */ \
static void Set##attr( AstPlot *this_plot, int axis, type value, int *status ) { \
\
/* Local Variables: */ \
   AstPlot3D *this; \
   AstPlot *plot; \
   int axis2d; \
\
/* Check the global error status. */ \
   if ( !astOK ) return; \
\
/* Set the supplied value in the parent Plot class. This will validate \
   the axis index. */ \
   (*parent_set##attr)( this_plot, axis, value, status ); \
\
/* If this went OK, also set the value for the appropriate axis of the \
   appropriate encapsulated Plot(s). First get a pointer to the Plot3D \
   structure. */ \
   if( astOK ) { \
      this = (AstPlot3D *) this_plot; \
\
/* If requested set the attribute in all three Plots. */ \
      if( whichplots < 0 ) { \
         astSet##attr( this->plotxy, axis, value ); \
         astSet##attr( this->plotxz, axis, value ); \
         astSet##attr( this->plotyz, axis, value ); \
\
/* Each axis in 3D graphics space is described by two of the encapsulated \
   Plots, but only one of these two Plots is used to generate labels for \
   the axis. First deal with cases where we are setting the attribute \
   value in both of the two Plots that describe the axis. */ \
      } else if( whichplots == 0 ) { \
         if( axis == 0 ) { \
            astSet##attr( this->plotxy, 0, value ); \
            astSet##attr( this->plotxz, 0, value ); \
\
         } else if( axis == 1 ) { \
            astSet##attr( this->plotxy, 1, value ); \
            astSet##attr( this->plotyz, 0, value ); \
\
         } else { \
            astSet##attr( this->plotxz, 1, value ); \
            astSet##attr( this->plotyz, 1, value ); \
         } \
\
/* Now deal with cases where we are setting the attribute value only in  \
   the Plot that is used to label the axis. */ \
      } else { \
         plot = AxisPlot( this, axis, &axis2d, status ); \
         astSet##attr( plot, axis2d, value ); \
      } \
   } \
}


/* Header files. */
/* ============= */
/* Interface definitions. */
/* ---------------------- */

#include "globals.h"             /* Thread-safe global data access */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory allocation facilities */
#include "object.h"              /* Base Object class */
#include "cmpframe.h"            /* Compound Frames */
#include "cmpmap.h"              /* Compound Mappings */
#include "unitmap.h"             /* Unit mappings */
#include "permmap.h"             /* Axis permutations */
#include "winmap.h"              /* Scale and shift mappings */
#include "frame.h"               /* Coordinate systems */
#include "frameset.h"            /* Inter-related coordinate systems */
#include "keymap.h"              /* Hash array */
#include "plot.h"                /* Interface definition for parent class */
#include "plot3d.h"              /* Interface definition for this class */
#include "grf3d.h"               /* The grf3D interface */
#include "pointset.h"            /* Sets of points */
#include "globals.h"             /* Thread-safe global data access */

/* Error code definitions. */
/* ----------------------- */
#include "ast_err.h"             /* AST error codes */

/* C header files. */
/* --------------- */
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h>
#include <math.h>
#include <limits.h>

/* Module Variables. */
/* ================= */

/* Address of this static variable is used as a unique identifier for
   member of this class. */
static int class_check;

/* Pointers to parent class methods which are used or extended by this
   class. */
static AstObject *(* parent_cast)( AstObject *, AstObject *, int * );
static void (* parent_removeframe)( AstFrameSet *, int, int * );
static int (* parent_getobjsize)( AstObject *, int * );
static int (* parent_equal)( AstObject *, AstObject *, int * );
static void (* parent_vset)( AstObject *, const char *, char **, va_list, int * );
static void (* parent_clear)( AstObject *, const char *, int * );
static void (* parent_clearcurrent)( AstFrameSet *, int * );
static void (* parent_setcurrent)( AstFrameSet *, int, int * );
static const char *(* parent_getattrib)( AstObject *, const char *, int * );
static int (* parent_testattrib)( AstObject *, const char *, int * );
static void (* parent_clearattrib)( AstObject *, const char *, int * );
static void (* parent_setattrib)( AstObject *, const char *, int * );

/* A FrameSet pointer that is used when calling astCast. */
static AstFrameSet *dummy_frameset = NULL;

#if defined(THREAD_SAFE)
static int (* parent_managelock)( AstObject *, int, int, AstObject **, int * );
#endif

/* Define macros for accessing each item of thread specific global data. */
#ifdef THREAD_SAFE

/* Define how to initialise thread-specific globals. */
#define GLOBAL_inits \
   globals->Class_Init = 0; \
   globals->GetAttrib_Buff[ 0 ] = 0;

/* Create the function that initialises global data for this module. */
astMAKE_INITGLOBALS(Plot3D)

/* Define macros for accessing each item of thread specific global data. */
#define class_init astGLOBAL(Plot3D,Class_Init)
#define class_vtab astGLOBAL(Plot3D,Class_Vtab)
#define getattrib_buff astGLOBAL(Plot3D,GetAttrib_Buff)

static pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX2 pthread_mutex_lock( &mutex2 );
#define UNLOCK_MUTEX2 pthread_mutex_unlock( &mutex2 );

static pthread_mutex_t mutex3 = PTHREAD_MUTEX_INITIALIZER;
#define LOCK_MUTEX3 pthread_mutex_lock( &mutex3 );
#define UNLOCK_MUTEX3 pthread_mutex_unlock( &mutex3 );

/* If thread safety is not needed, declare and initialise globals at static
   variables. */
#else

static char getattrib_buff[ 101 ];


/* Define the class virtual function table and its initialisation flag
   as static variables. */
static AstPlot3DVtab class_vtab;   /* Virtual function table */
static int class_init = 0;       /* Virtual function table initialised? */

#define LOCK_MUTEX2
#define UNLOCK_MUTEX2

#define LOCK_MUTEX3
#define UNLOCK_MUTEX3

#endif

/* Prototypes for Private Member Functions. */
/* ======================================== */
static AstFrameSet *Fset3D( AstFrameSet *, int, int * );
static AstKeyMap *GetGrfContext( AstPlot *, int * );
static AstObject *Cast( AstObject *, AstObject *, int * );
static AstPlot *AxisPlot( AstPlot3D *, int, int *, int * );
static AstPointSet *ExtendTicks( AstPlot *, AstPointSet *, int * );
static AstPointSet *Transform( AstMapping *, AstPointSet *, int, AstPointSet *, int * );
static const char *RootCornerString( int, int * );
static int Attr3D( AstKeyMap *, int, double, double *, int, int * );
static int Border( AstPlot *, int * );
static int Element2D( AstPlot3D *, int, int *, int *, int * );
static int Equal( AstObject *, AstObject *, int * );
static int GetObjSize( AstObject *, int * );
static int Plot3DAttr( AstKeyMap *, int, double, double *, int );
static int Plot3DCap( AstKeyMap *, int, int );
static int Plot3DFlush( AstKeyMap * );
static int Plot3DLine( AstKeyMap *, int, const float *, const float * );
static int Plot3DMark( AstKeyMap *, int, const float *, const float *, int );
static int Plot3DQch( AstKeyMap *, float *, float * );
static int Plot3DScales( AstKeyMap *, float *, float * );
static int Plot3DText( AstKeyMap *, const char *, float, float, const char *, float, float );
static int Plot3DTxExt( AstKeyMap *, const char *, float, float, const char *, float, float, float *, float * );
static int RootCornerInt( const char *, int * );
static void BoundingBox( AstPlot *, float[2], float[2], int * );
static void ChangeRootCorner( AstPlot3D *, int, int, int * );
static void Clear( AstObject *, const char *, int * );
static void ClearCurrent( AstFrameSet *, int * );
static void Clip( AstPlot *, int, const double [], const double [], int * );
static void Copy( const AstObject *, AstObject *, int * );
static void CreatePlots( AstPlot3D *, AstFrameSet *, const float *, const double *, int * );
static void Curve( AstPlot *, const double [], const double [], int * );
static void Delete( AstObject *, int * );
static void Dump( AstObject *, AstChannel *, int * );
static void GenCurve( AstPlot *, AstMapping *, int * );
static void GrfPop( AstPlot *, int * );
static void GrfPush( AstPlot *, int * );
static void GrfSet( AstPlot *, const char *,  AstGrfFun, int * );
static void Grid( AstPlot *, int * );
static void GridLine( AstPlot *, int, const double [], double, int * );
static void Mark( AstPlot *, int, int, int, const double *, int, int * );
static void PolyCurve( AstPlot *, int, int, int, const double *, int * );
static void RemoveFrame( AstFrameSet *, int, int * );
static void Set3DGrf( AstPlot3D *, AstPlot *, int, int * );
static void SetCurrent( AstFrameSet *, int, int * );
static void SetPlotAttr( AstPlot *, int, int[ 2 ], int * );
static void SetTickValues( AstPlot *, int, int, double *, int, double *, int * );
static void SplitFrameSet( AstFrameSet *, AstFrameSet **, int[2], int[2], AstFrameSet **, int[2], int[2], AstFrameSet **, int[2], int[2], int *, int * );
static void StoreAxisInfo( AstPlot3D *, int[2], int[2], int[2], int[2], int[2], int[2], int * );
static void Text( AstPlot *, const char *, const double [], const float [2], const char *, int * );
static void UpdatePlots( AstPlot3D *, int * );
static void VSet( AstObject *, const char *, char **, va_list, int * );

static const char *GetAttrib( AstObject *, const char *, int * );
static int TestAttrib( AstObject *, const char *, int * );
static void ClearAttrib( AstObject *, const char *, int * );
static void SetAttrib( AstObject *, const char *, int * );

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *, int, int, AstObject **, int * );
#endif

/* Declare private member functions that access Plot3D attributes.
   --------------------------------------------------------------*/

/* Axis independent... */

#define DECLARE_PLOT3D_ACCESSORS(attr,type) \
   static type Get##attr(AstPlot3D *,int *); \
   static void Set##attr(AstPlot3D *,type,int *); \
   static void Clear##attr(AstPlot3D *,int *); \
   static int Test##attr(AstPlot3D *,int *);

DECLARE_PLOT3D_ACCESSORS(RootCorner,int)

#undef DECLARE_PLOT3D_ACCESSORS


/* Axis specific... */

#define DECLARE_PLOT3D_ACCESSORS(attr,type) \
   static type Get##attr(AstPlot3D *,int,int *); \
   static void Set##attr(AstPlot3D *,int,type,int *); \
   static void Clear##attr(AstPlot3D *,int,int *); \
   static int Test##attr(AstPlot3D *,int,int *);

DECLARE_PLOT3D_ACCESSORS(Norm,double)

#undef DECLARE_PLOT3D_ACCESSORS


/* Declare private member functions that access axis-specific attributes
   inherited from the Plot class. Also declare pointers to hold the parent
   function pointers.
   ----------------------------------------------------------------------*/

#define DECLARE_PLOT_ACCESSORS(attr,type) \
   static type Get##attr(AstPlot *,int,int *); \
   static void Set##attr(AstPlot *,int,type,int *); \
   static void Clear##attr(AstPlot *,int,int *); \
   static type (*parent_get##attr)(AstPlot *,int,int *); \
   static void (*parent_set##attr)(AstPlot *,int,type,int *); \
   static void (*parent_clear##attr)(AstPlot *,int,int *);

DECLARE_PLOT_ACCESSORS(MinTick,int)
DECLARE_PLOT_ACCESSORS(Abbrev,int)
DECLARE_PLOT_ACCESSORS(Gap,double)
DECLARE_PLOT_ACCESSORS(LogGap,double)
DECLARE_PLOT_ACCESSORS(LogPlot,int)
DECLARE_PLOT_ACCESSORS(LogTicks,int)
DECLARE_PLOT_ACCESSORS(LogLabel,int)
DECLARE_PLOT_ACCESSORS(LabelUp,int)
DECLARE_PLOT_ACCESSORS(DrawAxes,int)
DECLARE_PLOT_ACCESSORS(LabelUnits,int)
DECLARE_PLOT_ACCESSORS(MinTickLen,double)
DECLARE_PLOT_ACCESSORS(MajTickLen,double)
DECLARE_PLOT_ACCESSORS(NumLab,int)
DECLARE_PLOT_ACCESSORS(NumLabGap,double)
DECLARE_PLOT_ACCESSORS(TextLab,int)
DECLARE_PLOT_ACCESSORS(TextLabGap,double)

#undef DECLARE_PLOT_ACCESSORS


/* Declare private member functions that access element-specific attributes
   inherited from the Plot class. Also declare pointers to hold the parent
   function pointers.
   ----------------------------------------------------------------------*/

#define DECLARE_PLOT_ACCESSORS(attr,type) \
   static void Set##attr(AstPlot *,int,type,int *); \
   static void Clear##attr(AstPlot *,int,int *); \
   static void (*parent_set##attr)(AstPlot *,int,type,int *); \
   static void (*parent_clear##attr)(AstPlot *,int,int *);

DECLARE_PLOT_ACCESSORS(Style,int)
DECLARE_PLOT_ACCESSORS(Font,int)
DECLARE_PLOT_ACCESSORS(Colour,int)
DECLARE_PLOT_ACCESSORS(Width,double)
DECLARE_PLOT_ACCESSORS(Size,double)

#undef DECLARE_PLOT_ACCESSORS


/* Declare private member functions that access attributes inherited from
   the Plot class that do not need to override the Get method. This
   includes attributes that are not axis-specific or that do not have
   dynamic defaults. Also declare pointers to hold the parent function
   pointers.
   ----------------------------------------------------------------------*/
#define DECLARE_PLOT_ACCESSORS(attr,type) \
   static void Set##attr(AstPlot *,type,int *); \
   static void Clear##attr(AstPlot *,int *); \
   static void (*parent_set##attr)(AstPlot *,type,int *); \
   static void (*parent_clear##attr)(AstPlot *,int *);

DECLARE_PLOT_ACCESSORS(Ink,int)
DECLARE_PLOT_ACCESSORS(Tol,double)
DECLARE_PLOT_ACCESSORS(Invisible,int)
DECLARE_PLOT_ACCESSORS(TickAll,int)
DECLARE_PLOT_ACCESSORS(ForceExterior,int)
DECLARE_PLOT_ACCESSORS(Border,int)
DECLARE_PLOT_ACCESSORS(Clip,int)
DECLARE_PLOT_ACCESSORS(ClipOp,int)
DECLARE_PLOT_ACCESSORS(Escape,int)
DECLARE_PLOT_ACCESSORS(Grid,int)
DECLARE_PLOT_ACCESSORS(Labelling,int)

#undef DECLARE_PLOT_ACCESSORS



/* Member functions. */
/* ================= */

static int Attr3D( AstKeyMap *grfconID, int attr, double value,
                   double *old_value, int prim, int *status ){
/*
*  Name:
*     Attr3D

*  Purpose:
*     Get or set the value of a 3D grf attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     Attr3D( AstKeyMap *grfconID, int attr, double value, double *old_value,
*             int prim, int *status )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function gets or sets the current value of a specified graphics
*     attribute in the parent Plot structure of a Plot3D. It forwards the
*     call to the grf3D module being used by this Plot3D. It should be
*     registered with the parent Plot using astGrfSet.

*  Parameters:
*     grfconID
*       The Plot's GrfContext KeyMap.
*     attr
*       An integer value identifying the required attribute. This should
*       be one of the symbolic values defined in grf.h.
*     value
*       A new value to store for the attribute. If this is AST__BAD
*       no value is stored.
*     old_value
*       A pointer to a double in which to return the attribute value.
*       If this is NULL, no value is returned.
*     prim
*       The sort of graphics primitive to be drawn with the new attribute.
*       Identified by one of the values defined in grf.h.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

/* Local Variables: */
   int result;

/* Check the inherited status. */
   if( !astOK ) return 0;

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
   LOCK_MUTEX2;

/* Use the function in the external Grf3D module, selected at link-time
   using ast_link options. */
   result = astG3DAttr( attr, value, old_value, prim );

/* Allow the next thread to proceed. */
   UNLOCK_MUTEX2;

/* Return the result. */
   return result;
}

static AstPlot *AxisPlot( AstPlot3D *this, int axis3d, int *axis2d, int *status ){
/*
*  Name:
*     AxisPlot

*  Purpose:
*     Find the Plot used to label a 3D axis.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     AstPlot *AxisPlot( AstPlot3D *this, int axis3d, int *axis2d, int *status )

*  Class Membership:
*     Plot method.

*  Description:
*     This function returns a pointer to the encapsulated 2D Plot that
*     is used to label the given 3D axis. It also returns the index
*     of the labelled axis within the 2D Plot.

*  Parameters:
*     this
*        Pointer to a Plot3D.
*     axis3d
*        A zero-based axis index within the Plot3D.
*     axis2d
*        Pointer to an int in which to put the index of the labelled axis
*        within the returned 2D Plot.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the Plot used to label the 3D axis. Do not annul this
*     pointer.

*-
*/

/* Local Variables: */
   AstPlot *plot;

/* Check the global status. */
   if( !astOK ) return NULL;

/* Return the required information form the Plot3D structure. */
   plot = GET_PLOT( this->axis_plot1[ axis3d ] );
   if( ! plot ) {
      astError( AST__INTER, "AxisPlot(Plot3D): Illegal value %d "
                "for axis3d (internal AST programming error).", status,
                this->axis_plot1[ axis3d ] );
   }

   *axis2d = this->axis_index1[ axis3d ];

   return plot;
}

static int Border( AstPlot *this_plot, int *status ){
/*
*  Name:
*     Border

*  Purpose:
*     Draw a border around valid regions of a Plot.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "plot3d.h"
*     int Border( AstPlot *this, int *status )

*  Class Membership:
*     Plot method (overrides the astBorder method inherited from the
*     Plot class)

*  Description:
*     This function draws a (line) border around regions of the
*     plotting area of a Plot which correspond to valid, unclipped
*     physical coordinates. For example, when plotting using an
*     all-sky map projection, this function could be used to draw the
*     boundary of the celestial sphere when it is projected on to the
*     plotting surface.
*
*     If the entire plotting area contains valid, unclipped physical
*     coordinates, then the boundary will just be a rectangular box
*     around the edges of the plotting area.

*  Parameters:
*     this
*        Pointer to the Plot.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Zero is returned if the plotting area is completely filled by
*     valid, unclipped physical coordinates (so that only a
*     rectangular box was drawn around the edge). Otherwise, one is
*     returned.

*  Notes:
*     - The Plot3D implementation of this method, invokes the astBorder
*     method on each of the three encapsulated Plots, and returns the
*     logical OR of the three returned flags.
*     - A value of zero will be returned if this function is invoked
*     with the AST error status set, or if it should fail for any
*     reason.
*     - An error results if either the current Frame or the base Frame
*     of the Plot is not 2-dimensional, or (for a Plot3D) 3-dimensional.
*     - An error also results if the transformation between the base
*     and current Frames of the Plot is not defined (i.e. the Plot's
*     TranForward attribute is zero).
*/

/* Local Variables: */
   AstPlot3D *this;
   const char *class;
   const char *method;
   float x1;
   float y1;
   float z1;
   float x[ 2 ];
   float y[ 2 ];
   float z[ 2 ];
   int flag1;
   int flag2;
   int flag3;
   int naxes;
   int ok;
   int result;
   int root_corner;

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Get a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_plot;

/* Store the current method, and the class of the supplied object for use
   in error messages.*/
   method = "astBorder";
   class = astGetClass( this );

/* Check the base Frame of the Plot is 3-D. */
   naxes = astGetNin( this );
   if( naxes != 3 && astOK ){
      astError( AST__NAXIN, "%s(%s): Number of axes (%d) in the base "
                "Frame of the supplied %s is invalid - this number should "
                "be 3.", status, method, class, naxes, class );
   }

/* Check the current Frame of the Plot is 3-D. */
   naxes = astGetNout( this );
   if( naxes != 3 && astOK ){
      astError( AST__NAXIN, "%s(%s): Number of axes (%d) in the current "
                "Frame of the supplied %s is invalid - this number should "
                "be 3.", status, method, class, naxes, class );
   }

/* Invoke the astBorder method on each of the three encapsulated Plots. */
   flag1 = astBorder( this->plotxy );
   flag2 = astBorder( this->plotxz );
   flag3 = astBorder( this->plotyz );

/* If no bad values were encountered in any of the Plots, draw lines
   along the remaining plot edges. */
   result = ( flag1 || flag2 || flag3 );
   if( !result ) {

/* The three remaining edges ot be drawn all meet at the corner
   diagonally opposite the root corner. Get the root corner. */
      root_corner = astGetRootCorner( this );

/* The (x0,y0,z0) position is the graphics coords at the corner
   diagonally opposite the root corner. The x1, y1 and z1 values
   are the graphics x, y and z values at the ends of the three
   lines that remain to be drawn. */
      if( root_corner & 1 ) {
         x[ 0 ] = this->gbox[ 0 ];
         x1 = this->gbox[ 3 ];
      } else {
         x[ 0 ] = this->gbox[ 3 ];
         x1 = this->gbox[ 0 ];
      }

      if( root_corner & 2 ) {
         y[ 0 ] = this->gbox[ 1 ];
         y1 = this->gbox[ 4 ];
      } else {
         y[ 0 ] = this->gbox[ 4 ];
         y1 = this->gbox[ 1 ];
      }

      if( root_corner & 4 ) {
         z[ 0 ] = this->gbox[ 2 ];
         z1 = this->gbox[ 5 ];
      } else {
         z[ 0 ] = this->gbox[ 5 ];
         z1 = this->gbox[ 2 ];
      }

/* Establish the correct graphical attributes as defined by attributes
   with the supplied Plot. */
      astGrfAttrs( this, AST__BORDER_ID, 1, GRF__LINE, method, class );

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
      LOCK_MUTEX2;

/* Draw the remaining line parallel to the X axis. */
      x[ 1 ] = x1;
      y[ 1 ] = y[ 0 ];
      z[ 1 ] = z[ 0 ];
      ok = astG3DLine( 2, x, y, z );

/* Draw the remaining line parallel to the Y axis. */
      x[ 1 ] = x[ 0 ];
      y[ 1 ] = y1;
      z[ 1 ] = z[ 0 ];
      ok = ok && astG3DLine( 2, x, y, z );

/* Draw the remaining line parallel to the X axis. */
      x[ 1 ] = x[ 0 ];
      y[ 1 ] = y[ 0 ];
      z[ 1 ] = z1;
      ok = ok && astG3DLine( 2, x, y, z );

/* Allow the next thread to proceed. */
      UNLOCK_MUTEX2;

/* Re-establish the original graphical attributes. */
      astGrfAttrs( this, AST__BORDER_ID, 0, GRF__LINE, method, class );

/* Report an error if anything went wrong in the grf3d module. */
      if( !ok && astOK ) {
         astError( AST__GRFER, "%s(%s): Graphics error in astG3DLine. ", status,
                   method, class );
      }
   }

/* Return zero if an error has occurrred. */
   if( !astOK ) result = 0;

/* Return a flag indicating if any bad values were encountered in any of
   the Plots. */
   return result;
}

static AstObject *Cast( AstObject *this_object, AstObject *obj, int *status ) {
/*
*  Name:
*     Cast

*  Purpose:
*     Cast an Object into an instance of a sub-class.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     AstObject *Cast( AstObject *this, AstObject *obj, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the protected astCast
*     method inherited from the Frame class).

*  Description:
*     This function returns a deep copy of an ancestral component of the
*     supplied object. The required class of the ancestral component is
*     specified by another object. Specifically, if "this" and "new" are
*     of the same class, a copy of "this" is returned. If "this" is an
*     instance of a subclass of "obj", then a copy of the component
*     of "this" that matches the class of "obj" is returned. Otherwise,
*     a NULL pointer is returned without error.

*  Parameters:
*     this
*        Pointer to the Object to be cast.
*     obj
*        Pointer to an Object that defines the class of the returned Object.
*        The returned Object will be of the same class as "obj".

*  Returned Value:
*     A pointer to the new Object. NULL if "this" is not a sub-class of
*     "obj", or if an error occurs.

*  Notes:
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables; */
   AstObject *new;
   astDECLARE_GLOBALS
   int generation_gap;

/* Initialise */
   new = NULL;

/* Check inherited status */
   if( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* See how many steps up the class inheritance ladder it is from "obj"
   to this class (Plot3D). A positive value is returned if Plot3D
   is a sub-class of "obj". A negative value is returned if "obj" is
   a sub-class of Plot3D. Zero is returned if "obj" is a Plot3D.
   AST__COUSIN is returned if "obj" is not on the same line of descent
   as Plot3D. */
   generation_gap = astClassCompare( (AstObjectVtab *) &class_vtab,
                                     astVTAB( obj ) );

/* If "obj" is a Plot3D or a sub-class of Plot3D, we can cast by
   truncating the vtab for "this" so that it matches the vtab of "obJ",
   and then taking a deep copy of "this". */
   if( generation_gap <= 0 && generation_gap != AST__COUSIN ) {
      new = astCastCopy( this_object, obj );

/* If "obj" is a Plot (the parent class), we cast by returning a deep
   copy of the Plot covering the XY face. */
   } else if( generation_gap == 1 ) {
      new = astCopy( ( (AstPlot3D *) this_object)->plotxy );

/* If "obj" is a FrameSet or higher, we attempt to use the implementation
   inherited from the parent class to cast the FrameSet component into the
   class indicated by "obj". */
   } else {
      new = (*parent_cast)( this_object, obj, status );
   }

/* Return the new pointer. */
   return new;
}

static void ChangeRootCorner( AstPlot3D *this, int old, int new, int *status ){
/*
*  Name:
*     ChangeRootCorner

*  Purpose:
*     Use a new RootCorner value.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void ChangeRootCorner( AstPlot3D *this, int old, int new, int *status )

*  Class Membership:
*     Plot method.

*  Description:
*     This function sets the attributes of the encapsulated Plots so that
*     labels appear on the edges of the 3D graphics cube that join at the
*     specified new root corner. It also reverses the graphics axes in
*     the encapsulated Plots as required in order to ensure that the Plots
*     look "normal" when viewed from the outside of the 3D graphics cube.
*     This happens if a the Plot used to label a specific axis moves from
*     one face the the 3D graphics cube to the opposite face.

*  Parameters:
*     this
*        Pointer to a Plot3D.
*     old
*        The old RootCorner value.
*     new
*        The new RootCorner value.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Each RootCorner value is in the range 0 to 7 and is a 3 bit
*     bit-mask. Bit 0 describes the 3D graphics X axis, bit 1 describes
*     the graphics Y axis and bit 2 describes the graphics Z axis. If a
*     bit is set it means that the corner is at the upper bound on the
*     corresponding axis. If a bit is unset it means that the corner is
*     at the lower bound on the corresponding axis.

*-
*/

/* Local Variables: */
   AstKeyMap *grfcon;
   AstPlot *plot;
   AstPlot *plots[ 24 ];
   int edges[ 24 ];
   int axes[ 24 ];
   int axis2d;
   int edge;
   int i;
   int np;
   int xeqy;
   int xeqz;

/* Check the global status. */
   if( !astOK ) return;

/* If the corner has moved on the 3D X axis (from upper X bound to lower X
   bound or vice-versa), mirror the axis of the encapsulated Plot that is
   perpendicular to the 3D X axis. This means that the Plot can be thought
   of as being viewed from the outside of the 3D graphics cube. Also,
   update the constant X axis value at which the YZ plane is drawn. */
   if( ( old & 1 ) != ( new & 1 ) ) astMirror( this->plotyz, 0 );
   grfcon = (AstKeyMap *) astGetGrfContext( this->plotyz );
   astMapPut0D( grfcon, "Gcon", this->gbox[ ( new & 1 ) ? 3 : 0 ], "Constant X value" );
   astMapPut0I( grfcon, "RootCorner", new, "Labelled corner" );
   grfcon= astAnnul( grfcon );

/* Likewise mirror the other two axes if required. */
   if( ( old & 2 ) != ( new & 2 ) ) astMirror( this->plotxz, 0 );
   grfcon = (AstKeyMap *) astGetGrfContext( this->plotxz );
   astMapPut0D( grfcon, "Gcon", this->gbox[ ( new & 2 ) ? 4 : 1 ], "Constant Y value" );
   astMapPut0I( grfcon, "RootCorner", new, "Labelled corner" );
   grfcon= astAnnul( grfcon );

   if( ( old & 4 ) != ( new & 4 ) ) astMirror( this->plotxy, 0 );
   grfcon = (AstKeyMap *) astGetGrfContext( this->plotxy );
   astMapPut0D( grfcon, "Gcon", this->gbox[ ( new & 4 ) ? 5 : 2 ], "Constant Z value" );
   astMapPut0I( grfcon, "RootCorner", new, "Labelled corner" );
   grfcon= astAnnul( grfcon );

/* Set a flag saying whether the limits are equal at the new corner for
   the X and Y axes. */
   xeqy = ( ( ( new & 1 ) > 0 ) == ( ( new & 2 ) > 0 ) );

/* Set a flag saying whether the limits are equal at the new corner for
   the X and Z axes. */
   xeqz = ( ( ( new & 1 ) > 0 ) == ( ( new & 4 ) > 0 ) );

/* Ensure all Edge attributes are clear. This means that the public
   attribute accessors routines used below will return dynamic defaults for
   the Edge attributes. */
   astClearEdge( this->plotxy, 0 );
   astClearEdge( this->plotxy, 1 );
   astClearEdge( this->plotxz, 0 );
   astClearEdge( this->plotxz, 1 );
   astClearEdge( this->plotyz, 0 );
   astClearEdge( this->plotyz, 1 );

/* So far we have recorded no edges changes. */
   np = 0;

/* We now adjust the Edge attributes in the Plot used to annotate the 3D
   X axis in order to get the X axis labels on the correct edge of the 3D
   graphics cube. Get the Plot used to produce X axis labels (this will
   be either this->plotxy or this->plotxz). */
   plot = AxisPlot( this, 0, &axis2d, status );

/* See what edge of the Plot is used to annotate the first of the two WCS
   axis described by the 2D Plot. If the Edge(1) attribute has not been
   assigned a value, then a dynamic default will be used by the Plot class.
   We want to know what this dynamic default is, so we first cleared the
   attribute above. Now we set the attribute value explicitly to the dynamic
   default returned by astGetC. Note, we use astGetC rather than astGetEdge
   because the dynamic default is not calculated when calling astGetEdge. */
   astSetC( plot, "Edge(1)", astGetC( plot, "Edge(1)" ));
   edge = astGetEdge( plot, 0 );
   astClearEdge( plot, 0 );

/* If the 3D X axis is labelled using the Plot that spans the XY plane... */
   if( plot == this->plotxy ) {

/* ... and if the new root corner is at the upper limit on the Y axis... */
      if( new & 2 ) {

/* If the first WCS axis is currently labelled on either the top or bottom
   edge, ensure it is labelled on the upper Y (top) edge. */
         if( edge == 3 || edge == 1 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = TOP;

/* Otherwise ensure that the second WCS axis is labelled on the upper Y (top)
   edge. */
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = TOP;
         }

/* If the new root corner is at the lower limit on the Y axis... */
      } else {

/* If the first WCS axis is currently labelled on either the top or bottom
   edge, ensure it is labelled on the lower Y (bottom) edge. */
         if( edge == 3 || edge == 1 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = BOTTOM;

/* Otherwise ensure that the second WCS axis is labelled on the lower Y
   (bottom) edge. */
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = BOTTOM;
         }
      }

/* If the 3D X axis is labelled using the Plot that spans the XZ plane... */
   } else {

/* ... and if the new root corner is at the upper limit on the Z axis... */
      if( new & 4 ) {

/* If the first WCS axis is currently labelled on either the top or bottom
   edge, ensure it is labelled on the upper Z (top) edge. */
         if( edge == 3 || edge == 1 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = TOP;

/* Otherwise ensure that the second WCS axis is labelled on the upper Y (top)
   edge. */
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = TOP;
         }

/* If the new root corner is at the lower limit on the Z axis... */
      } else {

/* If the first WCS axis is currently labelled on either the top or bottom
   edge, ensure it is labelled on the lower Z (bottom) edge. */
         if( edge == 3 || edge == 1 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = BOTTOM;

/* Otherwise ensure that the second WCS axis is labelled on the lower Z
   (bottom) edge. */
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = BOTTOM;
         }
      }
   }

/* We now adjust the Edge attributes in the Plot used to annotate the 3D
   Y axis in order to get the Y axis labels on the correct edge of the 3D
   graphics cube. Get the Plot used to produce Y axis labels. */
   plot = AxisPlot( this, 1, &axis2d, status );

/* See what edge of the Plot is used to annotate the first of the two WCS
   axis described by the Plot. */
   astSetC( plot, "Edge(1)", astGetC( plot, "Edge(1)" ));
   edge = astGetEdge( plot, 0 );
   astClearEdge( plot, 0 );

/* If the 3D Y axis is labelled using the Plot that spans the XY plane... */
   if( plot == this->plotxy ) {

/* ... and if the new root corner is at the same limit on the X and Z axes,
   put Y labels on the right side of the Plot.  */
      if( xeqz ) {
         if( edge == 0 || edge == 2 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = RIGHT;
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = RIGHT;
         }

/* If the new root corner is at a different limit on the X and Z axes,
   put Y labels on the left side of the Plot.  */
      } else {
         if( edge == 0 || edge == 2 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = LEFT;
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = LEFT;
         }
      }

/* If the 3D Y axis is labelled using the Plot that spans the YZ plane... */
   } else {

/* ... and if the new root corner is at the upper Z limit, put Y labels on
   the top of the Plot.  */
      if( new & 4 ) {
         if( edge == 1 || edge == 3 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = TOP;
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = TOP;
         }

/* If the new root corner is at the lower Z limit, put Y labels on the
   bottom of the Plot.  */
      } else {
         if( edge == 1 || edge == 3 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = BOTTOM;
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = BOTTOM;
         }
      }
   }

/* We now adjust the Edge attributes in the Plot used to annotate the 3D
   Z axis in order to get the Z axis labels on the correct edge of the 3D
   graphics cube. Get the Plot used to produce Z axis labels. */
   plot = AxisPlot( this, 2, &axis2d, status );

/* See what edge of the Plot is used to annotate the first of the two WCS
   axis described by the Plot. */
   astSetC( plot, "Edge(1)", astGetC( plot, "Edge(1)" ));
   edge = astGetEdge( plot, 0 );
   astClearEdge( plot, 0 );

/* If the 3D Z axis is labelled using the Plot that spans the XZ plane... */
   if( plot == this->plotxz ) {

/* ... and if the new root corner is at the same limit on the X and Y axes,
   put Z labels on the left side of the Plot.  */
      if( xeqy ) {
         if( edge == 0 || edge == 2 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = LEFT;
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = LEFT;
         }

/* If the new root corner is at a different limit on the X and Y axes,
   put Y labels on the right side of the Plot.  */
      } else {
         if( edge == 0 || edge == 2 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = RIGHT;
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = RIGHT;
         }
      }

/* If the 3D Z axis is labelled using the Plot that spans the YZ plane... */
   } else {

/* ... and if the new root corner is at the same limit on the X and Y axes,
   put Z labels on the right side of the Plot.  */
      if( xeqz ) {
         if( edge == 0 || edge == 2 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = RIGHT;
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = RIGHT;
         }

/* If the new root corner is at a different limit on the X and Y axes,
   put Y labels on the left side of the Plot.  */
      } else {
         if( edge == 0 || edge == 2 ) {
            plots[ np ] = plot;
            axes[ np ] = 0;
            edges[ np++ ] = LEFT;
         } else {
            plots[ np ] = plot;
            axes[ np ] = 1;
            edges[ np++ ] = LEFT;
         }
      }
   }

/* Apply the set of edge changes determined above. */
   for( i = 0; i < np; i++ ) {
      astSetEdge( plots[ i ], axes[ i ], edges[ i ] );
   }

/* Ensure that the 2 Plot axes that are not being used have suitable values
   for their attributes. That is, no labels are drawn, and the ticked
   edges are the one that meet at the new RootCorner. */

   if( !astTestEdge( this->plotxy, 0 ) ) {
      astSetEdge( this->plotxy, 0, ( new & 2 ) ? TOP : BOTTOM );
   }

   if( !astTestEdge( this->plotxy, 1 ) ) {
      astSetEdge( this->plotxy, 1, xeqz ? RIGHT: LEFT );
   }

   if( !astTestEdge( this->plotxz, 0 ) ) {
      astSetEdge( this->plotxz, 0, ( new & 4 ) ? TOP : BOTTOM );
   }

   if( !astTestEdge( this->plotxz, 1 ) ) {
      astSetEdge( this->plotxz, 1, xeqy ? LEFT : RIGHT );
   }

   if( !astTestEdge( this->plotyz, 0 ) ) {
      astSetEdge( this->plotyz, 0, ( new & 4 ) ? TOP : BOTTOM );
   }

   if( !astTestEdge( this->plotyz, 1 ) ) {
      astSetEdge( this->plotyz, 1, xeqy ? RIGHT : LEFT );
   }


}

static void Clear( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     Clear

*  Purpose:
*     Clear attribute values for a Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void Clear( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the public astClear method
*     inherited from the Object class).

*  Description:
*     This function clears the values of a specified set of attributes
*     for a Plot3D. Clearing an attribute cancels any value that has
*     previously been explicitly set for it, so that the standard
*     default attribute value will subsequently be used instead. This
*     also causes the astTest function to return the value zero for
*     the attribute, indicating that no value has been set.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     attrib
*        Pointer to a null-terminated character string containing a
*        comma-separated list of the names of the attributes to be
*        cleared.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function preserves the integrity of the Plot3D (if
*     possible) by appropriately modifying the three encapsulated Plots.
*/

/* Check the global error status. */
   if ( !astOK ) return;

/* Invoke the parent astClear method to clear the Plot3D's attribute values. */
   (*parent_clear)( this_object, attrib, status );

/* Update the three 2D Plots stored in the Plot3D structure so that they
   reflect this modified FrameSet. */
   UpdatePlots( (AstPlot3D *) this_object, status );

}

static void ClearAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     ClearAttrib

*  Purpose:
*     Clear an attribute value for a Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot.h"
*     void ClearAttrib( AstObject *this, const char *attrib )

*  Class Membership:
*     Plot3D member function (over-rides the astClearAttrib protected
*     method inherited from the Plot class).

*  Description:
*     This function clears the value of a specified attribute for a
*     Plot3D, so that the default value will subsequently be used.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     attrib
*        Pointer to a null terminated string specifying the attribute
*        name.  This should be in lower case with no surrounding white
*        space.
*/

/* Local Variables: */
   AstPlot3D *this;              /* Pointer to the Plot3D structure */
   int axis;                     /* Axis index */
   int len;                      /* Length of attrib string */
   int nc;                       /* Number of characters read */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_object;

/* Obtain the length of the "attrib" string. */
   len = strlen( attrib );

/* Check the attribute name and clear the appropriate attribute. */

/* Norm. */
/* ----------- */
   if ( !strcmp( attrib, "norm" ) ) {
      astClearNorm( this, 0 );
      astClearNorm( this, 1 );
      astClearNorm( this, 2 );

/* Norm(axis). */
/* ----------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "norm(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      astClearNorm( this, axis - 1 );

/* RootCorner. */
/* ----------- */
   } else if ( !strcmp( attrib, "rootcorner" ) ) {
      astClearRootCorner( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_clearattrib)( this_object, attrib, status );
   }
}

static void ClearCurrent( AstFrameSet *this_frameset, int *status ) {
/*
*  Name:
*     ClearCurrent

*  Purpose:
*     Clear the value of the Current attribute for a Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int astClearCurrent( AstFrameSet *this )

*  Class Membership:
*     Plot3D member function (over-rides the public astClearCurrent method
*     inherited from the FrameSet class).

*  Description:
*     This function clears the value of the Current attribute for a
*     Plot3D. This attribute is an index that identifies the current
*     Frame for the Plot3D.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*/

/* Invoke the parent astClearCurrent method. */
   (*parent_clearcurrent)( this_frameset, status );

/* Update the three 2D Plots stored in the Plot3D structure so that they
   reflect this modified FrameSet. */
   UpdatePlots( (AstPlot3D *) this_frameset, status );
}

static void ClearRootCorner( AstPlot3D *this, int *status ){
/*
*+
*  Name:
*     astClearRootCorner

*  Purpose:
*     Clear the RootCorner attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "plot3d.h"
*     void astClearRootCorner( AstPlot3D *this )

*  Class Membership:
*     Plot method.

*  Description:
*     This function clears the RootCorner attribute.

*  Parameters:
*     this
*        Pointer to a Plot3D.

*-
*/

/* Local Variables: */
   int old;
   int new;

/* Check the global status. */
   if( !astOK ) return;

/* Get the current rootcorner value. */
   old = astGetRootCorner( this );

/* Clear the RootCorner attribute. */
   this->rootcorner = -1;

/* Get the new (default) rootcorner value. */
   new = astGetRootCorner( this );

/* If the root corner has changed, mirror any axes of the encapsulated Plots
   that need mirroring (this is done to ensure that Plots look right when
   viewed from the outside of the graphics cube), and modify the Edge
   attributes in the encapsulated Plots to ensure the labels appear on the
   requested edges of the 3D graphics cube. . */
   if( old != new ) ChangeRootCorner( this, old, new, status );
}

static void CreatePlots( AstPlot3D *this, AstFrameSet *fset, const float *gbox,
                         const double *bbox, int *status ) {
/*
*  Name:
*     CreatePlots

*  Purpose:
*     Create three 2D plots and store in the Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void CreatePlots( AstPlot3D *this, AstFrameSet *fset, const float *gbox,
                        const double *bbox, int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function splits the supplied FrameSet up into 3 independent 2D
*     FrameSets, each describing a 2D plane in the supplied 3D FrameSet.
*     It then uses these 2D FrameSets to create three Plots, one for each
*     plane in the graphics plotting space, and stores them in the Plot3D.
*
*     Each of the three Plots is notionally pasted onto one face of the
*     3D graphics cube (the RootCorner attribute is used to determine which
*     of the two parallel faces a particular Plot is pasted onto). The
*     Plot is pasted in such a way that, when viewed from the outside of
*     the graphics cube, the first graphics axis increases left to right
*     and the second increases bottom to top (this assumes that "up" is
*     parallel to the 3D Z axis).
*
*     Initially, the Plots are created assuming the default RootCorner
*     value ("LLL"). They will be changed later if the value of the
*     RootCorner attribute is changed.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     fset
*        Pointer to the FrameSet.
*     gbox
*        A pointer to an array of 6 values giving the graphics coordinates
*        of the bottom left and top right corners of a box on the graphics
*        output device. The first triple of values should be the graphics
*        coordinates of the bottom left corner of the box and the second
*        triple of values are the graphics coordinates of the top right corner.
*     bbox
*        A pointer to an array of 6 values giving the coordinates in the
*        supplied Frame or base Frame of the supplied FrameSet at the bottom
*        left and top right corners of the box specified by parameter gbox.
*        These should be supplied in the same order as for parameter "gbox".
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Each returned plot has 3 Frames: Frame 1 is the base (GRAPHICS)
*     Frame; Frame 2 is spanned by 2 of the 3 axes in the base Frame of
*     the supplied FrameSet; Frame 3 is the current Frame and is spanned
*     by 2 of the 3 axes in the current Frame of the supplied FrameSet.
*     Any future changes to this function that alter this structure should
*     reflected in equivalent changes to function UpdatePlots.

*/

/* Local Variables: */
   AstFrameSet *fsetxy;
   AstFrameSet *fsetxz;
   AstFrameSet *fsetyz;
   double basebox2d[ 4 ];
   float graphbox2d[ 4 ];
   int baseplane;
   int labelxy[ 2 ];
   int labelxz[ 2 ];
   int labelyz[ 2 ];
   int wcsxy[ 2 ];
   int wcsxz[ 2 ];
   int wcsyz[ 2 ];

/* Check the inherited status. */
   if( !astOK ) return;

/* Split the supplied FrameSet up into 3 FrameSets, each with a 2D base
   and current Frame. Each of these FrameSets describes one plane of
   the 3D cube. One of them will be spanned by two axes picked from the
   supplied 3D FrameSet. The other two FrameSets will each include a copy
   of the remaining 3rd axis from the supplied FrameSet, plus an extra
   dummy axis. These dummy axes will never be labelled. */
   SplitFrameSet( fset, &fsetxy, labelxy, wcsxy, &fsetxz, labelxz, wcsxz,
                  &fsetyz, labelyz, wcsyz, &baseplane, status );

/* If OK, annul any existing 2D plots. */
   if( astOK ) {
      if( this->plotxy ) this->plotxy = astAnnul( this->plotxy );
      if( this->plotxz ) this->plotxz = astAnnul( this->plotxz );
      if( this->plotyz ) this->plotyz = astAnnul( this->plotyz );

/* Create three Plots; one for each 2D plane in the graphics plotting
   space. Set the attributes of these plots so that the required axes are
   labelled and other axes are left blank. The "graphbox2d" and "basebox2d"
   values used to create each Plot define the sense, as well as the extent,
   of each axis. The first pair of values in each give the lower left corner
   of the Plot and the second pair give the top right corner. We want each
   Plot to have X increasing left to right and Y increasing bottom to
   top when viewed from the outside of the cube. We assume an initial
   RootCorner value of "LLL" (that is, the Plots are pasted onto the cube
   faces that meet at the lower limit on every axis). */
      graphbox2d[ 0 ] = gbox[ 3 ];
      graphbox2d[ 1 ] = gbox[ 1 ];
      graphbox2d[ 2 ] = gbox[ 0 ];
      graphbox2d[ 3 ] = gbox[ 4 ];

      basebox2d[ 0 ] = bbox[ 3 ];
      basebox2d[ 1 ] = bbox[ 1 ];
      basebox2d[ 2 ] = bbox[ 0 ];
      basebox2d[ 3 ] = bbox[ 4 ];

      if( this->plotxy ) this->plotxy = astAnnul( this->plotxy );
      this->plotxy = astPlot( fsetxy, graphbox2d, basebox2d, "", status );
      SetPlotAttr( this->plotxy, XY, labelxy, status );

      graphbox2d[ 0 ] = gbox[ 0 ];
      graphbox2d[ 1 ] = gbox[ 2 ];
      graphbox2d[ 2 ] = gbox[ 3 ];
      graphbox2d[ 3 ] = gbox[ 5 ];

      basebox2d[ 0 ] = bbox[ 0 ];
      basebox2d[ 1 ] = bbox[ 2 ];
      basebox2d[ 2 ] = bbox[ 3 ];
      basebox2d[ 3 ] = bbox[ 5 ];

      this->plotxz = astPlot( fsetxz, graphbox2d, basebox2d, "", status );
      SetPlotAttr( this->plotxz, XZ, labelxz, status );

      graphbox2d[ 0 ] = gbox[ 4 ];
      graphbox2d[ 1 ] = gbox[ 2 ];
      graphbox2d[ 2 ] = gbox[ 1 ];
      graphbox2d[ 3 ] = gbox[ 5 ];

      basebox2d[ 0 ] = bbox[ 4 ];
      basebox2d[ 1 ] = bbox[ 2 ];
      basebox2d[ 2 ] = bbox[ 1 ];
      basebox2d[ 3 ] = bbox[ 5 ];

      this->plotyz = astPlot( fsetyz, graphbox2d, basebox2d, "", status );
      SetPlotAttr( this->plotyz, YZ, labelyz, status );

/* Store information that allows each 3D WCS axis to be associatedf with
   a pair of Plots. Also store the WCS axis within each Plot that
   corresponds to the 3D WCS axis. */
      StoreAxisInfo( this, labelxy, wcsxy, labelxz, wcsxz, labelyz, wcsyz, status );

/* Store the Plot that spans two connected 3D axes. */
      this->baseplot = baseplane;

/* Free resources */
      fsetxy = astAnnul( fsetxy );
      fsetxz = astAnnul( fsetxz );
      fsetyz = astAnnul( fsetyz );

   }
}

static int Element2D( AstPlot3D *this, int element, int *elem2d1,
                      int *elem2d2, int *status ){
/*
*  Name:
*     Element2D

*  Purpose:
*     Convert a 3D graphics element identifier to a corresponding pair of
*     2D identifiers.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Element2D( AstPlot3D *this, int element, int *elem2d1,
*                    int *elem2d2, int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function takes an integer identifier for an element of a 3D
*     annotated grid (e.g. ticks, axis 1 labels, border, etc), and returns
*     a element identifers that can be used with the encapsualted 2D Plots.

*  Parameters:
*     this
*        Pointer to the Plot2D structure.
*     element
*        The 3D element identifier to convert.
*     elem2d1
*        Pointer to an int in which to return the 2D element identifier
*        to use with the first of the two Plots that span the axis to
*        which the 3D element identifier refers. Returned holding 0 if
*        the given 3D element identifier is not axis specific.
*     elem2d2
*        Pointer to an int in which to return the 2D element identifier
*        to use with the second of the two Plots that span the axis to
*        which the 3D element identifier refers. Returned holding 0 if
*        the given 3D element identifier is not axis specific.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The zero-based index of the 3D axis to which the given element
*     identifier refers, or -1 if the element identifier is not axis
*     specific.

*/

/* Local Variables: */
   int axis3d;

/* Check the global error status. */
   if ( !astOK ) return 0;

/* Get the zero-based index of the 3D axis to which the supplied element
   refers. Use an index of -1 to indicate that the element does not
   relate to a specific axis. Also get the corresponding element to use
   with the two Plots that share the speified 3D axis. */

/* Define a macro used to set the 2d element identifiers for a given 3d
   element identifier. */

#define SET_ELEM2D(id1,id2) \
   *elem2d1 = this->axis_index1[ axis3d ] ? id2 : id1; \
   *elem2d2 = this->axis_index2[ axis3d ] ? id2 : id1;

   if( element == AST__BORDER_ID ){
      axis3d = -1;

   } else if( element == AST__CURVE_ID ){
      axis3d = -1;

   } else if( element == AST__TITLE_ID ){
      axis3d = -1;

   } else if( element == AST__MARKS_ID ){
      axis3d = -1;

   } else if( element == AST__TEXT_ID ){
      axis3d = -1;

   } else if( element == AST__AXIS1_ID ){
      axis3d = 0;
      SET_ELEM2D(AST__AXIS1_ID,AST__AXIS2_ID)

   } else if( element == AST__AXIS2_ID ){
      axis3d = 1;
      SET_ELEM2D(AST__AXIS1_ID,AST__AXIS2_ID)

   } else if( element == AST__AXIS3_ID ){
      axis3d = 2;
      SET_ELEM2D(AST__AXIS1_ID,AST__AXIS2_ID)

   } else if( element == AST__NUMLAB1_ID ){
      axis3d = 0;
      SET_ELEM2D(AST__NUMLAB1_ID,AST__NUMLAB2_ID)

   } else if( element == AST__NUMLAB2_ID ){
      axis3d = 1;
      SET_ELEM2D(AST__NUMLAB1_ID,AST__NUMLAB2_ID)

   } else if( element == AST__NUMLAB3_ID ){
      axis3d = 2;
      SET_ELEM2D(AST__NUMLAB1_ID,AST__NUMLAB2_ID)

   } else if( element == AST__TEXTLAB1_ID ){
      axis3d = 0;
      SET_ELEM2D(AST__TEXTLAB1_ID,AST__TEXTLAB2_ID)

   } else if( element == AST__TEXTLAB2_ID ){
      axis3d = 1;
      SET_ELEM2D(AST__TEXTLAB1_ID,AST__TEXTLAB2_ID)

   } else if( element == AST__TEXTLAB3_ID ){
      axis3d = 2;
      SET_ELEM2D(AST__TEXTLAB1_ID,AST__TEXTLAB2_ID)

   } else if( element == AST__TICKS1_ID ){
      axis3d = 0;
      SET_ELEM2D(AST__TICKS1_ID,AST__TICKS2_ID)

   } else if( element == AST__TICKS2_ID ){
      axis3d = 1;
      SET_ELEM2D(AST__TICKS1_ID,AST__TICKS2_ID)

   } else if( element == AST__TICKS3_ID ){
      axis3d = 2;
      SET_ELEM2D(AST__TICKS1_ID,AST__TICKS2_ID)

   } else if( element == AST__GRIDLINE1_ID ){
      axis3d = 0;
      SET_ELEM2D(AST__GRIDLINE1_ID,AST__GRIDLINE2_ID)

   } else if( element == AST__GRIDLINE2_ID ){
      axis3d = 1;
      SET_ELEM2D(AST__GRIDLINE1_ID,AST__GRIDLINE2_ID)

   } else if( element == AST__GRIDLINE3_ID ){
      axis3d = 2;
      SET_ELEM2D(AST__GRIDLINE1_ID,AST__GRIDLINE2_ID)

   } else {
      axis3d = 0;
      astError( AST__INTER, "Element2D(Plot3D): The MAKE_CLEAR2 macro "
                "does not yet support element index %d (internal "
                "AST programming error).", status, element );
   }

#undef SET_ELEM2D

   return axis3d;

}

static int Equal( AstObject *this_object, AstObject *that_object, int *status ) {
/*
*  Name:
*     Equal

*  Purpose:
*     Test if two Plot3Ds are equivalent.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Equal( AstObject *this, AstObject *that, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the astEqual protected
*     method inherited from the Plot Object class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     two Plot3Ds are equivalent.

*  Parameters:
*     this
*        Pointer to the first Plot3D.
*     that
*        Pointer to the second Plot3D.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     One if the Plot3Ds are equivalent, zero otherwise.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstPlot3D *that;         /* Pointer to the second Plot3D structure */
   AstPlot3D *this;         /* Pointer to the first Plot3D structure */
   int result;                /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Invoke the Equal method inherited from the parent Plot class. This checks
   that the Plots are both of the same class (amongst other things). */
   if( (*parent_equal)( this_object, that_object, status ) ) {

/* Obtain pointers to the two Plot3D structures. */
      this = (AstPlot3D *) this_object;
      that = (AstPlot3D *) that_object;

/* Check the encapsulated Plots for equality. */
      result = ( astEqual( this->plotxz, that->plotxz ) &&
                 astEqual( this->plotyz, that->plotyz ) &&
                 astEqual( this->plotxy, that->plotxy ) );
   }

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static AstPointSet *ExtendTicks( AstPlot *plot, AstPointSet *ticks, int *status ){
/*
*  Name:
*     ExtendTicks

*  Purpose:
*     Add an extra tick to the start and end of a list of tick marks.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     AstPointSet *ExtendTicks( AstPlot *plot, AstPointSet *ticks, int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function takes a list of tick marks drawn using the supplied
*     Plot, and adds in an extra tick mark at the start and end of the
*     supplied list of ticks, returning the expanded list in a new
*     PointSet. The extra points are guaranteed to fall outside the area
*     enclosed within the supplied Plot.

*  Parameters:
*     plot
*        The Plot that was used to generate the list of tick marks.
*     ticks
*        A PointSet holding the 2D graphics coordinates (within the base
*        Frame of the supplied Plot) at which each tick mark starts.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a new PointSet that has 2 more entries than the
*     supplied PointSet. The first point is a new tick mark, then comes
*     all the ticks mark positions supplied in "ticks", and finally there
*     is another new tick mark.

*/

/* Local Variables: */
   AstPointSet *result;
   double **ptr_in;
   double **ptr_out;
   double *a_in;
   double *a_out;
   double *b_in;
   double *b_out;
   double *p;
   double delta;
   double hi;
   double lo;
   double range[ 2 ];
   double v;
   int axis;
   int i;
   int increasing;
   int np;

/* Check inherited status */
   if( !astOK || !ticks ) return NULL;

/* Get the number of tick marsk in the supplied PointSet and get pointers
   to the 3D Graphics values contained in the PointSet. */
   np = astGetNpoint( ticks );
   ptr_in = astGetPoints( ticks );

/* Create the returned PointSet with room for an extra pair of ticks. Get
   pointers to its data arrays */
   result = astPointSet( np + 2, 2, "", status );
   ptr_out = astGetPoints( result );

/* Check the pointers can be used safely. */
   if( astOK ) {

/* Find the index of the 2D graphics axis (0 or 1) that varies along the
   set of tick marks. We do this by finding the max and min value
   supplied for each axis, and then choosing the axis that has the highest
   range. */
      for( axis = 0; axis < 2; axis++ ) {
         hi = -DBL_MAX;
         lo = DBL_MAX;
         p = ptr_in[ axis ];

         for( i = 0; i < np; i++, p++ ) {
            v = *p;
            if( v != AST__BAD ) {
               if( v > hi ) hi = v;
               if( v < lo ) lo = v;
            }
         }

         if( lo != DBL_MAX ) {
            range[ axis ] = hi - lo;
         } else {
            astError( AST__INTER, "ExtendTicks{Plot3D): no good ticks on "
                      "axis %d (internal AST prgramming error).", status, axis );
         }
      }


/* Get the index of the axis with the largest range (the other range
   should be zero). */
      axis = ( range[ 0 ] > range[ 1 ] ) ? 0 : 1;

/* Copy the input graphics positions to the output PointSet, and add an
   extra position at the beginning and end of the output PointSet. */
      if( axis == 0 ) {
         lo = plot->xlo;
         hi = plot->xhi;
         a_in = ptr_in[ 0 ];
         b_in = ptr_in[ 1 ];
         a_out = ptr_out[ 0 ];
         b_out = ptr_out[ 1 ];

      } else {
         lo = plot->ylo;
         hi = plot->yhi;
         a_in = ptr_in[ 1 ];
         b_in = ptr_in[ 0 ];
         a_out = ptr_out[ 1 ];
         b_out = ptr_out[ 0 ];
      }

      delta = 0.2*( hi - lo );

      if( a_in[ 0 ] < a_in[ 1 ] ) {
         increasing = 1;
         *(a_out++) = lo - delta;
      } else {
         increasing = 0;
         *(a_out++) = hi + delta;
      }

      *(b_out++) = *b_in;
      for( i = 0; i < np; i++ ) {
         *(a_out++) = *(a_in++);
         *(b_out++) = *(b_in++);
      }
      *(b_out++) = b_in[ -1 ];


      if( increasing ) {
         *(a_out++) = hi + delta;
      } else {
         *(a_out++) = lo - delta;
      }
   }

/* Return the extended PointSet. */
   return result;
}

static AstFrameSet *Fset3D( AstFrameSet *fset, int ifrm, int *status ) {
/*
*  Name:
*     Fset3D

*  Purpose:
*     Create a FrameSet with no more than 3 dimensions for a given Frame.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     AstFrameSet *Fset3D( AstFrameSet *fset, int ifrm, int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function checks a specified Frame in the supplied FrameSet.
*     If the Frame has more than 3 dimensions, a new Frame is added to
*     the FrameSet containing just the first three axes of the specified
*     Frame. A PermMap is used to connect this Frame to the specified
*     Frame, which supplied bad values for any missing axes. If the
*     specified Frame is the base Frame in the supplied FrameSet, then the
*     new Frame becomes the base Frame in the returned FrameSet. Like-wise,
*     if the specified Frame is the current Frame, then the new Frame
*     will be the current Frame in the returned FrameSet.
*
*     If the specified Frame does not have more than 3 axes, then a clone
*     of the FrameSet pointer is returned, otherwise the returned pointer
*     points to a copy of the supplied FrameSet with the new 3-D Frame
*     added.

*  Parameters:
*     fset
*        Pointer to the FrameSet.
*     ifrm
*        The index of the Frame to check. This should be AST__BASE or
*        AST_CURRENT.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a FrameSet in which the Frame with index given by ifrm
*     has no more than 3 axes.
*/

/* Local Variables: */
   AstFrame *frm;
   AstFrame *newfrm;
   AstFrameSet *ret;
   AstPermMap *map;
   double zero;
   int *inperm;
   int axes[3];
   int i;
   int ic;
   int nax;

/* Check the inherited status. */
   if( !astOK ) return NULL;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   map = NULL;

/* Get a pointer to the requested Frame in the supplied FrameSet. */
   frm = astGetFrame( fset, ifrm );

/* See how many dimensions the specified Frame of the supplied FrameSet
   has. */
   nax = astGetNaxes( frm );

/* If it is more than 3-dimensionbal, create a 3D Frame by picking
   axes 1, 2 and 3 from the original Frame. */
   if( nax > 3 ) {
      axes[ 0 ] = 0;
      axes[ 1 ] = 1;
      axes[ 2 ] = 2;
      newfrm = astPickAxes( frm, 3, axes, NULL );

/* Create a PermMap to describe the mapping between the two Frames.
   Use zero as the value for unknown axes (the optional mapping which
   can be returned by astPickAxes uses AST__BAD for unknown axes). */
      inperm = (int *) astMalloc( sizeof(int)*(size_t) nax );
      if( astOK ){
         inperm[ 0 ] = 0;
         inperm[ 1 ] = 1;
         inperm[ 2 ] = 2;
         for( i = 3; i < nax; i++ ) inperm[ i ] = -1;
         zero = 0.0;
         map = astPermMap( nax, inperm, 3, axes, &zero, "", status );
         inperm = (int *) astFree( (void *) inperm );
      }

/* Get a copy of the supplied FrameSet. */
      ret = astCopy( fset );

/* Add the new Frame to the FrameSet (it becomes the current Frame). */
      ic = astGetCurrent( ret );
      astAddFrame( ret, ifrm, map, newfrm );
      newfrm = astAnnul( newfrm );

/* If the new Frame was derived from the base frame, set the new base
   Frame, and re-instate the original current Frame */
      if( ifrm == AST__BASE ){
         astSetBase( ret, astGetCurrent( ret ) );
         astSetCurrent( ret, ic );
      }

/* If the specified Frame in the supplied FrameSet is 3-dimensional, just
   return a clone of it. */
   } else {
      ret = astClone( fset );
   }

/* Annul the pointer to the original Frame. */
   frm = astAnnul( frm );

   return ret;

}

static const char *GetAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     GetAttrib

*  Purpose:
*     Get the value of a specified attribute for a Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     const char *GetAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the protected astGetAttrib
*     method inherited from the FrameSet class).

*  Description:
*     This function returns a pointer to the value of a specified
*     attribute for a Plot3D, formatted as a character string.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     attrib
*        Pointer to a null terminated string containing the name of
*        the attribute whose value is required. This name should be in
*        lower case, with all white space removed.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     - Pointer to a null terminated string containing the attribute
*     value.

*  Notes:
*     - The returned string pointer may point at memory allocated
*     within the Plot3D, or at static memory. The contents of the
*     string may be over-written or the pointer may become invalid
*     following a further invocation of the same function or any
*     modification of the Plot3D. A copy of the string should
*     therefore be made if necessary.
*     - A NULL pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstPlot3D *this;              /* Pointer to the Plot3D structure */
   const char *result;           /* Pointer value to return */
   double dval;                  /* Floating point attribute value */
   int axis;                     /* Axis index */
   int ival;                     /* Int attribute value */
   int len;                      /* Length of attrib string */
   int nc;                       /* Number of character read */

/* Initialise. */
   result = NULL;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(this_object);

/* Obtain a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Compare "attrib" with each recognised attribute name in turn,
   obtaining the value of the required attribute. If necessary, write
   the value into "getattrib_buff" as a null terminated string in an appropriate
   format.  Set "result" to point at the result string. */

/* Norm(axis). */
/* ----------- */
   if ( nc = 0,
        ( 1 == astSscanf( attrib, "norm(%d)%n", &axis, &nc ) )
          && ( nc >= len ) ) {
      dval = astGetNorm( this, axis - 1 );
      if ( astOK ) {
         (void) sprintf( getattrib_buff, "%.*g", DBL_DIG, dval );
         result = getattrib_buff;
      }

/* RootCorner. */
/* ----------- */
   } else if ( !strcmp( attrib, "rootcorner" ) ) {
      ival = astGetRootCorner( this );
      result = RootCornerString( ival, status );
      if( !result && astOK ) {
         astError( AST__INTER, "astGetAttrib(Plot3D): Illegal value %d "
                   "for RootCorner attribute (internal AST programming "
                   "error).", status, ival );
      }

/* If the attribute name was not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_getattrib)( this_object, attrib, status );
   }

/* Return the result. */
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
*     #include "plot3d.h"
*     int GetObjSize( AstObject *this, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the astGetObjSize protected
*     method inherited from the parent class).

*  Description:
*     This function returns the in-memory size of the supplied Plot3D,
*     in bytes.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The Object size, in bytes.

*  Notes:
*     - A value of zero will be returned if this function is invoked
*     with the global status set, or if it should fail for any reason.
*/

/* Local Variables: */
   AstPlot3D *this;         /* Pointer to Plot3D structure */
   int result;              /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointers to the Plot3D structure. */
   this = (AstPlot3D *) this_object;

/* Invoke the GetObjSize method inherited from the parent class, and then
   add on any components of the class structure defined by this class
   which are stored in dynamically allocated memory. */
   result = (*parent_getobjsize)( this_object, status );

   result += astGetObjSize( this->plotxy );
   result += astGetObjSize( this->plotxz );
   result += astGetObjSize( this->plotyz );

/* If an error occurred, clear the result value. */
   if ( !astOK ) result = 0;

/* Return the result, */
   return result;
}

static void Grid( AstPlot *this_plot, int *status ) {
/*
*  Name:
*     Grid

*  Purpose:
*     Draw a set of labelled coordinate axes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void Grid( AstPlot *this, int *status )

*  Class Membership:
*     Plot member function (over-rides the astGrid method inherited from
*     the Plot class).

*  Description:
*     This function draws a complete annotated set of 3-dimensional
*     coordinate axes for a Plot3D with (optionally) a coordinate grid
*     superimposed.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstPlot *baseplot;
   AstPlot *plot;
   AstPlot3D *this;
   AstPointSet *majticks;
   AstPointSet *minticks;
   AstPointSet *tmp;
   AstPointSet *wcsmajticks;
   AstPointSet *wcsminticks;
   const char *edge;
   double **ptrmin;
   double **ptrmaj;
   double gcon;
   int base_wax2d;
   int itick;
   int new_gaxis;
   int new_plot;
   int new_wax2d;
   int nmaj;
   int nmin;
   int perm[ 2 ];
   int rootcorner;

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_plot;

/* Invoke the astGrid method on the 2D base Plot. Both WCS axes in this
   Plot are inherited from the 3D FrameSet that was supplied when the Plot3D
   was constructed, and will be labelled. The other two Plots encapsulated
   in the Plot3D only inherit a single axis from the original 3D FrameSet
   (a dummy axis is used for the second WCS axis in these Plots). */
   baseplot = GET_PLOT( this->baseplot );
   astGrid( baseplot );

/* We next use astGrid to draw a grid on the 2D plane that shares the first
   base plot graphics axis. Get the identifier for this Plot and the 2D
   graphics axis index within the Plot that corresponds to the first base
   plot graphics axis. Also get the constant value on the 3rd graphics
   axis over the base plot */
   rootcorner = astGetRootCorner( this );
   if( this->baseplot == XY ) {
      new_plot = XZ;
      new_gaxis = 0;
      gcon = this->gbox[ ( rootcorner & 4 ) ? 5 : 2 ];

   } else if( this->baseplot == XZ ) {
      new_plot = XY;
      new_gaxis = 0;
      gcon = this->gbox[ ( rootcorner & 2 ) ? 4 : 1 ];

   } else {
      new_plot = XY;
      new_gaxis = 1;
      gcon = this->gbox[ ( rootcorner & 1 ) ? 3 : 0 ];
   }

/* Get a pointer to the Plot upon which astGrid is about to be invoked. */
   plot = GET_PLOT( new_plot );

/* Find which 2D WCS axis was labelled on graphics axis 0 (the bottom or
   top edge) of the base plot. */
   if( ( edge = astGetC( baseplot, "Edge(1)" ) ) ) {
      base_wax2d = ( !strcmp( edge, "bottom" ) || !strcmp( edge, "top" ) ) ? 0 : 1;
   } else {
      base_wax2d = 0;
   }

/* Get the 2D graphics coords within the base Plot at which the tick
   marks were drawn for this 2D WCS axis. Extend the PointSets holding
   the major tick values to include an extra tick above and below the
   ticks drawn by astGrid. These extra ticks are placed outside the
   bounds of the plot. This ensures that the curves on the other axis
   extend the full width of the plot. */
   tmp = astGetDrawnTicks( baseplot, base_wax2d, 1 );
   majticks = ExtendTicks( baseplot, tmp, status );
   nmaj = astGetNpoint( majticks );
   ptrmaj = astGetPoints( majticks );

   if( tmp ) tmp = astAnnul( tmp );
   minticks = astGetDrawnTicks( baseplot, base_wax2d, 0 );
   nmin = astGetNpoint( minticks );
   ptrmin = astGetPoints( minticks );

/* All the tick marks will have a constant value for the 2D graphics axis
   that is not being ticked (axis 1 at the moment). Change this constant
   value to be the value appropriate to the new Plot. */
   if( ptrmaj && ptrmin ) {
      for( itick = 0; itick < nmaj; itick++ ) ptrmaj[ 1 ][ itick ] = gcon;
      for( itick = 0; itick < nmin; itick++ ) ptrmin[ 1 ][ itick ] = gcon;
   }

/* If required, permute the axes in the tick mark positions. */
   if( new_gaxis != 0 ) {
      perm[ 0 ] = 1;
      perm[ 1 ] = 0;
      astPermPoints( majticks, 1, perm );
      astPermPoints( minticks, 1, perm );
   }

/* Transform the tick mark positions from 2D graphics coords to 2D WCS
   coords. */
   wcsmajticks = astTransform( plot, majticks, 1, NULL );
   wcsminticks = astTransform( plot, minticks, 1, NULL );

/* Find the index of the 2D WCS axis that will be labelled on the bottom
   or top edge (i.e. 2D graphics axis zero) of the new Plot. */
   if( ( edge = astGetC( plot, "Edge(1)" ) ) ) {
      new_wax2d = ( !strcmp( edge, "bottom" ) || !strcmp( edge, "top" ) ) ? 0 : 1;
   } else {
      new_wax2d = 0;
   }

/* Use the other WCS axis if we are ticking the left or right edge (i.e.
   2D graphics axis one) of the new Plot. This gives us the index of the
   2D WCS axis for which tick values are to be stored in the Plot. */
   if( new_gaxis == 1 ) new_wax2d = 1 - new_wax2d;

/* Store the tick mark values to be used on this WCS axis. */
   ptrmaj = astGetPoints( wcsmajticks );
   ptrmin = astGetPoints( wcsminticks );
   if( ptrmaj && ptrmin ) {
      astSetTickValues( plot, new_wax2d, nmaj, ptrmaj[ new_gaxis ],
                                         nmin, ptrmin[ new_gaxis ] );
   }

/* Invoke the astGrid method on this plot. */
   astGrid( plot );

/* Clear the stored tick values in the plot. */
   astSetTickValues( plot, new_wax2d, 0, NULL, 0, NULL );

/* Free resources */
   if( wcsmajticks ) wcsmajticks = astAnnul( wcsmajticks );
   if( wcsminticks ) wcsminticks = astAnnul( wcsminticks );
   if( majticks ) majticks = astAnnul( majticks );
   if( minticks ) minticks = astAnnul( minticks );

/* We next use astGrid to draw a grid on the 2D plane that shares the
   second base plot graphics axis. Get the identifier for this Plot and the
   2D graphics axis index within the Plot that corresponds to the first
   base plot graphics axis. */
   if( this->baseplot == XY ) {
      new_plot = YZ;
      new_gaxis = 0;

   } else if( this->baseplot == XZ ) {
      new_plot = YZ;
      new_gaxis = 1;

   } else {
      new_plot = XZ;
      new_gaxis = 1;
   }

/* Get a pointer to the Plot upon which astGrid is about to be invoked. */
   plot = GET_PLOT( new_plot );

/* Find which 2D WCS axis was labelled on graphics axis 1 (the left or
   right edge) of the base plot. */
   base_wax2d = 1 - base_wax2d;

/* Get the 2D graphics coords within the base Plot at which the tick
   marks were drawn for this 2D WCS axis. Extend the PointSets holding
   the major tick values to include an extra tick above and below the
   ticks drawn by astGrid. These extra ticks are placed outside the
   bounds of the plot. This ensures that the curves on the other axis
   extend the full width of the plot. */
   tmp = astGetDrawnTicks( baseplot, base_wax2d, 1 );
   majticks = ExtendTicks( baseplot, tmp, status );
   nmaj = astGetNpoint( majticks );
   ptrmaj = astGetPoints( majticks );

   if( tmp ) tmp = astAnnul( tmp );
   minticks = astGetDrawnTicks( baseplot, base_wax2d, 0 );
   nmin = astGetNpoint( minticks );
   ptrmin = astGetPoints( minticks );

/* All the tick marks will have a constant value for the 2D graphics axis
   that is not being ticked (axis 0 at the moment). Change this constant
   value to be the value appropriate to the new Plot. */
   if( ptrmaj && ptrmin ) {
      for( itick = 0; itick < nmaj; itick++ ) ptrmaj[ 0 ][ itick ] = gcon;
      for( itick = 0; itick < nmin; itick++ ) ptrmin[ 0 ][ itick ] = gcon;
   }

/* If required, permute the axes in the tick mark positions. */
   if( new_gaxis != 1 ) {
      perm[ 0 ] = 1;
      perm[ 1 ] = 0;
      astPermPoints( majticks, 1, perm );
      astPermPoints( minticks, 1, perm );
   }

/* Transform the tick mark positions from 2D graphics coords to 2D WCS
   coords. */
   wcsmajticks = astTransform( plot, majticks, 1, NULL );
   wcsminticks = astTransform( plot, minticks, 1, NULL );

/* Find the index of the 2D WCS axis that will be labelled on the bottom
   or top edge (i.e. 2D graphics axis zero) of the new Plot. */
   if( ( edge = astGetC( plot, "Edge(1)" ) ) ) {
      new_wax2d = ( !strcmp( edge, "bottom" ) || !strcmp( edge, "top" ) ) ? 0 : 1;
   } else {
      new_wax2d = 0;
   }

/* Use the other WCS axis if we are ticking the left or right edge (i.e.
   2D graphics axis one) of the new Plot. This gives us the index of the
   2D WCS axis for which tick values are to be stored in the Plot. */
   if( new_gaxis == 1 ) new_wax2d = 1 - new_wax2d;

/* Store the tick mark values to be used on this WCS axis. */
   ptrmaj = astGetPoints( wcsmajticks );
   ptrmin = astGetPoints( wcsminticks );
   if( ptrmaj && ptrmin ) {
      astSetTickValues( plot, new_wax2d, nmaj, ptrmaj[ new_gaxis ],
                                         nmin, ptrmin[ new_gaxis ] );
   }

/* Invoke the astGrid method on this plot. */
   astGrid( plot );

/* Clear the stored tick values in the plot. */
   astSetTickValues( plot, new_wax2d, 0, NULL, 0, NULL );

/* Free resources */
   if( wcsmajticks ) wcsmajticks = astAnnul( wcsmajticks );
   if( wcsminticks ) wcsminticks = astAnnul( wcsminticks );
   if( majticks ) majticks = astAnnul( majticks );
   if( minticks ) minticks = astAnnul( minticks );

}

void astInitPlot3DVtab_(  AstPlot3DVtab *vtab, const char *name, int *status ) {
/*
*+
*  Name:
*     astInitPlot3DVtab

*  Purpose:
*     Initialise a virtual function table for a Plot3D.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "plot3d.h"
*     void astInitPlot3DVtab( AstPlot3DVtab *vtab, const char *name )

*  Class Membership:
*     Plot3D vtab initialiser.

*  Description:
*     This function initialises the component of a virtual function
*     table which is used by the Plot3D class.

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
   astDECLARE_GLOBALS          /* Pointer to thread-specific global data */
   AstFrame *dummy_frame;      /* The Frame to put in dummy_frameset */
   AstPlotVtab *plot;          /* Pointer to Plot component of Vtab */
   AstFrameSetVtab *fset;      /* Pointer to FrameSet component of Vtab */
   AstMappingVtab *mapping;    /* Pointer to Mapping component of Vtab */
   AstObjectVtab *object;      /* Pointer to Object component of Vtab */

/* Check the local error status. */
   if ( !astOK ) return;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Initialize the component of the virtual function table used by the
   parent class. */
   astInitPlotVtab( (AstPlotVtab *) vtab, name );

/* Store a unique "magic" value in the virtual function table. This
   will be used (by astIsAPlot3D) to determine if an object belongs
   to this class.  We can conveniently use the address of the (static)
   class_check variable to generate this unique value. */
   vtab->id.check = &class_check;
   vtab->id.parent = &(((AstPlotVtab *) vtab)->id);

/* Initialise member function pointers. */
/* ------------------------------------ */
/* Store pointers to the member functions (implemented here) that
   provide virtual methods for this class. */

#define SET_PLOT3D_ACCESSORS(attr) \
   vtab->Set##attr = Set##attr; \
   vtab->Get##attr = Get##attr; \
   vtab->Test##attr = Test##attr; \
   vtab->Clear##attr = Clear##attr;

SET_PLOT3D_ACCESSORS(RootCorner)
SET_PLOT3D_ACCESSORS(Norm)

#undef SET_PLOT3D_ACCESSORS




/* Save the inherited pointers to methods that will be extended, and
   replace them with pointers to the new member functions. */
   object = (AstObjectVtab *) vtab;
   fset = (AstFrameSetVtab *) vtab;
   mapping = (AstMappingVtab *) vtab;
   plot = (AstPlotVtab *) vtab;

   parent_getobjsize = object->GetObjSize;
   object->GetObjSize = GetObjSize;

   parent_equal = object->Equal;
   object->Equal = Equal;

   parent_vset = object->VSet;
   object->VSet = VSet;

   parent_cast = object->Cast;
   object->Cast = Cast;

   parent_clear = object->Clear;
   object->Clear = Clear;

   parent_clearattrib = object->ClearAttrib;
   object->ClearAttrib = ClearAttrib;

   parent_getattrib = object->GetAttrib;
   object->GetAttrib = GetAttrib;

   parent_setattrib = object->SetAttrib;
   object->SetAttrib = SetAttrib;

   parent_testattrib = object->TestAttrib;
   object->TestAttrib = TestAttrib;

   parent_clearcurrent = fset->ClearCurrent;
   fset->ClearCurrent = ClearCurrent;

   parent_setcurrent = fset->SetCurrent;
   fset->SetCurrent = SetCurrent;

   parent_removeframe = fset->RemoveFrame;
   fset->RemoveFrame = RemoveFrame;

#if defined(THREAD_SAFE)
   parent_managelock = object->ManageLock;
   object->ManageLock = ManageLock;
#endif

/* Define a macro to override attribute accessors inherited from the
   parent Plot class. First do axis specific attributes. */

#define SET_PLOT_ACCESSORS(attr) \
   parent_get##attr = plot->Get##attr; \
   plot->Get##attr = Get##attr; \
   parent_set##attr = plot->Set##attr; \
   plot->Set##attr = Set##attr; \
   parent_clear##attr = plot->Clear##attr; \
   plot->Clear##attr = Clear##attr;

/* Use the above macro to override all the inherited attribute accessors. */
SET_PLOT_ACCESSORS(MinTick)
SET_PLOT_ACCESSORS(Abbrev)
SET_PLOT_ACCESSORS(Gap)
SET_PLOT_ACCESSORS(LogGap)
SET_PLOT_ACCESSORS(LogPlot)
SET_PLOT_ACCESSORS(LogTicks)
SET_PLOT_ACCESSORS(LogLabel)
SET_PLOT_ACCESSORS(LabelUp)
SET_PLOT_ACCESSORS(DrawAxes)
SET_PLOT_ACCESSORS(LabelUnits)
SET_PLOT_ACCESSORS(MinTickLen)
SET_PLOT_ACCESSORS(MajTickLen)
SET_PLOT_ACCESSORS(NumLab)
SET_PLOT_ACCESSORS(NumLabGap)
SET_PLOT_ACCESSORS(TextLab)
SET_PLOT_ACCESSORS(TextLabGap)

#undef SET_PLOT_ACCESSORS


/* Now do attributes that are not axis specific. */

#define SET_PLOT_ACCESSORS(attr) \
   parent_set##attr = plot->Set##attr; \
   plot->Set##attr = Set##attr; \
   parent_clear##attr = plot->Clear##attr; \
   plot->Clear##attr = Clear##attr;

SET_PLOT_ACCESSORS(Ink)
SET_PLOT_ACCESSORS(Tol)
SET_PLOT_ACCESSORS(Invisible)
SET_PLOT_ACCESSORS(TickAll)
SET_PLOT_ACCESSORS(ForceExterior)
SET_PLOT_ACCESSORS(Border)
SET_PLOT_ACCESSORS(Clip)
SET_PLOT_ACCESSORS(ClipOp)
SET_PLOT_ACCESSORS(Escape)
SET_PLOT_ACCESSORS(Grid)
SET_PLOT_ACCESSORS(Labelling)
SET_PLOT_ACCESSORS(Style)
SET_PLOT_ACCESSORS(Font)
SET_PLOT_ACCESSORS(Colour)
SET_PLOT_ACCESSORS(Width)
SET_PLOT_ACCESSORS(Size)

#undef SET_PLOT_ACCESSORS


/* Store replacement pointers for methods which will be over-ridden by new
   member functions implemented here. */
   plot->Grid = Grid;
   plot->Text = Text;
   plot->SetTickValues = SetTickValues;
   plot->PolyCurve = PolyCurve;
   plot->Border = Border;
   plot->BoundingBox = BoundingBox;
   plot->GetGrfContext = GetGrfContext;
   plot->GrfPop = GrfPop;
   plot->GrfPush = GrfPush;
   plot->GrfSet = GrfSet;
   plot->GridLine = GridLine;
   plot->Mark = Mark;
   plot->Curve = Curve;
   plot->GenCurve = GenCurve;
   plot->Clip = Clip;
   mapping->Transform = Transform;

/* Declare the copy constructor, destructor and class dump
   function. */
   astSetCopy( vtab, Copy );
   astSetDelete( vtab, Delete );
   astSetDump( vtab, Dump, "Plot3D", "Provide facilities for 3D graphical output" );

/* Create a FrameSet that can be used when calling astCast to indicate
   the class to which we want to cast. */
   LOCK_MUTEX3
   if( !dummy_frameset ) {
      dummy_frame = astFrame( 1, " ", status );
      dummy_frameset = astFrameSet( dummy_frame, " ", status );
      dummy_frame = astAnnul( dummy_frame );
   }
   UNLOCK_MUTEX3

/* If we have just initialised the vtab for the current class, indicate
   that the vtab is now initialised, and store a pointer to the class
   identifier in the base "object" level of the vtab. */
   if( vtab == &class_vtab ) {
      class_init = 1;
      astSetVtabClassIdentifier( vtab, &(vtab->id) );
   }
}

#if defined(THREAD_SAFE)
static int ManageLock( AstObject *this_object, int mode, int extra,
                       AstObject **fail, int *status ) {
/*
*  Name:
*     ManageLock

*  Purpose:
*     Manage the thread lock on an Object.

*  Type:
*     Private function.

*  Synopsis:
*     #include "object.h"
*     AstObject *ManageLock( AstObject *this, int mode, int extra,
*                            AstObject **fail, int *status )

*  Class Membership:
*     CmpMap member function (over-rides the astManageLock protected
*     method inherited from the parent class).

*  Description:
*     This function manages the thread lock on the supplied Object. The
*     lock can be locked, unlocked or checked by this function as
*     deteremined by parameter "mode". See astLock for details of the way
*     these locks are used.

*  Parameters:
*     this
*        Pointer to the Object.
*     mode
*        An integer flag indicating what the function should do:
*
*        AST__LOCK: Lock the Object for exclusive use by the calling
*        thread. The "extra" value indicates what should be done if the
*        Object is already locked (wait or report an error - see astLock).
*
*        AST__UNLOCK: Unlock the Object for use by other threads.
*
*        AST__CHECKLOCK: Check that the object is locked for use by the
*        calling thread (report an error if not).
*     extra
*        Extra mode-specific information.
*     fail
*        If a non-zero function value is returned, a pointer to the
*        Object that caused the failure is returned at "*fail". This may
*        be "this" or it may be an Object contained within "this". Note,
*        the Object's reference count is not incremented, and so the
*        returned pointer should not be annulled. A NULL pointer is
*        returned if this function returns a value of zero.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*    A local status value:
*        0 - Success
*        1 - Could not lock or unlock the object because it was already
*            locked by another thread.
*        2 - Failed to lock a POSIX mutex
*        3 - Failed to unlock a POSIX mutex
*        4 - Bad "mode" value supplied.

*  Notes:
*     - This function attempts to execute even if an error has already
*     occurred.
*/

/* Local Variables: */
   AstPlot3D *this;       /* Pointer to Plot3D structure */
   int result;            /* Returned status value */

/* Initialise */
   result = 0;

/* Check the supplied pointer is not NULL. */
   if( !this_object ) return result;

/* Obtain a pointers to the Plot3D structure. */
   this = (AstPlot3D *) this_object;

/* Invoke the ManageLock method inherited from the parent class. */
   if( !result ) result = (*parent_managelock)( this_object, mode, extra,
                                                fail, status );

/* Invoke the astManageLock method on any Objects contained within
   the supplied Object. */
   if( !result ) result = astManageLock( this->plotxy, mode, extra, fail );
   if( !result ) result = astManageLock( this->plotxz, mode, extra, fail );
   if( !result ) result = astManageLock( this->plotyz, mode, extra, fail );

   return result;

}
#endif

static void Mark( AstPlot *this_plot, int nmark, int ncoord, int indim,
                  const double *in, int type, int *status ){
/*
*  Name:
*     Mark

*  Purpose:
*     Draw a set of markers for a Plot3D.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "plot3d.h"
*     void Mark( AstPlot *this, int nmark, int ncoord, int indim,
*                const double *in, int type, int *status )

*  Class Membership:
*     Plot3d member function (overrides the astMark method inherited form
*     the parent Plot class).

*  Description:
*     This function draws a set of markers (symbols) at positions
*     specified in the physical coordinate system of a Plot3D. The
*     positions are transformed into graphical coordinates to
*     determine where the markers should appear within the plotting
*     area.
*
*     They are drawn on a 2D plane that has a normal vector given by the
*     current value of the Plot3D's "Norm" attribute.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     nmark
*        The number of markers to draw. This may be zero, in which
*        case nothing will be drawn.
*     ncoord
*        The number of coordinates being supplied for each mark
*        (i.e. the number of axes in the current Frame of the Plot, as
*        given by its Naxes attribute).
*     indim
*        The number of elements along the second dimension of the "in"
*        array (which contains the marker coordinates). This value is
*        required so that the coordinate values can be correctly
*        located if they do not entirely fill this array. The value
*        given should not be less than "nmark".
*     in
*        The address of the first element of a 2-dimensional array of
*        shape "[ncoord][indim]" giving the
*        physical coordinates of the points where markers are to be
*        drawn. These should be stored such that the value of
*        coordinate number "coord" for input mark number "mark" is
*        found in element "in[coord][mark]".
*     type
*        A value specifying the type (e.g. shape) of marker to be
*        drawn. The set of values which may be used (and the shapes
*        that will result) is determined by the underlying graphics
*        system.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Markers are not drawn at positions which have any coordinate
*     equal to the value AST__BAD (or where the transformation into
*     graphical coordinates yields coordinates containing the value
*     AST__BAD).
*     - An error results if the base Frame of the Plot is not 3-dimensional.
*     - An error also results if the transformation between the
*     current and base Frames of the Plot is not defined (i.e. the
*     Plot's TranInverse attribute is zero).
*/

/* Local Variables: */
   AstMapping *mapping;    /* Pointer to graphics->physical mapping */
   AstPlot3D *this;        /* Pointer to the Plot3D structure */
   AstPointSet *pset1;     /* PointSet holding physical positions */
   AstPointSet *pset2;     /* PointSet holding graphics positions */
   const char *class;      /* Object class */
   const char *method;     /* Current method */
   const double **ptr1;    /* Pointer to physical positions */
   double **ptr2;          /* Pointer to graphics positions */
   double *xpd;            /* Pointer to next double precision x value */
   double *ypd;            /* Pointer to next double precision y value */
   double *zpd;            /* Pointer to next double precision z value */
   float *x;               /* Pointer to single precision x values */
   float *xpf;             /* Pointer to next single precision x value */
   float *y;               /* Pointer to single precision y values */
   float *ypf;             /* Pointer to next single precision y value */
   float *z;               /* Pointer to single precision z values */
   float *zpf;             /* Pointer to next single precision z value */
   float norm[ 3 ];        /* Single precision normal vector */
   int axis;               /* Axis index */
   int i;                  /* Loop count */
   int naxes;              /* No. of axes in the base Frame */
   int nn;                 /* Number of good marker positions */

/* Check the global error status. */
   if ( !astOK ) return;

/* Get a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_plot;

/* Store the current method and class for inclusion in error messages
   generated by lower level functions. */
   method = "astMark";
   class = astClass( this );

/* Check the base Frame of the Plot is 3-D. */
   naxes = astGetNin( this );
   if( naxes != 3 && astOK ){
      astError( AST__NAXIN, "%s(%s): Number of axes (%d) in the base "
                "Frame of the supplied %s is invalid - this number should "
                "be 3.", status, method, class, naxes, class );
   }

/* Also validate the input array dimension argument. */
   if ( astOK && ( indim < nmark ) ) {
      astError( AST__DIMIN, "%s(%s): The input array dimension value "
                "(%d) is invalid.", status, method, class, indim );
      astError( AST__DIMIN, "This should not be less than the number of "
                "markers being drawn (%d).", status, nmark );
   }

/* Establish the correct graphical attributes as defined by attributes
   with the supplied Plot. */
   astGrfAttrs( this, AST__MARKS_ID, 1, GRF__MARK, method, class );

/* Create a PointSet to hold the supplied physical coordinates. */
   pset1 = astPointSet( nmark, ncoord, "", status );

/* Allocate memory to hold pointers to the first value on each axis. */
   ptr1 = (const double **) astMalloc( sizeof( const double * )*
                                       (size_t)( ncoord ));

/* Check the pointer can be used, then store pointers to the first value
   on each axis. */
   if( astOK ){
      for( axis = 0; axis < ncoord; axis++ ){
         ptr1[ axis ] = in + axis*indim;
      }
   }

/* Store these pointers in the PointSet. */
   astSetPoints( pset1, (double **) ptr1 );

/* Transform the supplied data from the current frame (i.e. physical
   coordinates) to the base frame (i.e. graphics coordinates) using
   the inverse Mapping defined by the Plot. */
   mapping = astGetMapping( this, AST__BASE, AST__CURRENT );
   pset2 = astTransform( mapping, pset1, 0, NULL );
   mapping = astAnnul( mapping );

/* Get pointers to the graphics coordinates. */
   ptr2 = astGetPoints( pset2 );

/* Allocate memory to hold single precision versions of the graphics
   coordinates. */
   x = (float *) astMalloc( sizeof( float )*(size_t) nmark );
   y = (float *) astMalloc( sizeof( float )*(size_t) nmark );
   z = (float *) astMalloc( sizeof( float )*(size_t) nmark );

/* Check the pointers can be used. */
   if( astOK ){

/* Store pointers to the next single and double precision x, y and z
   values. */
      xpf = x;
      ypf = y;
      zpf = z;
      xpd = ptr2[ 0 ];
      ypd = ptr2[ 1 ];
      zpd = ptr2[ 2 ];

/* Convert the double precision values to single precision, rejecting
   any bad marker positions. */
      nn = 0;
      for( i = 0; i < nmark; i++ ){
         if( *xpd != AST__BAD && *ypd != AST__BAD && *zpd != AST__BAD ){
            nn++;
            *(xpf++) = (float) *(xpd++);
            *(ypf++) = (float) *(ypd++);
            *(zpf++) = (float) *(zpd++);
         } else {
            xpd++;
            ypd++;
            zpd++;
         }
      }

/* If the nornmal vector has non-zero length, draw the remaining markers. */
      norm[ 0 ] = (float) astGetNorm( this, 0 );
      norm[ 1 ] = (float) astGetNorm( this, 1 );
      norm[ 2 ] = (float) astGetNorm( this, 2 );

      if( norm[ 0 ] != 0.0 || norm[ 1 ] != 0.0 || norm[ 2 ] != 0.0 ){

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
         LOCK_MUTEX2;

         if( !astG3DMark( nn, x, y, z, type, norm ) ) {
            astError( AST__GRFER, "%s(%s): Graphics error in astG3DMark. ", status,
                   method, class );
         }

/* Allow the next thread to proceed. */
         UNLOCK_MUTEX2;

      } else if( astOK ) {
         astError( AST__ATTIN, "%s(%s): The vector specified by the Norm "
                   "attribute has zero length.", status, method, class );
      }
   }

/* Free the memory used to store single precision graphics coordinates. */
   x = (float *) astFree( (void *) x );
   y = (float *) astFree( (void *) y );
   z = (float *) astFree( (void *) z );

/* Annul the PointSets. */
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* Free the memory holding the pointers to the first value on each axis. */
   ptr1 = (const double **) astFree( (void *) ptr1 );

/* Re-establish the original graphical attributes. */
   astGrfAttrs( this, AST__MARKS_ID, 0, GRF__MARK, method, class );

/* Return */
   return;
}

static int Plot3DAttr( AstKeyMap *grfconID, int attr, double value,
                       double *old_value, int prim ){
/*
*  Name:
*     Plot3DAttr

*  Purpose:
*     Get or set the value of a 2D grf attribute.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Plot3DAttr( AstKeyMap *grfconID, int attr, double value,
*                     double *old_value, int prim )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     get or set the current value of a specified graphics attribute. It
*     forwards the call to the grf3D module being used by this Plot3D. It
*     should be registered with each of the 2D Plots using astGrfSet.

*  Parameters:
*     grfconID
*       The Plot's GrfContext KeyMap. This is used to identify which of
*       the three Plots is calling this function.
*     attr
*       An integer value identifying the required attribute. This should
*       be one of the symbolic values defined in grf.h.
*     value
*       A new value to store for the attribute. If this is AST__BAD
*       no value is stored.
*     old_value
*       A pointer to a double in which to return the attribute value.
*       If this is NULL, no value is returned.
*     prim
*       The sort of graphics primitive to be drawn with the new attribute.
*       Identified by one of the values defined in grf.h.

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

/* Local Variables: */
   int result;
   int *status;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the inherited status. */
   if( !astOK ) return 0;

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
   LOCK_MUTEX2;

/* Use the function in the external Grf3D module, selected at link-time
   using ast_link options. Note, attribute values are the same for each
   of the three Plot. */
   result = astG3DAttr( attr, value, old_value, prim );

/* Allow the next thread to proceed. */
   UNLOCK_MUTEX2;

/* Return the result. */
   return result;
}

static int Plot3DCap( AstKeyMap *grfconID, int cap, int value ){
/*
*  Name:
*     Plot3DCap

*  Purpose:
*     Determine if the 2D grf module has a given capability.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Plot3DCap( AstKeyMap *grfconID, int cap, int value )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     determine if a given graphics capability is available. It forwards
*     the call to the grf3D module being used by this Plot3D. It should be
*     registered with each of the 2D Plots using astGrfSet.

*  Parameters:
*     grfconID
*       The Plot's GrfContext KeyMap. This is
*       used to identify which of the three Plots is calling this function.
*     cap
*       The capability being inquired about. This will be one of the
*       following constants defined in grf.h: GRF__SCALES, GRF__MJUST,
*       GRF__ESC,
*     value
*       The use of this parameter depends on the value of "cap" as
*       described in the documentation for the astGrfSet function in the
*       Plot class.

*  Returned Value:
*     The value returned by the function depends on the value of "cap"
*     as described in the astGrfSet documentation. Zero is returned if
*     the supplied capability is not recognised.

*/

/* Local Variables: */
   int result;
   int *status;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the inherited status. */
   if( !astOK ) return 0;

/* The astGScales function is implemented by code within the Plot3D class
   (not within the grf3D module). The Plot3D class assumes all axes are
   equally scaled. */
   if( cap == GRF__SCALES ) {
      result = 1;

/* All grf3D modules are assumed to support "M" justification. */
   } else if( cap == GRF__MJUST ) {
      result = 1;

/* Forward all other capability requests to the grf3D module. */
   } else {

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
      LOCK_MUTEX2;

      result = astG3DCap( cap, value );

/* Allow the next thread to proceed. */
      UNLOCK_MUTEX2;
   }

/* Return the result. */
   return result;
}

static int Plot3DFlush( AstKeyMap *grfconID ){
/*
*  Name:
*     Plot3DFlush

*  Purpose:
*     Flush any buffered graphical output.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Plot3DFlush( AstKeyMap *grfconID )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     ensure that the display device is up-to-date by flushing any pending
*     graphics to the output device. It forwards the call to the grf3D module
*     being used by this Plot3D. It should be registered with each of the
*     2D Plots using astGrfSet.

*  Parameters:
*     grfconID
*       The Plot's GrfContext KeyMap. This is
*       used to identify which of the three Plots is calling this function.

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

/* Local Variables: */
   int result;
   int *status;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the inherited status. */
   if( !astOK ) return 0;

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
   LOCK_MUTEX2;

/* Use the function in the external Grf3D module, selected at link-time
   using ast_link options. */
   result = astG3DFlush();

/* Allow the next thread to proceed. */
   UNLOCK_MUTEX2;

/* Return the result. */
   return result;
}

static int Plot3DLine( AstKeyMap *grfconID, int n, const float *x, const float *y ){
/*
*  Name:
*     Plot3DLine

*  Purpose:
*     Draw a polyline on a 2D surface.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Plot3DLine( AstKeyMap *grfconID, int n, const float *x,
*                     const float *y )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     draw a polyline. It forwards the call to the grf3D module being used
*     by this Plot3D. It should be registered with each of the 2D Plots
*     using astGrfSet.

*  Parameters:
*     grfconID
*       The Plot's GrfContext KeyMap. This is
*       used to identify which of the three Plots is calling this function.
*     n
*       The number of positions to be joined together.
*     x
*       A pointer to an array holding the "n" x values.
*     y
*       A pointer to an array holding the "n" y values.

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

/* Local Variables: */
   AstKeyMap *grfcon;
   double gcon;
   float *work;
   float *x3d = NULL;
   float *y3d = NULL;
   float *z3d = NULL;
   int i;
   int plane;
   int result = 0;
   int *status;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a genuine pointer from the supplied grfcon identifier. */
   grfcon = astMakePointer( grfconID );

/* Report an error if no grfcon object was supplied. */
   if( !grfcon ) {
      astError( AST__INTER, "astG3DLine(Plot3D): No grfcon Object supplied "
                "(internal AST programming error)." , status);

/* If a grfcon Object was supplied, get the graphics box array out of it. */
   } else if( !astMapGet0D( grfcon, "Gcon", &gcon ) ) {
      astError( AST__INTER, "astG3DLine(Plot3D): No \"Gcon\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* Also get the plane index out of it. */
   } else if( !astMapGet0I( grfcon, "Plane", &plane ) ) {
      astError( AST__INTER, "astG3DLine(Plot3D): No \"Plane\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);
   }

/* Allocate memory to hold the "n" values for the missing coordinate. */
   work = astMalloc( sizeof( float )*(size_t) n );
   if( work ) {

/* Set up pointers to the x, y and z arrays in the 3D graphics system.
   Fill the missing array with suitable values (the constant value of
   the third axis on the plane being drawn). */
      if( plane == XY ) {
         x3d = (float *) x;
         y3d = (float *) y;
         z3d = work;

         for( i = 0; i < n; i++ ) z3d[ i ] = gcon;

      } else if( plane == XZ ) {
         x3d = (float *) x;
         y3d = work;
         z3d = (float *) y;
         for( i = 0; i < n; i++ ) y3d[ i ] = gcon;

      } else if( plane == YZ ) {
         x3d = work;
         y3d = (float *) x;
         z3d = (float *) y;
         for( i = 0; i < n; i++ ) x3d[ i ] = gcon;

      } else {
         astError( AST__INTER, "astG3DLine(Plot3D): Illegal plane "
                   "identifier %d supplied (internal AST programming "
                   "error).", status, plane );
      }

/* If ok, draw the lines in the 3D graphics coordinate space. */
      if( x3d ) {

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
         LOCK_MUTEX2;

/* Draw the line */
         result = astG3DLine( n, x3d, y3d, z3d );

/* Allow the next thread to proceed. */
         UNLOCK_MUTEX2;
      }
   }

/* Free resources. */
   work = astFree( work );

/* Return the result. */
   return result;
}

static int Plot3DMark( AstKeyMap *grfconID, int n, const float *x,
                       const float *y, int type ){
/*
*  Name:
*     Plot3DMark

*  Purpose:
*     Draw a set of markers.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Plot3DMark( AstKeyMap *grfconID, int n, const float *x,
*                     const float *y, int type )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     draw a set of markers. It forwards the call to the grf3D module being
*     used by this Plot3D. It should be registered with each of the 2D Plots
*     using astGrfSet.

*  Parameters:
*     grfconID
*       The Plot's GrfContext KeyMap. This is
*       used to identify which of the three Plots is calling this function.
*     n
*       The number of markers to draw.
*     x
*       A pointer to an array holding the "n" x values.
*     y
*       A pointer to an array holding the "n" y values.
*     type
*       An integer which can be used to indicate the type of marker symbol
*       required.

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

/* Local Variables: */
   AstKeyMap *grfcon;
   double gcon;
   float *work;
   float *x3d = NULL;
   float *y3d = NULL;
   float *z3d = NULL;
   float norm[ 3 ];
   int i;
   int plane;
   int rc;
   int result = 0;
   int *status;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a genuine pointer from the supplied grfcon identifier. */
   grfcon = astMakePointer( grfconID );

/* Report an error if no grfcon object was supplied. */
   if( !grfcon ) {
      astError( AST__INTER, "astG3DMark(Plot3D): No grfcon Object supplied "
                "(internal AST programming error)." , status);

/* If a grfcon Object was supplied, get the graphics box array out of it. */
   } else if( !astMapGet0D( grfcon, "Gcon", &gcon ) ) {
      astError( AST__INTER, "astG3DMark(Plot3D): No \"Gcon\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* If a grfcon Object was supplied, get the RootCorner value out of it. */
   } else if( !astMapGet0I( grfcon, "RootCorner", &rc ) ) {
      astError( AST__INTER, "astG3DLine(Plot3D): No \"RootCornern\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* Also get the plane index out of it. */
   } else if( !astMapGet0I( grfcon, "Plane", &plane ) ) {
      astError( AST__INTER, "astG3DMark(Plot3D): No \"Plane\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);
   }

/* Allocate memory to hold the "n" values for the missing coordinate. */
   work = astMalloc( sizeof( float )*(size_t) n );
   if( work ) {

/* Set up pointers to the x, y and z arrays in the 3D graphics system.
   Fill the missing array with suitable values (the constant value of
   the third axis on the plane being drawn). Set the normal vector for
   the markers so that they are drawn in the plane described by the Plot.*/
      if( plane == XY ) {
         x3d = (float *) x;
         y3d = (float *) y;
         z3d = work;
         for( i = 0; i < n; i++ ) z3d[ i ] = gcon;
         norm[ 0 ] = 0;
         norm[ 1 ] = 0;
         norm[ 2 ] = ( rc & 4 ) ? 1 : -1;

      } else if( plane == XZ ) {
         x3d = (float *) x;
         y3d = work;
         z3d = (float *) y;
         for( i = 0; i < n; i++ ) y3d[ i ] = gcon;
         norm[ 0 ] = 0;
         norm[ 1 ] = ( rc & 2 ) ? 1 : -1;
         norm[ 2 ] = 0;

      } else if( plane == YZ ) {
         x3d = work;
         y3d = (float *) x;
         z3d = (float *) y;
         for( i = 0; i < n; i++ ) x3d[ i ] = gcon;
         norm[ 0 ] = ( rc & 1 ) ? 1 : -1;
         norm[ 1 ] = 0;
         norm[ 2 ] = 0;

      } else {
         astError( AST__INTER, "astG3DMark(Plot3D): Illegal plane "
                   "identifier %d supplied (internal AST programming "
                   "error).", status, plane );
      }

/* If ok, draw the markers in the 3D graphics coordinate space. */
      if( x3d ) {

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
         LOCK_MUTEX2;

/* Draw the markers */
         result = astG3DMark( n, x3d, y3d, z3d, type, norm );

/* Allow the next thread to proceed. */
         UNLOCK_MUTEX2;
      }
   }

/* Free resources. */
   work = astFree( work );

/* Return the result. */
   return result;
}

static int Plot3DQch( AstKeyMap *grfconID, float *chv, float *chh ){
/*
*  Name:
*     Plot3DQch

*  Purpose:
*     Get the current text size.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Plot3DQch( AstKeyMap *grfconID, float *chv, float *chh )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     get the current text size. It forwards the call to the grf3D module
*     being used by this Plot3D. It should be registered with each of the
*     2D Plots using astGrfSet.
*
*     The grf3D module assumes that the 3D graphics axes are equally
*     scaled and therefore the text height does not depend on the text
*     orientation. Therefore, "chv" and "chh" are returned holding the
*     same value.

*  Parameters:
*     grfconID
*       The Plot's GrfContext KeyMap. This is
*       used to identify which of the three Plots is calling this function.
*     chv
*        A pointer to the float which is to receive the height of
*        characters drawn with a vertical baseline in the 2D Plot. This
*        will be an increment in the 2D X axis.
*     chh
*       A pointer to the float which is to receive the height of
*       characters drawn with a horizontal baseline in the 2D Plot. This
*       will be an increment in the 2D Y axis.

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

/* Local Variables: */
   int result;
   float ch;
   int *status;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the inherited status. */
   if( !astOK ) return 0;

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
   LOCK_MUTEX2;

/* Use the function in the external Grf3D module, selected at link-time
   using ast_link options. Note, text height is the same for each
   of the three Plot. */
   result = astG3DQch( &ch );

/* Allow the next thread to proceed. */
   UNLOCK_MUTEX2;

/* Store the value in both the returned values. */
   *chv = ch;
   *chh = ch;

/* Return the error flag. */
   return result;
}

static int Plot3DScales( AstKeyMap *grfconID, float *alpha, float *beta ){
/*
*  Name:
*     Plot3DScales

*  Purpose:
*     Get the 2D axis scales.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*      int Plot3DScales( AstKeyMap *grfconID, float *alpha, float *beta )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     get the relative scales of the 2D axes. Since the grf3D module
*     assumes that all graphics axes are equally scaled, it just returns 1.0
*     for alpha and beta.

*  Parameters:
*     grfconID
*       The Plot's GrfContext KeyMap. This is
*       used to identify which of the three Plots is calling this function.
*     alpha
*       A pointer to the float which is to receive the scale for the X
*       axis
*     beta
*       A pointer to the float which is to receive the scale for the Y
*       axis

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

   *alpha = 1.0;
   *beta = 1.0;
   return 1;
}

static int Plot3DText( AstKeyMap *grfconID, const char *text, float x, float y,
                       const char *just, float upx, float upy ){
/*
*  Name:
*     Plot3DText

*  Purpose:
*     Draw a text string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Plot3DText( AstKeyMap *grfconID, const char *text, float x, float y,
*                     const char *just, float upx, float upy )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     draw a text string. It forwards the call to the grf3D module being
*     used by this Plot3D. It should be registered with each of the 2D Plots
*     using astGrfSet.

*  Parameters:
*     grfconID
*        The Plot's GrfContext KeyMap. This is
*        used to identify which of the three Plots is calling this function.
*     text
*        Pointer to a null-terminated character string to be displayed.
*     x
*        The reference x coordinate.
*     y
*        The reference y coordinate.
*     just
*        A character string which specifies the location within the
*        text string which is to be placed at the reference position
*        given by x and y.
*     upx
*        The x component of the up-vector for the text.
*     upy
*        The y component of the up-vector for the text.

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

/* Local Variables: */
   AstKeyMap *grfcon;
   double gcon;
   float norm[ 3 ];
   float ref[ 3 ];
   float up[ 3 ];
   int plane;
   int rc;
   int result = 0;
   int *status;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a genuine pointer from the supplied grfcon identifier. */
   grfcon = astMakePointer( grfconID );

/* Report an error if no grfcon object was supplied. */
   if( !grfcon ) {
      astError( AST__INTER, "astG3DText(Plot3D): No grfcon Object supplied "
                "(internal AST programming error)." , status);

/* If a grfcon Object was supplied, get the graphics box array out of it. */
   } else if( !astMapGet0D( grfcon, "Gcon", &gcon ) ) {
      astError( AST__INTER, "astG3DText(Plot3D): No \"Gcon\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* If a grfcon Object was supplied, get the RootCorner value out of it. */
   } else if( !astMapGet0I( grfcon, "RootCorner", &rc ) ) {
      astError( AST__INTER, "astG3DLine(Plot3D): No \"RootCornern\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* Also get the plane index out of it. */
   } else if( !astMapGet0I( grfcon, "Plane", &plane ) ) {
      astError( AST__INTER, "astG3DText(Plot3D): No \"Plane\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* If OK, draw the text. */
   } else {

/* Set up the reference, up and normal vectors so that the text appears
   on the required plane. */
      if( plane == XY ) {
         ref[ 0 ] = x;
         ref[ 1 ] = y;
         ref[ 2 ] = gcon;
         norm[ 0 ] = 0;
         norm[ 1 ] = 0;
         norm[ 2 ] = ( rc & 4 ) ? 1 : -1;
         up[ 0 ] = upx;
         up[ 1 ] = upy;
         up[ 2 ] = 0;

      } else if( plane == XZ ) {
         ref[ 0 ] = x;
         ref[ 1 ] = gcon;
         ref[ 2 ] = y;
         norm[ 0 ] = 0;
         norm[ 1 ] = ( rc & 2 ) ? 1 : -1;
         norm[ 2 ] = 0;
         up[ 0 ] = upx;
         up[ 1 ] = 0;
         up[ 2 ] = upy;

      } else if( plane == YZ ) {
         ref[ 0 ] = gcon;
         ref[ 1 ] = x;
         ref[ 2 ] = y;
         norm[ 0 ] = ( rc & 1 ) ? 1 : -1;
         norm[ 1 ] = 0;
         norm[ 2 ] = 0;
         up[ 0 ] = 0;
         up[ 1 ] = upx;
         up[ 2 ] = upy;

      } else {
         astError( AST__INTER, "astG3DText(Plot3D): Illegal plane "
                   "identifier %d supplied (internal AST programming "
                   "error).", status, plane );
      }

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
      LOCK_MUTEX2;

/* If ok, draw the markers in the 3D graphics coordinate space. */
      if( astOK ) result = astG3DText( text, ref, just, up, norm );

/* Allow the next thread to proceed. */
      UNLOCK_MUTEX2;
   }

/* Return the result. */
   return result;
}

static int Plot3DTxExt( AstKeyMap *grfconID, const char *text, float x, float y,
                        const char *just, float upx, float upy, float *xb,
                        float *yb ){
/*
*  Name:
*     Plot3DTxExt

*  Purpose:
*     Determine the plotted extent of a text string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int Plot3DTxExt( AstKeyMap *grfconID, const char *text, float x,
*                      float y, const char *just, float upx, float upy,
*                      float *xb, float *yb )

*  Class Membership:
*     Plot3D member function.

*  Description:
*     This function is called by one of the three encapsulated 2D Plots to
*     determine the extent a string would have if plotted using Plot3DText.
*     It forwards the call to the grf3D module being used by this Plot3D. It
*     should be registered with each of the 2D Plots using astGrfSet.

*  Parameters:
*     grfconID
*        The Plot's GrfContext KeyMap. This is
*        used to identify which of the three Plots is calling this function.
*     text
*        Pointer to a null-terminated character string to be displayed.
*     x
*        The reference x coordinate.
*     y
*        The reference y coordinate.
*     just
*        A character string which specifies the location within the
*        text string which is to be placed at the reference position
*        given by x and y.
*     upx
*        The x component of the up-vector for the text.
*     upy
*        The y component of the up-vector for the text.
*     xb
*        An array of 4 elements in which to return the x coordinate of
*        each corner of the bounding box.
*     yb
*        An array of 4 elements in which to return the y coordinate of
*        each corner of the bounding box.

*  Returned Value:
*     An integer value of 0 is returned if an error occurs, and 1 otherwise.

*/

/* Local Variables: */
   AstKeyMap *grfcon;
   double gcon;
   float *xb3d = NULL;
   float *yb3d = NULL;
   float *zb3d = NULL;
   float bl[ 3 ];
   float norm[ 3 ];
   float ref[ 3 ];
   float unused[ 4 ];
   float up[ 3 ];
   int plane;
   int rc;
   int result = 0;
   int *status;

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Get a genuine pointer from the supplied grfcon identifier. */
   grfcon = astMakePointer( grfconID );

/* Report an error if no grfcon object was supplied. */
   if( !grfcon ) {
      astError( AST__INTER, "astG3DTxExt(Plot3D): No grfcon Object supplied "
                "(internal AST programming error)." , status);

/* If a grfcon Object was supplied, get the graphics box array out of it. */
   } else if( !astMapGet0D( grfcon, "Gcon", &gcon ) ) {
      astError( AST__INTER, "astG3DTxExt(Plot3D): No \"Gcon\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* If a grfcon Object was supplied, get the RootCorner value out of it. */
   } else if( !astMapGet0I( grfcon, "RootCorner", &rc ) ) {
      astError( AST__INTER, "astG3DLine(Plot3D): No \"RootCornern\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* Also get the plane index out of it. */
   } else if( !astMapGet0I( grfcon, "Plane", &plane ) ) {
      astError( AST__INTER, "astG3DTxExt(Plot3D): No \"Plane\" key found "
                "in the supplied grfcon Object (internal AST programming "
                "error)." , status);

/* If OK, get the extent of the text. */
   } else {

/* Set up the reference, up and normal vectors so that the text appears
   on the required plane. */
      if( plane == XY ) {
         ref[ 0 ] = x;
         ref[ 1 ] = y;
         ref[ 2 ] = gcon;
         norm[ 0 ] = 0;
         norm[ 1 ] = 0;
         norm[ 2 ] = ( rc & 4 ) ? 1 : -1;
         up[ 0 ] = upx;
         up[ 1 ] = upy;
         up[ 2 ] = 0;
         xb3d = xb;
         yb3d = yb;
         zb3d = unused;

      } else if( plane == XZ ) {
         ref[ 0 ] = x;
         ref[ 1 ] = gcon;
         ref[ 2 ] = y;
         norm[ 0 ] = 0;
         norm[ 1 ] = ( rc & 2 ) ? 1 : -1;
         norm[ 2 ] = 0;
         up[ 0 ] = upx;
         up[ 1 ] = 0;
         up[ 2 ] = upy;
         xb3d = xb;
         yb3d = unused;
         zb3d = yb;

      } else if( plane == YZ ) {
         ref[ 0 ] = gcon;
         ref[ 1 ] = x;
         ref[ 2 ] = y;
         norm[ 0 ] = ( rc & 1 ) ? 1 : -1;
         norm[ 1 ] = 0;
         norm[ 2 ] = 0;
         up[ 0 ] = 0;
         up[ 1 ] = upx;
         up[ 2 ] = upy;
         xb3d = unused;
         yb3d = xb;
         zb3d = yb;

      } else {
         astError( AST__INTER, "astG3DTxExt(Plot3D): Illegal plane "
                   "identifier %d supplied (internal AST programming "
                   "error).", status, plane );
      }

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
      LOCK_MUTEX2;

/* If ok, get the extent of the text. */
      if( astOK ) result = astG3DTxExt( text, ref, just, up, norm, xb3d, yb3d,
                                        zb3d, bl );
/* Allow the next thread to proceed. */
      UNLOCK_MUTEX2;
   }

/* Return the result. */
   return result;
}

static void PolyCurve( AstPlot *this, int npoint, int ncoord, int indim,
                       const double *in, int *status ){
/*
*  Name:
*     PolyCurve

*  Purpose:
*     Draw a series of connected geodesic curves.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "plot3d.h"
*     void PolyCurve( AstPlot *this, int npoint, int ncoord, int indim,
*                     const double *in, int *status )

*  Class Membership:
*     Plot method (overrides the astPolyCurve method inherited form the
*     Plot class)

*  Description:
*     This function joins a series of points specified in the physical
*     coordinate system of a Plot by drawing a sequence of geodesic
*     curves.  It is equivalent to making repeated use of the astCurve
*     function (q.v.), except that astPolyCurve will generally be more
*     efficient when drawing many geodesic curves end-to-end. A
*     typical application of this might be in drawing contour lines.
*
*     As with astCurve, full account is taken of the Mapping between
*     physical and graphical coordinate systems. This includes any
*     discontinuities and clipping established using astClip.

*  Parameters:
*     this
*        Pointer to the Plot.
*     npoint
*        The number of points between which geodesic curves are to be drawn.
*     ncoord
*        The number of coordinates being supplied for each point (i.e.
*        the number of axes in the current Frame of the Plot, as given
*        by its Naxes attribute).
*     indim
*        The number of elements along the second dimension of the "in"
*        array (which contains the input coordinates). This value is
*        required so that the coordinate values can be correctly
*        located if they do not entirely fill this array. The value
*        given should not be less than "npoint".
*     in
*        The address of the first element in a 2-dimensional array of shape
*        "[ncoord][indim]" giving the
*        physical coordinates of the points which are to be joined in
*        sequence by geodesic curves. These should be stored such that
*        the value of coordinate number "coord" for point number
*        "point" is found in element "in[coord][point]".
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - No curve is drawn on either side of any point which has any
*     coordinate equal to the value AST__BAD.
*     - An error results if the base Frame of the Plot is not
*     2-dimensional.
*     - An error also results if the transformation between the
*     current and base Frames of the Plot is not defined (i.e. the
*     Plot's TranInverse attribute is zero).
*/

/* Check the global error status. */
   if ( !astOK ) return;

   astError( AST__INTER, "astPolyCurve(%s): The astPolyCurve "
             "method cannot be used with a %s (programming error).", status,
             astGetClass( this ), astGetClass( this ) );
}

static void RemoveFrame( AstFrameSet *this_fset, int iframe, int *status ) {
/*
*  Name:
*     RemoveFrame

*  Purpose:
*     Remove a Frame from a Plot3D.

*  Type:
*     Public virtual function.

*  Synopsis:
*     #include "plot.h"
*     void RemoveFrame( AstFrameSet *this_fset, int iframe, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the astRemoveFrame public
*     method inherited from the Plot class).

*  Description:
*     This function removes a Frame from a Plot3D. All other Frames
*     in the Plot3D have their indices re-numbered from one (if
*     necessary), but are otherwise unchanged.
*
*     If the index of the original base Frame is changed, the index value
*     stored in the Plot3D is updated. If the base Frame itself is
*     removed, an error is reported.

*  Parameters:
*     this_fset
*        Pointer to the FrameSet component of the Plot3D.
*     iframe
*        The index within the Plot3D of the Frame to be removed.
*        This value should lie in the range from 1 to the number of
*        Frames in the Plot3D (as given by its Nframe attribute).
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstPlot3D *this;          /* Pointer to Plot3D structure */
   int ifrm;                 /* Validated frame index */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_fset;

/* Validate the frame index. This translated AST__BASE and AST__CURRENT
   into actual Frame indices. */
   ifrm = astValidateFrameIndex( this_fset, iframe, "astRemoveFrame" );

/* Report an error if an attempt is made to delete the Frame that defines
   the mapping onto the screen (the original base Frame in the FrameSet
   supplied when the Plot3D was constructed). */
   if( ifrm == this->pix_frame ){
      astError( AST__PXFRRM, "astRemoveFrame(%s): Cannot delete Frame "
                "number %d from the supplied %s since it is the Frame "
                "that defines the mapping onto the graphics plane.", status,
                 astGetClass( this ), iframe, astGetClass( this ) );

/* Otherwise, invoke the parent astRemoveFrame method to remove the Frame. */
   } else {
      (*parent_removeframe)( this_fset, iframe, status );

/* If the index of the removed Frame is smaller than the original base Frame
   index, then decrement the original base Frame index so that the same Frame
   will be used in future. */
      if( astOK ){
         if( ifrm < this->pix_frame ) (this->pix_frame)--;
      }
   }
}

static int RootCornerInt( const char *rootcorner, int *status ){
/*
*  Name:
*     RootCornerInt

*  Purpose:
*     Convert a RootCorner string to an integer.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int RootCornerInt( const char *rootcorner, int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function converts a RootCorner string to an integer.

*  Parameters:
*     rootcorner
*        The string value to convert. Should be 3 characters long
*        and contain nothing but "U" or "L" (upper or lower case).
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     The integer value that is is the protected equivalent of the
*     supplied string. A negative value is returned if an error has
*     already occurred, of the supplied string is not legal.

*/

/* Local Variables: */
   int result;

/* Initialise */
   result = -1;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Compare thr supplied value with every legal value. */
   if( astChrMatch( rootcorner, "LLL" ) ) {
      result = LLL;

   } else if( astChrMatch( rootcorner, "ULL" ) ) {
      result = ULL;

   } else if( astChrMatch( rootcorner, "LUL" ) ) {
      result = LUL;

   } else if( astChrMatch( rootcorner, "UUL" ) ) {
      result = UUL;

   } else if( astChrMatch( rootcorner, "LLU" ) ) {
      result = LLU;

   } else if( astChrMatch( rootcorner, "ULU" ) ) {
      result = ULU;

   } else if( astChrMatch( rootcorner, "LUU" ) ) {
      result = LUU;

   } else if( astChrMatch( rootcorner, "UUU" ) ) {
      result = UUU;

   }

/* Return the result. */
   return result;
}

static const char *RootCornerString( int rootcorner, int *status ){
/*
*  Name:
*     RootCornerString

*  Purpose:
*     Convert a RootCorner integer to a string.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     const char *RootCornerString( int rootcorner, int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function converts a RootCorner integer to a string.

*  Parameters:
*     rootcorner
*        The integer value to convert. Should be in the range 0 to 7.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     A pointer to a static string that is the public equivalent of the
*     supplied integer. A NULL pointer is returned if an error has
*     already occurred, of the supplied integer is not legal.

*/

/* Local Variables: */
   char *result;

/* Initialise */
   result = NULL;

/* Check the inherited status. */
   if( !astOK ) return result;

/* Compare thr supplied value with every legal value. */
   if( rootcorner == LLL ) {
      result = "LLL";

   } else if( rootcorner == ULL ) {
      result = "ULL";

   } else if( rootcorner == LUL ) {
      result = "LUL";

   } else if( rootcorner == UUL ) {
      result = "UUL";

   } else if( rootcorner == LLU ) {
      result = "LLU";

   } else if( rootcorner == ULU ) {
      result = "ULU";

   } else if( rootcorner == LUU ) {
      result = "LUU";

   } else if( rootcorner == UUU ) {
      result = "UUU";

   }

/* Return the result. */
   return result;
}

static void Set3DGrf( AstPlot3D *this, AstPlot *plot, int plane, int *status ){
/*
*  Name:
*     Set3DGrf

*  Purpose:
*     Associate GRF functions with a Plot.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void Set3DGrf( AstPlot3D *this, AstPlot *plot, int plane, int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function registers grf functions defined in this class with
*     the supplied Plot, so that, whenever the Plot draws anything, the
*     plotting request is caught by this class and converted into an
*     appropriate grf3D call.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     plot
*        Pointer to the Plot.
*     plane
*        An integer identifier for the plane within 3D GRAPHICS
*        coordinates upon which the supplied Plot should draw.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstKeyMap *grfcon;

/* Check the global error status. */
   if ( !astOK ) return;

/* Register the plotting functions defined in this class, so that the
   plot will call these functions to do its 2D plotting. */
   astGrfSet( plot, "Attr", (AstGrfFun) Plot3DAttr );
   astGrfSet( plot, "Cap", (AstGrfFun) Plot3DCap );
   astGrfSet( plot, "Flush", (AstGrfFun) Plot3DFlush );
   astGrfSet( plot, "Line", (AstGrfFun) Plot3DLine );
   astGrfSet( plot, "Mark", (AstGrfFun) Plot3DMark );
   astGrfSet( plot, "Qch", (AstGrfFun) Plot3DQch );
   astGrfSet( plot, "Scales", (AstGrfFun) Plot3DScales );
   astGrfSet( plot, "Text", (AstGrfFun) Plot3DText );
   astGrfSet( plot, "TxExt", (AstGrfFun) Plot3DTxExt );

/* When the above functions are called, they need to know which plane
   they are drawing on. So we put this information into the GrfContext
   KeyMap stored in the Plot. This KeyMap will be passed to the above
   drawing functions when they are called from within the Plot class. */
   grfcon = astGetGrfContext( plot );
   astMapPut0I( grfcon, "Plane", plane, "The 2D plane being drawn on" );
   if( plane == XY ) {
      astMapPut0D( grfcon, "Gcon", this->gbox[2], "Constant Z value" );
   } else if( plane == XZ ) {
      astMapPut0D( grfcon, "Gcon", this->gbox[1], "Constant Y value" );
   } else {
      astMapPut0D( grfcon, "Gcon", this->gbox[0], "Constant X value" );
   }
   astMapPut0I( grfcon, "RootCorner", astGetRootCorner(this), "The labelled corner" );
   grfcon = astAnnul( grfcon );
}

static void SetAttrib( AstObject *this_object, const char *setting, int *status ) {
/*
*  Name:
*     SetAttrib

*  Purpose:
*     Set an attribute value for a Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void SetAttrib( AstObject *this, const char *setting, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the astSetAttrib protected
*     method inherited from the Plot class).

*  Description:
*     This function assigns an attribute value for a Plot3D, the
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
*        Pointer to the Plot3D.
*     setting
*        Pointer to a null terminated string specifying the new attribute
*        value.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Variables: */
   AstPlot3D *this;              /* Pointer to the Plot3D structure */
   double dval;                  /* Floating point attribute value */
   int axis;                     /* Axis index */
   int ival;                     /* Int attribute value */
   int len;                      /* Length of setting string */
   int nc;                       /* Number of characters read by astSscanf */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_object;

/* Obtain the length of the setting string. */
   len = (int) strlen( setting );

/* Test for each recognised attribute in turn, using "astSscanf" to parse
   the setting string and extract the attribute value (or an offset to
   it in the case of string values). In each case, use the value set
   in "nc" to check that the entire string was matched. Once a value
   has been obtained, use the appropriate method to set it. */

/* Norm(axis). */
/* ----------- */
   if ( nc = 0,
        ( 2 == astSscanf( setting, "norm(%d)= %lg %n",
                          &axis, &dval, &nc ) )
               && ( nc >= len ) ) {
      astSetNorm( this, axis - 1, dval );

/* RootCorner. */
/* ----------- */
   } else if( nc = 0,
        ( 0 == astSscanf( setting, "rootcorner=%n%*[^\n]%n", &ival, &nc ) )
               && ( nc >= len ) ) {
      ival = RootCornerInt( setting + ival, status );
      if( astOK && ival < 0 ) {
         astError( AST__ATTIN, "astSetAttrib(Plot3D): Unusable value \"%s\" "
                   "given for attribute RootCorner.", status, setting + ival );
      } else {
         astSetRootCorner( this, ival );
      }

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      (*parent_setattrib)( this_object, setting, status );
   }

/* Undefine macros local to this function. */
#undef MATCH
}

static void SetCurrent( AstFrameSet *this_frameset, int iframe, int *status ) {
/*
*  Name:
*     SetCurrent

*  Purpose:
*     Set a value for the Current attribute of a Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int astSetCurrent( AstFrameSet *this, int iframe, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the public astSetCurrent method
*     inherited from the FrameSet class).

*  Description:
*     This function sets a value for the Current attribute of a
*     Plot3D. This attribute is an index that identifies the current
*     Frame for the Plot3D.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     iframe
*        Value to be set for the Current attribute.
*     status
*        Pointer to the inherited status variable.
*/

/* Invoke the parent astSetCurrent method. */
   (*parent_setcurrent)( this_frameset, iframe, status );

/* Update the three 2D Plots stored in the Plot3D structure so that they
   reflect this modified FrameSet. */
   UpdatePlots( (AstPlot3D *) this_frameset, status );
}

static void SetPlotAttr( AstPlot *plot, int plane, int label[ 2 ], int *status ){
/*
*  Name:
*     SetPlotAttr

*  Purpose:
*     Set the attributes ofr one of the encapsulated Plots.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void SetPlotAttr( AstPlot *plot, int plane, int label[ 2 ], int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function sets the attributes of one of the encapsulated Plots.

*  Parameters:
*     plot
*        Pointer to the Plot to modify.
*     plane
*        The 3D plane spanned by the 2D plot.
*     label
*        Array indicating if each WCS axis should be labelled or not.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int axis;

/* Check the inherited status. */
   if( !astOK ) return;

/* Ensure that the Plot uses the grf interface registered using
   astGrfSet. */
   astSetGrf( plot, 1 );

/* Ensure that no title is drawn. */
   astSetDrawTitle( plot, 0 );

/* For each axis, ensure that no axis labels or ticks are produced unless
   the axis is indicated as a labelled axis in the supplied array. */
   for( axis = 0; axis < 2; axis++ ) {
      if( !label[ axis ] ) {
         astSetLabelUnits( plot, axis, 0 );
         astSetNumLab( plot, axis, 0 );
         astSetTextLab( plot, axis, 0 );
      }
   }
}

static void SetRootCorner( AstPlot3D *this, int rootcorner, int *status ){
/*
*+
*  Name:
*     astSetRootCorner

*  Purpose:
*     Set a new value for the RootCorner attribute.

*  Type:
*     Protected virtual function.

*  Synopsis:
*     #include "plot3d.h"
*     void astSetRootCorner( AstPlot3D *this, int rootcorner )

*  Class Membership:
*     Plot method.

*  Description:
*     This function sets a new value for the RootCorner attribute.

*  Parameters:
*     this
*        Pointer to a Plot3D.
*     rootcorner
*        The new RootCorner value.

*-
*/

/* Check the global status. */
   if( !astOK ) return;

/* Report an error if the new value is out of bounds. */
   if( rootcorner < 0 || rootcorner > 7 ){
      astError( AST__ATTIN, "astSetRootCorner(Plot3D): Invalid value %d "
                "supplied for RootCorner attribute", status,rootcorner);

/* If the new corner is OK, mirror any axes of the encapsulated Plots
   that need mirroring (this is done to ensure that Plots look right when
   viewed from the outside of the graphics cube), and modify the Edge
   attributes in the encapsulated Plots to ensure the labels appear on the
   requested edges of the 3D graphics cube. . */
   } else {
      ChangeRootCorner( this, astGetRootCorner(this), rootcorner, status );

/* Store the new value. */
      this->rootcorner = rootcorner;
   }
}

static void SetTickValues( AstPlot *this, int axis, int nmajor, double *major,
                           int nminor, double *minor, int *status ){
/*
*  Name:
*     SetTickValues

*  Purpose:
*     Store the tick mark values to use for a given Plot axis.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "plot3d.h"
*     void SetTickValues( AstPlot *this, int axis, int nmajor,
*                         double *major, int nminor, double *minor, int *status )

*  Class Membership:
*     Plot method (overrides the astSetTickValues method inherited form
*     the Plot class)

*  Description:
*     This function stores a set of tick mark values that should be used by
*     subsequent calls to astGrid.

*  Parameters:
*     this
*        Pointer to a Plot.
*     axis
*        The zero-based index of the axis for which tick marks values
*        have been supplied.
*     nmajor
*        The number of major tick mark values. If zero is supplied then
*        the other parameters are ignored, and subsequent calls to
*        astGrid will itself determine the tick values to be used.
*     major
*        Pointer to an array holding "nmajor" values for axis "axis" in
*        the current Frame of the suppled Plot. Major tick marks will be
*        drawn at these values.
*     nminor
*        The number of minor tick mark values.
*     minor
*        Pointer to an array holding "nminor" values for axis "axis" in
*        the current Frame of the suppled Plot. Minor tick marks will be
*        drawn at these values.
*     status
*        Pointer to the inherited status variable.
*/

/* Check the global status. */
   if( !astOK ) return;

   astError( AST__INTER, "astSetTickValues(%s): The astSetTickValues "
             "method cannot be used with a %s (programming error).", status,
             astGetClass( this ), astGetClass( this ) );
}

static void SplitFrameSet( AstFrameSet *fset,
                           AstFrameSet **fsetxy, int labelxy[2], int wcsxy[2],
                           AstFrameSet **fsetxz, int labelxz[2], int wcsxz[2],
                           AstFrameSet **fsetyz, int labelyz[2], int wcsyz[2],
                           int *baseplane, int *status ){
/*
*  Name:
*     SplitFrameSet

*  Purpose:
*     Split a 3D FrameSet into three 2D FrameSets.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void SplitFrameSet( AstFrameSet *fset,
*                         AstFrameSet **fsetxy, int labelxy[2], int wcsxy[2],
*                         AstFrameSet **fsetxz, int labelxz[2], int wcsxz[2],
*                         AstFrameSet **fsetyz, int labelyz[2], int wcsyz[2],
*                         int *baseplane, int *status )

*  Class Membership:
*     Plot3D member function

*  Description:
*     This function returns 3 FrameSets, each of which has 2-dimensional
*     base and current Frames. Each of the 2D base Frames span a single
*     plane in the 3D base Frame of the supplied FrameSet. Likewise, each
*     of the 2D current Frames span a single plane in the 3D current Frame
*     of the supplied FrameSet. An error is reported if there is no
*     one-to-one association between the three planes in the supplied
*     current Frame and the 3 planes in the supplied base Frame.

*  Parameters:
*     fset
*        Pointer to a FrameSet that has a 3D base Frame and a 3D current
*        Frame.
*     fsetxy
*        Pointer to a location at which to return a pointer to a new FrameSet
*        that has a 2D base Frame and a 2D current. The base Frame spans
*        axes 1 and 2 of the 3D base Frame in "fset".
*     labelxy
*        Returned holding flags indicating if the two axes of the current
*        Frame in *fsetxy should be labelled or not.
*     wcsxy
*        Returned holding the zero based axis index within the current Frame
*        of the supplied FrameSet that corresponds to each of the two
*        current Frame axes in the "fsetxy" FrameSet.
*     fsetxz
*        Pointer to a location at which to return a pointer to a new FrameSet
*        that has a 2D base Frame and a 2D current. The base Frame spans
*        axes 1 and 3 of the 3D base Frame in "fset".
*     labelxz
*        Returned holding flags indicating if the two axes of the current
*        Frame in *fsetxz should be labelled or not.
*     wcsxz
*        Returned holding the zero based axis index within the current Frame
*        of the supplied FrameSet that corresponds to each of the two
*        current Frame axes in the "fsetxz" FrameSet.
*     fsetyz
*        Pointer to a location at which to return a pointer to a new FrameSet
*        that has a 2D base Frame and a 2D current. The base Frame spans
*        axes 2 and 3 of the 3D base Frame in "fset".
*     labelyz
*        Returned holding flags indicating if the two axes of the current
*        Frame in *fsetyz should be labelled or not.
*     wcsyz
*        Returned holding the zero based axis index within the current Frame
*        of the supplied FrameSet that corresponds to each of the two
*        current Frame axes in the "fsetxy" FrameSet.
*     baseplane
*        Index of the plane that is spanned by two connected 3D axes.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - Null pointers will be returned for the three new FrameSets if this
*     function is invoked with the global status set, or if it should fail
*     for any reason.
*     - Each returned FrameSet has 2 Frames: Frame 1 is the base Frame
*     and is spanned by 2 of the 3 axes in the base Frame of the supplied
*     FrameSet; Frame 2 is the current Frame and is spanned by 2 of the 3
*     axes in the current Frame of the supplied FrameSet. Any future changes
*     to this function that alter this structure should reflected in
*     equivalent changes to functions CreatePlots and UpdatePlots.
*/

/* Local Variables: */
   AstFrame *bfrm2d;
   AstFrame *bfrm;
   AstFrame *cfrm1d;
   AstFrame *cfrm2d;
   AstFrame *cfrm;
   AstFrame *dummy;
   AstFrameSet *fset1;
   AstFrameSet *fset2;
   AstFrameSet *fset3;
   AstMapping *map1d;
   AstMapping *map2d;
   AstMapping *map;
   AstMapping *smap;
   AstUnitMap *unit1d;
   int *axout;
   int *other_axout;
   int axin2[ 2 ];
   int axin[ 2 ];
   int i;
   int label1[ 2 ];
   int label2[ 2 ];
   int label3[ 2 ];
   int other_axin;
   int wcsax1[ 2 ];
   int wcsax2[ 2 ];
   int wcsax3[ 2 ];

/* Initialise. */
   *fsetxy = NULL;
   *fsetxz = NULL;
   *fsetyz = NULL;
   *baseplane = 0;

/* Check the global error status. */
   if ( !astOK ) return;

/* Get the simplified base -> current Mapping form the supplied FrameSet.
   Also get the base and current Frames themselves. */
   map = astGetMapping( fset, AST__BASE, AST__CURRENT );
   smap = astSimplify( map );
   map = astAnnul( map );
   cfrm = astGetFrame( fset, AST__CURRENT );
   bfrm = astGetFrame( fset, AST__BASE );

/* Create a 1D UnitMap. */
   unit1d = astUnitMap( 1, "", status );

/* First try to identify a pair of base Frame axes that map onto a pair
   of current Frame axes. This will be the case for instance if the 3D
   FrameSet describes a spectral cube in which two of the axes are spanned
   by a SkyFrame. We try all three possible pairs of axes in turn until a
   pair is found succesfully. */
   for( i = 0; i < 3  && ! *fsetxy; i++ ) {
      axin[ 0 ] = ( i < 2 ) ? 0 : 1;
      axin[ 1 ] = ( i == 0 ) ? 1 : 2;

/* Try to get a Mapping that connects the inputs given by axin to a
   distinct subset of outputs. */
      axout = astMapSplit( smap, 2, axin, &map2d );

/* If succesful, check that the 2 inputs correspond to exactly 2 outputs. */
      if( map2d ){
         if( astGetNout( map2d ) == 2 ) {

/* Get the index of the other input axis (the one not included in the pair). */
            other_axin = 3 - axin[ 0 ] - axin[ 1 ];

/* Try to get a Mapping that connects this remaining input to a distinct
   subset of outputs. */
            other_axout = astMapSplit( smap, 1, &other_axin, &map1d );

/* If succesful, check that the 1 input correspond to exactly 1 output. */
            if( map1d ){
               if( astGetNout( map1d ) == 1 ) {

/* Pick the two axes from the 3D base and current Frames that correspond to
   the paired axes found above. */
                  bfrm2d = astPickAxes( bfrm, 2, axin, NULL );
                  cfrm2d = astPickAxes( cfrm, 2, axout, NULL );

/* Pick the axis from the 3D current Frame that correspond to
   the other axis. */
                  cfrm1d = astPickAxes( cfrm, 1, other_axout, NULL );

/* Construct a FrameSet using these 2D Frames and Mapping. */
                  fset1 = astFrameSet( bfrm2d, "", status );
                  astAddFrame( fset1, AST__BASE, map2d, cfrm2d );

                  bfrm2d = astAnnul( bfrm2d );
                  cfrm2d = astAnnul( cfrm2d );
                  map2d = astAnnul( map2d );

/* Indicate that both axes in the current Frame of this FrameSet should
   be labelled. */
                  label1[ 0 ] = 1;
                  label1[ 1 ] = 1;

/* Store the index of the axis within the supplied current Frame that
   corresponds to each axis in the current Frame of the FrameSet created
   above. */
                  wcsax1[ 0 ] = axout[ 0 ];
                  wcsax1[ 1 ] = axout[ 1 ];

/* Pick the two axes from the 3D base Frame that correspond to the first of
   the paired axes, and the unpaired axis. Order them so that we get the
   2 axes in the order xy, xz or yz. Also, store the index of the axis
   within the supplied  current Frame that corresponds to each axis in
   the current Frame of the FrameSet created below. */
                  if( i < 2 ) {
                     axin2[ 0 ] = axin[ 0 ];
                     axin2[ 1 ] = other_axin;
                     wcsax2[ 0 ] = axout[ 0 ];
                     wcsax2[ 1 ] = other_axout[ 0 ];
                  } else {
                     axin2[ 0 ] = other_axin;
                     axin2[ 1 ] = axin[ 0 ];
                     wcsax2[ 0 ] = other_axout[ 0 ];
                     wcsax2[ 1 ] = axout[ 0 ];
                  }
                  bfrm2d = astPickAxes( bfrm, 2, axin2, NULL );

/* The corresponding current Frame in the new FrameSet will be a compound
   2D Frame holding the other current Frame axis and a copy of the input
   base Frame axis. Combine them so that we get the 2 axes in the order
   xy, xz or yz. */
                  dummy = astPickAxes( bfrm, 1, axin, NULL );
                  if( i < 2 ) {
                     cfrm2d = (AstFrame *) astCmpFrame( dummy, cfrm1d, "", status );
                  } else {
                     cfrm2d = (AstFrame *) astCmpFrame( cfrm1d, dummy, "", status );
                  }
                  dummy = astAnnul( dummy );

/* The Mapping that joins this 2D base Frame to the 2D current Frame uses
   the above 1D mapping for the other axis, and a UnitMap for the copied
   base Frame axis. */
                  if( i < 2 ) {
                     map2d = (AstMapping *) astCmpMap( unit1d, map1d, 0, "", status );
                  } else {
                     map2d = (AstMapping *) astCmpMap( map1d, unit1d, 0, "", status );
                  }

/* Construct a FrameSet using these 2D Frames and Mapping. */
                  fset2 = astFrameSet( bfrm2d, "", status );
                  astAddFrame( fset2, AST__BASE, map2d, cfrm2d );

                  bfrm2d = astAnnul( bfrm2d );
                  cfrm2d = astAnnul( cfrm2d );
                  map2d = astAnnul( map2d );

/* Indicate that only one of the axes in the current Frame of this FrameSet
   should be labelled. */
                  if( i < 2 ) {
                     label2[ 0 ] = 0;
                     label2[ 1 ] = 1;
                  } else {
                     label2[ 0 ] = 1;
                     label2[ 1 ] = 0;
                  }

/* Pick the two axes from the 3D base Frame that correspond to the second
   of the paired axes, and the unpaired axis. Order them so that we get the
   2 axes in the order xy, xz or yz. Also, store the index of the axis
   within the supplied  current Frame that corresponds to each axis in
   the current Frame of the FrameSet created below. */
                  if( i == 0 ) {
                     axin2[ 0 ] = axin[ 1 ];
                     axin2[ 1 ] = other_axin;
                     wcsax3[ 0 ] = axout[ 1 ];
                     wcsax3[ 1 ] = other_axout[ 0 ];
                  } else {
                     axin2[ 0 ] = other_axin;
                     axin2[ 1 ] = axin[ 1 ];
                     wcsax3[ 0 ] = other_axout[ 0 ];
                     wcsax3[ 1 ] = axout[ 1 ];
                  }
                  bfrm2d = astPickAxes( bfrm, 2, axin2, NULL );

/* The corresponding current Frame in the new FrameSet will be a compound
   2D Frame holding the other current Frame axis and a copy of the input
   base Frame axis. Combine them so that we get the 2 axes in the order
   xy, xz or yz. */
                  dummy = astPickAxes( bfrm, 1, axin + 1, NULL );
                  if( i == 0 ) {
                     cfrm2d = (AstFrame *) astCmpFrame( dummy, cfrm1d, "", status );
                  } else {
                     cfrm2d = (AstFrame *) astCmpFrame( cfrm1d, dummy, "", status );
                  }
                  dummy = astAnnul( dummy );

/* The Mapping that joins this 2D base Frame to the 2D current Frame uses
   the above 1D mapping for the other axis, and a UnitMap for the copied
   base Frame axis. */
                  if( i == 0 ) {
                     map2d = (AstMapping *) astCmpMap( unit1d, map1d, 0, "", status );
                  } else {
                     map2d = (AstMapping *) astCmpMap( map1d, unit1d, 0, "", status );
                  }

/* Construct a FrameSet using these 2D Frames and Mapping. */
                  fset3 = astFrameSet( bfrm2d, "", status );
                  astAddFrame( fset3, AST__BASE, map2d, cfrm2d );

                  bfrm2d = astAnnul( bfrm2d );
                  cfrm2d = astAnnul( cfrm2d );
                  map2d = astAnnul( map2d );

/* Indicate that neither axis in the current Frame of this FrameSet
   should be labelled. */
                  label3[ 0 ] = 0;
                  label3[ 1 ] = 0;

/* Store each FrameSet in the correct returned pointer. */
                  if( i == 0 ) {
                     *baseplane = XY;
                     *fsetxy = fset1;
                     *fsetxz = fset2;
                     *fsetyz = fset3;

                     labelxy[ 0 ] = label1[ 0 ];
                     labelxy[ 1 ] = label1[ 1 ];
                     labelxz[ 0 ] = label2[ 0 ];
                     labelxz[ 1 ] = label2[ 1 ];
                     labelyz[ 0 ] = label3[ 0 ];
                     labelyz[ 1 ] = label3[ 1 ];

                     wcsxy[ 0 ] = wcsax1[ 0 ];
                     wcsxy[ 1 ] = wcsax1[ 1 ];
                     wcsxz[ 0 ] = wcsax2[ 0 ];
                     wcsxz[ 1 ] = wcsax2[ 1 ];
                     wcsyz[ 0 ] = wcsax3[ 0 ];
                     wcsyz[ 1 ] = wcsax3[ 1 ];

                  } else if( i == 1 ) {
                     *baseplane = XZ;
                     *fsetxy = fset2;
                     *fsetxz = fset1;
                     *fsetyz = fset3;

                     labelxy[ 0 ] = label2[ 0 ];
                     labelxy[ 1 ] = label2[ 1 ];
                     labelxz[ 0 ] = label1[ 0 ];
                     labelxz[ 1 ] = label1[ 1 ];
                     labelyz[ 0 ] = label3[ 0 ];
                     labelyz[ 1 ] = label3[ 1 ];

                     wcsxy[ 0 ] = wcsax2[ 0 ];
                     wcsxy[ 1 ] = wcsax2[ 1 ];
                     wcsxz[ 0 ] = wcsax1[ 0 ];
                     wcsxz[ 1 ] = wcsax1[ 1 ];
                     wcsyz[ 0 ] = wcsax3[ 0 ];
                     wcsyz[ 1 ] = wcsax3[ 1 ];

                  } else {
                     *baseplane = YZ;
                     *fsetxy = fset2;
                     *fsetxz = fset3;
                     *fsetyz = fset1;

                     labelxy[ 0 ] = label2[ 0 ];
                     labelxy[ 1 ] = label2[ 1 ];
                     labelxz[ 0 ] = label3[ 0 ];
                     labelxz[ 1 ] = label3[ 1 ];
                     labelyz[ 0 ] = label1[ 0 ];
                     labelyz[ 1 ] = label1[ 1 ];

                     wcsxy[ 0 ] = wcsax2[ 0 ];
                     wcsxy[ 1 ] = wcsax2[ 1 ];
                     wcsxz[ 0 ] = wcsax3[ 0 ];
                     wcsxz[ 1 ] = wcsax3[ 1 ];
                     wcsyz[ 0 ] = wcsax1[ 0 ];
                     wcsyz[ 1 ] = wcsax1[ 1 ];

                  }

/*  Free resources */
                  cfrm1d = astAnnul( cfrm1d );
               }

/*  Free resources */
               map1d = astAnnul( map1d );
               other_axout = astFree( other_axout );
            }
         }

/*  Free resources */
         if( map2d ) map2d = astAnnul( map2d );
         axout = astFree( axout );

/* Leave the loop if we now have the required FrameSets, or an error has
   occurred. */
         if( *fsetxy || !astOK ) break;

      }
   }

/* Free resources */
   cfrm = astAnnul( cfrm );
   bfrm = astAnnul( bfrm );
   smap = astAnnul( smap );
   unit1d = astAnnul( unit1d );

/* Return null pointers if an error has occurred. */
   if( !astOK ) {
      *fsetxy = astAnnul( *fsetxy );
      *fsetxz = astAnnul( *fsetxz );
      *fsetyz = astAnnul( *fsetyz );

/* Report an error if the supplied FrameSet could not be split into 3
   independent 2D planes. */
   } if( ! *fsetxy ) {
      astError( AST__3DFSET, "astInitPlot3D(Plot3D): Supplied %s contains "
                             "no independent axes.", status, astGetClass( fset ) );
   }
}

static void StoreAxisInfo( AstPlot3D *this, int labelxy[2], int wcsxy[2],
                           int labelxz[2], int wcsxz[2], int labelyz[2],
                           int wcsyz[2], int *status ) {
/*
*  Name:
*     StoreAxisInfo

*  Purpose:
*     Store information connecting 3D axis with associuated 2D Plot axes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void StoreAxisInfo( AstPlot3D *this, int labelxy[2], int wcsxy[2],
*                         int labelxz[2], int wcsxz[2], int labelyz[2],
*                         int wcsyz[2], int *status )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function stores information inside the Plot3D that allows each
*     3D axis to be associated with the 2 Plots that share the 3D axis,
*     and with the axis index within each of these two Plots.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     labelxy
*        Flags indicating if the two axes of the current Frame in the XY
*        Plot should be labelled or not.
*     wcsxy
*        The zero based axis index within the 3D current Frame that
*        corresponds to each of the two current Frame axes in the XY Plot.
*        A value of -1 should be used for 2D axes that do not correspond
*        to any 3D axis.
*     labelxz
*        Flags indicating if the two axes of the current Frame in the XZ
*        Plot should be labelled or not.
*     wcsxz
*        The zero based axis index within the 3D current Frame that
*        corresponds to each of the two current Frame axes in the XZ Plot.
*        A value of -1 should be used for 2D axes that do not correspond
*        to any 3D axis.
*     labelyz
*        Flags indicating if the two axes of the current Frame in the YZ
*        Plot should be labelled or not.
*     wcsyz
*        The zero based axis index within the 3D current Frame that
*        corresponds to each of the two current Frame axes in the YZ Plot.
*        A value of -1 should be used for 2D axes that do not correspond
*        to any 3D axis.
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   int axis2d;
   int axis3d;
   int gotfirst;
   int gotsecond;
   int temp;

/* Check the inherited status. */
   if( !astOK ) return;

/* Store information that allows each 3D WCS axis to be associated with
   a pair of Plots. Also store the WCS axis within each Plot that
   corresponds to the 3D WCS axis. Do each 3D WCS axis in turn. */
   for( axis3d = 0; axis3d < 3; axis3d++ ) {

/* Indicate we have not yet found either of the two Plots that share the
   current 3D axis. */
      gotfirst = 0;
      gotsecond = 0;

/* Check each of the 2 axes in the plot spanning the XY face of the 3D
   graphics cube. Break early if we have found both Plots for the current
   3D WCS axis. */
      for( axis2d = 0; axis2d < 2 && !gotsecond; axis2d++ ) {

/* See if this 2D axis corresponds to the required 3D axis. If so, store
   the Plot identifier and the 2D axis index. */
         if( wcsxy[ axis2d ] == axis3d ) {
            if( gotfirst ) {
               this->axis_plot2[ axis3d ] = XY;
               this->axis_index2[ axis3d ] = axis2d;
               gotsecond = 1;
            } else {
               this->axis_plot1[ axis3d ] = XY;
               this->axis_index1[ axis3d ] = axis2d;
               gotfirst = 1;
            }
         }
      }

/* Check the plot spanning the XZ face in the same way. */
      for( axis2d = 0; axis2d < 2 && !gotsecond; axis2d++ ) {
         if( wcsxz[ axis2d ] == axis3d ) {
            if( gotfirst ) {
               this->axis_plot2[ axis3d ] = XZ;
               this->axis_index2[ axis3d ] = axis2d;
               gotsecond = 1;
            } else {
               this->axis_plot1[ axis3d ] = XZ;
               this->axis_index1[ axis3d ] = axis2d;
               gotfirst = 1;
            }
         }
      }

/* Check the plot spanning the YZ face in the same way. */
      for( axis2d = 0; axis2d < 2 && !gotsecond; axis2d++ ) {
         if( wcsyz[ axis2d ] == axis3d ) {
            if( gotfirst ) {
               this->axis_plot2[ axis3d ] = YZ;
               this->axis_index2[ axis3d ] = axis2d;
               gotsecond = 1;
            } else {
               this->axis_plot1[ axis3d ] = YZ;
               this->axis_index1[ axis3d ] = axis2d;
               gotfirst = 1;
            }
         }
      }
   }

/* Ensure that the first Plot within each pair is the one that is used to
   generate labels for the 3D axis. */
   for( axis2d = 0; axis2d < 2; axis2d++ ) {
      if( labelxy[ axis2d ] ) {
         axis3d = wcsxy[ axis2d ];
         if( this->axis_plot1[ axis3d ] != XY ){
            temp = this->axis_plot1[ axis3d ];
            this->axis_plot1[ axis3d ] = this->axis_plot2[ axis3d ];
            this->axis_plot2[ axis3d ] = temp;

            temp = this->axis_index1[ axis3d ];
            this->axis_index1[ axis3d ] = this->axis_index2[ axis3d ];
            this->axis_index1[ axis3d ] = temp;
         }
      }
   }

   for( axis2d = 0; axis2d < 2; axis2d++ ) {
      if( labelxz[ axis2d ] ) {
         axis3d = wcsxz[ axis2d ];
         if( this->axis_plot1[ axis3d ] != XZ ){
            temp = this->axis_plot1[ axis3d ];
            this->axis_plot1[ axis3d ] = this->axis_plot2[ axis3d ];
            this->axis_plot2[ axis3d ] = temp;

            temp = this->axis_index1[ axis3d ];
            this->axis_index1[ axis3d ] = this->axis_index2[ axis3d ];
            this->axis_index1[ axis3d ] = temp;
         }
      }
   }

   for( axis2d = 0; axis2d < 2; axis2d++ ) {
      if( labelyz[ axis2d ] ) {
         axis3d = wcsyz[ axis2d ];
         if( this->axis_plot1[ axis3d ] != YZ ){
            temp = this->axis_plot1[ axis3d ];
            this->axis_plot1[ axis3d ] = this->axis_plot2[ axis3d ];
            this->axis_plot2[ axis3d ] = temp;

            temp = this->axis_index1[ axis3d ];
            this->axis_index1[ axis3d ] = this->axis_index2[ axis3d ];
            this->axis_index1[ axis3d ] = temp;
         }
      }
   }
}

static int TestAttrib( AstObject *this_object, const char *attrib, int *status ) {
/*
*  Name:
*     TestAttrib

*  Purpose:
*     Test if a specified attribute value is set for a Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     int TestAttrib( AstObject *this, const char *attrib, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the astTestAttrib protected
*     method inherited from the Plot class).

*  Description:
*     This function returns a boolean result (0 or 1) to indicate whether
*     a value has been set for one of a Plot3D's attributes.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     attrib
*        Pointer to a null terminated string specifying the attribute
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
   AstPlot3D *this;              /* Pointer to the Plot3D structure */
   int axis;                     /* Axis index */
   int len;                      /* Length of attrib string */
   int nc;                       /* Number of character read */
   int result;                   /* Result value to return */

/* Initialise. */
   result = 0;

/* Check the global error status. */
   if ( !astOK ) return result;

/* Obtain a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_object;

/* Obtain the length of the attrib string. */
   len = strlen( attrib );

/* Check the attribute name and test the appropriate attribute. */

/* Norm. */
/* ----- */
   if ( !strcmp( attrib, "norm" ) ) {
      result = astTestNorm( this, 0 ) ||
               astTestNorm( this, 1 ) ||
               astTestNorm( this, 2 );

/* Norm(axis). */
/* ----------- */
   } else if ( nc = 0,
               ( 1 == astSscanf( attrib, "norm(%d)%n", &axis, &nc ) )
               && ( nc >= len ) ) {
      result = astTestNorm( this, axis -  1 );

/* RootCorner. */
/* ----------- */
   } else if ( !strcmp( attrib, "rootcorner" ) ) {
      result = astTestRootCorner( this );

/* If the attribute is still not recognised, pass it on to the parent
   method for further interpretation. */
   } else {
      result = (*parent_testattrib)( this_object, attrib, status );
   }

/* Return the result, */
   return result;
}

static void Text( AstPlot *this_plot, const char *text, const double pos[],
                  const float up[], const char *just, int *status ){
/*
*  Name:
*     Text

*  Purpose:
*     Draw a text string for a Plot3D.

*  Type:
*     Private member function.

*  Synopsis:
*     #include "plot3d.h"
*     void Text( AstPlot *this, const char *text, const double pos[],
*                const float up[], const char *just, int *status )

*  Class Membership:
*     Plot3D method (overrides the Text method inherited form the Plot
*     class).

*  Description:
*     This function draws a string of text at a position specified in
*     the physical coordinate system of a Plot3D. The physical position
*     is transformed into graphical coordinates to determine where the
*     text should appear within the plotting area.
*
*     The text is drawn on a 2D plane that has a normal vector given by the
*     current value of the Plot3D's "Norm" attribute.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     text
*        Pointer to a null-terminated character string containing the
*        text to be drawn. Trailing white space is ignored.
*     pos
*        An array, with one element for each axis of the Plot3D, giving
*        the physical coordinates of the point where the reference
*        position of the text string is to be placed.
*     up
*        An array holding the components of a vector in the "up"
*        direction of the text (in graphical coordinates). For
*        example, to get horizontal text, the vector {0.0f,1.0f} should
*        be supplied. "Up" is taken to be the projection of the positive
*        Z axis onto the plane specified by the current value of the
*     just
*        Pointer to a null-terminated character string identifying the
*        reference point for the text being drawn. The first character in
*        this string identifies the reference position in the "up" direction
*        and may be "B" (baseline), "C" (centre), "T" (top) or "M" (bottom).
*        The second character identifies the side-to-side reference position
*        and may be "L" (left), "C" (centre) or "R" (right ). The string is
*        case-insensitive, and only the first two characters are significant.
*
*        For example, a value of "BL" means that the left end of the
*        baseline of the original (un-rotated) text is to be drawn at the
*        position given by "pos".
*     status
*        Pointer to the inherited status variable.

*/

/* Local Variables: */
   AstMapping *mapping;    /* Pointer to graphics->physical mapping */
   AstPlot3D *this;        /* Pointer to the Plot3D structure */
   AstPointSet *pset1;     /* PointSet holding physical positions */
   AstPointSet *pset2;     /* PointSet holding graphics positions */
   char *ltext;            /* Local copy of "text" excluding trailing spaces */
   char ljust[3];          /* Upper case copy of "just" */
   const char *class;      /* Object class */
   const char *method;     /* Current method */
   const double **ptr1;    /* Pointer to physical positions */
   double **ptr2;          /* Pointer to graphics positions */
   float norm[ 3 ];        /* Single precision normal vector */
   float ref[ 3 ];         /* Single precision ref position */
   int axis;               /* Axis index */
   int escs;               /* Original astEscapes value */
   int naxes;              /* No. of axes in the base Frame */
   int ncoord;             /* No. of axes in the current Frame */
   int ulen;               /* Length of "text" excluding trailing spaces */

/* Check the global error status. */
   if ( !astOK || !text ) return;

/* Store a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_plot;

/* Store the current method and class for inclusion in error messages
   generated by lower level functions. */
   method = "astText";
   class = astClass( this );

/* Check the base Frame of the Plot is 3-D. */
   naxes = astGetNin( this );
   if( naxes != 3 && astOK ){
      astError( AST__NAXIN, "%s(%s): Number of axes (%d) in the base "
                "Frame of the supplied %s is invalid - this number should "
                "be 3.", status, method, class, naxes, class );
   }

/* Ensure AST functions do not included graphical escape sequences in any
   returned text strings. This is because the Plot3D implementation of
   this method cannot currently handle graphical escape sequences. */
   escs = astEscapes( 0 );

/* Establish the correct graphical attributes as defined by attributes
   with the supplied Plot. */
   astGrfAttrs( this, AST__TEXT_ID, 1, GRF__TEXT, method, class );

/* Get the number of coordinates in the physical coordinate frame. */
   ncoord = astGetNout( this );

/* Create a PointSet to hold the supplied physical coordinates. */
   pset1 = astPointSet( 1, ncoord, "", status );

/* Allocate memory to hold pointers to the first value on each axis. */
   ptr1 = (const double **) astMalloc( sizeof( const double * )*
                                       (size_t)( ncoord ));

/* Check the pointer can be used, then store pointers to the first value
   on each axis. */
   if( astOK ){
      for( axis = 0; axis < ncoord; axis++ ){
         ptr1[ axis ] = pos + axis;
      }
   }

/* Store these pointers in the PointSet. */
   astSetPoints( pset1, (double **) ptr1 );

/* Transform the supplied data from the current frame (i.e. physical
   coordinates) to the base frame (i.e. graphics coordinates) using
   the inverse Mapping defined by the Plot. */
   mapping = astGetMapping( this, AST__BASE, AST__CURRENT );
   pset2 = astTransform( mapping, pset1, 0, NULL );
   mapping = astAnnul( mapping );

/* Get pointers to the graphics coordinates. */
   ptr2 = astGetPoints( pset2 );

/* Take a copy of the string excluding any trailing white space. */
   ulen = astChrLen( text );
   ltext = (char *) astStore( NULL, (void *) text, ulen + 1 );

/* Check the pointers can be used. */
   if( astOK ){

/* Terminate the local copy of the text string. */
      ltext[ ulen ] = 0;

/* Produce an upper-case copy of the first two characters in "just". */
      ljust[0] = (char) toupper( (int) just[0] );
      ljust[1] = (char) toupper( (int) just[1] );
      ljust[2] = 0;

/* Convert the double precision values to single precision, checking for
   bad positions. */
      if( ptr2[0][0] != AST__BAD && ptr2[1][0] != AST__BAD &&
          ptr2[2][0] != AST__BAD ){
         ref[ 0 ] = (float) ptr2[0][0];
         ref[ 1 ] = (float) ptr2[1][0];
         ref[ 2 ] = (float) ptr2[2][0];

/* If the nornmal vector has non-zero length, draw the text. */
         norm[ 0 ] = (float) astGetNorm( this, 0 );
         norm[ 1 ] = (float) astGetNorm( this, 1 );
         norm[ 2 ] = (float) astGetNorm( this, 2 );

/* Since we are about to call an external function which may not be
   thread safe, prevent any other thread from executing the following code
   until the current thread has finished executing it. */
         LOCK_MUTEX2;

         if( norm[ 0 ] != 0.0 || norm[ 1 ] != 0.0 || norm[ 2 ] != 0.0 ){
            if( !astG3DText( ltext, ref, ljust, (float *) up, norm ) ) {
               astError( AST__GRFER, "%s(%s): Graphics error in astG3DText. ", status,
                         method, class );
            }
         } else if( astOK ) {
            astError( AST__ATTIN, "%s(%s): The vector specified by the Norm "
                      "attribute has zero length.", status, method, class );
         }

/* Allow the next thread to proceed. */
         UNLOCK_MUTEX2;
      }

/* Free the local copy of the string. */
      ltext = (char *) astFree( (void *) ltext );

   }

/* Annul the PointSets. */
   pset1 = astAnnul( pset1 );
   pset2 = astAnnul( pset2 );

/* Free the memory holding the pointers to the first value on each axis. */
   ptr1 = (const double **) astFree( (void *) ptr1 );

/* Re-establish the original graphical attributes. */
   astGrfAttrs( this, AST__TEXT_ID, 0, GRF__TEXT, method, class );

/* Restore the original value of the flag which says whether graphical
   escape sequences should be incldued in any returned text strings. */
   astEscapes( escs );

/* Return */
   return;
}

static AstPointSet *Transform( AstMapping *this_mapping, AstPointSet *in,
                               int forward, AstPointSet *out, int *status ) {
/*
*  Name:
*     Transform

*  Purpose:
*     Use a Plot to transform a set of points.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     AstPointSet *Transform( AstMapping *this, AstPointSet *in,
*                             int forward, AstPointSet *out, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the astTransform protected
*     method inherited from the Plot class).

*  Description:
*     This function takes a Plot3D and a set of points encapsulated in a
*     PointSet and transforms the points from graphics coordinates to
*     physical coordinates (in the forward direction). No clipping or
*     normalisation is applied.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     in
*        Pointer to the PointSet holding the input coordinate data.
*     forward
*        A non-zero value indicates that the forward coordinate
*        transformation should be applied while a zero value requests the
*        inverse transformation.
*     out
*        Pointer to a PointSet which will hold the transformed (output)
*        coordinate values. A NULL value may also be given, in which case a
*        new PointSet will be created by this function.
*     status
*        Pointer to the inherited status variable.

*  Returned Value:
*     Pointer to the output (possibly new) PointSet.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*     -  The number of coordinate values per point in the input PointSet must
*     match the number of coordinates for the Plot being applied.
*     -  If an output PointSet is supplied, it must have space for sufficient
*     number of points and coordinate values per point to accommodate the
*     result. Any excess space will be ignored.
*/

/* Local Variables: */
   AstMapping *map;              /* Pointer to the mapping */
   AstPointSet *result;          /* Positions in output Frame */
   AstPlot3D *this;              /* The Plot3D */

/* Check the global error status. */
   if ( !astOK ) return NULL;

/* Get a pointer to the Plot3D. */
   this = (AstPlot3D *) this_mapping;

/* Get the Mapping from the base to the current Frame. */
   map = astGetMapping( this, AST__BASE, AST__CURRENT );

/* Do the transformation. */
   result = astTransform( map, in, forward, out );

/* Annul the mapping. */
   map = astAnnul( map );

/* If an error has occurred, annul the result. */
   if( !astOK ) result = astAnnul( result );

/* Return a pointer to the output PointSet. */
   return result;

}

static void UpdatePlots( AstPlot3D *this, int *status ) {
/*
*  Name:
*     UpdatePlots

*  Purpose:
*     Update the three 2D plots stored in the Plot3D.

*  Type:
*     Private function.

*  Synopsis:
*     #include "plot3d.h"
*     void UpdatePlots( AstPlot3D *this )

*  Class Membership:
*     Plot3D method.

*  Description:
*     This function splits the parent FrameSet up into 3 independent 2D
*     FrameSets, each describing a 2D plane in the supplied 3D FrameSet.
*     It then uses these 2D FrameSets to update the three Plots in the
*     Plot3D, by removing the existing current Frame and adding in the
*     new current Frame with the associated Mapping.

*  Parameters:
*     this
*        Pointer to the Plot3D.

*  Notes:
*     - Each of the 3 plots has 3 Frames: Frame 1 is the base (GRAPHICS)
*     Frame; Frame 2 is spanned by 2 of the 3 axes in the base Frame of
*     the supplied FrameSet; Frame 3 is the current Frame and is spanned
*     by 2 of the 3 axes in the current Frame of the supplied FrameSet.
*     Any future changes to this function that alter this structure should
*     reflected in equivalewnt changes to function CreatePlots.
*/

/* Local Variables: */
   AstFrame *frm;
   AstFrameSet *fsetxy;
   AstFrameSet *fsetxz;
   AstFrameSet *fsetyz;
   AstFrameSet *fset;
   AstMapping *map;
   int baseplane;
   int labelxy[ 2 ];
   int labelxz[ 2 ];
   int labelyz[ 2 ];
   int wcsxy[ 2 ];
   int wcsxz[ 2 ];
   int wcsyz[ 2 ];
   int rootcorner;

/* Check the inherited status. */
   if( !astOK ) return;

/* Return without action if the Plot3D does not contain the three 2D
   Plots. This may be the case for instance if this function is called
   before the Plot3D is fully constructed. */
   if( this->plotxy && this->plotxz && this->plotyz ){

/* We need a FrameSet that is equivalent to the one that was used to
   construct the Plot3D (i.e. a FrameSet that does not have the GRAPHICS
   Frame that was added by the Plot3D constructor). So take a copy of the
   parent FrameSet, remove the GRAPHICS Frame (Frame 1) and set the base
   Frame to be the Frame that was the base Frame when the Plot3D was
   constructed. */
      fset = (AstFrameSet *) astCast( this, dummy_frameset );
      astSetBase( fset, this->pix_frame );
      astRemoveFrame( fset, 1 );

/* Split the supplied FrameSet up into 3 FrameSets, each with a 2D base
   and current Frame. Each of these FrameSets describes one plane of
   the 3D cube. */
      SplitFrameSet( fset, &fsetxy, labelxy, wcsxy, &fsetxz, labelxz, wcsxz,
                     &fsetyz, labelyz, wcsyz, &baseplane, status );

/* First do the XY Plot. Extract the current Frame and base->current
   Mapping from the new FrameSet. It is assumed that he base Frame in the
   new FrameSet is equivalent to Frame 2 in the existing Plot. This
   assumption is based on the way the CreatePlots and SplitFrameSet
   functions work, and should be reviewed if either of these functions
   is changed. */
      frm = astGetFrame( fsetxy, 2 );
      map = astGetMapping( fsetxy, 1, 2 );

/* Add the new Frame into the existing Plot using the Mapping to connect
   it to Frame 2 in the Plot. It becomes the current Frame in the Plot. */
      astAddFrame( this->plotxy, 2, map, frm );

/* Delete the original current Frame in the Plot since it is not needed
   any more. */
      astRemoveFrame( this->plotxy, 3 );

/* Set the Plot attributes. */
      SetPlotAttr( this->plotxy, XY, labelxy, status );

/* Free resources */
      fsetxy = astAnnul( fsetxy );
      map = astAnnul( map );
      frm = astAnnul( frm );

/* Do the same for the XZ plot. */
      frm = astGetFrame( fsetxz, 2 );
      map = astGetMapping( fsetxz, 1, 2 );
      astAddFrame( this->plotxz, 2, map, frm );
      astRemoveFrame( this->plotxz, 3 );
      SetPlotAttr( this->plotxz, XZ, labelxz, status );
      fsetxz = astAnnul( fsetxz );
      map = astAnnul( map );
      frm = astAnnul( frm );

/* Do the same for the YZ plot. */
      frm = astGetFrame( fsetyz, 2 );
      map = astGetMapping( fsetyz, 1, 2 );
      astAddFrame( this->plotyz, 2, map, frm );
      astRemoveFrame( this->plotyz, 3 );
      SetPlotAttr( this->plotyz, YZ, labelyz, status );
      fsetyz = astAnnul( fsetyz );
      map = astAnnul( map );
      frm = astAnnul( frm );

/* Store information that allows each 3D WCS axis to be associatedf with
   a pair of Plots. Also store the WCS axis within each Plot that
   corresponds to the 3D WCS axis. */
      StoreAxisInfo( this, labelxy, wcsxy, labelxz, wcsxz, labelyz, wcsyz, status );

/* Set up the Edges attributes in the encapsulated Plots to produce labels
   on the required edges of the 3D graphics cube. */
      rootcorner = astGetRootCorner( this );
      ChangeRootCorner( this, rootcorner, rootcorner, status );

/* Store the plane spanned by two connected 3D axes. */
      this->baseplot = baseplane;

/* Free remaining resources */
      fset = astAnnul( fset );
   }
}

static void VSet( AstObject *this_object, const char *settings,
                  char **text, va_list args, int *status ) {
/*
*  Name:
*     VSet

*  Purpose:
*     Set values for a Plot3D's attributes.

*  Type:
*     Private function.

*  Synopsis:
*     #include "frameset.h"
*     void VSet( AstObject *this, const char *settings, char **text,
*                va_list args, int *status )

*  Class Membership:
*     Plot3D member function (over-rides the protected astVSet
*     method inherited from the Object class).

*  Description:
*     This function assigns a set of attribute values for a Plot3D,
*     the attributes and their values being specified by means of a
*     string containing a comma-separated list of the form:
*
*        "attribute1 = value1, attribute2 = value2, ... "
*
*     Here, "attribute" specifies an attribute name and the value to
*     the right of each "=" sign should be a suitable textual
*     representation of the value to be assigned to that attribute. This
*     will be interpreted according to the attribute's data type.
*
*     The string supplied may also contain "printf"-style format
*     specifiers identified by a "%" sign in the usual way. If
*     present, these will be substituted by values supplied as
*     optional arguments (as a va_list variable argument list), using
*     the normal "printf" rules, before the string is used.

*  Parameters:
*     this
*        Pointer to the Plot3D.
*     settings
*        Pointer to a null-terminated string containing a
*        comma-separated list of attribute settings.
*     text
*        Pointer to a location at which to return a pointer to dynamic
*        memory holding a copy of the expanded setting string. This memory
*        should be freed using astFree when no longer needed. If a NULL
*        pointer is supplied, no string is created.
*     args
*        The variable argument list which contains values to be
*        substituted for any "printf"-style format specifiers that
*        appear in the "settings" string.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     - This function preserves the integrity of the Plot3D (if
*     possible) by appropriately modifying the three encapsulated Plots.
*/

/* Invoke the parent astVSet method to set the Plot3D's attribute values. */
   (*parent_vset)( this_object, settings, text, args, status );

/* Update the three 2D Plots stored in the Plot3D structure so that they
   reflect this modified FrameSet. */
   UpdatePlots( (AstPlot3D *) this_object, status );
}

/* Functions which access Plot3D class attributes. */
/* ----------------------------------------------- */

/* RootCorner. */
/* ----------- */
/*
*att++
*  Name:
*     RootCorner

*  Purpose:
*     Specifies which edges of the 3D box should be annotated.

*  Type:
*     Public attribute.

*  Synopsis:
*     String.

*  Description:
*     This attribute controls the appearance of an annotated
c     coordinate grid (drawn with the astGrid function) by determining
f     coordinate grid (drawn with the AST_GRID routine) by determining
*     which edges of the cube enclosing the 3D graphics space are used
*     for displaying numerical and descriptive axis labels. The attribute
*     value identifies one of the eight corners of the cube within
*     which graphics are being drawn (i.e. the cube specified by the
c     "graphbox" parameter when astPlot3D
f     GRAPHBOX argument when AST_PLOT3D
*     was called tp create the Plot3D). Axis labels and tick marks will
*     be placed on the three cube edges that meet at the given corner.
*
*     The attribute value should consist of three character, each of
*     which must be either "U" or "L". The first character in the string
*     specifies the position of the corner on the first graphics axis.
*     If the character is "U" then the corner is at the upper bound on the
*     first graphics axis. If it is "L", then the corner is at the lower
*     bound on the first axis. Likewise, the second and third characters
*     in the string specify the location of the corner on the second and
*     third graphics axes.
*
*     For instance, corner "LLL" is the corner that is at the lower bound
*     on all three graphics axes, and corner "ULU" is at the upper bound
*     on axes 1 and 3 but at the lower bound on axis 2.
*
*     The default value is "LLL".

*  Applicability:
*     Plot3D
*        All Plot3Ds have this attribute.

*att--
*/

/* Internally, the RootCorner is represented as an integer bit mask, with
   bit zero for the X axis, bit 1 for the Y axis and bit 2 for the Z axis.
   A bit is set if the corner is at the upper bound on the axis and unset
   if it is at the lower bound. A value of -1 indicates that the attribue
   has not been assigned a value. */
astMAKE_GET(Plot3D,RootCorner,int,0,( this->rootcorner == -1 ? 0 : this->rootcorner))
astMAKE_TEST(Plot3D,RootCorner,( this->rootcorner != -1 ))


/* Norm(axis). */
/* ----------- */
/*
*att++
*  Name:
*     Norm(axis)

*  Purpose:
*     Specifies the plane upon which a Plot3D draws text and markers.

*  Type:
*     Public attribute.

*  Synopsis:
*     Floating point.

*  Description:
*     This attribute controls the appearance of text and markers drawn
*     by a Plot3D. It specifies the orientation of the plane upon which
*     text and markers will be drawn by all subsequent invocations of the
c     astText and astMark functions.
f     AST_TEXT and AST_MARK functions.
*
*     When setting or getting the Norm attribute, the attribute name must
*     be qualified by an axis index in the range 1 to 3. The 3 elements of
*     the Norm attribute are together interpreted as a vector in 3D graphics
*     coordinates that is normal to the plane upon which text and marks
*     should be drawn. When testing or clearing the attribute, the axis
*     index is optional. If no index is supplied, then clearing the Norm
*     attribute will clear all three elements, and testing the Norm attribute
*     will return a non-zero value if any of the three elements are set.
*
*     The default value is 1.0 for each of the 3 elements. The length of
*     the vector is insignificant, but an error will be reported when
*     attempting to draw text or markers if the vector has zero length.

*  Applicability:
*     Plot
*        All Plot3Ds have this attribute.

*att--
*/
MAKE_CLEAR3(Norm,norm,AST__BAD,3)
MAKE_SET3(Norm,double,norm,value,3)
MAKE_TEST3(Norm,( this->norm[axis] != AST__BAD ),3)
MAKE_GET3(Norm,double,AST__BAD,(this->norm[axis]!=AST__BAD?this->norm[axis]:1.0),3)



/* Functions which access Plot class attributes. */
/* --------------------------------------------- */

/* First do axis specific attributes. */

#define MAKE_ALL(attr,type,badval,whichplots) \
   MAKE_CLEAR(attr,whichplots) \
   MAKE_SET(attr,type,whichplots) \
   MAKE_GET(attr,type,badval )

MAKE_ALL(MinTick,int,0,0)
MAKE_ALL(Abbrev,int,0,1)
MAKE_ALL(Gap,double,AST__BAD,1)
MAKE_ALL(LogGap,double,AST__BAD,1)
MAKE_ALL(LogPlot,int,0,0)
MAKE_ALL(LogTicks,int,0,0)
MAKE_ALL(LogLabel,int,0,1)
MAKE_ALL(LabelUp,int,0,1)
MAKE_ALL(DrawAxes,int,0,0)
MAKE_ALL(LabelUnits,int,0,1)
MAKE_ALL(MinTickLen,double,0.0,0)
MAKE_ALL(MajTickLen,double,0.0,0)
MAKE_ALL(NumLab,int,0,1)
MAKE_ALL(NumLabGap,double,AST__BAD,1)
MAKE_ALL(TextLab,int,0,1)
MAKE_ALL(TextLabGap,double,AST__BAD,1)

#undef MAKE_ALL


/* Now do attributes that are not axis specific. */

#define MAKE_ALL(attr,type) \
   MAKE_CLEAR1(attr) \
   MAKE_SET1(attr,type)

MAKE_ALL(Ink,int)
MAKE_ALL(Tol,double)
MAKE_ALL(Invisible,int)
MAKE_ALL(TickAll,int)
MAKE_ALL(ForceExterior,int)
MAKE_ALL(Border,int)
MAKE_ALL(Clip,int)
MAKE_ALL(ClipOp,int)
MAKE_ALL(Escape,int)
MAKE_ALL(Grid,int)
MAKE_ALL(Labelling,int)

#undef MAKE_ALL



/* First do element-specific attributes. */

#define MAKE_ALL(attr,type) \
   MAKE_CLEAR2(attr) \
   MAKE_SET2(attr,type)

MAKE_ALL(Style,int)
MAKE_ALL(Font,int)
MAKE_ALL(Colour,int)
MAKE_ALL(Width,double)
MAKE_ALL(Size,double)

#undef MAKE_ALL





/* Copy constructor. */
/* ----------------- */
static void Copy( const AstObject *objin, AstObject *objout, int *status ) {
/*
*  Name:
*     Copy

*  Purpose:
*     Copy constructor for Plot3D objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Copy( const AstObject *objin, AstObject *objout, int *status )

*  Description:
*     This function implements the copy constructor for Plot3D objects.

*  Parameters:
*     objin
*        Pointer to the object to be copied.
*     objout
*        Pointer to the object being constructed.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     -  This constructor makes a deep copy.
*/

/* Local Variables: */
   AstPlot3D *in;             /* Pointer to input Plot3D */
   AstPlot3D *out;            /* Pointer to output Plot3D */

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain pointers to the input and output Plot3Ds. */
   in = (AstPlot3D *) objin;
   out = (AstPlot3D *) objout;

/* Nullify the pointers stored in the output object since these will
   currently be pointing at the input data (since the output is a simple
   byte-for-byte copy of the input). Otherwise, the input data could be
   freed by accidient if the output object is deleted due to an error
   occuring in this function. */
   out->plotxy = NULL;
   out->plotxz = NULL;
   out->plotyz = NULL;

/* Copy the encapsulated Plots */
   if( in->plotxy ) out->plotxy = astCopy( in->plotxy );
   if( in->plotxz ) out->plotxz = astCopy( in->plotxz );
   if( in->plotyz ) out->plotyz = astCopy( in->plotyz );

/* If an error has occurred, free the output resources. */
   if( !astOK ) Delete( (AstObject *) out, status );

}

/* Destructor. */
/* ----------- */
static void Delete( AstObject *obj, int *status ) {
/*
*  Name:
*     Delete

*  Purpose:
*     Destructor for Plot3D objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Delete( AstObject *obj, int *status )

*  Description:
*     This function implements the destructor for Plot3D objects.

*  Parameters:
*     obj
*        Pointer to the object to be deleted.
*     status
*        Pointer to the inherited status variable.

*  Notes:
*     This function attempts to execute even if the global error status is
*     set.
*/

/* Local Variables: */
   AstPlot3D *this;

/* Release the memory referred to in the Plot3D structure. */
   this = (AstPlot3D *) obj;
   if( this ) {
      this->plotxy = astDelete( this->plotxy );
      this->plotxz = astDelete( this->plotxz );
      this->plotyz = astDelete( this->plotyz );
   }
}

/* Dump function. */
/* -------------- */
static void Dump( AstObject *this_object, AstChannel *channel, int *status ) {
/*
*  Name:
*     Dump

*  Purpose:
*     Dump function for Plot3D objects.

*  Type:
*     Private function.

*  Synopsis:
*     void Dump( AstObject *this, AstChannel *channel, int *status )

*  Description:
*     This function implements the Dump function which writes out data
*     for the Plot3D class to an output Channel.

*  Parameters:
*     this
*        Pointer to the Plot3D whose data are being written.
*     channel
*        Pointer to the Channel to which the data are being written.
*     status
*        Pointer to the inherited status variable.
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   AstPlot3D *this;
   double dval;
   char key[ KEY_LEN + 1 ];
   int axis;
   int helpful;
   int ival;
   int set;
   const char *text;

/* Check the global error status. */
   if ( !astOK ) return;

/* Obtain a pointer to the Plot3D structure. */
   this = (AstPlot3D *) this_object;

/* Write out values representing the instance variables for the
   Plot3D class.  Accompany these with appropriate comment strings,
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


/* Norm. */
/* ----- */
   for ( axis = 0; axis < 3; axis++ ) {
      dval = astGetNorm( this, axis );
      helpful = ( dval != -DBL_MAX );
      (void) sprintf( key, "Norm%d", axis + 1 );
      astWriteDouble( channel, key, 0, helpful, dval, "Text plane normal vector");
   }

/* RootCorner. */
/* ----------- */
   set = TestRootCorner( this, status );
   ival = set ? GetRootCorner( this, status ) : astGetRootCorner( this );
   text = RootCornerString( ival, status );
   if( text ) {
      astWriteString( channel, "RootCn", set, 1, text, "Corner where labelled axes meet" );
   } else if( astOK ) {
      astError( AST__INTER, "astDump(Plot3D): Illegal value %d found for "
                "RootCorner attribute (interbal AST programming error).", status,
                ival );
   }

/* Labelled axes */
   astWriteInt( channel, "AxPlX1", 1, 1, this->axis_plot1[0],
                "Plot used to label the 3D X axis" );
   astWriteInt( channel, "AxPlY1", 1, 1, this->axis_plot1[1],
                "Plot used to label the 3D Y axis" );
   astWriteInt( channel, "AxPlZ1", 1, 1, this->axis_plot1[2],
                "Plot used to label the 3D Z axis" );
   astWriteInt( channel, "AxInX1", 1, 1, this->axis_index1[0],
                "Plot axis index used to label the 3D X axis" );
   astWriteInt( channel, "AxInY1", 1, 1, this->axis_index1[1],
                "Plot axis index used to label the 3D Y axis" );
   astWriteInt( channel, "AxInZ1", 1, 1, this->axis_index1[2],
                "Plot axis index used to label the 3D Z axis" );

/* Unlabelled axes */
   astWriteInt( channel, "AxPlX2", 1, 1, this->axis_plot2[0],
                "Other Plot touching the 3D X axis" );
   astWriteInt( channel, "AxPlY2", 1, 1, this->axis_plot2[1],
                "Other Plot touching the 3D Y axis" );
   astWriteInt( channel, "AxPlZ2", 1, 1, this->axis_plot2[2],
                "Other Plot touching the 3D Z axis" );
   astWriteInt( channel, "AxInX2", 1, 1, this->axis_index2[0],
                "Other Plot axis index touching the 3D X axis" );
   astWriteInt( channel, "AxInY2", 1, 1, this->axis_index2[1],
                "Other Plot axis index touching the 3D Y axis" );
   astWriteInt( channel, "AxInZ2", 1, 1, this->axis_index2[2],
                "Other Plot axis index touching the 3D Z axis" );

/* The Plot that spans the two connected axes. */
   astWriteInt( channel, "BasePl", 1, 1, this->baseplot,
                "Plot spanning two connected 3D axes" );

/* XY Plot */
   astWriteObject( channel, "PlotXY", 1, 1, this->plotxy,
                   "Plot describing the XY plane" );

/* XZ Plot */
   astWriteObject( channel, "PlotXZ", 1, 1, this->plotxz,
                   "Plot describing the XZ plane" );

/* YZ Plot */
   astWriteObject( channel, "PlotYZ", 1, 1, this->plotyz,
                   "Plot describing the YZ plane" );

/* The index within the Plot3D FrameSet, of the original base Frame in
   the FrameSet supplied when the Plot3D was constructed. */
   astWriteInt( channel, "PixFrm", 1, 0, this->pix_frame,
                "Index of original base Frame" );

/* Undefine macros local to this function. */
#undef KEY_LEN
}

/* Standard class functions. */
/* ========================= */
/* Implement the astIsAPlot3D and astCheckPlot3D functions using the
   macros defined for this purpose in the "object.h" header file. */
astMAKE_ISA(Plot3D,Plot)
astMAKE_CHECK(Plot3D)


AstPlot3D *astPlot3D_( void *frame_void, const float *graphbox,
                       const double *basebox, const char *options, int *status, ...) {
/*
*+
*  Name:
*     astPlot3D

*  Purpose:
*     Create a Plot3D.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "plot3d.h"
*     AstPlot3D *astPlot3D( AstFrame *frame, const float *graphbox,
*                           const double *basebox, const char *options, ..., int *status )

*  Class Membership:
*     Plot3D constructor.

*  Description:
*     This function creates a new Plot3D and optionally initialises its
*     attributes.
*
*     The supplied Frame (or the base frame if a FrameSet was supplied) is
*     assumed to be related to the graphics world coordinate system by a
*     simple shift and scale along each axis. The mapping between graphics
*     world coordinates and this Frame is specified by supplying the
*     coordinates in both systems at the lower bounds and upper bounds corners
*     of a box on the graphics device. By default, no graphics will be
*     produced outside the supplied box, but this default behaviour can be
*     changed by setting explicit values for the various clipping attributes.

*  Parameters:
*     frame
*        A pointer to a Frame or FrameSet to be annotated. If a NULL pointer
*        is supplied, then a default 3-D Frame will be created to which labels,
*        etc, can be attached by setting the relevant Frame attributes.
*     graphbox
*        A pointer to an array of 6 values giving the graphics world
*        coordinates of the lower bounds and upper bound corners of a box on
*        the graphics output device. The first triple of values should be the
*        coordinates of the lower bounds corner of the box and the second
*        triple of values should be the coordinates of the upper bounds corner.
*     basebox
*        A pointer to an array of 6 values giving the coordinates in the
*        supplied Frame, or base frame of the supplied FrameSet, at the
*        lower bounds corner and upper bounds corners of the box specified
*        by parameter graphbox. These should be supplied in the same order as
*        for parameter "graphbox".
*     options
*        Pointer to a null terminated string containing an optional
*        comma-separated list of attribute assignments to be used for
*        initialising the new Plot3D. The syntax used is the same as for the
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
*     A pointer to the new Plot3D.

*  Notes:
*     -  A NULL pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-

*  Implementation Notes:
*     - This function implements the basic Plot3D constructor which
*     is available via the protected interface to the Plot3D class.
*     A public interface is provided by the astPlot3DId_ function.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstPlot3D *new;               /* Pointer to new Plot3D */
   va_list args;                 /* Variable argument list */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   new = NULL;

/* Obtain and validate a pointer to any supplied Frame structure. */
   if( frame_void ){
      frame = astCheckFrame( frame_void );
   } else {
      frame = NULL;
   }

/* Check the pointer can be used. */
   if ( astOK ) {

/* Initialise the Plot3D, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitPlot3D( NULL, sizeof( AstPlot3D ), !class_init,
                           &class_vtab, "Plot3D", frame, graphbox,
                           basebox );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   Plot's attributes. */
         va_start( args, status );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return a pointer to the new Plot. */
   return new;
}

AstPlot3D *astInitPlot3D_( void *mem, size_t size, int init,
                           AstPlot3DVtab *vtab, const char *name,
                           AstFrame *frame, const float *graphbox,
                           const double *basebox, int *status ) {
/*
*+
*  Name:
*     astInitPlot3D

*  Purpose:
*     Initialise a Plot3D.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "plot3d.h"
*     AstPlot3D *astInitPlot3D( void *mem, size_t size, int init,
*                               AstPlotVtab *vtab, const char *name,
*                               AstFrame *frame, const float *graphbox,
*                               const double *basebox )

*  Class Membership:
*     Plot3D initialiser.

*  Description:
*     This function is provided for use by class implementations to
*     initialise a new Plot3D object. It allocates memory (if
*     necessary) to accommodate the Plot3D plus any additional data
*     associated with the derived class. It then initialises a
*     Plot3D structure at the start of this memory. If the "init"
*     flag is set, it also initialises the contents of a virtual function
*     table for a Plot3D at the start of the memory passed via the
*     "vtab" parameter.

*  Parameters:
*     mem
*        A pointer to the memory in which the Plot3D is to be
*        created. This must be of sufficient size to accommodate the
* 	 Plot3D data (sizeof(Plot3D)) plus any data used by
* 	 the derived class. If a value of NULL is given, this function
*	 will allocate the memory itself using the "size" parameter to
*	 determine its size.
*     size
*        The amount of memory used by the Plot3D (plus derived
*	 class data). This will be used to allocate memory if a value of
*	 NULL is given for the "mem" parameter. This value is also stored
*	 in the Plot3D structure, so a valid value must be supplied
*	 even if not required for allocating memory.
*     init
*        A logical flag indicating if the Plot3D's virtual function
*	 table is to be initialised. If this value is non-zero, the
*	 virtual function table will be initialised by this function.
*     vtab
*        Pointer to the start of the virtual function table to be
*	 associated with the new Plot3D.
*     name
*        Pointer to a constant null-terminated character string which
*	 contains the name of the class to which the new object belongs
*	 (it is this pointer value that will subsequently be returned by
*	 the astGetClass method).
*     fset
*        A pointer to the Frame or Frameset to be annotated.
*     graphbox
*        A pointer to an array of 6 values giving the graphics coordinates
*        of the bottom left and top right corners of a box on the graphics
*        output device. The first triple of values should be the graphics
*        coordinates of the bottom left corner of the box and the second
*        triple of values are the graphics coordinates of the top right corner.
*     basebox
*        A pointer to an array of 6 values giving the coordinates in the
*        supplied Frame or base Frame of the supplied FrameSet at the bottom
*        left and top right corners of the box specified by parameter graphbox.
*        These should be supplied in the same order as for parameter "graphbox".

*  Returned Value:
*     A pointer to the new Plot3D.

*  Notes:
*     -  A null pointer will be returned if this function is invoked with the
*     global error status set, or if it should fail for any reason.
*-
*/

/* Local Variables: */
   AstFrame *baseframe = NULL;  /* Pointer to base frame */
   AstFrame *graphicsframe = NULL; /* Pointer to graphics frame */
   AstFrameSet *fset0 = NULL;   /* The n-D FrameSet to be annotated */
   AstFrameSet *fset = NULL;    /* The 2-D FrameSet to be annotated */
   AstMapping *map = NULL;      /* Mapping for converting bbox -> gbox */
   AstPlot3D *new = NULL;       /* Pointer to new Plot3D */
   char *mess = NULL;           /* Pointer to a descriptive message */
   double djunkbox[ 4 ] = { 0.0, 0.0, 1.0, 1.0 }; /* Dummy 2D basebox */
   float fjunkbox[ 4 ] = { 0.0, 0.0, 1.0, 1.0 }; /* Dummy 2D graphbox */
   int bi;                      /* Index of base frame */
   int ci;                      /* Index of current frame */
   int i;                       /* Loop count */
   int ii;                      /* Loop count */
   int naxes;                   /* No. of axes in frame */
   int nfrm;                    /* Number of Frames in frameset */

/* Check the global status. */
   if ( !astOK ) return NULL;

/* If necessary, initialise the virtual function table. */
   if ( init ) astInitPlot3DVtab( vtab, name );

/* First of all we need to ensure that we have a FrameSet and a base
   Frame on which to base the new Plot3D. If a NULL Frame pointer was
   supplied, create a default 3-D Frame, and then create a FrameSet
   containing just this default Frame. Also store a pointer to a
   message which can be used to describe the object within error
   messages. */
   if( !frame ){
      baseframe = astFrame( 3, "", status );
      fset = astFrameSet( baseframe, "", status );
      mess = "default 3-d Frame";

/* If an object was supplied, report an error if it is not a Frame or
   an object derived from a Frame (such as a FrameSet). */
   } else if( !astIsAFrame( frame ) ){
      if( astOK ){
         astError( AST__BDOBJ, "astInitPlot3D(%s): Supplied Object (class '%s') "
                   "is not a Frame.", status, name, astGetClass( frame ) );
      }

/* If the supplied object is a Plot3D or an object derived from a Plot3D
   (a Plot3D is a sort of Frame and so will pass the above test), extract a
   FrameSet from the Plot3D, and clear the Domain attribute for any existing
   Frames which have Domain GRAPHICS. */
   } else if( astIsAPlot3D( frame ) ){
      fset0 = astFrameSet( frame, "", status );
      fset = astCopy( fset0 );
      fset0 = astAnnul( fset0 );

      for( i = 0; i < astGetNframe( fset ); i++ ) {
         graphicsframe = astGetFrame( fset, i );
         if( !strcmp( astGetDomain( graphicsframe ), "GRAPHICS" ) ) {
            astClearDomain( graphicsframe );
         }
         graphicsframe = astAnnul( graphicsframe );
      }

      baseframe = astGetFrame( fset, astGetBase( fset ) );
      mess = "base Frame of the supplied Plot3D";

/* If the object is not a FrameSet, create a FrameSet holding the
   supplied Frame. If the Frame is not 3D, an extra 3D Frame is
   included in the FrameSet derived from axes 1, 2 and 3 of the supplied
   Frame. This new Frame becomes the base Frame. */
   } else if( !astIsAFrameSet( frame ) ){
      fset0 = astFrameSet( frame, "", status );
      mess = "supplied Frame";
      fset = Fset3D( fset0, AST__BASE, status );
      fset0 = astAnnul( fset0 );
      baseframe = astGetFrame( fset, astGetBase( fset ) );

/* If a FrameSet was supplied, ensure it has a 3D base Frame.
   If the supplied FrameSet is not 3D, then a new base Frame is
   inserted into it which is derived from axes 1, 2 and 3 of the
   original base Frame. */
   } else {
      fset = Fset3D( (AstFrameSet *) frame, AST__BASE, status );
      baseframe = astGetFrame( fset, astGetBase( fset ) );
      mess = "base Frame of the supplied FrameSet";
   }

/* Check that there are 3 axes in the base frame of the FrameSet. */
   naxes = astGetNaxes( baseframe );
   if ( naxes != 3 && astOK ) {
      astError( AST__NAXIN, "astInitPlot3D(%s): Number of axes (%d) in the %s "
                "is invalid - this number should be 3.", status, name, naxes, mess );
   }

/* Check that no dimension of the graphbox is bad. */
   if( astISBAD(graphbox[0]) || astISBAD(graphbox[1]) ||
       astISBAD(graphbox[2]) || astISBAD(graphbox[3]) ||
       astISBAD(graphbox[4]) || astISBAD(graphbox[5]) ) {
      astError( AST__BADBX, "astInitPlot3D(%s): The plotting volume has undefined limits "
                "in the graphics world coordinate system.", status, name );
   }

/* Check that no dimension of the graphbox is zero. */
   if( ( graphbox[ 3 ] == graphbox[ 0 ] ||
         graphbox[ 4 ] == graphbox[ 1 ] ||
         graphbox[ 5 ] == graphbox[ 2 ] ) && astOK ){
      astError( AST__BADBX, "astInitPlot3D(%s): The plotting volume has zero size "
                "in the graphics world coordinate system.", status, name );
   }

/* Check that no dimension of the basebox is bad. */
   if( astISBAD(basebox[0]) || astISBAD(basebox[1]) ||
       astISBAD(basebox[2]) || astISBAD(basebox[3]) ||
       astISBAD(basebox[4]) || astISBAD(basebox[5]) ) {
      astError( AST__BADBX, "astInitPlot3D(%s): The limits of "
                "the %s are undefined or bad.", status, name, name );
   }

/* Create a Frame which describes the graphics world coordinate system. */
   graphicsframe = astFrame( 3, "Domain=GRAPHICS,Title=Graphical Coordinates", status );

/* Initialise a 2D Plot structure (the parent class) as the first component
   within the Plot3D structure, allocating memory if necessary. We supply
   dummy arguments since we will not be using the parent Plot class to
   draw anything. We supply a NULL vtab pointer so that methods defined by
   the Plot class will be used during the construction of the Plot3D. Once
   the Plot3D is fully constructed, we will use astSetVtab to establish
   the correct vtab. */
   new = (AstPlot3D *) astInitPlot( mem, size, 0, NULL, name, NULL, fjunkbox,
                                    djunkbox );
   if ( astOK ) {

/* Initialise the Plot3D data. */
/* ----------------------------- */

/* Remove all Frames from the parent FrameSet except for the base (2D graphics)
   Frame (we leave this last Frame since an error is reported if the last
   Frame is removed from a FrameSet). We do this by repeatedly removing the
   first Frame, causing all remaining Frames to have their index reduced by 1.
   When the base Frame arrives at index 1, we skip it and start removing the
   second frame instead. */
      nfrm = astGetNframe( new );
      i = 1;
      for( ii = 0; ii < nfrm; ii++ ) {
         if( i > 1 || astGetBase( new ) != 1 ) {
            astRemoveFrame( new, i );
         } else {
            i = 2;
         }
      }

/* Add in the 3D graphics Frame, using a PermMap to connect it to the
   2D graphics Frame. */
      map = (AstMapping *) astPermMap( 2, NULL, 3, NULL, NULL, "", status );
      astAddFrame( new, 1, map, graphicsframe );
      map = astAnnul( map );

/* And remove the 2D GRAPHICS Frame, leaving just the 3D GRAPHICS Frame
   in the FrameSet, with index 1. */
      astRemoveFrame( new, 1 );

/* Get the index of the current (physical) and base (pixel) Frames in
   the supplied FrameSet. */
      bi = astGetBase( fset );
      ci = astGetCurrent( fset );

/* Temporarily set the current Frame to be the pixel frame. */
      astSetCurrent( fset, bi );

/* Get a double precision version of "graphbox", and store it in the
   Plot3D structure. */
      new->gbox[ 0 ] = (double) graphbox[ 0 ];
      new->gbox[ 1 ] = (double) graphbox[ 1 ];
      new->gbox[ 2 ] = (double) graphbox[ 2 ];
      new->gbox[ 3 ] = (double) graphbox[ 3 ];
      new->gbox[ 4 ] = (double) graphbox[ 4 ];
      new->gbox[ 5 ] = (double) graphbox[ 5 ];

/* The base Frame of the supplied FrameSet is mapped linearly onto the
   graphics frame. Create a WinMap that maps the base box (within the
   base Frame of the supplied FrameSet) onto the graphics box. */
      map = (AstMapping *) astWinMap( 3, new->gbox, new->gbox + 3, basebox,
                                      basebox + 3, "", status );

/* Add the supplied FrameSet into the Plot3D (i.e. FrameSet) created
   earlier. This leaves the graphics frame with index 1 in the
   returned  Plot3D. */
      astAddFrame( (AstFrameSet *) new, 1, map, fset );
      map = astAnnul( map );

/* Set the current Frame in the Plot to be the physical coordinate Frame
   (with index incremented by one because the graphics Frame has been added). */
      astSetCurrent( (AstFrameSet *) new, ci + 1 );

/* Note the index of the original base Frame in the Plot3D FrameSet */
      new->pix_frame = bi + 1;

/* Re-establish the original current Frame in the supplied FrameSet. */
      astSetCurrent( fset, ci );

/* Initialise the Plot pointers. */
      new->plotxy = NULL;
      new->plotxz = NULL;
      new->plotyz = NULL;

/* Initialise other attributes */
      new->rootcorner = -1;

/* Initialise the normal vector to the plane used by astText and astMark. */
      new->norm[ 0 ] = AST__BAD;
      new->norm[ 1 ] = AST__BAD;
      new->norm[ 2 ] = AST__BAD;

/* Create three 2D Plots to describe the three planes in the cube. */
      CreatePlots( new, fset, graphbox, basebox, status );

/* Ensure that attempts to use the graphics interface of the parent
   Plot structure get forwarded to the relevant 3D routines defined in
   this class. */
      astGrfSet( new, "Attr", (AstGrfFun) Attr3D );
      astSetGrf( new, 1 );

/* Change the virtual function table stored in the new Plot3D, from the Plot
   vtab (established when astINitPlot was called above), to the supplied
   vtab. */
      if( vtab ) astSetVtab( new, vtab );

/* Ensure that these Plots use the grf functions defined by this class
   (Plot3D). This means that whenever a Plot draws anything, it will use
   the appropriate grf function defined in this class to do the drawing.
   The grf functions defined in this class, convert the grf call into a
   grf3D call apprpriate the plane spanned by the Plot. */
      Set3DGrf( new, new->plotxy, XY, status );
      Set3DGrf( new, new->plotxz, XZ, status );
      Set3DGrf( new, new->plotyz, YZ, status );

/* Set up the Edges attributes in the encapsulated Plots so that labels
   appear on the requited edges. Initially, the root corner is "LLL"
   (i.e. the lower bound on every axis). */
      ChangeRootCorner( new, 0, 0, status );
   }

/* Annul the frame. */
   graphicsframe = astAnnul( graphicsframe );

/* If an error occurred, clean up by deleting the new object. */
   if ( !astOK ) new = astDelete( new );

/* Annul the pointer to the base Frame and FrameSet. */
   baseframe = astAnnul( baseframe );
   fset = astAnnul( fset );

/* Return a pointer to the new object. */
   return new;
}


AstPlot3D *astLoadPlot3D_( void *mem, size_t size, AstPlot3DVtab *vtab,
                           const char *name, AstChannel *channel, int *status ) {
/*
*+
*  Name:
*     astLoadPlot3D

*  Purpose:
*     Load a Plot3D.

*  Type:
*     Protected function.

*  Synopsis:
*     #include "plot3d.h"
*     AstPlot3D *astLoadPlot3D( void *mem, size_t size,
*                               AstPlot3DVtab *vtab,
*                               const char *name, AstChannel *channel )

*  Class Membership:
*     Plot3D loader.

*  Description:
*     This function is provided to load a new Plot3D using data read
*     from a Channel. It first loads the data used by the parent class
*     (which allocates memory if necessary) and then initialises a
*     Plot3D structure in this memory, using data read from the
*     input Channel.

*  Parameters:
*     mem
*        A pointer to the memory into which the Plot3D is to be
*        loaded.  This must be of sufficient size to accommodate the
*        Plot3D data (sizeof(Plot3D)) plus any data used by
*        derived classes. If a value of NULL is given, this function
*        will allocate the memory itself using the "size" parameter to
*        determine its size.
*     size
*        The amount of memory used by the Plot3D (plus derived class
*        data).  This will be used to allocate memory if a value of
*        NULL is given for the "mem" parameter. This value is also
*        stored in the Plot3D structure, so a valid value must be
*        supplied even if not required for allocating memory.
*
*        If the "vtab" parameter is NULL, the "size" value is ignored
*        and sizeof(AstPlot3D) is used instead.
*     vtab
*        Pointer to the start of the virtual function table to be
*        associated with the new Plot3D. If this is NULL, a pointer
*        to the (static) virtual function table for the Plot3D class
*        is used instead.
*     name
*        Pointer to a constant null-terminated character string which
*        contains the name of the class to which the new object
*        belongs (it is this pointer value that will subsequently be
*        returned by the astGetClass method).
*
*        If the "vtab" parameter is NULL, the "name" value is ignored
*        and a pointer to the string "Plot3D" is used instead.

*  Returned Value:
*     A pointer to the new Plot3D.

*  Notes:
*     - A null pointer will be returned if this function is invoked
*     with the global error status set, or if it should fail for any
*     reason.
*-
*/

/* Local Constants: */
#define KEY_LEN 50               /* Maximum length of a keyword */

/* Local Variables: */
   astDECLARE_GLOBALS
   AstPlot3D *new;
   char key[ KEY_LEN + 1 ];
   char *text;
   int axis;
   int i;

/* Initialise. */
   new = NULL;

/* Check the global error status. */
   if( !astOK ) return new;

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(channel);

/* If a NULL virtual function table has been supplied, then this is
   the first loader to be invoked for this Plot3D. In this case the
   Plot3D belongs to this class, so supply appropriate values to be
   passed to the parent class loader (and its parent, etc.). */
   if ( !vtab ) {
      size = sizeof( AstPlot3D );
      vtab = &class_vtab;
      name = "Plot3D";

/* If required, initialise the virtual function table for this class. */
      if ( !class_init ) {
         astInitPlot3DVtab( vtab, name );
         class_init = 1;
      }
   }

/* Allocate memory to hold the new Object. We allocate it now rather than
   waiting for astInitObject to allocate it so that we can pass a NULL
   vtab pointer to the Plot loader, thus causing the Plot loader to use the
   function implementations provided by the Plot class rather than those
   provided by the class being instantiated. */
   if( !mem ) mem = astMalloc( size );

/* Invoke the parent class loader to load data for all the ancestral
   classes of the current one, returning a pointer to the resulting
   partly-built Plot3D. Pass a NULL vtab pointer so that the "new" object
   will be loaded using Plot methods rather than than Plot3D methods.
   This is important because the implementations provided by the Plot3D
   class for the Plot attribute accessors require the existence of the
   encapsulated Plots held within the Plot3D, but these have not yet been
   created. */
   new = astLoadPlot( mem, size, NULL, name, channel );
   if ( astOK ) {

/* Now modify the new object to use the supplied vtab. */
      astSetVtab( new, vtab );

/* Read input data. */
/* ================ */
/* Request the input Channel to read all the input data appropriate to
   this class into the internal "values list". */
      astReadClassData( channel, "Plot3D" );

/* Now read each individual data item from this list and use it to
   initialise the appropriate instance variable(s) for this class. */

/* In the case of attributes, we first read the "raw" input value,
   supplying the "unset" value as the default. If a "set" value is
   obtained, we then use the appropriate (private) Set... member
   function to validate and set the value properly. */

/* Norm. */
/* ----- */
      for( axis = 0; axis < 3; axis++ ) {
         (void) sprintf( key, "norm%d", axis + 1 );
         new->norm[ axis ] = astReadDouble( channel, key, AST__BAD );
         if( TestNorm( new, axis, status ) ) SetNorm( new, axis, new->norm[ axis ], status );
      }

/* RootCorner. */
/* ----------- */
      text = astReadString( channel, "rootcn", " " );
      if( astOK && strcmp( text, " " ) ) {
         new->rootcorner = RootCornerInt( text, status );
         if( new->rootcorner < 0 && astOK ) {
            astError( AST__INTER, "astRead(Plot3D): Corrupt Plot3D contains "
                      "invalid RootCorner attribute value (%s).", status, text );
         }
      } else {
         new->rootcorner = -1;
      }
      if( TestRootCorner( new, status ) ) SetRootCorner( new, new->rootcorner, status );
      text = astFree( text );

/* Labelled axes */
      new->axis_plot1[0] = astReadInt( channel, "axplx1", -1 );
      new->axis_plot1[1] = astReadInt( channel, "axply1", -1 );
      new->axis_plot1[2] = astReadInt( channel, "axplz1", -1 );

      new->axis_index1[0] = astReadInt( channel, "axinx1", -1 );
      new->axis_index1[1] = astReadInt( channel, "axiny1", -1 );
      new->axis_index1[2] = astReadInt( channel, "axinz1", -1 );

/* Unlabelled axes */
      new->axis_plot2[0] = astReadInt( channel, "axplx2", -1 );
      new->axis_plot2[1] = astReadInt( channel, "axply2", -1 );
      new->axis_plot2[2] = astReadInt( channel, "axplz2", -1 );

      new->axis_index2[0] = astReadInt( channel, "axinx2", -1 );
      new->axis_index2[1] = astReadInt( channel, "axiny2", -1 );
      new->axis_index2[2] = astReadInt( channel, "axinz2", -1 );

/* Plot that spans two connected 3D axes. */
      new->baseplot = astReadInt( channel, "basepl", -1 );

/* XY Plot */
      new->plotxy = astReadObject( channel, "plotxy", NULL );

/* XZ Plot */
      new->plotxz = astReadObject( channel, "plotxz", NULL );

/* YZ Plot */
      new->plotyz = astReadObject( channel, "plotyz", NULL );

/* The index within the Plot3D FrameSet, of the original base Frame in
   the FrameSet supplied when the Plot3D was constructed. */
      new->pix_frame = astReadInt( channel, "pixfrm", AST__NOFRAME );

/* Ensure that these Plots use the grf functions defined by this class
   (Plot3D). This means that whener a Plot draws anything, it will use
   the appropriate grf function defined in this class to do the drawing.
   The grf functions defined in this class, convert the grf call into a
   grf3D call apprpriate the plane spanned by the Plot. */
      Set3DGrf( new, new->plotxy, XY, status );
      Set3DGrf( new, new->plotxz, XZ, status );
      Set3DGrf( new, new->plotyz, YZ, status );

/* For attributes of the parent Plot class will have been loaded
   each attribute that has a set value in the parent Plot structure,
   re-set the value so that it gets copied to the copy the  to the
   encapsulated Plots. First do axis specific attributes. */

#define COPY_ATTR(attr,nval) \
   for( i = 0; i < nval; i++ ) { \
      if( astTest##attr(new,i) ) astSet##attr(new,i,astGet##attr(new,i)); \
   }

   COPY_ATTR(MinTick,3)
   COPY_ATTR(Abbrev,3)
   COPY_ATTR(Gap,3)
   COPY_ATTR(LogGap,3)
   COPY_ATTR(LogPlot,3)
   COPY_ATTR(LogTicks,3)
   COPY_ATTR(LogLabel,3)
   COPY_ATTR(LabelUp,3)
   COPY_ATTR(DrawAxes,3)
   COPY_ATTR(LabelUnits,3)
   COPY_ATTR(MinTickLen,3)
   COPY_ATTR(MajTickLen,3)
   COPY_ATTR(NumLab,3)
   COPY_ATTR(NumLabGap,3)
   COPY_ATTR(TextLab,3)
   COPY_ATTR(TextLabGap,3)

   COPY_ATTR(Style,AST__NPID)
   COPY_ATTR(Font,AST__NPID)
   COPY_ATTR(Colour,AST__NPID)
   COPY_ATTR(Width,AST__NPID)
   COPY_ATTR(Size,AST__NPID)

#undef COPY_ATTR

/* Now do attributes that are not axis specific. */

#define COPY_ATTR(attr) \
   if( astTest##attr(new) ) astSet##attr(new,astGet##attr(new));

   COPY_ATTR(Ink)
   COPY_ATTR(Tol)
   COPY_ATTR(Invisible)
   COPY_ATTR(TickAll)
   COPY_ATTR(ForceExterior)
   COPY_ATTR(Border)
   COPY_ATTR(Clip)
   COPY_ATTR(ClipOp)
   COPY_ATTR(Escape)
   COPY_ATTR(Grid)
   COPY_ATTR(Labelling)

#undef COPY_ATTR

/* If an error occurred, clean up by deleting the new Plot3D. */
      if ( !astOK ) new = astDelete( new );
   }

/* Return the new Plot3D pointer. */
   return new;

/* Undefine macros local to this function. */
#undef KEY_LEN
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

void astClearRootCorner_( AstPlot3D *this, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Plot3D,ClearRootCorner))( this, status );
}

void astSetRootCorner_( AstPlot3D *this, int value, int *status ) {
   if ( !astOK ) return;
   (**astMEMBER(this,Plot3D,SetRootCorner))( this, value, status );
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
AstPlot3D *astPlot3DId_( void *frame_void, const float graphbox[6],
                         const double basebox[6], const char *, ... );

/* Special interface function implementations. */
/* ------------------------------------------- */
AstPlot3D *astPlot3DId_( void *frame_void, const float graphbox[6],
                         const double basebox[6], const char *options, ... ) {
/*
*++
*  Name:
c     astPlot3D
f     AST_PLOT3D

*  Purpose:
*     Create a Plot3D.

*  Type:
*     Public function.

*  Synopsis:
c     #include "plot3d.h"
c     AstPlot3D *astPlot3D( AstFrame *frame, const float graphbox[ 6 ],
c                           const double basebox[ 6 ], const char *options, ... )
f     RESULT = AST_PLOT3D( FRAME, GRAPHBOX, BASEBOX, OPTIONS, STATUS )

*  Class Membership:
*     Plot3D constructor.

*  Description:
*     This function creates a new Plot3D and optionally initialises
*     its attributes.
*
*     A Plot3D is a specialised form of Plot that provides facilities
*     for producing 3D graphical output.

*  Parameters:
c     frame
f     FRAME = INTEGER (Given)
*        Pointer to a Frame describing the physical coordinate system
*        in which to plot. A pointer to a FrameSet may also be given,
*        in which case its current Frame will be used to define the
*        physical coordinate system and its base Frame will be mapped
*        on to graphical coordinates (see below).
*
*        If a null Object pointer (AST__NULL) is given, a default
*        3-dimensional Frame will be used to describe the physical
*        coordinate system. Labels, etc. may then be attached to this
*        by setting the appropriate Frame attributes
*        (e.g. Label(axis)) for the Plot.
c     graphbox
f     GRAPHBOX( 6 ) = REAL (Given)
*        An array giving the position and extent of the plotting volume
*        (within the plotting space of the underlying graphics system)
*        in which graphical output is to appear. This must be
*        specified using graphical coordinates appropriate to the
*        underlying graphics system.
*
*        The first triple of values should give the coordinates of the
*        bottom left corner of the plotting volume and the second triple
*        should give the coordinates of the top right corner. The
*        coordinate on the horizontal axis should be given first in
*        each pair. Note that the order in which these points are
*        given is important because it defines up, down, left and
*        right for subsequent graphical operations.
c     basebox
f     BASEBOX( 6 ) = DOUBLE PRECISION (Given)
*        An array giving the coordinates of two points in the supplied
*        Frame (or in the base Frame if a FrameSet was supplied) which
*        correspond to the bottom left and top right corners of the
*        plotting volume, as specified above. This range of coordinates
*        will be mapped linearly on to the plotting area. The
*        coordinates should be given in the same order as above.
c     options
f     OPTIONS = CHARACTER * ( * ) (Given)
c        Pointer to a null-terminated string containing an optional
c        comma-separated list of attribute assignments to be used for
c        initialising the new Plot3D. The syntax used is identical to
c        that for the astSet function and may include "printf" format
c        specifiers identified by "%" symbols in the normal way.
c        If no initialisation is required, a zero-length string may be
c        supplied.
f        A character string containing an optional comma-separated
f        list of attribute assignments to be used for initialising the
f        new Plot3D. The syntax used is identical to that for the
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
c     astPlot3D()
f     AST_PLOT3D = INTEGER
*        A pointer to the new Plot3D.

*  Notes:
*     - The base Frame of the returned Plot3D will be a new Frame which
*     is created by this function to represent the coordinate system
*     of the underlying graphics system (graphical coordinates). It is
*     given a Frame index of 1 within the Plot3D. The choice of base
*     Frame (Base attribute) should not, in general, be changed once a
*     Plot3D has been created (although you could use this as a way of
*     moving the plotting area around on the plotting surface).
c     - If a Frame is supplied (via the "frame" pointer), then it
f     - If a Frame is supplied (via the FRAME pointer), then it
*     becomes the current Frame of the new Plot3D and is given a Frame
*     index of 2.
c     - If a FrameSet is supplied (via the "frame" pointer), then
f     - If a FrameSet is supplied (via the FRAME pointer), then
*     all the Frames within this FrameSet become part of the new Plot3D
*     (where their Frame indices are increased by 1), with the
*     FrameSet's current Frame becoming the current Frame of the Plot3D.
*     - If a null Object pointer (AST__NULL) is supplied (via the
c     "frame" pointer), then the returned Plot3D will contain two
f     FRAME pointer), then the returned Plot3D will contain two
*     Frames, both created by this function. The base Frame will
*     describe graphics coordinates (as above) and the current Frame
*     will be a basic Frame with no attributes set (this will
*     therefore give default values for such things as the Plot3D Title
*     and the Label on each axis). Physical coordinates will be mapped
*     linearly on to graphical coordinates.
*     - An error will result if the Frame supplied (or the base Frame
*     if a FrameSet was supplied) is not 3-dimensional.
*     - A null Object pointer (AST__NULL) will be returned if this
c     function is invoked with the AST error status set, or if it
f     function is invoked with STATUS set to an error value, or if it
*     should fail for any reason.
*--

*  Implementation Notes:
*     - This function implements the external (public) interface to
*     the astPlot3D constructor function. It returns an ID value
*     (instead of a true C pointer) to external users, and must be
*     provided because astPlot3D_ has a variable argument list which
*     cannot be encapsulated in a macro (where this conversion would
*     otherwise occur).
*     - Because no checking or casting of arguments is performed
*     before the function is invoked, the "frame" parameter is of type
*     (void *) and is converted from an ID value to a pointer and
*     validated within the function itself.
*     - The variable argument list also prevents this function from
*     invoking astPlot3D_ directly, so it must be a
*     re-implementation of it in all respects, except for the final
*     conversion of the result to an ID value.
*/

/* Local Variables: */
   astDECLARE_GLOBALS            /* Pointer to thread-specific global data */
   AstFrame *frame;              /* Pointer to Frame structure */
   AstPlot3D *new;               /* Pointer to new Plot3D */
   va_list args;                 /* Variable argument list */
   int *status;                  /* Pointer to inherited status value */

/* Get a pointer to the thread specific global data structure. */
   astGET_GLOBALS(NULL);

/* Get a pointer to the inherited status value. */
   status = astGetStatusPtr;

/* Check the global status. */
   if ( !astOK ) return NULL;

/* Initialise variables to avoid "used of uninitialised variable"
   messages from dumb compilers. */
   new = NULL;

/* Obtain a Frame pointer from any ID supplied and validate the
   pointer to ensure it identifies a valid Frame. */
   if( frame_void ){
      frame = astVerifyFrame( astMakePointer( frame_void ) );
   } else {
      frame = NULL;
   }

/* Check the pointer can be used. */
   if ( astOK ) {

/* Initialise the Plot3D, allocating memory and initialising the
   virtual function table as well if necessary. */
      new = astInitPlot3D( NULL, sizeof( AstPlot3D ), !class_init,
                           &class_vtab, "Plot3D", frame, graphbox,
                           basebox );

/* If successful, note that the virtual function table has been
   initialised. */
      if ( astOK ) {
         class_init = 1;

/* Obtain the variable argument list and pass it along with the
   options string to the astVSet method to initialise the new
   Plot3D's attributes. */
         va_start( args, options );
         astVSet( new, options, NULL, args );
         va_end( args );

/* If an error occurred, clean up by deleting the new object. */
         if ( !astOK ) new = astDelete( new );
      }
   }

/* Return an ID value for the new Plot3D. */
   return astMakeId( new );

}



/* Macros that define method to override the methods of the Plot class
   that are not currently implemented by this class. They just report an
   error if called. */

#define METHOD1(name) \
static void name(ARGLIST,int *status){ \
   if( !astOK ) return; \
   astError( AST__INTER, "ast##name(%s): The ast##name " \
             "method cannot be used with a %s (programming error).", status,  \
             astGetClass( this ), astGetClass( this ) ); \
}

#define METHOD2(name,rettype,retval) \
static rettype name(ARGLIST,int *status){ \
   if( !astOK ) return retval; \
   astError( AST__INTER, "ast##name(%s): The ast##name " \
             "method cannot be used with a %s (programming error).", status,  \
             astGetClass( this ), astGetClass( this ) ); \
   return retval; \
}


#define ARGLIST AstPlot *this
METHOD2(GetGrfContext,AstKeyMap *,NULL)
#undef ARGLIST

#define ARGLIST AstPlot *this
METHOD1(GrfPop)
#undef ARGLIST

#define ARGLIST AstPlot *this
METHOD1(GrfPush)
#undef ARGLIST

#define ARGLIST AstPlot *this, const char *name, AstGrfFun fun
METHOD1(GrfSet)
#undef ARGLIST

#define ARGLIST AstPlot *this, int axis, const double start[], double length
METHOD1(GridLine)
#undef ARGLIST

#define ARGLIST AstPlot *this, float lbnd[2], float ubnd[2]
METHOD1(BoundingBox)
#undef ARGLIST

#define ARGLIST AstPlot *this, int iframe, const double lbnd[], const double ubnd[]
METHOD1(Clip)
#undef ARGLIST

#define ARGLIST AstPlot *this, const double start[], const double finish[]
METHOD1(Curve)
#undef ARGLIST

#define ARGLIST AstPlot *this, AstMapping *map
METHOD1(GenCurve)
#undef ARGLIST








