#include <dx/dx.h>

static Error DoSXEnum( Object o, char *name, char *dep );

Error m_SXEnum( Object *in, Object*out){
/*
*+
*  Name:
*     SXEnum

*  Purpose:
*     enumerating the positions or connections in a field

*  Language:
*     ANSI C

*  Syntax:
*     output = SXEnum( input, name, dep );

*  Classification:
*     Realisation

*  Description:
*     The SXEnum module creates the component specified by "name", and adds
*     it to the "output" field. The values in the new component start at zero
*     and increment by one for each position or connection in the field.
*
*     The new component is in one-to-one correspondance with either the
*     "positions" or "connections" component, dependant on "dep".

*  Parameters:
*     input = field (Given)
*        input field [none]
*     name = string (Given)
*        name of component to store the enumeration ["data"]
*     dep = string (Given)
*        object to be enumerated; "positions" or "connections" ["positions"]
*     output = field (Returned)
*        output field

*  Components:
*     Adds a component with the given "name", deleting any existing
*     component. All other components are copied from the "input" field.

*  Examples:
*     In this example, the 17th frame is extracted from a data set
*     containing scattered data, and a field created holding only those
*     positions with offsets between 10 and 20.
*
*        input = Import("/usr/lpp/dx/samples/data/CO2.general");
*        frame17 = Select(input,17);
*        enum = SXEnum(frame17,"index","positions");
*        marked = Mark(enum,"index");
*        included = Include(marked,10,20,1);
*        subset = Unmark(included,"index");

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Local Variables. */

      char   *dep;
      char   *name;
      Object  o=NULL;


/*  Check that the "input" object has been supplied. */

      if( !in[0] ){
         DXSetError( ERROR_BAD_PARAMETER, "missing parameter \"input\".");
         goto error;
      }


/*  Get a value for the "name" object. */

      if( !in[1] ){
         name = "data";

      } else {
         name = DXGetString( (String) in[1] );
         if( !name ){
            DXSetError( ERROR_BAD_PARAMETER, "unable to get string parameter \"name\".");
            goto error;
         }
      }


/*  Get a value for the "dep" object. */

      if( !in[2] ){
         dep = "positions";

      } else {
         dep = DXGetString( (String) in[2] );
         if( !dep ){
            DXSetError( ERROR_BAD_PARAMETER, "unable to get string parameter \"dep\".");
            goto error;
         }

         if( strcmp( dep, "positions" ) && strcmp( dep, "connections" ) ){
            DXSetError( ERROR_BAD_PARAMETER, "bad value (\"%s\") obtained for parameter \"dep\".",dep );
            goto error;
         }

      }


/*  Get a modifiable copy of the "input" object. */

      o = (Object) DXCopy( in[0], COPY_STRUCTURE );
      if( !o ) goto error;


/*  Enumerate it. */

      if( !DoSXEnum( o, name, dep ) ) goto error;


/*  Return the output object. */

      out[0] = o;
      return( OK );

error:
      DXDelete( o );
      return( ERROR );

}

static Error DoSXEnum( Object o, char *name, char *dep ){

      Array a;
      int n, i, *to;
      Object oo;


/*  If the supplied object is a field... */

      switch( DXGetObjectClass( o ) ){
      case CLASS_FIELD:


/*  See how many items there are in the requested component. */

         a = (Array) DXGetComponentValue( (Field) o, dep );
         if( !a ) {
            DXSetError( ERROR_DATA_INVALID, "field has no \"%s\" component", dep );
            return( ERROR );
         }

         DXGetArrayInfo( a, &n, NULL, NULL, NULL, NULL );


/*  Create a new array to hold the enumeration, and get a pointer to it. */

         a = (Array) DXNewArray( TYPE_INT, CATEGORY_REAL, 0 );
         if( !DXAddArrayData( a, 0, n, NULL ) ) return( ERROR );
         to = (int *) DXGetArrayData( a );


/*  Add this new array to the field. */

         DXSetComponentValue( (Field) o, name, (Object) a );


/*  Store a value for the "dep" attribute of the new array. */

         DXSetComponentAttribute( (Field) o, name, "dep", (Object) DXNewString( dep ) );


/*  Store the enumeration values. */

         for( i=0; i<n; i++ ) *(to++) = i;


/*  Indicate that the component values have changed, and complete the
 *  output field. */

         DXChangedComponentValues( (Field) o, name );
         if( !DXEndField( (Field) o ) ) return( ERROR );

         break;


/*  If the supplied object is a group, call this function recursively for
 *  each member of the group. */

      case CLASS_GROUP:

         for( i=0; oo=(Object)DXGetEnumeratedMember((Group)o,i,NULL); i++ ){
            if( !DoSXEnum( oo, name, dep ) ) return( ERROR );
         }

         break;

      }

      return( OK );

}
