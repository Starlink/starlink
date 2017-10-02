/*
*  Name:
*     ndg_votutils.c

*  Purpose:
*     Provides various VOTABLE utilities.

*  Type of Module:
*     C source file.

*  Description:
*     Provides various VOTABLE utilities. NDG is probably not the right
*     place for this module, but it is hard to see where else to put it
*     since it is needed by ndg_provenance.c.

*  Functions Provides:
*     This modules provides the following public functions.
*
*     - ndgHds2vot: Create a VOTABLE representation of an HDS object.
*     - ndgPutGroup: Create a new GROUP element.
*     - ndgPutParam: Create a new multi-dimensional PARAM element.
*     - ndgPutParam0: Create a new scalar PARAM element.
*     - ndgVot2hds: Restore an HDS object from its VOTABLE representation.
*     - ndgGetAttrib: Get the value of an attribute as a string.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (JAC, Hawaii)

*  History:
*     14-JUL-2009 (DSB):
*        Original version.
*/

/* Starlink include files */
#include "star/hds.h"
#include "ndf.h"
#include "ndg.h"
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

/* System include files */
#include <string.h>
#include <ctype.h>

/* Templates for private functions */
static HDSLoc *ndg1Vot2hds( AstXmlElement *, HDSLoc *, int, int * );



/* Public Functions: */
/* --------------------------------------------------------------------- */

const char *ndgGetAttrib( AstXmlElement *elem, const char *name,
                          const char *method, int *status ){
/*
*+
*  Name:
*     ndgGetAttrib

*  Purpose:
*     Get the value of an attribute as a string.

*  Invocation:
*     const char *ndgGetAttrib( AstXmlElement *elem, const char *name,
*                               const char *method, int *status )

*  Description:
*     This function returns a pointer to a string holding the value of a
*     named attribute within the given element. An error is reported if
*     the attribute does not exist.

*  Arguments:
*     elem
*        A pointer to the XmlElement.
*     name
*        The name of the attribute.
*     method
*        The name of calling puplic method for inclusion in error messages.
*     status
*        The inherited status.

*  Returned Value:
*     A pointer to the attribute string value, or NULL if an error occurs.

*-
*/

/* Local Varianles: */
   const char *result = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Get the attribute value. */
   result = astXmlGetAttributeValue( elem, name );

/* Report an error if the attribute was not found. */
   if( !result && *status == SAI__OK ) {
      msgSetc( "EN", astXmlGetName( (AstXmlObject *) elem ) );
      msgSetc( "AN", name );
      msgSetc( "M", method );
      *status = SAI__ERROR;
      errRep( "", "^M: The supplied <^EN> element has no '^AN' attribute.",
              status );
   }

/* Return a pointer to the attribute value. */
   return result;
}

AstXmlElement *ndgHds2vot( const HDSLoc *loc, AstXmlElement *elem,
                           int *status ){
/*
*+
*  Name:
*     ndgHds2vot

*  Purpose:
*     Create a VOTABLE representation of an HDS object.

*  Invocation:
*     AstXmlElement *ndgHds2vot( const HDSLoc *loc, AstXmlElement *elem,
*                                int *status )

*  Description:
*     This function stores the supplied HDS object as a new child element
*     within the supplied XmlElement. The HDS object can be re-constructed
*     from the element using ndgVot2hds.
*
*     If the HDS object is a primitive, it is represented by a <PARAM>
*     element. If it is a structure, it is represented by a <GROUP>
*     element containing other GROUPS and/or PARAMS. In both cases the
*     name of the HDS object is used as the "name" attribute for the
*     PARAM or GROUP element, and the HDS type of the supplied object is
*     used as the "utype" attribute (with a name-space prefix of
*     "hds_type").
*
*     HDS multi-dimensional arrays are handled, but each element is
*     accessed individually using datCell. So storing large HDS arrays
*     using this function will be very inefficient.

*  Arguments:
*     loc
*        A locator for the HDS object.
*     elem
*        A pointer to the XmlElement in which the HDS Object is to be
*        stored. NULL should be supplied if the new element is not to be
*        stored in an existing element (a pointer to the new element is
*        still returned as the function value).
*     status
*        The inherited status.

*  Returned Value:
*     A pointer to the new element.

*-
*/

/* Local Varianles: */
   AstXmlElement *result = NULL;
   HDSLoc *loc2 = NULL;
   HDSLoc *loc3 = NULL;
   char *cbuf;
   char *vbuf;
   char name[ DAT__SZNAM + 1 ];
   char type[ DAT__SZTYP + 1 ];
   char utype[ DAT__SZTYP + 10 ];
   char utype2[ DAT__SZTYP + 12 + NDF__MXDIM*11 ];
   const char *votype;
   hdsdim dim[ NDF__MXDIM + 1 ];
   hdsdim icell;
   int i;
   int icomp;
   int is_char;
   int nc;
   int ncomp;
   int ndim;
   int prim;
   int store_utype;
   size_t clen;
   size_t ncell;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Get the name and data type for the supplied HDS object. */
   datName( loc, name, status );
   datType( loc, type, status );

/* Turn the HDS type into a utype by prepending a name space. */
   sprintf( utype, "hds_type:%s", type );

/* See if the supplied HDS object is primitive. */
   datPrim( loc, &prim, status );

/* Get the shape of the object. */
   datShape( loc, NDF__MXDIM, dim, &ndim, status );

/* Initialise */
   clen = 0;
   is_char = 0;
   store_utype = 0;

/* First deal with primitives. */
   if( prim ) {

/* Get the formatted length of a single primitive data value. */
      datClen( loc, &clen, status );

/* Select the smallest VOTABLE data type that can be used to represent
   the HDS data type. Set a flag indicating if this is different to the
   HDS data type (in which case the HDS type will be included in the XML
   as the "utype" attribute). */
      if( !strcmp( type, "_INTEGER" ) ) {
         votype = "int";
         store_utype = 1;

      } else if( !strcmp( type, "_REAL" ) ) {
         votype = "float";

      } else if( !strcmp( type, "_DOUBLE" ) ) {
         votype = "double";

      } else if( !strcmp( type, "_LOGICAL" ) ) {
         votype = "boolean";

      } else if( !strncmp( type, "_CHAR", 5 ) ) {
         votype = "char";
         is_char = 1;

      } else if( !strcmp( type, "_WORD" ) ) {
         votype = "short";
         store_utype = 1;

      } else if( !strcmp( type, "_UWORD" ) ) {
         votype = "int";
         store_utype = 1;

      } else if( !strcmp( type, "_BYTE" ) ) {
         votype = "short";
         store_utype = 1;

      } else if( !strcmp( type, "_UBYTE" ) ) {
         votype = "unsignedByte";

      } else if( *status == SAI__OK ) {
         msgSetc( "T", type );
         *status = SAI__ERROR;
         errRep( "", "ndgHds2vot: Unknown HDS data type: '^T' (programming "
                 "error).", status );
      }

/* Allocate a buffer to hold a single formatted value */
      cbuf = astMalloc( sizeof( char )*( clen + 1 ) );

/* Scalars */
      if( ndim == 0 ) {
         datGet0C( loc, cbuf, clen + 1, status );
         result = ndgPutParam0( elem, name, votype, cbuf, status );
         if( store_utype ) astXmlAddAttr( result, "utype", utype, NULL );

/* Arrays. */
      } else {

/* Turn the object into a one-dimensional vector of cells, and get the
   length of the vector. */
         datVec( loc, &loc2, status );
         datSize( loc2, &ncell, status );

/* Indicate we do not yet have a buffer for the vector of formatted
   values. */
         vbuf = NULL;
         nc = 0;

/* Loop round all elements. */
         for( icell = 1; icell <= ncell; icell++ ) {

/* Get the cell value as a character string. */
            datCell( loc2, 1, &icell, &loc3, status );
            datGet0C( loc3, cbuf, clen + 1, status );
            datAnnul( &loc3, status );

/* For strings, ensure the string is padded with spaces to its full size. */
            if( is_char ) {
               for( i = strlen( cbuf ); i < clen; i++ ) cbuf[ i ] = ' ';
               cbuf[ i ] = 0;
            }

/* Append it to the buffer holding all cell values, and then append an
   extra space (unless we are dealing with an array of strings). */
            vbuf = astAppendString( vbuf, &nc, cbuf );
            if( !is_char ) vbuf = astAppendString( vbuf, &nc, " " );
         }

/* Remove any trailing separator. */
         if( !is_char ) vbuf[ nc - 1 ] = 0;

/* If this is an array of (fixed-length) strings, store the length of each
   string as the first dimension, and shuffle all other dimensions down
   one space. */
         if( is_char ) {
            for( i = ndim; i > 0; i-- ) dim[ i ] = dim[ i - 1 ];
            dim[ 0 ] = clen;
            ndim++;
         }

/* Add a multi-dimensional PARAM element into the parent element, using
   the HDS name as the "name" attribute. The HDS type is stored in a
   ficticious utype value */
         result = ndgPutParam( elem, name, ndim, dim, votype, vbuf, status );
         if( store_utype ) astXmlAddAttr( result, "utype", utype, NULL );

/* Free resources */
         datAnnul( &loc2, status );
         vbuf = astFree( vbuf );
      }
      cbuf = astFree( cbuf );

/* Now deal with structures. */
   } else {

/* Scalars... */
      if( ndim == 0 ) {

/* Add a GROUP element into the supplied parent element. This is used to
   collect together the components in the HDS structure. The HDS type is
   stored as the utype of the GROUP. */
         result = ndgPutGroup( elem, name, status );
         astXmlAddAttr( result, "utype", utype, NULL );

/* Loop round each component. Call this function recirsively to add the
   component into the group. */
         datNcomp( loc, &ncomp, status );
         for( icomp = 1; icomp <= ncomp; icomp++ ) {
            datIndex( loc, icomp, &loc2, status );
            (void) ndgHds2vot( loc2, result, status );
            datAnnul( &loc2, status );
         }

/* Arrays... */
      } else {

/* Append the dimensions of the array to the utype. This indicates that
   the GROUP contains an array. */
         nc = sprintf( utype2, "%s(%d", utype, dim[ 0 ] );
         for( i = 1; i < ndim; i++ ) {
            nc += sprintf( utype2 + nc, ",%d", dim[ i ] );
         }
         sprintf( utype2 + nc, ")" );

/* Add the GROUP element into the supplied parent element, and set its
   "utype" attribute. */
         result = ndgPutGroup( elem, name, status );
         astXmlAddAttr( result, "utype", utype2, NULL );

/* Turn the object into a one-dimensional vector of cells, and get the
   length of the vector. */
         datVec( loc, &loc2, status );
         datSize( loc2, &ncell, status );

/* Loop round each cell. Call this function recirsively to add the cell
   into the group. */
         for( icell = 1; icell <= ncell; icell++ ) {
            datCell( loc2, 1, &icell, &loc3, status );
            (void) ndgHds2vot( loc3, result, status );
            datAnnul( &loc3, status );
         }

/* Free resources. */
         datAnnul( &loc2, status );
      }
   }

/* Return a pointer to the new element. */
   return result;
}

AstXmlElement *ndgPutGroup( AstXmlElement *elem, const char *name, int *status ){
/*
*+
*  Name:
*     ndgPutGroup

*  Purpose:
*     Create a new GROUP element.

*  Invocation:
*     AstXmlElement *ndgPutGroup( AstXmlElement *elem, const char *name,
*                                 int *status )

*  Description:
*     This function creates a new GROUP element, and stores it in the given
*     parent element. It also returns a pointer to the new GROUP element.

*  Arguments:
*     elem
*        A pointer to the XmlElement in which the GROUP element is to
*        be stored. May be NULL.
*     name
*        The string to be used as the "name" attribute for the new GROUP
*        element.
*     status
*        The inherited status.

*  Returned Value:
*     A pointer to the new GROUP element.

*-
*/

/* Local Varianles: */
   AstXmlElement *group = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return group;

/* Create the new element, and add it into the supplied parent element. */
   group = astXmlAddElement( elem, "GROUP", NULL );

/* Set it's name attributes. */
   astXmlAddAttr( group, "name", name, NULL );

/* Return a pointer to the new GROUP element. */
   return group;
}

AstXmlElement *ndgPutParam( AstXmlElement *elem, const char *name,
                            int ndim, hdsdim *dim, const char *datatype,
                            const char *values, int *status ){
/*
*+
*  Name:
*     ndgPutParam

*  Purpose:
*     Create a new multi-dimensional PARAM element.

*  Invocation:
*     AstXmlElement *ndgPutParam( AstXmlElement *elem, const char *name,
*                                 int ndim, hdsdim *dim, const char *datatype,
*                                 const char *values, int *status )

*  Description:
*     This function creates a new multi-dimensional PARAM element, and
*     stores it in the given parent element. It also returns a pointer
*     to the new PARAM element.

*  Arguments:
*     elem
*        A pointer to the XmlElement in which the PARAM element is to
*        be stored. May be NULL.
*     name
*        The string to be used as the "name" attribute for the new PARAM
*        element.
*     ndim
*        The number of dimensions.
*     dim
*        Pointer to an array holding the "ndim" dimensions.
*     datatype
*        The string to be used as the "datatype" attribute for the new PARAM
*        element.
*     value
*        The string to be used as the "value" attribute for the new PARAM
*        element. Numerical values should be stored in Fortran order, and
*        should be separated by spaces. For strings, all strings should
*        be of fixed length (given by dim[0]), and there should be no
*        spaces between strings.
*     status
*        The inherited status.

*  Returned Value:
*     A pointer to the new PARAM element.

*-
*/

/* Local Varianles: */
   AstXmlElement *param = NULL;
   char *arraysize;
   char buf[ 30 ];
   int i;
   int nc;

/* Check inherited status */
   if( *status != SAI__OK ) return param;

/* Create the new element, and add it into the supplied parent element. */
   param = astXmlAddElement( elem, "PARAM", NULL );

/* Set the name and dataype attributes. */
   astXmlAddAttr( param, "name", name, NULL );
   astXmlAddAttr( param, "datatype", datatype, NULL );

/* Format the first dimension into a dynamically allocated string. */
   sprintf( buf, "%" HDS_DIM_FORMAT, dim[ 0 ] ) ;
   arraysize = NULL;
   nc = 0;
   arraysize = astAppendString( arraysize, &nc, buf );

/* For each other axis, add the dimension size to the buffer. */
   for( i = 1; i < ndim; i++ ) {
      sprintf( buf, "x%" HDS_DIM_FORMAT, dim[ i ] ) ;
      arraysize = astAppendString( arraysize, &nc, buf );
   }

/* Add the "arraysize" attribute. */
   astXmlAddAttr( param, "arraysize", arraysize, NULL );

/* Free resources. */
   arraysize = astFree( arraysize );

/* Finally set the value attribute. */
   astXmlAddAttr( param, "value", values, NULL );

/* Return a pointer to the new PARAM element. */
   return param;
}

AstXmlElement *ndgPutParam0( AstXmlElement *elem, const char *name,
                             const char *datatype, const char *value,
                             int *status ){
/*
*+
*  Name:
*     ndgPutParam0

*  Purpose:
*     Create a new scalar PARAM element.

*  Invocation:
*     AstXmlElement *ndgPutParam0( AstXmlElement *elem, const char *name,
*                                  const char *datatype, const char *value,
*                                  int *status )

*  Description:
*     This function creates a new PARAM element with a scalar value, and
*     stores it in the given parent element. It also returns a pointer to
*     the new PARAM element.

*  Arguments:
*     elem
*        A pointer to the XmlElement in which the PARAM element is to
*        be stored. May be NULL.
*     name
*        The string to be used as the "name" attribute for the new PARAM
*        element.
*     datatype
*        The string to be used as the "datatype" attribute for the new PARAM
*        element.
*     value
*        The string to be used as the "value" attribute for the new PARAM
*        element.
*     status
*        The inherited status.

*  Returned Value:
*     A pointer to the new PARAM element.
*-
*/

/* Local Varianles: */
   AstXmlElement *param = NULL;
   char buf[ 30 ];

/* Check inherited status */
   if( *status != SAI__OK ) return param;

/* Create the new element, and add it into the supplied parent element. */
   param = astXmlAddElement( elem, "PARAM", NULL );

/* Set the dane and datatype attributes. */
   astXmlAddAttr( param, "name", name, NULL );
   astXmlAddAttr( param, "datatype", datatype, NULL );

/* For strings, add an "arraysize" attribute. */
   if( !strcmp( datatype, "char" ) ) {
      sprintf( buf, "%d", (int) strlen( value ) );
      astXmlAddAttr( param, "arraysize", buf, NULL );
   }

/* Finally set the value attribute. */
   astXmlAddAttr( param, "value", value, NULL );

/* Return a pointer to the new PARAM element. */
   return param;
}


HDSLoc *ndgVot2hds( AstXmlElement *elem, HDSLoc *ploc, int *status ){
/*
*+
*  Name:
*     ndgVot2hds

*  Purpose:
*     Restore an HDS object from its VOTABLE representation.

*  Invocation:
*     HDSLoc *ndgVot2hds( AstXmlElement *elem, HDSLoc *ploc, int *status )

*  Description:
*     This function re-creates an HDS object from the supplied XmlElement
*     and stores it as a component in a given HDS structure. The XmlElement
*     should have been created using ndgHds2vot.

*  Arguments:
*     elem
*        A pointer to the XmlElement in which the HDS Object is stored.
*     ploc
*        A locator for a scalar HDS structure in which the restored object is
*        to be stored.
*     status
*        The inherited status.

*  Returned Value:
*     A locator for new HDS object.

*-
*/

/* Call a private function to do the work, indicating that the HDS object
   represented by "elem" should be added to "result (rather than the
   components of "elem" being added to "result"). */
   return ndg1Vot2hds( elem, ploc, 0, status );
}






/* Private Functions: */
/* --------------------------------------------------------------------- */

static HDSLoc *ndg1Vot2hds( AstXmlElement *elem, HDSLoc *ploc, int comps,
                            int *status ){
/*
*  Name:
*     ndg1Vot2hds

*  Purpose:
*     Restore an HDS object from its VOTABLE representation.

*  Invocation:
*     HDSLoc *ndg1Vot2hds( AstXmlElement *elem, HDSLoc *ploc, int comps,
*                          int *status )

*  Description:
*     This function does the work for the public function hdsVot2hds.

*  Arguments:
*     elem
*        A pointer to the XmlElement in which the HDS Object is stored.
*     ploc
*        A locator for a scalar HDS structure in which the restored object is
*        to be stored.
*     comps
*        If zero, the HDS object represented by "elem" is added as a single
*        component into the "ploc" structure. If non-zero, the components
*        of the HDS object represented by "elem" are added separately into
*        the "ploc" structure. Zero is assumed if "elem" does not
*        represent a scalar structure.
*     status
*        The inherited status.

*  Returned Value:
*     A locator for new HDS object.

*/

/* Local Varianles: */
   AstXmlContentItem *item;
   HDSLoc *cloc = NULL;
   HDSLoc *newloc = NULL;
   HDSLoc *result = NULL;
   HDSLoc *vloc = NULL;
   char **strings;
   char *vbuf = NULL;
   char type[ DAT__SZNAM + 1 ];
   const char *arraysize;
   const char *ename;
   const char *name;
   const char *p;
   const char *q;
   const char *utype;
   const char *value;
   const char *votype;
   hdsdim dim[ NDF__MXDIM ];
   hdsdim icell;
   int clen;
   int i;
   int is_char;
   int nc;
   int ndim;
   int nitem;
   int nmatch;
   int prim;
   int use_utype;
   size_t ln;
   size_t ncell;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Check the supplied locator is a scalar structure. */
   datPrim( ploc, &prim, status );
   if( prim && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "ndgVot2hds: Supplied HDS object is a primitive "
              "(programming error).", status );
   }

   datShape( ploc, NDF__MXDIM, dim, &ndim, status );
   if( ndim > 0 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRep( "", "ndgVot2hds: Supplied HDS object is an array "
              "(programming error).", status );
   }

/* Get the name of the supplied element. */
   ename = astXmlGetName( (AstXmlObject *) elem );

/* Deal with HDS primitives (represented in XML by <PARAM> elements). */
   if( !strcmp( ename, "PARAM" ) ){

/* Get the mandatory "name", "datatype" and "value" attributes from the PARAM
   element, reporting an error if any are not present. */
      name = ndgGetAttrib( elem, "name", "ndgVot2hds", status );
      votype = ndgGetAttrib( elem, "datatype", "ndgVot2hds", status );
      value = ndgGetAttrib( elem, "value", "ndgVot2hds", status );

/* Select the appropriate HDS data type. Set a flag indicating that the
   date type should be extracted from the utype attribute if it is
   ambiguous. */
      use_utype = 0;
      is_char = 0;
      if( !strcmp( votype, "int" ) ) {
         use_utype = 1;

      } else if( !strcmp( votype, "float" ) ) {
         strcpy( type, "_REAL" );

      } else if( !strcmp( votype, "double" ) ) {
         strcpy( type, "_DOUBLE" );

      } else if( !strcmp( votype, "boolean" ) ) {
         strcpy( type, "_LOGICAL" );

      } else if( !strcmp( votype, "char" ) ) {
         strcpy( type, "_CHAR" );
         is_char = 1;

      } else if( !strcmp( votype, "short" ) ) {
         use_utype = 1;

      } else if( !strcmp( votype, "unsignedByte" ) ) {
         strcpy( type, "_UBYTE" );

      } else if( *status == SAI__OK ) {
         msgSetc( "T", votype );
         msgSetc( "N", name );
         *status = SAI__ERROR;
         errRep( "", "ndgVot2hds: Unknown data type in <PARAM name=\"^N\" "
                 "datatype=\"^T\">.", status );
      }

/* If the HDS data type was not determined uniquely by the "datatype"
   attribute, get the HDS data type from the "utype" attribute. */
      if( use_utype ) {
         utype = ndgGetAttrib( elem, "utype", "ndgVot2hds", status );
         if( utype && sscanf( utype, "hds_type:%s", type ) != 1 &&
             *status == SAI__OK ) {
            msgSetc( "T", utype );
            msgSetc( "N", name );
            *status = SAI__ERROR;
            errRep( "", "ndgVot2hds: Unknown utype in <PARAM name=\"^N\" "
                    "utype=\"^T\">.", status );
         }
      }

/* If the data-type is "char", get the "arraysize" attribute reporting an
   error if it is not present. For other data-types, attempt to get the
   "arraysize" attribute, but do not report an error if it is not present
   since scalar values do not need an arraysize. */
      if( is_char ) {
         arraysize = ndgGetAttrib( elem, "arraysize", "ndgVot2hds", status );
      } else {
         arraysize = astXmlGetAttributeValue( elem, "arraysize" );
      }

/* If we have an arraysize value, extract the array dimensions from it. */
      ndim = 0;
      if( arraysize ) {
         p = arraysize;
         while( ndim < NDF__MXDIM ) {
            nc = 0;
            if( !sscanf( p, "%d%n", dim + ndim, &nc ) || !nc ) break;
            ndim++;
            p += nc;
            if( *p == 'x' ) p++;
         }

         if( astChrLen( p ) > 0 && *status == SAI__OK ) {
            msgSetc( "A", arraysize );
            msgSetc( "N", name );
            *status = SAI__ERROR;
            errRep( "", "ndgVot2hds: Cannot read array dimensions from "
                    "'arraysize' attribute in <PARAM name=\"^N\" "
                    "arraysize=\"^A\">.", status );
         }
      }

/* For a string array, the first XML dimension is the fixed string length.
   Extract the string length into "clen" and remove it from the list of
   dimensions. Also append it to the end of the HDS data type. */
      if( is_char ) {
         clen = dim[ 0 ];
         for( i = 1; i < ndim; i++ ) dim[ i - 1 ] = dim[ i ];
         ndim--;
         sprintf( type, "_CHAR*%d", clen );
      }

/* Create the HDS component and get a locator to it. */
      datNew( ploc, name, type, ndim, dim, status );
      datFind( ploc, name, &result, status );

/* For scalars, just store the value in the string read from the "value"
   attribute. */
      if( ndim == 0 ) {
         datPut0C( result, value, status );

/* For arrays, write the values as a vector. */
      } else {

/* Get a pointer to the first non-white character in the "value" string. */
         p = value;
         if( !is_char ) {
            while( isspace( *p ) ) p++;
         }

/* Get a locator to a vectorise form of the array. */
         datVec( result, &vloc, status );

/* Get the number of cells in the array. */
         ncell = 0;
         datSize( vloc, &ncell, status );

/* Loop round all cells. */
         for( icell = 1; icell <= ncell && *status == SAI__OK; icell++ ) {

/* Get a locator to the cell. */
            datCell( vloc, 1, &icell, &cloc, status );

/* Copy the string holding the next value into a null-terminated buffer. For
   fixed-length strings, just copy the next block of "clen" characters into
   the buffer, terminate it, and move "p" on to point to the start of the
   next block of "clen" characters. */
            if( is_char ) {
               vbuf = astStore( vbuf, p, clen + 1 );
               if( vbuf ) vbuf[ clen ] = 0;
               p += clen;

/* For other data types... */
            } else {

/* Get a pointer to the next space (or null) character. */
               q = p;
               while( *q && !isspace( *q ) ) q++;

/* Get the number of characters, and copy them into the buffer, expanding
   the buffer as needed, and then terminate the buffer. */
               ln = q - p;
               vbuf = astStore( vbuf, p, ln + 1 );
               if( vbuf ) vbuf[ ln ] = 0;

/* Get a pointer to the next non-space character. */
               p = q;
               while( isspace( *p ) ) p++;
            }

/* Store the buffer value. */
            datPut0C( cloc, vbuf, status );

/* Free resources. */
            datAnnul( &cloc, status );
         }

         vbuf = astFree( vbuf );
         datAnnul( &vloc, status );
      }

/* Deal with HDS structures (represented in XML by <GROUP> elements). */
   } else if( !strcmp( ename, "GROUP" ) ){

/* Get the mandatory "name" and "utype" attributes from the GROUP element,
   reporting an error if either is not present. */
      name = ndgGetAttrib( elem, "name", "ndgVot2hds", status );
      utype = ndgGetAttrib( elem, "utype", "ndgVot2hds", status );

/* Check the utype has a "hds_type:" prefix. Report an error if not. */
      if( utype && sscanf( utype, "hds_type:%s", type ) != 1 && *status == SAI__OK ) {
         msgSetc( "T", utype );
         msgSetc( "N", name );
         *status = SAI__ERROR;
         errRep( "", "ndgVot2hds: Unknown utype in <GROUP name=\"^N\" "
                 "utype=\"^T\">.", status );
      }

/* See if array dimensions are appended to the end of the HDS type. If
   they are, then this GROUP represents an array of structures. Extract the
   dimensions into an int array, and extract the HDS type without the
   array dimensions. */
      ndim = 0;
      strings = astChrSplitRE( type, "(.+)\\(([0123456789,]+)\\)$", &nmatch,
                               NULL );
      if( nmatch ) {

         p = strings[ 1 ];
         while( ndim < NDF__MXDIM ) {
            nc = 0;
            if( !sscanf( p, "%d%n", dim + ndim, &nc ) || !nc ) break;
            ndim++;
            p += nc;
            if( *p == ',' ) p++;
         }

         if( astChrLen( p ) > 0 && *status == SAI__OK ) {
            msgSetc( "A", utype );
            msgSetc( "N", name );
            *status = SAI__ERROR;
            errRep( "", "ndgVot2hds: Cannot read array dimensions from "
                    "'utype' attribute in <GROUP name=\"^N\" "
                    "utype=\"^A\">.", status );
         }

         if( *status == SAI__OK ) strcpy( type, strings[ 0 ] );

         strings[ 0 ] = astFree( strings[ 0 ] );
         strings[ 1 ] = astFree( strings[ 1 ] );
         strings = astFree( strings );
      }

/* If requied create an HDS structure to contain the components, and get a
   locator to it. */
      if( ! comps ) {
         datNew( ploc, name, type, ndim, dim, status );
         datFind( ploc, name, &result, status );

/* If we are storing the components separately in the supplied parent,
   use a clone of the supplied locator. */
      } else {
         datClone( ploc, &result, status );
      }

/* Loop round all the children elements within the supplied element. Call
   this function recursively to add it into the HDS structure. */
      icell = 0;
      nitem = astXmlGetNitem( elem );
      for( i = 0; i < nitem; i++ ) {
         item = astXmlGetItem( elem, i );
         if( astXmlCheckType( item, AST__XMLELEM ) ) {
            if( ndim ) {
               icell++;
               datCell( result, 1, &icell, &cloc, status );
               newloc = ndg1Vot2hds( (AstXmlElement *) item, cloc, 1, status );
               datAnnul( &cloc, status );
            } else {
               newloc = ndg1Vot2hds( (AstXmlElement *) item, result, 0, status );
            }
            datAnnul( &newloc, status );
         }
      }

/* Report an error if the name of the supplied element is not PARAM
   or GROUP. */
   } else if( *status == SAI__OK ) {
      msgSetc( "N", ename );
      *status = SAI__ERROR;
      errRep( "", "ndgVot2hds: Supplied XML element has an unknown name: "
              "<^N> (must be <PARAM> or <GROUP>).", status );
   }

/* Return a pointer to the new element. */
   return result;
}

