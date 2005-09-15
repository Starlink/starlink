#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "f77.h"              /* F77 <-> C interface macros                  */
#include "cnf.h"              /* F77 <-> C string handling functions         */
#include "str.h"              /* String constants                            */
#include "dat_par.h"          /* DAT__ constant definitions                  */
#include "hds.h"              /* HDS C interface			     */
#include "hds1.h"
#define TRUE 1
#define FALSE 0
#define SAI__OK 0

F77_SUBROUTINE(dat_getc)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          CHARACTER(values),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(values)
			  );

F77_SUBROUTINE(dat_alter)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{
/*==================================*/
/* DAT_ALTER - Alter size of object */
/*==================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the locator string                  */

   cnfImpch( locator, DAT__SZLOC, locator_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];
      
/* Call pure C routine                                       */
   datAlter( locator_c, *ndim, dims64, status );
#else
   datAlter( locator_c, *ndim, dims, status );
#endif
}

F77_SUBROUTINE(dat_annul)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{
/*===========================*/
/* DAT_ANNUL - Annul locator */
/*===========================*/
/* Local variable */
   char locator_c[DAT__SZLOC];

/* Enter routine.	*/

/* Import the locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Call pure C routine                                       */
   datAnnul( locator_c, status);

/* Export returned locator */
   cnfExpch( locator_c, locator, DAT__SZLOC );
}

F77_SUBROUTINE(dat_basic)( CHARACTER(locator),
                           CHARACTER(mode),
                           F77_POINTER_TYPE *pntr,
                           F77_INTEGER_TYPE *len,
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator) 
                           TRAIL(mode) )
{

/*===============================================*/
/* DAT_BASIC - Map data (in basic machine units) */
/*===============================================*/

/* Local variables.     */
   char mode_c[DAT__SZMOD+1];
   unsigned char *cpntr = NULL; /* initialise in case of bad return status */
   char locator_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the locator and mode strings  */

   cnfImpch( locator, DAT__SZLOC, locator_c);
   cnfImpn( mode, mode_length, DAT__SZMOD, mode_c);

/* Call pure C routine                                       */
   datBasic( locator_c, mode_c, &cpntr, len, status);

/* Export the C pointer as a FORTRAN pointer */
   *pntr = cnfFptr( cpntr );
}

F77_SUBROUTINE(dat_ccopy)( CHARACTER(locator1),
                           CHARACTER(locator2),
                           CHARACTER(name),
                           CHARACTER(locator3),
                           F77_INTEGER_TYPE *status
 	                   TRAIL(locator1)
                           TRAIL(locator2)
                           TRAIL(name)
                           TRAIL(locator3) )
 {

/*======================================*/
/* DAT_CCOPY - copy one structure level */
/*======================================*/

/* Local variables.     */ 
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
   char locator3_c[DAT__SZLOC];
   char name_c[DAT__SZNAM+1];
   
/* Enter routine.	*/

/* Import maxmimum length strings  */
   cnfImpn( name, name_length, DAT__SZNAM, name_c);

/* Import the input locator strings                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c);
   cnfImpch( locator2, DAT__SZLOC, locator2_c);
 
/* Call pure C routine                                       */
   datCcopy( locator1_c, locator2_c, name_c, locator3_c, status);

/* Export returned locator    */
   cnfExpch( locator3_c, locator3, DAT__SZLOC);
}

F77_SUBROUTINE(dat_cell)( CHARACTER(locator1),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE subs[],
                          CHARACTER(locator2),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator1)
                          TRAIL(locator2) )
{

/*============================================*/
/* DAT_CELL - Locate a "cell" (array element) */
/*============================================*/

/* Local variables.     */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG subs64[DAT__MXDIM];
   int i;
#endif

/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      subs64[i] = subs[i];
      
/* Call pure C routine                                       */
   datCell( locator1_c, *ndim, subs64, locator2_c, status);
#else
   datCell( locator1_c, *ndim, subs, locator2_c, status);
#endif

/* Export returned locator */
   cnfExpch( locator2_c, locator2, DAT__SZLOC ); 
}

F77_SUBROUTINE(dat_clen)( CHARACTER(locator),
                          F77_INTEGER_TYPE *clen,
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator) )
{

/*===========================================*/
/* DAT_CLEN - Obtain character string length */
/*===========================================*/

/* Local variables */
   char locator_c[DAT__SZLOC];

/* Enter routine.	*/

/* Import the locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Call pure C routine                                       */
   datClen( locator_c, clen, status);
}

F77_SUBROUTINE(dat_clone)( CHARACTER(locator1),
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator1)
                           TRAIL(locator2) )
{

/*===========================*/
/* DAT_CLONE - clone locator */
/*===========================*/

/* Local variables */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];

/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c);

/* Call pure C routine                                       */
   datClone( locator1_c, locator2_c, status);

/* Export the returned locator */
   cnfExpch( locator2_c, locator2, DAT__SZLOC );
}

F77_SUBROUTINE(dat_coerc)( CHARACTER(locator1),
                           F77_INTEGER_TYPE *ndim,
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator1)
                           TRAIL(locator2) )
{

/*=================================*/
/* DAT_COERC - coerce object shape */
/*=================================*/

/* Local variables.     */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c);

/* Call pure C routine                                       */
   datCoerc( locator1_c, *ndim, locator2_c, status);

/* Export the returned locator                      */
   cnfExpch( locator2_c, locator2, DAT__SZLOC );
}

F77_SUBROUTINE(dat_copy)( CHARACTER(locator1),
                          CHARACTER(locator2),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator1)
                          TRAIL(locator2)
                          TRAIL(name) )
{

/*========================*/
/* DAT_COPY - copy object */
/*========================*/

/* Local variables.     */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
   char name_c[DAT__SZNAM+1];
   
/* Enter routine.	*/

/* Import name string */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c);

/* Import the locator strings                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c);
   cnfImpch( locator2, DAT__SZLOC, locator2_c);

/* Call pure C routine                                       */
   datCopy( locator1_c, locator2_c, name_c, status);
}

F77_SUBROUTINE(dat_drep)( CHARACTER(locator),
                          CHARACTER(format),
                          CHARACTER(order),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator)
                          TRAIL(format)
                          TRAIL(order) )
{

/*=============================================================*/
/* DAT_DREP - Obtain primitive data representation information */
/*=============================================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char *format_c;
   char *order_c;
   
/* Enter routine.	*/

/* Import the locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Call pure C routine                                      */
   datDrep( locator_c, &format_c, &order_c, status);

/* Export the returned C strings to the FORTRAN variables    */

   if( *status == SAI__OK )
   {
      cnfExprt( format_c, format, format_length );
      cnfExprt( order_c,  order,  order_length  );
   }
}

F77_SUBROUTINE(dat_erase)( CHARACTER(locator),
                           CHARACTER(name),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(name) )
{

/*=========================================*/
/* DAT_ERASE - Erase object                */
/*=========================================*/

/* Local variables.     */
   char name_c[DAT__SZNAM+1];
   char locator_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import name string  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c);

/* Import the locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Call pure C routine                                       */
   datErase( locator_c, name_c, status);
}

F77_SUBROUTINE(dat_ermsg)( F77_INTEGER_TYPE *status,
			   F77_INTEGER_TYPE *length,
                           CHARACTER(msg)
                           TRAIL(msg) )
{

/*============================================================*/
/* DAT_ERMSG - Translate a status value into an error message */
/*============================================================*/

/* Local variables.     */
   char *msg_c;
   
/* Enter routine.	*/

/* Call pure C routine                                       */
   datErmsg( status, length, &msg_c );

/* Export the returned C string to FORTRAN variable    */
   cnfExprt( msg_c, msg, msg_length );
}

F77_SUBROUTINE(dat_find)( CHARACTER(locator1),
                          CHARACTER(name),
                          CHARACTER(locator2),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator1)
                          TRAIL(name)
                          TRAIL(locator2) )
{

/*=================================*/
/* DAT_FIND - Find named component */
/*=================================*/

/* Local variables.     */
   char name_c[DAT__SZNAM+1];
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import name  string  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c);

/* Import the input locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c);

/* Call pure C routine                                       */
   datFind( locator1_c, name_c, locator2_c, status);

/* Export the returned locator */
   cnfExpch( locator2_c, locator2, DAT__SZLOC);
}

F77_SUBROUTINE(dat_get)( CHARACTER(locator),
                         CHARACTER(type),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_BYTE_TYPE values[],
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(type)
			 TRAIL(values))
{

/*=============================*/
/* DAT_GET - Read primitive(s) */
/*=============================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char type_c[DAT__SZTYP+1];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import FORTRAN to C string  */
   cnfImpn( type, type_length, DAT__SZTYP,  type_c);

   if (strncmp(type,"_CHAR",5) == 0) {
     /* Call dat_getC instead as a special case using the argument
        that Fortran has pushed onto the end solely for STRINGs */
     F77_CALL(dat_getc)(locator,ndim,dims,
			CHARACTER_ARG(values), status
			TRAIL_ARG(locator)
			TRAIL_ARG(values));
     return;
   }


/* Import the locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datGet( locator_c, type_c, *ndim, dims64, values, status);
#else
   datGet( locator_c, type_c, *ndim, dims, values, status);
#endif
}

F77_SUBROUTINE(dat_getc)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          CHARACTER(values),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(values)
			  )
{

/*====================================*/
/* DAT_GETC - Read _CHAR primitive(s) */
/*====================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datGetC( locator_c, *ndim, dims64, values, values_length, status);
#else
   datGetC( locator_c, *ndim, dims, values, values_length, status);
#endif
}

F77_SUBROUTINE(dat_getd)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_DOUBLE_TYPE values[],
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*======================================*/
/* DAT_GETD - Read _DOUBLE primitive(s) */
/*======================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datGetD( locator_c, *ndim, dims64, values, status);
#else
   datGetD( locator_c, *ndim, dims, values, status);
#endif
}

F77_SUBROUTINE(dat_geti)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_INTEGER_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*=======================================*/
/* DAT_GETI - Read _INTEGER primitive(s) */
/*=======================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datGetI( locator_c, *ndim, dims64, values, status);
#else
   datGetI( locator_c, *ndim, dims, values, status);
#endif
}

F77_SUBROUTINE(dat_getl)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_LOGICAL_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*=======================================*/
/* DAT_GETL - Read _LOGICAL primitive(s) */
/*=======================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datGetL( locator_c, *ndim, dims64, values, status);
#else
   datGetL( locator_c, *ndim, dims, values, status);
#endif
}

F77_SUBROUTINE(dat_getr)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_REAL_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*====================================*/
/* DAT_GETR - Read _REAL primitive(s) */
/*====================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datGetR( locator_c, *ndim, dims64, values, status);
#else
   datGetR( locator_c, *ndim, dims, values, status);
#endif
}

F77_SUBROUTINE(dat_index)( CHARACTER(locator1),
                           F77_INTEGER_TYPE *index,
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator1)
                           TRAIL(locator2) )
{

/*=======================================*/
/* DAT_INDEX - Index into component list */
/*=======================================*/

/* Local variables.     */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c);

/* Call pure C routine                                       */
   datIndex( locator1_c, *index, locator2_c, status);

/*Export the returned locator */
   cnfExpch( locator2_c, locator2, DAT__SZLOC );
}

F77_SUBROUTINE(dat_len)( CHARACTER(locator),
                         F77_INTEGER_TYPE *len,
                         F77_INTEGER_TYPE *status
                         TRAIL(locator) )
{

/*====================================*/
/* DAT_LEN - Inquire primitive length */
/*====================================*/

/* Local variables */
   char locator_c[DAT__SZLOC];

/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Call pure C routine                                       */
   datLen( locator_c, len, status );
}

F77_SUBROUTINE(dat_map)( CHARACTER(locator),
                         CHARACTER(type),
                         CHARACTER(mode),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_POINTER_TYPE *pntr,
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(type)
                         TRAIL(mode) )
{

/*============================*/
/* DAT_MAP - Map primitive(s) */
/*============================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char type_c[DAT__SZTYP+1];
   char mode_c[DAT__SZMOD+1];
   unsigned char *cpntr = NULL; /* initialise in case of bad return status */
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Import FORTRAN to C strings  */
   cnfImpn( type, type_length, DAT__SZTYP,  type_c);
   cnfImpn( mode, mode_length, DAT__SZMOD,  mode_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datMap( locator_c, type_c, mode_c, *ndim, dims64, &cpntr, status);
#else
   datMap( locator_c, type_c, mode_c, *ndim, dims, &cpntr, status);
#endif

/* Export the C pointer as a FORTRAN POINTER */
   *pntr = cnfFptr( cpntr );
}

F77_SUBROUTINE(dat_mapc)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) )
{

/*===================================*/
/* DAT_MAPC - Map _CHAR primitive(s) */
/*===================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char mode_c[DAT__SZMOD+1];
   unsigned char *cpntr = NULL; /* Initialise in case of bad return status */
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Import mode string  */
   cnfImpn( mode, mode_length, DAT__SZMOD,  mode_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datMapC( locator_c, mode_c, *ndim, dims64, &cpntr, status);
#else
   datMapC( locator_c, mode_c, *ndim, dims, &cpntr, status);
#endif

/* Export the C pointer as a FORTRAN POINTER */
   *pntr = cnfFptr( cpntr );
}

F77_SUBROUTINE(dat_mapd)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) )
{

/*=====================================*/
/* DAT_MAPD - Map _DOUBLE primitive(s) */
/*=====================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char mode_c[DAT__SZMOD+1];
   double *cpntr = NULL; /* initialise in case of bad return status */
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import mode string  */
   cnfImpn( mode, mode_length, DAT__SZMOD,  mode_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datMapD( locator_c, mode_c, *ndim, dims64, &cpntr, status);
#else
   datMapD( locator_c, mode_c, *ndim, dims, &cpntr, status);
#endif

/* Export the C pointer as a FORTRAN POINTER */
   *pntr = cnfFptr( cpntr );
}

F77_SUBROUTINE(dat_mapi)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) )
{

/*======================================*/
/* DAT_MAPI - Map _INTEGER primitive(s) */
/*======================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char mode_c[DAT__SZMOD+1];
   int *cpntr = NULL; /* initialise in case of bad return status */
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import mode string  */
   cnfImpn( mode, mode_length, DAT__SZMOD,  mode_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datMapI( locator_c, mode_c, *ndim, dims64, &cpntr, status);
#else
   datMapI( locator_c, mode_c, *ndim, dims, &cpntr, status);
#endif

/* Export the C pointer as a FORTRAN POINTER */
   *pntr = cnfFptr( cpntr );
}

F77_SUBROUTINE(dat_mapl)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) )
{

/*======================================*/
/* DAT_MAPL - Map _LOGICAL primitive(s) */
/*======================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char mode_c[DAT__SZMOD+1];
   int *cpntr = NULL; /* initialise in case of bad return status */
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import mode string  */
   cnfImpn( mode, mode_length, DAT__SZMOD,  mode_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datMapL( locator_c, mode_c, *ndim, dims64, &cpntr, status);
#else
   datMapL( locator_c, mode_c, *ndim, dims, &cpntr, status);
#endif

/* Export the C pointer as a FORTRAN POINTER */
   *pntr = cnfFptr( cpntr );
}

F77_SUBROUTINE(dat_mapr)( CHARACTER(locator),
                          CHARACTER(mode),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_POINTER_TYPE *pntr,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(mode) )
{

/*===================================*/
/* DAT_MAPR - Map _REAL primitive(s) */
/*===================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char mode_c[DAT__SZMOD+1];
   float *cpntr = NULL; /* initialise in case of bad return status */
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import mode string  */
   cnfImpn( mode, mode_length, DAT__SZMOD,  mode_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datMapR( locator_c, mode_c, *ndim, dims64, &cpntr, status );
#else
   datMapR( locator_c, mode_c, *ndim, dims, &cpntr, status );
#endif

/* Export the C pointer as a FORTRAN POINTER */
   *pntr = cnfFptr( cpntr );
}

F77_SUBROUTINE(dat_mould)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{
/*==================================*/
/* DAT_MOULD - Alter shape of object */
/*==================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datMould( locator_c, *ndim, dims64, status );
#else
   datMould( locator_c, *ndim, dims, status );
#endif
}

F77_SUBROUTINE(dat_move)( CHARACTER(locator1),
                          CHARACTER(locator2),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator1)
                          TRAIL(locator2)
                          TRAIL(name) )
{

/*========================*/
/* DAT_MOVE - Move object */
/*========================*/

/* Local variables.     */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
   char name_c[DAT__SZNAM+1];
   
/* Enter routine.	*/

/* Import the first locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c );

/* Import the second locator string                  */
   cnfImpch( locator2, DAT__SZLOC, locator2_c );

/* Import "name" to C string  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c );

/* Call pure C routine                                       */
   datMove( locator1_c, locator2_c, name_c, status );

/* Export returned locator - will be now DAT__NOLOC */
   cnfExpch( locator1_c, locator1, DAT__SZLOC );
}

F77_SUBROUTINE(dat_name)( CHARACTER(locator),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(name) )
{

/*================================*/
/* DAT_NAME - Enquire object name */
/*================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char name_c[DAT__SZNAM+1]; 
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datName( locator_c, name_c, status );

/* Export returned name to FORTRAN string                 */
   cnfExpn( name_c, DAT__SZNAM, name, name_length );
}

F77_SUBROUTINE(dat_ncomp)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ncomp,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{

/*==========================================*/
/* DAT_NCOMP - Inquire number of components */
/*==========================================*/

/* Local variables */
   char locator_c[DAT__SZLOC];

/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datNcomp( locator_c, ncomp, status );
}

F77_SUBROUTINE(dat_new)( CHARACTER(locator),
                         CHARACTER(name),
                         CHARACTER(type),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(name)
                         TRAIL(type) )
{

/*================================*/
/* DAT_NEW - Create new component */
/*================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char name_c[DAT__SZNAM+1];
   char type_c[DAT__SZTYP+1];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import FORTRAN to C strings  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c);
   cnfImpn( type, type_length, DAT__SZTYP,  type_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datNew( locator_c, name_c, type_c, *ndim, dims64, status);
#else
   datNew( locator_c, name_c, type_c, *ndim, dims, status);
#endif
}

F77_SUBROUTINE(dat_newc)( CHARACTER(locator),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *len,
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(name) )
{

/*============================================*/
/* DAT_NEWC - Create new _CHAR type component */
/*============================================*/

/* Local variables */
   char locator_c[DAT__SZLOC];
   char name_c[DAT__SZNAM+1];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import "name" to C string  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datNewC( locator_c, name_c, *len, *ndim, dims64, status );
#else
   datNewC( locator_c, name_c, *len, *ndim, dims, status );
#endif
}

F77_SUBROUTINE(dat_paren)( CHARACTER(locator1),
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator1)
                           TRAIL(locator2) )
{

/*=====================================*/
/* DAT_PAREN - Locate parent structure */
/*=====================================*/

/* Local variables.     */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c );

/* Call pure C routine                                       */
   datParen( locator1_c, locator2_c, status );

/* Export returned locator to FORTRAN string                 */
   cnfExpch( locator2_c, locator2, DAT__SZLOC );
}

F77_SUBROUTINE(dat_prim)( CHARACTER(locator),
                          F77_LOGICAL_TYPE *reply,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*=====================================*/
/* DAT_PRIM - Enquire object primitive */
/*=====================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   int reply_c;
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datPrim( locator_c, &reply_c, status );

/* Set FORTRAN locical return                                */
   if( reply_c )
      *reply = F77_TRUE;
   else
      *reply = F77_FALSE;   
}

F77_SUBROUTINE(dat_prmry)( F77_LOGICAL_TYPE *set,
                           CHARACTER(locator),
                           F77_LOGICAL_TYPE *prmry,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{

/*==========================================================*/
/* DAT_PRMRY - Set/Enquire primary/secondary locator status */
/*==========================================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   int set_c, primary_c;
   
/* Enter routine.	*/

   if( *set == F77_TRUE ) {
      set_c = TRUE;
      if( *prmry == F77_TRUE )
         primary_c = TRUE;
      else
         primary_c = FALSE;
   } else
      set_c = FALSE;

/* Import the locator string (given and returned)  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Call pure C routine                                       */
   datPrmry( set_c, locator_c, &primary_c, status );

/* Export returned locator - may be set to DAT__NOLOC */
   cnfExpch( locator_c, locator, DAT__SZLOC );

/* Set FORTRAN logical return                                */
   if( *set == F77_FALSE )
   {
      if (primary_c == TRUE )
         *prmry = F77_TRUE;
      else
         *prmry = F77_FALSE;
   }   
}

F77_SUBROUTINE(dat_putc)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          CHARACTER(values),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(values) )
{

/*==================================*/
/* DAT_PUTC - Write _CHAR primitive */
/*==================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];
      
/* Call C routine                                       */
   datPutC( locator_c, *ndim, dims64, values, values_length, status);
#else
   datPutC( locator_c, *ndim, dims, values, values_length, status);
#endif

}

F77_SUBROUTINE(dat_putd)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_DOUBLE_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*=====================================*/
/* DAT_PUTD - Write _DOUBLE primitives */
/*=====================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];
      
/* Call pure C routine                                       */
   datPutD( locator_c, *ndim, dims64, values, status );
#else
   datPutD( locator_c, *ndim, dims, values, status );
#endif
}

F77_SUBROUTINE(dat_puti)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_INTEGER_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*======================================*/
/* DAT_PUTI - Write _INTEGER primitives */
/*======================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];
      
/* Call pure C routine                                       */
   datPutI( locator_c, *ndim, dims64, values, status );
#else
   datPutI( locator_c, *ndim, dims, values, status );
#endif
}

F77_SUBROUTINE(dat_putr)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_REAL_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*===================================*/
/* DAT_PUTR - Write _REAL primitives */
/*===================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];
      
/* Call pure C routine                                       */
   datPutR( locator_c, *ndim, dims64, values, status );
#else
   datPutR( locator_c, *ndim, dims, values, status );
#endif
}

F77_SUBROUTINE(dat_putl)( CHARACTER(locator),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          F77_LOGICAL_TYPE *values,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*======================================*/
/* DAT_PUTL - Write _LOGICAL primitives */
/*======================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];
      
/* Call pure C routine                                       */
   datPutL( locator_c, *ndim, dims64, values, status );
#else
   datPutL( locator_c, *ndim, dims, values, status );
#endif
}

F77_SUBROUTINE(dat_put)( CHARACTER(locator),
                         CHARACTER(type),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         F77_BYTE_TYPE values[],
                         F77_INTEGER_TYPE *status
                         TRAIL(locator)
                         TRAIL(type)
			 TRAIL(values)
			 )
{

/*===========================*/
/* DAT_PUT - Write primitive */
/*===========================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char type_c[DAT__SZTYP+1];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import "type" string  */
   cnfImpn( type, type_length, DAT__SZTYP,  type_c );

   if (strncmp(type,"_CHAR",5) == 0) { 
     /* Call dat_putc instead as a special case using the argument 
        that Fortran has pushed onto the end solely for STRINGs */ 
     F77_CALL(dat_putc)(locator,ndim,dims, 
                        CHARACTER_ARG(values), status 
                        TRAIL_ARG(locator) 
                        TRAIL_ARG(values)); 
     return; 
   } 



#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];
      
/* Call pure C routine                                       */
   datPut( locator_c, type_c, *ndim, dims64, values, status );
#else
   datPut( locator_c, type_c, *ndim, dims, values, status );
#endif
}

F77_SUBROUTINE(dat_refct)( CHARACTER(locator),
                           F77_INTEGER_TYPE *refct,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{

/*====================================================*/
/* DAT_REFCT - Enquire container file reference count */
/*====================================================*/

/* Local variables */
   char locator_c[DAT__SZLOC];

/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datRefct( locator_c, refct, status );
}

F77_SUBROUTINE(dat_renam)( CHARACTER(locator),
                           CHARACTER(name),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(name) )
{

/*==============================*/
/* DAT_RENAM - Rename an object */
/*==============================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char name_c[DAT__SZNAM+1]; 
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import "name" to C string  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c );

/* Call pure C routine                                       */
   datRenam( locator_c, name_c, status );
}

F77_SUBROUTINE(dat_reset)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{

/*================================*/
/* DAT_RESET - Reset object state */
/*================================*/

/* Local variables */
   char locator_c[DAT__SZLOC];

/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datReset( locator_c, status );
}

F77_SUBROUTINE(dat_retyp)( CHARACTER(locator),
                           CHARACTER(type),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(type) )
{

/*================================*/
/* DAT_RETYP - Change object type */
/*================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char type_c[DAT__SZTYP+1]; 
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import "type" to C string  */
   cnfImpn( type, type_length, DAT__SZTYP,  type_c );

/* Call pure C routine                                       */
   datRetyp( locator_c, type_c, status);
}

F77_SUBROUTINE(dat_shape)( CHARACTER(locator),
                           F77_INTEGER_TYPE *ndimx,
                           FORTRAN_INDEX_TYPE dims[],
                           F77_INTEGER_TYPE *ndim,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{
/*==================================*/
/* DAT_SHAPE - Enquire object shape */
/*==================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

#if defined(HDS_64)
/* Call pure C routine                                       */
   datShape( locator_c, *ndimx, dims64, ndim, status );

   /* Copy the array back to fortran from HDS_PTYPE */
   for( i = 0; i<_min(*ndimx,*ndim); i++)
      dims[i] = dims64[i];

#else
   datShape( locator_c, *ndimx, dims, ndim, status );
#endif
}

F77_SUBROUTINE(dat_size)( CHARACTER(locator),
                          F77_INTEGER_TYPE *size,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator) )
{

/*================================*/
/* DAT_SIZE - Enquire object size */
/*================================*/

/* Local variables */
   char locator_c[DAT__SZLOC];

/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datSize( locator_c, size, status );
}

F77_SUBROUTINE(dat_slice)( CHARACTER(locator1),
                           F77_INTEGER_TYPE *ndim,
                           FORTRAN_INDEX_TYPE diml[],
                           FORTRAN_INDEX_TYPE dimu[],
                           CHARACTER(locator2),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator1)
                           TRAIL(locator2) )
{

/*=================================*/
/* DAT_SLICE - Locate object slice */
/*=================================*/

/* Local variables.     */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
#if defined(HDS_64)
   INT_BIG diml64[DAT__MXDIM];
   INT_BIG dimu64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ ) {
      diml64[i] = diml[i];
      dimu64[i] = dimu[i];
   }

/* Call pure C routine                                       */
   datSlice( locator1_c, *ndim, diml64, dimu64, locator2_c, status );
#else
   datSlice( locator1_c, *ndim, diml, dimu, locator2_c, status );
#endif

/* Export returned locator */
   cnfExpch( locator2_c, locator2, DAT__SZLOC );
}

F77_SUBROUTINE(dat_state)( CHARACTER(locator),
                           F77_LOGICAL_TYPE *reply,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{

/*==================================*/
/* DAT_STATE - Enquire object state */
/*==================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   int reply_c;
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datState( locator_c, &reply_c, status );

/* Set FORTRAN locical return                                */
   if( reply_c )
      *reply = F77_TRUE;
   else
      *reply = F77_FALSE;   
}

F77_SUBROUTINE(dat_struc)( CHARACTER(locator),
                           F77_LOGICAL_TYPE *reply,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{

/*======================================*/
/* DAT_STRUC - Enquire object structure */
/*======================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   int reply_c;
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datStruc( locator_c, &reply_c, status );

/* Set FORTRAN locical return                                */
   if( reply_c )
      *reply = F77_TRUE;
   else
      *reply = F77_FALSE;   
}

F77_SUBROUTINE(dat_temp)( CHARACTER(type),
                          F77_INTEGER_TYPE *ndim,
                          FORTRAN_INDEX_TYPE dims[],
                          CHARACTER(locator),
                          F77_INTEGER_TYPE *status
                          TRAIL(type)
                          TRAIL(locator) )
{

/*====================================*/
/* DAT_TEMP - Create temporary object */
/*====================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char type_c[DAT__SZTYP+1];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import "type" to C string  */
   cnfImpn( type, type_length, DAT__SZTYP,  type_c );

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];

/* Call pure C routine                                       */
   datTemp( type_c, *ndim, dims64, locator_c, status);
#else
   datTemp( type_c, *ndim, dims, locator_c, status);
#endif

/* Export returned locator */
   cnfExpch( locator_c, locator, DAT__SZLOC );
}

F77_SUBROUTINE(dat_there)( CHARACTER(locator),
                           CHARACTER(name),
                           F77_LOGICAL_TYPE *reply,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(name) )
{

/*=========================================*/
/* DAT_THERE - Enquire component existence */
/*=========================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char name_c[DAT__SZNAM+1];
   int reply_c;
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import "name" string  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c);

/* Call pure C routine                                       */
   datThere( locator_c, name_c, &reply_c, status );

/* Set FORTRAN locical return                                */
   if( reply_c )
      *reply = F77_TRUE;
   else
      *reply = F77_FALSE;   
}

F77_SUBROUTINE(dat_type)( CHARACTER(locator),
                          CHARACTER(type),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(type) )
{

/*================================*/
/* DAT_TYPE - Enquire object type */
/*================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char type_c[DAT__SZTYP+1]; 
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datType( locator_c, type_c, status );

/* Export returned type name to FORTRAN                 */
   cnfExpn( type_c, DAT__SZNAM, type, type_length );
}

F77_SUBROUTINE(dat_unmap)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{
/*==========================*/
/* DAT_UNMAP - Unmap object */
/*==========================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   datUnmap( locator_c, status );
}

F77_SUBROUTINE(dat_valid)( CHARACTER(locator),
                           F77_LOGICAL_TYPE *reply,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{

/*===================================*/
/* DAT_VALID - Enquire locator valid */
/*===================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   int reply_c;
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Call pure C routine                                       */
   datValid( locator_c, &reply_c, status );

/* Set FORTRAN logical return                                */
   if( reply_c )
      *reply = F77_TRUE;
   else
      *reply = F77_FALSE;   
}

F77_SUBROUTINE(dat_vec)( CHARACTER(locator1),
                         CHARACTER(locator2),
                         F77_INTEGER_TYPE *status
                         TRAIL(locator1)
                         TRAIL(locator2) )
{

/*============================*/
/* DAT_VEC - Vectorise object */
/*============================*/

/* Local variables.     */
   char locator1_c[DAT__SZLOC];
   char locator2_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator1, DAT__SZLOC, locator1_c );

/* Call pure C routine                                       */
   datVec( locator1_c, locator2_c, status );

/* Export returned locator */
   cnfExpch( locator2_c, locator2, DAT__SZLOC );
}

F77_SUBROUTINE(dat_where)( CHARACTER(locator),
                           F77_INTEGER_TYPE *block,
                           F77_INTEGER_TYPE *offset,
                           F77_INTEGER_TYPE *status
                           TRAIL(locator) )
{

/*===============================================*/
/* DAT_WHERE Find primitive position in HDS file */
/*===============================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   INT_BIG bloc;
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c);

/* Call pure C routine                                       */
   datWhere( locator_c, &bloc, offset, status );

   *block = bloc;
}

F77_SUBROUTINE(hds_copy)( CHARACTER(locator),
                          CHARACTER(file),
                          CHARACTER(name),
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(file)
                          TRAIL(name) )
{

/*===================================================*/
/* HDS_COPY - Copy an object to a new container file */
/*===================================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char *file_c;
   char name_c[DAT__SZNAM+1];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import FILE argument to C string */
   file_c = cnfCreim( file, file_length );

/* Import "name" to C string  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c );

/* Call pure C routine                                       */
   hdsCopy( locator_c, file_c, name_c, status );

/* Free allocated string memory.                             */
   cnfFree( file_c );
}

F77_SUBROUTINE(hds_erase)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator) )
{
/*==================================*/
/* HDS_ERASE - Erase container file */
/*==================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   hdsErase( locator_c, status );
}

F77_SUBROUTINE(hds_ewild) ( F77_INTEGER_TYPE *iwld,
                            F77_INTEGER_TYPE *status )
{
/*================================================================*/
/* HDS_EWILD - End a wild card search for HDS container files     */
/*================================================================*/

/* Enter routine.       */

/* Call pure C routine               */
   hdsEwild( iwld, status );
}

F77_SUBROUTINE(hds_flush)( CHARACTER(group),
                           F77_INTEGER_TYPE *status
                           TRAIL(group) )
{

/*=================================*/
/* HDS_FLUSH - Flush locator group */
/*==================================*/

/* Local variables.     */
   char group_c[DAT__SZGRP+1];
   
/* Enter routine.	*/

/* Import the maximum length strings  */

   cnfImpn( group, group_length,  DAT__SZGRP, group_c );

/* Call pure C routine                                       */
   hdsFlush( group_c, status );
}

F77_SUBROUTINE(hds_free)( CHARACTER(locator),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator) )
{

/*================================*/
/* HDS_FREE - Free container file */
/*================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   hdsFree( locator_c, status );
}

F77_SUBROUTINE(hds_group)( CHARACTER(locator),
                           CHARACTER(group),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(group) )
{

/*===================================*/
/* HDS_GROUP - Enquire locator group */
/*===================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char group_c  [DAT__SZGRP+1];;
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   hdsGroup( locator_c, group_c, status );

/* Export returned group name to FORTRAN string                 */
   cnfExpn( group_c, DAT__SZGRP, group, group_length);
}

F77_SUBROUTINE(hds_gtune) ( CHARACTER(param_str),
                            F77_INTEGER_TYPE *value,
                            F77_INTEGER_TYPE *status
                            TRAIL(param_str) )
{
/*=========================================*/
/* HDS_GTUNE - Get HDS tuning parameter     */
/*=========================================*/

/* Local variables.     */
   char *param_str_c;
 
/* Enter routine.       */

/* Import PARAM argument to C string */

   param_str_c = cnfCreim( param_str, param_str_length );
   
/* Call pure C routine */
   hdsGtune( param_str_c, value, status );

/* Free allocated string memory.                             */
   cnfFree( param_str_c );
}

F77_SUBROUTINE(hds_link)( CHARACTER(locator),
                          CHARACTER(group),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator) 
                          TRAIL(group) )
{

/*==================================*/
/* HDS_LINK - Link locator to group */
/*==================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char group_c[DAT__SZGRP+1];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import the group name string                  */
   cnfImpn( group, group_length, DAT__SZGRP, group_c );

/* Call pure C routine                                       */
   hdsLink( locator_c, group_c, status );
}

F77_SUBROUTINE(hds_lock)( CHARACTER(locator),
                          F77_INTEGER_TYPE *status
	                  TRAIL(locator) )
{

/*================================*/
/* HDS_LOCK - Lock container file */
/*================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   hdsLock( locator_c, status );
}

F77_SUBROUTINE(hds_new)( CHARACTER(file),
                         CHARACTER(name),
                         CHARACTER(type),
                         F77_INTEGER_TYPE *ndim,
                         FORTRAN_INDEX_TYPE dims[],
                         CHARACTER(locator),
                         F77_INTEGER_TYPE *status
                         TRAIL(file)
                         TRAIL(name)
                         TRAIL(type)
	                 TRAIL(locator) )
{

/*=====================================*/
/* HDS_NEW - Create new container file */
/*=====================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char *file_c;
   char name_c[DAT__SZNAM + 1];
   char type_c[DAT__SZTYP + 1];
#if defined(HDS_64)
   INT_BIG dims64[DAT__MXDIM];
   int i;
#endif
   
/* Enter routine.	*/

/* Import FILE argument to C string */
   file_c = cnfCreim( file, file_length );

/* Import FORTRAN to C  strings  */
   cnfImpn( name, name_length, DAT__SZNAM,  name_c);
   cnfImpn( type, type_length, DAT__SZTYP,  type_c);

#if defined(HDS_64)
/* Ensure that array subscripts are 64-bits         */
   for( i = 0; i<DAT__MXDIM; i++ )
      dims64[i] = dims[i];
      
/* Call pure C routine                                       */
   hdsNew( file_c, name_c, type_c, *ndim, dims64, locator_c, status );
#else
   hdsNew( file_c, name_c, type_c, *ndim, dims, locator_c, status );
#endif

/* Export returned locator */
   cnfExpch( locator_c, locator, DAT__SZLOC );
   
/* Free allocated string memory.                             */
   cnfFree( file_c );
}

F77_SUBROUTINE(hds_open)( CHARACTER(file),
                          CHARACTER(mode),
                          CHARACTER(locator),
                          F77_INTEGER_TYPE *status
                          TRAIL(file)
                          TRAIL(mode)
	                  TRAIL(locator) )
{

/*=========================================*/
/* HDS_OPEN - Open existing container file */
/*=========================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char *file_c;
   char mode_c[DAT__SZMOD + 1];
   
/* Enter routine.	*/

/* Import FILE argument to C string */
   file_c = cnfCreim( file, file_length );

/* Import maxmimum length strings  */
   cnfImpn( mode, mode_length, DAT__SZMOD, mode_c );

/* Call pure C routine                                       */
   hdsOpen( file_c, mode_c, locator_c, status );

/* Export returned locator */
   cnfExpch( locator_c, locator, DAT__SZLOC );
   
/* Free allocated string memory.                             */
   cnfFree( file_c );
}

F77_SUBROUTINE(hds_show)( CHARACTER(topic),
                          F77_INTEGER_TYPE *status
                          TRAIL(topic) )
{

/*================================*/
/* HDS_SHOW - Show HDS statistics */
/*================================*/

/* Local variables.     */
   char *topic_c;
   
/* Enter routine.	*/

/* Import TOPIC argument to C string */
   topic_c = cnfCreim( topic, topic_length );

/* Call pure C routine                                       */
   hdsShow( topic_c, status);

/* Free allocated string memory.                             */
   cnfFree( topic_c );
}

F77_SUBROUTINE(hds_state) (int *state,
                           int *status )
{
/*================================================*/
/* HDS_STATE - Enquire the current state of HDS   */
/*================================================*/

/* Call pure C routine                                                     */
   hdsState( state, status );
}

F77_SUBROUTINE(hds_stop) ( F77_INTEGER_TYPE *status )
{
/*=============================*/
/* HDS_STOP - Close down HDS   */
/*=============================*/

/* Call pure C routine */
   hdsStop( status );
}

F77_SUBROUTINE(hds_trace)( CHARACTER(locator),
			   F77_INTEGER_TYPE *nlev,
                           CHARACTER(path),
                           CHARACTER(file),
                           F77_INTEGER_TYPE *status
                           TRAIL(locator)
                           TRAIL(path)
                           TRAIL(file) )
{

/*===============================*/
/* HDS_TRACE - Trace object path */
/*===============================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char path_c[STR_K_LENGTH];
   char file_c[STR_K_LENGTH];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   hdsTrace( locator_c, nlev, path_c, file_c, status,
            STR_K_LENGTH, STR_K_LENGTH );

/* Export the returned C strings to the FORTRAN variables    */
   cnfExprt( path_c, path, path_length );
   cnfExprt( file_c, file, file_length );
}

F77_SUBROUTINE(hds_tune) ( CHARACTER(param_str),
                           F77_INTEGER_TYPE *value,
                           F77_INTEGER_TYPE *status
                           TRAIL(param_str) )
{
/*=========================================*/
/* HDS_TUNE - Set HDS tuning parameter     */
/*=========================================*/

/* Local variables.     */
   char *param_str_c;
 
/* Enter routine.       */

/* Import PARAM argument to C string */

   param_str_c = cnfCreim( param_str, param_str_length );
   
/* Call pure C routine  */
   hdsTune( param_str_c, value, status );
   
/* Free allocated string memory.                             */
   cnfFree( param_str_c );
}

F77_SUBROUTINE(hds_wild) ( CHARACTER(fspec),
                           CHARACTER(mode),
                           F77_INTEGER_TYPE *iwld,
                           CHARACTER(locator),
                           F77_INTEGER_TYPE *status
                           TRAIL(fspec)
                           TRAIL(mode)
                           TRAIL(locator) )
{
/*=================================================================*/
/* HDS_WILD - Perform a wild-card search for HDS container files   */
/*=================================================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   char *fspec_c;
   char *mode_c;

/* Enter routine.       */

/* Import FSPEC argument to C string */
   fspec_c = cnfCreim( fspec, fspec_length );

/* Import FSPEC argument to C string */
   mode_c = cnfCreim( mode, mode_length );

/* Call pure C routine  */
   hdsWild( fspec_c, mode_c, iwld, locator_c, status );

/* Export returned locator */
   cnfExpch( locator_c, locator, DAT__SZLOC );
      
/* Free allocated string memory.                             */
   cnfFree( fspec_c );
   cnfFree( mode_c );
}

/*=================================================================*/
/*  Deprecated routines!                                           */   
/*=================================================================*/

F77_SUBROUTINE(dat_conv)( CHARACTER(locator),
                          CHARACTER(type),
                          F77_LOGICAL_TYPE *reply,
                          F77_INTEGER_TYPE *status
                          TRAIL(locator)
                          TRAIL(type))
{

/*=========================================*/
/* DAT_CONV - Enquire conversion possible? */
/*=========================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   int reply_c;
   char type_c[DAT__SZTYP+1];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Import "type" to C string  */
   cnfImpn( type, type_length, DAT__SZTYP,  type_c);

/* Call pure C routine                                       */
   datConv( locator_c, type_c, &reply_c, status );

/* Set FORTRAN logical return                                */
   if( reply_c )
      *reply = F77_TRUE;
   else
      *reply = F77_FALSE;   
}

F77_SUBROUTINE(hds_close)( CHARACTER(locator),
                           F77_INTEGER_TYPE *status
	                   TRAIL(locator) )
{

/*======================================================*/
/* HDS_CLOSE - Close container file (Obselete routine!) */
/*======================================================*/

/* Local variables.     */
   char locator_c[DAT__SZLOC];
   
/* Enter routine.	*/

/* Import the input locator string                  */
   cnfImpch( locator, DAT__SZLOC, locator_c );

/* Call pure C routine                                       */
   hdsClose( locator_c, status );
}
