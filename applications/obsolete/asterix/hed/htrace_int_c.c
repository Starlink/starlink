#include "sae_par.h"
#include "/star/starlink/lib/hds/dat_par.h"

#include "f77.h"
#include "cnf.h"


#define _ok(x) ((x)==SAI__OK)

/*  htrace_int0 - Scan the HDS structure from the given object downwards
 *           
 *   Description
 *
 *   Author :
 *
 *    David J. Allan (BHVAD::DJA)
 *
 *   History :
 *
 *    15 Nov 93 : Original (DJA)
 *
 *   Import :
 *
 *     level    - Current level within scope
 *     objloc	- The raw locator data for the object
 *
 *   Export :
 *
 *     newnode  - Pointer to the new node reference
 *
 */

void htrace_int0( CHARACTER(objloc), 
		  LOGICAL(full_struct),
		  int level,
                  int blank_after_object,
                  int suppress_func,
                  void (*func)(),
                  INTEGER(status) TRAIL(objloc) )
  {
  GENPTR_CHARACTER(objloc)
  GENPTR_LOGICAL(full_struct)
  GENPTR_INTEGER(status)

  static DECLARE_INTEGER(mxdim) 	/* Size of dimensions arrays */
	   = DAT__MXDIM;	

  DECLARE_INTEGER_ARRAY(bnd,DAT__MXDIM);/* Cell no. of structure array */
  DECLARE_CHARACTER(cloc,DAT__SZLOC);	/* Sub-component of 'objloc' */
  DECLARE_INTEGER_ARRAY(dims,DAT__MXDIM);
  DECLARE_LOGICAL(doblank);		/* Print a blank line? */
  DECLARE_INTEGER(i);			/* Loop over sub-components */
  DECLARE_INTEGER(icomp);		/* Loop over sub-components */
  DECLARE_INTEGER(idim);		/* Loop over dimensions */
  DECLARE_INTEGER(ncomp);		/* Number of structure components */
  DECLARE_INTEGER(ndim);		/* Object dimensionality */
  DECLARE_INTEGER(ok);			/*  */
  DECLARE_LOGICAL(prim);		/* Is object primitve? */
  DECLARE_LOGICAL(valid);		/* Is object valid? */

  if ( !_ok(*status) )			/* Check status */
    return;

  F77_CALL(dat_prim)(                   /* Is object primitive? */
	    CHARACTER_ARG(objloc), 	
	    LOGICAL_ARG(&prim), 
            INTEGER_ARG(status) 
            TRAIL_ARG(objloc) );

  F77_CALL(dat_valid)(              	/* Is object data primitive? */
	   CHARACTER_ARG(objloc), 
	   LOGICAL_ARG(&valid), 
           INTEGER_ARG(status) 
           TRAIL_ARG(objloc) );

  F77_CALL(dat_shape)(			/* Get object's dimensions */
	   CHARACTER_ARG(objloc), 
	   INTEGER_ARG(&mxdim), 
	   INTEGER_ARRAY_ARG(dims), 
	   INTEGER_ARG(&ndim), 
           INTEGER_ARG(status) 
           TRAIL_ARG(objloc) );

  if ( blank_after_object )		/* Print a blank for last elements */
    doblank = F77_TRUE;
  else
    doblank = F77_FALSE;

  if ( ! suppress_func )                /* Invoke user procedure for object */
    (*func)( 
           CHARACTER_ARG(objloc), 	
           INTEGER_ARG(&level),
	   INTEGER_ARG(&ndim), 
	   INTEGER_ARRAY_ARG(dims), 
           LOGICAL_ARG(&doblank),
           INTEGER_ARG(status) 
           TRAIL_ARG(objloc) );

  if ( F77_ISTRUE(valid) &&             /* Object is valid scalar structure */
       F77_ISFALSE(prim) &&
       (ndim==0) )
    { 
    F77_CALL(dat_ncomp)(                /* Number of sub-components */
               CHARACTER_ARG(objloc), 	
               INTEGER_ARG(&ncomp), 
               INTEGER_ARG(status) 
               TRAIL_ARG(objloc) );

    for ( icomp=1 ; icomp<=ncomp ; 	/* Loop over components */
          icomp++ )
      {
      F77_CALL(dat_index)(              /* Locate the sub-component */
                 CHARACTER_ARG(objloc), 
		 INTEGER_ARG(&icomp),
                 CHARACTER_ARG(cloc), 
		 INTEGER_ARG(status) 
                 TRAIL_ARG(objloc) 
		 TRAIL_ARG(cloc) );
        

      htrace_int0( CHARACTER_ARG(cloc),	/* Scan in its information */
                 LOGICAL_ARG(full_struct),
                 level+1, 
                 (icomp==ncomp)&&(ncomp>1),
                 0, func, 
                 INTEGER_ARG(status) 
                 TRAIL_ARG(cloc) );

      F77_CALL(dat_annul)(              /* Free this child object */
		 CHARACTER_ARG(cloc),	
                 INTEGER_ARG(status)
                 TRAIL_ARG(cloc) );
      }
    }

  else if ( F77_ISTRUE(valid) &&        /* Object is valid structure array */
       F77_ISFALSE(prim) &&
       (ndim>0) )
    { 
    ncomp = 1;				/* Default no. of elements to dump */

    if ( F77_ISTRUE(*full_struct) )	/* Full structure dump? */
      {
      for ( i = 0; i<ndim; i++ )	/* Find total number of elements */
        ncomp *= dims[i];
      } 

    for ( i = 0; i<ndim; i++ )		/* Define bounds of slice */
      bnd[i] = 1;

    for ( i = ncomp; i>0; i-- )		/* Loop over components to dump */
      {
      F77_CALL(htrace_sarray)( 
           CHARACTER_ARG(objloc), 	/* Announce structure array */
           INTEGER_ARG(&level),
	   INTEGER_ARG(&ndim), 
	   INTEGER_ARRAY_ARG(bnd), 
           INTEGER_ARG(status) 
           TRAIL_ARG(objloc) );

      F77_CALL(dat_cell)(              /* Locate the sub-component */
                 CHARACTER_ARG(objloc), 
		 INTEGER_ARG(&ndim),
		 INTEGER_ARRAY_ARG(bnd),
                 CHARACTER_ARG(cloc), 
		 INTEGER_ARG(status) 
                 TRAIL_ARG(objloc) 
		 TRAIL_ARG(cloc) );
        

      htrace_int0( CHARACTER_ARG(cloc),	/* Scan in its information */
                 LOGICAL_ARG(full_struct),
                 level, 1, 1, func, 
                 INTEGER_ARG(status) 
                 TRAIL_ARG(cloc) );

      F77_CALL(dat_annul)(              /* Free this child object */
		 CHARACTER_ARG(cloc),	
                 INTEGER_ARG(status)
                 TRAIL_ARG(cloc) );

      ok = 0;				/* Find next element index */
      idim = 0;
      do 
        {
        bnd[idim] += 1;
        if ( bnd[idim] <= dims[idim] )
          ok = 1;
        else
          {
          bnd[idim] = 1;
          idim++;
          }
        }
      while ( (idim<ndim) && ! ok );
      }
    }

  }



/*  HTRACE_INT - Call the user supplied procedure for every object inside TOPLOC
 *           
 *   Description
 *
 *   Author :
 *
 *    David J. Allan (BHVAD::DJA)
 *
 *   History :
 *
 *    15 Nov 93 : Original (DJA)
 *
 *   Import :
 *
 *     scope    - Scope of the object
 *     level    - Current level within scope
 *     toploc	- The raw locator data for the object
 */
F77_SUBROUTINE(htrace_int)( CHARACTER(toploc), 
                            LOGICAL(full_struct),
                            void (* func)(), 
                            INTEGER(status) TRAIL(toploc) )
  {
  GENPTR_CHARACTER(toploc)
  GENPTR_LOGICAL(full_struct)
  GENPTR_INTEGER(status)

  if ( !_ok(*status) )			/* Check status */
    return;

  htrace_int0( CHARACTER_ARG(toploc), 
               LOGICAL_ARG(full_struct),
	       0, 0, 0, func,
               INTEGER_ARG(status) 
               TRAIL_ARG(toploc) );
  }
