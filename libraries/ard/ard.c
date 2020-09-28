/*
 *  Name:
 *     ard.c

 *  Purpose:
 *     Implement the C interface to the ARD library.

 *  Description:
 *     This module implements C-callable wrappers for the public
 *     routines in the ARD library. The interface to these wrappers
 *     is defined in ard.h.

 *  Notes:
 *     - This interface is not complete, extend it as required.

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     07-JUL-2006 (PWD):
 *        Original version.
 *     {enter_further_changes_here}

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA
 */

/* Header files. */
/* ============= */
#include <string.h>
#include "f77.h"
#include "ard.h"
#include "star/grp.h"

/* Prototypes of ARD Fortran routines. */
F77_SUBROUTINE(ard_work)( INTEGER(igrp),
                          INTEGER(ndim),
                          INTEGER_ARRAY(lbnd),
                          INTEGER_ARRAY(ubnd),
                          REAL_ARRAY(trcoef),
                          LOGICAL(concat),
                          INTEGER(regval),
                          INTEGER_ARRAY(mask),
                          INTEGER_ARRAY(lbndi),
                          INTEGER_ARRAY(ubndi),
                          INTEGER_ARRAY(lbnde),
                          INTEGER_ARRAY(ubnde),
                          INTEGER(status) );

F77_SUBROUTINE(ard_work8)( INTEGER(igrp),
                           INTEGER(ndim),
                           INTEGER8_ARRAY(lbnd),
                           INTEGER8_ARRAY(ubnd),
                           REAL_ARRAY(trcoef),
                           LOGICAL(concat),
                           INTEGER(regval),
                           INTEGER_ARRAY(mask),
                           INTEGER8_ARRAY(lbndi),
                           INTEGER8_ARRAY(ubndi),
                           INTEGER8_ARRAY(lbnde),
                           INTEGER8_ARRAY(ubnde),
                           INTEGER(status) );

F77_SUBROUTINE(ard_grpex)( CHARACTER(desc),
                           INTEGER(igrp1),
                           INTEGER(igrp2),
                           LOGICAL(flag),
                           INTEGER(status)
                           TRAIL(desc) );


/* Interface wrappers. */
void ardWork( const Grp *grp,
              int ndim,
              const int *lbnd,
              const int *ubnd,
              const float *trcoef,
              int concat,
              int *regval,
              int *mask,
              int *lbndi,
              int *ubndi,
              int *lbnde,
              int *ubnde,
              int *status ) {

    /* Declare Fortran variables */
    DECLARE_INTEGER(figrp);
    DECLARE_INTEGER(fndim);
    DECLARE_INTEGER_ARRAY_DYN(flbnd);
    DECLARE_INTEGER_ARRAY_DYN(fubnd);
    DECLARE_REAL_ARRAY_DYN(ftrcoef);
    DECLARE_LOGICAL(fconcat);
    DECLARE_INTEGER(fregval);
    DECLARE_INTEGER_ARRAY_DYN(fmask);
    DECLARE_INTEGER_ARRAY_DYN(flbndi);
    DECLARE_INTEGER_ARRAY_DYN(fubndi);
    DECLARE_INTEGER_ARRAY_DYN(flbnde);
    DECLARE_INTEGER_ARRAY_DYN(fubnde);
    DECLARE_INTEGER(fstatus);

    /* Export C arguments to Fortran. Note that the Grp identifier needs
     * special handling */
    figrp = grpC2F( grp, status );

    F77_EXPORT_INTEGER(ndim,fndim);

    F77_CREATE_INTEGER_ARRAY(flbnd,ndim);
    F77_EXPORT_INTEGER_ARRAY(lbnd,flbnd,ndim);

    F77_CREATE_INTEGER_ARRAY(fubnd,ndim);
    F77_EXPORT_INTEGER_ARRAY(ubnd,fubnd,ndim);

    F77_CREATE_REAL_ARRAY(ftrcoef,(ndim+1)*ndim);
    F77_EXPORT_REAL_ARRAY(trcoef,ftrcoef,(ndim+1)*ndim);

    F77_EXPORT_LOGICAL(concat,fconcat);

    F77_EXPORT_INTEGER(*regval,fregval);

    /* Note size is incorrect for mask, but doesn't matter unless C int !=
     * Fortran INTEGER, in which case we should calculate it correctly. */
    F77_CREATE_INTEGER_ARRAY(fmask,ndim);
    F77_ASSOC_INTEGER_ARRAY(fmask,mask);

    F77_CREATE_INTEGER_ARRAY(flbndi,ndim);
    F77_ASSOC_INTEGER_ARRAY(flbndi,lbndi);

    F77_CREATE_INTEGER_ARRAY(fubndi,ndim);
    F77_ASSOC_INTEGER_ARRAY(fubndi,ubndi);

    F77_CREATE_INTEGER_ARRAY(flbnde,ndim);
    F77_ASSOC_INTEGER_ARRAY(flbnde,lbnde);

    F77_CREATE_INTEGER_ARRAY(fubnde,ndim);
    F77_ASSOC_INTEGER_ARRAY(fubnde,ubnde);

    F77_EXPORT_INTEGER(*status,fstatus);

    /* Do the work */
    F77_LOCK( F77_CALL(ard_work)( INTEGER_ARG(&figrp),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(flbnd),
                        INTEGER_ARRAY_ARG(fubnd),
                        REAL_ARRAY_ARG(ftrcoef),
                        LOGICAL_ARG(&fconcat),
                        INTEGER_ARG(&fregval),
                        INTEGER_ARRAY_ARG(fmask),
                        INTEGER_ARRAY_ARG(flbndi),
                        INTEGER_ARRAY_ARG(fubndi),
                        INTEGER_ARRAY_ARG(flbnde),
                        INTEGER_ARRAY_ARG(fubnde),
                        INTEGER_ARG(&fstatus) ); )

    /* Free local variables and import and results */
    F77_FREE_INTEGER(flbnd);
    F77_FREE_INTEGER(fubnd);
    F77_FREE_REAL(ftrcoef);

    F77_IMPORT_INTEGER(fregval,*regval);

    /*F77_IMPORT_POINTER(fmask,mask);*/

    F77_IMPORT_INTEGER_ARRAY(flbndi,lbndi,ndim);
    F77_FREE_INTEGER(flbndi);

    F77_IMPORT_INTEGER_ARRAY(fubndi,ubndi,ndim);
    F77_FREE_INTEGER(fubndi);

    F77_IMPORT_INTEGER_ARRAY(flbnde,lbnde,ndim);
    F77_FREE_INTEGER(flbnde);

    F77_IMPORT_INTEGER_ARRAY(fubnde,ubnde,ndim);
    F77_FREE_INTEGER(fubnde);

    F77_IMPORT_INTEGER(fstatus,*status);

    return;
}


void ardWork8( const Grp *grp,
              int ndim,
              const int64_t *lbnd,
              const int64_t *ubnd,
              const float *trcoef,
              int concat,
              int *regval,
              int *mask,
              int64_t *lbndi,
              int64_t *ubndi,
              int64_t *lbnde,
              int64_t *ubnde,
              int *status ) {

    /* Declare Fortran variables */
    DECLARE_INTEGER(figrp);
    DECLARE_INTEGER(fndim);
    DECLARE_INTEGER8_ARRAY_DYN(flbnd);
    DECLARE_INTEGER8_ARRAY_DYN(fubnd);
    DECLARE_REAL_ARRAY_DYN(ftrcoef);
    DECLARE_LOGICAL(fconcat);
    DECLARE_INTEGER(fregval);
    DECLARE_INTEGER_ARRAY_DYN(fmask);
    DECLARE_INTEGER8_ARRAY_DYN(flbndi);
    DECLARE_INTEGER8_ARRAY_DYN(fubndi);
    DECLARE_INTEGER8_ARRAY_DYN(flbnde);
    DECLARE_INTEGER8_ARRAY_DYN(fubnde);
    DECLARE_INTEGER(fstatus);

    /* Export C arguments to Fortran. Note that the Grp identifier needs
     * special handling */
    figrp = grpC2F( grp, status );

    F77_EXPORT_INTEGER(ndim,fndim);

    F77_CREATE_INTEGER8_ARRAY(flbnd,ndim);
    F77_EXPORT_INTEGER8_ARRAY(lbnd,flbnd,ndim);

    F77_CREATE_INTEGER8_ARRAY(fubnd,ndim);
    F77_EXPORT_INTEGER8_ARRAY(ubnd,fubnd,ndim);

    F77_CREATE_REAL_ARRAY(ftrcoef,(ndim+1)*ndim);
    F77_EXPORT_REAL_ARRAY(trcoef,ftrcoef,(ndim+1)*ndim);

    F77_EXPORT_LOGICAL(concat,fconcat);

    F77_EXPORT_INTEGER(*regval,fregval);

    /* Note size is incorrect for mask, but doesn't matter unless C int !=
     * Fortran INTEGER, in which case we should calculate it correctly. */
    F77_CREATE_INTEGER_ARRAY(fmask,ndim);
    F77_ASSOC_INTEGER_ARRAY(fmask,mask);

    F77_CREATE_INTEGER8_ARRAY(flbndi,ndim);
    F77_ASSOC_INTEGER8_ARRAY(flbndi,lbndi);

    F77_CREATE_INTEGER8_ARRAY(fubndi,ndim);
    F77_ASSOC_INTEGER8_ARRAY(fubndi,ubndi);

    F77_CREATE_INTEGER8_ARRAY(flbnde,ndim);
    F77_ASSOC_INTEGER8_ARRAY(flbnde,lbnde);

    F77_CREATE_INTEGER8_ARRAY(fubnde,ndim);
    F77_ASSOC_INTEGER8_ARRAY(fubnde,ubnde);

    F77_EXPORT_INTEGER(*status,fstatus);

    /* Do the work */
    F77_LOCK( F77_CALL(ard_work8)( INTEGER_ARG(&figrp),
                        INTEGER_ARG(&fndim),
                        INTEGER8_ARRAY_ARG(flbnd),
                        INTEGER8_ARRAY_ARG(fubnd),
                        REAL_ARRAY_ARG(ftrcoef),
                        LOGICAL_ARG(&fconcat),
                        INTEGER_ARG(&fregval),
                        INTEGER_ARRAY_ARG(fmask),
                        INTEGER8_ARRAY_ARG(flbndi),
                        INTEGER8_ARRAY_ARG(fubndi),
                        INTEGER8_ARRAY_ARG(flbnde),
                        INTEGER8_ARRAY_ARG(fubnde),
                        INTEGER_ARG(&fstatus) ); )

    /* Free local variables and import and results */
    F77_FREE_INTEGER8(flbnd);
    F77_FREE_INTEGER8(fubnd);
    F77_FREE_REAL(ftrcoef);

    F77_IMPORT_INTEGER(fregval,*regval);

    /*F77_IMPORT_POINTER(fmask,mask);*/

    F77_IMPORT_INTEGER8_ARRAY(flbndi,lbndi,ndim);
    F77_FREE_INTEGER(flbndi);

    F77_IMPORT_INTEGER8_ARRAY(fubndi,ubndi,ndim);
    F77_FREE_INTEGER(fubndi);

    F77_IMPORT_INTEGER8_ARRAY(flbnde,lbnde,ndim);
    F77_FREE_INTEGER(flbnde);

    F77_IMPORT_INTEGER8_ARRAY(fubnde,ubnde,ndim);
    F77_FREE_INTEGER(fubnde);

    F77_IMPORT_INTEGER(fstatus,*status);

    return;
}


void ardGrpex( const char *desc,
               const Grp *grp1,
               Grp **grp2,
               int *flag,
               int *status ) {

    /* Declare Fortran variables */
    DECLARE_CHARACTER_DYN(fdesc);
    DECLARE_INTEGER(figrp1);
    DECLARE_INTEGER(figrp2);
    DECLARE_LOGICAL(fflag);
    DECLARE_INTEGER(fstatus);

    /* Export C arguments to Fortran. Note that the Grp identifiers need
     * special handling */
    F77_CREATE_CHARACTER(fdesc,strlen(desc));
    F77_EXPORT_CHARACTER(desc,fdesc,fdesc_length);

    figrp1 = grpC2F( grp1, status );
    figrp2 = grpC2F( *grp2, status );

    F77_EXPORT_INTEGER(*status,fstatus);

    /* Do the work */
    F77_LOCK( F77_CALL(ard_grpex)( CHARACTER_ARG(fdesc),
                         INTEGER_ARG(&figrp1),
                         INTEGER_ARG(&figrp2),
                         LOGICAL_ARG(&fflag),
                         INTEGER_ARG(&fstatus)
                         TRAIL_ARG(fdesc) ); )


    /* Free local variables and import and results */
    F77_FREE_CHARACTER(fdesc);

    *grp2 = grpF2C( figrp2, status );

    F77_IMPORT_LOGICAL(fflag,*flag);
    F77_IMPORT_INTEGER(fstatus,*status);

   return;
}
