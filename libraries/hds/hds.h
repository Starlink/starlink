#include "dat_par.h"

/* Relative location of type definitions depends on whether we are
   building the library or using the installed version */
#if HDS_INTERNAL_INCLUDES
#  include "hds_types.h"
#else
#  include "star/hds_types.h"
#endif

/*=================================*/
/* datAlter - Alter size of object */
/*=================================*/

int
datAlter(HDSLoc    *locator,
         int       ndim,
         hdsdim    dims[],
         int       *status);

/*==========================*/
/* datAnnul - Annul locator */
/*==========================*/

int
datAnnul(HDSLoc    **locator,
         int       *status);

/*==============================================*/
/* datBasic - Map data (in basic machine units) */
/*==============================================*/

int
datBasic(HDSLoc    *locator,
         char      *mode_c,
         unsigned char **pntr,
         int       *len,
         int       *status);

/*=====================================*/
/* datCcopy - copy one structure level */
/*=====================================*/

int
datCcopy(HDSLoc   *locator1,
         HDSLoc   *locator2,
         char     *name,
         HDSLoc   **locator3,
         int      *status );

/*=======================================*/
/* datCctyp - construct _CHAR*nnn string */
/*=======================================*/

void
datCctyp( size_t size,
          char   type[DAT__SZTYP+1] );


/*===========================================*/
/* datCell - Locate a "cell" (array element) */
/*===========================================*/

int
datCell( HDSLoc   *locator1,
         int      ndim,
         hdsdim   subs[],
	 HDSLoc   **locator2,
         int      *status);

/*==========================================*/
/* datClen - Obtain character string length */
/*==========================================*/

int
datClen( HDSLoc   *locator,
         size_t   *clen,
         int      *status );

/*===========================*/
/* datClone - clone locator */
/*===========================*/

int
datClone(HDSLoc   *locator1,
	 HDSLoc   **locator2,
         int      *status);

/*================================*/
/* datCoerc - coerce object shape */
/*================================*/

int
datCoerc(HDSLoc   *locator1,
         int      ndim,
	 HDSLoc   **locator2,
         int      *status);

/*=======================*/
/* datCopy - copy object */
/*=======================*/

int
datCopy(HDSLoc    *locator1,
	HDSLoc    *locator2,
        char      *name_c,
        int       *status );

/*============================================================*/
/* datDrep - Obtain primitive data representation information */
/*============================================================*/

int
datDrep(HDSLoc    *locator,
        char      **format_str,
        char      **order_str,
        int       *status);

/*========================================*/
/* datErase - Erase object                */
/*========================================*/

int
datErase(HDSLoc   *locator,
         char     *name_str,
         int      *status);

/*===========================================================*/
/* datErmsg - Translate a status value into an error message */
/*===========================================================*/

int
datErmsg(int      status,
         size_t   *len,
         char     **msg_str);

/*================================*/
/* datFind - Find named component */
/*================================*/

int
datFind( HDSLoc   *locator1,
         char     *name_str,
	 HDSLoc   **locator2,
         int      *status );

/*============================*/
/* datGet - Read primitive(s) */
/*============================*/

int
datGet(HDSLoc     *locator,
       char       *type_str,
       int        ndim,
       hdsdim     dims[],
       void       *values,
       int        *status);

/*===================================*/
/* datGetC - Read _CHAR primitive(s) */
/*===================================*/

int
datGetC(HDSLoc    *locator,
        int       ndim,
        hdsdim    dims[],
        char      values[],
        size_t    char_len,
        int       *status);

/*=====================================*/
/* datGetD - Read _DOUBLE primitive(s) */
/*=====================================*/

int
datGetD(HDSLoc    *locator,
        int       ndim,
        hdsdim    dims[],
        double    values[],
        int       *status);

/*======================================*/
/* datGetI - Read _INTEGER primitive(s) */
/*======================================*/

int
datGetI(HDSLoc    *locator,
        int       ndim,
        hdsdim    dims[],
        int       values[],
        int       *status);

/*======================================*/
/* datGetL - Read _LOGICAL primitive(s) */
/*======================================*/

int
datGetL(HDSLoc    *locator,
        int       ndim,
        hdsdim    dims[],
        int       values[],
        int       *status);

/*===================================*/
/* datGetR - Read _REAL primitive(s) */
/*===================================*/

int
datGetR(HDSLoc    *locator,
        int       ndim,
        hdsdim    dims[],
        float     values[],
        int       *status);

/*======================================*/
/* datGet0C - Read scalar string value  */
/*======================================*/

int
datGet0C( HDSLoc * loc, char * value, size_t len, int * status );

/*======================================*/
/* datGet0D - Read scalar double value  */
/*======================================*/

int
datGet0D( HDSLoc * loc, double * value, int * status );

/*=====================================*/
/* datGet0R - Read scalar float value  */
/*=====================================*/

int
datGet0R( HDSLoc * loc, float * value, int * status );

/*=======================================*/
/* datGet0I - Read scalar integer value  */
/*=======================================*/

int
datGet0I( HDSLoc * loc, int * value, int * status );

/*=======================================*/
/* datGet0L - Read scalar logical value  */
/*=======================================*/

int
datGet0L( HDSLoc * loc, int * value, int * status );

/*======================================*/
/* datIndex - Index into component list */
/*======================================*/

int
datIndex(HDSLoc   *locator1,
         int      index,
         HDSLoc   **locator2,
         int      *status );

/*===================================*/
/* datLen - Inquire primitive length */
/*===================================*/

int
datLen(const HDSLoc *locator,
       size_t       *len,
       int          *status);

/*===========================*/
/* datMap - Map primitive(s) */
/*===========================*/

int
datMap(HDSLoc     *locator,
       char       *type_str,
       char       *mode_str,
       int        ndim,
       hdsdim     dims[],
       void       **pntr,
       int        *status);

/*==================================*/
/* datMapC - Map _CHAR primitive(s) */
/*==================================*/

int
datMapC(HDSLoc    *locator,
        char      *mode_str,
        int       ndim,
        hdsdim    dims[],
        unsigned char **pntr,
        int       *status );

/*====================================*/
/* datMapD - Map _DOUBLE primitive(s) */
/*====================================*/

int
datMapD(HDSLoc    *locator,
        char      *mode_str,
        int       ndim,
        hdsdim    dims[],
        double    **pntr,
        int       *status );

/*=====================================*/
/* datMapI - Map _INTEGER primitive(s) */
/*=====================================*/

int
datMapI(HDSLoc    *locator,
        char      *mode_str,
        int       ndim,
        hdsdim    dims[],
        int       **pntr,
        int       *status );

/*=====================================*/
/* datMapL - Map _LOGICAL primitive(s) */
/*=====================================*/

int
datMapL(HDSLoc *locator,
        char      *mode_str,
        int       ndim,
        hdsdim dims[],
        int       **pntr,
        int       *status );
        
/*==================================*/
/* datMapR - Map _REAL primitive(s) */
/*==================================*/

int
datMapR(HDSLoc *locator,
        char      *mode_str,
        int       ndim,
        hdsdim dims[],
        float     **pntr,
        int       *status );


/*==================================*/
/* datMapV - Map vectorized primitive(s) */
/*==================================*/

int
datMapV(HDSLoc    *locator,
        char      *type_str,
        char      *mode_str,
        void      **pntr,
        size_t    *actval,
        int       *status );

        
/*==================================*/
/* datMould - Alter shape of object */
/*==================================*/

int
datMould(HDSLoc *locator,
          int       ndim,
          hdsdim dims[],
          int       *status);

/*=======================*/
/* datMove - Move object */
/*=======================*/

int
datMove(HDSLoc **locator1,
        HDSLoc *locator2,
        char *name_str,
        int *status);

/*======================================*/
/* datMsg - store filename in EMS token */
/*======================================*/

void
datMsg( const char * token, 
        const HDSLoc * loc );

/*===============================*/
/* datName - Enquire object name */
/*===============================*/

int
datName(const HDSLoc *locator,
        char name_str[DAT__SZNAM+1],
        int *status);

/*=========================================*/
/* datNcomp - Inquire number of components */
/*=========================================*/

int
datNcomp( const HDSLoc *locator,
          int *ncomp,
          int *status);

/*===============================*/
/* datNew - Create new component */
/*===============================*/

int
datNew( HDSLoc    *locator,
        char      *name_str,
        char      *type_str,
        int       ndim,
        hdsdim    dims[],
        int       *status);

/*============================================*/
/* datNewC - Create new _CHAR type component */
/*============================================*/

int
datNewC(HDSLoc    *locator,
        char      *name_str,
        size_t    len,
        int       ndim,
        hdsdim    dims[],
        int       *status);

/*====================================*/
/* datParen - Locate parent structure */
/*====================================*/

int
datParen(HDSLoc *locator1,
         HDSLoc **locator2,
         int *status);


/*=====================================*/
/* datPrec - Enquire storage precision */
/*=====================================*/

int
datPrec(const HDSLoc *locator,
        size_t *nbytes,
        int *status);

/*====================================*/
/* datPrim - Enquire object primitive */
/*====================================*/

int
datPrim(const HDSLoc *locator,
        int *prim,
        int *status);

/*=========================================================*/
/* datPrmry - Set/Enquire primary/secondary locator status */
/*=========================================================*/

int
datPrmry(int set,
         HDSLoc **locator,
         int *prmry,
         int *status);

/*==================================*/
/* datPutC - Write _CHAR primitive */
/*==================================*/

int
datPutC( HDSLoc *locator,
         int       ndim,
         hdsdim dims[],
         char      *string,
         size_t    string_length,
         int       *status);

/*====================================*/
/* datPutD - Write _DOUBLE primitives */
/*====================================*/

int
datPutD( HDSLoc *locator,
         int       ndim,
         hdsdim dims[],
         double    *values,
         int       *status);

/*=====================================*/
/* datPutI - Write _INTEGER primitives */
/*=====================================*/

int
datPutI( HDSLoc *locator,
         int     ndim,
         hdsdim dims[],
         int     *values,
         int     *status);

/*==================================*/
/* datPutR - Write _REAL primitives */
/*==================================*/

int
datPutR( HDSLoc *locator,
         int       ndim,
         hdsdim dims[],
         float     *values,
         int       *status);

/*=====================================*/
/* datPutL - Write _LOGICAL primitives */
/*=====================================*/

int
datPutL( HDSLoc *locator,
         int       ndim,
         hdsdim dims[],
         int       *values,
         int       *status);

/*==========================*/
/* datPut - Write primitive */
/*==========================*/

int
datPut( HDSLoc  *locator,
        char    *type_str,
        int     ndim,
        hdsdim  dims[],
        void    *values,
        int     *status);

/*=======================================*/
/* datPut0C - Write scalar string value  */
/*=======================================*/

int
datPut0C( HDSLoc * loc, char * value, int * status );

/*=======================================*/
/* datPut0D - Write scalar double value  */
/*=======================================*/

int
datPut0D( HDSLoc * loc, double value, int * status );

/*======================================*/
/* datPut0R - Write scalar float value  */
/*======================================*/

int
datPut0R( HDSLoc * loc, float value, int * status );

/*========================================*/
/* datPut0I - Write scalar integer value  */
/*========================================*/

int
datPut0I( HDSLoc * loc, int value, int * status );

/*========================================*/
/* datPut0L - Write scalar logical value  */
/*========================================*/

int
datPut0L( HDSLoc * loc, int value, int * status );

/*===================================================*/
/* datRefct - Enquire container file reference count */
/*===================================================*/

int
datRefct(HDSLoc *locator,
         int *refct,
         int *status);

/*=============================*/
/* datRenam - Rename an object */
/*=============================*/

int
datRenam(HDSLoc *locator,
          char *name_str,
          int  *status);

/*================================*/
/* datReset - Reset object state */
/*================================*/

int 
datReset(HDSLoc *locator,
          int *status);

/*================================*/
/* datRetyp - Change object type */
/*================================*/

int
datRetyp(HDSLoc *locator,
          char *type_str,
          int *status);

/*=================================*/
/* datShape - Enquire object shape */
/*=================================*/

int
datShape(const HDSLoc *locator,
         int       maxdim,
         hdsdim dims[],
         int       *actdim,
         int       *status);

/*===============================*/
/* datSize - Enquire object size */
/*===============================*/

int
datSize(const HDSLoc *locator,
        size_t *size,
        int *status );

/*================================*/
/* datSlice - Locate object slice */
/*================================*/

int
datSlice(HDSLoc  *locator1,
         int     ndim,
         hdsdim  lower[],
         hdsdim  upper[],
         HDSLoc  **locator2,
         int     *status );

/*=================================*/
/* datState - Enquire object state */
/*=================================*/

int   
datState(const HDSLoc *locator,
         int *state,
         int *status);

/*=====================================*/
/* datStruc - Enquire object structure */
/*=====================================*/

int
datStruc(const HDSLoc *locator,
         int *struc,
         int *status);

/*===================================*/
/* datTemp - Create temporary object */
/*===================================*/

int
datTemp(char      *type_str,
        int       ndim,
        hdsdim    dims[],
        HDSLoc    **locator,
        int       *status);

/*=========================================*/
/* datThere - Enquire component existence */
/*=========================================*/

int
datThere(const HDSLoc *locator,
         char *name_c,
         int *there,
         int *status);

/*===============================*/
/* datType - Enquire object type */
/*===============================*/

int
datType(const HDSLoc *locator,
        char type_str[DAT__SZTYP + 1],
        int *status );

/*=========================*/
/* datUnmap - Unmap object */
/*=========================*/

int
datUnmap(HDSLoc *locator,
         int  *status);

/*==================================*/
/* datValid - Enquire locator valid */
/*==================================*/

int
datValid(const HDSLoc *locator,
         int    *valid,
         int    *status);

/*===========================*/
/* datVec - Vectorise object */
/*===========================*/

int
datVec(HDSLoc *locator1,
       HDSLoc **locator2,
       int    *status );

/*================================================*/
/* datWhere - Find primitive position in HDS file */
/*            Currently not part of the public    */
/*            C API                               */
/*================================================*/

/*==================================================*/
/* hdsCopy - Copy an object to a new container file */
/*==================================================*/

int
hdsCopy(HDSLoc *locator,
        char *file_str,
        char name_str[DAT__SZNAM],
        int *status );

/*=================================*/
/* hdsErase - Erase container file */
/*=================================*/

int
hdsErase(HDSLoc **locator,
         int *status);

/*===============================================================*/
/* hdsEwild - End a wild card search for HDS container files     */
/*===============================================================*/

int
hdsEwild( int *iwld,
          int *status);

/*================================*/
/* hdsFlush - Flush locator group */
/*=================================*/

int
hdsFlush( char *group_str,
          int *status);

/*===============================*/
/* hdsFree - Free container file */
/*===============================*/

int
hdsFree(HDSLoc *locator,
        int *status);

/*==================================*/
/* hdsGroup - Enquire locator group */
/*==================================*/

int
hdsGroup(HDSLoc *locator,
         char group_str[DAT__SZGRP+1],
         int *status);

/*=========================================*/
/* hdsGtune - Get HDS tuning parameter     */
/*=========================================*/

int
hdsGtune(char *param_str,
         int *value,
         int *status);

/*=================================*/
/* hdsLink - Link locator to group */
/*=================================*/

int
hdsLink(HDSLoc *locator,
        char *group_str,
        int *status);

/*================================*/
/* hdsLock - Lock container file */
/*================================*/

int
hdsLock(HDSLoc *locator,
        int *status);

/*====================================*/
/* hdsNew - Create new container file */
/*====================================*/

int
hdsNew(char *file_str,
       char *name_str,
       char *type_str,
       int  ndim,
       hdsdim  dims[],
       HDSLoc **locator,
       int *status);

/*========================================*/
/* hdsOpen - Open existing container file */
/*========================================*/

int
hdsOpen(char *file_str,
        char *mode_str,
        HDSLoc **locator,
        int *status);

/*===============================*/
/* hdsShow - Show HDS statistics */
/*===============================*/

int
hdsShow(char *topic_str,
        int  *status);

/*===============================================*/
/* hdsState - Enquire the current state of HDS   */
/*===============================================*/

int
hdsState( int *state,
          int *status);

/*============================*/
/* hdsStop - Close down HDS   */
/*============================*/

int
hdsStop( int *status);

/*==============================*/
/* hdsTrace - Trace object path */
/*==============================*/

int
hdsTrace(const HDSLoc *locator,
         int  *nlev,
         char *path_str,
         char *file_str,
         int  *status,
         size_t path_length,
         size_t file_length);

/*========================================*/
/* hdsTune - Set HDS tuning parameter     */
/*========================================*/

int
hdsTune(char *param_str,
        int  *value,
        int  *status);

/*=================================================================*/
/* hdsWild - Perform a wild-card search for HDS container files   */
/*=================================================================*/

int 
hdsWild(char *fspec,
        char *mode,
        int *iwld,
        HDSLoc **loc,
        int *status);

/*=================================================================*/
/*  Deprecated routines!                                           */   
/*=================================================================*/

/*========================================*/
/* datConv - Enquire conversion possible? */
/*========================================*/

int
datConv(const HDSLoc *locator,
        char *type_str,
        int *conv,
        int *status);

/*=====================================================*/
/* hdsClose - Close container file (Obselete routine!) */
/*=====================================================*/

int
hdsClose(HDSLoc **locator,
        int *status);
