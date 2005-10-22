#include "dat_par.h"
#include "hds_types.h"


/*=================================*/
/* datAlter - Alter size of object */
/*=================================*/

int
datAlter(char      locator_str[DAT__SZLOC],
         int       ndim,
         hdsdim    dims[],
         int       *status);

/*==========================*/
/* datAnnul - Annul locator */
/*==========================*/

int
datAnnul(char locator_str[DAT__SZLOC],
         int  *status);

/*==============================================*/
/* datBasic - Map data (in basic machine units) */
/*==============================================*/

int
datBasic(char locator_str[DAT__SZLOC],
         char *mode_c,
         unsigned char **pntr,
         int *len,
         int *status);

/*=====================================*/
/* datCcopy - copy one structure level */
/*=====================================*/

int
datCcopy(char locator1[DAT__SZLOC],
         char locator2[DAT__SZLOC],
         char *name,
         char locator3[DAT__SZLOC],
         int  *status );

/*===========================================*/
/* datCell - Locate a "cell" (array element) */
/*===========================================*/

int
datCell( char     loc1_str[DAT__SZLOC],
         int      ndim,
         hdsdim   subs[],
         char     loc2_str[DAT__SZLOC],
         int      *status);

/*==========================================*/
/* datClen - Obtain character string length */
/*==========================================*/

int
datClen( char locator_str[DAT__SZLOC],
         int *clen,
         int *status );

/*===========================*/
/* datClone - clone locator */
/*===========================*/

int
datClone(char locator1_str[DAT__SZLOC],
         char locator2_str[DAT__SZLOC],
         int *status);

/*================================*/
/* datCoerc - coerce object shape */
/*================================*/

int
datCoerc(char locator1_str[DAT__SZLOC],
         int ndim,
         char locator2_str[DAT__SZLOC],
         int *status);

/*=======================*/
/* datCopy - copy object */
/*=======================*/

int
datCopy(char locator1_str[DAT__SZLOC],
        char locator2_str[DAT__SZLOC],
        char *name_c,
        int  *status );

/*============================================================*/
/* datDrep - Obtain primitive data representation information */
/*============================================================*/

int
datDrep( char locator_str[DAT__SZLOC],
         char **format_str,
         char **order_str,
         int *status);

/*========================================*/
/* datErase - Erase object                */
/*========================================*/

int
datErase(char locator_str[DAT__SZLOC],
         char *name_str,
         int *status);


/*===========================================================*/
/* datErmsg - Translate a status value into an error message */
/*===========================================================*/

int
datErmsg(int  *status,
         int  *len,
         char **msg_str);

/*================================*/
/* datFind - Find named component */
/*================================*/

int
datFind( char loc1_str[DAT__SZLOC],
         char *name_str,
         char loc2_str[DAT__SZLOC],
         int  *status );

/*============================*/
/* datGet - Read primitive(s) */
/*============================*/

int
datGet(char       locator_str[DAT__SZLOC],
       char       *type_str,
       int        ndim,
       hdsdim  dims[],
       unsigned   char *values,
       int        *status);

/*===================================*/
/* datGetC - Read _CHAR primitive(s) */
/*===================================*/

int
datGetC(char      locator_str[DAT__SZLOC],
        int       ndim,
        hdsdim dims[],
        char      values[],
        int       char_len,
        int       *status);

/*=====================================*/
/* datGetD - Read _DOUBLE primitive(s) */
/*=====================================*/

int
datGetD(char      locator_str[DAT__SZLOC],
        int       ndim,
        hdsdim dims[],
        double    values[],
        int       *status);

/*======================================*/
/* datGetI - Read _INTEGER primitive(s) */
/*======================================*/

int
datGetI(char      locator_str[DAT__SZLOC],
        int       ndim,
        hdsdim dims[],
        int       values[],
        int       *status);

/*======================================*/
/* datGetL - Read _LOGICAL primitive(s) */
/*======================================*/

int
datGetL(char      locator_str[DAT__SZLOC],
        int       ndim,
        hdsdim dims[],
        int       values[],
        int       *status);

/*===================================*/
/* datGetR - Read _REAL primitive(s) */
/*===================================*/

int
datGetR(char      locator_str[DAT__SZLOC],
        int       ndim,
        hdsdim dims[],
        float     values[],
        int       *status);

/*======================================*/
/* datIndex - Index into component list */
/*======================================*/

int
datIndex(char locator1_str[DAT__SZLOC],
         int index,
         char locator2_str[DAT__SZLOC],
         int *status );

/*===================================*/
/* datLen - Inquire primitive length */
/*===================================*/

int
datLen(char locator_str[DAT__SZLOC],
       int *len,
       int *status);

/*===========================*/
/* datMap - Map primitive(s) */
/*===========================*/

int
datMap(char      locator_str[DAT__SZLOC],
       char      *type_str,
       char      *mode_str,
       int       ndim,
       hdsdim dims[],
       unsigned char **pntr,
       int       *status);

/*==================================*/
/* datMapC - Map _CHAR primitive(s) */
/*==================================*/

int
datMapC(char      locator_str[DAT__SZLOC],
        char      *mode_str,
        int       ndim,
        hdsdim dims[],
        unsigned char **pntr,
        int       *status );

/*====================================*/
/* datMapD - Map _DOUBLE primitive(s) */
/*====================================*/

int
datMapD(char      locator_str[DAT__SZLOC],
        char      *mode_str,
        int       ndim,
        hdsdim dims[],
        double    **pntr,
        int       *status );

/*=====================================*/
/* datMapI - Map _INTEGER primitive(s) */
/*=====================================*/

int
datMapI(char      locator_str[DAT__SZLOC],
        char      *mode_str,
        int       ndim,
        hdsdim dims[],
        int       **pntr,
        int       *status );

/*=====================================*/
/* datMapL - Map _LOGICAL primitive(s) */
/*=====================================*/

int
datMapL(char      locator_str[DAT__SZLOC],
        char      *mode_str,
        int       ndim,
        hdsdim dims[],
        int       **pntr,
        int       *status );
        
/*==================================*/
/* datMapR - Map _REAL primitive(s) */
/*==================================*/

int
datMapR(char      locator_str[DAT__SZLOC],
        char      *mode_str,
        int       ndim,
        hdsdim dims[],
        float     **pntr,
        int       *status );
        
/*==================================*/
/* datMould - Alter shape of object */
/*==================================*/

int
datMould(char       locator_str[DAT__SZLOC],
          int       ndim,
          hdsdim dims[],
          int       *status);

/*=======================*/
/* datMove - Move object */
/*=======================*/

int
datMove(char locator1_str[DAT__SZLOC],
        char locator2_str[DAT__SZLOC],
        char *name_str,
        int *status);

/*===============================*/
/* datName - Enquire object name */
/*===============================*/

int
datName(char locator_str[DAT__SZLOC],
        char name_str[DAT__SZNAM+1],
        int *status);

/*=========================================*/
/* datNcomp - Inquire number of components */
/*=========================================*/

int
datNcomp( char locator_str[DAT__SZLOC],
          int *ncomp,
          int *status);

/*===============================*/
/* datNew - Create new component */
/*===============================*/

int
datNew( char      locator_str[DAT__SZLOC],
        char      *name_str,
        char      *type_str,
        int       ndim,
        hdsdim dims[],
        int       *status);

/*============================================*/
/* datNewC - Create new _CHAR type component */
/*============================================*/

int
datNewC(char      locator_str[DAT__SZLOC],
        char      *name_str,
        int       len,
        int       ndim,
        hdsdim dims[],
        int       *status);

/*====================================*/
/* datParen - Locate parent structure */
/*====================================*/

int
datParen(char locator1_str[DAT__SZLOC],
         char locator2_str[DAT__SZLOC],
         int *status);

/*====================================*/
/* datPrim - Enquire object primitive */
/*====================================*/

int
datPrim(char locator_str[DAT__SZLOC],
        int *prim,
        int *status);

/*=========================================================*/
/* datPrmry - Set/Enquire primary/secondary locator status */
/*=========================================================*/

int
datPrmry(int set,
         char loc[DAT__SZLOC],
         int *prmry,
         int *status);

/*==================================*/
/* datPutC - Write _CHAR primitive */
/*==================================*/

int
datPutC( char      locator_str[DAT__SZLOC],
         int       ndim,
         hdsdim dims[],
         char      *string,
         int       string_length,
         int       *status);

/*====================================*/
/* datPutD - Write _DOUBLE primitives */
/*====================================*/

int
datPutD( char      locator_str[DAT__SZLOC],
         int       ndim,
         hdsdim dims[],
         double    *values,
         int       *status);

/*=====================================*/
/* datPutI - Write _INTEGER primitives */
/*=====================================*/

int
datPutI( char    locator_str[DAT__SZLOC],
         int     ndim,
         hdsdim dims[],
         int     *values,
         int     *status);

/*==================================*/
/* datPutR - Write _REAL primitives */
/*==================================*/

int
datPutR( char      locator_str[DAT__SZLOC],
         int       ndim,
         hdsdim dims[],
         float     *values,
         int       *status);

/*=====================================*/
/* datPutL - Write _LOGICAL primitives */
/*=====================================*/

int
datPutL( char      locator_str[DAT__SZLOC],
         int       ndim,
         hdsdim dims[],
         int       *values,
         int       *status);

/*==========================*/
/* datPut - Write primitive */
/*==========================*/

int
datPut( char     locator_str[DAT__SZLOC],
        char     *type_str,
        int      ndim,
        hdsdim  dims[],
        unsigned char *values,
        int      *status);

/*===================================================*/
/* datRefct - Enquire container file reference count */
/*===================================================*/

int
datRefct(char loc[DAT__SZLOC],
         int *refct,
         int *status);

/*=============================*/
/* datRenam - Rename an object */
/*=============================*/

int
datRenam(char locator_str[DAT__SZLOC],
          char *name_str,
          int  *status);

/*================================*/
/* datReset - Reset object state */
/*================================*/

int 
datReset(char locator_str[DAT__SZLOC],
          int *status);

/*================================*/
/* datRetyp - Change object type */
/*================================*/

int
datRetyp(char locator_str[DAT__SZLOC],
          char *type_str,
          int *status);

/*=================================*/
/* datShape - Enquire object shape */
/*=================================*/

int
datShape(char      locator_str[DAT__SZLOC],
         int       maxdim,
         hdsdim dims[],
         int       *actdim,
         int       *status);

/*===============================*/
/* datSize - Enquire object size */
/*===============================*/

int
datSize(char locator_str[DAT__SZLOC],
        int *size,
        int *status );

/*================================*/
/* datSlice - Locate object slice */
/*================================*/

int
datSlice(char     locator1_str[DAT__SZLOC],
         int      ndim,
         hdsdim  lower[],
         hdsdim  upper[],
         char     locator2_str[DAT__SZLOC],
         int      *status );

/*=================================*/
/* datState - Enquire object state */
/*=================================*/

int   
datState(char locator_str[DAT__SZLOC],
         int *state,
         int *status);

/*=====================================*/
/* datStruc - Enquire object structure */
/*=====================================*/

int
datStruc(char locator_str[DAT__SZLOC],
         int *struc,
         int *status);

/*===================================*/
/* datTemp - Create temporary object */
/*===================================*/

int
datTemp(char      *type_str,
        int       ndim,
        hdsdim dims[],
        char      locator_str[DAT__SZLOC],
        int       *status);

/*=========================================*/
/* datThere - Enquire component existence */
/*=========================================*/

int
datThere(char locator_str[DAT__SZLOC],
         char *name_c,
         int *there,
         int *status);

/*===============================*/
/* datType - Enquire object type */
/*===============================*/

int
datType(char locator_str[DAT__SZLOC],
        char type_str[DAT__SZTYP + 1],
        int *status );

/*=========================*/
/* datUnmap - Unmap object */
/*=========================*/

int
datUnmap(char locator_str[DAT__SZLOC],
         int  *status);

/*==================================*/
/* datValid - Enquire locator valid */
/*==================================*/

int
datValid(char locator_str[DAT__SZLOC],
         int *valid,
         int *status);

/*===========================*/
/* datVec - Vectorise object */
/*===========================*/

int
datVec(char locator1_str[DAT__SZLOC],
       char locator2_str[DAT__SZLOC],
       int  *status );

/*================================================*/
/* datWhere - Find primitive position in HDS file */
/*            Currently not part of the public    */
/*            C API                               */
/*================================================*/

/*==================================================*/
/* hdsCopy - Copy an object to a new container file */
/*==================================================*/

int
hdsCopy(char locator_str[DAT__SZLOC],
        char *file_str,
        char name_str[DAT__SZNAM],
        int *status );

/*=================================*/
/* hdsErase - Erase container file */
/*=================================*/

int
hdsErase(char locator_str[DAT__SZLOC],
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
hdsFree(char locator_str[DAT__SZLOC],
        int *status);

/*==================================*/
/* hdsGroup - Enquire locator group */
/*==================================*/

int
hdsGroup(char locator_str[DAT__SZLOC],
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
hdsLink(char locator_str[DAT__SZLOC],
        char *group_str,
        int *status);

/*================================*/
/* hdsLock - Lock container file */
/*================================*/

int
hdsLock(char locator_str[DAT__SZLOC],
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
       char locator_str[DAT__SZLOC],
       int *status);

/*========================================*/
/* hdsOpen - Open existing container file */
/*========================================*/

int
hdsOpen(char *file_str,
        char *mode_str,
        char locator_str[DAT__SZLOC],
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
hdsTrace(char locator_str[DAT__SZLOC],
         int  *nlev,
         char *path_str,
         char *file_str,
         int  *status,
         int  path_length,
         int  file_length);

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
            char *loc,
            int *status);

/*=================================================================*/
/*  Deprecated routines!                                           */   
/*=================================================================*/

/*========================================*/
/* datConv - Enquire conversion possible? */
/*========================================*/

int
datConv(char locator_str[DAT__SZLOC],
        char *type_str,
        int *conv,
        int *status);

/*=====================================================*/
/* hdsClose - Close container file (Obselete routine!) */
/*=====================================================*/

int
hdsClose(char locator_str[DAT__SZLOC],
        int *status);
