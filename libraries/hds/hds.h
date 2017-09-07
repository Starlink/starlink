/* Protect against multiple inclusion */
#ifndef STAR_HDS_H_INCLUDED
#define STAR_HDS_H_INCLUDED

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
datAlter(const HDSLoc    *locator,
         int       ndim,
         const hdsdim    dims[],
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
datBasic(const HDSLoc    *locator,
         const char      *mode_c,
         unsigned char **pntr,
         size_t    *len,
         int       *status);

/*=====================================*/
/* datCcopy - copy one structure level */
/*=====================================*/

int
datCcopy(const HDSLoc   *locator1,
         const HDSLoc   *locator2,
         const char     *name,
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
datCell( const HDSLoc   *locator1,
         int      ndim,
         const hdsdim   subs[],
	 HDSLoc   **locator2,
         int      *status);

/*=================================================*/
/* datChscn - validate the supplied component name */
/*=================================================*/

int
datChscn( const char * name,
         int      *status );

/*==========================================*/
/* datClen - Obtain character string length */
/*==========================================*/

int
datClen( const HDSLoc   *locator,
         size_t   *clen,
         int      *status );

/*===========================*/
/* datClone - clone locator */
/*===========================*/

int
datClone(const HDSLoc   *locator1,
	 HDSLoc   **locator2,
         int      *status);

/*================================*/
/* datCoerc - coerce object shape */
/*================================*/

int
datCoerc(const HDSLoc   *locator1,
         int      ndim,
	 HDSLoc   **locator2,
         int      *status);

/*=======================*/
/* datCopy - copy object */
/*=======================*/

int
datCopy(const HDSLoc    *locator1,
	const HDSLoc    *locator2,
        const char      *name_c,
        int       *status );

/*============================================================*/
/* datDrep - Obtain primitive data representation information */
/*============================================================*/

int
datDrep(const HDSLoc    *locator,
        char      **format_str,
        char      **order_str,
        int       *status);

/*========================================*/
/* datErase - Erase object                */
/*========================================*/

int
datErase(const HDSLoc   *locator,
         const char     *name_str,
         int      *status);

/*===========================================================*/
/* datErmsg - Translate a status value into an error message */
/*===========================================================*/

int
datErmsg(int      status,
         size_t   *len,
         char     *msg_str);

/*================================*/
/* datFind - Find named component */
/*================================*/

int
datFind( const HDSLoc   *locator1,
         const char     *name_str,
	 HDSLoc   **locator2,
         int      *status );

/*============================*/
/* datGet - Read primitive(s) */
/*============================*/

int
datGet(const HDSLoc     *locator,
       const char       *type_str,
       int        ndim,
       const hdsdim     dims[],
       void       *values,
       int        *status);

/*===================================*/
/* datGetC - Read _CHAR primitive(s) */
/*===================================*/

int
datGetC(const HDSLoc    *locator,
        const int       ndim,
        const hdsdim    dims[],
        char      values[],
        size_t    char_len,
        int       *status);

/*=====================================*/
/* datGetD - Read _DOUBLE primitive(s) */
/*=====================================*/

int
datGetD(const HDSLoc    *locator,
        int       ndim,
        const hdsdim    dims[],
        double    values[],
        int       *status);

/*======================================*/
/* datGetI - Read _INTEGER primitive(s) */
/*======================================*/

int
datGetI(const HDSLoc    *locator,
        int       ndim,
        const hdsdim    dims[],
        int       values[],
        int       *status);

/*======================================*/
/* datGetK - Read _INT64 primitive(s) */
/*======================================*/

int
datGetK(const HDSLoc    *locator,
        int       ndim,
        const hdsdim    dims[],
        int64_t    values[],
        int       *status);

/*===================================*/
/* datGetW - Read _WORD primitive(s) */
/*===================================*/

int
datGetW(const HDSLoc    *locator,
        int       ndim,
        const hdsdim    dims[],
        short     values[],
        int       *status);

/*===================================*/
/* datGetUW - Read _UWORD primitive(s) */
/*===================================*/

int
datGetUW(const HDSLoc    *locator,
        int       ndim,
        const hdsdim    dims[],
        unsigned short  values[],
        int       *status);

/*======================================*/
/* datGetL - Read _LOGICAL primitive(s) */
/*======================================*/

int
datGetL(const HDSLoc    *locator,
        int       ndim,
        const hdsdim    dims[],
        int       values[],
        int       *status);

/*===================================*/
/* datGetR - Read _REAL primitive(s) */
/*===================================*/

int
datGetR(const HDSLoc    *locator,
        int       ndim,
        const hdsdim    dims[],
        float     values[],
        int       *status);

/*======================================*/
/* datGet0C - Read scalar string value  */
/*======================================*/

int
datGet0C( const HDSLoc * loc, char * value, size_t len, int * status );

/*======================================*/
/* datGet0D - Read scalar double value  */
/*======================================*/

int
datGet0D( const HDSLoc * loc, double * value, int * status );

/*=====================================*/
/* datGet0R - Read scalar float value  */
/*=====================================*/

int
datGet0R( const HDSLoc * loc, float * value, int * status );

/*=======================================*/
/* datGet0I - Read scalar integer value  */
/*=======================================*/

int
datGet0I( const HDSLoc * loc, int * value, int * status );

/*================================================*/
/* datGet0K - Read scalar 64-bit integer value  */
/*================================================*/

int
datGet0K( const HDSLoc * loc, int64_t * value, int * status );

/*=============================================*/
/* datGet0W - Read scalar short integer value  */
/*=============================================*/

int
datGet0W( const HDSLoc * loc, short * value, int * status );

/*=============================================*/
/* datGet0UW - Read scalar unsigned short integer value  */
/*=============================================*/

int
datGet0UW( const HDSLoc * loc, unsigned short * value, int * status );

/*=======================================*/
/* datGet0L - Read scalar logical value  */
/*=======================================*/

int
datGet0L( const HDSLoc * loc, int * value, int * status );

/*==================================*/
/* DAT_GET1C - Read 1D string array */
/*==================================*/

int
datGet1C( const HDSLoc * locator,
	  size_t maxval,
	  size_t bufsize,
	  char *buffer,
	  char *pntrs[],
	  size_t * actval,
	  int * status );

/*==================================*/
/* DAT_GET1D - Read 1D Double array */
/*==================================*/

int
datGet1D( const HDSLoc * locator,
	  size_t maxval,
	  double values[],
	  size_t *actval,
	  int * status );

/*==================================*/
/* DAT_GET1I - Read 1D Integer array */
/*==================================*/

int
datGet1I( const HDSLoc * locator,
	  size_t maxval,
	  int values[],
	  size_t *actval,
	  int * status );

/*============================================*/
/* DAT_GET1K - Read 1D 64-bit Integer array */
/*============================================*/

int
datGet1K( const HDSLoc * locator,
	  size_t maxval,
	  int64_t values[],
	  size_t *actval,
	  int * status );

/*=========================================*/
/* DAT_GET1W - Read 1D Short Integer array */
/*=========================================*/

int
datGet1W( const HDSLoc * locator,
	  size_t maxval,
	  short values[],
	  size_t *actval,
	  int * status );

/*===================================================*/
/* DAT_GET1UW - Read 1D Unsigned Short Integer array */
/*===================================================*/

int
datGet1UW( const HDSLoc * locator,
	  size_t maxval,
	  unsigned short values[],
	  size_t *actval,
	  int * status );

/*==================================*/
/* DAT_GET1R - Read 1D REAL array */
/*==================================*/

int
datGet1R( const HDSLoc * locator,
	  size_t maxval,
	  float values[],
	  size_t *actval,
	  int * status );

/*==================================*/
/* DAT_GET1L - Read 1D Logical array */
/*==================================*/

int
datGet1L( const HDSLoc * locator,
	  size_t maxval,
	  int values[],
	  size_t *actval,
	  int * status );

/*==================================*/
/* DAT_GETVC - Read vectorized 1D string array */
/*==================================*/

int
datGetVC( const HDSLoc * locator,
	  size_t maxval,
	  size_t bufsize,
	  char *buffer,
	  char *pntrs[],
	  size_t * actval,
	  int * status );


/*==========================================*/
/* DAT_GETVD - Read vectorized Double array */
/*==========================================*/

int
datGetVD( const HDSLoc * locator,
	  size_t maxval,
	  double values[],
	  size_t *actval,
	  int * status );

/*==========================================*/
/* DAT_GETVI - Read vectorized Integer array */
/*==========================================*/

int
datGetVI( const HDSLoc * locator,
	  size_t maxval,
	  int values[],
	  size_t *actval,
	  int * status );

/*==========================================*/
/* DAT_GETVK - Read vectorized Int64 array */
/*==========================================*/

int
datGetVK( const HDSLoc * locator,
	    size_t maxval,
	    int64_t values[],
	    size_t *actval,
	    int * status );

/*==========================================*/
/* DAT_GETVR - Read vectorized REAL array */
/*==========================================*/

int
datGetVR( const HDSLoc * locator,
	  size_t maxval,
	  float values[],
	  size_t *actval,
	  int * status );

/*==========================================*/
/* DAT_GETVL - Read vectorized Logical array */
/*==========================================*/

int
datGetVL( const HDSLoc * locator,
	  size_t maxval,
	  int values[],
	  size_t *actval,
	  int * status );


/*======================================*/
/* datIndex - Index into component list */
/*======================================*/

int
datIndex(const HDSLoc   *locator1,
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
datMap(const HDSLoc     *locator,
       const char       *type_str,
       const char       *mode_str,
       int        ndim,
       const hdsdim     dims[],
       void       **pntr,
       int        *status);

/*==================================*/
/* datMapC - Map _CHAR primitive(s) */
/*==================================*/

int
datMapC(const HDSLoc    *locator,
        const char      *mode_str,
        int       ndim,
        const hdsdim    dims[],
        unsigned char **pntr,
        int       *status );

/*====================================*/
/* datMapD - Map _DOUBLE primitive(s) */
/*====================================*/

int
datMapD(const HDSLoc    *locator,
        const char      *mode_str,
        int       ndim,
        const hdsdim    dims[],
        double    **pntr,
        int       *status );

/*=====================================*/
/* datMapI - Map _INTEGER primitive(s) */
/*=====================================*/

int
datMapI(const HDSLoc    *locator,
        const char      *mode_str,
        int       ndim,
        const hdsdim    dims[],
        int       **pntr,
        int       *status );

/*=====================================*/
/* datMapK - Map _INT64 primitive(s) */
/*=====================================*/

int
datMapK(const HDSLoc    *locator,
        const char      *mode_str,
        int       ndim,
        const hdsdim    dims[],
        int       **pntr,
        int       *status );

/*=====================================*/
/* datMapL - Map _LOGICAL primitive(s) */
/*=====================================*/

int
datMapL(const HDSLoc *locator,
        const char      *mode_str,
        int       ndim,
        const hdsdim dims[],
        int       **pntr,
        int       *status );

/*==================================*/
/* datMapR - Map _REAL primitive(s) */
/*==================================*/

int
datMapR(const HDSLoc *locator,
        const char      *mode_str,
        int       ndim,
        const hdsdim dims[],
        float     **pntr,
        int       *status );


/*========================================*/
/* datMapN - Map primitive as N-dim array */
/*========================================*/

int
datMapN(const HDSLoc     *locator,
	const char       *type_str,
        const char       *mode_str,
	int        ndim,
	void       **pntr,
	hdsdim     dims[],
	int        *status);

/*==================================*/
/* datMapV - Map vectorized primitive(s) */
/*==================================*/

int
datMapV(const HDSLoc    *locator,
        const char      *type_str,
        const char      *mode_str,
        void      **pntr,
        size_t    *actval,
        int       *status );


/*==================================*/
/* datMould - Alter shape of object */
/*==================================*/

int
datMould(const HDSLoc *locator,
         int       ndim,
         const hdsdim dims[],
         int       *status);

/*=======================*/
/* datMove - Move object */
/*=======================*/

int
datMove(HDSLoc **locator1,
        const HDSLoc *locator2,
        const char *name_str,
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
datNew( const HDSLoc    *locator,
        const char      *name_str,
        const char      *type_str,
        int       ndim,
        const hdsdim    dims[],
        int       *status);

/*============================================*/
/* datNewC - Create new _CHAR type component */
/*============================================*/

int
datNewC(const HDSLoc    *locator,
        const char      *name_str,
        size_t    len,
        int       ndim,
        const hdsdim    dims[],
        int       *status);

/*=======================================*/
/* datNew0 - Create new scalar component */
/*=======================================*/

int
datNew0( const HDSLoc    *locator,
        const char      *name_str,
        const char      *type_str,
        int       *status);

/*===============================================*/
/* datNew0D - Create new scalar double component */
/*===============================================*/

int
datNew0D( const HDSLoc    *locator,
        const char      *name_str,
        int       *status);

/*================================================*/
/* datNew0I - Create new scalar integer component */
/*================================================*/

int
datNew0I( const HDSLoc    *locator,
        const char      *name_str,
        int       *status);

/*=========================================================*/
/* datNew0K - Create new scalar 64-bit integer component */
/*=========================================================*/

int
datNew0K( const HDSLoc    *locator,
            const char      *name_str,
            int       *status);

/*======================================================*/
/* datNew0W - Create new scalar short integer component */
/*======================================================*/

int
datNew0W( const HDSLoc    *locator,
        const char      *name_str,
        int       *status);

/*================================================================*/
/* datNew0UW - Create new scalar unsigned short integer component */
/*================================================================*/

int
datNew0UW( const HDSLoc    *locator,
           const char      *name_str,
           int       *status);

/*=============================================*/
/* datNew0R - Create new scalar real component */
/*=============================================*/

int
datNew0R( const HDSLoc    *locator,
        const char      *name_str,
        int       *status);

/*================================================*/
/* datNew0L - Create new scalar logical component */
/*================================================*/

int
datNew0L( const HDSLoc    *locator,
        const char      *name_str,
        int       *status);

/*================================================*/
/* datNew0L - Create new scalar logical component */
/*================================================*/

int
datNew0C( const HDSLoc    *locator,
	  const char      *name_str,
	  size_t          len,
	  int             *status);



/*=======================================*/
/* datNew1 - Create new vector component */
/*=======================================*/

int
datNew1( const HDSLoc    *locator,
	 const char      *name_str,
	 const char      *type_str,
	 size_t          len,
	 int       *status);

/*=======================================*/
/* datNew1C - Create new vector string  */
/*=======================================*/

int
datNew1C( const HDSLoc    *locator,
	  const char      *name_str,
	  size_t          len,
	  size_t          nelem,
	  int       *status);

/*=======================================*/
/* datNew1d - Create new vector double   */
/*=======================================*/

int
datNew1D( const HDSLoc    *locator,
	 const char      *name_str,
	 size_t          len,
	 int       *status);

/*=======================================*/
/* datNew1I - Create new vector integer  */
/*=======================================*/

int
datNew1I( const HDSLoc    *locator,
	 const char      *name_str,
	 size_t          len,
	 int       *status);

/*================================================*/
/* datNew1K - Create new vector 64-bit integer  */
/*================================================*/

int
datNew1K( const HDSLoc    *locator,
	 const char      *name_str,
	 size_t          len,
	 int       *status);

/*=============================================*/
/* datNew1W - Create new vector short integer  */
/*=============================================*/

int
datNew1W( const HDSLoc    *locator,
	 const char      *name_str,
	 size_t          len,
	 int       *status);

/*=======================================================*/
/* datNew1UW - Create new vector unsigned short integer  */
/*=======================================================*/

int
datNew1UW( const HDSLoc    *locator,
	   const char      *name_str,
	   size_t          len,
	   int       *status);

/*=======================================*/
/* datNew1L - Create new vector logical   */
/*=======================================*/

int
datNew1L( const HDSLoc    *locator,
	 const char      *name_str,
	 size_t          len,
	 int       *status);

/*=======================================*/
/* datNew1R - Create new vector float   */
/*=======================================*/

int
datNew1R( const HDSLoc    *locator,
	 const char      *name_str,
	 size_t          len,
	 int       *status);

/*====================================*/
/* datParen - Locate parent structure */
/*====================================*/

int
datParen(const HDSLoc *locator1,
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
datPutC( const HDSLoc *locator,
         int       ndim,
         const hdsdim dims[],
         const char      string[],
         size_t    string_length,
         int       *status);

/*====================================*/
/* datPutD - Write _DOUBLE primitives */
/*====================================*/

int
datPutD( const HDSLoc *locator,
         int       ndim,
         const hdsdim dims[],
         const double    values[],
         int       *status);

/*=====================================*/
/* datPutI - Write _INTEGER primitives */
/*=====================================*/

int
datPutI( const HDSLoc *locator,
         int     ndim,
         const hdsdim dims[],
         const int     values[],
         int     *status);

/*=====================================*/
/* datPutK - Write _INT64 primitives */
/*=====================================*/

int
datPutK( const HDSLoc *locator,
         int     ndim,
         const hdsdim dims[],
         const int64_t values[],
         int     *status);

/*=====================================*/
/* datPutW - Write _WORD primitives */
/*=====================================*/

int
datPutW( const HDSLoc *locator,
         int     ndim,
         const hdsdim dims[],
         const short     values[],
         int     *status);

/*====================================*/
/* datPutUW - Write _UWORD primitives */
/*====================================*/

int
datPutUW( const HDSLoc *locator,
         int     ndim,
         const hdsdim dims[],
         const unsigned short values[],
         int     *status);

/*==================================*/
/* datPutR - Write _REAL primitives */
/*==================================*/

int
datPutR( const HDSLoc *locator,
         int       ndim,
         const hdsdim dims[],
         const float     values[],
         int       *status);

/*=====================================*/
/* datPutL - Write _LOGICAL primitives */
/*=====================================*/

int
datPutL( const HDSLoc *locator,
         int       ndim,
         const hdsdim dims[],
         const int values[],
         int       *status);

/*==========================*/
/* datPut - Write primitive */
/*==========================*/

int
datPut( const HDSLoc  *locator,
        const char    *type_str,
        int     ndim,
        const hdsdim  dims[],
        const void    *values,
        int     *status);

/*=======================================*/
/* datPut0C - Write scalar string value  */
/*=======================================*/

int
datPut0C( const HDSLoc * loc, const char * value, int * status );

/*=======================================*/
/* datPut0D - Write scalar double value  */
/*=======================================*/

int
datPut0D( const HDSLoc * loc, double value, int * status );

/*======================================*/
/* datPut0R - Write scalar float value  */
/*======================================*/

int
datPut0R( const HDSLoc * loc, float value, int * status );

/*========================================*/
/* datPut0I - Write scalar integer value  */
/*========================================*/

int
datPut0I( const HDSLoc * loc, int value, int * status );

/*========================================*/
/* datPut0I - Write scalar 64-bit integer value  */
/*========================================*/

int
datPut0K( const HDSLoc * loc, int64_t value, int * status );

/*==============================================*/
/* datPut0W - Write scalar short integer value  */
/*===============================================*/

int
datPut0W( const HDSLoc * loc, short value, int * status );

/*========================================================*/
/* datPut0UW - Write scalar unsigned short integer value  */
/*========================================================*/

int
datPut0UW( const HDSLoc * loc, unsigned short value, int * status );

/*========================================*/
/* datPut0L - Write scalar logical value  */
/*========================================*/

int
datPut0L( const HDSLoc * loc, int value, int * status );

/*========================================*/
/* datPut1C - Write 1D character array       */
/*========================================*/

int
datPut1C( const HDSLoc * locator,
	  size_t nval,
	  const char *values[],
	  int * status );

/*========================================*/
/* datPut1D - Write 1D double array       */
/*========================================*/

int
datPut1D( const HDSLoc * loc,
	  size_t nval,
	  const double values[],
	  int * status );

/*========================================*/
/* datPut1I - Write 1D int array       */
/*========================================*/

int
datPut1I( const HDSLoc * loc,
	  size_t nval,
	  const int values[],
	  int * status );

/*========================================*/
/* datPut1K - Write 1D 64-bit int array */
/*========================================*/

int
datPut1K( const HDSLoc * loc,
	  size_t nval,
	  const int64_t values[],
	  int * status );

/*===========================================*/
/* datPut1W - Write 1D short int array       */
/*===========================================*/

int
datPut1W( const HDSLoc * loc,
	  size_t nval,
	  const short values[],
	  int * status );

/*===============================================*/
/* datPut1UW - Write 1D unsigned short int array */
/*===============================================*/

int
datPut1UW( const HDSLoc * loc,
           size_t nval,
           const unsigned short values[],
           int * status );

/*========================================*/
/* datPut1R - Write 1D double array       */
/*========================================*/

int
datPut1R( const HDSLoc * loc,
	  size_t nval,
	  const float values[],
	  int * status );

/*========================================*/
/* datPut1L - Write 1D Logical/int array       */
/*========================================*/

int
datPut1L( const HDSLoc * loc,
	  size_t nval,
	  const int values[],
	  int * status );

/*================================================*/
/* datPutVD - Write vectorized double array       */
/*================================================*/

int
datPutVD( const HDSLoc * loc,
	  size_t nval,
	  const double values[],
	  int * status );

/*================================================*/
/* datPutVI - Write vectorized int array       */
/*================================================*/

int
datPutVI( const HDSLoc * loc,
	  size_t nval,
	  const int values[],
	  int * status );

/*================================================*/
/* datPutVI - Write vectorized int64 array       */
/*================================================*/

int
datPutVK( const HDSLoc * loc,
	    size_t nval,
	    const int64_t values[],
	    int * status );

/*================================================*/
/* datPutVR - Write vectorized REAL/float array       */
/*================================================*/

int
datPutVR( const HDSLoc * loc,
	  size_t nval,
	  const float values[],
	  int * status );

/*================================================*/
/* datPutVL - Write vectorized Logical array       */
/*================================================*/

int
datPutVL( const HDSLoc * loc,
	  size_t nval,
	  const int values[],
	  int * status );

/*================================================*/
/* datPutVC - Write vectorized character array       */
/*================================================*/

int
datPutVC( const HDSLoc * locator,
	  size_t nval,
	  const char *values[],
	  int * status );


/*========================================*/
/* datRef - Enquire object reference name */
/*========================================*/

int
datRef( const HDSLoc * loc, char * ref, size_t reflen, int *status );

/*===================================================*/
/* datRefct - Enquire container file reference count */
/*===================================================*/

int
datRefct(const HDSLoc *locator,
         int *refct,
         int *status);

/*=============================*/
/* datRenam - Rename an object */
/*=============================*/

int
datRenam(const HDSLoc *locator,
         const  char *name_str,
	 int  *status);

/*================================*/
/* datReset - Reset object state */
/*================================*/

int
datReset(const HDSLoc *locator,
          int *status);

/*================================*/
/* datRetyp - Change object type */
/*================================*/

int
datRetyp(const HDSLoc *locator,
         const char *type_str,
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
datSlice(const HDSLoc  *locator1,
         int     ndim,
         const hdsdim  lower[],
         const hdsdim  upper[],
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
datTemp(const char      *type_str,
        int       ndim,
        const hdsdim    dims[],
        HDSLoc    **locator,
        int       *status);

/*=========================================*/
/* datThere - Enquire component existence */
/*=========================================*/

int
datThere(const HDSLoc *locator,
         const char *name_c,
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
datUnmap(const HDSLoc *locator,
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
datVec(const HDSLoc *locator1,
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
hdsCopy(const HDSLoc *locator,
        const char *file_str,
        const char name_str[DAT__SZNAM],
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
hdsFlush( const char *group_str,
          int *status);

/*===============================*/
/* hdsFree - Free container file */
/*===============================*/

int
hdsFree(const HDSLoc *locator,
        int *status);

/*==================================*/
/* hdsGroup - Enquire locator group */
/*==================================*/

int
hdsGroup(const HDSLoc *locator,
         char group_str[DAT__SZGRP+1],
         int *status);

/*=========================================*/
/* hdsGtune - Get HDS tuning parameter     */
/*=========================================*/

int
hdsGtune(const char *param_str,
         int *value,
         int *status);

/*=========================================*/
/* hdsGtune - Get HDS status integers      */
/*=========================================*/

int
hdsInfoI(const HDSLoc* loc,
	 const char *topic_str,
	 const char *extra,
	 int *result,
	 int  *status);

/*=================================*/
/* hdsLink - Link locator to group */
/*=================================*/

int
hdsLink(const HDSLoc *locator,
        const char *group_str,
        int *status);

/*================================*/
/* hdsLock - Lock container file */
/*================================*/

int
hdsLock(const HDSLoc *locator,
        int *status);

/*====================================*/
/* hdsNew - Create new container file */
/*====================================*/

int
hdsNew(const char *file_str,
       const char *name_str,
       const char *type_str,
       int  ndim,
       const hdsdim  dims[],
       HDSLoc **locator,
       int *status);

/*========================================*/
/* hdsOpen - Open existing container file */
/*========================================*/

int
hdsOpen(const char *file_str,
        const char *mode_str,
        HDSLoc **locator,
        int *status);

/*===============================*/
/* hdsShow - Show HDS statistics */
/*===============================*/

int
hdsShow(const char *topic_str,
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
hdsTune(const char *param_str,
        int  value,
        int  *status);

/*=================================================================*/
/* hdsWild - Perform a wild-card search for HDS container files   */
/*=================================================================*/

int
hdsWild(const char *fspec,
        const char *mode,
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
        const char *type_str,
        int *conv,
        int *status);

/*=====================================================*/
/* hdsClose - Close container file (Obsolete routine!) */
/*=====================================================*/

int
hdsClose(HDSLoc **locator,
        int *status);


/*===================================================================*/
/* hdsFind - Find an object (Fortran routine, requires hdsf library) */
/*===================================================================*/
void
hdsFind( const HDSLoc *loc1, const char *name, const char *mode,
         HDSLoc **loc2, int *status );



/*===================================================================*/
/* Stubs for functions to be introduced in HDS V5.
/*===================================================================*/
int datLock( HDSLoc *locator, int recurs, int readonly, int *status );
int datLocked( const HDSLoc *locator, int *status );
int datUnlock( HDSLoc *locator, int recurs, int *status );

/* STAR_HDS_H_INCLUDED */
#endif
