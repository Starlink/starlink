 /*
 				define.h

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*
*	Part of:	SExtractor
*
*	Author:		E.BERTIN (IAP)
*
*	Contents:	global definitions.
*
*	Last modify:	11/02/2000
*
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/

/*------------------------ what, who, when and where ------------------------*/

#define		BANNER		"SExtractor"
#define		VERSION		"2.1.6 (Feb 11, 2000)"
#define		COPYRIGHT	"Emmanuel BERTIN (bertin@iap.fr)"
#define		WEBSITE		"http://terapix.iap.fr/sextractor"
#define	       	MAILINGLIST	"sextractor@iap.fr"
#define	       	MAILINGLISTREQ	"sextractor-request@iap.fr"
#define		INSTITUTE	"IAP"

/*--------------------------- Internal constants ----------------------------*/

#define	BIG			1e+30		/* a huge number */
#define	DATA_BUFSIZE		262144		/* data buffer size */
#define	MARGIN_SCALE		2.0		/* Margin / object height */ 
#define	MAXCHAR			512		/* max. number of characters */
#define	MAXCHECK		32		/* max. # of CHECKimages */
#define	MAXDEBAREA		3		/* max. area for deblending */
#define	MAXFLAG			4		/* max. # of FLAG-images */
#define	MAXIMAGE		2		/* max. # of input images */
#define	MAXNAPER		32		/* max. number of apertures */
#define	MAXNASSOC		32		/* max. number of assoc. */
#define	MAXPICSIZE		1048576		/* max. image size */
#define	NISO			8		/* number of isophotes */
#define	OUTPUT			stderr		/* where all msgs are sent */
#define PSF_NPSFMAX		11		/* Max number of fitted PSFs */

#ifndef PI
#define	PI			3.1415926535898	/* never met before? */
#endif

/* NOTES:
 *
 *One must have:	BIG < the biggest element a float can store
 *			DATA_BUFSIZE >= 2880 with DATA_BUFSIZE%8 = 0
 *			MAXCHAR >= 16
 *			1 <= MAXCHECK <= MAXLIST (see prefs.h)
 *			1 <= MAXDEBAREA (see prefs.c & extract.c)
 *			1 <= MAXFLAG <= MAXLIST (see prefs.h)
 *			1 <= MAXIMAGE <= MAXLIST (see prefs.h)
 *			1 <= MAXNAPER <= MAXLIST (see prefs.h)
 *			1 <= MAXNASSOC <= MAXLIST (see prefs.h)
 *			MAXPICSIZE > size of any image!!
 *			NISO = 8 (otherwise need to change prefs.h)
 *			1 <= PSF_NPSFMAX
*/

/*---- Set defines according to machine's specificities and customizing -----*/

#ifdef DEC_ALPHA
#define	BSWAP
#define	INT64
#endif

#ifdef HP_UX
#define	_HPUX_SOURCE
#endif

#ifdef PC_LINUX
#define	BSWAP
#endif

#ifdef SUN_ULTRASPARC
#define	INT64
#endif

#ifdef IBM_AIX
#define	INT64
#endif

#ifdef SUN_OS
#endif

#ifdef DEC_ULTRIX
#define	BSWAP
#endif

#ifdef MEMORY_LINK
#define MEMORY_READ
#endif

/*--------------------- in case of missing constants ------------------------*/

#ifndef		SEEK_SET
#define		SEEK_SET	0
#endif
#ifndef		SEEK_CUR
#define		SEEK_CUR	1
#endif

#ifndef	EXIT_SUCCESS
#define	EXIT_SUCCESS		0
#endif
#ifndef	EXIT_FAILURE
#define	EXIT_FAILURE		-1
#endif

/*---------------------------- return messages ------------------------------*/

#define		RETURN_OK		0
#define		RETURN_ERROR		(-1)
#define		RETURN_FATAL_ERROR	(-2)

/*------------------- a few definitions to read FITS parameters ------------*/

#define	FBSIZE	2880L	/* size (in bytes) of one FITS block */

#define	FITSTOF(k, def) \
			(st[0]=0,((point = fitsnfind(buf, k, n))? \
				 fixexponent(point), \
				atof(strncat(st, &point[10], 70)) \
				:(def)))

#define	FITSTOI(k, def) \
			(st[0]=0,(point = fitsnfind(buf, k, n))? \
				 atoi(strncat(st, &point[10], 70)) \
				:(def))

#define	FITSTOS(k, str, def) \
                { if (fitsread(buf,k,str,H_STRING,T_STRING)!= RETURN_OK) \
                    strcpy(str, (def)); \
                }

/*------------------------------- Other Macros -----------------------------*/

#define	DEXP(x)	exp(2.30258509299*(x))	/* 10^x */

#define QFREAD(ptr, size, afile, fname) \
		if (fread(ptr, (size_t)(size), (size_t)1, afile)!=1) \
		  error(EXIT_FAILURE, "*Error* while reading ", fname)

#define QFWRITE(ptr, size, afile, fname) \
		if (fwrite(ptr, (size_t)(size), (size_t)1, afile)!=1) \
		  error(EXIT_FAILURE, "*Error* while writing ", fname)

#define	QFSEEK(afile, offset, pos, fname) \
		if (fseek(afile, (offset), pos)) \
		  error(EXIT_FAILURE,"*Error*: file positioning failed in ", \
			fname)

#define	QFTELL(pos, afile, fname) \
		if ((pos=ftell(afile))==-1) \
		  error(EXIT_FAILURE,"*Error*: file position unknown in ", \
			fname)

#define	QCALLOC(ptr, typ, nel) \
		{if (!(ptr = (typ *)calloc((size_t)(nel),sizeof(typ)))) \
		  error(EXIT_FAILURE, "Not enough memory for ", \
			#ptr " (" #nel " elements) !");;}

#define	QMALLOC(ptr, typ, nel) \
		{if (!(ptr = (typ *)malloc((size_t)(nel)*sizeof(typ)))) \
		  error(EXIT_FAILURE, "Not enough memory for ", \
			#ptr " (" #nel " elements) !");;}

#define	QFREE(ptr) \
		{free(ptr); \
		ptr = NULL;}

#define	QREALLOC(ptr, typ, nel) \
		{if (!(ptr = (typ *)realloc(ptr, (size_t)(nel)*sizeof(typ)))) \
		   error(EXIT_FAILURE, "Not enough memory for ", \
			#ptr " (" #nel " elements) !");;}

#define QMEMCPY(ptrin, ptrout, typ, nel) \
		{if (ptrin) \
                  {if (!(ptrout = (typ *)malloc((size_t)(nel)*sizeof(typ)))) \
                    error(EXIT_FAILURE, "Not enough memory for ", \
                        #ptrout " (" #nel " elements) !"); \
                   memcpy(ptrout, ptrin, (size_t)(nel)*sizeof(typ));};}

#define	RINT(x)	(int)(floor(x+0.5))

#define	PIX(pic, x, y)	pic->strip[(((int)y)%pic->stripheight) \
				*pic->width +(int)x]

#define	NPRINTF		if (prefs.verbose_type == NORM \
				|| prefs.verbose_type==WARN) fprintf

#define	NFPRINTF(w,x)	{if (prefs.verbose_type==NORM \
				|| prefs.verbose_type==WARN) \
				fprintf(w, "\33[1M> %s\n\33[1A",x); \
			else if (prefs.verbose_type == FULL) \
				fprintf(w, "%s.\n", x);}

#define	QPRINTF		if (prefs.verbose_type != QUIET)	fprintf

#define	FPRINTF		if (prefs.verbose_type == FULL)	fprintf

#define	QWARNING       	if (prefs.verbose_type==WARN \
				|| prefs.verbose_type==FULL)	warning

#define FLAG(x)		(*((char *)&flag##x))

#define VECFLAG(x)	(*((char *)flag##x))
