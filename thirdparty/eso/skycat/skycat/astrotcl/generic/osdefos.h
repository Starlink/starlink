/* static char sccsid[] = "@(#) ST-ECF os/h/osdefos.h	4.1	10/16/92"; */
/* @(#)osdefos.h	1.1.1.1 (ESO-IPG) 7/11/91 20:25:38 */
/*+++++++++++++++++++++
.TYPE                Header
.LANGUAGE            C
.IDENTIFICATION      osdefos.h
.AUTHOR              BP  [ESO-IPG], Francois Ochsenbein
.KEYWORDS            operating system, parameters, fortran compilers
.ENVIRONMENT    VMS  ULTRIX  MSDOS  SYSTEM_FIVE  SYSV_V2  BSD
.COMMENTS            This modules defines the OS #def's, and an option
			SW_LEVEL set to zero for minimizing
			the size of the generated programs.
		This version includes #def's keywords
		OS_VMS  OS_ULTRIX  OS_MSDOS  OS_SYSV  OS_SYSV_V2  OS_BSD
		and the memory classes EXTERN, etc...
.VERSION 1.0	     10-Mar-1987  Also set defines for standard calls BP
.VERSION 1.1	     11-Apr-1990  EXTERN is defined as extern for UNIX. CG
--------------------------------------------------------------------------*/

#ifndef OSDEFOS_DEF
#define OSDEFOS_DEF	0

/*===========================================================================
 *             List of supported Operating Systems
 *===========================================================================*/

#define _VMS		1
#define _ULTRIX		2
#define _MSDOS		3
#define _BSD		4
#define _SYSV		5
#define _SYSV_V2	6

#define OS_VMS		(OS_ENV == _VMS)
#define OS_ULTRIX	(OS_ENV == _ULTRIX)
#define OS_MSDOS	(OS_ENV == _MSDOS)
#define OS_BSD		(OS_ENV == _BSD)
#define OS_SYSV		(OS_ENV == _SYSV)
#define OS_SYSV_V2	(OS_ENV == _SYSV_V2)


/*===========================================================================
 *             Define Here Your Specific Implementation
 *===========================================================================*/

#if 0	/* Example of minimal implementation for MicroComputer running MSDOS */
#define SW_LEVEL	0	
#define OS_ENV		_MSDOS
#endif	/* End of Example */


#ifndef OS_ENV
/*===========================================================================
 *             Definition of Default Operating System
 *===========================================================================*/

#ifdef VMS
#define OS_ENV		_VMS
#endif

#ifndef OS_ENV
#ifdef vms
#define OS_ENV		_VMS
#endif
#endif

#ifdef ULTRIX
#define OS_ENV		_ULTRIX
#endif

#ifdef MSDOS
#define OS_ENV		_MSDOS
#endif

#ifdef __MSDOS_
#define OS_ENV		_MSDOS
#endif

#ifdef SYSV_V2
#define OS_ENV		_SYSV_V2
#endif

#ifdef SYSV
#define OS_ENV		_SYSV	
#endif

#ifdef BSD
#define OS_ENV		_BSD	
#endif

#endif

	/* If no OS variable defined, choose among ULTRIX / SYSV / BSD */
	
#ifndef OS_ENV
# ifdef vax
#   define OS_ENV	_ULTRIX
# else
#   ifdef sun
#     define OS_ENV	_BSD
#   else
#     define OS_ENV	_SYSV
#   endif
# endif 
#endif 


/*===========================================================================
 *             Definition of Memory Classes (related to OS)
 *===========================================================================*/

#ifndef SW_LEVEL
#define SW_LEVEL	9
#endif

#define Rstatic		RSTATIC

#ifdef 	vax11c
#define REGISTER	register
#define STATIC		static noshare		/* standard 		*/
#define RSTATIC		static readonly		/* Added    13-Feb-1986	*/
#define GLOBALDEF	globaldef 		/* standard 		*/
#define GLOBALREF	globalref		/* standard 		*/
#define GLOBALVALUE	globalvalue		/* standard		*/
#define	EXTERN		globalref		/* Mod.  13-Feb-1986	*/
#define	REXTERN		globalref		/* Added 13-Feb-1986	*/
#define GLOBAL		globaldef noshare 	/* Added 13-Feb-1986	*/
#define RGLOBAL		globaldef readonly	/* Added 13-Feb-1986	*/

#else  /* gcc */
#define REGISTER 	register 
#define STATIC 		static 
#define RSTATIC		static 
#define GLOBALDEF
#define GLOBALREF	extern
#define GLOBALVALUE
#define	EXTERN		extern
#define REXTERN		extern
#define GLOBAL
#define RGLOBAL
#endif

#endif
