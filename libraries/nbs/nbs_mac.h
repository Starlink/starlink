/*+			N B S _ M A C . H

 *  Module name:
      NBS_MAC.H

 *  Function:
      Macros for the noticeboard system.

 *  Description:
      This module contains definitions of constants that are not directly
      required by the type definitions, plus a few utility macros.

 *  Language:
      C

 *  Support: William Lupton, AAO

 *  Version date: 07-Feb-90
 *-

 *  History:
      03-Feb-86 - WFL - Original version
      17-Jul-87 - WFL - Change VERSION to 2 to correspond to change in the
 		definition of ITEM_DESCRIPTOR.
      20-Jul-87 - WFL - Add _CHMOVE macro.
      21-Jul-87 - WFL - Remove definition of VERSION.
      06-Nov-87 - WFL - Portable VMS / UNIX version. On VMS, use OTS$MOVE3,
		but on UNIX use MEMCPY. Also, define HUGE.
      12-Feb-88 - WFL - Version of _CHMOVE that does 1, 2, 4 and 8 byte moves
		directly.
      17-Feb-88 - WFL - Move MAXALLOC, ITEM_BASE and DATA_BASE to NBS.C, since
		they are only used there.
      25-Jan-89 - WFL - Add extra parentheses to _CHMOVE to ensure that any
		expressions in the arguments are evaluated first
      01-Feb-90 - WFL - Add definition of NBS_WAIT
      07-Feb-90 - WFL - Add definition of _ADD_INTERLOCKED using _ADAWI; modify
		_CHMOVE to use _MOVC3
      07-Feb-93 - DJA - Added argument processing macros 
      05-May-93 - DJA - Removed _wait macro. Replaced by NBS_SLEEPMS.
      08-Mar-94 - DJA - Added EXPORT_POINTER macro to export pointers to
                        C and Fortran portably.
      08-Feb-95 - DJA - Changed NBS__OK to SAI__OK 
      27-Jun-04 - AA - Changed ifdef logic for building under Mac OSX
 */

/* Constant definitions	*/

/*  NIL, NO and YES are the obvious */

#define NIL 0
#define NO 0
#define YES 1

/*  HUGE is a character string size. NBS_STREXP will never copy more than
    HUGE characters to an output string */

#define HUGE 256

/*  Macro definitions   */

/*  Utility expression evaluation. Trap predefined MAX/MIN (for GNU GCC) */

#ifdef MAX
#undef MAX
#undef MIN
#endif
                                                                
#define MAX(i,j) ((i)>(j) ? (i) : (j))
#define MIN(i,j) ((i)<(j) ? (i) : (j))
#define ODD(i) ((i)&1 == 1)

/*  Checking whether status value is SAI__OK */

#define OK(status) (status == SAI__OK)

/*  If VMS, need builtins for some of the following macros */

#ifdef vms
#pragma builtins
#endif

/*  Bulk copy using RTL routine */

#ifdef vms

#define _chmove_gen(num,sptr,dptr)\
if (!(num & 0xffff0000))\
   _MOVC3(num,sptr,dptr);\
else {\
   unsigned int _chmove_gen_l;\
   unsigned int _chmove_gen_n = num;\
   char* _chmove_gen_s = (char*)(sptr);\
   char* _chmove_gen_d = (char*)(dptr);\
   while (_chmove_gen_n) {\
      _chmove_gen_l = MIN (_chmove_gen_n,0xffff);\
      _MOVC3(_chmove_gen_l,_chmove_gen_s,_chmove_gen_d,\
                          &_chmove_gen_s,&_chmove_gen_d);\
      _chmove_gen_n -= _chmove_gen_l;\
   }\
}

#define _chmove(num,sptr,dptr) {\
   typedef struct { char body[1]; } byte1;\
   typedef struct { char body[2]; } byte2;\
   typedef struct { char body[4]; } byte4;\
   typedef struct { char body[8]; } byte8;\
   unsigned int _chmove_n = num;\
   switch (_chmove_n) {\
      case 0: break;\
      case 1: *((byte1*)(dptr)) = *((byte1*)(sptr)); break;\
      case 2: *((byte2*)(dptr)) = *((byte2*)(sptr)); break;\
      case 4: *((byte4*)(dptr)) = *((byte4*)(sptr)); break;\
      case 8: *((byte8*)(dptr)) = *((byte8*)(sptr)); break;\
      default: _chmove_gen(_chmove_n,sptr,dptr);\
   }\
}

#endif


/* see Apple Developer Connection Tech Notes
  http://developer.apple.com/technotes/tn2002/tn2071.html */
#if defined(unix) || ( defined(__APPLE__) && defined(__MACH__) )

#define _chmove(num,sptr,dptr)\
{\
typedef struct {  char body[1]; } byte1;\
typedef struct {  char body[2]; } byte2;\
typedef struct {  char body[4]; } byte4;\
typedef struct {  char body[8]; } byte8;\
switch (num) {\
case 1: *((byte1*)(dptr)) = *((byte1*)(sptr)); break;\
case 2: *((byte2*)(dptr)) = *((byte2*)(sptr)); break;\
case 4: *((byte4*)(dptr)) = *((byte4*)(sptr)); break;\
case 8: *((byte8*)(dptr)) = *((byte8*)(sptr)); break;\
default: memcpy(dptr,sptr,num);\
}\
}
#endif


/* Interlocked add. This is a word instruction operating on a longword so, if
 * the result is zero, add 0x8000 to it to simulate a longword instruction. */

#ifdef vms
#define _add_interlocked(dptr)\
if (!_ADAWI(1,(unsigned short *)(dptr))) (*(dptr))+=0x10000
#endif


/* see Apple Developer Connection Tech Notes
  http://developer.apple.com/technotes/tn2002/tn2071.html */
#if defined(unix) || ( defined(__APPLE__) && defined(__MACH__) )

#define _add_interlocked(dptr) (*(dptr))++
#endif


/* Argument declaration macros. R_ stands for input, W_ for output */

#ifdef c_string

#define RW_CHARACTER(x)     char *x
#define R_INTEGER(x)        int_id x
#define W_INTEGER(x)        int    *x
#define RW_BYTE_ARRAY(x)    char x []
#define RW_INTEGER_ARRAY(x) int x []
#define RW_POINTER(x)       char * x

#define GENPTR_CHARACTER(x)
#define GENPTR_INTEGER(x)
#define GENPTR_BYTE_ARRAY(x)
#define GENPTR_INTEGER_ARRAY(x)
#define GENPTR_POINTER(x)

#define TRAIL(x) 

#define CF_C_ARG(x)     x
#define CF_TRAIL(x)     

#define EXPORT_POINTER(sptr,dptr_addr) (*(dptr_addr)) = (sptr);

#else

#define RW_CHARACTER(x)     CHARACTER(x)
#define R_INTEGER(x)        INTEGER(x)
#define W_INTEGER(x)        INTEGER(x)
#define RW_BYTE_ARRAY(x)    BYTE_ARRAY(x)
#define RW_INTEGER_ARRAY(x) INTEGER_ARRAY(x)
#define RW_POINTER(x)       POINTER(x)

#define CF_C_ARG(x)     CHARACTER_ARG(x)
#define CF_TRAIL(x)     TRAIL_ARG(x)

#define EXPORT_POINTER(sptr,dptr_addr) \
{F77_POINTER_TYPE _pnt = (F77_POINTER_TYPE) sptr; \
*((F77_POINTER_TYPE *) dptr_addr) = _pnt;}

#endif
