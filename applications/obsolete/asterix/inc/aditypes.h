/*
 *
 *    Defines following types :
 *
 *     Type name                Description of contents
 *
 *     ADIeproc                 External procedures (callbacks)
 *     ADImethcomb              Method combination form
 *     ADIgeneric               Generic function
 *     ADImethod                Method instance
 *
 *     ADICB			Void callback function, used for casting
 *     ADIcMethodCB		C method executor
 *     ADIfMethodCB		Fortran method executor
 *     ADIcGenericDispatchCB	C generic dispatch routine
 *     ADIfGenericDispatchCB	Fortran generic dispatch routine
 *     ADIcMethodCombinationCB	C method combination routine
 *     ADIcOpenRCB              C representation OPEN callback
 *     ADIfOpenRCB              Fortran representation OPEN callback
 */

#if !defined(_ADI_TYPES_H_)

#define _ADI_TYPES_H_ 1

#define ADI_MAJOR_VERSION	1
#define ADI_MINOR_VERSION	0

#ifndef __MSDOS__
#include "f77.h"
#define  ADI_F77   1
#endif

#include <stdlib.h>
#include <float.h>

/* Globally visible constants */

#define ADI__MXBLK      2048
#define ADI__MXDIM      7

/* Internal configuration constants */

#define ADI__EXCLSTAB   16
#define ADI__EXHANTAB   512

/* Data access modes */

#define ADI__AC_VALUE      1
#define ADI__AC_MEMBER     2
#define ADI__AC_PROPERTY   3


/* Raw object identifier - this is what's given to client s/w */

#ifdef __MSDOS__
#define INT32 long
#define INT32_MIN LONG_MIN
#define INT32_MAX LONG_MAX
#else
#define INT32_MIN INT_MIN
#define INT32_MAX INT_MAX
#define INT32 int
#endif

/*
 * Define the standard types
 */
typedef
  INT32                 ADIstatype;

typedef
  ADIstatype            *ADIstatus;

typedef
  char			ADIbyte;
typedef
  unsigned char		ADIubyte;
typedef
  short			ADIword;
typedef
  unsigned short	ADIuword;
typedef
  INT32			ADIinteger;
typedef
  float			ADIreal;
typedef
  double		ADIdouble;
typedef
  int			ADIlogical;
typedef
  void			*ADIpointer;
  
typedef
  INT32                 ADIobj;

/*
 * ADI interface language
 */
typedef
  enum {
    ADIlangC, ADIlangF77
    }
  ADIilang;


typedef
  struct {
    char	*data;
    int		len;
    ADIilang	lang;
    }
  ADIspr;

#define GENADI_CSP(_name,_data) \
  ADIspr _name = {_data,_CSM,ADIlangC}
#define GENADI_CSP_N(_name,_data,_n) \
  ADIspr _name = {_data,_n,ADIlangC}
#define GENADI_FSP(_name,_n) \
  ADIspr _name = {_data,_CSM,ADIlangF77}

typedef
  struct {
    char        *data;
    size_t      len;
    }
  ADIstring;

/*  Define true and false. Not done by enumeration so assignment to bit
 *  fields is ok.
 */
#define ADI__true (1==1)
#define ADI__false (!ADI__true)

typedef
  enum {
    ADI__read, ADI__write, ADI__update
    }
  ADIacmode;

typedef
  short        ADIidIndex;
#define ADI_MAX_IDINDEX SHRT_MAX

typedef
  struct ADIclassDefTag *ADIclassDefPtr;

typedef
  union {
    struct {
      ADIidIndex        blk;            /* Basic block number */
      ADIidIndex        obj;            /* Slot number within basic block */
      } sep;
    ADIobj              id;             /* The whole thing */
    }
  ADIrawID;


typedef
  struct ADIobjHanData {
    ADIobj              id;             /* The object data */
    char		*data;		/* Result of _dtdat call? */
    ADIobj              pl;             /* Object property list */
    ADIobj		pid;		/* Object parent id */
    ADIobj		name;		/* Object name id */
    ADIobj		lock;		/* Object lock list */
    unsigned int        ref;            /* Reference count */
    unsigned            markdel:1;      /* Marked for delete? */
    unsigned            readonly:1;     /* Static data? */
    unsigned            dataset:1;      /* Data is set */
    unsigned		slice:1;	/* Object is a slice? */
    unsigned		recur:1;	/* Recursive operation tag */
    }
  ADIobjHan;


/*
 * Type denoting the different forms of symbol binding. The enumeration
 * values occupy a single bit so that they can be masked together to
 * form symbol binding search sets.
 */
typedef
  enum {
    ADI__class_sb = (1<<0),             /* Class definition */
    ADI__defn_sb = (1<<1),		/* A symbol value */
    ADI__trnsfm_sb = (1<<2),		/* A transformation */
    ADI__enum_sb = (1<<3),		/* Enumeration constant */
    ADI__generic_sb = (1<<4),		/* Generic function definition */
    ADI__fun_sb = (1<<5),		/* Function definition */
    ADI__mcf_sb = (1<<6), 		/* Method combination form */
    ADI__frep_sb = (1<<7),		/* File representation */
    ADI__command_sb = (1<<8)		/* Command name */
    }
  ADIsbindForm;


typedef
  struct {
    ADIsbindForm	form;		/* Form of symbolic binding */
    ADIobj		defn;		/* Definition qualifying name */
    ADIobj		next;		/* Next binding in list */
    }
  ADIsymBinding;


/*
 *  Kernel types
 */


#define		KT_DEFN_sbind    ADI_G_tdef_sbind
#define         _sbind_data(_x)  ((ADIsymBinding *) _ID_DATA(_x))
#define         _sbind_form(_x)  (_sbind_data(_x)->form)
#define         _sbind_defn(_x)  (_sbind_data(_x)->defn)
#define         _sbind_next(_x)  (_sbind_data(_x)->next)

#define		KT_DEFN_mapctrl    ADI_G_tdef_mapctrl
#define         _mapctrl_data(_x)  ((ADImapCtrl *) _ID_DATA(_x))
#define         _mapctrl_mode(_x)  (_mapctrl_data(_x)->mode)
#define         _mapctrl_nbyte(_x) (_mapctrl_data(_x)->nbyte)
#define         _mapctrl_type(_x)  (_mapctrl_data(_x)->type)
#define         _mapctrl_dynamic(_x)  (_mapctrl_data(_x)->dynamic)
#define         _mapctrl_dptr(_x)  (_mapctrl_data(_x)->dptr)
#define         _mapctrl_q(_x)     (_ID_BLOCK(_x)->cdef==&KT_DEFN_mapctrl)

#define		KT_DEFN_mthd	ADI_G_tdef_mthd
#define         _mthd_data(_x)  ((ADImethod *) _ID_DATA(_x))
#define         _mthd_args(_x)  (_mthd_data(_x)->args)
#define         _mthd_form(_x)  (_mthd_data(_x)->form)
#define         _mthd_exec(_x)  (_mthd_data(_x)->exec)
#define         _mthd_q(_x)     (_ID_BLOCK(_x)->cdef==&KT_DEFN_mthd)

#define		KT_DEFN_mco	ADI_G_tdef_mco
#define         _mco_data(_x)   ((ADImethComb *) _ID_DATA(_x))
#define         _mco_name(_x)   (_mco_data(_x)->name)
#define         _mco_cexec(_x)  (_mco_data(_x)->cexec)

#define		KT_DEFN_gnrc	ADI_G_tdef_gnrc
#define         _gnrc_data(_x)  ((ADIgeneric *) _ID_DATA(_x))
#define         _gnrc_name(_x)  (_gnrc_data(_x)->name)
#define         _gnrc_narg(_x)  (_gnrc_data(_x)->narg)
#define         _gnrc_args(_x)  (_gnrc_data(_x)->args)
#define         _gnrc_mcomb(_x) (_gnrc_data(_x)->mcomb)
#define         _gnrc_cdisp(_x) (_gnrc_data(_x)->cdisp)
#define         _gnrc_fdisp(_x) (_gnrc_data(_x)->fdisp)
#define         _gnrc_mlist(_x) (_gnrc_data(_x)->mlist)

#define		KT_DEFN_eprc	ADI_G_tdef_eprc
#define         _eprc_data(_x)  ((ADIeproc *) _ID_DATA(_x))
#define         _eprc_c(_x)     (_eprc_data(_x)->c)
#define         _eprc_prc(_x)   (_eprc_data(_x)->prc)
#define         _eprc_q(_x)     (_ID_BLOCK(_x)->cdef==&KT_DEFN_eprc)

#define		KT_DEFN_pdef	ADI_G_tdef_pdef
#define         _pdef_data(_x)  ((ADIsuperclassDef *) _ID_DATA(_x))
#define         _pdef_name(_x)  (_pdef_data(_x)->name)
#define         _pdef_clsid(_x) (_pdef_data(_x)->clsid)
#define         _pdef_next(_x)  (_pdef_data(_x)->next)

#define		KT_DEFN_mdef	ADI_G_tdef_mdef
#define         _mdef_data(_x)  ((ADImemberDef *) _ID_DATA(_x))
#define         _mdef_name(_x)  (_mdef_data(_x)->name)
#define         _mdef_nlen(_x)  (_mdef_data(_x)->nlen)
#define         _mdef_defcls(_x) (_mdef_data(_x)->defcls)
#define         _mdef_cdata(_x) (_mdef_data(_x)->cdata)
#define         _mdef_aname(_x) (_mdef_data(_x)->aname)
#define         _mdef_next(_x)  (_mdef_data(_x)->next)
#define         _mdef_q(_x)     (_ID_BLOCK(_x)->cdef==&KT_DEFN_mdef)

#define		KT_DEFN_mta	ADI_G_tdef_mta
#define         _mta_data(_x)   ((ADImta *) _ID_DATA(_x))

#define		KT_DEFN_cdef	ADI_G_tdef_cdef
#define         _cdef_q(_x)     (_ID_BLOCK(_x)->cdef==&KT_DEFN_cdef)
#define         _cdef_data(_x)  ((ADIclassDef *) _ID_DATA(_x))
#define         _cdef_ctrl(_x)  (&_cdef_data(_x)->alloc)
#define         _cdef_dest(_x)  (_cdef_data(_x)->destruc)
#define         _cdef_prnt(_x)  (_cdef_data(_x)->prnt)

#define		KT_DEFN_obj	ADI_G_tdef_obj
#define         _obj_data(_x)   ((ADIobj *) _DTDAT(_x))
#define		_obj_q(_x)      (_DTDEF(_x)->selfid==UT_cid_ref)

#define		KT_DEFN_han	ADI_G_tdef_han
#define         _han_q(_x)      (_ID_BLOCK(_x)->cdef==&KT_DEFN_han)
#define         _han_data(_x)   ((ADIobjHan *) _ID_DATA(_x))
#define         _han_id(_x)     (_han_data(_x)->id)
#define         _han_pid(_x)    (_han_data(_x)->pid)
#define         _han_name(_x)   (_han_data(_x)->name)
#define         _han_pl(_x)     (_han_data(_x)->pl)
#define         _han_lock(_x)   (_han_data(_x)->lock)
#define         _han_ref(_x)    (_han_data(_x)->ref)
#define         _han_set(_x)    (_han_data(_x)->dataset)
#define         _han_readonly(_x) (_han_data(_x)->readonly)
#define         _han_slice(_x)  (_han_data(_x)->slice)
#define         _han_recur(_x)  (_han_data(_x)->recur)


/*
 *  User types
 */
/* Byte */
#define         UT_CTYPE_b      ADIbyte
#define         UT_FTYPE_b      F77_BYTE_TYPE
#define         UT_cid_b      ADI_G_alloc_b
#define         UT_BAD_b        0x80
#define         UT_MIN_b        -128
#define         UT_MAX_b        127
#define         _byte_q(_x)     (_DTDEF(_x)->selfid==UT_cid_b)

/* Unsigned byte */
#define         UT_CTYPE_ub     ADIubyte
#define         UT_FTYPE_ub     F77_UBYTE_TYPE
#define         UT_cid_ub     ADI_G_alloc_ub
#define         UT_BAD_ub       0xFF
#define         UT_MIN_ub       0
#define         UT_MAX_ub       255
#define         _ubyte_q(_x)    (_DTDEF(_x)->selfid==UT_cid_ub)

/* Word */
#define         UT_CTYPE_w      ADIword
#define         UT_FTYPE_w      F77_WORD_TYPE
#define         UT_cid_w      ADI_G_alloc_w
#define         UT_BAD_w        0x8000
#define         UT_MIN_w        SHRT_MIN
#define         UT_MAX_w        SHRT_MAX
#define         _word_q(_x)     (_DTDEF(_x)->selfid==UT_cid_w)

/* Unsigned word */
#define         UT_CTYPE_uw     ADIuword
#define         UT_FTYPE_uw     F77_UWORD_TYPE
#define         UT_cid_uw     ADI_G_alloc_uw
#define         UT_BAD_uw       0xFFFF
#define         UT_MIN_uw       0
#define         UT_MAX_uw       USHRT_MAX
#define         _uword_q(_x)     (_DTDEF(_x)->selfid==UT_cid_uw)

/* Integer */
#define         UT_CTYPE_i      ADIinteger
#define         UT_FTYPE_i      F77_INTEGER_TYPE
#define         UT_cid_i      ADI_G_alloc_i
#define         UT_BAD_i        0x80000000L
#define         UT_MIN_i        INT32_MIN
#define         UT_MAX_i        INT32_MAX
#define         _int_q(_x)     (_DTDEF(_x)->selfid==UT_cid_i)

/* Single precision FP */
#define         UT_CTYPE_r      ADIreal
#define         UT_FTYPE_r      F77_REAL_TYPE
#define         UT_cid_r      ADI_G_alloc_r
#define         UT_BAD_r        0xFF7FFFFFL
#define         UT_MIN_r        (-FLT_MAX)
#define         UT_MAX_r        FLT_MAX
#define         _real_q(_x)     (_DTDEF(_x)->selfid==UT_cid_r)

/* Double precision FP */
#define         UT_CTYPE_d      ADIdouble
#define         UT_FTYPE_d      F77_DOUBLE_TYPE
#define         UT_cid_d      ADI_G_alloc_d
#define         UT_BAD_d        0xFFEFFFFFFFFFFFFFL
#define         UT_MIN_d        (-DBL_MAX)
#define         UT_MAX_d        DBL_MAX
#define         _dble_q(_x)     (_DTDEF(_x)->selfid==UT_cid_d)

/* Logical */
#define         UT_CTYPE_l      ADIlogical
#define         UT_FTYPE_l      F77_LOGICAL_TYPE
#define         UT_cid_l      ADI_G_alloc_l
#define         _logical_q(_x)  (_DTDEF(_x)->selfid==UT_cid_l)

/* Character string */
#define         UT_CTYPE_c      char *
#define         UT_cid_c      ADI_G_alloc_c
#define         _str_data(_x)   ((ADIstring *) _DTDAT(_x))
#define         _str_q(_x)      (_DTDEF(_x)->selfid==UT_cid_c)
#define         _str_len(_x)    (_str_data(_x)->len)
#define         _str_dat(_x)    (_str_data(_x)->data)

/* Pointers */
#define         UT_CTYPE_p      ADIpointer
#define         UT_FTYPE_p      F77_POINTER_TYPE
#define         UT_cid_p      ADI_G_alloc_p

/* Structures */
#define		UT_cid_struc	ADI_G_alloc_struc
#define		_struc_data(_x)	((ADIobj *) _DTDAT(_x))
#define		_struc_q(_x)    (_DTDEF(_x)->selfid==UT_cid_struc)

#define         _class_data(_x) ((ADIobj *) _DTDAT(_x))

/* Change bad, min and max for MSDOS floating point
 *
 */
#ifdef __MSDOS__
#undef		UT_BAD_d
#define         UT_BAD_d        0xFFEFFFFFFFFFFFFFL
#endif


/* Change bad, min and max for VMS floating point
 *
 */
#ifdef VMS
#undef          UT_BAD_r
#define         UT_BAD_r        0xFFFFFFFF
#undef          UT_MIN_r
#define         UT_MIN_r        0xFFFEFFFF
#undef          UT_MAX_r
#define         UT_MAX_r        0xFFFF7FFF
#undef          UT_BAD_d
#define         UT_BAD_d        0xFFFFFFFFFFFFFFFF
#undef          UT_MIN_d
#define         UT_MIN_d        0xFFFEFFFFFFFFFFFF
#undef          UT_MAX_d
#define         UT_MAX_d        0xFFEFFFFFFFFF7FFF
#endif

#ifdef VMS
#define         _TM_name(_root,_t)   _root/**/_t
#define         _TM_fname(_root,_t)   _root/**/_t
#define         _TM_ctype(_t)        UT_CTYPE_/**/_t
#define         _TM_ftype(_t)        UT_FTYPE_/**/_t
#define         _TM_alloc(_t)        UT_cid_/**/_t
#define         _TM_bad(_t)          UT_BAD_/**/_t
#define         _TM_min(_t)          UT_MIN_/**/_t
#define         _TM_max(_t)          UT_MAX_/**/_t
#ifdef F77_TRUNC
#define         _TM_fnames(_root,_t) #_root #_t
#else
#define         _TM_fnames(_root,_t) "ADI_" #_root #_t
#endif

#else
#define         _TM_name(_root,_t)   _root##_t
#define         _TM_names(_root,_t)  #_root #_t
#ifdef F77_TRUNC
#define         _TM_fname(_root,_t)  _root##_t
#define         _TM_fnames(_root,_t) #_root #_t
#else
#define         _TM_fname(_root,_t)  adi_##_root##_t
#define         _TM_fnames(_root,_t) "ADI_" #_root #_t
#endif
#define         _TM_ctype(_t)        UT_CTYPE_##_t
#define         _TM_ftype(_t)        UT_FTYPE_##_t
#define         _TM_alloc(_t)        UT_cid_##_t
#define         _TM_bad(_t)          UT_BAD_##_t
#define         _TM_min(_t)          UT_MIN_##_t
#define         _TM_max(_t)          UT_MAX_##_t
#endif


/* Access block & slot parts of an object ID
 *
 */
#define _FORM_ID(_id,_block,_obnum) \
 {ADIrawID tem; tem.sep.blk=(_block);tem.sep.obj=(_obnum);_id = tem.id;}
#define _ID_BLOCK(_id)  (ADI_G_blks[(((ADIrawID *) &(_id))->sep.blk)])
#define _ID_IBLK(_id)   (((ADIrawID *) &(_id))->sep.blk)
#define _ID_SLOT(_id)   (((ADIrawID *) &(_id))->sep.obj)
#define _ID_TYPE(_id)   (_ID_BLOCK(_id)->cdef)
#define _ID_DATA(_id)   adix_idd(_id)

/* Predicates to help with object processing
 *
 */
#define _null_q(_id)    ((_id)==ADI__nullid)
#define _valid_q(_id)   ((_id)!=ADI__nullid)
#define _krnl_q(_id)    (_ID_BLOCK(_id)->cdef->kernel)
#define _prim_q(_id)    (_ID_BLOCK(_id)->cdef->prim)


/* Wrap ups to access object data info. Have to be clever for kernel objects
 * usually. Having these in definitions speeds things up a bit
 * 
 *  _DTDAT(id)	- access base data location, ie. the data address of a user
 *                object, or that of the referenced data in a kernel object
 *  _DTDEF(id)	- access base data type definition, ie. the type of a user
 *                object, or that of the referenced data in a kernel object
 */
#define _DTDAT(_id)     adix_dtdat(_id,status)
#define _DTDEF(_id)     adix_dtdef(_id,status)


/* Macro to move memory from one place to another */

#define _CH_MOVE(_dest,_source,_nc) \
 {char	*oOp = _dest; char *iIp = _source;\
 ADIinteger i=_nc;for(;i;i--) *oOp++ = *iIp++;}

#define ADI__nullid     (-1)

/*
 *  Macros to access primitive data
 */
#define _IVAL(_x)	*((ADIinteger *) _DTDAT(_x))
#define _LVAL(_x)	*((ADIlogical *) _DTDAT(_x))
#define _RVAL(_x)	*((ADIreal *) _DTDAT(_x))
#define _DVAL(_x)	*((ADIdouble *) _DTDAT(_x))

/*
 *  Various callback types
 */
typedef
  void (*ADICB)(void);

typedef
  ADIobj (*ADIcMethodCB)(int,ADIobj [],ADIstatus);

typedef
  void (*ADIfMethodCB)(int *,ADIobj [],ADIobj *,ADIstatus);

typedef
  void (*ADIcOpenRCB)(ADIobj,ADIobj,ADIobj *,ADIstatus);

typedef
  void (*ADIfOpenRCB)(ADIobj *,ADIobj *,ADIobj *,ADIstatus);

typedef
  void (*ADIcCreatRCB)(ADIobj,ADIobj,ADIobj *,ADIstatus);

typedef
  void (*ADIfCreatRCB)(ADIobj *,ADIobj *,ADIobj *,ADIstatus);

typedef
  ADIobj (*ADIcGenericDispatchCB)(ADICB, int,ADIobj [],ADIstatus);

typedef
  ADIobj (*ADIfGenericDispatchCB)(ADICB *, int *,ADIobj [],ADIstatus);

typedef
  ADIobj (*ADIcMethodCombinationCB)(ADIobj,int,ADIobj [],ADIstatus);

typedef
  void (*ADIooCB)(ADIobj,ADIobj,ADIstatus);

typedef
  void (*ADIoCB)(ADIobj,ADIstatus);

typedef
  void (*ADIfooCB)(ADIobj *,ADIobj *,ADIstatus);

typedef
  void (*ADIfoCB)(ADIobj *,ADIstatus);

typedef
  void (*ADIcbcOO)(ADIobj,ADIobj,ADIstatus);

typedef
  void (*ADIcbfOO)(ADIobj *,ADIobj *,ADIstatus);

#define _CSM  (-1)
#define _CSTRING_Q(_x) ( (_x)==_CSM ? ADI__true : ADI__false)

#define _GET_STRING(_name,_len) \
  if ( (_len)==_CSM ) (_len) = _name ? strlen(_name) : 0

#define _GET_NAME(_name,_len) \
  _GET_STRING(_name,_len); if ( _len ) adix_ntrunc( _name, &_len );


/*  This structure stores the info required to control a basic block
 *  allocator.
 */


typedef
  struct {
    ADIclassDefPtr	cdef;		/* Class definition */
    size_t              size;           /* Size of an allocatable unit */
    ADIidIndex          nunit;          /* Number of units per block */
    ADIidIndex          f_block;        /* First block in list */
    }
  ADIblockCtrl;


typedef
  struct ADIblockData {
    ADIclassDefPtr	cdef;		/* Class definition */
    char		*data;		/* Block data */
    size_t		size;		/* Copy of size */
    union {
      struct {
	unsigned char   *used;          /* Allocation bits */
	ADIidIndex	ffree;		/* First free object in this block */
	} 		std;		/* Standard block stuff */
      struct {
	ADIinteger	ntotal;		/* Total # units in master/slave grp */
	}		mas;		/* Master block stuff */
      } 		vdat;		/* variable data */
    ADIidIndex          next;           /* Next block for this type */
    ADIidIndex		nunit;		/* Number of units in this block */
    ADIidIndex		master;		/* Non-null if a slave block */
    ADIidIndex		nfree;		/* No. slots free in block */
    }
  ADIblock;

typedef
  struct {
    ADIobj		name;		/* Superclass name */
    ADIobj		clsid;		/* Class definition id */
    ADIobj		next;		/* Next superclass in chain */
    }
  ADIsuperclassDef;

typedef
  struct ADImemberDefTag {
    char                *name;          /* Name of member */
    int			nlen;		/* Length of name */
    ADIobj              aname;          /* Name in common string table */
    ADIobj              defcls;         /* Default class for creation */
    ADIobj		cdata;		/* Constant data? */
    int                 islot;          /* Slot number in class data */
    ADIobj		next;		/* Next member in chain */
    }
  ADImemberDef;

/*
 *  Memory transfer structure
 */
typedef
  struct {
    ADIclassDefPtr	tdef;		/* Class definition of data */
    int              	size;           /* Element size */
    int                 ndim;           /* Dimensionality */
    int                 udims[ADI__MXDIM];        /* Used subset of data */
    int                 uorig[ADI__MXDIM];        /* Used subset of data */
    int                 ddims[ADI__MXDIM];
    void                *data;          /* Data address */
    ADIobj              id;             /* Object identifier */
    unsigned		clang:1;	/* C language target? */
    unsigned 		contig:1;	/* Data is contiguous? */
    unsigned		trunc:1;	/* Data truncated on transfer? */
    }
  ADImta;


typedef
  struct ADIclassDefTag {
    ADIblockCtrl        alloc;          /* Allocation control info */
    char		*name;          /* C string name */
    ADIobj		aname;	        /* ADI name string */
    ADIobj		selfid;		/* Self object container */
    ADIobj		superclasses;   /* The superclass list */
    ADIobj		sdslist;	/* Direct supers set for method work */
    ADIobj		members;	/* Member list */
    int     		defmem;         /* Default member for access */
    char 		*pdata;		/* Primitive data initialiser */
    int			nslot;		/* # of slots needed to hold data */
    ADIclassDefPtr      link;           /* Next class definition structure */
    ADIobj		bexpr[3];	/* Constant blank expressions */
    unsigned		kernel:1;	/* Kernel type? */
    unsigned 		prim:1;		/* Primitive type? */
    unsigned		meminit:1;	/* Class member initialisation required */
    unsigned		adider:1;	/* Derived from ADIbase? */
    ADIobj		cnvs;		/* Convertor functions */
    ADIobj		destruc;	/* Destructor */
    ADIobj		prnt;		/* Printer function */
    ADIobj		istrm;		/* Input streamer function */
    ADIobj		ostrm;		/* Output streamer function */
    }
  ADIclassDef;

#define DEF_MEMBER_FLAG_VALUE (-1)

#define _DEF_STATIC_ALLOC(suffix,alloc_unit,_type) \
   {&ADI_G_tdef_##suffix, sizeof(_type), alloc_unit, ADI__nullid}

#define _DEF_STATIC_CDEF(_name,_abb,_bsize,_type) \
  ADIclassDef ADI_G_tdef_##_abb = \
  {_DEF_STATIC_ALLOC(_abb,_bsize,_type), \
  _name, ADI__nullid, ADI__nullid, \
  ADI__nullid, ADI__nullid, ADI__nullid, DEF_MEMBER_FLAG_VALUE, \
  NULL, 0, NULL, \
  {ADI__nullid, ADI__nullid, ADI__nullid}, ADI__true, ADI__true, \
  ADI__false, ADI__false, \
  ADI__nullid, ADI__nullid, ADI__nullid, ADI__nullid, ADI__nullid }


typedef
  struct {
    void		*dptr;		/* Mapped data pointer */
    ADIacmode		mode;		/* Access mode */
    ADIclassDef		*type;		/* The mapping type */
    size_t		nbyte;		/* Amount of mapped dynamic memory */
    unsigned int	nref;		/* Reference count */
    char		dynamic;	/* Mapped data dynamically? */
    }
  ADImapCtrl;


/*  ADIeproc
 *
 *   prc        - Address of the procedure
 *   c          - Is this a C procedure?
 */
typedef
  struct {
    void                (*prc)();
    char                c;
    }
  ADIeproc;


/*  ADImethComb
 *
 *   name       - entry in the common string table holding the name of
 *                the method combination form
 *   cexec      - C routine performing method dispatch
 */
typedef
  struct {
    ADIobj              name;
    ADIobj              cexec;
    }
  ADImethComb;


/*  ADIgeneric
 */
typedef
  struct {
    ADIobj              name;		/* Common string table name entry */
    short               narg;		/* Number of arguments */
    ADIobj              args;		/* Argument specifications */
    ADIobj              mcomb;		/* Method combination form */
    ADIobj              cdisp;		/* C dispatch procedure */
    ADIobj              fdisp;		/* Fortran dispatch procedure */
    ADIobj		mlist;		/* Associated method list */
    }
  ADIgeneric;


/*  ADImethod
 *
 *   form       - primary, before, after or around method?
 *   exec       - executor routine
 */
typedef
  struct {
    ADIobj              args;
    ADIobj              form;
    ADIobj              exec;
    }
  ADImethod;


typedef
  enum {
    ADIcmpValue, ADIcmpMember, ADIcmpProperty, ADIcmpStruct
    }
  ADIcmpType;

typedef
  struct {
    ADIobj		*data;
    ADIobj		parent;
    ADIobj		name;
    ADIobj		*lentry;
    ADIcmpType		ctype;
    }
  ADIobjRequest;


#define _chk_stat       if ( ! _ok(status) ) return
#define _chk_stat_ret(_x) if ( ! _ok(status) ) return (_x)
#define _chk_init	if ( ! ADI_G_init ) adi_init( status )
#define _chk_init_err	if ( ! ADI_G_init ) adic_setecs(ADI__NOTACT,0,status)
#define _chk_ndim(_x)   if ( (ndim<0) || (ndim>ADI__MXDIM)) \
 {adic_setecs(ADI__INVARG,"Dimensionality must lie in range 0..%d",status,ADI__MXDIM);}
#define _chk_han(_x)    if ( ! _han_q(_x) ) adic_setecs( ADI__ILLKOP, 0,status )

#define _chk_dims(_n,_d)

#define _chk_type(_id,_t)

/*
 * A type and macro to help with common string table definition
 */
typedef
  struct {
    ADIobj	*saddr;
    char	*string;
    }
  ADIcstrTableEntry;

#define DEFINE_CSTR_TABLE(_name) \
  static ADIcstrTableEntry _name[] = {
#define CSTR_TENTRY(_var,_string)  {&_var,_string}
#define END_CSTR_TABLE	{NULL,NULL}}

/*
 * A type and macro to help with command parser
 */

typedef
  struct {
    char		*name;
    ADIcMethodCB        cb;
    }
  ADIcmdParTableEntry;

#define DEFINE_CMDPAR_TABLE(_name) \
  static ADIcmdParTableEntry _name[] = {
#define CMDPAR_TENTRY(_name,_cb)  {_name,(ADIcMethodCB) _cb}
#define END_CMDPAR_TABLE	CMDPAR_TENTRY(NULL,NULL) }

/*
 * A type and macro to help with method loading
 */
typedef
  struct {
    char		*name;
    ADIcMethodCB	cb;
    }
  ADImthdTableEntry;

#define DEFINE_MTHD_TABLE(_name) \
  static ADImthdTableEntry _name[] = {
#define MTHD_TENTRY(_name,_cb)  {_name,(ADIcMethodCB) _cb}
#define END_MTHD_TABLE	MTHD_TENTRY(NULL,NULL) }

typedef
  struct {
    char        *spec;
    ADIcGenericDispatchCB       cdisp;
    ADIfGenericDispatchCB       fdisp;
    }
  ADIgnrcTableEntry;

#define DEFINE_GNRC_TABLE(_name) \
  static ADIgnrcTableEntry _name[] = {
#define GNRC_TENTRY(_name,_ccb,_fcb)  {_name,_ccb,_fcb}
#define END_GNRC_TABLE	GNRC_TENTRY(NULL,NULL,NULL) }

typedef
  struct {
    char		*name;
    size_t		size;
    ADIobj		*alloc;
    ADIcMethodCB	prnt;
    ADIcMethodCB	istrm;
    ADIcMethodCB	ostrm;
    }
  ADIptypeTableEntry;

#define DEFINE_PTYPE_TABLE(_name) \
  static ADIptypeTableEntry _name[] = {
#define PTYPE_TENTRY(_name,_type,_alloc,_prnt,_istrm,_ostrm)  \
	{_name,sizeof(_type),_alloc,(ADIcMethodCB) _prnt, \
	(ADIcMethodCB) _istrm,(ADIcMethodCB) _ostrm}
#define END_PTYPE_TABLE	PTYPE_TENTRY(NULL,int,NULL,NULL,NULL,NULL) }

typedef
  enum {
    SA_Listable = 1, SA_HoldAll = 2, SA_HoldFirst = 4, SA_HoldRest = 8
    }
  ADIsymAttribute;

#define FA_L	(SA_Listable)
#define FA_1	(SA_HoldFirst)
#define FA_A	(SA_HoldAll)
#define FA_L1	(SA_Listable|SA_HoldFirst)

typedef
  struct {
    char		*spec;
    ADIcMethodCB	exec;
    int			flags;
    }
  ADIfuncTableEntry;

#define DEFINE_FUNC_TABLE(_name) \
  static ADIfuncTableEntry _name[] = {
#define FUNC_TENTRY(_spec,_cb,_flgs)  {_spec,(ADIcMethodCB) _cb,_flgs}
#define END_FUNC_TABLE	FUNC_TENTRY(NULL,NULL,0) }

typedef
  struct {
    ADIobj	name;
    ADIstatype	code;
    ADIobj	errtext;
    ADIobj	stack;
    }
  ADIexception;

#define _INIT_EXCEPTION  {ADI__nullid,SAI__OK,ADI__nullid,ADI__nullid}

typedef
  struct {
    ADIobj 	func;
    ADIobj	syms;
    ADIobj	*top;
    unsigned short first_slot;
    unsigned char nslot;
    }
  ADIstackFrame;

#define UT_cid_strm	ADI_G_alloc_strm

typedef
  struct {
    ADIblock        	*blks; 		/* Block address array */
    long          	nbyte;      	/* Global ADI memory count */
    long                nalloc;     	/* Number of memory allocations */
    ADIlogical		init;		/* Interpreter is initialised? */
    ADIlogical		init_fail;	/* Initialisation has failed */
    ADIobj		cstring;	/* Common string table */
    ADIexception	exec;		/* Exception handling */
    ADIobj		StdIn;		/* Input stream */
    ADIobj		StdOut;		/* Output stream */
    ADIobj		StdErr;		/* Error stream */
    }
  ADIinterp;

#define _INIT_INTERP { \
  NULL, 0L, 0L, \
  ADI__false, ADI__false, \
  ADI__nullid, _INIT_EXCEPTION, \
  ADI__nullid, ADI__nullid, ADI__nullid }


/*
 *  End of _ADI_H aleady defined test
 */
#endif
