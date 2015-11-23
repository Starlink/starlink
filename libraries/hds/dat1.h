#if !defined( DAT1_INCLUDED )	 /* dat1.h already included?		    */
#define DAT1_INCLUDED 1

#if defined( vms )		 /* VMS version include files:		    */
#include <descrip.h>		 /* Data descriptor definitions for VMS	    */
#endif

#include "f77.h"		 /* Fortran <=> C macro definitions	    */

/* We may need a configure test for this */
#if HAVE_CONFIG_H
#include <config.h>
#endif

#if HAVE_INTTYPES_H
#include <inttypes.h>            /* int64_t                                 */
#endif

/* Global Constants:							    */
/* ================							    */
/* The following are "public" values. Copies are made available externally  */
/* in the dat_par.h and dat_par(.f) files.				    */
#define DAT__MXDIM 7		 /* Maximum number of object dimensions	    */
#define DAT__NOLOC "<NOT A LOCATOR> "/*	Null (invalid) locator value	    */
#define DAT__NOWLD 0		 /* Null wild-card search context	    */
#define DAT__ROOT  "<ROOT LOCATOR>  "/* Root locator value		    */
#define DAT__SZGRP 15		 /* Size of group name			    */
#define DAT__SZLOC ( ( 15 > (int) sizeof( struct LOC ) ) ? \
		     15 : (int) sizeof( struct LOC ) )
				 /* Size of locator string		    */
#define DAT__SZMOD 15		 /* Size of access mode string		    */
#define DAT__SZNAM 15		 /* Size of object name			    */
#define DAT__SZTYP 15            /* Size of type string			    */
#ifdef vms
#define DAT__FLEXT ".SDF"        /* Default HDS file extension              */
#else
#define DAT__FLEXT ".sdf"        /* Default HDS file extension              */
#endif
#define DAT__SZFLX 4             /* Length of DAT__FLEXT                    */

/* The following are "private" values. These are only used internally.	    */
#define DAT__CONTAINER 1	 /* Container record class		    */
#define DAT__STRUCTURE 2	 /* Structure record class		    */
#define DAT__COMPONENT 3	 /* Component record class		    */
#define DAT__PRIMITIVE 4	 /* Primitive record class		    */

#define DAT__LOCCHECK 0x7f7f7f7f /* Locator check value			    */
#define DAT__MXSLICE 3		 /* Maximum no. of slice dimensions	    */
#define DAT__SZCRV 24            /* Size of Component Record Vector element */
#define DAT__SZOCRV 20           /* Size of HDS V3 CRV element              */
#define DAT__SZDIM 4		 /* Size of a packed ODL dimension size	    */
#define DAT__SZNCOMP 4		 /* Size of a packed component count	    */
#define DAT__SZNDIM 1		 /* Size of a packed ODL dimension count    */
#define DAT__SZSRV 8		 /* Size of Structure Record Vector element */
#define DAT__SZOSRV 4		 /* Size of HDS V3 SRV element              */

#define LOWER 0			 /* Lower-bound vector element		    */
#define UPPER 1			 /* Upper-bound vector element		    */

/* Primitive data type codes:						    */
/* =========================						    */
/* Note these values should not be changed, as they are encoded in data	    */
/* files.								    */
#define DAT__B 0		 /* _BYTE				    */
#define DAT__C 1	   	 /* _CHAR				    */
#define DAT__D 2		 /* _DOUBLE				    */
#define DAT__I 3		 /* _INTEGER				    */
#define DAT__L 4		 /* _LOGICAL				    */
#define DAT__R 5		 /* _REAL				    */
#define DAT__UB 6		 /* _UBYTE				    */
#define DAT__UW 7		 /* _UWORD				    */
#define DAT__W 8		 /* _WORD                                   */
#define DAT__K 9                 /* _INT64                                  */

#define DAT__MXPRM 10		 /* Number of primitive data types	    */

/* Primitive data storage order:					    */
/* =============================					    */
/* Note these values should not be changed, as they are encoded in data	    */
/* files.								    */
#define DAT__MSB 0		 /* Most significant byte first		    */
#define DAT__LSB 1		 /* Least significant byte first	    */

/* Primitive data formats:						    */
/* =======================						    */
/* Note these values should not be changed, as they are encoded in data	    */
/* files.								    */
#define DAT__BIT0 0x0		 /* Logical: bit 0 encodes the value	    */
#define DAT__NZ 0x1		 /* Logical: non-zero ==> TRUE		    */
#define DAT__BINARY 0x10	 /* Binary encoded unsigned integer	    */
#define DAT__2COMP 0x11		 /* 2's complement encoded signed integer   */
#define DAT__VAXF 0x20		 /* VAX single precision (F format)	    */
#define DAT__IEEE_S 0x21	 /* IEEE single precision floating point    */
#define DAT__VAXD 0x30		 /* VAX double precision (D format)	    */
#define DAT__IEEE_D 0x31	 /* IEEE double precision floating point    */
#define DAT__ASCII 0x40		 /* ASCII characters			    */

#define DAT__UNKNOWN 0xff	 /* Format unknown (used to trap errors)    */
#define DAT__MXCHR 0xffff	 /* Max characters in a character data type */

/* Primitive type definitions:						    */
/* ===========================						    */
#define _BYTE F77_BYTE_TYPE
#define _CHAR F77_CHARACTER_TYPE
#define _DOUBLE F77_DOUBLE_TYPE
#define _INTEGER F77_INTEGER_TYPE
#define _LOGICAL F77_LOGICAL_TYPE
#define _REAL F77_REAL_TYPE
#if defined( F77_UBYTE_TYPE )
#define _UBYTE F77_UBYTE_TYPE
#else
#define _UBYTE unsigned F77_BYTE_TYPE
#endif
#if defined( F77_UWORD_TYPE )
#define _UWORD F77_UWORD_TYPE
#else
#define _UWORD unsigned F77_WORD_TYPE
#endif
#define _WORD F77_WORD_TYPE

/* 64-bit integer type */
#ifdef F77_INTEGER8_TYPE
#define _INT64 F77_INTEGER8_TYPE
#else
#define _INT64 int64_t
#endif

/* Workaround until this definition is added to f77.h:			    */
#if !defined( F77_POINTER_TYPE )
#if defined( __alpha )
#define F77_POINTER_TYPE int
#else
#define F77_POINTER_TYPE void *
#endif
#endif

      typedef int INT4;		 /* Signed int with at least 4 bytes	    */
      typedef unsigned int UINT4;/* Unsigned int with at least 4 bytes	    */

/* Macros:								    */
/* ======								    */
/* Determine if a _DOUBLE value passed from Fortran will be adequately	    */
/* aligned to perform C arithmetic or conversion operations on it. The	    */
/* argument is a pointer to the value, the macro should return 0 or 1. The  */
/* required behaviour is determined by the (externally defined) _noalign    */
/* macro.								    */
#if defined( _noalign )
#define _aligned_D( p ) 1	 /* Handles any alignment		    */

#else
#define _aligned_D( p ) ( ( ( (unsigned long int) (p) ) %\
                            ( (unsigned long int) sizeof( _DOUBLE ) ) ) == 0 )
				 /* _DOUBLE must be aligned on double	    */
				 /* boundary				    */
#endif

/* Determine the size of sructures depending the 64-bit mode type  */
#define SZSRV (hds_gl_64bit ? DAT__SZSRV : DAT__SZOSRV)
#define SZCRV (hds_gl_64bit ? DAT__SZCRV : DAT__SZOCRV)
#define SET_64BIT_MODE(han) (hds_gl_64bit =\
    (rec_ga_fcv[han->slot].hds_version > REC__VERSION3))

/* Check status and return on error after setting error message */
#define _call(event)\
{\
*status = (event);\
if (!_ok(*status))\
	{\
        hds_gl_status = *status;\
        emsRep(context_name,context_message,status);\
	return hds_gl_status;\
        }\
}

/* Check status and return on error after setting error message. Error
   message can be supplied by caller. */
#define _callm(event, errmsg)				\
{\
*status = (event);\
if (!_ok(*status))\
	{\
        hds_gl_status = *status;\
        emsRep(context_name,context_message errmsg,status);\
	return hds_gl_status;\
        }\
}



/* Check status and return void after setting error message */

#define _callv(event)\
{\
*status = (event);\
if (!_ok(*status))\
	{\
        hds_gl_status = *status;\
        emsRep(context_name,context_message,status);\
	return;\
        }\
}

/* Check status and return on error after setting error message that
   includes the component name. Variable "locator" must be in scope
   and refer to the relevant locator. */
#define _callnam(event)\
{\
*status = (event);\
if (!_ok(*status))\
	{\
        char private_context_message[132];\
        char dname[DAT__SZNAM+1];\
        int privstat = DAT__OK;\
        emsMark();\
        datName(locator, dname, &privstat );\
        if (privstat != DAT__OK) dname[0] = '\0';\
        emsAnnul(&privstat);\
        emsRlse();\
        sprintf( private_context_message,\
             context_message ": '%s'", dname);\
        hds_gl_status = *status;\
        emsRep(context_name,private_context_message,status);\
	      return hds_gl_status;\
        }\
}

/* Data Structure Definitions:						    */
/* ==========================						    */
/* PRM - Union to hold all the primitive data types.			    */
      union PRM
      {
	 _BYTE B;
	 _DOUBLE D;
	 _INTEGER I;
	 _LOGICAL L;
	 _REAL R;
	 _UBYTE UB;
	 _UWORD UW;
	 _WORD W;
         _CHAR C;
         _INT64 K;
      };

/* NDR - Native data representation.					    */
      struct NDR
      {
	 union PRM bad;		 /* "Bad" data value			    */
	 union PRM max;		 /* Maximum value			    */
	 union PRM min;		 /* Minimum (most negative) value	    */
	 const char *name;	 /* Pointer to data type name		    */
         unsigned short int length; /* Size of data element		    */
	 unsigned char format;	 /* Data format code			    */
	 unsigned char order;	 /* Storage order code			    */
	 unsigned char digits;	 /* No. decimal digits of precision	    */
	 unsigned char txtsize;  /* Characters required for formatting	    */
      };

/* PDD - Primitive Data Descriptor.					    */
      struct PDD
      {
         unsigned short int length; /* Object length			    */
         unsigned char dtype;	 /* Object type				    */
         unsigned char class;	 /* Object class			    */
	 unsigned char format;   /* Number format			    */
	 unsigned char order;    /* Character (byte) storage order	    */
         unsigned char *body;	 /* Pointer to object			    */
      };

/* ODL - Object Descriptor Label.					    */
      struct ODL
      {
         char type[ DAT__SZTYP ];    /* Object type specification	    */
         int naxes;		     /* Number of axes			    */
         HDS_PTYPE axis[ DAT__MXDIM ]; /* Vector of axis sizes		    */
      };

/* LCP - Locator Control Packet.					    */

/* LCP dynamic state flags						    */
      struct LCP_STATE
      {
         int broken;		 /* Object is broken (discontiguous)	    */
         int cell;		 /* Object is a cell			    */
         int mapped;		 /* Object data is mapped		    */
         int slice;		 /* Object is a slice			    */
         int unlike;		 /* Unlike data types			    */
         int vector;		 /* Object is vectorised		    */
         int vmcopy;		 /* Program has memory copy of data	    */
      };

/* LCP data fields							    */
      struct LCP_DATA
      {
         struct HAN han;	 /* Record handle			    */
         struct RID parent;	 /* Parent record ID			    */
         struct LCP_STATE state; /* LCP dynamic state flags		    */
         struct PDD app;	 /* Application primitive data descriptor   */
         struct PDD obj;	 /* Object primitive data descriptor	    */
         HDS_PTYPE bounds[ 3 ][ 2 ];/* Dimension bounds			    */
         int level;		 /* Component level			    */
         int naxes;		 /* Number of axes			    */
         UINT_BIG offset;        /* Slice or cell offset		    */
         UINT_BIG size;	         /* Size of object			    */
         char group[ DAT__SZGRP + 1 ]; /* Group specification		    */
         char name[ DAT__SZNAM + 1 ]; /* Name  specification		    */
         char type[ DAT__SZTYP + 1 ]; /* Type specification		    */
         char mode;		 /* Access mode				    */
	 int filemap;		 /* Accessed by file-mapping?		    */
         int read;		 /* Read-only?				    */
         int struc;		 /* Structure object?			    */
         int valid;		 /* Locator is valid			    */
         int erased;             /* Target of LCP has been erased by datErase*/
#if defined ( vms )
         struct dsc$descriptor vmsdsc; /* Descriptor for mapped characters  */
#endif
      };

      struct LCP
      {
         struct LCP *flink;	 /* Forward link to next LCP		    */
         struct LCP *blink;	 /* Backward link to last LCP		    */
         struct LCP_DATA data;	 /* LCP data fields			    */
         unsigned int seqno;	 /* Locator sequence number		    */
	 int primary;		 /* Is this a primary locator?		    */
      };

/* LOC - Locator.							    */
      struct LOC
      {
         struct LCP *lcp;	 /* Address of Locator Control Packet	    */
         unsigned int check;	 /* Validity check			    */
         unsigned int seqno;	 /* Sequence number			    */
      };

/* External Variables:							    */
/* ==================							    */
      extern struct LCP *dat_ga_wlq; /*	Working Locator Queue		    */
      extern struct LCP *dat_ga_flq; /*	Free Locator Queue		    */
      extern int dat_gl_wlqsize; /* Working Locator Queue size		    */
      extern struct NDR dat_gl_ndr[ DAT__MXPRM ]; /* Native data rep.	    */

/* Function Prototypes:							    */
/* ===================							    */
      int dat1_alloc_lcp( struct LOC **loc, struct LCP **lcp );
      void dat1_annul_lcp( struct LCP **lcp );
      void dat1_check_mode( const char *mode, INT mode_len, char *modechar,
			    INT *status );
      int dat1_check_type( const struct DSC *type, char ptype[ DAT__SZTYP ] );
      int dat1_cvt( int bad, UINT_BIG nval, struct PDD *imp, struct PDD *exp,
                    int *nbad );
      int dat1_cvt_char( int bad, UINT_BIG nval, struct PDD *imp, struct PDD *exp,
                         int *nbad);
      int dat1_cvt_dtype( int bad, UINT_BIG nval, struct PDD *imp, struct PDD *exp,
                          int *nbad );
      void dat1_cvt_format( int bad, UINT_BIG nval, const struct PDD *imp,
                            struct PDD *exp, int *nbad, int *status );
      void dat1_cvt_order( UINT_BIG nval, const struct PDD *imp, struct PDD *exp,
                           int *status );
      void dat1_decoy( long int arg1, void *arg2 );
      void dat1_free_hdsloc( struct LOC ** loc );
      void dat1_getenv( const char *varname, int def, int *val );
      int dat1_get_ncomp( const struct HAN *han, int *ncomp );
      int dat1_get_odl( const struct HAN *han, struct ODL *odl );
      int dat1_get_off( int ndim, const HDS_PTYPE *dims, const HDS_PTYPE *subs,
                     UINT_BIG *offset );
      int dat1_import_loc( const struct LOC *loc,
                            struct LCP **lcp );
      int dat1_init( void );
      void dat1_init_ndr( int *status );
      void dat1_intune( int *status );
      int dat1_locate_name( unsigned char *pcrv, int i, char **name );
      int dat1_make_scratch( void );
      int dat1_move_object( int ncomp, struct HAN *src, unsigned char *src_crv,
                            struct HAN *des, unsigned char *des_crv );
      int dat1_pack_crv( const struct RID *rid, int i, unsigned char *pcrv );
      int dat1_pack_odl( const struct ODL *odl, unsigned char *podl );
      int dat1_pack_srv( const struct RID *rid, unsigned char psrv[ ] );
      int dat1_put_ncomp( const struct HAN *han, int ncomp );
      int dat1_put_odl( const struct HAN *han, struct ODL *odl );
      void dat1_show_ndr( int *status );
      int dat1_unpack_crv( const unsigned char *pcrv, int i, struct RID *rid );
      int dat1_unpack_odl( const unsigned char *podl, struct ODL *odl );
      int dat1_unpack_srv( const unsigned char psrv[ ], struct RID *rid );
      int dat1_unpack_type( const char ptype[ DAT__SZTYP ], struct PDD *pdd );

      int dau_check_name( struct DSC *name, char *buf );
      int dau_check_shape( int ndim, const HDS_PTYPE *dims, struct ODL *odl );
      int dau_copy_object( int ncomp, struct HAN *src, unsigned char *src_crv,
                           struct HAN *des, unsigned char *des_crv );
      int dau_defuse_lcp( struct LCP **pntr );
      int dau_export_loc( struct DSC *locator, struct LCP **pntr );
      int dau_free_flq( void );
      int dau_flush_data( struct LCP_DATA *data );
      int dau_gather_data( int bad, struct LCP_DATA *data, int *nbad );
      int dau_get_shape( struct LCP_DATA *data, int *naxes, HDS_PTYPE *axis );
      int dau_import_loc( struct DSC *locator, struct LCP **pntr );
      int dau_match_types( struct PDD *obj, struct PDD *app );
      int dau_move_data( UINT_BIG nval, struct PDD *imp, struct PDD *exp );
      int dau_refill_flq( void );
      int dau_scatter_data( int bad, struct LCP_DATA *data, int *nbad );

      int hds1_check_group( struct DSC *group, char *buf );
      int hds1_cleanup( int *status );
      int hds1_encode_subs( int nlim, int nsub, HDS_PTYPE *subs, char *buf,
			    int *nchar );
      void hds1_exit( void );
      int hds1_get_subs( int ndim, HDS_PTYPE *dims, INT_BIG offset,
                         HDS_PTYPE  *subs );

      int dat1_import_floc ( const char flocator[DAT__SZLOC],
			      int loc_length, HDSLoc *clocator,
			      int * status);

#endif
