#if !defined(_ADI_FFACE_H_)
#define _ADI_FFACE_H_ 1

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/* 
 * Is name truncation required?
 */
#ifdef F77TRUNC
#define adifn(_x) _x
#else
#define adifn(_x) adi_##_x
#endif

/*
 * Interface to error system
 */
F77_SUBROUTINE(adifn(errmsg))( INTEGER(code), CHARACTER(buf) TRAIL(buf) );
F77_SUBROUTINE(adifn(setec))( INTEGER(code), INTEGER(status) );
F77_SUBROUTINE(adifn(setecs))( INTEGER(code), CHARACTER(ctx), INTEGER(status) TRAIL(ctx) );
F77_SUBROUTINE(adifn(setes))( CHARACTER(ctx), INTEGER(status) TRAIL(ctx) );
F77_SUBROUTINE(adifn(setetc))( CHARACTER(tok), CHARACTER(val) TRAIL(tok) TRAIL(val) );
F77_SUBROUTINE(adifn(seteti))( CHARACTER(tok), INTEGER(val) TRAIL(tok) );
#ifdef NOEMS
F77_SUBROUTINE(adifn(errctx))( CHARACTER(buf) TRAIL(buf) );
#endif

/*
 * Data creation
 */
F77_SUBROUTINE(adifn(new))( CHARACTER(cls), INTEGER(ndim),
		INTEGER_ARRAY(dims), INTEGER(id), INTEGER(status) TRAIL(cls) );
F77_SUBROUTINE(adifn(new0))( CHARACTER(cls), INTEGER(id),
		INTEGER(status) TRAIL(cls) );
F77_SUBROUTINE(adifn(new1))( CHARACTER(cls), INTEGER(nval),
		INTEGER(id), INTEGER(status) TRAIL(cls) );

#define _decl(_n) \
 F77_SUBROUTINE(adifn(_n))( INTEGER(ndim), INTEGER_ARRAY(dims), INTEGER(id), INTEGER(status) );
_decl(newb)	_decl(neww)	_decl(newi)	_decl(newr)
_decl(newd)	_decl(newl)	_decl(newc)
#undef _decl

#define _decl(_n) \
 F77_SUBROUTINE(adifn(_n))( INTEGER(id), INTEGER(status) );
_decl(new0b)	_decl(new0w)	_decl(new0i)	_decl(new0r)
_decl(new0d)	_decl(new0l)	_decl(new0c)
#undef _decl

#define _decl(_n) \
 F77_SUBROUTINE(adifn(_n))( INTEGER(nval), INTEGER(id), INTEGER(status) );
_decl(new1b)	_decl(new1w)	_decl(new1i)	_decl(new1r)
_decl(new1d)	_decl(new1l)	_decl(new1c)
#undef _decl

#define _decl(_t,_ft) \
 F77_SUBROUTINE(_TM_fname(newv,_t))( INTEGER(ndim), INTEGER_ARRAY(dims), _ft##_ARRAY(val), INTEGER(id), INTEGER(status) ); \
 F77_SUBROUTINE(_TM_fname(newv0,_t))( _ft(val), INTEGER(id), INTEGER(status) ); \
 F77_SUBROUTINE(_TM_fname(newv1,_t))( INTEGER(nval), _ft##_ARRAY(val), INTEGER(id), INTEGER(status) );

 _decl(b,BYTE)	_decl(w,WORD)	_decl(i,INTEGER)
 _decl(r,REAL)	_decl(d,DOUBLE)	_decl(l,LOGICAL)
#undef _decl

F77_SUBROUTINE(adifn(newvc))( INTEGER(ndim), INTEGER_ARRAY(dims), CHARACTER_ARRAY(str), INTEGER(id), INTEGER(status) TRAIL(str) );
F77_SUBROUTINE(adifn(newv0c))( CHARACTER(str), INTEGER(id), INTEGER(status) TRAIL(str) );
F77_SUBROUTINE(adifn(newv1c))( INTEGER(nval), CHARACTER_ARRAY(str), INTEGER(id), INTEGER(status) TRAIL(str) );


/*
 * Component data creation
 */

F77_SUBROUTINE(adifn(cnew))( INTEGER(pid), CHARACTER(name), CHARACTER(cls), INTEGER(ndim),
		INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) TRAIL(cls) );
F77_SUBROUTINE(adifn(cnew0))( INTEGER(pid), CHARACTER(name), CHARACTER(cls),
		INTEGER(status) TRAIL(name) TRAIL(cls) );
F77_SUBROUTINE(adifn(cnew1))( INTEGER(pid), CHARACTER(name), CHARACTER(cls), INTEGER(nval),
		INTEGER(status) TRAIL(name) TRAIL(cls) );

#define _decl(_n) \
 F77_SUBROUTINE(adifn(_n))( INTEGER(pid), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) );
_decl(cnewb)	_decl(cneww)	_decl(cnewi)	_decl(cnewr)
_decl(cnewd)	_decl(cnewl)	_decl(cnewc)
#undef _decl

#define _decl(_n) \
 F77_SUBROUTINE(adifn(_n))( INTEGER(pid), CHARACTER(name), INTEGER(status) TRAIL(name) );
_decl(cnew0b)	_decl(cnew0w)	_decl(cnew0i)	_decl(cnew0r)
_decl(cnew0d)	_decl(cnew0l)	_decl(cnew0c)
#undef _decl

#define _decl(_n) \
 F77_SUBROUTINE(adifn(_n))( INTEGER(pid), CHARACTER(name), INTEGER(nval), INTEGER(status) TRAIL(name) );
_decl(cnew1b)	_decl(cnew1w)	_decl(cnew1i)	_decl(cnew1r)
_decl(cnew1d)	_decl(cnew1l)	_decl(cnew1c)
#undef _decl

#define _decl(_t,_ft) \
 F77_SUBROUTINE(_TM_fname(cnewv,_t))( INTEGER(pid), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), _ft##_ARRAY(val), INTEGER(status) TRAIL(name) ); \
 F77_SUBROUTINE(_TM_fname(cnewv0,_t))( INTEGER(pid), CHARACTER(name), _ft(val), INTEGER(status) TRAIL(name) ); \
 F77_SUBROUTINE(_TM_fname(cnewv1,_t))( INTEGER(pid), CHARACTER(name), INTEGER(nval), _ft##_ARRAY(val), INTEGER(status) TRAIL(name) );

 _decl(b,BYTE)	_decl(w,WORD)	_decl(i,INTEGER)
 _decl(r,REAL)	_decl(d,DOUBLE)	_decl(l,LOGICAL)
#undef _decl

F77_SUBROUTINE(adifn(cnewvc))( INTEGER(pid), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), CHARACTER_ARRAY(str), INTEGER(status) TRAIL(name) TRAIL(str) );
F77_SUBROUTINE(adifn(cnewv0c))( INTEGER(pid), CHARACTER(name), CHARACTER(str), INTEGER(status) TRAIL(name) TRAIL(str) );
F77_SUBROUTINE(adifn(cnewv1c))( INTEGER(pid), CHARACTER(name), INTEGER(nval), CHARACTER_ARRAY(str), INTEGER(status) TRAIL(name) TRAIL(str) );

/*
 * Data access
 */

#define _decl(_t,_ft) \
 F77_SUBROUTINE(_TM_fname(get,_t))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(dims), _ft##_ARRAY(val), INTEGER(status) ); \
 F77_SUBROUTINE(_TM_fname(get0,_t))( INTEGER(id), _ft(val), INTEGER(status) ); \
 F77_SUBROUTINE(_TM_fname(get1,_t))( INTEGER(id), INTEGER(mxval), _ft##_ARRAY(val), INTEGER(nval), INTEGER(status) ); \
 F77_SUBROUTINE(_TM_fname(put,_t))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(dims), _ft##_ARRAY(val), INTEGER(status) ); \
 F77_SUBROUTINE(_TM_fname(put0,_t))( INTEGER(id), _ft(val), INTEGER(status) ); \
 F77_SUBROUTINE(_TM_fname(put1,_t))( INTEGER(id), INTEGER(nval), _ft##_ARRAY(val), INTEGER(status) );

 _decl(b,BYTE)	_decl(w,WORD)	_decl(i,INTEGER)
 _decl(r,REAL)	_decl(d,DOUBLE)	_decl(l,LOGICAL)
#undef _decl

F77_SUBROUTINE(adifn(get))( INTEGER(id), CHARACTER(type), INTEGER(ndim), 
			INTEGER_ARRAY(dimx), BYTE(str),
			INTEGER_ARRAY(dims), INTEGER(status) TRAIL(type) );
F77_SUBROUTINE(adifn(getc))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(dimx), CHARACTER_ARRAY(str), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(str) );
F77_SUBROUTINE(adifn(get0c))( INTEGER(id), CHARACTER(str), INTEGER(status) TRAIL(str) );
F77_SUBROUTINE(adifn(get1c))( INTEGER(id), INTEGER(mxval), CHARACTER_ARRAY(str), INTEGER(nactval), INTEGER(status) TRAIL(str) );
F77_SUBROUTINE(adifn(putc))( INTEGER(id), INTEGER(ndim), INTEGER_ARRAY(dims), CHARACTER_ARRAY(str), INTEGER(status) TRAIL(str) );
F77_SUBROUTINE(adifn(put0c))( INTEGER(id), CHARACTER(str), INTEGER(status) TRAIL(str) );
F77_SUBROUTINE(adifn(put1c))( INTEGER(id), INTEGER(nval), CHARACTER_ARRAY(str), INTEGER(status) TRAIL(str) );

/*
 * Component data access
 */

F77_SUBROUTINE(adifn(cget))( INTEGER(id), CHARACTER(name), CHARACTER(type), 
			INTEGER(ndim), INTEGER_ARRAY(dimx), BYTE_ARRAY(val), 
			INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name)
			TRAIL(type) );
F77_SUBROUTINE(adifn(cgetb))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dimx), BYTE_ARRAY(val), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cgetw))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dimx), WORD_ARRAY(val), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cgeti))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dimx), INTEGER_ARRAY(val), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cgetr))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dimx), REAL_ARRAY(val), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cgetd))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dimx), DOUBLE_ARRAY(val), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cgetl))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dimx), LOGICAL_ARRAY(val), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cgetc))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dimx), CHARACTER_ARRAY(str), INTEGER_ARRAY(dims), INTEGER(status) TRAIL(name) TRAIL(str) );

/* Scalar class data get
 */
F77_SUBROUTINE(adifn(cget0b))( INTEGER(id), CHARACTER(name), BYTE(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget0w))( INTEGER(id), CHARACTER(name), WORD(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget0i))( INTEGER(id), CHARACTER(name), INTEGER(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget0r))( INTEGER(id), CHARACTER(name), REAL(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget0d))( INTEGER(id), CHARACTER(name), DOUBLE(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget0l))( INTEGER(id), CHARACTER(name), LOGICAL(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget0c))( INTEGER(id), CHARACTER(name), CHARACTER(str), INTEGER(status) TRAIL(name) TRAIL(str) );

/* Vector class data get
 */
F77_SUBROUTINE(adifn(cget1b))( INTEGER(id), CHARACTER(name), INTEGER(mxval), BYTE_ARRAY(val), INTEGER(nval), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget1w))( INTEGER(id), CHARACTER(name), INTEGER(mxval), WORD_ARRAY(val), INTEGER(nval), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget1i))( INTEGER(id), CHARACTER(name), INTEGER(mxval), INTEGER_ARRAY(val), INTEGER(nval), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget1r))( INTEGER(id), CHARACTER(name), INTEGER(mxval), REAL_ARRAY(val), INTEGER(nval), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget1d))( INTEGER(id), CHARACTER(name), INTEGER(mxval), DOUBLE_ARRAY(val), INTEGER(nval), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget1l))( INTEGER(id), CHARACTER(name), INTEGER(mxval), LOGICAL_ARRAY(val), INTEGER(nval), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cget1c))( INTEGER(id), CHARACTER(name), INTEGER(mxval), CHARACTER_ARRAY(str), INTEGER(nactval), INTEGER(status) TRAIL(name) TRAIL(str) );

/* n-D class data put
 */
F77_SUBROUTINE(adifn(cputb))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), BYTE_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cputw))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), WORD_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cputi))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), INTEGER_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cputr))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), REAL_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cputd))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), DOUBLE_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cputl))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), LOGICAL_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cputc))( INTEGER(id), CHARACTER(name), INTEGER(ndim), INTEGER_ARRAY(dims), CHARACTER_ARRAY(str), INTEGER(status) TRAIL(name) TRAIL(str) );

/* Scalar class data put
 */
F77_SUBROUTINE(adifn(cput0b))( INTEGER(id), CHARACTER(name), BYTE(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput0w))( INTEGER(id), CHARACTER(name), WORD(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput0i))( INTEGER(id), CHARACTER(name), INTEGER(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput0r))( INTEGER(id), CHARACTER(name), REAL(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput0d))( INTEGER(id), CHARACTER(name), DOUBLE(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput0l))( INTEGER(id), CHARACTER(name), LOGICAL(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput0c))( INTEGER(id), CHARACTER(name), CHARACTER(str), INTEGER(status) TRAIL(name) TRAIL(str) );

/* Vector class data put
 */
F77_SUBROUTINE(adifn(cput1b))( INTEGER(id), CHARACTER(name), INTEGER(nval), BYTE_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput1w))( INTEGER(id), CHARACTER(name), INTEGER(nval), WORD_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput1i))( INTEGER(id), CHARACTER(name), INTEGER(nval), INTEGER_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput1r))( INTEGER(id), CHARACTER(name), INTEGER(nval), REAL_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput1d))( INTEGER(id), CHARACTER(name), INTEGER(nval), DOUBLE_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput1l))( INTEGER(id), CHARACTER(name), INTEGER(nval), LOGICAL_ARRAY(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(cput1c))( INTEGER(id), CHARACTER(name), INTEGER(nval), CHARACTER_ARRAY(str), INTEGER(status) TRAIL(name) TRAIL(str) );

F77_SUBROUTINE(adifn(cputid))( INTEGER(id), CHARACTER(name), INTEGER(val), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(find))( INTEGER(id), CHARACTER(name), INTEGER(cid), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(there))( INTEGER(id), CHARACTER(name), LOGICAL(there), INTEGER(status) TRAIL(name) );

/*
 * Reference count access & manipulation
 */
F77_SUBROUTINE(adifn(clone))( INTEGER(id), INTEGER(cid), INTEGER(status) );
F77_SUBROUTINE(adifn(refcnt))( INTEGER(id), INTEGER(cnt), INTEGER(status) );
F77_SUBROUTINE(adifn(refadj))( INTEGER(id), INTEGER(incr), INTEGER(status) );

/*
 * Property manipulation
 */
F77_SUBROUTINE(adifn(delprp))( INTEGER(id), CHARACTER(pname), INTEGER(status)
		TRAIL(pname) );
F77_SUBROUTINE(adifn(locprp))( INTEGER(id), CHARACTER(pname), INTEGER(pid),
		INTEGER(status) TRAIL(pname) );
F77_SUBROUTINE(adifn(nprp))( INTEGER(id), INTEGER(nprp), INTEGER(status) );
F77_SUBROUTINE(adifn(indprp))( INTEGER(id), INTEGER(index), INTEGER(pid),
		INTEGER(status) );

/*
 * Structure manipulation
 */
F77_SUBROUTINE(adifn(delcmp))( INTEGER(id), CHARACTER(cname), INTEGER(status)
		TRAIL(cname) );
F77_SUBROUTINE(adifn(loccmp))( INTEGER(id), CHARACTER(cname), INTEGER(cid),
		INTEGER(status) TRAIL(cname) );
F77_SUBROUTINE(adifn(ncmp))( INTEGER(id), INTEGER(ncmp), INTEGER(status) );
F77_SUBROUTINE(adifn(indcmp))( INTEGER(id), INTEGER(index), INTEGER(cid),
		INTEGER(status) );

/*
 * Context manipulation
 */
F77_SUBROUTINE(adifn(mark))( void );
F77_SUBROUTINE(adifn(rlse))( void );

/*
 * System method interfaces
 */
F77_SUBROUTINE(adifn(copy))( INTEGER(id), INTEGER(cid), INTEGER(status) );
F77_SUBROUTINE(adifn(print))( INTEGER(id), INTEGER(status) );
F77_SUBROUTINE(adifn(setlnk))( INTEGER(id), INTEGER(lid), INTEGER(status) );
F77_SUBROUTINE(adifn(unlnk))( INTEGER(id), INTEGER(status) );

/*
 * Object enquiry
 */
F77_SUBROUTINE(adifn(class))( INTEGER(id), CHARACTER(cls), INTEGER(status) TRAIL(cls) );
F77_SUBROUTINE(adifn(name))( INTEGER(id), CHARACTER(buf), INTEGER(status) TRAIL(buf) );
F77_SUBROUTINE(adifn(shape))( INTEGER(id), INTEGER(mxdnim), INTEGER_ARRAY(dims), INTEGER(ndim), INTEGER(status) );

/*
 * Object destruction
 */
F77_SUBROUTINE(adifn(cerase))( INTEGER(id), CHARACTER(name), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(erase))( INTEGER(id), INTEGER(status) );

/* Other */
F77_SUBROUTINE(adifn(calnxt))( INTEGER(status) );
F77_SUBROUTINE(adifn(defgen))( CHARACTER(spec), CHARACTER(options),
			    ADICB rtn, INTEGER(id),
			    INTEGER(status) TRAIL(spec) TRAIL(options) );
F77_SUBROUTINE(adifn(defmth))( CHARACTER(spec), ADIfMethodCB rtn,
			    INTEGER(id), INTEGER(status) TRAIL(spec) );
F77_SUBROUTINE(adifn(defrcb))( INTEGER(rid), CHARACTER(name),
			    ADICB rtn, INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(defrep))( CHARACTER(name), INTEGER(id), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(exec))( CHARACTER(func), INTEGER(narg),
	INTEGER_ARRAY(args), INTEGER(res), INTEGER(status) TRAIL(func) );
F77_SUBROUTINE(adifn(execi))( INTEGER(func), INTEGER(narg),
	INTEGER_ARRAY(args), INTEGER(res), INTEGER(status) );
F77_SUBROUTINE(adifn(getfile))( INTEGER(id), INTEGER(fid), INTEGER(status) );
F77_SUBROUTINE(adifn(getlink))( INTEGER(id), INTEGER(lid), INTEGER(status) );
F77_SUBROUTINE(adifn(getpath))( INTEGER(id), CHARACTER(path), INTEGER(lpath), INTEGER(status) TRAIL(path) );
F77_SUBROUTINE(adifn(locrcb))( INTEGER(rid), CHARACTER(name),
			    INTEGER(rtn), INTEGER(status) TRAIL(name) );
F77_SUBROUTINE(adifn(locrep))( CHARACTER(name),
			    INTEGER(rep), INTEGER(status) TRAIL(name) );

#ifdef __cplusplus
}
#endif

#endif
