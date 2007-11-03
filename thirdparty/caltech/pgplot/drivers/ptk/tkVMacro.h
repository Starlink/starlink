#ifdef _LANG
#include "Lang.m"
#endif

#if defined(_TK)
#include "tk.m"
#endif

#if defined(_TK) || defined(_XLIB_H_)
# if !defined(_XLIB) && !defined(_XLIB_H_)
#  include <X11/Xlib.h>
# endif
# ifdef _TKINTXLIBDECLS
#  include "tkIntXlibDecls.m"
# else
#  if defined(_XLIB_H) && !defined(_XLIB)
#   define _XLIB
#  endif
#  include "Xlib.m"
# endif
#endif

#ifdef _TKINT
#include "tkInt.m"
#endif
#ifdef _TKIMGPHOTO
#include "tkImgPhoto.m"
#endif
#ifdef _TIX
#include "tix.m"
#endif
#ifdef _TIXINT
#include "tixInt.m"
#endif
#ifdef _TKOPTION
#include "tkOption.m"
#endif
#ifdef _TIXIMGXPM
#include "tixImgXpm.m"
#endif
#ifdef _IMGINT
#include "imgInt.m"
#endif

#ifdef _TCLDECLS
#include "tclDecls.m"
#endif

#ifdef _TKDECLS
#include "tkDecls.m"
#endif

#ifdef _TKPLATDECLS
#include "tkPlatDecls.m"
#endif

#ifdef _TKINTDECLS
#include "tkIntDecls.m"
#endif

#ifdef _TKINTPLATDECLS
#include "tkIntPlatDecls.m"
#endif

#ifdef _TKEVENT
#include "tkEvent.m"
#endif


