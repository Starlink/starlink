// -*-c++-*-
#ifndef _NativeImageData_h_
#define _NativeImageData_h_
/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: NativeImageData.h,v 1.1 1998/03/23 00:46:55 abrighto Exp $" 
 *
 * NativeImageData.h - cpp template definitions to support native byte
 *                     order images on byte swapped machines (i386, vax, etc.)
 *
 * This file uses some #defines to declare native versions of the
 * ImageData subclasses for use on byte swapped machines. On other
 * machines, this has no effect and no new classes are created.
 *
 * The purpose is to allow derived packages to use image types that are
 * either in native byte order already or have been read in and
 * byte-swapped already by a library routine. This can be controlled by a
 * flag set in a class derived from the ImageIO class (astrotcl/imageio).
 *
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  20/03/98  Created
 */

#if defined(i386) || defined(__i386__) || defined(__alpha) || \
     defined(vax) || defined(__vax__) || (defined(mips) && defined(MIPSEL))

// watch out for #include guards...
#ifndef _ShortImageData_h_
#include "ShortImageData.h"
#endif
#define ShortImageData NativeShortImageData
#undef _ShortImageData_h_
#include "ShortImageData.h"
#undef ShortImageData

#ifndef _UShortImageData_h_
#include "UShortImageData.h"
#endif
#define UShortImageData NativeUShortImageData
#undef _UShortImageData_h_
#include "UShortImageData.h"
#undef UShortImageData

#ifndef _LongImageData_h_
#include "LongImageData.h"
#endif
#define LongImageData NativeLongImageData
#undef _LongImageData_h_
#include "LongImageData.h"
#undef LongImageData

#ifndef _FloatImageData_h_
#include "FloatImageData.h"
#endif
#define FloatImageData NativeFloatImageData
#undef _FloatImageData_h_
#include "FloatImageData.h"
#undef FloatImageData

#else  /* ! defined(i386) ... */

// no byte swapping needed, so just use existing classes
#define NativeShortImageData ShortImageData
#define NativeUShortImageData UShortImageData
#define NativeLongImageData LongImageData
#define NativeFloatImageData FloatImageData

#endif /* defined(i386) ... */


#endif _NativeImageData_h_
