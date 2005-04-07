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
 * Peter W. Draper 30/05/01  Added double support.
 *                 07/04/04  Added __x86_64
 */

#if defined(i386) || defined(__i386__) || defined(__x86_64) || \
    defined(__alpha) || \
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

#ifndef _DoubleImageData_h_
#include "DoubleImageData.h"
#endif
#define DoubleImageData NativeDoubleImageData
#undef _DoubleImageData_h_
#include "DoubleImageData.h"
#undef DoubleImageData

#else  /* ! defined(i386) ... */

// no byte swapping needed, so just use existing classes
#define NativeShortImageData ShortImageData
#define NativeUShortImageData UShortImageData
#define NativeLongImageData LongImageData
#define NativeFloatImageData FloatImageData
#define NativeDoubleImageData DoubleImageData

#endif /* defined(i386) ... */


#endif _NativeImageData_h_
