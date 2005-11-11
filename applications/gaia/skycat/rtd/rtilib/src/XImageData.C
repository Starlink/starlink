/*
 * E.S.O. - VLT project 
 *
 * "@(#) $Id: XImageData.C,v 1.4 2005/02/02 01:43:02 brighton Exp $" 
 *
 * XImageData.C - member functions for class XImageData
 *
 * See the man page ImageData(3) for a complete description of this class
 * library.
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 * pbiereic        17/02/03  Added 'using namespace std'.
 */
static const char* const rcsId="@(#) $Id: XImageData.C,v 1.4 2005/02/02 01:43:02 brighton Exp $";


using namespace std;
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cstring>
#include <cassert>
#include <cmath>
#include "XImageData.h"
#include "define.h"


/*
 * Include some standard methods as (cpp macro) templates:
 * These are methods that are the same for all derived classes of ImageData,
 * except that they work on a different raw data type
 */
#define CLASS_NAME XImageData
#define DATA_TYPE byte
#define NTOH(x) (x)
#include "ImageTemplates.C"
#undef CLASS_NAME
#undef DATA_TYPE
#undef NTOH


