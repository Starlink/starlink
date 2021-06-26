/*+
 *  Name:
 *     VOTableWriteFunctions

 *  Purpose:
 *     Utility functions for converting a VOTable into a tab table.

 *  Description:
 *     These functions make use of the codesynthesis classes generated using
 *     the VOTable 1.1 schema to read a VOTable and write it out as a
 *     tab table.
 *
 *     There are two fundamental versions of theses schema derived classes
 *     (sadly not derived from each other, so no common interface, which is
 *     why these functions are preprocessed using differing C++ namespaces)
 *     one fully namespace qualified, the official schema, and one where all
 *     the elements are in the default namespace (it happens, in fact may
 *     be standard).
 *
 *     (Note in passing: some of this could be done using templates, but that
 *     looks untidy as you cannot use a namespace as a template, just types,
 *     so each type used would need to be declared, that's NS::VOTABLE,
 *     NS::TABLE, NS::PARAM etc., see emptyTable for a starter if you prefer
 *     that approach).

 *  Language:
 *     C++.

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     05-JUN-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

/*  System includes. */
#include <iostream>
#include <fstream>
#include <sstream>
#include <memory>
#include <string>
#include <iomanip>
#include <fcntl.h>
#include <typeinfo>
#include <algorithm>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <xercesc/dom/DOM.hpp>

/*  Skycat includes. */
#include <HTTP.h>
#include <Fits_IO.h>
#include <Mem.h>
#include <CatalogInfo.h>
#include <LocalCatalog.h>

/*  Local includes. */
#include "VOTable1.1_dns.hxx"
#include "VOTable1.1.hxx"
#include "VOTable1.2.hxx"
#include "VOTable1.3.hxx"
#include "VOTable1.4.hxx"
#include "VOTable.h"
#include "VOTableStream.h"
#include "GaiaUtils.h"
#include "GaiaBase64.h"
#include "GaiaGzip.h"

using namespace std;

/*  Using the xerces namespace */
XERCES_CPP_NAMESPACE_USE

namespace gaia {

    // Actual members are defined elsewhere.
#define NS votable_11_dns
#define NSVERS 11
#include "VOTableWriteFunctions.icc"
#undef NS
#undef NSVERS

#define NS votable_11
#define NSVERS 11
#include "VOTableWriteFunctions.icc"
#undef NS
#undef NSVERS

#define NS votable_12
#define NSVERS 12
#include "VOTableWriteFunctions.icc"
#undef NS
#undef NSVERS

#define NS votable_13
#define NSVERS 13
#include "VOTableWriteFunctions.icc"
#undef NS
#undef NSVERS

#define NS votable_14
#define NSVERS 13
#include "VOTableWriteFunctions.icc"
#undef NS
#undef NSVERS

}
