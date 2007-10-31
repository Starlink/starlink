/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cm_xmlrpc.h,v $
  Language:  C++
  Date:      $Date: 2006/10/27 20:03:32 $
  Version:   $Revision: 1.1.2.1 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __cm_xmlrpc_h
#define __cm_xmlrpc_h

/* Use the xmlrpc library configured for CMake.  */
#include "cmThirdParty.h"
#ifdef CMAKE_USE_SYSTEM_XMLRPC
# include <xmlrpc.h>
# include <xmlrpc_client.h>
#else
# include <cmxmlrpc/xmlrpc.h>
# include <cmxmlrpc/xmlrpc_client.h>
#endif

#endif
