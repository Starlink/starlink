/*=========================================================================

  Program:   CMake - Cross-Platform Makefile Generator
  Module:    $RCSfile: cmCursesFilePathWidget.cxx,v $
  Language:  C++
  Date:      $Date: 2006/03/16 15:44:55 $
  Version:   $Revision: 1.5 $

  Copyright (c) 2002 Kitware, Inc., Insight Consortium.  All rights reserved.
  See Copyright.txt or http://www.cmake.org/HTML/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "cmCursesFilePathWidget.h"

cmCursesFilePathWidget::cmCursesFilePathWidget(int width, int height, 
                                           int left, int top) :
  cmCursesPathWidget(width, height, left, top)
{
  this->Type = cmCacheManager::FILEPATH;
}

