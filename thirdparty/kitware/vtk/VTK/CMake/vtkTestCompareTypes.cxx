/*=========================================================================

  Program:   Visualization Toolkit
  Module:    $RCSfile: vtkTestCompareTypes.cxx,v $

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#define TYPE_LONG_LONG long long

typedef VTK_TEST_COMPARE_TYPE_1 Type1;
typedef VTK_TEST_COMPARE_TYPE_2 Type2;

void function(Type1**) {}

int main()
{
  Type2** p = 0;
  function(p);
  return 0;
}
