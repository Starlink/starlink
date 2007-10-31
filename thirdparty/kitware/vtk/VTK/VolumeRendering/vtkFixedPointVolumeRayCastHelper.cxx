/*=========================================================================

  Program:   Visualization Toolkit
  Module:    $RCSfile: vtkFixedPointVolumeRayCastHelper.cxx,v $

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "vtkFixedPointVolumeRayCastHelper.h"
#include "vtkObjectFactory.h"

#include <math.h>

vtkCxxRevisionMacro(vtkFixedPointVolumeRayCastHelper, "$Revision: 1.2 $");
vtkStandardNewMacro(vtkFixedPointVolumeRayCastHelper);

vtkFixedPointVolumeRayCastHelper::vtkFixedPointVolumeRayCastHelper()
{
}

vtkFixedPointVolumeRayCastHelper::~vtkFixedPointVolumeRayCastHelper()
{
}

void vtkFixedPointVolumeRayCastHelper::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}


