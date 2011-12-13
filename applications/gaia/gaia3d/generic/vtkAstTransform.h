/*+
 *   Name:
 *      vtkAstTransform

 *   Purpose:
 *      A type of vtkTransform based on an AST mapping.

 *   Language:
 *      C++

 *  Copyright:
 *     Copyright (C) 2007 Science and Technology Facilities Council
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

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      25-SEP-2007 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#ifndef __vtkAstTransform_h
#define __vtkAstTransform_h

#include "vtk/vtkWarpTransform.h"
extern "C" {
#include <ast.h>
}

class VTK_COMMON_EXPORT vtkAstTransform : public vtkWarpTransform
{
 public:
    static vtkAstTransform *New();
    vtkTypeRevisionMacro( vtkAstTransform,vtkWarpTransform );
    virtual void PrintSelf( ostream& os, vtkIndent indent );

    // Description:
    // Make another transform of the same type.
    vtkAbstractTransform *MakeTransform();

    // Set the AST mapping.
    vtkSetMacro( Mapping, AstMapping * );
    vtkGetMacro( Mapping, AstMapping * );

 protected:
    vtkAstTransform();
    ~vtkAstTransform();

    // Description:
    // Copy this transform from another of the same type.
    void InternalDeepCopy( vtkAbstractTransform *transform );

    // Description:
    // Internal functions for calculating the transformation.
    void ForwardTransformPoint( const float in[3], float out[3] );
    void ForwardTransformPoint( const double in[3], double out[3] );

    void ForwardTransformDerivative( const float in[3], float out[3],
                                     float derivative[3][3] );
    void ForwardTransformDerivative( const double in[3], double out[3],
                                     double derivative[3][3] );

    void InverseTransformPoint( const float in[3], float out[3] );
    void InverseTransformPoint( const double in[3], double out[3] );

    void InverseTransformDerivative( const float in[3], float out[3],
                                     float derivative[3][3] );
    void InverseTransformDerivative( const double in[3], double out[3],
                                     double derivative[3][3] );

 private:
    vtkAstTransform( const vtkAstTransform& ); // Not implemented.
    void operator=( const vtkAstTransform& ); // Not implemented.

    AstMapping *Mapping;
};

#endif

