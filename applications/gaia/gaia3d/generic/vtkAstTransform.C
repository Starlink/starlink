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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "vtkAstTransform.h"
#include "vtk/vtkObjectFactory.h"
#include <cstdlib>
extern "C" {
#include <ast.h>
}

//  Standard VTK object macros (IsA etc.).

vtkCxxRevisionMacro( vtkAstTransform, "$Id" );
vtkStandardNewMacro( vtkAstTransform );

//----------------------------------------------------------------------------
vtkAstTransform::vtkAstTransform()
{
    Mapping = (AstMapping *) NULL;
}

vtkAstTransform::~vtkAstTransform()
{
    //  Release the AST mapping.
    if ( Mapping != NULL ) {
        astAnnul( Mapping );
        Mapping = (AstMapping *) NULL;
    }
}

void vtkAstTransform::PrintSelf( ostream& os, vtkIndent indent )
{
    if ( Mapping != NULL ) {
        os << "Mapping defined:" << astGetC( Mapping, "Class" ) << endl;
    }
    else {
        os << "No mapping defined" << endl;
    }
    vtkAbstractTransform::PrintSelf( os,indent );
}

void vtkAstTransform::InternalDeepCopy( vtkAbstractTransform *transform )
{
    vtkAstTransform *astTransform = (vtkAstTransform *) transform;

    //  Copy these even though they aren't used.
    this->SetInverseTolerance( astTransform->InverseTolerance );
    this->SetInverseIterations( astTransform->InverseIterations );

    //  Copy the inverse flag, which is used
    if ( this->InverseFlag != astTransform->InverseFlag ) {
        this->InverseFlag = astTransform->InverseFlag;
        this->Modified();
    }

    //  Deep so full copy.
    if ( Mapping != NULL ) {
        this->SetMapping( (AstMapping *) astCopy( Mapping ) );
    }
}

vtkAbstractTransform *vtkAstTransform::MakeTransform()
{
    return vtkAstTransform::New();
}

//  Transform a single point.
template<class T>
void vtkTransform1( AstMapping *mapping, const T inPoint[3],
                    int forwards, T outPoint[3] )
{
    double in[3];
    double out[3];

    in[0] = (double) inPoint[0];
    in[1] = (double) inPoint[1];
    in[2] = (double) inPoint[2];

    //  Fudge. Not sure why.
    //in[0]+=1.0;
    //in[1]+=1.0;
    //in[2]+=1.0;

    //cout << in[0]  << ", " << in[1]  << ", " << in[2];
    //cout << " --> ";
    astTranN( mapping, 1, 3, 1, in, forwards, 3, 1, out );
    //cout << out[0] << ", " << out[1] << ", " << out[2] << endl;

    outPoint[0] = out[0];
    outPoint[1] = out[1];
    outPoint[2] = out[2];
}

template<class T>
void vtkForwardTransform( AstMapping *mapping, const T inPoint[3],
                          T outPoint[3], T derivative[3][3] )
{
    vtkTransform1( mapping, inPoint, 1, outPoint );

    if ( derivative ) {
        vtkTransform1( mapping, &derivative[0][0], 1, &derivative[0][0] );
        vtkTransform1( mapping, &derivative[1][0], 1, &derivative[1][0] );
        vtkTransform1( mapping, &derivative[2][0], 1, &derivative[2][0] );
    }
}

template<class T>
void vtkForwardTransform( AstMapping *mapping, const T inPoint[3],
                          T outPoint[3] )
{
    vtkTransform1( mapping, inPoint, 1, outPoint );
}

template<class T>
void vtkBackwardTransform( AstMapping *mapping, const T inPoint[3],
                           T outPoint[3] )
{
    vtkTransform1( mapping, inPoint, 0, outPoint );
}

void vtkAstTransform::ForwardTransformPoint( const float inPoint[3],
                                             float outPoint[3] )
{
    vtkForwardTransform( Mapping, inPoint, outPoint );
}

void vtkAstTransform::ForwardTransformPoint( const double inPoint[3],
                                             double outPoint[3] )
{
    vtkForwardTransform( Mapping, inPoint, outPoint );
}

void vtkAstTransform::ForwardTransformDerivative( const float inPoint[3],
                                                  float outPoint[3],
                                                  float derivative[3][3] )
{
    vtkForwardTransform( Mapping, inPoint, outPoint, derivative );
}

void vtkAstTransform::ForwardTransformDerivative
    ( const double inPoint[3], double outPoint[3], double derivative[3][3] )
{
    vtkForwardTransform( Mapping, inPoint, outPoint, derivative );
}

void vtkAstTransform::InverseTransformPoint( const float inPoint[3],
                                             float outPoint[3] )
{
    vtkBackwardTransform( Mapping, inPoint, outPoint );
}

void vtkAstTransform::InverseTransformPoint( const double inPoint[3],
                                             double outPoint[3] )
{
    vtkBackwardTransform( Mapping, inPoint, outPoint );
}

void vtkAstTransform::InverseTransformDerivative( const float inPoint[3],
                                                  float outPoint[3],
                                                  float derivative[3][3] )
{
    float tmp[3];
    vtkBackwardTransform( Mapping, inPoint, outPoint );
    vtkForwardTransform( Mapping, outPoint, tmp, derivative );
}

void vtkAstTransform::InverseTransformDerivative( const double inPoint[3],
                                                  double outPoint[3],
                                                  double derivative[3][3] )
{
    double tmp[3];
    vtkBackwardTransform( Mapping, inPoint, outPoint );
    vtkForwardTransform( Mapping, outPoint, tmp, derivative );
}
