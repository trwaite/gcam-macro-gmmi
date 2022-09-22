/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


#ifndef _EXOGENOUS_SHUTDOWN_DECIDER_H_
#define _EXOGENOUS_SHUTDOWN_DECIDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file exogenous_shutdown_decider.h
 * \ingroup Objects
 * \brief The ExogenousShutdownDecider header file.
 * \author Matthew Binsted
 */
#include "technologies/include/ishutdown_decider.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

#include <string>
#include <map>

class Tabs;

/*! 
 * \ingroup Objects
 * \brief This object scales production for a vintage using a single
 *        scalar value for each year after the initial operating period.
 
 // Scale technology vintage production using an exogenously specified scaling factor.
 // Scaling can increase or decrease production relative to initial output level.
 
 * \details This object uses an exogenously specified scaling factor to adjust the output 
 *          of a vintaged technology for each year after the initial operating period.
 *          The class simply applies the exogenously specified scaling factor to output 
 *          of the vintaged technology in periods after the initial operating period.
 *          The scaling can increase or decrease production relative to initial output level.
 *         
 *
 *          <b>XML specification for ExogenousShutdownDecider</b>
 *          - XML name: \c exogneous-shutdown-decider
 *          - Contained by: Technology
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c output-scalar ExogenousShutdownDecider::mOutputScalar
 *     
 * \author Matthew Binsted
 */
class ExogenousShutdownDecider: public IShutdownDecider
{
public:
    ExogenousShutdownDecider();
    
    virtual ~ExogenousShutdownDecider();
    
    // IParsedComponent methods.
    virtual ExogenousShutdownDecider* clone() const;
    
    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;

    virtual bool isSameType( const std::string& aType ) const;

    virtual const std::string& getName() const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    // IShutdownDecider methods.
    virtual double calcShutdownCoef( const double aCalculatedProfits,
                                     const std::string& aRegionName,
                                     const std::string& aSectorName,
                                     const int aInstallationYear,
                                     const int aPeriod ) const;
protected:

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IShutdownDecider,

        //! The name of this shutdown decider in case we want to stack multiple.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! The output scalar.  This needs to be specified by period.
        DEFINE_VARIABLE( ARRAY, "output-scalar", mOutputScalar, objects::TechVintageVector<Value> )
    )

    void copy( const ExogenousShutdownDecider& aOther );
};

#endif // _EXOGENOUS_SHUTDOWN_DECIDER_H_
