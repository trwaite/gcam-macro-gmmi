#ifndef _IINFO_H_
#define _IINFO_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file iinfo.h
* \ingroup objects
* \brief The IInfo interface header file.
* \author Josh Lurz
*/
#include <string>
#include <iosfwd>

class Tabs;

/*!
* \ingroup Objects
* \brief This interface represents a set of properties which can be accessed by
*        their unique identifier.
* \details The IInfo interface represents a set of searchable properties
*          accessed by their string key. The properties may be booleans,
*          integers, double or strings. Operations exist to set or update values
*          for a key, query if a key exists, and get the value for a key.
* \todo Evaluate whether functions to add to a double value, and update an
*       average would be useful as additions to the interface.
* \todo Add longevity to properties.
* \author Josh Lurz
*/

class IInfo
{
public:
	virtual inline ~IInfo();

	/*! \brief Set a boolean value for a given key.
	* \details Updates the value associated with the key if it is already
    *          present, and creates a new one key-value pair if it does not
    *          exist.
	* \param aStringKey The key for which to set or update the value.
    * \param aValue The new value.
    */
	virtual bool setBoolean( const std::string& aStringKey,
		                     const bool aValue ) = 0;

	/*! \brief Set an integer value for a given key.
	* \details Updates the value associated with the key if it is already
    *          present, and creates a new key-value pair if it does not exist.
	* \param aStringKey The key for which to set or update the value.
    * \param aValue The new value.
    */
	virtual bool setInteger( const std::string& aStringKey,
		                     const int aValue ) = 0;
	
	/*! \brief Set a dobule value for a given key.
	* \details Updates the value associated with the key if it is already
    *          present, and creates a new key-value pair if it does not exist.
	* \param aStringKey The key for which to set or update the value.
    * \param aValue The new value.
    */
	virtual bool setDouble( const std::string& aStringKey,
		                    const double aValue ) = 0;
	
	/*! \brief Set a string value for a given key.
	* \details Updates the value associated with the key if it is already
    *          present, and creates a new key-value pair if it does not exist.
	* \param aStringKey The key for which to set or update the value.
    * \param aValue The new value.
    */
	virtual bool setString( const std::string& aStringKey,
		                    const std::string& aValue ) = 0;

	/*! \brief Get a boolean from the IInfo with a specified key.
	* \details Searches the key set for the given key and returns the associated
    *          value. If the value does not exist the default value will be
    *          returned.
	* \param aStringKey The key for which to search the IInfo object.
	* \param aMustExist Whether the value should exist in the IInfo.
	* \return The boolean associated with the key or false if it does not exist.
    */
	virtual const bool getBoolean( const std::string& aStringKey,
								   const bool aMustExist ) const = 0;
	
	/*! \brief Get a string from the IInfo with a specified key.
	* \details Searches the key set for the given key and returns the associated
    *          value. If the value does not exist the default value will be
    *          returned.
	* \param aStringKey The key for which to search the IInfo object.
	* \param aMustExist Whether the value should exist in the IInfo.
	* \return The integer associated with the key or zero if it does not exist.
    */
	virtual const int getInteger( const std::string& aStringKey,
		                          const bool aMustExist ) const = 0;
	
	/*! \brief Get a string from the IInfo with a specified key.
	* \details Searches the key set for the given key and returns the associated
    *          value. If the value does not exist the default value will be
    *          returned.
	* \param aStringKey The key for which to search the IInfo object.
	* \param aMustExist Whether the value should exist in the IInfo.
	* \return The double associated with the key or zero if it does not exist.
    */
	virtual const double getDouble( const std::string& aStringKey,
		                            const bool aMustExist ) const = 0;
	
	/*! \brief Get a string from the IInfo with a specified key.
	* \details Searches the key set for the given key and returns the associated
    *          value. If the value does not exist the default value will be
    *          returned.
	* \param aStringKey The key for which to search the IInfo object.
	* \param aMustExist Whether the value should exist in the IInfo.
	* \return The string(by reference) associated with the key or the empty
    *         string if it does not exist.
    */
	virtual const std::string& getString( const std::string& aStringKey,
		                                  const bool aMustExist ) const = 0;
	
	/*! \brief Return whether a value exists in the IInfo.
	* \details Performs a search of the IInfo object using the same method as
    *          all getter methods of the object. The method performs a full
    *          search and so should be avoided whenever the default value and a
    *          warning from the get method will return enough information.
	* \param aStringKey The key for which to search the IInfo object.
	* \return Whether the key exists in the IInfo.
    */
	virtual bool hasValue( const std::string& aStringKey ) const = 0;

    /*! \brief Write the IInfo object to an output stream as XML.
    * \details Writes the set of keys and values to an output stream as XML.
    * \param aPeriod Model period for which to write debugging information.
    * \param aTabs Tabs manager.
    * \param aOut Output stream.
    */
    virtual void toDebugXML( const int aPeriod,
                             Tabs* aTabs,
                             std::ostream& aOut ) const = 0;
};

//! Empty inline destructor needed so that IInfo objects can be deleted through
//! base class pointers.
IInfo::~IInfo(){
}

#endif // _IINFO_
