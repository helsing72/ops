#include "OPSTypeDefs.h"
#include "KeyFilterQoSPolicy.h"
#include "OPSObject.h"
namespace ops
{
	KeyFilterQoSPolicy::KeyFilterQoSPolicy()
	{
	}
	KeyFilterQoSPolicy::KeyFilterQoSPolicy(std::string keyString)
	{
		keyStrings.push_back(keyString);
	}
	KeyFilterQoSPolicy::KeyFilterQoSPolicy(std::vector<std::string> keyStrings)
	{
		this->keyStrings = keyStrings;
	}
	void KeyFilterQoSPolicy::setKeys(std::vector<std::string> keyStrings)
	{
		SafeLock lock(this);

		this->keyStrings = keyStrings;
	}
	void KeyFilterQoSPolicy::setKey(std::string key)
	{
		SafeLock lock(this);

		this->keyStrings.clear();
		this->keyStrings.push_back(key);
	}
	std::vector<std::string> KeyFilterQoSPolicy::getKeys()
	{
		SafeLock lock(this);

		return keyStrings;
	}
    
	KeyFilterQoSPolicy::~KeyFilterQoSPolicy()
    {
    }
    
    bool KeyFilterQoSPolicy::applyFilter(OPSObject* o)
	{
		SafeLock lock(this);

		// An empty key filter is the same as no filter
		if (keyStrings.size() == 0) return true;

		for (unsigned int i = 0; i < keyStrings.size(); i++)
		{
			if(o->getKey() == keyStrings[i])
			{
				return true;	// match, so keep this object
			}
		}
		return false;	// no match, skip this object
    }
}
