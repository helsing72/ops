
#include "gtest/gtest.h"

int main(int argc, char** argv)
{
	// required init of gtest library
	::testing::InitGoogleTest(&argc, argv);
	int result = RUN_ALL_TESTS();
	std::cout << "RUN_ALL_TESTS returned " << result << std::endl;
	return result;
}
