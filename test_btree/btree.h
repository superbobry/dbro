#include <string>

class btree
{
public:
	typedef int key_type;
	typedef int value_type;

	btree(const char* path);
	void add(key_type key, value_type val);
	void erase_all(key_type key);
	void erase(key_type key_tip, value_type val);
	int	 find(key_type key);
	int* find_range(key_type begin, key_type end);
	void print_path();



private:
	std::string m_Path;

};

extern "C"
{

btree* create_btree();


}