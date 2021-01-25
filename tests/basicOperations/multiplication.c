int main()
{
	#ifdef MCC
		int a;
		int b;
		int c;
	#else
		long a, b, c;
	#endif
	a = 7;
	b = 2;
	c = a * b;
	return c;
}
