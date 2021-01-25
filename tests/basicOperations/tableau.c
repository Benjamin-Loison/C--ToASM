int main()
{
	#ifdef MCC
		int a;
	#else
		long* a;
	#endif
	a = malloc(10);
	a[2] = 42;
	return a[2];
}
