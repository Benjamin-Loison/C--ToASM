int main()
{
	#ifdef MCC
		int a;
	#else
		long* a;
	#endif
	a = malloc(10);
	return 69;
}
