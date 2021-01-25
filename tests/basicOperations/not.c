int main()
{
	#ifdef MCC
		int a;
	#else
		long a;
	#endif
	a = 7;
	a = ~a;
	return a;
}
