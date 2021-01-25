//
// A collection of functions that tests the compiler
// for compilance with C--'s semantics.
// Written by Antoine Delignat-Lavaud, ENS Cachan, in 2008.
//

int i; int k;

int main()
{
 int j; int g; int h; int tot;
 printf("Starting Testsuite for C--, see Goubault's webpage for semantics references.\n");
 printf("============================================================\n");
 printf("          ++ Testing basic compiler features ++\n");
 printf("============================================================\n");
 printf(" [o] Testing conditional blocks ...\t\t");
 if(1) printf("OK.\n"); else exit("ERROR !\n");
 printf(" [o] Testing local and global variables ... \t");
 if(variables()) return printf("ERROR !\n");
 printf("OK.\n [o] Testing compare operators ...\t\t");
 if(test_comp()) exit(printf("ERROR !\n"));
 printf("OK.\n [o] Testing binary operators ... \n");
 test_op();
 printf(" [o] Testing loops ...\t\t\t\t");
 for(j=0;j++<3;);
 if(j==4) printf("OK.\n");
 else return printf("ERROR !\n");
}

int test_comp()
{
 int i;
 char *s;
 i = 2;
 if(i==2) 0;
 else return 1;
 if(i<3) 0;
 else return 2;
 if(i>1) 0;
 else return 3;
 if(i != 0) 0;
 else return 4;
 s = (i==2 ? 0 : 1 );
 return s;
}

int test_op()
{
 int i; int t;
 i = 0; t = 0;
 if(i++ == 0 && i == 1){ t=t+1; printf("  [+] Post increment OK\n");}
 if(i-- == 1 && i == 0){ t=t+1; printf("  [+] Post decrement OK\n");}
 if(++i == 1 && i == 1){ t=t+1; printf("  [+] Pre increment OK\n");}
 if(--i == 0 && i == 0){ t=t+1; printf("  [+] Pre decrement OK\n");}

 if(t != 4) exit(printf(" [x] Some tests failed ! Check your increment/decrement operators.\n"));
 i = malloc(8); t=0;
 i[0] = 5; i[1] = 9;

 if(i[0]++ == 5 && i[0] == 6){t++; printf("  [+] Post increment array OK\n");}
 if(i[1]-- == 9 && i[1] == 8){t++; printf("  [+] Post decrement array OK\n");}
 if(++i[0] == 7 && i[0] == 7){t++; printf("  [+] Pre increment array OK\n");}
 if(--i[1] == 7 && i[1] == 7){t++; printf("  [+] Pre decrement array OK\n");}

 if(t!=4) exit(printf(" [x] Some tests failed ! Check your increment/decrement operators.\n"));
 t=0;

 if(13 % 5 == 3){t++; printf("  [+] Modulo OK\n");}
 if(7+5 == 12){t++; printf("  [+] Sum OK\n");}
 if(-4 * 23 == -92){t++; printf("  [+] Product OK\n");}
 if(7/3 == 2){t++; printf("  [+] Quotient OK\n");}
 if(!0){t++; printf("  [+] Negation OK\n");}
 if(12-5 == 7){t++; printf("  [+] Substract OK\n");}

 if(t != 6) printf(" [x] Some tests failed ! Check your binary operators.\n");
 return 0;
}

int zer()
{ return i=0;}
int inci()
{ return ++i;}

int variables()
{
 int i;
 i = 3;
 zer();
 {
 int i; i=5; 
 if(i!=5) return 1;
 }
 if(i != 3) return 1;
 if(inci() != 1 || inci() != 2) return 1;
 return 0;
}

