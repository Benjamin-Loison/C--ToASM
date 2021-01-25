int main() {
  /* put long to understand it well */
  int x;	 
  int y;
  x = 12;
  y = 10;
  //printf("%d = 12? ",(1?x:y));
  printf("%d = 10? ",(0?x:y));
  printf("%d = 12?, if 10 not false but not the gcc one",(18?x:y));

  return 0;
}
