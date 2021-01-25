int main() {
  int y;
  y=7;
  if (1) {
    int y;
    printf("on est dans le if");
    y=3;
  }
  else {
    y=4;
  }
  printf("%d",y);
  return 0;
}
