// Some tests for C--
// Written by Laurent Prosperi, ENS Cachan, in 2015.

#ifndef MCC
#include <stdio.h>
#include <stdlib.h>
#endif
#define NULL 0


int global_3;
int global_4;
int* global_5;
int* global_6;

int test_expr(){
  int i,j,k,l;
  int e1,e2,e3,e4,e5,e6,e7,e8,e9;
  int* tab;
  int l1, l2;
  int a, b;

  a=587;
  b=-158;
  
  i=156;
  j=129;
  k=11;
  l=-1;
  e1=-(i*j+k-l+2+10/2);
  e2=-5*(i+j+k-l)*(l*5+3)+(k/11);
  e3=(e1*e2+l-k+8)*7-6;
  e4 = e2-(e2++);
  e5 = e3-(e3--);
  e6 = (++i)-(--i);
  e9 = (i++)-(i--);
  e7 = ~k;
  e8 = e1 % ( l+k );

  tab = malloc( 8*8);
  tab[0]=e1;
  tab[1]=e2;
  tab[4]= (tab[0]+e3-tab[1]*5) + tab[0] % 3;
  tab[6]= 1;
  tab[5]= ~(tab[4]*tab[6]);
  tab[7]= (5*tab[0] + (tab[3] = e3));

  /*l1 = ((0 == 0) < 56 ) <= e8;
  l2 =  4 == (((e2 == e1) < e4 ) <= e8);

  j= ((a+b)<5) ? (l1+l2+e6+e7+e3+e2) : (l1*l2+(e6-1)*e7*e3-e2); ///e6-1 compence la différence entre gcc et mcc
  k= ((a+b)<5) ? (l1+l2-e6*e7+e3+e2) : (l1*l2+(e6-1)*e7*e3-e2-9*e4+e5);*/


  global_1 = j+k*2;
  printf("global_1 = %d\n", global_1);
  printf("e1  %s\n", (-20143==e1) ? "pass" : "failed");  
  printf("e1  th=%d,  value=%d\n", -20143, e1);
  printf("----------------------\n");
  
  printf("e2  %s\n", (2972==e2) ? "pass" : "failed");  
  printf("e2  th=%d,  value=%d\n", 2972, e2);
  printf("----------------------\n");

  printf("e3  %s\n", (-418914006==e3) ? "pass" : "failed");  
  printf("e3  th=%d,  value=%d\n", -418914006, e3);
  printf("----------------------\n");
  
  printf("e4  %s\n", (1==e4) ? "pass" : "failed");  
  printf("e4  th=%d,  value=%d\n", 1, e4);
  printf("----------------------\n");
  
  printf("e5  %s\n", (-1==e5) ? "pass" : "failed");  
  printf("e5  th=%d,  value=%d\n", -1, e5);
  printf("----------------------\n");
  
  printf("e6  %s\n", (1==e6) ? "pass" : "failed");   ///D'après gcc ça fait 0, sémantique c-- => 1
  printf("e6  th=%d,  value=%d\n", 1, e6);
  printf("----------------------\n");

  printf("e7  %s\n", (-12==e7) ? "pass" : "failed");  
  printf("e7  th=%d,  value=%d\n", -12, e7);
  printf("----------------------\n");
  
  printf("e8  %s\n", (-3==e8) ? "pass" : "failed");  
  printf("e8  th=%d,  value=%d\n", -3, e8);
  printf("----------------------\n");
  
  printf("e9  %s\n", (-1==e9) ? "pass" : "failed");  
  printf("e9  th=%d,  value=%d\n", -1, e9);
  printf("----------------------\n");
  
  printf("tab[0]  %s\n", (-20143==tab[0]) ? "pass" : "failed");  
  printf("tab[0]  th=%d,  value=%d\n", -20143, tab[0]);
  printf("----------------------\n");
  
  printf("tab[1]  %s\n", (2972==tab[1]) ? "pass" : "failed");  
  printf("tab[1]  th=%d,  value=%d\n", 2972, tab[1]);
  printf("----------------------\n");
  
  printf("tab[2]  %s\n", (0==tab[2]) ? "pass" : "failed");  
  printf("tab[2]  th=%d,  value=%d\n", 0, tab[2]);
  printf("----------------------\n");
  
  printf("tab[3]  %s\n", (-418914006==tab[3]) ? "pass" : "failed");  
  printf("tab[3]  th=%d,  value=%d\n", -418914006, tab[3]);
  printf("----------------------\n");
  
  printf("tab[4]  %s\n", (-418949010==tab[4]) ? "pass" : "failed");  
  printf("tab[4]  th=%d,  value=%d\n", -418949010, tab[4]);
  printf("----------------------\n");
  
  printf("tab[5]  %s\n", (418949009==tab[5]) ? "pass" : "failed");  
  printf("tab[5]  th=%d,  value=%d\n", 418949009, tab[5]);
  printf("----------------------\n");

  printf("tab[7]  %s\n", (-419014721==tab[7]) ? "pass" : "failed");
  printf("tab[7]  th=%d,  value=%d\n", -419014721, tab[7]);
  printf("----------------------\n");
  
  printf("l1  %s\n", (0==l1) ? "pass" : "failed");  
  printf("l1  th=%d,  value=%d\n", 0, l1);
  printf("----------------------\n");
  
  printf("l2  %s\n", (0==l2) ? "pass" : "failed");  
  printf("l2  th=%d,  value=%d\n", 0, l2);
  printf("----------------------\n");
  
  printf("j  %s\n", (-2972==j) ? "pass" : "failed");  
  printf("j  th=%d,  value=%d\n", -2972, j);
  printf("----------------------\n");
  
  printf("k  %s\n", (-2982==k) ? "pass" : "failed");  
  printf("k  th=%d,  value=%d\n", -2982, k);
  printf("----------------------\n");
  

  free( tab );

  return 56;
}

int test_code(){
  int i,j,k,l;
  int a,b,c,d;
  int x,y,z;

  a=0;
  b=0;
  c=0;
  d=0;
  i=156;
  j=129;
  k=11;
  l=-1;

  printf("global_1 = %d\n", global_1);
  global_2 = i+k-1;
  printf("global_1 = %d\n", global_1);
  global_1--;
  printf("global_1 = %d\n", global_1);

  if( (i+j-3)<10 ){
    x=50;
    while(0<x){
      a=k+a+3*l;
      x--;
    }
    if((i+k+l*6)==(i*j*k*l)){
      b = 42;
    }else{
      b=5;
    }
  }else{
    if((i+k+l*6)==(i*j*k*l)){
      y=50;
      while(0<=y){
	b=k+b+3*l;
	y--;
      }
    }else{
      y=12;
      while(0<=y){
	a=k+b+3*l;
	y--;
      }
    }
  }

  x=50;
  y=0;
  z=0;
  while(0<x){
    while(y<15){
      while( z ==0){
	z=1;
	c=i+d-c;
      }
      d=a+b-d;
      y++;
    }
    x--;
  }

  printf("a  %s\n", (8==a) ? "pass" : "failed");  
  printf("a  th=%d, value=%d\n", 8, a);
  printf("----------------------\n");
  
  printf("b  %s\n", (0==b) ? "pass" : "failed");  
  printf("b  th=%d, value=%d\n", 0, b);
  printf("----------------------\n");
  
  printf("c  %s\n", (156==c) ? "pass" : "failed");  
  printf("c  th=%d, value=%d\n", 156, c);
  printf("----------------------\n");
  
  printf("d  %s\n", (8==d) ? "pass" : "failed");  
  printf("d  th=%d, value=%d\n", 8, d);
  printf("----------------------\n");

  return (a*b-9/3+c-d);
}

int test_args(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j ){
  printf("args_a  %s\n", (5==a) ? "pass" : "failed");  
  printf("args_a  th=%d, value=%d\n", 5, a);
  printf("----------------------\n");
  
  printf("args_b  %s\n", (-8937==b) ? "pass" : "failed");  
  printf("args_b  th=%d, value=%d\n", -8937, b);
  printf("----------------------\n");
  
  printf("args_c  %s\n", (-1483542==c) ? "pass" : "failed");  
  printf("args_c  th=%d, value=%d\n", -1483542, c);
  printf("----------------------\n");
  
  printf("args_d  %s\n", (-8938==d) ? "pass" : "failed");  
  printf("args_d  th=%d, value=%d\n", -8938, d);
  printf("----------------------\n");
  
  printf("args_e  %s\n", (1547==e) ? "pass" : "failed");  
  printf("args_e  th=%d, value=%d\n", 1547, e);
  printf("----------------------\n");
  
  printf("args_f  %s\n", (12==f) ? "pass" : "failed");  
  printf("args_f  th=%d, value=%d\n", 12, f);
  printf("----------------------\n");
  
  printf("args_g  %s\n", (-589==g) ? "pass" : "failed");  
  printf("args_g  th=%d, value=%d\n", -589, g);
  printf("----------------------\n");
  
  printf("args_h  %s\n", (166==h) ? "pass" : "failed");  
  printf("args_h  th=%d, value=%d\n", 166, h);
  printf("----------------------\n");
  
  printf("args_i  %s\n", (-8937==i) ? "pass" : "failed");  
  printf("args_i  th=%d, value=%d\n", -8937, i);
  printf("----------------------\n");
  
  printf("args_j  %s\n", (5==j) ? "pass" : "failed");  
  printf("args_j  th=%d, value=%d\n", 5, j);
  printf("----------------------\n");
  return ( (a+b+c+d+e+f-h*i*j) < 0);
}

int test_args_2(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k){

  /* printf("args_a  %s\n", (5==a) ? "pass" : "failed");   */
  /* printf("args_a  th=%d, value=%d\n", 5, a); */
  /* printf("----------------------\n"); */
  
  printf("args_b  %s\n", (-8937==b) ? "pass" : "failed");  
  printf("args_b  th=%d, value=%d\n", -8937, b);
  printf("----------------------\n");
  
  printf("args_c  %s\n", (-1483542==c) ? "pass" : "failed");  
  printf("args_c  th=%d, value=%d\n", -1483542, c);
  printf("----------------------\n");
  
  printf("args_d  %s\n", (-8938==d) ? "pass" : "failed");  
  printf("args_d  th=%d, value=%d\n", -8938, d);
  printf("----------------------\n");
  
  printf("args_e  %s\n", (1547==e) ? "pass" : "failed");  
  printf("args_e  th=%d, value=%d\n", 1547, e);
  printf("----------------------\n");
  
  printf("args_f  %s\n", (12==f) ? "pass" : "failed");  
  printf("args_f  th=%d, value=%d\n", 12, f);
  printf("----------------------\n");
  
  printf("args_g  %s\n", (-589==g) ? "pass" : "failed");  
  printf("args_g  th=%d, value=%d\n", -589, g);
  printf("----------------------\n");
  
  printf("args_h  %s\n", (166==h) ? "pass" : "failed");  
  printf("args_h  th=%d, value=%d\n", 166, h);
  printf("----------------------\n");
  
  printf("args_i  %s\n", (-8937==i) ? "pass" : "failed");  
  printf("args_i  th=%d, value=%d\n", -8937, i);
  printf("----------------------\n");
  
  printf("args_j  %s\n", (5==j) ? "pass" : "failed");  
  printf("args_j  th=%d, value=%d\n", 5, j);
  printf("----------------------\n");
  
  printf("args_k  %s\n", (-2==k) ? "pass" : "failed");  
  printf("args_k  th=%d, value=%d\n", -2, k);
  printf("----------------------\n");
  return ( (a+b+c+d+e+f-h*i*j) < 0);
}

int* test_tabs(int* tab_1){
  int i;
  int* ret;
  if( tab_1 != NULL ){
    free( tab_1 );
  }
  tab_1 = malloc(8*100);
  global_5 = malloc(8*100);
  global_6 = malloc(8*100);
  
  ///Init
  i=0;
  while(i<100){
    tab_1[i] = 0;
    global_5[i] = i;
    global_6[i] = (global_3++)+(--global_4)+(global_5[i]++);
    i++;
  } 
  
  ///Calcul
  i=1;
  while(i<100){
    tab_1[i] = (global_5[(i-1)] + global_6[i])*2;
    global_5[i]= (global_5[i] * global_6[(i-1)]) % 9;
    i++;
  }
  
  ///Tests
  //i=0; //gérérateur pour gcc
  //while(i<100){
  //printf("printf(\"tab_1[%d]  %%s\\n\",(%d==tab_1[%d]) ? \"pass\" : \"failed\");\n", i, tab_1[i], i);
//printf("printf(\"tab_1[%d]  th=%%d, value=%%d\\n\", %d, tab_1[%d]);\n", i, tab_1[i], i);
//printf("printf(\"----------------------\\n\");\n");
//printf("printf(\"global_5[%d]  %%s\\n\",(%d==global_5[%d]) ? \"pass\" : \"failed\");\n", i, global_5[i], i);
//printf("printf(\"global_5[%d]  th=%%d, value=%%d\\n\", %d, global_5[%d]);\n", i, global_5[i], i);
//printf("printf(\"----------------------\\n\");\n");
//printf("printf(\"global_6[%d]  %%s\\n\",(%d==global_6[%d]) ? \"pass\" : \"failed\");\n", i, global_6[i], i);
//printf("printf(\"global_6[%d]  th=%%d, value=%%d\\n\", %d, global_6[%d]);\n", i, global_6[i], i);
//printf("printf(\"----------------------\\n\");\n");
//i++;
//}

{
printf("tab_1[0]  %s\n",(0==tab_1[0]) ? "pass" : "failed");
printf("tab_1[0]  th=%d, value=%d\n", 0, tab_1[0]);
printf("----------------------\n");
printf("tab_1[1]  %s\n",(2==tab_1[1]) ? "pass" : "failed");
printf("tab_1[1]  th=%d, value=%d\n", 2, tab_1[1]);
printf("----------------------\n");
printf("tab_1[2]  %s\n",(-2==tab_1[2]) ? "pass" : "failed");
printf("tab_1[2]  th=%d, value=%d\n", -2, tab_1[2]);
printf("----------------------\n");
printf("tab_1[3]  %s\n",(4==tab_1[3]) ? "pass" : "failed");
printf("tab_1[3]  th=%d, value=%d\n", 4, tab_1[3]);
printf("----------------------\n");
printf("tab_1[4]  %s\n",(14==tab_1[4]) ? "pass" : "failed");
printf("tab_1[4]  th=%d, value=%d\n", 14, tab_1[4]);
printf("----------------------\n");
printf("tab_1[5]  %s\n",(10==tab_1[5]) ? "pass" : "failed");
printf("tab_1[5]  th=%d, value=%d\n", 10, tab_1[5]);
printf("----------------------\n");
printf("tab_1[6]  %s\n",(10==tab_1[6]) ? "pass" : "failed");
printf("tab_1[6]  th=%d, value=%d\n", 10, tab_1[6]);
printf("----------------------\n");
printf("tab_1[7]  %s\n",(14==tab_1[7]) ? "pass" : "failed");
printf("tab_1[7]  th=%d, value=%d\n", 14, tab_1[7]);
printf("----------------------\n");
printf("tab_1[8]  %s\n",(22==tab_1[8]) ? "pass" : "failed");
printf("tab_1[8]  th=%d, value=%d\n", 22, tab_1[8]);
printf("----------------------\n");
printf("tab_1[9]  %s\n",(16==tab_1[9]) ? "pass" : "failed");
printf("tab_1[9]  th=%d, value=%d\n", 16, tab_1[9]);
printf("----------------------\n");
printf("tab_1[10]  %s\n",(32==tab_1[10]) ? "pass" : "failed");
printf("tab_1[10]  th=%d, value=%d\n", 32, tab_1[10]);
printf("----------------------\n");
printf("tab_1[11]  %s\n",(34==tab_1[11]) ? "pass" : "failed");
printf("tab_1[11]  th=%d, value=%d\n", 34, tab_1[11]);
printf("----------------------\n");
printf("tab_1[12]  %s\n",(22==tab_1[12]) ? "pass" : "failed");
printf("tab_1[12]  th=%d, value=%d\n", 22, tab_1[12]);
printf("----------------------\n");
printf("tab_1[13]  %s\n",(32==tab_1[13]) ? "pass" : "failed");
printf("tab_1[13]  th=%d, value=%d\n", 32, tab_1[13]);
printf("----------------------\n");
printf("tab_1[14]  %s\n",(28==tab_1[14]) ? "pass" : "failed");
printf("tab_1[14]  th=%d, value=%d\n", 28, tab_1[14]);
printf("----------------------\n");
 printf("tab_1[15]  %s\n",(28==tab_1[15]) ? "pass" : "failed");
 printf("tab_1[15]  th=%d, value=%d\n", 28, tab_1[15]);
 printf("----------------------\n");
 printf("tab_1[16]  %s\n",(32==tab_1[16]) ? "pass" : "failed");
 printf("tab_1[16]  th=%d, value=%d\n", 32, tab_1[16]);
 printf("----------------------\n");
 printf("tab_1[17]  %s\n",(40==tab_1[17]) ? "pass" : "failed");
 printf("tab_1[17]  th=%d, value=%d\n", 40, tab_1[17]);
 printf("----------------------\n");
 printf("tab_1[18]  %s\n",(34==tab_1[18]) ? "pass" : "failed");
 printf("tab_1[18]  th=%d, value=%d\n", 34, tab_1[18]);
 printf("----------------------\n");
 printf("tab_1[19]  %s\n",(50==tab_1[19]) ? "pass" : "failed");
 printf("tab_1[19]  th=%d, value=%d\n", 50, tab_1[19]);
 printf("----------------------\n");
 printf("tab_1[20]  %s\n",(52==tab_1[20]) ? "pass" : "failed");
 printf("tab_1[20]  th=%d, value=%d\n", 52, tab_1[20]);
 printf("----------------------\n");
 printf("tab_1[21]  %s\n",(40==tab_1[21]) ? "pass" : "failed");
 printf("tab_1[21]  th=%d, value=%d\n", 40, tab_1[21]);
 printf("----------------------\n");
 printf("tab_1[22]  %s\n",(50==tab_1[22]) ? "pass" : "failed");
 printf("tab_1[22]  th=%d, value=%d\n", 50, tab_1[22]);
 printf("----------------------\n");
 printf("tab_1[23]  %s\n",(46==tab_1[23]) ? "pass" : "failed");
 printf("tab_1[23]  th=%d, value=%d\n", 46, tab_1[23]);
 printf("----------------------\n");
 printf("tab_1[24]  %s\n",(46==tab_1[24]) ? "pass" : "failed");
 printf("tab_1[24]  th=%d, value=%d\n", 46, tab_1[24]);
 printf("----------------------\n");
 printf("tab_1[25]  %s\n",(50==tab_1[25]) ? "pass" : "failed");
 printf("tab_1[25]  th=%d, value=%d\n", 50, tab_1[25]);
 printf("----------------------\n");
 printf("tab_1[26]  %s\n",(58==tab_1[26]) ? "pass" : "failed");
 printf("tab_1[26]  th=%d, value=%d\n", 58, tab_1[26]);
 printf("----------------------\n");
 printf("tab_1[27]  %s\n",(52==tab_1[27]) ? "pass" : "failed");
 printf("tab_1[27]  th=%d, value=%d\n", 52, tab_1[27]);
 printf("----------------------\n");
 printf("tab_1[28]  %s\n",(68==tab_1[28]) ? "pass" : "failed");
 printf("tab_1[28]  th=%d, value=%d\n", 68, tab_1[28]);
 printf("----------------------\n");
 printf("tab_1[29]  %s\n",(70==tab_1[29]) ? "pass" : "failed");
 printf("tab_1[29]  th=%d, value=%d\n", 70, tab_1[29]);
 printf("----------------------\n");
 printf("tab_1[30]  %s\n",(58==tab_1[30]) ? "pass" : "failed");
 printf("tab_1[30]  th=%d, value=%d\n", 58, tab_1[30]);
 printf("----------------------\n");
 printf("tab_1[31]  %s\n",(68==tab_1[31]) ? "pass" : "failed");
 printf("tab_1[31]  th=%d, value=%d\n", 68, tab_1[31]);
 printf("----------------------\n");
 printf("tab_1[32]  %s\n",(64==tab_1[32]) ? "pass" : "failed");
 printf("tab_1[32]  th=%d, value=%d\n", 64, tab_1[32]);
 printf("----------------------\n");
 printf("tab_1[33]  %s\n",(64==tab_1[33]) ? "pass" : "failed");
 printf("tab_1[33]  th=%d, value=%d\n", 64, tab_1[33]);
 printf("----------------------\n");
 printf("tab_1[34]  %s\n",(68==tab_1[34]) ? "pass" : "failed");
 printf("tab_1[34]  th=%d, value=%d\n", 68, tab_1[34]);
 printf("----------------------\n");
 printf("tab_1[35]  %s\n",(76==tab_1[35]) ? "pass" : "failed");
 printf("tab_1[35]  th=%d, value=%d\n", 76, tab_1[35]);
 printf("----------------------\n");
 printf("tab_1[36]  %s\n",(70==tab_1[36]) ? "pass" : "failed");
 printf("tab_1[36]  th=%d, value=%d\n", 70, tab_1[36]);
 printf("----------------------\n");
 printf("tab_1[37]  %s\n",(86==tab_1[37]) ? "pass" : "failed");
 printf("tab_1[37]  th=%d, value=%d\n", 86, tab_1[37]);
 printf("----------------------\n");
 printf("tab_1[38]  %s\n",(88==tab_1[38]) ? "pass" : "failed");
 printf("tab_1[38]  th=%d, value=%d\n", 88, tab_1[38]);
 printf("----------------------\n");
 printf("tab_1[39]  %s\n",(76==tab_1[39]) ? "pass" : "failed");
 printf("tab_1[39]  th=%d, value=%d\n", 76, tab_1[39]);
 printf("----------------------\n");
 printf("tab_1[40]  %s\n",(86==tab_1[40]) ? "pass" : "failed");
 printf("tab_1[40]  th=%d, value=%d\n", 86, tab_1[40]);
 printf("----------------------\n");
 printf("tab_1[41]  %s\n",(82==tab_1[41]) ? "pass" : "failed");
 printf("tab_1[41]  th=%d, value=%d\n", 82, tab_1[41]);
 printf("----------------------\n");
 printf("tab_1[42]  %s\n",(82==tab_1[42]) ? "pass" : "failed");
 printf("tab_1[42]  th=%d, value=%d\n", 82, tab_1[42]);
 printf("----------------------\n");
 printf("tab_1[43]  %s\n",(86==tab_1[43]) ? "pass" : "failed");
 printf("tab_1[43]  th=%d, value=%d\n", 86, tab_1[43]);
 printf("----------------------\n");
 printf("tab_1[44]  %s\n",(94==tab_1[44]) ? "pass" : "failed");
 printf("tab_1[44]  th=%d, value=%d\n", 94, tab_1[44]);
 printf("----------------------\n");
 printf("tab_1[45]  %s\n",(88==tab_1[45]) ? "pass" : "failed");
 printf("tab_1[45]  th=%d, value=%d\n", 88, tab_1[45]);
 printf("----------------------\n");
 printf("tab_1[46]  %s\n",(104==tab_1[46]) ? "pass" : "failed");
 printf("tab_1[46]  th=%d, value=%d\n", 104, tab_1[46]);
 printf("----------------------\n");
 printf("tab_1[47]  %s\n",(106==tab_1[47]) ? "pass" : "failed");
 printf("tab_1[47]  th=%d, value=%d\n", 106, tab_1[47]);
 printf("----------------------\n");
 printf("tab_1[48]  %s\n",(94==tab_1[48]) ? "pass" : "failed");
 printf("tab_1[48]  th=%d, value=%d\n", 94, tab_1[48]);
 printf("----------------------\n");
 printf("tab_1[49]  %s\n",(104==tab_1[49]) ? "pass" : "failed");
 printf("tab_1[49]  th=%d, value=%d\n", 104, tab_1[49]);
 printf("----------------------\n");
 printf("tab_1[50]  %s\n",(100==tab_1[50]) ? "pass" : "failed");
 printf("tab_1[50]  th=%d, value=%d\n", 100, tab_1[50]);
 printf("----------------------\n");
 printf("tab_1[51]  %s\n",(100==tab_1[51]) ? "pass" : "failed");
 printf("tab_1[51]  th=%d, value=%d\n", 100, tab_1[51]);
 printf("----------------------\n");
 printf("tab_1[52]  %s\n",(104==tab_1[52]) ? "pass" : "failed");
 printf("tab_1[52]  th=%d, value=%d\n", 104, tab_1[52]);
 printf("----------------------\n");
 printf("tab_1[53]  %s\n",(112==tab_1[53]) ? "pass" : "failed");
 printf("tab_1[53]  th=%d, value=%d\n", 112, tab_1[53]);
 printf("----------------------\n");
 printf("tab_1[54]  %s\n",(106==tab_1[54]) ? "pass" : "failed");
 printf("tab_1[54]  th=%d, value=%d\n", 106, tab_1[54]);
 printf("----------------------\n");
 printf("tab_1[55]  %s\n",(122==tab_1[55]) ? "pass" : "failed");
 printf("tab_1[55]  th=%d, value=%d\n", 122, tab_1[55]);
 printf("----------------------\n");
 printf("tab_1[56]  %s\n",(124==tab_1[56]) ? "pass" : "failed");
 printf("tab_1[56]  th=%d, value=%d\n", 124, tab_1[56]);
 printf("----------------------\n");
 printf("tab_1[57]  %s\n",(112==tab_1[57]) ? "pass" : "failed");
 printf("tab_1[57]  th=%d, value=%d\n", 112, tab_1[57]);
 printf("----------------------\n");
 printf("tab_1[58]  %s\n",(122==tab_1[58]) ? "pass" : "failed");
 printf("tab_1[58]  th=%d, value=%d\n", 122, tab_1[58]);
 printf("----------------------\n");
 printf("tab_1[59]  %s\n",(118==tab_1[59]) ? "pass" : "failed");
 printf("tab_1[59]  th=%d, value=%d\n", 118, tab_1[59]);
 printf("----------------------\n");
 printf("tab_1[60]  %s\n",(118==tab_1[60]) ? "pass" : "failed");
 printf("tab_1[60]  th=%d, value=%d\n", 118, tab_1[60]);
 printf("----------------------\n");
 printf("tab_1[61]  %s\n",(122==tab_1[61]) ? "pass" : "failed");
 printf("tab_1[61]  th=%d, value=%d\n", 122, tab_1[61]);
 printf("----------------------\n");
 printf("tab_1[62]  %s\n",(130==tab_1[62]) ? "pass" : "failed");
 printf("tab_1[62]  th=%d, value=%d\n", 130, tab_1[62]);
 printf("----------------------\n");
 printf("tab_1[63]  %s\n",(124==tab_1[63]) ? "pass" : "failed");
 printf("tab_1[63]  th=%d, value=%d\n", 124, tab_1[63]);
 printf("----------------------\n");
 printf("tab_1[64]  %s\n",(140==tab_1[64]) ? "pass" : "failed");
 printf("tab_1[64]  th=%d, value=%d\n", 140, tab_1[64]);
 printf("----------------------\n");
 printf("tab_1[65]  %s\n",(142==tab_1[65]) ? "pass" : "failed");
 printf("tab_1[65]  th=%d, value=%d\n", 142, tab_1[65]);
 printf("----------------------\n");
 printf("tab_1[66]  %s\n",(130==tab_1[66]) ? "pass" : "failed");
 printf("tab_1[66]  th=%d, value=%d\n", 130, tab_1[66]);
 printf("----------------------\n");
 printf("tab_1[67]  %s\n",(140==tab_1[67]) ? "pass" : "failed");
 printf("tab_1[67]  th=%d, value=%d\n", 140, tab_1[67]);
 printf("----------------------\n");
 printf("tab_1[68]  %s\n",(136==tab_1[68]) ? "pass" : "failed");
 printf("tab_1[68]  th=%d, value=%d\n", 136, tab_1[68]);
 printf("----------------------\n");
 printf("tab_1[69]  %s\n",(136==tab_1[69]) ? "pass" : "failed");
 printf("tab_1[69]  th=%d, value=%d\n", 136, tab_1[69]);
 printf("----------------------\n");
 printf("tab_1[70]  %s\n",(140==tab_1[70]) ? "pass" : "failed");
 printf("tab_1[70]  th=%d, value=%d\n", 140, tab_1[70]);
 printf("----------------------\n");
 printf("tab_1[71]  %s\n",(148==tab_1[71]) ? "pass" : "failed");
 printf("tab_1[71]  th=%d, value=%d\n", 148, tab_1[71]);
 printf("----------------------\n");
 printf("tab_1[72]  %s\n",(142==tab_1[72]) ? "pass" : "failed");
 printf("tab_1[72]  th=%d, value=%d\n", 142, tab_1[72]);
 printf("----------------------\n");
 printf("tab_1[73]  %s\n",(158==tab_1[73]) ? "pass" : "failed");
 printf("tab_1[73]  th=%d, value=%d\n", 158, tab_1[73]);
 printf("----------------------\n");
 printf("tab_1[74]  %s\n",(160==tab_1[74]) ? "pass" : "failed");
 printf("tab_1[74]  th=%d, value=%d\n", 160, tab_1[74]);
 printf("----------------------\n");
 printf("tab_1[75]  %s\n",(148==tab_1[75]) ? "pass" : "failed");
 printf("tab_1[75]  th=%d, value=%d\n", 148, tab_1[75]);
 printf("----------------------\n");
 printf("tab_1[76]  %s\n",(158==tab_1[76]) ? "pass" : "failed");
 printf("tab_1[76]  th=%d, value=%d\n", 158, tab_1[76]);
 printf("----------------------\n");
 printf("tab_1[77]  %s\n",(154==tab_1[77]) ? "pass" : "failed");
 printf("tab_1[77]  th=%d, value=%d\n", 154, tab_1[77]);
 printf("----------------------\n");
 printf("tab_1[78]  %s\n",(154==tab_1[78]) ? "pass" : "failed");
 printf("tab_1[78]  th=%d, value=%d\n", 154, tab_1[78]);
 printf("----------------------\n");
 printf("tab_1[79]  %s\n",(158==tab_1[79]) ? "pass" : "failed");
 printf("tab_1[79]  th=%d, value=%d\n", 158, tab_1[79]);
 printf("----------------------\n");
 printf("tab_1[80]  %s\n",(166==tab_1[80]) ? "pass" : "failed");
 printf("tab_1[80]  th=%d, value=%d\n", 166, tab_1[80]);
 printf("----------------------\n");
 printf("tab_1[81]  %s\n",(160==tab_1[81]) ? "pass" : "failed");
 printf("tab_1[81]  th=%d, value=%d\n", 160, tab_1[81]);
 printf("----------------------\n");
 printf("tab_1[82]  %s\n",(176==tab_1[82]) ? "pass" : "failed");
 printf("tab_1[82]  th=%d, value=%d\n", 176, tab_1[82]);
 printf("----------------------\n");
 printf("tab_1[83]  %s\n",(178==tab_1[83]) ? "pass" : "failed");
 printf("tab_1[83]  th=%d, value=%d\n", 178, tab_1[83]);
 printf("----------------------\n");
 printf("tab_1[84]  %s\n",(166==tab_1[84]) ? "pass" : "failed");
 printf("tab_1[84]  th=%d, value=%d\n", 166, tab_1[84]);
 printf("----------------------\n");
 printf("tab_1[85]  %s\n",(176==tab_1[85]) ? "pass" : "failed");
 printf("tab_1[85]  th=%d, value=%d\n", 176, tab_1[85]);
 printf("----------------------\n");
 printf("tab_1[86]  %s\n",(172==tab_1[86]) ? "pass" : "failed");
 printf("tab_1[86]  th=%d, value=%d\n", 172, tab_1[86]);
 printf("----------------------\n");
 printf("tab_1[87]  %s\n",(172==tab_1[87]) ? "pass" : "failed");
 printf("tab_1[87]  th=%d, value=%d\n", 172, tab_1[87]);
 printf("----------------------\n");
 printf("tab_1[88]  %s\n",(176==tab_1[88]) ? "pass" : "failed");
 printf("tab_1[88]  th=%d, value=%d\n", 176, tab_1[88]);
 printf("----------------------\n");
 printf("tab_1[89]  %s\n",(184==tab_1[89]) ? "pass" : "failed");
 printf("tab_1[89]  th=%d, value=%d\n", 184, tab_1[89]);
 printf("----------------------\n");
 printf("tab_1[90]  %s\n",(178==tab_1[90]) ? "pass" : "failed");
 printf("tab_1[90]  th=%d, value=%d\n", 178, tab_1[90]);
 printf("----------------------\n");
 printf("tab_1[91]  %s\n",(194==tab_1[91]) ? "pass" : "failed");
 printf("tab_1[91]  th=%d, value=%d\n", 194, tab_1[91]);
 printf("----------------------\n");
 printf("tab_1[92]  %s\n",(196==tab_1[92]) ? "pass" : "failed");
 printf("tab_1[92]  th=%d, value=%d\n", 196, tab_1[92]);
 printf("----------------------\n");
 printf("tab_1[93]  %s\n",(184==tab_1[93]) ? "pass" : "failed");
 printf("tab_1[93]  th=%d, value=%d\n", 184, tab_1[93]);
 printf("----------------------\n");
 printf("tab_1[94]  %s\n",(194==tab_1[94]) ? "pass" : "failed");
 printf("tab_1[94]  th=%d, value=%d\n", 194, tab_1[94]);
 printf("----------------------\n");
 printf("tab_1[95]  %s\n",(190==tab_1[95]) ? "pass" : "failed");
 printf("tab_1[95]  th=%d, value=%d\n", 190, tab_1[95]);
 printf("----------------------\n");
 printf("tab_1[96]  %s\n",(190==tab_1[96]) ? "pass" : "failed");
 printf("tab_1[96]  th=%d, value=%d\n", 190, tab_1[96]);
 printf("----------------------\n");
 printf("tab_1[97]  %s\n",(194==tab_1[97]) ? "pass" : "failed");
 printf("tab_1[97]  th=%d, value=%d\n", 194, tab_1[97]);
 printf("----------------------\n");
 printf("tab_1[98]  %s\n",(202==tab_1[98]) ? "pass" : "failed");
 printf("tab_1[98]  th=%d, value=%d\n", 202, tab_1[98]);
 printf("----------------------\n");
 printf("tab_1[99]  %s\n",(196==tab_1[99]) ? "pass" : "failed");
 printf("tab_1[99]  th=%d, value=%d\n", 196, tab_1[99]);
 printf("----------------------\n");
 }
 
 
///Return 
 ret = malloc( 3*8);
 ret[0] = global_1+global_5[51]*global_6[10]-1;
 ret[1] = tab_1[4];
 ret[2] = (ret[0] * ret[1]) % 98;
 return ret;
 
}

int verify_global(){
  
  printf("global_1  %s\n", (0==global_1) ? "pass" : "failed");  
  printf("global_1  th=%d, value=%d\n", 0, global_1);
  printf("----------------------\n");
  
  printf("global_2  %s\n", (45==global_2) ? "pass" : "failed");  
  printf("global_2  th=%d, value=%d\n", 45, global_2);
  printf("----------------------\n");
  
  printf("global_3  %s\n", (-55==global_3) ? "pass" : "failed");  
  printf("global_3  th=%d, value=%d\n", -55, global_3);
  printf("----------------------\n");
  
  printf("global_4  %s\n", (52==global_4) ? "pass" : "failed");  
  printf("global_4  th=%d, value=%d\n", 52, global_4);
  printf("----------------------\n");
  
  {
    printf("global_5[0]  %s\n",(1==global_5[0]) ? "pass" : "failed");
    printf("global_5[0]  th=%d, value=%d\n", 1, global_5[0]);
    printf("----------------------\n");
    printf("global_6[0]  %s\n",(-1==global_6[0]) ? "pass" : "failed");
    printf("global_6[0]  th=%d, value=%d\n", -1, global_6[0]);
    printf("----------------------\n");
    printf("global_5[1]  %s\n",(-2==global_5[1]) ? "pass" : "failed");
    printf("global_5[1]  th=%d, value=%d\n", -2, global_5[1]);
    printf("----------------------\n");
    printf("global_6[1]  %s\n",(0==global_6[1]) ? "pass" : "failed");
    printf("global_6[1]  th=%d, value=%d\n", 0, global_6[1]);
    printf("----------------------\n");
    printf("global_5[2]  %s\n",(0==global_5[2]) ? "pass" : "failed");
    printf("global_5[2]  th=%d, value=%d\n", 0, global_5[2]);
    printf("----------------------\n");
    printf("global_6[2]  %s\n",(1==global_6[2]) ? "pass" : "failed");
    printf("global_6[2]  th=%d, value=%d\n", 1, global_6[2]);
    printf("----------------------\n");
    printf("global_5[3]  %s\n",(4==global_5[3]) ? "pass" : "failed");
    printf("global_5[3]  th=%d, value=%d\n", 4, global_5[3]);
    printf("----------------------\n");
    printf("global_6[3]  %s\n",(2==global_6[3]) ? "pass" : "failed");
    printf("global_6[3]  th=%d, value=%d\n", 2, global_6[3]);
    printf("----------------------\n");
    printf("global_5[4]  %s\n",(1==global_5[4]) ? "pass" : "failed");
    printf("global_5[4]  th=%d, value=%d\n", 1, global_5[4]);
    printf("----------------------\n");
    printf("global_6[4]  %s\n",(3==global_6[4]) ? "pass" : "failed");
    printf("global_6[4]  th=%d, value=%d\n", 3, global_6[4]);
    printf("----------------------\n");
    printf("global_5[5]  %s\n",(0==global_5[5]) ? "pass" : "failed");
    printf("global_5[5]  th=%d, value=%d\n", 0, global_5[5]);
    printf("----------------------\n");
    printf("global_6[5]  %s\n",(4==global_6[5]) ? "pass" : "failed");
    printf("global_6[5]  th=%d, value=%d\n", 4, global_6[5]);
    printf("----------------------\n");
    printf("global_5[6]  %s\n",(1==global_5[6]) ? "pass" : "failed");
    printf("global_5[6]  th=%d, value=%d\n", 1, global_5[6]);
    printf("----------------------\n");
    printf("global_6[6]  %s\n",(5==global_6[6]) ? "pass" : "failed");
    printf("global_6[6]  th=%d, value=%d\n", 5, global_6[6]);
    printf("----------------------\n");
    printf("global_5[7]  %s\n",(4==global_5[7]) ? "pass" : "failed");
    printf("global_5[7]  th=%d, value=%d\n", 4, global_5[7]);
    printf("----------------------\n");
    printf("global_6[7]  %s\n",(6==global_6[7]) ? "pass" : "failed");
    printf("global_6[7]  th=%d, value=%d\n", 6, global_6[7]);
    printf("----------------------\n");
    printf("global_5[8]  %s\n",(0==global_5[8]) ? "pass" : "failed");
    printf("global_5[8]  th=%d, value=%d\n", 0, global_5[8]);
    printf("----------------------\n");
    printf("global_6[8]  %s\n",(7==global_6[8]) ? "pass" : "failed");
    printf("global_6[8]  th=%d, value=%d\n", 7, global_6[8]);
    printf("----------------------\n");
    printf("global_5[9]  %s\n",(7==global_5[9]) ? "pass" : "failed");
    printf("global_5[9]  th=%d, value=%d\n", 7, global_5[9]);
    printf("----------------------\n");
    printf("global_6[9]  %s\n",(8==global_6[9]) ? "pass" : "failed");
    printf("global_6[9]  th=%d, value=%d\n", 8, global_6[9]);
    printf("----------------------\n");
    printf("global_5[10]  %s\n",(7==global_5[10]) ? "pass" : "failed");
    printf("global_5[10]  th=%d, value=%d\n", 7, global_5[10]);
    printf("----------------------\n");
    printf("global_6[10]  %s\n",(9==global_6[10]) ? "pass" : "failed");
    printf("global_6[10]  th=%d, value=%d\n", 9, global_6[10]);
    printf("----------------------\n");
    printf("global_5[11]  %s\n",(0==global_5[11]) ? "pass" : "failed");
    printf("global_5[11]  th=%d, value=%d\n", 0, global_5[11]);
    printf("----------------------\n");
    printf("global_6[11]  %s\n",(10==global_6[11]) ? "pass" : "failed");
    printf("global_6[11]  th=%d, value=%d\n", 10, global_6[11]);
    printf("----------------------\n");
    printf("global_5[12]  %s\n",(4==global_5[12]) ? "pass" : "failed");
    printf("global_5[12]  th=%d, value=%d\n", 4, global_5[12]);
    printf("----------------------\n");
    printf("global_6[12]  %s\n",(11==global_6[12]) ? "pass" : "failed");
    printf("global_6[12]  th=%d, value=%d\n", 11, global_6[12]);
    printf("----------------------\n");
    printf("global_5[13]  %s\n",(1==global_5[13]) ? "pass" : "failed");
    printf("global_5[13]  th=%d, value=%d\n", 1, global_5[13]);
    printf("----------------------\n");
    printf("global_6[13]  %s\n",(12==global_6[13]) ? "pass" : "failed");
    printf("global_6[13]  th=%d, value=%d\n", 12, global_6[13]);
    printf("----------------------\n");
    printf("global_5[14]  %s\n",(0==global_5[14]) ? "pass" : "failed");
    printf("global_5[14]  th=%d, value=%d\n", 0, global_5[14]);
    printf("----------------------\n");
    printf("global_6[14]  %s\n",(13==global_6[14]) ? "pass" : "failed");
    printf("global_6[14]  th=%d, value=%d\n", 13, global_6[14]);
    printf("----------------------\n");
    printf("global_5[15]  %s\n",(1==global_5[15]) ? "pass" : "failed");
    printf("global_5[15]  th=%d, value=%d\n", 1, global_5[15]);
    printf("----------------------\n");
    printf("global_6[15]  %s\n",(14==global_6[15]) ? "pass" : "failed");
    printf("global_6[15]  th=%d, value=%d\n", 14, global_6[15]);
    printf("----------------------\n");
    printf("global_5[16]  %s\n",(4==global_5[16]) ? "pass" : "failed");
    printf("global_5[16]  th=%d, value=%d\n", 4, global_5[16]);
    printf("----------------------\n");
    printf("global_6[16]  %s\n",(15==global_6[16]) ? "pass" : "failed");
    printf("global_6[16]  th=%d, value=%d\n", 15, global_6[16]);
    printf("----------------------\n");
    printf("global_5[17]  %s\n",(0==global_5[17]) ? "pass" : "failed");
    printf("global_5[17]  th=%d, value=%d\n", 0, global_5[17]);
    printf("----------------------\n");
    printf("global_6[17]  %s\n",(16==global_6[17]) ? "pass" : "failed");
    printf("global_6[17]  th=%d, value=%d\n", 16, global_6[17]);
    printf("----------------------\n");
    printf("global_5[18]  %s\n",(7==global_5[18]) ? "pass" : "failed");
    printf("global_5[18]  th=%d, value=%d\n", 7, global_5[18]);
    printf("----------------------\n");
    printf("global_6[18]  %s\n",(17==global_6[18]) ? "pass" : "failed");
    printf("global_6[18]  th=%d, value=%d\n", 17, global_6[18]);
    printf("----------------------\n");
    printf("global_5[19]  %s\n",(7==global_5[19]) ? "pass" : "failed");
    printf("global_5[19]  th=%d, value=%d\n", 7, global_5[19]);
    printf("----------------------\n");
    printf("global_6[19]  %s\n",(18==global_6[19]) ? "pass" : "failed");
    printf("global_6[19]  th=%d, value=%d\n", 18, global_6[19]);
    printf("----------------------\n");
    printf("global_5[20]  %s\n",(0==global_5[20]) ? "pass" : "failed");
    printf("global_5[20]  th=%d, value=%d\n", 0, global_5[20]);
    printf("----------------------\n");
    printf("global_6[20]  %s\n",(19==global_6[20]) ? "pass" : "failed");
    printf("global_6[20]  th=%d, value=%d\n", 19, global_6[20]);
    printf("----------------------\n");
    printf("global_5[21]  %s\n",(4==global_5[21]) ? "pass" : "failed");
    printf("global_5[21]  th=%d, value=%d\n", 4, global_5[21]);
    printf("----------------------\n");
    printf("global_6[21]  %s\n",(20==global_6[21]) ? "pass" : "failed");
    printf("global_6[21]  th=%d, value=%d\n", 20, global_6[21]);
    printf("----------------------\n");
    printf("global_5[22]  %s\n",(1==global_5[22]) ? "pass" : "failed");
    printf("global_5[22]  th=%d, value=%d\n", 1, global_5[22]);
    printf("----------------------\n");
    printf("global_6[22]  %s\n",(21==global_6[22]) ? "pass" : "failed");
    printf("global_6[22]  th=%d, value=%d\n", 21, global_6[22]);
    printf("----------------------\n");
    printf("global_5[23]  %s\n",(0==global_5[23]) ? "pass" : "failed");
    printf("global_5[23]  th=%d, value=%d\n", 0, global_5[23]);
    printf("----------------------\n");
    printf("global_6[23]  %s\n",(22==global_6[23]) ? "pass" : "failed");
    printf("global_6[23]  th=%d, value=%d\n", 22, global_6[23]);
    printf("----------------------\n");
    printf("global_5[24]  %s\n",(1==global_5[24]) ? "pass" : "failed");
    printf("global_5[24]  th=%d, value=%d\n", 1, global_5[24]);
    printf("----------------------\n");
    printf("global_6[24]  %s\n",(23==global_6[24]) ? "pass" : "failed");
    printf("global_6[24]  th=%d, value=%d\n", 23, global_6[24]);
    printf("----------------------\n");
    printf("global_5[25]  %s\n",(4==global_5[25]) ? "pass" : "failed");
    printf("global_5[25]  th=%d, value=%d\n", 4, global_5[25]);
    printf("----------------------\n");
    printf("global_6[25]  %s\n",(24==global_6[25]) ? "pass" : "failed");
    printf("global_6[25]  th=%d, value=%d\n", 24, global_6[25]);
    printf("----------------------\n");
    printf("global_5[26]  %s\n",(0==global_5[26]) ? "pass" : "failed");
    printf("global_5[26]  th=%d, value=%d\n", 0, global_5[26]);
    printf("----------------------\n");
    printf("global_6[26]  %s\n",(25==global_6[26]) ? "pass" : "failed");
    printf("global_6[26]  th=%d, value=%d\n", 25, global_6[26]);
    printf("----------------------\n");
    printf("global_5[27]  %s\n",(7==global_5[27]) ? "pass" : "failed");
    printf("global_5[27]  th=%d, value=%d\n", 7, global_5[27]);
    printf("----------------------\n");
    printf("global_6[27]  %s\n",(26==global_6[27]) ? "pass" : "failed");
    printf("global_6[27]  th=%d, value=%d\n", 26, global_6[27]);
    printf("----------------------\n");
    printf("global_5[28]  %s\n",(7==global_5[28]) ? "pass" : "failed");
    printf("global_5[28]  th=%d, value=%d\n", 7, global_5[28]);
    printf("----------------------\n");
    printf("global_6[28]  %s\n",(27==global_6[28]) ? "pass" : "failed");
    printf("global_6[28]  th=%d, value=%d\n", 27, global_6[28]);
    printf("----------------------\n");
    printf("global_5[29]  %s\n",(0==global_5[29]) ? "pass" : "failed");
    printf("global_5[29]  th=%d, value=%d\n", 0, global_5[29]);
    printf("----------------------\n");
    printf("global_6[29]  %s\n",(28==global_6[29]) ? "pass" : "failed");
    printf("global_6[29]  th=%d, value=%d\n", 28, global_6[29]);
    printf("----------------------\n");
    printf("global_5[30]  %s\n",(4==global_5[30]) ? "pass" : "failed");
    printf("global_5[30]  th=%d, value=%d\n", 4, global_5[30]);
    printf("----------------------\n");
    printf("global_6[30]  %s\n",(29==global_6[30]) ? "pass" : "failed");
    printf("global_6[30]  th=%d, value=%d\n", 29, global_6[30]);
    printf("----------------------\n");
    printf("global_5[31]  %s\n",(1==global_5[31]) ? "pass" : "failed");
    printf("global_5[31]  th=%d, value=%d\n", 1, global_5[31]);
    printf("----------------------\n");
    printf("global_6[31]  %s\n",(30==global_6[31]) ? "pass" : "failed");
    printf("global_6[31]  th=%d, value=%d\n", 30, global_6[31]);
    printf("----------------------\n");
    printf("global_5[32]  %s\n",(0==global_5[32]) ? "pass" : "failed");
    printf("global_5[32]  th=%d, value=%d\n", 0, global_5[32]);
    printf("----------------------\n");
    printf("global_6[32]  %s\n",(31==global_6[32]) ? "pass" : "failed");
    printf("global_6[32]  th=%d, value=%d\n", 31, global_6[32]);
    printf("----------------------\n");
    printf("global_5[33]  %s\n",(1==global_5[33]) ? "pass" : "failed");
    printf("global_5[33]  th=%d, value=%d\n", 1, global_5[33]);
    printf("----------------------\n");
    printf("global_6[33]  %s\n",(32==global_6[33]) ? "pass" : "failed");
    printf("global_6[33]  th=%d, value=%d\n", 32, global_6[33]);
    printf("----------------------\n");
    printf("global_5[34]  %s\n",(4==global_5[34]) ? "pass" : "failed");
    printf("global_5[34]  th=%d, value=%d\n", 4, global_5[34]);
    printf("----------------------\n");
    printf("global_6[34]  %s\n",(33==global_6[34]) ? "pass" : "failed");
    printf("global_6[34]  th=%d, value=%d\n", 33, global_6[34]);
    printf("----------------------\n");
    printf("global_5[35]  %s\n",(0==global_5[35]) ? "pass" : "failed");
    printf("global_5[35]  th=%d, value=%d\n", 0, global_5[35]);
    printf("----------------------\n");
    printf("global_6[35]  %s\n",(34==global_6[35]) ? "pass" : "failed");
    printf("global_6[35]  th=%d, value=%d\n", 34, global_6[35]);
    printf("----------------------\n");
    printf("global_5[36]  %s\n",(7==global_5[36]) ? "pass" : "failed");
    printf("global_5[36]  th=%d, value=%d\n", 7, global_5[36]);
    printf("----------------------\n");
    printf("global_6[36]  %s\n",(35==global_6[36]) ? "pass" : "failed");
    printf("global_6[36]  th=%d, value=%d\n", 35, global_6[36]);
    printf("----------------------\n");
    printf("global_5[37]  %s\n",(7==global_5[37]) ? "pass" : "failed");
    printf("global_5[37]  th=%d, value=%d\n", 7, global_5[37]);
    printf("----------------------\n");
    printf("global_6[37]  %s\n",(36==global_6[37]) ? "pass" : "failed");
    printf("global_6[37]  th=%d, value=%d\n", 36, global_6[37]);
    printf("----------------------\n");
    printf("global_5[38]  %s\n",(0==global_5[38]) ? "pass" : "failed");
    printf("global_5[38]  th=%d, value=%d\n", 0, global_5[38]);
    printf("----------------------\n");
    printf("global_6[38]  %s\n",(37==global_6[38]) ? "pass" : "failed");
    printf("global_6[38]  th=%d, value=%d\n", 37, global_6[38]);
    printf("----------------------\n");
    printf("global_5[39]  %s\n",(4==global_5[39]) ? "pass" : "failed");
    printf("global_5[39]  th=%d, value=%d\n", 4, global_5[39]);
    printf("----------------------\n");
    printf("global_6[39]  %s\n",(38==global_6[39]) ? "pass" : "failed");
    printf("global_6[39]  th=%d, value=%d\n", 38, global_6[39]);
    printf("----------------------\n");
    printf("global_5[40]  %s\n",(1==global_5[40]) ? "pass" : "failed");
    printf("global_5[40]  th=%d, value=%d\n", 1, global_5[40]);
    printf("----------------------\n");
    printf("global_6[40]  %s\n",(39==global_6[40]) ? "pass" : "failed");
    printf("global_6[40]  th=%d, value=%d\n", 39, global_6[40]);
    printf("----------------------\n");
    printf("global_5[41]  %s\n",(0==global_5[41]) ? "pass" : "failed");
    printf("global_5[41]  th=%d, value=%d\n", 0, global_5[41]);
    printf("----------------------\n");
    printf("global_6[41]  %s\n",(40==global_6[41]) ? "pass" : "failed");
    printf("global_6[41]  th=%d, value=%d\n", 40, global_6[41]);
    printf("----------------------\n");
    printf("global_5[42]  %s\n",(1==global_5[42]) ? "pass" : "failed");
    printf("global_5[42]  th=%d, value=%d\n", 1, global_5[42]);
    printf("----------------------\n");
    printf("global_6[42]  %s\n",(41==global_6[42]) ? "pass" : "failed");
    printf("global_6[42]  th=%d, value=%d\n", 41, global_6[42]);
    printf("----------------------\n");
    printf("global_5[43]  %s\n",(4==global_5[43]) ? "pass" : "failed");
    printf("global_5[43]  th=%d, value=%d\n", 4, global_5[43]);
    printf("----------------------\n");
    printf("global_6[43]  %s\n",(42==global_6[43]) ? "pass" : "failed");
    printf("global_6[43]  th=%d, value=%d\n", 42, global_6[43]);
    printf("----------------------\n");
    printf("global_5[44]  %s\n",(0==global_5[44]) ? "pass" : "failed");
    printf("global_5[44]  th=%d, value=%d\n", 0, global_5[44]);
    printf("----------------------\n");
    printf("global_6[44]  %s\n",(43==global_6[44]) ? "pass" : "failed");
    printf("global_6[44]  th=%d, value=%d\n", 43, global_6[44]);
    printf("----------------------\n");
    printf("global_5[45]  %s\n",(7==global_5[45]) ? "pass" : "failed");
    printf("global_5[45]  th=%d, value=%d\n", 7, global_5[45]);
    printf("----------------------\n");
    printf("global_6[45]  %s\n",(44==global_6[45]) ? "pass" : "failed");
    printf("global_6[45]  th=%d, value=%d\n", 44, global_6[45]);
    printf("----------------------\n");
    printf("global_5[46]  %s\n",(7==global_5[46]) ? "pass" : "failed");
    printf("global_5[46]  th=%d, value=%d\n", 7, global_5[46]);
    printf("----------------------\n");
    printf("global_6[46]  %s\n",(45==global_6[46]) ? "pass" : "failed");
    printf("global_6[46]  th=%d, value=%d\n", 45, global_6[46]);
    printf("----------------------\n");
    printf("global_5[47]  %s\n",(0==global_5[47]) ? "pass" : "failed");
    printf("global_5[47]  th=%d, value=%d\n", 0, global_5[47]);
    printf("----------------------\n");
    printf("global_6[47]  %s\n",(46==global_6[47]) ? "pass" : "failed");
    printf("global_6[47]  th=%d, value=%d\n", 46, global_6[47]);
    printf("----------------------\n");
    printf("global_5[48]  %s\n",(4==global_5[48]) ? "pass" : "failed");
    printf("global_5[48]  th=%d, value=%d\n", 4, global_5[48]);
    printf("----------------------\n");
    printf("global_6[48]  %s\n",(47==global_6[48]) ? "pass" : "failed");
    printf("global_6[48]  th=%d, value=%d\n", 47, global_6[48]);
    printf("----------------------\n");
    printf("global_5[49]  %s\n",(1==global_5[49]) ? "pass" : "failed");
    printf("global_5[49]  th=%d, value=%d\n", 1, global_5[49]);
    printf("----------------------\n");
    printf("global_6[49]  %s\n",(48==global_6[49]) ? "pass" : "failed");
    printf("global_6[49]  th=%d, value=%d\n", 48, global_6[49]);
    printf("----------------------\n");
    printf("global_5[50]  %s\n",(0==global_5[50]) ? "pass" : "failed");
    printf("global_5[50]  th=%d, value=%d\n", 0, global_5[50]);
    printf("----------------------\n");
    printf("global_6[50]  %s\n",(49==global_6[50]) ? "pass" : "failed");
    printf("global_6[50]  th=%d, value=%d\n", 49, global_6[50]);
    printf("----------------------\n");
    printf("global_5[51]  %s\n",(1==global_5[51]) ? "pass" : "failed");
    printf("global_5[51]  th=%d, value=%d\n", 1, global_5[51]);
    printf("----------------------\n");
    printf("global_6[51]  %s\n",(50==global_6[51]) ? "pass" : "failed");
    printf("global_6[51]  th=%d, value=%d\n", 50, global_6[51]);
    printf("----------------------\n");
    printf("global_5[52]  %s\n",(4==global_5[52]) ? "pass" : "failed");
    printf("global_5[52]  th=%d, value=%d\n", 4, global_5[52]);
    printf("----------------------\n");
    printf("global_6[52]  %s\n",(51==global_6[52]) ? "pass" : "failed");
    printf("global_6[52]  th=%d, value=%d\n", 51, global_6[52]);
    printf("----------------------\n");
    printf("global_5[53]  %s\n",(0==global_5[53]) ? "pass" : "failed");
    printf("global_5[53]  th=%d, value=%d\n", 0, global_5[53]);
    printf("----------------------\n");
    printf("global_6[53]  %s\n",(52==global_6[53]) ? "pass" : "failed");
    printf("global_6[53]  th=%d, value=%d\n", 52, global_6[53]);
    printf("----------------------\n");
    printf("global_5[54]  %s\n",(7==global_5[54]) ? "pass" : "failed");
    printf("global_5[54]  th=%d, value=%d\n", 7, global_5[54]);
    printf("----------------------\n");
    printf("global_6[54]  %s\n",(53==global_6[54]) ? "pass" : "failed");
    printf("global_6[54]  th=%d, value=%d\n", 53, global_6[54]);
    printf("----------------------\n");
    printf("global_5[55]  %s\n",(7==global_5[55]) ? "pass" : "failed");
    printf("global_5[55]  th=%d, value=%d\n", 7, global_5[55]);
    printf("----------------------\n");
    printf("global_6[55]  %s\n",(54==global_6[55]) ? "pass" : "failed");
    printf("global_6[55]  th=%d, value=%d\n", 54, global_6[55]);
    printf("----------------------\n");
    printf("global_5[56]  %s\n",(0==global_5[56]) ? "pass" : "failed");
    printf("global_5[56]  th=%d, value=%d\n", 0, global_5[56]);
    printf("----------------------\n");
    printf("global_6[56]  %s\n",(55==global_6[56]) ? "pass" : "failed");
    printf("global_6[56]  th=%d, value=%d\n", 55, global_6[56]);
    printf("----------------------\n");
    printf("global_5[57]  %s\n",(4==global_5[57]) ? "pass" : "failed");
    printf("global_5[57]  th=%d, value=%d\n", 4, global_5[57]);
    printf("----------------------\n");
    printf("global_6[57]  %s\n",(56==global_6[57]) ? "pass" : "failed");
    printf("global_6[57]  th=%d, value=%d\n", 56, global_6[57]);
    printf("----------------------\n");
    printf("global_5[58]  %s\n",(1==global_5[58]) ? "pass" : "failed");
    printf("global_5[58]  th=%d, value=%d\n", 1, global_5[58]);
    printf("----------------------\n");
    printf("global_6[58]  %s\n",(57==global_6[58]) ? "pass" : "failed");
    printf("global_6[58]  th=%d, value=%d\n", 57, global_6[58]);
    printf("----------------------\n");
    printf("global_5[59]  %s\n",(0==global_5[59]) ? "pass" : "failed");
    printf("global_5[59]  th=%d, value=%d\n", 0, global_5[59]);
    printf("----------------------\n");
    printf("global_6[59]  %s\n",(58==global_6[59]) ? "pass" : "failed");
    printf("global_6[59]  th=%d, value=%d\n", 58, global_6[59]);
    printf("----------------------\n");
    printf("global_5[60]  %s\n",(1==global_5[60]) ? "pass" : "failed");
    printf("global_5[60]  th=%d, value=%d\n", 1, global_5[60]);
    printf("----------------------\n");
    printf("global_6[60]  %s\n",(59==global_6[60]) ? "pass" : "failed");
    printf("global_6[60]  th=%d, value=%d\n", 59, global_6[60]);
    printf("----------------------\n");
    printf("global_5[61]  %s\n",(4==global_5[61]) ? "pass" : "failed");
    printf("global_5[61]  th=%d, value=%d\n", 4, global_5[61]);
    printf("----------------------\n");
    printf("global_6[61]  %s\n",(60==global_6[61]) ? "pass" : "failed");
    printf("global_6[61]  th=%d, value=%d\n", 60, global_6[61]);
    printf("----------------------\n");
    printf("global_5[62]  %s\n",(0==global_5[62]) ? "pass" : "failed");
    printf("global_5[62]  th=%d, value=%d\n", 0, global_5[62]);
    printf("----------------------\n");
    printf("global_6[62]  %s\n",(61==global_6[62]) ? "pass" : "failed");
    printf("global_6[62]  th=%d, value=%d\n", 61, global_6[62]);
    printf("----------------------\n");
    printf("global_5[63]  %s\n",(7==global_5[63]) ? "pass" : "failed");
    printf("global_5[63]  th=%d, value=%d\n", 7, global_5[63]);
    printf("----------------------\n");
    printf("global_6[63]  %s\n",(62==global_6[63]) ? "pass" : "failed");
    printf("global_6[63]  th=%d, value=%d\n", 62, global_6[63]);
    printf("----------------------\n");
    printf("global_5[64]  %s\n",(7==global_5[64]) ? "pass" : "failed");
    printf("global_5[64]  th=%d, value=%d\n", 7, global_5[64]);
    printf("----------------------\n");
    printf("global_6[64]  %s\n",(63==global_6[64]) ? "pass" : "failed");
    printf("global_6[64]  th=%d, value=%d\n", 63, global_6[64]);
    printf("----------------------\n");
    printf("global_5[65]  %s\n",(0==global_5[65]) ? "pass" : "failed");
    printf("global_5[65]  th=%d, value=%d\n", 0, global_5[65]);
    printf("----------------------\n");
    printf("global_6[65]  %s\n",(64==global_6[65]) ? "pass" : "failed");
    printf("global_6[65]  th=%d, value=%d\n", 64, global_6[65]);
    printf("----------------------\n");
    printf("global_5[66]  %s\n",(4==global_5[66]) ? "pass" : "failed");
    printf("global_5[66]  th=%d, value=%d\n", 4, global_5[66]);
    printf("----------------------\n");
    printf("global_6[66]  %s\n",(65==global_6[66]) ? "pass" : "failed");
    printf("global_6[66]  th=%d, value=%d\n", 65, global_6[66]);
    printf("----------------------\n");
    printf("global_5[67]  %s\n",(1==global_5[67]) ? "pass" : "failed");
    printf("global_5[67]  th=%d, value=%d\n", 1, global_5[67]);
    printf("----------------------\n");
    printf("global_6[67]  %s\n",(66==global_6[67]) ? "pass" : "failed");
    printf("global_6[67]  th=%d, value=%d\n", 66, global_6[67]);
    printf("----------------------\n");
    printf("global_5[68]  %s\n",(0==global_5[68]) ? "pass" : "failed");
    printf("global_5[68]  th=%d, value=%d\n", 0, global_5[68]);
    printf("----------------------\n");
    printf("global_6[68]  %s\n",(67==global_6[68]) ? "pass" : "failed");
    printf("global_6[68]  th=%d, value=%d\n", 67, global_6[68]);
    printf("----------------------\n");
    printf("global_5[69]  %s\n",(1==global_5[69]) ? "pass" : "failed");
    printf("global_5[69]  th=%d, value=%d\n", 1, global_5[69]);
    printf("----------------------\n");
    printf("global_6[69]  %s\n",(68==global_6[69]) ? "pass" : "failed");
    printf("global_6[69]  th=%d, value=%d\n", 68, global_6[69]);
    printf("----------------------\n");
    printf("global_5[70]  %s\n",(4==global_5[70]) ? "pass" : "failed");
    printf("global_5[70]  th=%d, value=%d\n", 4, global_5[70]);
    printf("----------------------\n");
    printf("global_6[70]  %s\n",(69==global_6[70]) ? "pass" : "failed");
    printf("global_6[70]  th=%d, value=%d\n", 69, global_6[70]);
    printf("----------------------\n");
    printf("global_5[71]  %s\n",(0==global_5[71]) ? "pass" : "failed");
    printf("global_5[71]  th=%d, value=%d\n", 0, global_5[71]);
    printf("----------------------\n");
    printf("global_6[71]  %s\n",(70==global_6[71]) ? "pass" : "failed");
    printf("global_6[71]  th=%d, value=%d\n", 70, global_6[71]);
    printf("----------------------\n");
    printf("global_5[72]  %s\n",(7==global_5[72]) ? "pass" : "failed");
    printf("global_5[72]  th=%d, value=%d\n", 7, global_5[72]);
    printf("----------------------\n");
    printf("global_6[72]  %s\n",(71==global_6[72]) ? "pass" : "failed");
    printf("global_6[72]  th=%d, value=%d\n", 71, global_6[72]);
    printf("----------------------\n");
    printf("global_5[73]  %s\n",(7==global_5[73]) ? "pass" : "failed");
    printf("global_5[73]  th=%d, value=%d\n", 7, global_5[73]);
    printf("----------------------\n");
    printf("global_6[73]  %s\n",(72==global_6[73]) ? "pass" : "failed");
    printf("global_6[73]  th=%d, value=%d\n", 72, global_6[73]);
    printf("----------------------\n");
    printf("global_5[74]  %s\n",(0==global_5[74]) ? "pass" : "failed");
    printf("global_5[74]  th=%d, value=%d\n", 0, global_5[74]);
    printf("----------------------\n");
    printf("global_6[74]  %s\n",(73==global_6[74]) ? "pass" : "failed");
    printf("global_6[74]  th=%d, value=%d\n", 73, global_6[74]);
    printf("----------------------\n");
    printf("global_5[75]  %s\n",(4==global_5[75]) ? "pass" : "failed");
    printf("global_5[75]  th=%d, value=%d\n", 4, global_5[75]);
    printf("----------------------\n");
    printf("global_6[75]  %s\n",(74==global_6[75]) ? "pass" : "failed");
    printf("global_6[75]  th=%d, value=%d\n", 74, global_6[75]);
    printf("----------------------\n");
    printf("global_5[76]  %s\n",(1==global_5[76]) ? "pass" : "failed");
    printf("global_5[76]  th=%d, value=%d\n", 1, global_5[76]);
    printf("----------------------\n");
    printf("global_6[76]  %s\n",(75==global_6[76]) ? "pass" : "failed");
    printf("global_6[76]  th=%d, value=%d\n", 75, global_6[76]);
    printf("----------------------\n");
    printf("global_5[77]  %s\n",(0==global_5[77]) ? "pass" : "failed");
    printf("global_5[77]  th=%d, value=%d\n", 0, global_5[77]);
    printf("----------------------\n");
    printf("global_6[77]  %s\n",(76==global_6[77]) ? "pass" : "failed");
    printf("global_6[77]  th=%d, value=%d\n", 76, global_6[77]);
    printf("----------------------\n");
    printf("global_5[78]  %s\n",(1==global_5[78]) ? "pass" : "failed");
    printf("global_5[78]  th=%d, value=%d\n", 1, global_5[78]);
    printf("----------------------\n");
    printf("global_6[78]  %s\n",(77==global_6[78]) ? "pass" : "failed");
    printf("global_6[78]  th=%d, value=%d\n", 77, global_6[78]);
    printf("----------------------\n");
    printf("global_5[79]  %s\n",(4==global_5[79]) ? "pass" : "failed");
    printf("global_5[79]  th=%d, value=%d\n", 4, global_5[79]);
    printf("----------------------\n");
    printf("global_6[79]  %s\n",(78==global_6[79]) ? "pass" : "failed");
    printf("global_6[79]  th=%d, value=%d\n", 78, global_6[79]);
    printf("----------------------\n");
    printf("global_5[80]  %s\n",(0==global_5[80]) ? "pass" : "failed");
    printf("global_5[80]  th=%d, value=%d\n", 0, global_5[80]);
    printf("----------------------\n");
    printf("global_6[80]  %s\n",(79==global_6[80]) ? "pass" : "failed");
    printf("global_6[80]  th=%d, value=%d\n", 79, global_6[80]);
    printf("----------------------\n");
    printf("global_5[81]  %s\n",(7==global_5[81]) ? "pass" : "failed");
    printf("global_5[81]  th=%d, value=%d\n", 7, global_5[81]);
    printf("----------------------\n");
    printf("global_6[81]  %s\n",(80==global_6[81]) ? "pass" : "failed");
    printf("global_6[81]  th=%d, value=%d\n", 80, global_6[81]);
    printf("----------------------\n");
    printf("global_5[82]  %s\n",(7==global_5[82]) ? "pass" : "failed");
    printf("global_5[82]  th=%d, value=%d\n", 7, global_5[82]);
    printf("----------------------\n");
    printf("global_6[82]  %s\n",(81==global_6[82]) ? "pass" : "failed");
    printf("global_6[82]  th=%d, value=%d\n", 81, global_6[82]);
    printf("----------------------\n");
    printf("global_5[83]  %s\n",(0==global_5[83]) ? "pass" : "failed");
    printf("global_5[83]  th=%d, value=%d\n", 0, global_5[83]);
    printf("----------------------\n");
    printf("global_6[83]  %s\n",(82==global_6[83]) ? "pass" : "failed");
    printf("global_6[83]  th=%d, value=%d\n", 82, global_6[83]);
    printf("----------------------\n");
    printf("global_5[84]  %s\n",(4==global_5[84]) ? "pass" : "failed");
    printf("global_5[84]  th=%d, value=%d\n", 4, global_5[84]);
    printf("----------------------\n");
    printf("global_6[84]  %s\n",(83==global_6[84]) ? "pass" : "failed");
    printf("global_6[84]  th=%d, value=%d\n", 83, global_6[84]);
    printf("----------------------\n");
    printf("global_5[85]  %s\n",(1==global_5[85]) ? "pass" : "failed");
    printf("global_5[85]  th=%d, value=%d\n", 1, global_5[85]);
    printf("----------------------\n");
    printf("global_6[85]  %s\n",(84==global_6[85]) ? "pass" : "failed");
    printf("global_6[85]  th=%d, value=%d\n", 84, global_6[85]);
    printf("----------------------\n");
    printf("global_5[86]  %s\n",(0==global_5[86]) ? "pass" : "failed");
    printf("global_5[86]  th=%d, value=%d\n", 0, global_5[86]);
    printf("----------------------\n");
    printf("global_6[86]  %s\n",(85==global_6[86]) ? "pass" : "failed");
    printf("global_6[86]  th=%d, value=%d\n", 85, global_6[86]);
    printf("----------------------\n");
    printf("global_5[87]  %s\n",(1==global_5[87]) ? "pass" : "failed");
    printf("global_5[87]  th=%d, value=%d\n", 1, global_5[87]);
    printf("----------------------\n");
    printf("global_6[87]  %s\n",(86==global_6[87]) ? "pass" : "failed");
    printf("global_6[87]  th=%d, value=%d\n", 86, global_6[87]);
    printf("----------------------\n");
    printf("global_5[88]  %s\n",(4==global_5[88]) ? "pass" : "failed");
    printf("global_5[88]  th=%d, value=%d\n", 4, global_5[88]);
    printf("----------------------\n");
    printf("global_6[88]  %s\n",(87==global_6[88]) ? "pass" : "failed");
    printf("global_6[88]  th=%d, value=%d\n", 87, global_6[88]);
    printf("----------------------\n");
    printf("global_5[89]  %s\n",(0==global_5[89]) ? "pass" : "failed");
    printf("global_5[89]  th=%d, value=%d\n", 0, global_5[89]);
    printf("----------------------\n");
    printf("global_6[89]  %s\n",(88==global_6[89]) ? "pass" : "failed");
    printf("global_6[89]  th=%d, value=%d\n", 88, global_6[89]);
    printf("----------------------\n");
    printf("global_5[90]  %s\n",(7==global_5[90]) ? "pass" : "failed");
    printf("global_5[90]  th=%d, value=%d\n", 7, global_5[90]);
    printf("----------------------\n");
    printf("global_6[90]  %s\n",(89==global_6[90]) ? "pass" : "failed");
    printf("global_6[90]  th=%d, value=%d\n", 89, global_6[90]);
    printf("----------------------\n");
    printf("global_5[91]  %s\n",(7==global_5[91]) ? "pass" : "failed");
    printf("global_5[91]  th=%d, value=%d\n", 7, global_5[91]);
    printf("----------------------\n");
    printf("global_6[91]  %s\n",(90==global_6[91]) ? "pass" : "failed");
    printf("global_6[91]  th=%d, value=%d\n", 90, global_6[91]);
    printf("----------------------\n");
    printf("global_5[92]  %s\n",(0==global_5[92]) ? "pass" : "failed");
    printf("global_5[92]  th=%d, value=%d\n", 0, global_5[92]);
    printf("----------------------\n");
    printf("global_6[92]  %s\n",(91==global_6[92]) ? "pass" : "failed");
    printf("global_6[92]  th=%d, value=%d\n", 91, global_6[92]);
    printf("----------------------\n");
    printf("global_5[93]  %s\n",(4==global_5[93]) ? "pass" : "failed");
    printf("global_5[93]  th=%d, value=%d\n", 4, global_5[93]);
    printf("----------------------\n");
    printf("global_6[93]  %s\n",(92==global_6[93]) ? "pass" : "failed");
    printf("global_6[93]  th=%d, value=%d\n", 92, global_6[93]);
    printf("----------------------\n");
    printf("global_5[94]  %s\n",(1==global_5[94]) ? "pass" : "failed");
    printf("global_5[94]  th=%d, value=%d\n", 1, global_5[94]);
    printf("----------------------\n");
    printf("global_6[94]  %s\n",(93==global_6[94]) ? "pass" : "failed");
    printf("global_6[94]  th=%d, value=%d\n", 93, global_6[94]);
    printf("----------------------\n");
    printf("global_5[95]  %s\n",(0==global_5[95]) ? "pass" : "failed");
    printf("global_5[95]  th=%d, value=%d\n", 0, global_5[95]);
    printf("----------------------\n");
    printf("global_6[95]  %s\n",(94==global_6[95]) ? "pass" : "failed");
    printf("global_6[95]  th=%d, value=%d\n", 94, global_6[95]);
    printf("----------------------\n");
    printf("global_5[96]  %s\n",(1==global_5[96]) ? "pass" : "failed");
    printf("global_5[96]  th=%d, value=%d\n", 1, global_5[96]);
    printf("----------------------\n");
    printf("global_6[96]  %s\n",(95==global_6[96]) ? "pass" : "failed");
    printf("global_6[96]  th=%d, value=%d\n", 95, global_6[96]);
    printf("----------------------\n");
    printf("global_5[97]  %s\n",(4==global_5[97]) ? "pass" : "failed");
    printf("global_5[97]  th=%d, value=%d\n", 4, global_5[97]);
    printf("----------------------\n");
    printf("global_6[97]  %s\n",(96==global_6[97]) ? "pass" : "failed");
    printf("global_6[97]  th=%d, value=%d\n", 96, global_6[97]);
    printf("----------------------\n");
    printf("global_5[98]  %s\n",(0==global_5[98]) ? "pass" : "failed");
    printf("global_5[98]  th=%d, value=%d\n", 0, global_5[98]);
    printf("----------------------\n");
    printf("global_6[98]  %s\n",(97==global_6[98]) ? "pass" : "failed");
    printf("global_6[98]  th=%d, value=%d\n", 97, global_6[98]);
    printf("----------------------\n");
    printf("global_5[99]  %s\n",(7==global_5[99]) ? "pass" : "failed");
    printf("global_5[99]  th=%d, value=%d\n", 7, global_5[99]);
    printf("----------------------\n");
    printf("global_6[99]  %s\n",(98==global_6[99]) ? "pass" : "failed");
    printf("global_6[99]  th=%d, value=%d\n", 98, global_6[99]);
    printf("----------------------\n");
  }
  
}

int test_global(int k){
  k = k*k + 10 + (++k);
  global_1 = (global_1 * global_2+3) % (global_3+1) / global_4;
  global_2 = (k + global_1) % 121;
  global_3 = global_4 + global_2;
  global_4  = (global_4++)+152;
  
  verify_global();
}

int test_last(int a, int b){
  int alpha, beta;
  alpha= (a+b) / 2;
  beta = (a+b)*(a+b)-14;
  
  alpha= (--alpha)*(++alpha) % (beta--) + (beta++) / (alpha--) + (a++);
  beta= ((--alpha)*(++alpha) % (beta--) + (beta++) / (alpha--)) * ( (a--) - (b--));
  
  printf("alpha  %s\n", (6056==alpha) ? "pass" : "failed");  
  printf("alpha  th=%d, value=%d\n", 6056, alpha);
  printf("----------------------\n");
  
  printf("beta  %s\n", (-545541==beta) ? "pass" : "failed");  
  printf("beta  th=%d, value=%d\n", -545541, beta);
  printf("----------------------\n");
}

int test_ordre_args(int a, int b, int c){
  int i;
  i = a + b + c;
  return (i=3)*i+b;
}

int main(){
  int t1, t2, t3, t5,t6;
  int* t4;
  int a, b, c, res, decl;
  t1 = test_expr();
  t2 = test_code();
  t3 = test_args(5,
		 global_1,
		 global_2*global_1,
		 8-9+global_1,
		 1547,
		 12,
		 -589,
		 global_2,
		 global_1,
		 5);
  t6 = test_args_2(5,
		 global_1,
		 global_2*global_1,
		 8-9+global_1,
		 1547,
		 12,
		 -589,
		 global_2,
		 global_1,
		 5,
		 -2);
  
  t4 = test_tabs( NULL );
  
  test_global(1947);
  
  test_last(54, 98);

  a = 67;
  b = 91;
  res = test_ordre_args( c=a , ++c, c=b);

  decl = test_cltq(9876543210);
  
  printf("test_expr  %s\n", (56==t1) ? "pass" : "failed");  
  printf("test_expr th=%d, value=%d\n", 56,  t1);
  printf("----------------------\n");
  
  printf("test_code  %s\n", (145==t2) ? "pass" : "failed");  
  printf("test_code th=%d, value=%d\n", 145, t2 );
  printf("----------------------\n");
  
  printf("test_args  %s\n", (0==t3) ? "pass" : "failed");  
  printf("test_args th=%d, value=%d\n", 0, t3);
  printf("----------------------\n");
  
  printf("test_args_2  %s\n", (0==t6) ? "pass" : "failed");  
  printf("test_args_2 th=%d, value=%d\n", 0, t6);
  printf("----------------------\n");
  
  printf("test_tabs[0]  %s\n", (-8929==t4[0]) ? "pass" : "failed");  
  printf("test_tabs[0] th=%d, value=%d\n", -8929, t4[0]);
  printf("----------------------\n");
  
  printf("test_tabs[1]  %s\n", (14==t4[1]) ? "pass" : "failed");  
  printf("test_tabs[1] th=%d, value=%d\n", 14, t4[1]);
  printf("----------------------\n");
  
  printf("test_tabs[2]  %s\n", (-56==t4[2]) ? "pass" : "failed");  
  printf("test_tabs[2] th=%d, value=%d\n", -56, t4[2]);
  printf("----------------------\n");
  
  printf("ordre_args  %s\n", (67==c) ? "pass" : "failed");
  printf("c_arg th=%d, value=%d\n", 67, c);
  printf("----------------------\n");

  printf("ordre_args  %s\n", (842==res) ? "pass" : "failed");
  printf("result th=%d, value=%d\n", 842, res);
  printf("----------------------\n");

  printf("test_cltq  %s\n", (9876543210==decl) ? "pass" : "failed");
  printf("test_cltq th=%ld, value=%ld\n", 9876543210, decl);
  printf("----------------------\n");
  
  if( t4 )
    free( t4 );
  if( global_5 )
    free( global_5);
  if( global_6 )
    free( global_6);

  printf("Tests finished!\n");

  return 0;
}

int test_cltq(int n){ return n; }


int global_1;
int global_2;
