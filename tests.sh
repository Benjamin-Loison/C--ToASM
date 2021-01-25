echo "3"
./mcc tests/ex0.c && ./tests/ex0; echo $?; echo ""

echo "42"
./mcc tests/ex1.c && ./tests/ex1; echo $?; echo ""

echo "3"
./mcc tests/ex2.c && ./tests/ex2; echo $?; echo ""

echo "1"
./mcc tests/ex3.c && ./tests/ex3; echo $?; echo ""

echo "ok"
./mcc tests/ex4.c && ./tests/ex4; echo -e "\n"

echo "1,2"
./mcc tests/ex5.c && ./tests/ex5; echo -e "\n"

echo "1"
./mcc tests/ex6.c && ./tests/ex6; echo -e "\n"

echo "53"
./mcc tests/ex7.c && ./tests/ex7; echo $?; echo ""

echo "12"
./mcc tests/ex8.c && ./tests/ex8; echo -e "\n"

echo "10 12 10"
./mcc tests/ex9.c && ./tests/ex9; echo -e "\n"

echo "on est dans le if7"
./mcc tests/ex10.c && ./tests/ex10; echo -e "\n"

echo "5 6 4 5"
./mcc tests/ex11.c && ./tests/ex11; echo -e "\n"

echo "ok"
./mcc tests/ex12.c && ./tests/ex12; echo -e "\n"

echo "7 3"
./mcc tests/ordre.c && ./tests/ordre; echo -e "\n"

echo "ab cd"
./mcc tests/cat.c && ./tests/cat tests/a tests/b; echo -e "\n"

echo "120"
./mcc tests/fact.c && ./tests/fact 5; echo -e "\n"
