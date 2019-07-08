extern int func2(int, int);

int func1(int a, int b) {
	return func2(a, b);
}

int main() {
	int r;
	r = func1(1, 2);
	return r - 3;
}

int func2(int a, int b) {
	return a + b;
}
