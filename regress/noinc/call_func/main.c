int func1(int, int);

int main() {
	int r;
	r = func1(1, 2);
	return r - 3;
}

int func1(int a, int b) {
	return a + b;
}
