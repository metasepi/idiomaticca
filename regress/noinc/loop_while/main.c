int fib(int n) {
	int tmp, f0 = 0, f1 = 1;

	do {
		n--;
		tmp = f1;
		f1 = f0 + f1;
		f0 = tmp;
	} while (n > 0);

	return f0;
}

int main() {
	return fib(10) - 55;
}
