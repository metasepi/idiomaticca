int char_fib(char n) {
	int f0 = 0, f1 = 1;

	while (n > '\0' ) {
		n = n - 1;
		int tmp = f1;
		f1 = f0 + f1;
		f0 = tmp;
	}

	return f0;
}

int main() {
	return char_fib('\n') - 55;
}
