int for_break() {
	int a = 10, b = 10, i;

	for (i = 0; i < 20; i++) {
		a++;
		if (a > 15) {
			break;
		}
		b++;
	}

	return a + b;
}

int main() {
	return for_break() - 16 - 15;
}
