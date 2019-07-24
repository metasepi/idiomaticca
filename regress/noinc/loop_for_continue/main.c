int for_continue() {
	int a = 10, b = 10, i;

	for (i = 0; i < 20; i++) {
		a++;
		if (a > 15) {
			continue;
		}
		b++;
	}

	return a + b + i;
}

int main() {
	return for_continue() - 30 - 15 - 20;
}
