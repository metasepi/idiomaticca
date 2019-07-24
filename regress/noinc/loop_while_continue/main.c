int while_continue() {
	int a = 10, b = 10;

	while(a < 20) {
		a++;
		if (a > 15) {
			a++;
			continue;
		}
		b++;
	}

	return a + b;
}

int main() {
	return while_continue() - 21 - 15;
}
