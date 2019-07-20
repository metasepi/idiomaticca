int while_break() {
	int a = 10, b = 10;

	while(a < 20) {
		a++;
		if (a > 15) {
			break;
		}
		b++;
	}

	return a + b;
}

int main() {
	return while_break() - 16 - 15;
}
