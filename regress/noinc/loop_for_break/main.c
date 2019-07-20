int for_break() {
	int a = 10;

	while(a < 20) {
		a++;
		if (a > 15) {
			break;
		}
	}

	return a;
}

int main() {
	return for_break() - 16;
}
