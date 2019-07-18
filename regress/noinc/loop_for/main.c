int sum1(int n) {
	int i, sum = 0;

	for (i = 1; i <= n; i = i + 1) {
		sum = sum + i;
	}

	return sum;
}

int sum2(int n) {
	int i, sum = 0;

	for (i = 1; i <= n; i++) {
		sum = sum + i;
	}

	return sum;
}

int main() {
	return sum1(5) - sum2(5);
}
