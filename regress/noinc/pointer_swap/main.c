void swap2(int *px, int *py) {
	int temp;

	temp = *px;
	*px = *py;
	*py = temp;
}

void swap1(int *px, int *py) {
	swap2(px, py);
}

int main() {
	int x = 1, y = 2;
	int *px;

	px = &x;
	swap1(px, &y);

	return x * 10 + y - 21;
}
