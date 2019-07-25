void swap(int *px, int *py) {
	int temp;

	temp = *px;
	*px = *py;
	*py = temp;
}

int main() {
	int x = 1, y = 2;
	int *px;

	px = &x;
	swap(px, &y);

	return x * 10 + y - 21;
}
