int main() {
	int x = 1, y = 2;
	int *ip;

	ip = &x;
	y = *ip;

	return y - 1;
}
