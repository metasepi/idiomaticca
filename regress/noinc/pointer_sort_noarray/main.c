void sort(int *pa, int *pb, int *pc) {
	int temp = 0, flag = 1;

	while (flag == 1) {
		flag = 0;
		if (*pa > *pb) {
			temp = *pa;
			*pa = *pb;
			*pb = temp;
			flag = 1;
		}
		if (*pb > *pc) {
			temp = *pb;
			*pb = *pc;
			*pc = temp;
			flag = 1;
		}
	}
}

int main() {
	int a = 3, b = 1, c = 2;
	int *pa, *pb;

	pa = &a;
	pb = &b;
	sort(pa, pb, &c);

	return a * 100 + b * 10 + c * 1 - 123;
}
