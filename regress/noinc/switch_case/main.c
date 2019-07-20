int switch_case(char c) {
	int ret;

	switch (c) {
	case '1':
	case '2':
	case '3':
		ret = 1;
		break;
	case ' ':
		ret = 2;
		break;
	default:
		ret = 1;
		ret = 3;
		break;
	}

	return ret;
}

int main() {
	int num, space, other;

	num = switch_case('1');
	space = switch_case(' ');
	other = switch_case('Q');

	return num * 100 + space * 10 + other - 123;
}
