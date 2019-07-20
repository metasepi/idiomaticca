int switch_case(char c) {
	int ret;

	switch (c) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		ret = 1;
		break;
	case ' ':
	case '\n':
	case '\t':
		ret = 2;
		break;
	default:
		ret = 3;
		break;
	}

	return ret;
}

int main() {
	int num, space, other;

	num = switch_case('0');
	space = switch_case(' ');
	other = switch_case('Q');

	return num * 100 + space * 10 + other - 123;
}
