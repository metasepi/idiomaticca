int switch_case(char c) {
	int ret;

	switch (c) {
	case '0':
		ret = 1;
		break;
	case ' ':
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

	num = switch_case('0') == 1;
	space = switch_case(' ') == 2;
	other = switch_case('Q') == 3;

	return !(num && space && other);
}
