int add(int a, int b) {
	return a+b;
}

void main() {
	int x = add(2,3) + add(3,4);
	x *= 2;
	assert(x >= 20 && x <= 28); // Le mieux qu'on puisse espérer (intervalles)
}

