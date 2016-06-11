void main() {
	int a = rand(1,3);
	int b = rand(0,6);
	int c;
	if(a+b >= 8) {
		c = b - a;
		assert(b >= 5 && a >= 2);
	}
}

