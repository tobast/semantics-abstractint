int main() {
	int x;
	x = 5;
	x = x*4;
	x = x / 2;
	x = x - 3 + 2;
	if(x >= 7)
		x = x / 3;
	else
		x = x * 3;
	assert(x==3);
	return x;
}

